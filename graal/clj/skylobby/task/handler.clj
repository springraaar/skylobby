(ns skylobby.task.handler
  (:require
   [clojure.core.cache :as cache]
   [clojure.java.io :as io]
   [clojure.pprint :refer [pprint]]
   clojure.set
   [clojure.string :as string]
   [me.raynes.fs :as raynes-fs]
   [skylobby.client.gloss :as gloss]
   [skylobby.client.message :as message]
   [skylobby.download :as download]
   [skylobby.event.battle :as event.battle]
   [skylobby.fs :as fs]
   [skylobby.fs.sdfz :as fs.sdfz]
   [skylobby.fs.smf :as fs.smf]
   [skylobby.git :as git]
   [skylobby.http :as http]
   [skylobby.import :as import]
   [skylobby.rapid :as rapid]
   [skylobby.replay :as replay]
   [skylobby.resource :as resource]
   [skylobby.task :as task]
   [skylobby.util :as u]
   [taoensso.timbre :as log]
   [version-clj.core :as version])
  (:import
   (java.util.regex Matcher)))


(set! *warn-on-reflection* true)


(def maps-batch-size 5)
(def mods-batch-size 5)
(def minimap-batch-size 3)
(def replays-batch-size 10)

(defn update-battle-sync-statuses [state-atom]
  (let [state @state-atom]
    (doseq [[server-key server-data] (concat
                                      (-> state :by-server u/valid-servers)
                                      (-> state u/complex-servers))]
      (let [server-url (-> server-data :client-data :server-url)
            {:keys [servers spring-isolation-dir]} state
            spring-root (or (-> servers (get server-url) :spring-isolation-dir)
                            spring-isolation-dir)
            spring-root-path (fs/canonical-path spring-root)

            spring (-> state :by-spring-root (get spring-root-path))

            new-sync-status (resource/sync-status server-data spring (:mod-details state) (:map-details state))

            new-sync-number (u/sync-number new-sync-status)
            {:keys [battle client-data username]} server-data
            {:keys [battle-status team-color]} (-> battle :users (get username))
            old-sync-number (-> battle :users (get username) :battle-status :sync)]
        (when (and (:battle-id battle)
                   (not= old-sync-number new-sync-number))
          (log/info "Updating battle sync status for" server-key "from" old-sync-number
                    "to" new-sync-number)
          (if (#{:direct-client :direct-host} (u/server-type server-key))
            (event.battle/update-player-or-bot-state
             state-atom
             server-key
             {:username username}
             {:battle-status {:sync new-sync-number}})
            (let [new-battle-status (assoc battle-status :sync new-sync-number)]
              (message/send state-atom client-data
                            (str "MYBATTLESTATUS " (gloss/encode-battle-status new-battle-status) " " (or team-color 0))))))))))

(defn update-my-battle-status
  [state-atom
   {:keys [client-data server-key username]}
   battle-status team-color]
  (if client-data ; TODO other server types
    (message/send state-atom client-data
                  (str "MYBATTLESTATUS " (gloss/encode-battle-status battle-status) " " (or team-color "0")))
    (let [data {:battle-status battle-status
                :team-color team-color}]
      (log/info "No client, assuming singleplayer")
      (swap! state-atom
             (fn [state]
               (-> state
                   (update-in [:by-server server-key :battles :singleplayer :users username] merge data)
                   (update-in [:by-server server-key :battle :users username] merge data)))))))

(defn- update-preferred-faction [state-atom state mod-name mod-details]
  (let [{:keys [by-server preferred-factions]} state
        preferred-faction (get preferred-factions (:mod-name-only mod-details))
        servers-to-update (->> by-server
                               (map
                                (fn [[server-key server-data]]
                                  (let [{:keys [battle battles client-data username]} server-data]
                                    {:server-key server-key
                                     :client-data client-data
                                     :mod-name (get-in battles [(:battle-id battle) :battle-modname])
                                     :user-data (assoc
                                                 (get-in battle [:users username])
                                                 :username username)})))
                               (filter (comp #{mod-name} :mod-name))
                               (remove (comp #{preferred-faction} :side :battle-status :user-data)))]
    (when preferred-faction
      (doseq [{:keys [client-data server-key user-data]} servers-to-update]
        (log/info "Fixing faction for" server-key user-data "to" preferred-faction)
        (let [{:keys [battle-status team-color username]} user-data
              new-battle-status (assoc battle-status :side preferred-faction)]
          (update-my-battle-status state-atom
                                   {:client-data client-data
                                    :server-key server-key
                                    :username username}
                                   new-battle-status team-color))))))

(defn- read-map-details [{:keys [map-name map-file]}]
  (let [log-map-name (str "'" map-name "'")]
    (try
      (if map-file
        (fs/read-map-data map-file)
        (log/warn "No file found for map" log-map-name))
      (catch Exception e
        (log/error e "Error reading map data from" map-file)))))

(defn- update-cached-minimaps
  ([state-atom maps]
   (update-cached-minimaps state-atom maps nil))
  ([state-atom maps {:keys [priorities] :as opts}]
   (let [to-update
         (->> maps
              (filter :map-name)
              (filter
               (fn [map-details]
                 (let [heightmap-file (-> map-details :map-name (fs/minimap-image-cache-file {:minimap-type "heightmap"}))
                       metalmap-file (-> map-details :map-name (fs/minimap-image-cache-file {:minimap-type "metalmap"}))
                       minimap-file (-> map-details :map-name (fs/minimap-image-cache-file {:minimap-type "minimap"}))]
                   (or (:force opts)
                       (not (fs/exists? heightmap-file))
                       (not (fs/exists? metalmap-file))
                       (not (fs/exists? minimap-file))))))
              (sort-by :map-name)
              (concat priorities))
         this-round (take minimap-batch-size to-update)
         next-round (drop minimap-batch-size to-update)]
     (log/info (count to-update) "maps do not have cached minimap image files")
     (doseq [map-details this-round]
       (if-let [map-file (:file map-details)]
         (do
           (log/info "Caching minimap for" map-file)
           (let [{:keys [map-name smf]} (fs/read-map-data map-file {:map-images true})
                 {:keys [minimap-height minimap-width]} smf]
             (when-let [minimap-image (:minimap-image smf)]
               (let [minimap-image-scaled (fs.smf/scale-minimap-image minimap-width minimap-height minimap-image)]
                 (fs/write-image-png minimap-image-scaled (fs/minimap-image-cache-file map-name {:minimap-type "minimap"}))))
             (when-let [metalmap-image (:metalmap-image smf)]
               (let [metalmap-image-scaled (fs.smf/scale-minimap-image minimap-width minimap-height metalmap-image)]
                 (fs/write-image-png metalmap-image-scaled (fs/minimap-image-cache-file map-name {:minimap-type "metalmap"}))))
             (when-let [heightmap-image (:heightmap-image smf)]
               (let [heightmap-image-scaled (fs.smf/scale-minimap-image minimap-width minimap-height heightmap-image)]
                 (fs/write-image-png heightmap-image-scaled (fs/minimap-image-cache-file map-name {:minimap-type "heightmap"}))))
             (swap! state-atom
                    (fn [state]
                      (-> state
                          (assoc-in [:cached-minimap-updated (fs/canonical-path map-file)] (u/curr-millis))
                          (assoc-in [:cached-minimap-updated map-name] (u/curr-millis)))))))
         (log/error "Map is missing file" (:map-name map-details))))
     (when (seq next-round)
       (task/add-task! state-atom
                       {:spring-lobby/task-type :spring-lobby/update-cached-minimaps
                        :priorities priorities})))))
                        ;:todo (count next-round)})))))

(defn- refresh-maps
  "Reads map details and caches for maps missing from :maps in state."
  ([state-atom]
   (refresh-maps state-atom nil))
  ([state-atom spring-root]
   (refresh-maps state-atom spring-root nil))
  ([state-atom spring-root {:keys [priorities]}]
   (log/info "Refreshing maps in" spring-root)
   (let [before (u/curr-millis)
         {:keys [spring-isolation-dir] :as state} @state-atom
         spring-root (or spring-root spring-isolation-dir)
         _ (log/info "Updating maps in" spring-root)
         spring-root-path (fs/canonical-path spring-root)
         maps (-> state :by-spring-root (get spring-root-path) :maps)
         map-files (fs/map-files spring-root)
         known-files (->> maps (map :file) set)
         error-files (->> maps (filter :error) (map :file))
         known-paths (->> known-files (map fs/canonical-path) set)
         todo (->> map-files
                   (remove
                    (fn [file]
                      (contains? known-paths (fs/canonical-path file))))
                   shuffle)
         _ (log/info "Found" (count todo) "maps to load in" (- (u/curr-millis) before) "ms")
         battle-maps (->> state
                          :by-server
                          (map second)
                          (map
                           (fn [{:keys [battle battles]}]
                             (-> battles (get (:battle-id battle)) :battle-map)))
                          (filter some?))
         priorities (->> todo
                         (filter
                          (comp
                           (fn [resource]
                             (some
                              #(resource/could-be-this-map? % resource)
                              battle-maps))
                           (fn [f]
                             {:resource-filename (fs/filename f)})))
                         (concat priorities))
         _ (log/info "Prioritizing maps in battles" (pr-str priorities))
         this-round (concat priorities
                            (if (<= (count todo) maps-batch-size) ; add errors if there's not a full round left
                              (do
                                (log/info "Adding map directories and maps with errors to scan")
                                (concat todo
                                        (filter fs/is-directory? map-files) ; re-scan directories just before errors
                                        error-files))
                              (take maps-batch-size todo)))
         next-round (drop maps-batch-size todo)
         all-paths (set (filter some? (map (comp fs/canonical-path :file) maps)))
         next-round (->> next-round
                         (remove (comp all-paths fs/canonical-path))
                         seq)
         next-round-count (count next-round)
         missing-paths (set
                        (concat
                         (->> known-files
                              (remove fs/exists?)
                              (map fs/canonical-path))
                         (->> known-files
                              (remove (partial fs/descendant? spring-root))
                              (map fs/canonical-path))))]
     (apply fs/update-file-cache! state-atom map-files)
     (fs/make-dirs fs/maps-cache-root)
     (when (seq this-round)
       (log/info "Adding" (count this-round) "maps this iteration:" (with-out-str (pprint this-round))))
     (let [map-file-to-name (into {}
                                  (for [map-file this-round]
                                    (do
                                      (log/info "Reading" map-file)
                                      (let [map-data (fs/read-map-data map-file)
                                            relevant-map-data (select-keys map-data [:error :file :map-name])]
                                        (swap! state-atom update-in [:by-spring-root spring-root-path :maps]
                                               (fn [maps]
                                                 (let [valid-maps (->> maps
                                                                       (remove (comp #{(fs/canonical-path map-file)} fs/canonical-path :file)))]
                                                   (set
                                                    (cond-> valid-maps
                                                      map-data
                                                      (conj relevant-map-data))))))
                                        [map-file (:map-name map-data)]))))
           priority-maps (map (fn [map-file]
                                {:map-name (get map-file-to-name map-file)
                                 :file map-file})
                              priorities)]
       (log/debug "Removing maps with no name, and" (count missing-paths) "maps with missing files")
       (swap! state-atom update-in [:by-spring-root spring-root-path :maps]
              (fn [maps]
                (->> maps
                     (filter (comp (partial instance? java.io.File) :file))
                     (map
                       (fn [{:keys [file path] :as m}]
                         (if path
                           m
                           (assoc m :path (fs/canonical-path file)))))
                     (remove
                      (fn [{:keys [error map-name]}]
                        (and (not error)
                             (string/blank? map-name))))
                     (remove (comp missing-paths :path))
                     set)))
       (if next-round
         (do
           (log/info "Scheduling map load since there are" next-round-count "maps left to load")
           (when (seq priority-maps)
             (task/add-task! state-atom {:spring-lobby/task-type :spring-lobby/update-cached-minimaps
                                         :priorities priority-maps}))
           (task/add-task! state-atom
                           {:spring-lobby/task-type :spring-lobby/refresh-maps
                            :spring-root spring-root}))
                            ;:todo next-round-count}))
         (do
           (log/info "Finished map refresh")
           (task/add-task! state-atom {:spring-lobby/task-type :spring-lobby/update-cached-minimaps
                                       :priorities priority-maps})))
       {:todo-count next-round-count}))))

(defn refresh-maps-all-spring-roots [state-atom]
  (let [spring-roots (fs/spring-roots @state-atom)]
    (log/info "Refreshing maps in" (pr-str spring-roots))
    (doseq [spring-root spring-roots]
      (refresh-maps state-atom spring-root))))

(defn- refresh-engines
  "Reads engine details and updates missing engines in :engines in state."
  ([state-atom]
   (refresh-engines state-atom nil))
  ([state-atom spring-root]
   (refresh-engines state-atom spring-root nil))
  ([state-atom spring-root opts]
   (log/info "Refreshing engines in" spring-root)
   (apply fs/update-file-cache! state-atom (file-seq (fs/download-dir))) ; TODO move this somewhere
   (let [before (u/curr-millis)
         {:keys [spring-isolation-dir] :as state} @state-atom
         spring-root (or spring-root spring-isolation-dir)
         _ (log/info "Updating engines in" spring-root)
         engine-dirs (fs/engine-dirs spring-root)
         spring-root-path (fs/canonical-path spring-root)
         known-canonical-paths (->> (-> state
                                        :by-spring-root
                                        (get spring-root-path))
                                    :engines
                                    (map :path)
                                    (filter some?)
                                    set)
         to-add (if (:force opts) ; refresh existing engines too
                  engine-dirs
                  (remove (comp known-canonical-paths fs/canonical-path) engine-dirs))
         canonical-path-set (set (map fs/canonical-path engine-dirs))
         missing-files (set
                        (concat
                         (->> known-canonical-paths
                              (remove
                                (comp (every-pred
                                        fs/exists?
                                        fs/is-directory?)
                                      io/file)))
                         (->> known-canonical-paths
                              (remove (comp (partial fs/descendant? spring-root) io/file)))))
         to-remove (set
                    (concat missing-files
                            (remove canonical-path-set known-canonical-paths)))]
     (apply fs/update-file-cache! state-atom known-canonical-paths)
     (log/info "Found" (count to-add) "engines to load in" (- (u/curr-millis) before) "ms")
     (doseq [engine-dir to-add]
       (log/info "Detecting engine data for" engine-dir)
       (let [engine-data (fs/engine-data engine-dir)]
         (when (:error engine-data)
           (log/warn "Error getting engine data for" (:file engine-data)))
         (swap! state-atom update-in [:by-spring-root spring-root-path :engines]
                (fn [engines]
                  (set
                   (conj
                    (remove (comp #{(fs/canonical-path engine-dir)} fs/canonical-path :file) engines)
                    engine-data))))))
     (log/debug "Removing" (count to-remove) "engines")
     (swap! state-atom update-in [:by-spring-root spring-root-path :engines]
            (fn [engines]
              (->> engines
                   (filter (comp fs/canonical-path :file))
                   (map
                     (fn [{:keys [file path] :as m}]
                       (if path
                         m
                         (assoc m :path (fs/canonical-path file)))))
                   (filter fs/is-current-engine-data?)
                   (remove (comp to-remove :path))
                   set)))
     {:to-add-count (count to-add)
      :to-remove-count (count to-remove)})))

(defn refresh-engines-all-spring-roots [state-atom]
  (let [spring-roots (fs/spring-roots @state-atom)]
    (log/info "Refreshing engines in" (pr-str spring-roots))
    (doseq [spring-root spring-roots]
      (refresh-engines state-atom spring-root))))

(defn set-sdd-modinfo-version [modinfo-str mod-version]
  (let [[_ prefix old-version] (re-find #"(version\s*=\s*)'([^']+)'" modinfo-str)]
    (log/info "Replacing version after" prefix (pr-str old-version) "with" mod-version)
    (string/replace
      modinfo-str
      #"version\s*=\s*'[^']+'"
      (Matcher/quoteReplacement
        (str prefix "'" mod-version "'")))))

(defn read-mod-data
  ([f]
   (read-mod-data f nil))
  ([f {:keys [delete-invalid-sdp] :as opts}]
   (let [filename (fs/filename f)
         is-sdp (and (string? filename)
                     (string/ends-with? filename ".sdp"))]
     (try
       (let [mod-data
             (if (string/ends-with? (fs/filename f) ".sdp")
               (rapid/read-sdp-mod f opts)
               (fs/read-mod-file f opts))
             name-and-version (u/mod-name-and-version mod-data opts)]
         (merge mod-data name-and-version))
       (catch Exception e
         (log/warn e "Error reading mod details")
         (when (and delete-invalid-sdp is-sdp)
           (log/info "Deleting invalid sdp file" f)
           (raynes-fs/delete f))
         {:file f
          :path (fs/canonical-path f)
          :error true})))))

(defn- update-mod
  ([state-atom spring-root-path file]
   (update-mod state-atom spring-root-path file nil))
  ([state-atom spring-root-path file {:keys [use-git-mod-version] :as opts}]
   (when (fs/is-directory? file)
     (try
       (let [git-commit-id (try
                             (git/latest-id file)
                             (catch java.io.FileNotFoundException _e
                               (log/warn "Not a git repository at" file))
                             (catch Exception e
                               (log/error e "Error loading git commit id")))
             mod-version (if use-git-mod-version
                           (str "git:" (u/short-git-commit git-commit-id))
                           "$VERSION")]
         (log/info "Setting version in modinfo.lua to" mod-version)
         (let [modinfo-file (fs/file file "modinfo.lua")
               contents (slurp modinfo-file)]
           (spit modinfo-file (set-sdd-modinfo-version contents mod-version))))
       (catch Exception e
         (log/error e "Error setting modinfo version for git"))))
   (let [path (fs/canonical-path file)
         mod-data (try
                    (read-mod-data file (assoc opts :modinfo-only false))
                    (catch Exception e
                      (log/error e "Error reading mod data for" file)
                      {:file file
                       :error true}))
         mod-details (select-keys mod-data [:error :file :mod-name :mod-name-only :mod-version :path ::fs/source :git-commit-id])
         mod-details (assoc mod-details
                            :is-game
                            (boolean
                             (or (:engineoptions mod-data)
                                 (:modoptions mod-data))))]
     (log/info "Read mod:" (with-out-str (pprint (select-keys mod-details [:error :file :path :is-game :mod-name :mod-name-only :mod-version]))))
     (let [[old-state {:keys [by-spring-root mod-details use-git-mod-version]}]
           (swap-vals! state-atom update-in [:by-spring-root spring-root-path :mods]
                       (fn [mods]
                         (set
                           (cond->
                             (remove (comp #{path} fs/canonical-path :file) mods)
                             mod-details
                             (conj mod-details)))))
           cached (get mod-details path)]
       (log/info "Mod count in" spring-root-path "changed from"
                 (count (get-in old-state [:by-spring-root spring-root-path :mods]))
                 "to"
                 (count (get-in by-spring-root [spring-root-path :mods])))
       (when (and cached
                  (not= (:mod-version cached)
                        (:mod-version mod-data)))
         (task/add-task! state-atom
                         {:spring-lobby/task-type :spring-lobby/mod-details
                          :mod-name (:mod-name mod-data)
                          :mod-file file
                          :use-git-mod-version use-git-mod-version})))
     mod-data)))

(defn- refresh-mods
  "Reads mod details and updates missing mods in :mods in state."
  ([state-atom]
   (refresh-mods state-atom nil))
  ([state-atom spring-root]
   (refresh-mods state-atom spring-root nil))
  ([state-atom spring-root {:keys [delete-invalid-sdp priorities] :as opts}]
   (log/info "Refreshing mods in" spring-root opts)
   (let [before (u/curr-millis)
         {:keys [rapid-by-spring-root spring-isolation-dir use-git-mod-version] :as state} @state-atom
         spring-root (or spring-root spring-isolation-dir)
         _ (log/info "Updating mods in" spring-root)
         spring-root-path (fs/canonical-path spring-root)
         {:keys [rapid-data-by-version]} (get rapid-by-spring-root spring-root-path)
         mods (-> state :by-spring-root (get spring-root-path) :mods)
         error-files (->> mods (filter :error) (map :file))
         {:keys [directory]} (group-by ::fs/source mods)
         known-paths (set (map (comp fs/canonical-path :file) mods))
         mod-files (fs/mod-files spring-root)
         sdp-files (rapid/sdp-files spring-root)
         _ (log/info "Found" (count mod-files) "files and"
                     (count sdp-files) "rapid archives to scan for mods")
         to-add-file (set (remove (comp known-paths fs/canonical-path) mod-files))
         to-add-rapid (remove (comp known-paths fs/canonical-path) sdp-files)
         todo (shuffle (concat to-add-file to-add-rapid))
         _ (log/info "Found" (count to-add-file) "mod files and" (count to-add-rapid)
                     "rapid files to scan for mods in" (- (u/curr-millis) before) "ms")
         battle-mods (->> state
                          :by-server
                          (map second)
                          (map
                           (fn [{:keys [battle battles]}]
                             (let [{:keys [battle-id]} battle]
                               (get-in battles [battle-id :battle-modname]))))
                          (filter some?))
         battle-rapid-data (map (partial get rapid-data-by-version) battle-mods)
         battle-rapid-hashes (->> battle-rapid-data
                                  (map :hash)
                                  (filter some?)
                                  set)
         priorities (->> todo
                         (filter
                          (some-fn
                           (comp
                            (fn [resource]
                              (some
                               #(resource/could-be-this-mod? % resource)
                               battle-mods))
                            (fn [f]
                              {:resource-filename (fs/filename f)}))
                           (fn [f]
                             (when-let [filename (fs/filename f)]
                               (when (string/ends-with? filename ".sdp")
                                 (let [id (first (string/split filename #"\."))]
                                   (contains? battle-rapid-hashes id)))))))
                         (concat priorities))
         _ (log/info "Prioritizing mods in battles" (pr-str priorities))
         this-round (concat priorities
                            (if (<= (count todo) mods-batch-size) ; add errors if there's not a full round left
                              (do
                                (log/info "Adding mod directories and mods with errors to scan")
                                (concat todo
                                        (map :file directory) ; re-scan directories just before errors
                                        error-files))
                              (take mods-batch-size todo)))
         all-paths (set (filter some? (map (comp fs/canonical-path :file) mods)))
         missing-files (set
                        (concat
                         (->> all-paths
                              (remove (comp fs/exists? io/file)))
                         (->> all-paths
                              (remove (comp (partial fs/descendant? spring-root) io/file)))))
         next-round
         (->> (drop mods-batch-size todo)
              (remove (comp all-paths fs/canonical-path))
              seq)]
     (apply fs/update-file-cache! state-atom all-paths)
     (when (seq this-round)
       (log/info "Adding" (count this-round) "mods this iteration:" (with-out-str (pprint this-round))))
     (doseq [file this-round]
       (log/info "Reading mod from" file)
       (update-mod state-atom spring-root-path file {:delete-invalid-sdp delete-invalid-sdp :use-git-mod-version use-git-mod-version}))
     (log/info "Removing mods with no name, and mods with missing files:" (pr-str missing-files))
     (let [[old-state new-state]
           (swap-vals! state-atom
                       (fn [state]
                         (update-in state [:by-spring-root spring-root-path :mods]
                                    (fn [mods]
                                      (->> mods
                                           (remove
                                            (fn [{:keys [error mod-name]}]
                                              (and (not error)
                                                   (string/blank? mod-name))))
                                           (filter (comp (partial instance? java.io.File) :file))
                                           (map
                                             (fn [{:keys [file path] :as m}]
                                               (if path
                                                 m
                                                 (assoc m :path (fs/canonical-path file)))))
                                           (remove (comp missing-files fs/canonical-path :file))
                                           set)))))]
       (log/info "Mod count in" spring-root-path "changed from"
                 (count (get-in old-state [:by-spring-root spring-root-path :mods]))
                 "to"
                 (count (get-in new-state [:by-spring-root spring-root-path :mods]))))
     (if next-round
       (do
         (log/info "Scheduling mod load since there are" (count next-round) "mods left to load")
         (task/add-task! state-atom
                         {:spring-lobby/task-type :spring-lobby/refresh-mods
                          :spring-root spring-root}))
                          ;:todo (count next-round)}))
       (log/info "Finished mod refresh"))
     {:to-add-file-count (count to-add-file)
      :to-add-rapid-count (count to-add-rapid)})))

(defn refresh-mods-all-spring-roots [state-atom]
  (let [spring-roots (fs/spring-roots @state-atom)]
    (log/info "Refreshing mods in" (pr-str spring-roots))
    (doseq [spring-root spring-roots]
      (refresh-mods state-atom spring-root))))

(defn parse-rapid-progress [line]
  (when (string/starts-with? line "[Progress]")
    (if-let [[_all _percent _bar current total] (re-find #"\[Progress\]\s+(\d+)%\s+\[([\s=]+)\]\s+(\d+)/(\d+)" line)]
      {:current (Long/parseLong current)
       :total (Long/parseLong total)}
      (log/warn "Unable to parse rapid progress" (pr-str line)))))

(defn- old-valid-replay-fn [all-paths-set]
  (fn [[path replay]]
    (and
     (contains? all-paths-set path) ; remove missing files
     (-> replay :header :game-id) ; re-parse if no game id
     (not (-> replay :file-size zero?)) ; remove empty files
     (not (-> replay :game-type #{:invalid})))))

(defn- valid-replay-fn [all-paths-set]
  (fn [[path replay]]
    (and
     (contains? all-paths-set path) ; remove missing files
     (:replay-id replay) ; re-parse if no replay id
     (not (-> replay :file-size zero?)) ; remove empty files
     (not (-> replay :game-type #{:invalid}))))) ; remove invalid

(defn migrate-replay [replay]
  (merge
   (select-keys replay [:file :filename :file-size :source-name])
   (fs.sdfz/replay-metadata replay)))

(def parsed-replays-config
  {:select-fn #(select-keys % [:invalid-replay-paths :parsed-replays-by-path])
   :filename "parsed-replays.edn"
   :nippy true})

(defn add-handlers [task-handler state-atom]
  (defmethod task-handler :spring-lobby/rapid-download
    [{:keys [engine-file rapid-id spring-isolation-dir]}]
    (swap! state-atom
           (fn [state]
             (-> state
                 (assoc-in [:rapid-download rapid-id] {:running true
                                                       :message "Preparing to run pr-downloader"})
                 (assoc-in [:rapid-failures rapid-id] false))))
    (try
      (let [^java.io.File root spring-isolation-dir
            pr-downloader-file (fs/pr-downloader-file engine-file)
            _ (fs/set-executable pr-downloader-file)
            command [(fs/canonical-path pr-downloader-file)
                     "--filesystem-writepath" (fs/wslpath root)
                     "--rapid-download" rapid-id]
            runtime (Runtime/getRuntime)
            detected-sdp-atom (atom nil)]
        (log/info "Running '" command "'")
        (let [^"[Ljava.lang.String;" cmdarray (into-array String command)
              ^"[Ljava.lang.String;" envp 
                (if (or (string/includes? rapid-id "Beyond All Reason") (string/includes? rapid-id "byar"))
                   (do
                      ;; 1. ensure rapid tag resolution order is set in springsettings file
                      (let [
                         config-file    (io/file (str (fs/wslpath root) "/springsettings.cfg"))
                         specific-line  "RapidTagResolutionOrder=repos-cdn.beyondallreason.dev"]
                      (if (.exists config-file)
                         (let [existing-lines (set (line-seq (io/reader config-file)))]
                            (when-not (contains? existing-lines specific-line)
                               (spit config-file (str specific-line "\n") :append true)))
                               ;; if the file does not exist, create it with the line
                               (spit config-file (str specific-line "\n"))))
                               ;; 2. Return the environment variables array.
                               (into-array String
                                 ["PRD_RAPID_USE_STREAMER=false"
                                 "PRD_RAPID_REPO_MASTER=https://repos-cdn.beyondallreason.dev/repos.gz"
                                 "PRD_HTTP_SEARCH_URL=https://files-cdn.beyondallreason.dev/find"]))
                               nil)
              ^java.lang.Process process (.exec runtime cmdarray envp root)]
          (future
            (with-open [^java.io.BufferedReader reader (io/reader (.getInputStream process))]
              (loop []
                (if-let [line (.readLine reader)]
                  (let [progress (try (parse-rapid-progress line)
                                      (catch Exception e
                                        (log/debug e "Error parsing rapid progress")))]
                    (swap! state-atom update-in [:rapid-download rapid-id] merge
                           {:message line}
                           progress)
                    (log/info "(pr-downloader" rapid-id "out)" line)
                    (recur))
                  (log/info "pr-downloader" rapid-id "stdout stream closed")))))
          (future
            (with-open [^java.io.BufferedReader reader (io/reader (.getErrorStream process))]
              (loop []
                (if-let [line (.readLine reader)]
                  (do
                    (swap! state-atom assoc-in [:rapid-download rapid-id :message] line)
                    (log/info "(pr-downloader" rapid-id "err)" line)
                    (when-let [[_all sdp] (re-find #" for ([0-9a-f]+)$" line)]
                      (reset! detected-sdp-atom sdp))
                    (recur))
                  (log/info "pr-downloader" rapid-id "stderr stream closed")))))
          (let [exit-code (.waitFor process)]
            (log/info "pr-downloader exited with code" exit-code)
            (when (not= 0 exit-code)
              (if-let [sdp @detected-sdp-atom]
                (let [sdp-file (rapid/sdp-file spring-isolation-dir (rapid/sdp-filename sdp))]
                  (log/info "Non-zero pr-downloader exit, deleting corrupt sdp file" sdp-file)
                  (raynes-fs/delete sdp-file))
                (log/info "No sdp file detected"))
              (log/info "Non-zero pr-downloader exit, deleting corrupt packages dir")
              (task/add-task! state-atom
                              {:spring-lobby/task-type :spring-lobby/delete-corrupt-rapid
                               :spring-root spring-isolation-dir})))))
                               ;:update-rapid-task task})))))
      (catch Exception e
        (log/error e "Error downloading" rapid-id)
        (swap! state-atom assoc-in [:rapid-download rapid-id :message] (.getMessage e))
        (log/info "Scheduling delete of corrupt rapid dir in" spring-isolation-dir)
        (task/add-task! state-atom
                        {:spring-lobby/task-type :spring-lobby/delete-corrupt-rapid
                         :spring-root spring-isolation-dir}))
                         ;:update-rapid-task task}))
      (finally
        (task/add-tasks! state-atom
                         [{:spring-lobby/task-type :spring-lobby/update-rapid-packages
                           :spring-isolation-dir spring-isolation-dir}
                          {:spring-lobby/task-type :spring-lobby/refresh-mods
                           :spring-root spring-isolation-dir}])
        (swap! state-atom assoc-in [:rapid-download rapid-id :running] false)
        (u/update-cooldown state-atom [:rapid rapid-id])
        (apply fs/update-file-cache! state-atom (rapid/sdp-files spring-isolation-dir)))))
  (defmethod task-handler :spring-lobby/update-rapid
    [{:keys [engine-version mod-name rapid-id rapid-repo spring-isolation-dir] :as task}]
    (let [before (u/curr-millis)
          {:keys [by-spring-root db file-cache use-db-for-rapid] :as state} @state-atom
          spring-isolation-dir (or spring-isolation-dir (:spring-isolation-dir state))
          spring-root-path (fs/canonical-path spring-isolation-dir)
          engines (get-in by-spring-root [spring-root-path :engines])
          engine-version (or engine-version (:engine-version state))
          preferred-engine-details (resource/engine-details engines engine-version)
          engine-details (if (and preferred-engine-details (:file preferred-engine-details))
                           preferred-engine-details
                           (->> engines
                                (filter (comp fs/canonical-path :file))
                                first))
          rapid-repo (or rapid-repo
                         (resource/update-mod-repo-name mod-name))
          rapid-id (or rapid-id
                       (str rapid-repo ":test"))]
      (if (and engine-details (:file engine-details))
        (do
          (log/info "Initializing rapid by calling download")
          (task/add-task! state-atom
            {:spring-lobby/task-type :spring-lobby/rapid-download
             :mod-name mod-name
             :rapid-id rapid-id
             :engine-file (:file engine-details)
             :spring-isolation-dir spring-isolation-dir}))
        (log/warn "No engine details to do rapid init"))
      (log/info "Updating rapid versions in" spring-isolation-dir)
      (let [rapid-repos (rapid/repos spring-isolation-dir)
            _ (log/info "Found" (count rapid-repos) "rapid repos")
            rapid-repo-files (map (partial rapid/version-file spring-isolation-dir) rapid-repos)
            new-files (->> rapid-repo-files
                           (map fs/file-cache-data)
                           fs/file-cache-by-path)
            needs-rapid-delete (atom false)
            rapid-versions (->> rapid-repo-files
                                (filter
                                 (fn [f]
                                   (let [path (fs/canonical-path f)
                                         prev-time (or (-> file-cache (get path) :last-modified) 0)
                                         curr-time (or (-> new-files (get path) :last-modified) Long/MAX_VALUE)]
                                     (or
                                      (< prev-time curr-time)
                                      (:force task)))))
                                (mapcat
                                 (fn [f]
                                   (try
                                     (rapid/rapid-versions f)
                                     (catch Exception e
                                       (log/error e "Error reading rapid versions in" f
                                                  "scheduling delete of rapid folder and another rapid update")
                                       (reset! needs-rapid-delete true)
                                       nil))))
                                (filter (comp string? :version))
                                (sort-by :version version/version-compare)
                                reverse)
            _ (log/info "Found" (count rapid-versions) "rapid versions")]
        (if (and db use-db-for-rapid)
          (let [_ (log/info "Using database for rapid" db)
                data (mapv
                      (fn [data]
                        (-> data
                            (select-keys [:id :hash :detail :version])
                            (clojure.set/rename-keys
                             {:id ::rapid/id
                              :hash ::rapid/hash
                              :detail ::rapid/detail
                              :version ::rapid/version})
                            (assoc ::rapid/spring-root spring-root-path)))
                      rapid-versions)
                before (u/curr-millis)]
            (rapid/update-rapid-data db spring-root-path data)
            (log/info "Wrote" (count rapid-versions) "to database in" (- (u/curr-millis) before) "ms")
            (swap! state-atom
                   (fn [state]
                     (-> state
                         (update-in [:rapid-by-spring-root spring-root-path]
                                    (fn [rapid]
                                      (-> rapid
                                          (assoc :rapid-updated (u/curr-millis))
                                          (assoc :rapid-repos rapid-repos))))
                         (update :file-cache merge new-files)
                         (update :rapid-by-spring-root
                                 (fn [rapid-by-spring-root]
                                   (reduce-kv
                                    (fn [m k v]
                                      (assoc m k (dissoc v :rapid-data-by-hash :rapid-data-by-id :rapid-data-by-version :rapid-versions)))
                                    {}
                                    rapid-by-spring-root)))))))
          (let [rapid-data-by-hash (->> rapid-versions
                                        (remove (comp #(string/ends-with? % ":test") :id))
                                        ; prevents duplicates, uses specific version
                                        (map (juxt :hash identity))
                                        (into {}))
                rapid-data-by-id (->> rapid-versions
                                      (map (juxt :id identity))
                                      (into {}))
                rapid-data-by-version (->> rapid-versions
                                           (remove (comp #(string/ends-with? % ":test") :id))
                                           ; prevents duplicates, uses specific version
                                           (map (juxt :version identity))
                                           (into {}))]
            (when (and mod-name
                       (not (get rapid-data-by-version mod-name)))
              (log/warn "Against all odds rapid update has not found version" mod-name
                        "delete the local repos")
              (reset! needs-rapid-delete true))
            (swap! state-atom
                   (fn [state]
                     (-> state
                         (update-in [:rapid-by-spring-root spring-root-path]
                                    (fn [rapid]
                                      (-> rapid
                                          (assoc :rapid-updated (u/curr-millis))
                                          (assoc :rapid-repos rapid-repos)
                                          (update :rapid-data-by-hash merge rapid-data-by-hash)
                                          (update :rapid-data-by-id merge rapid-data-by-id)
                                          (update :rapid-data-by-version merge rapid-data-by-version)
                                          (update :rapid-versions (fn [old-versions]
                                                                    (set
                                                                     (vals
                                                                      (merge
                                                                       (into {}
                                                                             (map (juxt :id identity) old-versions))
                                                                       (into {}
                                                                             (map (juxt :id identity) rapid-versions))))))))))
                         (update :file-cache merge new-files))))))
        (when @needs-rapid-delete
          (task/add-task! state-atom
                          {:spring-lobby/task-type :spring-lobby/delete-corrupt-rapid
                           :spring-root spring-isolation-dir
                           :update-rapid-task task}))
        (log/info "Updated rapid repo data in" (- (u/curr-millis) before) "ms"))
      (u/update-cooldown state-atom [:update-rapid spring-root-path])
      (task/add-tasks! state-atom
                       [{:spring-lobby/task-type :spring-lobby/update-rapid-packages
                         :spring-isolation-dir spring-isolation-dir}
                        {:spring-lobby/task-type :spring-lobby/refresh-mods}])))
  (defmethod task-handler :spring-lobby/update-rapid-packages
    [{:keys [spring-isolation-dir]}]
    (swap! state-atom assoc :rapid-update true)
    (let [{:keys [db rapid-by-spring-root use-db-for-rapid] :as state} @state-atom
          spring-isolation-dir (or spring-isolation-dir (:spring-isolation-dir state))
          spring-root-path (fs/canonical-path spring-isolation-dir)
          {:keys [rapid-data-by-hash]} (get rapid-by-spring-root spring-root-path)
          sdp-files (doall (rapid/sdp-files spring-isolation-dir))
          get-by-hash-fn (fn [rapid-hash]
                           (if (and db use-db-for-rapid)
                             (rapid/rapid-data-by-hash db spring-root-path rapid-hash)
                             (get rapid-data-by-hash rapid-hash)))
          packages (->> sdp-files
                        (filter some?)
                        (map
                         (fn [f]
                           (let [rapid-data
                                 (->> f
                                      rapid/sdp-hash
                                      get-by-hash-fn)]
                             {:id (->> rapid-data :id str)
                              :filename (-> f fs/filename str)
                              :version (->> rapid-data :version str)})))
                        (sort-by :version version/version-compare)
                        reverse
                        doall)]
      (swap! state-atom update-in [:rapid-by-spring-root spring-root-path]
             assoc
             :sdp-files sdp-files
             :rapid-packages packages
             :rapid-update false)
      (task/add-task! state-atom
                      {:spring-lobby/task-type :spring-lobby/refresh-mods
                       :spring-root spring-isolation-dir})))
  (defmethod task-handler :spring-lobby/delete-corrupt-rapid
    [{:keys [update-rapid-task spring-root]}]
    (log/info "Attempting to delete corrupt rapid dir after error, then updating rapid again")
    (try
      (fs/delete-rapid-dir spring-root)
      (task/add-task! state-atom update-rapid-task)
      (catch Exception e
        (log/error e "Error deleting corrupt rapid dir"))))
  (defmethod task-handler :spring-lobby/refresh-maps [{:keys [spring-root] :as opts}]
    (u/update-cooldown state-atom [:refresh-maps])
    (if spring-root
      (refresh-maps state-atom spring-root opts)
      (refresh-maps-all-spring-roots state-atom)))
  (defmethod task-handler :spring-lobby/refresh-mods [{:keys [spring-root] :as opts}]
    (if spring-root
      (refresh-mods state-atom spring-root opts)
      (refresh-mods-all-spring-roots state-atom)))
  (defmethod task-handler :spring-lobby/refresh-engines [{:keys [spring-root] :as opts}]
    (if spring-root
      (refresh-engines state-atom spring-root opts)
      (refresh-engines-all-spring-roots state-atom)))
  (defmethod task-handler :spring-lobby/update-cached-minimaps
    [{:keys [maps priorities]}]
    (let [todo-maps (or maps
                        (let [{:keys [by-spring-root]} @state-atom]
                          (mapcat :maps (vals by-spring-root))))]
      (log/info "Found" (count todo-maps) "maps to update cached minimaps for")
      (update-cached-minimaps state-atom todo-maps {:priorities priorities})))
  (defmethod task-handler :spring-lobby/map-details [{:keys [map-name map-file tries] :as map-data}]
    (let [new-tries ((fnil inc 0) tries)
          error-data {:error true
                      :file map-file
                      :map-name map-name
                      :tries new-tries}
          cache-key (fs/canonical-path map-file)]
      (when-not cache-key
        (throw (ex-info "No cache key for" {:map-name map-name :map-file map-file})))
      (try
        (if map-file
          (do
            (log/info "Updating battle map details for" map-name)
            (let [map-details (read-map-details map-data)
                  map-details (if (or (not map-details) (:error map-details))
                                error-data
                                map-details)]
              (log/info "Got map details for" map-name map-file (keys map-details))
              (swap! state-atom update :map-details cache/miss cache-key map-details)))
          (do
            (log/info "Map not found, setting empty details for" map-name)
            (swap! state-atom update :map-details cache/miss cache-key {:tries new-tries})))
        (catch Throwable t
          (log/error t "Error updating map details")
          (swap! state-atom update :map-details cache/miss cache-key error-data)
          (throw t)))
      (update-battle-sync-statuses state-atom)))
  (defmethod task-handler :spring-lobby/mod-details
    [{:keys [mod-name mod-file use-git-mod-version]}]
    (let [error-data {:mod-name mod-name
                      :file mod-file
                      :error true
                      :tries 1} ; TODO inc
          cache-key (fs/canonical-path mod-file)]
      (when-not cache-key
        (throw (ex-info "No cache key for" {:mod-name mod-name :mod-file mod-file})))
      (try
        (if mod-file
          (do
            (log/info "Updating mod details for" mod-name)
            (let [mod-details (or (read-mod-data mod-file {:use-git-mod-version use-git-mod-version}) error-data)]
              (log/info "Got mod details for" mod-name mod-file (keys mod-details))
              (let [state (swap! state-atom update :mod-details cache/miss cache-key mod-details)]
                (update-preferred-faction state-atom state mod-name mod-details))
              (when (:error mod-details)
                (task/add-task! state-atom
                                {:spring-lobby/task-type :spring-lobby/refresh-mods}))))
          (do
            (log/info "Battle mod not found, setting empty details for" mod-name)
            (swap! state-atom update :mod-details cache/miss cache-key {})))
        (catch Throwable t
          (log/error t "Error updating mod details")
          (swap! state-atom update :mod-details cache/miss cache-key error-data)
          (throw t)))
      (update-battle-sync-statuses state-atom)))
  (defmethod task-handler :spring-lobby/delete-corrupt-map-file
    [{:keys [indexed-map spring-root]}]
    (let [{:keys [file map-name]} indexed-map]
      (log/info "Attempting to delete corrupt map" map-name "at" file)
      (raynes-fs/delete file)
      (fs/update-file-cache! state-atom file)
      (task/add-task! state-atom
                      {:spring-lobby/task-type :spring-lobby/refresh-maps
                       :spring-root spring-root})))
  (defmethod task-handler :spring-lobby/delete-corrupt-mod-file
    [{:keys [indexed-mod spring-root]}]
    (let [{:keys [file mod-name]} indexed-mod]
      (log/info "Attempting to delete corrupt mod" mod-name "at" file)
      (raynes-fs/delete file)
      (fs/update-file-cache! state-atom file)
      (task/add-task! state-atom
                      {:spring-lobby/task-type :spring-lobby/refresh-mods
                       :spring-root spring-root})))
  (defmethod task-handler :spring-lobby/refresh-replays [_]
    (log/info "Refreshing replays")
    (let [before (u/curr-millis)
          {:keys [db parsed-replays-by-path replay-sources-enabled use-db-for-replays] :as state} @state-atom
          all-files (mapcat
                      (fn [{:keys [file recursive replay-source-name]}]
                        (let [files (fs/replay-files file {:recursive recursive})]
                          (log/info "Found" (count files) "replay files from" replay-source-name "at" file)
                          (map
                            (juxt (constantly replay-source-name) identity)
                            files)))
                      (filter
                        (fn [{:keys [file path]}]
                          (let [path (or path (fs/canonical-path file))]
                            (or (not (contains? replay-sources-enabled path))
                                (get replay-sources-enabled path))))
                        (fs/replay-sources state)))
          all-paths (set (map (comp fs/canonical-path second) all-files))
          old-valid-replay? (old-valid-replay-fn all-paths)
          valid-replay? (valid-replay-fn all-paths)
          existing-valid-paths (->> parsed-replays-by-path
                                    (filter valid-replay?)
                                    keys
                                    set)
          todo (->> all-files
                    (remove (comp existing-valid-paths fs/canonical-path second))
                    shuffle)
          this-round (take replays-batch-size todo)
          _ (log/info "Parsing replays" (with-out-str (pprint this-round)))
          parsed-replays-by-path (->> this-round
                                      (map
                                       (fn [[source f]]
                                         [(fs/canonical-path f)
                                          (merge
                                           (fs.sdfz/parse-replay f)
                                           {:source-name source})]))
                                      doall)]
      (log/info "Parsed" (count this-round) "of" (count todo) "new replays in" (- (u/curr-millis) before) "ms")
      (when (and db use-db-for-replays)
        (replay/update-replays db (map second parsed-replays-by-path)))
      (let [new-state
            (if (and db use-db-for-replays)
              (let [all-replays (replay/all-replays db)
                    invalid-replay-paths (->> all-replays
                                              (map (juxt (comp fs/canonical-path :file) identity))
                                              (remove valid-replay?)
                                              (map first)
                                              set)]
                (swap! state-atom
                  (fn [state]
                    (-> state
                        (dissoc :parsed-replays-by-path nil)
                        (assoc :invalid-replay-paths invalid-replay-paths)))))
              (swap! state-atom
                     (fn [state]
                       (let [old-replays (:parsed-replays-by-path state)
                             replays-by-path (if (map? old-replays) old-replays {})
                             all-replays (into {} (concat replays-by-path parsed-replays-by-path))
                             valid-replays (->> all-replays
                                                (filter valid-replay?)
                                                (map
                                                  (fn [[k {:keys [path] :as replay}]]
                                                    [k (if path
                                                         replay
                                                         (assoc replay :path k))]))
                                                (into {}))
                             migratable-replays (->> all-replays
                                                     (remove valid-replay?)
                                                     (filter old-valid-replay?))
                             _ (log/info "Migrating" (count migratable-replays) "replays")
                             migrated-replays (->> migratable-replays
                                                   (map (fn [[path replay]] [path (migrate-replay replay)]))
                                                   (into {}))
                             _ (log/info "Migrated" (count migratable-replays) "replays")
                             replays-by-path (merge valid-replays migrated-replays)
                             valid-replay-paths (set (concat (keys replays-by-path)))
                             invalid-replay-paths (->> all-replays
                                                       (remove (some-fn old-valid-replay? valid-replay?))
                                                       keys
                                                       (concat (:invalid-replay-paths state))
                                                       (remove valid-replay-paths)
                                                       set)]
                         (assoc state
                                :parsed-replays-by-path replays-by-path
                                :invalid-replay-paths invalid-replay-paths)))))
            invalid-replay-paths (set (:invalid-replay-paths new-state))
            existing-valid-paths (->> new-state
                                      :parsed-replays-by-path
                                      (filter valid-replay?)
                                      keys
                                      set)
            valid-next-round (->> all-files
                                  (remove (comp existing-valid-paths fs/canonical-path second))
                                  (remove (comp invalid-replay-paths fs/canonical-path second)))]
        (let [nonzero-size-invalid-replay-files (->> invalid-replay-paths
                                                     (map fs/file)
                                                     (filter fs/exists?)
                                                     (filterv (comp pos? fs/size)))
              invalid-replays-folder (fs/file (fs/app-root) "invalid-replays")]
          (when (seq nonzero-size-invalid-replay-files)
            (fs/make-dirs invalid-replays-folder)
            (log/info "Moving" (count nonzero-size-invalid-replay-files) "invalid replay files to" invalid-replays-folder)
            (doseq [path nonzero-size-invalid-replay-files]
              (let [source (fs/file path)]
                (fs/move source (fs/file invalid-replays-folder (fs/filename source)))))))
        (cond
          (empty? valid-next-round)
          (do
            (log/info "No valid replays left to parse")
            (fs/spit-app-edn
             ((:select-fn parsed-replays-config) new-state)
             (:filename parsed-replays-config)
             parsed-replays-config)
            (task/add-task! state-atom {:spring-lobby/task-type :spring-lobby/refresh-replay-resources}))
          (= (set todo)
             (set valid-next-round))
          (log/warn "Infinite loop detected," (count todo) "todo and" (count valid-next-round)
                    "valid, aborting refresh-replays")
          :else
          (task/add-task! state-atom
                          {:spring-lobby/task-type :spring-lobby/refresh-replays})))))
                           ;:todo (count todo)})))))
  (defmethod task-handler :spring-lobby/refresh-replay-resources [_]
    [state-atom]
    (log/info "Refresh replay resources")
    (let [before (u/curr-millis)
          {:keys [db downloadables-by-url importables-by-path parsed-replays-by-path use-db-for-replays]} @state-atom
          parsed-replays (if (and db use-db-for-replays)
                           (replay/all-replays db)
                           (vals parsed-replays-by-path))
          engine-versions (->> parsed-replays
                               (map :replay-engine-version)
                               (filter some?)
                               set)
          mod-names (->> parsed-replays
                         (map :replay-mod-name)
                         set)
          map-names (->> parsed-replays
                         (map :replay-map-name)
                         set)
          downloads (vals downloadables-by-url)
          imports (vals importables-by-path)
          engine-downloads (filter (comp #{:spring-lobby/engine} :resource-type) downloads)
          replay-engine-downloads (->> engine-versions
                                       (map
                                        (fn [engine-version]
                                          (when-let [imp (->> engine-downloads
                                                              (filter (partial resource/could-be-this-engine? engine-version))
                                                              first)]
                                            [engine-version imp])))
                                       (into {}))
          mod-imports (filter (comp #{:spring-lobby/mod} :resource-type) imports)
          replay-mod-imports (->> mod-names
                                  (map
                                   (fn [mod-name]
                                     (when-let [imp (->> mod-imports
                                                         (filter (partial resource/could-be-this-mod? mod-name))
                                                         first)]
                                       [mod-name imp])))
                                  (into {}))
          mod-downloads (filter (comp #{:spring-lobby/mod} :resource-type) downloads)
          replay-mod-downloads (->> mod-names
                                    (map
                                     (fn [mod-name]
                                       (when-let [dl (->> mod-downloads
                                                          (filter (partial resource/could-be-this-mod? mod-name))
                                                          first)]
                                         [mod-name dl])))
                                    (into {}))
          map-imports (filter (comp #{:spring-lobby/map} :resource-type) imports)
          replay-map-imports (->> map-names
                                  (map
                                   (fn [map-name]
                                     (when-let [imp (->> map-imports
                                                         (filter (partial resource/could-be-this-map? map-name))
                                                         first)]
                                       [map-name imp])))
                                  (into {}))
          map-downloads (filter (comp #{:spring-lobby/map} :resource-type) downloads)
          replay-map-downloads (->> map-names
                                    (map
                                     (fn [map-name]
                                       (when-let [dl (->> map-downloads
                                                          (filter (partial resource/could-be-this-map? map-name))
                                                          first)]
                                         [map-name dl])))
                                    (into {}))]
      (log/info "Refreshed replay resources in" (- (u/curr-millis) before) "ms")
      (swap! state-atom assoc
             :replay-downloads-by-engine replay-engine-downloads
             :replay-downloads-by-mod replay-mod-downloads
             :replay-imports-by-mod replay-mod-imports
             :replay-downloads-by-map replay-map-downloads
             :replay-imports-by-map replay-map-imports)))
  (defmethod task-handler :spring-lobby/update-downloadables
    [source]
    (download/update-download-source state-atom source))
  (defmethod task-handler :spring-lobby/scan-imports
    [task]
    (import/scan-imports state-atom task))
  (defmethod task-handler :spring-lobby/extract-7z
    [{:keys [file dest]}]
    (fs/update-file-cache! state-atom file dest)
    (let [dest (if dest
                 (fs/extract-7z-fast file dest)
                 (fs/extract-7z-fast file))]
      (fs/update-file-cache! state-atom file dest))
    (task/add-task! state-atom {:spring-lobby/task-type :spring-lobby/refresh-engines}))
  (defmethod task-handler :spring-lobby/search-springfiles
    [{:keys [download-if-found springname] :or {download-if-found true} :as e}]
    (if-not (string/blank? springname)
      (let [search-result (download/search-springfiles e)]
        (log/info "Found details for" springname "on springfiles" search-result)
        (swap! state-atom assoc-in [:springfiles-search-results springname] search-result)
        (when (and search-result download-if-found)
          (task/add-task! state-atom
            (assoc e
                   :spring-lobby/task-type :spring-lobby/download-springfiles
                   :search-result search-result)))
        search-result)
      (log/warn "No springname to search springfiles" e)))
  (defmethod task-handler :spring-lobby/download-springfiles
    [{:keys [resource-type search-result springname spring-isolation-dir url]}]
    (if-let [{:keys [filename] :as search-result} (or search-result
                                                      (do
                                                        (task/add-task! state-atom
                                                          {:spring-lobby/task-type :spring-lobby/search-springfiles
                                                           :springname springname})
                                                        nil))]
      (let [url (or url
                    (http/springfiles-url search-result))]
        (task/add-task! state-atom
          {:spring-lobby/task-type :spring-lobby/http-downloadable
           :downloadable {:download-url url
                          :resource-filename filename
                          :resource-type resource-type}
           :springname springname
           :spring-isolation-dir spring-isolation-dir}))
      (log/info "No mirror to download" springname "on springfiles")))
  (defmethod task-handler :spring-lobby/http-downloadable
    [task]
    @(download/download-http-resource state-atom task))
  (defmethod task-handler :spring-lobby/download-and-extract
    [{:keys [downloadable spring-isolation-dir] :as task}]
    @(download/download-http-resource state-atom task)
    (let [download-file (fs/file (fs/download-dir) "engine" (:resource-filename downloadable))
          extract-file (when download-file
                         (io/file spring-isolation-dir "engine" (fs/filename download-file)))
          _ (fs/update-file-cache! state-atom download-file extract-file)
          dest (fs/extract-7z-fast download-file extract-file)]
      (fs/update-file-cache! state-atom download-file dest)
      (task/add-task! state-atom {:spring-lobby/task-type :spring-lobby/refresh-engines})))
  (defmethod task-handler :spring-lobby/import [e]
    (import/import-resource state-atom e))
  (defmethod task-handler :spring-lobby/scan-all-imports
    [{:keys [sources] :or {sources (import/import-sources (:extra-import-sources @state-atom))}}]
    (doseq [import-source sources]
      (task/add-task! state-atom
        (merge
          {:spring-lobby/task-type :spring-lobby/scan-imports}
          import-source))))
  (defmethod task-handler :spring-lobby/update-all-downloadables
    [opts]
    (doseq [download-source http/download-sources]
      (task/add-task! state-atom
        (merge
          {:spring-lobby/task-type :spring-lobby/update-downloadables
           :force (:force opts)}
          download-source))))
  (defmethod task-handler :spring-lobby/clear-map-and-mod-details
    [{:keys [map-resource mod-resource spring-root]}]
    (let [map-key (resource/details-cache-key map-resource)
          mod-key (resource/details-cache-key mod-resource)
          {:keys [use-git-mod-version]}
          (swap! state-atom
            (fn [state]
              (cond-> state
                      map-key
                      (update :map-details cache/miss map-key nil)
                      mod-key
                      (update :mod-details cache/miss mod-key nil))))]
      (task/add-tasks! state-atom
        (concat
          [{:spring-lobby/task-type :spring-lobby/refresh-engines
            :force true
            :spring-root spring-root}
           {:spring-lobby/task-type :spring-lobby/refresh-mods
            :spring-root spring-root
            :priorities [(:file mod-resource)]}
           {:spring-lobby/task-type :spring-lobby/refresh-maps
            :spring-root spring-root
            :priorities [(:file map-resource)]}]
          (when-let [mod-file (:file mod-resource)]
            [{:spring-lobby/task-type :spring-lobby/mod-details
              :mod-name (:mod-name mod-resource)
              :mod-file mod-file
              :use-git-mod-version use-git-mod-version}])
          (when-let [map-file (:file map-resource)]
            [{:spring-lobby/task-type :spring-lobby/map-details
              :map-name (:map-name map-resource)
              :map-file map-file}])))))
  (defmethod task-handler :spring-lobby/clear-map-details
    [{:keys [map-resource spring-root]}]
    (let [map-key (resource/details-cache-key map-resource)]
      (swap! state-atom
        (fn [state]
          (cond-> state
                  map-key
                  (update :map-details cache/miss map-key nil))))
      (task/add-tasks! state-atom
        (concat
          [
           {:spring-lobby/task-type :spring-lobby/refresh-maps
            :spring-root spring-root
            :priorities [(:file map-resource)]}]
          (when-let [map-file (:file map-resource)]
            [{:spring-lobby/task-type :spring-lobby/map-details
              :map-name (:map-name map-resource)
              :map-file map-file}])))))
  (defmethod task-handler :spring-lobby/clear-mod-details
    [{:keys [mod-resource spring-root]}]
    (let [
          mod-key (resource/details-cache-key mod-resource)
          {:keys [use-git-mod-version]}
          (swap! state-atom
            (fn [state]
              (cond-> state
                      mod-key
                      (update :mod-details cache/miss mod-key nil))))]
      (task/add-tasks! state-atom
        (concat
          [
           {:spring-lobby/task-type :spring-lobby/refresh-mods
            :spring-root spring-root
            :priorities [(:file mod-resource)]}]
          (when-let [mod-file (:file mod-resource)]
            [{:spring-lobby/task-type :spring-lobby/mod-details
              :mod-name (:mod-name mod-resource)
              :mod-file mod-file
              :use-git-mod-version use-git-mod-version}]))))))
