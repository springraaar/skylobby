(ns skylobby.fx.mod-sync
  (:require
    [cljfx.api :as fx]
    [clojure.string :as string]
    [skylobby.fs :as fs]
    skylobby.fx
    [skylobby.fx.sub :as sub]
    [skylobby.fx.sync :refer [sync-pane]]
    [skylobby.http :as http]
    [skylobby.rapid :as rapid]
    [skylobby.resource :as resource]
    [skylobby.util :as u]
    [taoensso.tufte :as tufte]))


(set! *warn-on-reflection* true)


(defn mod-download-source [mod-name]
  (cond
    (string/blank? mod-name) nil
    (string/includes? mod-name "Total Atomization Prime") "TAP GitHub releases"
    (string/includes? mod-name "Total Atomic Power") "TAP GitHub releases"
    (string/includes? mod-name "Evolution RTS") "Evolution-RTS GitHub releases"
    (string/includes? mod-name "Balanced Annihilation") "Balanced Annihilation GitHub releases"
    :else nil))


(defn get-rapid-data-by-version-sub [context spring-root-path]
  (let [
        use-db-for-rapid (fx/sub-val context :use-db-for-rapid)
        db (fx/sub-val context :db)
        rapid-data-by-version (fx/sub-val context get-in [:rapid-by-spring-root spring-root-path :rapid-data-by-version])
        get-rapid-data-by-version (fn [version]
                                    (if (and db use-db-for-rapid)
                                      (rapid/rapid-data-by-version db spring-root-path version)
                                      (get rapid-data-by-version version)))]
    get-rapid-data-by-version))

(defn update-download-sources-sub [context]
  (let [
        update-download-sources (->> (fx/sub-ctx context skylobby.fx/tasks-of-type-sub :spring-lobby/update-downloadables)
                                     (map :download-source-name)
                                     set)]
    update-download-sources))

(defn mod-update-tasks-sub [context]
  (let [
        refresh-mods-tasks (fx/sub-ctx context skylobby.fx/tasks-of-type-sub :spring-lobby/refresh-mods)
        mod-details-tasks (fx/sub-ctx context skylobby.fx/tasks-of-type-sub :spring-lobby/mod-details)
        mod-update-tasks (concat refresh-mods-tasks mod-details-tasks)]
    mod-update-tasks))

(defn rapid-tasks-by-id-sub [context spring-root-path]
  (let [
        rapid-tasks-by-id (->> (concat
                                 (fx/sub-ctx context skylobby.fx/tasks-of-type-sub :spring-lobby/update-rapid)
                                 (fx/sub-ctx context skylobby.fx/tasks-of-type-sub :spring-lobby/rapid-download))
                               (filter (comp #{spring-root-path} fs/canonical-path :spring-isolation-dir))
                               (map (juxt :rapid-id identity))
                               (into {}))]
    rapid-tasks-by-id))

(defn download-tasks-by-url-sub [context]
  (let [
        tasks-by-type (fx/sub-ctx context skylobby.fx/tasks-by-type-sub)
        download-tasks-by-url (->> (get tasks-by-type :spring-lobby/http-downloadable)
                                   (map (juxt (comp :download-url :downloadable) identity))
                                   (into {}))]
    download-tasks-by-url))

(defn mod-sync-pane-impl
  [{:fx/keys [context]
    :keys [dependency engine-version index-only mod-name spring-isolation-dir]}]
  (let [
        copying (fx/sub-val context :copying)
        file-cache (fx/sub-val context :file-cache)
        http-download (fx/sub-val context :http-download)
        {:keys [spring-root-path]} (fx/sub-ctx context sub/spring-resources spring-isolation-dir)
        get-rapid-data-by-version (fx/sub-ctx context get-rapid-data-by-version-sub spring-root-path)
        rapid-downloads-by-id (fx/sub-val context :rapid-download)
        springfiles-search-results (fx/sub-val context :springfiles-search-results)
        tasks-by-type (fx/sub-ctx context skylobby.fx/tasks-by-type-sub)
        indexed-mod (fx/sub-ctx context sub/indexed-mod spring-isolation-dir mod-name)
        exact-version-match (= mod-name (:mod-name indexed-mod))
        is-unversioned (and mod-name
                            (string/ends-with? mod-name "$VERSION"))
        mod-details (fx/sub-ctx context skylobby.fx/mod-details-sub indexed-mod)
        no-mod-details (not (resource/details? mod-details))
        refresh-mods-tasks (fx/sub-ctx context skylobby.fx/tasks-of-type-sub :spring-lobby/refresh-mods)
        update-download-sources (fx/sub-ctx context update-download-sources-sub)
        mod-update-tasks (fx/sub-ctx context mod-update-tasks-sub)
        rapid-tasks-by-id (fx/sub-ctx context rapid-tasks-by-id-sub spring-root-path)
        mod-file (or (:file mod-details) (:file indexed-mod))
        mod-path (or (:path mod-details) (:path indexed-mod))
        engine-details (fx/sub-ctx context sub/indexed-engine spring-isolation-dir engine-version)
        engine-file (:file engine-details)
        download-tasks-by-url (fx/sub-ctx context download-tasks-by-url-sub)
        refresh-in-progress (seq refresh-mods-tasks)]
    {:fx/type sync-pane
     :h-box/margin 8
     :resource "Game"
     :browse-action {:event/type :spring-lobby/desktop-browse-dir
                     :file (fs/mods-dir spring-isolation-dir)}
     :in-progress refresh-in-progress
     :issues
     (concat
       (let [severity (if no-mod-details
                        (if indexed-mod
                          (if (or dependency index-only)
                            0
                            (if (not= mod-name (:mod-name indexed-mod))
                              1
                              -1))
                          2)
                        0)]
         [{:severity severity
           :text "info"
           :human-text (if mod-name
                         mod-name
                         "No game version specified")
           :tooltip (if (zero? severity)
                      mod-path
                      (if indexed-mod
                        (if exact-version-match
                          (str "Loading mod details for '" mod-name "' at " (:file indexed-mod))
                          "Cannot determine exact version match")
                        (if mod-name
                          (str "Game '" mod-name "' not found locally")
                          "No game version specified")))}])
       (when (and indexed-mod (not exact-version-match))
         [{:severity 1
           :text "mismatch"
           :human-text (str "Cannot match version, using " (:mod-name indexed-mod))}])
       (when (and no-mod-details (not indexed-mod))
         (let [
               downloadable (fx/sub-ctx context sub/could-be-this-mod-download mod-name)]
           (concat
             (let [
                   download-url (:download-url downloadable)
                   download (get http-download download-url)
                   possible-source-name (mod-download-source mod-name)
                   in-progress (if downloadable
                                 (or (:running download)
                                     (contains? download-tasks-by-url download-url))
                                 (get update-download-sources possible-source-name))
                   {:keys [download-source-name download-url]} downloadable
                   dest (resource/resource-dest spring-isolation-dir downloadable)
                   dest-path (fs/canonical-path dest)
                   dest-exists (fs/file-exists? file-cache (resource/resource-dest spring-isolation-dir downloadable))]
               (when (or downloadable possible-source-name)
                 [{:severity (if dest-exists -1 2)
                   :text "download"
                   :human-text (if in-progress
                                 (if downloadable
                                   (str "Downloading " (u/download-progress download))
                                   (str "Refreshing " possible-source-name))
                                 (if no-mod-details
                                   (if downloadable
                                     (str "Download from " download-source-name)
                                     (if possible-source-name
                                       (str "Update download source " possible-source-name)
                                       (str "No download for " mod-name)))
                                   (:mod-name mod-details)))
                   :in-progress in-progress
                   :tooltip (if in-progress
                              (if downloadable
                                (str "Downloading " (u/download-progress download))
                                (str "Refreshing " possible-source-name))
                              (if dest-exists
                                (str "Downloaded to " dest-path)
                                (if downloadable
                                  (str "Download from " download-source-name " at " download-url)
                                  (if possible-source-name
                                    (str "Update download source " possible-source-name)
                                    (str "No http download found for " mod-name)))))
                   :action
                   (when-not dest-exists
                     (if downloadable
                       {:event/type :spring-lobby/add-task
                        :task
                        {:spring-lobby/task-type :spring-lobby/http-downloadable
                         :downloadable downloadable
                         :spring-isolation-dir spring-isolation-dir}}
                       {:event/type :spring-lobby/add-task
                        :task
                        (merge
                          {:spring-lobby/task-type :spring-lobby/update-downloadables
                           :force true}
                          (get http/download-sources-by-name possible-source-name))}))}]))
             (let [
                   springname mod-name
                   springfiles-searched (contains? springfiles-search-results springname)
                   springfiles-search-result (get springfiles-search-results springname)
                   springfiles-mirror-set (set (:mirrors springfiles-search-result))
                   springfiles-download (->> http-download
                                             (filter (comp springfiles-mirror-set first))
                                             first
                                             second)
                   springfiles-in-progress (or (:running springfiles-download)
                                               (->> download-tasks-by-url
                                                    keys
                                                    (filter springfiles-mirror-set)
                                                    seq))]
               (when (and springname (not (some #(re-find % springname) resource/no-springfiles)))
                 [{:severity 2
                   :text "springfiles"
                   :human-text (if springfiles-in-progress
                                 (str "Downloading " (u/download-progress springfiles-download))
                                 (if springfiles-searched
                                   (if springfiles-search-result
                                     "Download from springfiles"
                                     "Not found on springfiles")
                                   "Search springfiles and download"))
                   :in-progress springfiles-in-progress
                   :action
                   (when (or (not springfiles-searched) springfiles-search-result)
                     {:event/type :spring-lobby/add-task
                      :task
                      (if springfiles-searched
                        {:spring-lobby/task-type :spring-lobby/download-springfiles
                         :resource-type :spring-lobby/mod
                         :search-result springfiles-search-result
                         :springname springname
                         :spring-isolation-dir spring-isolation-dir}
                        {:spring-lobby/task-type :spring-lobby/search-springfiles
                         :springname springname
                         :resource-type :spring-lobby/mod
                         :spring-isolation-dir spring-isolation-dir})})}]))
             (when (and mod-name
                        (not is-unversioned)
                        (not (some #(re-find % mod-name) resource/no-rapid)))
               (let [rapid-data (get-rapid-data-by-version mod-name)
                     rapid-repo (resource/mod-repo-name mod-name)
                     default-rapid-id (when rapid-repo (str rapid-repo ":test"))
                     rapid-id (or (:id rapid-data) default-rapid-id)
                     rapid-download (get rapid-downloads-by-id rapid-id)
                     running (some? (or (get rapid-tasks-by-id rapid-id)
                                        (get rapid-tasks-by-id mod-name)))
                     sdp-file (rapid/sdp-file spring-isolation-dir (str (:hash rapid-data) ".sdp"))
                     package-exists (fs/file-exists? file-cache sdp-file)
                     rapid-update-tasks (->> tasks-by-type
                                             (filter (comp #{:spring-lobby/update-rapid-packages :spring-lobby/update-rapid} first))
                                             (mapcat second)
                                             (filter (comp #{spring-root-path} fs/canonical-path :spring-isolation-dir))
                                             seq)
                     rapid-update-id default-rapid-id
                     rapid-update-download (get rapid-downloads-by-id rapid-update-id)
                     in-progress (or running
                                     rapid-update-tasks
                                     (and package-exists
                                          (seq mod-update-tasks)))]
                 (mapv
                   (fn [id]
                     {:severity 2
                      :text "rapid"
                      :human-text
                                  (if engine-file
                                    (if rapid-update-tasks
                                      (if rapid-update-download
                                        (str "Downloading " (u/download-progress rapid-update-download))
                                        "Updating rapid packages...")
                                      (if id
                                        (if in-progress
                                          (str "Downloading " (u/download-progress rapid-download))
                                          (str "Download rapid " id))
                                        "No rapid download found, update packages"))
                                    "Needs engine first to download with rapid")
                      :tooltip (if id
                                 (if engine-file
                                   (if package-exists
                                     (str sdp-file)
                                     (str "Use rapid downloader to get resource id " id
                                          " using engine " (:engine-version engine-details)))
                                   "Rapid requires an engine to work, get engine first")
                                 (str "No rapid download found for " mod-name))
                      :in-progress in-progress
                      :action
                      (cond
                        (not engine-file) nil
                        (and id engine-file)
                        {:event/type :spring-lobby/add-task
                         :task
                         {:spring-lobby/task-type :spring-lobby/rapid-download
                          :mod-name mod-name
                          :rapid-id id
                          :engine-file engine-file
                          :spring-isolation-dir spring-isolation-dir}}
                        package-exists
                        {:event/type :spring-lobby/add-task
                         :task
                         {:spring-lobby/task-type :spring-lobby/update-file-cache
                          :file sdp-file}}
                        :else
                        {:event/type :spring-lobby/add-task
                         :task {:spring-lobby/task-type :spring-lobby/update-rapid
                                :engine-version (:engine-version engine-details)
                                :force true
                                :mod-name mod-name
                                :spring-isolation-dir spring-isolation-dir}})})
                   ; omit the rapid-id or ":test" option because redundant and also wasn't greening up after completion, for some reason
                   ;[rapid-id mod-name]
                   [mod-name]
                   )))
             (let [
                   importable (fx/sub-ctx context sub/could-be-this-mod-import mod-name)
                   resource-file (:resource-file importable)
                   dest (resource/resource-dest spring-isolation-dir importable)
                   dest-exists (fs/file-exists? file-cache dest)]
               (when importable
                 [{:severity (if dest-exists -1 2)
                   :text "import"
                   :human-text (if importable
                                 (str "Import from " (:import-source-name importable))
                                 "No import found")
                   :tooltip (if importable
                              (str "Copy game from " (:import-source-name importable)
                                   " at " resource-file)
                              (str "No local import found for " mod-name))
                   :in-progress (get copying resource-file)
                   :action (when importable
                             {:event/type :spring-lobby/add-task
                              :task
                              {:spring-lobby/task-type :spring-lobby/import
                               :importable importable
                               :spring-isolation-dir spring-isolation-dir}})}]))
            (when refresh-in-progress
              [{:severity -1
                :text "refresh"
                :human-text "Refreshing games"
                :tooltip "Refreshing games"
                :in-progress true}]))))
       (when (= :directory
                (::fs/source indexed-mod))
         (let [
               battle-mod-git-ref (u/mod-git-ref mod-name)
               severity (if (= mod-name
                               (:mod-name indexed-mod))
                          0 1)
               in-progress (->> (get tasks-by-type :spring-lobby/git-mod)
                                (filter (comp #{mod-path} fs/canonical-path :file))
                                seq
                                boolean)]
           (concat
             [{:severity severity
               :human-text (str "You have " (:mod-name indexed-mod))}
              (merge
                {:severity severity
                 :text "git"
                 :in-progress (or in-progress (seq mod-update-tasks))}
                (if (= battle-mod-git-ref "$VERSION")
                  ; unspecified git commit ^
                  {:human-text (str "Unspecified git ref " battle-mod-git-ref)
                   :tooltip (str "SpringLobby does not specify version, "
                                 "yours may not be compatible")}
                  {:human-text (if (zero? severity)
                                 (str "git at ref " battle-mod-git-ref)
                                 (if in-progress
                                   (str "Resetting " (fs/filename (:file mod-details))
                                        " git to ref " battle-mod-git-ref)
                                   (if (seq mod-update-tasks)
                                     "Refreshing mods..."
                                     (str "Reset " (fs/filename (:file mod-details))
                                          " git to ref " battle-mod-git-ref))))
                   :action
                   {:event/type :spring-lobby/add-task
                    :task
                    {:spring-lobby/task-type :spring-lobby/git-mod
                     :file mod-file
                     :battle-mod-git-ref battle-mod-git-ref
                     :spring-root spring-isolation-dir}}}))]
             (when (and (not (zero? severity))
                        (not= battle-mod-git-ref "$VERSION"))
               [(merge
                  {:severity 1
                   :text "rehost"
                   :human-text "Or rehost to change game version"
                   :tooltip (str "Leave battle and host again to use game "
                                 (:mod-name mod-details))})])))))}))

(defn mod-sync-pane [state]
  (tufte/profile {:dynamic? true
                  :id :skylobby/ui}
    (tufte/p :mod-sync-pane
      (mod-sync-pane-impl state))))


(defn mod-and-deps-sync-pane
  [{:fx/keys [context]
    :keys [mod-name spring-isolation-dir]
    :as props}]
  (let [
        indexed-mod (fx/sub-ctx context sub/indexed-mod spring-isolation-dir mod-name)
        mod-details (fx/sub-ctx context skylobby.fx/mod-details-sub indexed-mod)
        mod-dependencies (vals (get-in mod-details [:modinfo :depend]))]
    {:fx/type :v-box
     :children
     (concat
       [(merge
          {:fx/type mod-sync-pane}
          props)]
       (mapv
         (fn [mod-name]
           (merge
             props
             {:fx/type mod-sync-pane
              :dependency true
              :mod-name mod-name}))
         mod-dependencies))}))
