(ns skylobby.fx.sub
  (:require
    [cljfx.api :as fx]
    [cljfx.coerce :as coerce]
    [clojure.edn :as edn]
    [clojure.string :as string]
    [skylobby.data :as data]
    [skylobby.fs :as fs]
    [skylobby.resource :as resource]
    [skylobby.spring :as spring]
    [skylobby.util :as u]
    [taoensso.timbre :as log])
  (:import
    (javafx.scene.paint Color)))


(set! *warn-on-reflection* true)


(defn get-battle-id [state server-key]
  (get-in state [:by-server server-key :battle :battle-id]))

(defn host-username [context server-key]
  (let [battle-id (fx/sub-val context get-battle-id server-key)]
    (fx/sub-val context get-in [:by-server server-key :battles battle-id :host-username])))

(defn am-host [context server-key]
  (let [username (fx/sub-val context get-in [:by-server server-key :username])
        host-username (fx/sub-ctx context host-username server-key)]
    (= username host-username)))

(defn host-ingame [context server-key]
  (let [host-username (fx/sub-ctx context host-username server-key)]
    (fx/sub-val context get-in [:by-server server-key :users host-username :client-status :ingame])))

(defn my-battle-state [context server-key]
  (let [username (fx/sub-val context get-in [:by-server server-key :username])]
    (fx/sub-val context get-in [:by-server server-key :battle :users username])))

(defn my-battle-status [context server-key]
  (let [my-battle-state (fx/sub-ctx context my-battle-state server-key)]
    (:battle-status my-battle-state)))

(defn my-sync-status [context server-key]
  (let [my-battle-status (fx/sub-ctx context my-battle-status server-key)]
    (int
      (if (= :direct server-key)
        (let [battle-id (fx/sub-val context get-battle-id server-key)]
          (if (and (fx/sub-val context get-in [:by-server server-key :battles battle-id :battle-version])
                   (fx/sub-val context get-in [:by-server server-key :battles battle-id :battle-modname])
                   (fx/sub-val context get-in [:by-server server-key :battles battle-id :battle-map]))
            1
            2))
        (or (:sync my-battle-status) 0)))))

(defn my-team-color [context server-key]
  (let [my-battle-state (fx/sub-ctx context my-battle-state server-key)]
    (:team-color my-battle-state)))

(defn am-spec [context server-key]
  (let [my-battle-status (fx/sub-ctx context my-battle-status server-key)]
    (-> my-battle-status :mode not)))

(defn my-client-status [context server-key]
  (let [username (fx/sub-val context get-in [:by-server server-key :username])]
    (fx/sub-val context get-in [:by-server server-key :users username :client-status])))

(defn am-ingame [context server-key]
  (let [my-client-status (fx/sub-ctx context my-client-status server-key)]
    (:ingame my-client-status)))

(defn startpostype [context server-key]
  (let [scripttags (fx/sub-val context get-in [:by-server server-key :battle :scripttags])]
    (spring/startpostype-name (get-in scripttags ["game" "startpostype"]))))

(defn spring-resources [context spring-root]
  (resource/spring-root-resources spring-root (fx/sub-val context :by-spring-root)))

(defn indexed-engine [context spring-root engine-version]
  (let [{:keys [engines-by-version]} (fx/sub-ctx context spring-resources spring-root)]
    (get engines-by-version engine-version)))

(defn indexed-engines [context spring-root engine-version]
  (let [{:keys [engines-grouped-by-version]} (fx/sub-ctx context spring-resources spring-root)]
    (get engines-grouped-by-version engine-version)))

(defn indexed-map [context spring-root map-name]
  (let [{:keys [maps-by-name]} (fx/sub-ctx context spring-resources spring-root)]
    (get maps-by-name map-name)))

(defn indexed-mod [context spring-root mod-name]
  (let [{:keys [mods-by-name mods-by-name-only]} (fx/sub-ctx context spring-resources spring-root)
        mod-name-no-git-ref (u/mod-name-git-no-ref mod-name)
        directory-match (->> (get mods-by-name-only
                               (when mod-name
                                 (let [[_all game-type] (re-find #"(.*)\s+\$VERSION" mod-name)]
                                   game-type)))
                             (filter (comp #{:directory} :skylobby.fs/source))
                             first)]
    (or (get mods-by-name mod-name)
        (get mods-by-name mod-name-no-git-ref)
        directory-match)))

(defn server-url [context server-key]
   (fx/sub-val context get-in [:by-server server-key :client-data :server-url]))

(defn spring-root [context server-key]
  (or (when (not= :local server-key)
        (let [servers (fx/sub-val context :servers)
              server-url (fx/sub-ctx context server-url server-key)]
          (-> servers (get server-url) :spring-isolation-dir)))
      (fx/sub-val context :spring-isolation-dir)))

(defn filtered-games [context spring-root]
  (let [
        {:keys [mods]} (fx/sub-ctx context spring-resources spring-root)
        mod-filter (fx/sub-val context :mod-filter)
        filter-lc (if mod-filter (string/lower-case mod-filter) "")]
    (->> mods
         (filter :is-game)
         (map :mod-name)
         (filter string?)
         (filter #(string/includes? (string/lower-case %) filter-lc))
         sort)))

(defn filtered-engines [context spring-root]
  (let [
        {:keys [engines]} (fx/sub-ctx context spring-resources spring-root)
        engine-filter (fx/sub-val context :engine-filter)
        filter-lc (if engine-filter (string/lower-case engine-filter) "")]
    (->> engines
         (map :engine-version)
         (filter some?)
         (filter #(string/includes? (string/lower-case %) filter-lc))
         sort)))



(defn parse-filter-tokens
  "Parses filter tokens into different types of filters."
  [filter-str]
  (let [tokens (string/split (string/trim filter-str) #"\s+")]
    (reduce 
     (fn [acc token]
       (cond
         ;; Size filter
         (re-matches #"s(<=|>=|<|>|=)(\d+)" token)
         (let [[_ op num] (re-matches #"s(<=|>=|<|>|=)(\d+)" token)
               num (Long/parseLong num)]
           (update acc :size-filters conj {:op op :value num}))
         
         ;; Author filter
         (string/starts-with? token "a:")
         (update acc :author-filters conj (subs token (count "a:")))
         
         ;; Default: name filter
         :else
         (update acc :name-filters conj token)))
     {:name-filters []
      :size-filters []
      :author-filters []}
     tokens)))

(defn matches-filter?
  "Check if a map entry matches all filter criteria."
  [map-entry {:keys [name-filters author-filters size-filters]}]
  (let [map-name-lc (string/lower-case (:map-name map-entry ""))
        map-author-lc (string/lower-case (:map-author map-entry ""))]
    (and 
     ;; Name filters
     (or (empty? name-filters)
         (some #(string/includes? map-name-lc (string/lower-case %)) name-filters))
     
     ;; Author filters
     (or (empty? author-filters)
         (some #(string/includes? map-author-lc (string/lower-case %)) author-filters))
     
     ;; Size filters
     (every?
       (fn [{:keys [op value]}]
         (let [map-size (max (:map-width map-entry 0) (:map-height map-entry 0))]
           (case op
             "<"  (< map-size value)
             "<=" (<= map-size value)
             ">"  (> map-size value)
             ">=" (>= map-size value)
             "="  (= map-size value)
             false)))
       size-filters))))

(defn filtered-maps
  ([context spring-root]
   (filtered-maps context spring-root (fx/sub-val context :map-input-prefix)))
  ([context spring-root maps-filter]
   (let [
         {:keys [maps]} (fx/sub-ctx context spring-resources spring-root)
         filter-tokens (parse-filter-tokens (or maps-filter ""))]
     (->> maps
          (filter #(matches-filter? % filter-tokens))
          (map :map-name)
          (sort String/CASE_INSENSITIVE_ORDER)))))

(defn details-filtered-by-map-name
  ([context spring-root]
   (details-filtered-by-map-name context spring-root (fx/sub-val context :map-input-prefix)))
  ([context spring-root maps-filter]
   (let [
         {:keys [maps]} (fx/sub-ctx context spring-resources spring-root)
         filter-tokens (parse-filter-tokens (or maps-filter ""))]
     (->> maps
          (filter #(matches-filter? % filter-tokens))
          (reduce
           (fn [acc map-entry]
             (assoc acc (:map-name map-entry) map-entry))
           {})))))

(defn parsed-selected-server-tab [context]
  (let [selected-server-tab (fx/sub-val context :selected-server-tab)]
    (or (try
          (edn/read-string selected-server-tab)
          (catch Exception e
            (log/debug e "Error parsing selected server tab edn")))
        selected-server-tab)))

(defn replay-sources [context]
  (let [
        extra-replay-sources (fx/sub-val context :extra-replay-sources)
        servers (fx/sub-val context :servers)
        spring-isolation-dir (fx/sub-val context :spring-isolation-dir)]
    (fs/replay-sources
      {:extra-replay-sources extra-replay-sources
       :servers servers
       :spring-isolation-dir spring-isolation-dir})))

(defn filtered-battles [context server-key filter-data]
  (let [
        battles (fx/sub-val context get-in [:by-server server-key :battles])]
    (skylobby.data/filter-battles battles filter-data)))

(defn battles-by-id [context server-key]
  (let [
        battles (fx/sub-val context get-in [:by-server server-key :battles])]
    (into {} (map (juxt :battle-id identity) battles))))


(defn engine-details [context spring-isolation-dir engine-version]
  (let [
        spring-root-path (fs/canonical-path spring-isolation-dir)
        engine-override (fs/canonical-path (fx/sub-val context get-in [:engine-overrides spring-root-path engine-version]))
        indexed-engines (fx/sub-ctx context indexed-engines spring-isolation-dir engine-version)]
    (or (first (filter (comp #{engine-override} fs/canonical-path :file) indexed-engines))
        (fx/sub-ctx context indexed-engine spring-isolation-dir engine-version))))

(defn could-be-this-engine-downloads [context engine-version]
  (->> (fx/sub-val context :downloadables-by-url)
       vals
       (filter (comp #{:spring-lobby/engine} :resource-type))
       (filter (partial resource/could-be-this-engine? engine-version))
       seq
       doall))

(defn could-be-this-engine-import [context engine-version]
  (let [
        importables-by-path (fx/sub-val context :importables-by-path)]
    (some->> importables-by-path
             vals
             (filter (comp #{:spring-lobby/engine} :resource-type))
             (filter (partial resource/could-be-this-engine? engine-version))
             first)))

(defn could-be-this-mod-download [context mod-name]
  (let [
        downloadables-by-url (fx/sub-val context :downloadables-by-url)]
    (->> downloadables-by-url
         vals
         (filter (comp #{:spring-lobby/mod} :resource-type))
         (filter (partial resource/could-be-this-mod? mod-name))
         first)))

(defn could-be-this-mod-import [context mod-name]
  (let [
        importables-by-path (fx/sub-val context :importables-by-path)]
    (some->> importables-by-path
             vals
             (filter (comp #{:spring-lobby/mod} :resource-type))
             (filter (partial resource/could-be-this-mod? mod-name))
             first)))

(defn could-be-this-map-downloads [context map-name]
  (let [
        downloadables-by-url (fx/sub-val context :downloadables-by-url)]
    (->> downloadables-by-url
         vals
         (filter (comp #{:spring-lobby/map} :resource-type))
         (filter (partial resource/could-be-this-map? map-name))
         doall)))

(defn could-be-this-map-import [context map-name]
  (let [
        importables-by-path (fx/sub-val context :importables-by-path)]
    (some->> importables-by-path
             vals
             (filter (comp #{:spring-lobby/map} :resource-type))
             (filter (partial resource/could-be-this-map? map-name))
             first)))


(defn sorted-replays [context]
  (let [
        replays (fx/sub-val context :filtered-replays)]
    (->> replays
         (sort-by :replay-unix-time-str)
         reverse
         doall)))


(defn ignore-users-set [context server-key]
  (let [
        ignore-users (fx/sub-val context get-in [:ignore-users server-key])]
    (->> ignore-users
         (filter first)
         (filter second)
         (map first)
         set)))

(defn hide-spads-set [context]
  (let [
        hide-spads-messages (fx/sub-val context :hide-spads-messages)]
    (->> hide-spads-messages
         (filter second)
         (map first)
         set)))

(defn as-color [context k]
  (or
    (when-let [color (fx/sub-val context k)]
      (try
        (coerce/color color)
        (catch Exception e
          (log/trace e "Error parsing color" color))))
    Color/BLACK))
