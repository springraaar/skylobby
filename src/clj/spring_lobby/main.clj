(ns spring-lobby.main
  (:require
    [clj-http.client :as clj-http]
    [cljfx.api :as fx]
    [cljfx.css :as css]
    clojure.core.async
    [clojure.core.cache :as cache]
    [clojure.java.io :as io]
    [clojure.string :as string]
    [clojure.tools.cli :as cli]
    [skylobby.direct :as direct]
    [skylobby.fs :as fs]
    [skylobby.fs.sdfz :as sdfz]
    skylobby.fx
    [skylobby.fx.replay :as fx.replay]
    [skylobby.fx.root :as fx.root]
    [skylobby.git :as git]
    [skylobby.spring :as spring]
    [skylobby.util :as u]
    [skylobby.watch :as watch]
    spring-lobby
    [spring-lobby.replays :as replays]
    [taoensso.timbre :as log])
  (:import
    (javafx.application Platform))
  (:gen-class))


(set! *warn-on-reflection* true)


(def players-disply-types #{"group" "table"})

(def cli-options
  [[nil "--chat-channel CHANNEL_NAME" "Add a default chat channel to connect to"
    :assoc-fn (fn [m k v]
                (update m k conj v))]
   [nil "--css-file CSS_FILE" "Use the given file for CSS style"]
   [nil "--css-preset CSS_PRESET_NAME" "Use the given CSS preset"]
   [nil "--filter-battles FILTER_BATTLES" "Set the initial battles filter string"]
   [nil "--filter-users FILTER_USERS" "Set the initial users filter string"]
   [nil "--music-dir MUSIC_DIR" "Set the music-dir config to the given directory"]
   [nil "--music-volume MUSIC_VOLUME" "Set the music-volume config to the given value between 0.0 and 1.0"
    :parse-fn #(Double/parseDouble %)
    :validate [#(<= 0 % 1) "Must be a number between 0.0 and 1.0"]]
   [nil "--no-update-check" "Diable skylobby self update check"]
   [nil "--open-url URL" "Open a URL in the default system browser on start"]
   [nil "--players-display-type GROUP_OR_TABLE" (str "Display type for players in a battle, one of " players-disply-types)
    :validate [players-disply-types (str "Players display type must be one of " players-disply-types)]]
   [nil "--port PORT" "Port to use for web ui AND ipc for file associations like replays"]
   [nil "--replay-source REPLAY_SOURCE" "Replace default replay sources with one or more overrides"
    :assoc-fn (fn [m k v]
                (update m k conj v))]
   [nil "--skylobby-root SKYLOBBY_ROOT" "Set the config and log dir for skylobby"]
   [nil "--spring-root SPRING_ROOT" "Set the spring-root config to the given directory"]
   [nil "--spring-type SPRING_TYPE" "Set the spring engine executable type to use, \"dedicated\" or \"headless\"."
    :parse-fn keyword]
   [nil "--server-url SERVER_URL" "Set the selected server config by url"]
   [nil "--update-copy-jar JAR_DEST" "Copy updated jar to the given location"]
   [nil "--window-maximized" "Start with the main window maximized"]])


(defn get-app-version []
  (or (u/manifest-version)
      (try
        (str "git:" (u/short-git-commit (git/latest-id (io/file "."))))
        (catch Exception e
          (log/debug e "Error getting git version")))
      (try
        (slurp (io/resource (str u/app-name ".version")))
        (catch Exception e
          (log/debug e "Error getting version from file")))
      "UNKNOWN"))

; maybe not on init
(alter-var-root #'skylobby.util/app-version (fn [& _] (get-app-version)))


(defn parse-replay-file [args]
  (let [
        first-arg-as-file (some-> args first fs/file)
        first-arg-filename (fs/filename first-arg-as-file)
        all-args-as-file (fs/file (string/join " " args))
        all-args-filename (fs/filename all-args-as-file)
        single-arg-file (and (not (string/blank? first-arg-filename))
                             (string/ends-with? first-arg-filename ".sdfz")
                             (fs/exists? first-arg-as-file))]
    (if single-arg-file
      first-arg-as-file
      (when (and (not (string/blank? all-args-filename))
                 (string/ends-with? all-args-filename ".sdfz")
                 (fs/exists? all-args-as-file))
        all-args-as-file))))


(defn -ui-main [& args]
  (let [{:keys [arguments errors options]} (cli/parse-opts args cli-options :in-order true)]
    (if errors
      (do
        (println "Error parsing arguments:\n\n"
                 (string/join \newline errors))
        (System/exit -1))
      (let [replay-file (try
                          (parse-replay-file arguments)
                          (catch Exception e
                            (log/error e "Error parsing replay file from" (pr-str arguments))))
            opening-replay? (some? replay-file)]
        (try
          (when-let [dest (fs/file (:update-copy-jar options))]
            (when-let [jar-file (u/jar-file)]
              (log/info "Copying update jar")
              (fs/copy jar-file dest))
            (spring-lobby/restart-process nil dest))
          (catch Exception e
            (log/error e "Error copying update jar")))
        (try
          (alter-var-root #'spring-lobby/main-args (constantly args))
          (when-let [url (:open-url options)]
            (spring-lobby/browse-url (str url)))
          (when (:no-update-check options)
            (alter-var-root #'spring-lobby/disable-update-check (constantly true)))
          (when-let [app-root-override (:skylobby-root options)]
            (alter-var-root #'fs/app-root-override (constantly app-root-override)))
          (when-let [spring-type (:spring-type options)]
            (alter-var-root #'spring/spring-type (constantly spring-type)))
          (when-let [replay-sources (seq (:replay-source options))]
            (let [replay-sources-override (map
                                            (fn [source]
                                              {:replay-source-name ""
                                               :file (fs/file source)
                                               :builtin true})
                                            replay-sources)]
              (log/info "Replacing replay sources with" (pr-str replay-sources-override))
              (alter-var-root #'fs/replay-sources-override (constantly replay-sources-override))))
          (let [before (u/curr-millis)]
            (log/info "UI Main")
            (Platform/setImplicitExit true)
            (log/info "Set JavaFX implicit exit")
            (let [before-state (u/curr-millis)
                  _ (log/info "Loading initial state")
                  initial-state (spring-lobby/initial-state)
                  state (merge
                          initial-state
                          {:standalone true}
                          (when (contains? options :spring-root)
                            (let [f (fs/file (:spring-root options))]
                              {:spring-isolation-dir f
                               ::spring-root-arg f}))
                          (when-let [port (:port options)]
                            {:ipc-server-port port})
                          (when (contains? options :music-dir)
                            {:music-dir (fs/file (:music-dir options))})
                          (when (contains? options :music-volume)
                            {:music-volume (:music-volume options)})
                          (when (contains? options :filter-battles)
                            {:filter-battles (:filter-battles options)})
                          (when (contains? options :filter-users)
                            {:filter-users (:filter-users options)})
                          (when (contains? options :players-display-type)
                            {:battle-players-display-type (:players-display-type options)})
                          (when (contains? options :window-maximized)
                            {:window-maximized true})
                          (when (contains? options :server-url)
                            (let [server (->> initial-state
                                              :servers
                                              (filter (comp #{(:server-url options)} first))
                                              first)
                                  {:keys [password username]} (->> initial-state
                                                                   :logins
                                                                   (filter (comp #{(:server-url options)} first))
                                                                   first
                                                                   second)]
                              {:server server
                               :password password
                               :username username}))
                          (when (contains? options :chat-channel)
                            {:global-chat-channels
                             (map
                               (juxt identity (constantly {}))
                               (:chat-channel options))}))]
              (log/info "Loaded initial state in" (- (u/curr-millis) before-state) "ms")
              (reset! spring-lobby/*state state)
              (spring-lobby/add-ui-state-watcher spring-lobby/*state spring-lobby/*ui-state)
              (let [previous-css (css/register :skylobby.fx/current
                    (or (and (:css-preset state)
                               (or (get skylobby.fx/style-presets (:css-preset state))
                         (:css state)))
                         skylobby.fx/default-style-data)) 
                    css (cond
                          (:css-file options)
                          {:cljfx.css/url (some-> options :css-file fs/file .toURI .toURL)}
                          (:css-preset options)
                          (css/register :skylobby.fx/current
                            (get skylobby.fx/style-presets (some-> options :css-preset string/lower-case)))
                          :else previous-css)]
                (swap! spring-lobby/*state assoc :css css)))
            (cond
              (= "skyreplays" (first arguments))
              (do
                (log/info "Starting skyreplays")
                (swap! spring-lobby/*state assoc :show-replays true :standalone true)
                (replays/create-renderer)
                (spring-lobby/init-async spring-lobby/*state))
              opening-replay?
              (let [replay-details (sdfz/parse-replay replay-file {:details true :parse-stream true})
                    replay-path (fs/canonical-path replay-file)]
                (log/info "Opening replay view")
                (swap! spring-lobby/*state
                  (fn [state]
                    (-> state
                        (assoc :parsed-replays-by-path {replay-path replay-details}
                               :selected-replay replay-details
                               :selected-replay-file replay-file
                               :single-replay-view true)
                        (update :replay-details cache/miss replay-path replay-details))))
                (watch/replay-map-and-mod-details-watcher nil spring-lobby/*state nil @spring-lobby/*state)
                (let [r (fx/create-renderer
                          :middleware (comp
                                        fx/wrap-context-desc
                                        (fx/wrap-map-desc (fn [_] {:fx/type fx.replay/standalone-replay-window})))
                          :opts {:fx.opt/map-event-handler spring-lobby/event-handler
                                 :fx.opt/type->lifecycle #(or (fx/keyword->lifecycle %)
                                                              (fx/fn->lifecycle-with-context %))})]
                  (log/info "Mounting renderer")
                  (fx/mount-renderer spring-lobby/*ui-state r))
                (spring-lobby/standalone-replay-init spring-lobby/*state)
                (log/info "Main finished in" (- (u/curr-millis) before) "ms"))
              (= "headless" (first arguments))
              (do
                (log/info "Starting headless")
                (future
                  (spring-lobby/auto-connect-servers spring-lobby/*state))
                (spring-lobby/init spring-lobby/*state)
                (spring-lobby/browse-url (str "http://localhost:" (or (:port options)
                                                                      u/default-ipc-port))))
              (= "direct" (first arguments))
              (apply direct/-main (rest arguments))
              :else
              (do
                (log/info "Creating renderer")
                (let [r (fx/create-renderer
                          :middleware (comp
                                        fx/wrap-context-desc
                                        (fx/wrap-map-desc (fn [_] {:fx/type fx.root/root-view})))
                          :opts {:fx.opt/map-event-handler spring-lobby/event-handler
                                 :fx.opt/type->lifecycle #(or (fx/keyword->lifecycle %)
                                                              (fx/fn->lifecycle-with-context %))})]
                  (log/info "Mounting renderer")
                  (fx/mount-renderer spring-lobby/*ui-state r))
                (spring-lobby/init-async spring-lobby/*state)
                (future
                  (fs/delete-skylobby-update-jars))
                (future
                  (spring-lobby/auto-connect-servers spring-lobby/*state))
                (log/info "Main finished in" (- (u/curr-millis) before) "ms"))))
          (catch Throwable t
            (let [st (with-out-str (.printStackTrace t))]
              (println st)
              (spit "skylobby-fatal-error.txt" st))
            (log/error t "Fatal error")
            (System/exit -1)))))))


(defn -main [& args]
  (u/log-to-file (fs/canonical-path (fs/config-file (str u/app-name ".log"))))
  (log/info "Main" (pr-str args))
  (let [{:keys [arguments]} (cli/parse-opts args [])
        replay-file (parse-replay-file arguments)
        opening-replay? (some? replay-file)
        port u/default-ipc-port]
    (try
      (if (and opening-replay? (not (u/is-port-open? port)))
        (do
          (log/info "Sending IPC to existing skylobby instance on port" port)
          (clj-http/post
            (str "http://localhost:" port "/ipc/replay")
            {:query-params {:path (fs/canonical-path replay-file)}})
          (System/exit 0))
        (apply -ui-main args))
      (catch Throwable t
        (let [st (with-out-str (.printStackTrace t))]
          (println st)
          (spit "skylobby-fatal-error.txt" st))
        (log/error t "Fatal error")
        (System/exit -1)))))
