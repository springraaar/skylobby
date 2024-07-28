(ns skylobby.fx.main
  (:require
    [cljfx.api :as fx]
    [cljfx.ext.tab-pane :as fx.ext.tab-pane]
    [clojure.string :as string]
    skylobby.direct
    skylobby.fx
    [skylobby.fx.battle :as fx.battle]
    [skylobby.fx.bottom-bar :as fx.bottom-bar]
    [skylobby.fx.download :as fx.download]
    [skylobby.fx.import :as fx.import]
    [skylobby.fx.pick-spring-root :as fx.pick-spring-root]
    [skylobby.fx.rapid :as fx.rapid]
    [skylobby.fx.replay :as fx.replay]
    [skylobby.fx.scenarios :as fx.scenarios]
    [skylobby.fx.server-tab :as fx.server-tab]
    [skylobby.fx.settings :as fx.settings]
    [skylobby.fx.tasks :as fx.tasks]
    [skylobby.fx.welcome :as fx.welcome]
    [skylobby.util :as u]
    [taoensso.tufte :as tufte])
  (:import
    (java.util.function UnaryOperator)
    (javafx.scene.control TextFormatter$Change)
    (org.apache.commons.validator.routines InetAddressValidator)))


(set! *warn-on-reflection* true)


(def ^InetAddressValidator ip-validator
  (InetAddressValidator/getInstance))


; https://stackoverflow.com/a/66505103
(def direct-connect-username-filter
  (reify UnaryOperator
    (apply [_this change]
      (let [^TextFormatter$Change change change]
        (if (re-find skylobby.direct/illegal-direct-connect-username-re (.getText change))
          (do
            (.setText change "")
            (.setRange change (.getRangeStart change) (.getRangeStart change))
            change)
          change)))))

(defn protocol-cell
  [x]
  {:text (name x)})

(defn direct-connect-tab
  [{:fx/keys [context]}]
  (let [
        direct-connect-ip (fx/sub-val context :direct-connect-ip)
        direct-connect-port (fx/sub-val context :direct-connect-port)
        direct-connect-port (int (or (when integer? direct-connect-port) direct-connect-port
                                      u/default-server-port))
        direct-connect-username (fx/sub-val context :direct-connect-username)
        direct-connect-host-username (or (fx/sub-val context :direct-connect-host-username)
                                         direct-connect-username)
        direct-connect-join-username (or (fx/sub-val context :direct-connect-join-username)
                                         direct-connect-username)
        direct-connect-password (fx/sub-val context :direct-connect-password)
        direct-connect-protocol (or (fx/sub-val context :direct-connect-protocol) :http)
        direct-connect-public-ip (fx/sub-val context :direct-connect-public-ip)
        server-keys (set (fx/sub-val context u/complex-server-keys))
        server-key {:server-type :direct
                    :protocol :skylobby
                    :hostname direct-connect-ip
                    :port direct-connect-port
                    :username direct-connect-host-username}
        host-server-key (assoc server-key :host true :hostname "localhost")
        client-server-key (assoc server-key :host false)
        login-error (fx/sub-val context :login-error)]
    {:fx/type :v-box
     :children
     [
      {:fx/type :h-box
       :style {:-fx-font-size 20}
       :v-box/vgrow :always
       :children
       [{:fx/type :pane
         :h-box/hgrow :always}
        {:fx/type :v-box
         :alignment :center
         :children
         [{:fx/type :pane
           :v-box/vgrow :always}
          {:fx/type :tab-pane
           :tabs
           [
            {:fx/type :tab
             :closable false
             :graphic {:fx/type :label
                       :text "Join"}
             :content
             {:fx/type :v-box
              :children
              [
                {:fx/type :h-box
                 :alignment :center-left
                 :children
                 [{:fx/type :label
                   :text "Username: "}
                  {:fx/type :pane
                   :h-box/hgrow :always}
                  {:fx/type :text-field
                   :text-formatter
                   {:fx/type :text-formatter
                    :filter direct-connect-username-filter
                    :value-converter :default
                    :value (str direct-connect-join-username)
                    :on-value-changed {:event/type :spring-lobby/assoc
                                       :key :direct-connect-join-username}}}]}
                #_
                {:fx/type :h-box
                 :alignment :center-left
                 :children
                 [{:fx/type :label
                   :text "Password: "}
                  {:fx/type :pane
                   :h-box/hgrow :always}
                  {:fx/type :text-field
                   :text (str direct-connect-password)
                   :on-text-changed {:event/type :spring-lobby/assoc
                                     :key :direct-connect-password}}]}
                {:fx/type :h-box
                 :alignment :center-left
                 :children
                 [{:fx/type :label
                   :text "Port: "}
                  {:fx/type :pane
                   :h-box/hgrow :always}
                  {:fx/type :text-field
                   :text-formatter
                   {:fx/type :text-formatter
                    :value-converter :integer
                    :value direct-connect-port
                    :on-value-changed {:event/type :spring-lobby/assoc
                                       :key :direct-connect-port}}}]}
                {:fx/type :h-box
                 :alignment :center-left
                 :children
                 [{:fx/type :label
                   :text "Host IP: "}
                  {:fx/type :pane
                   :h-box/hgrow :always}
                  {:fx/type :text-field
                   :text (str direct-connect-ip)
                   :prompt-text "0.0.0.0"
                   :on-text-changed {:event/type :spring-lobby/assoc
                                     :key :direct-connect-ip}}]}
                {:fx/type :h-box
                 :alignment :center-left
                 :children
                 [{:fx/type :label
                   :text "Protocol: "}
                  {:fx/type :combo-box
                   :value direct-connect-protocol
                   :items [:http :https]
                   :on-value-changed {:event/type :spring-lobby/assoc
                                      :key :direct-connect-protocol}
                   :button-cell protocol-cell
                   :cell-factory
                   {:fx/cell-type :list-cell
                    :describe protocol-cell}}]}
                {:fx/type :button
                 :style-class ["button" "skylobby-normal"]
                 :text "Join"
                 :disable (or (string/blank? direct-connect-join-username)
                              (string/blank? direct-connect-ip)
                              (contains? server-keys client-server-key))
                 :on-action {:event/type :skylobby.fx.event.direct/join
                             :direct-connect-ip direct-connect-ip
                             :direct-connect-password direct-connect-password
                             :direct-connect-port direct-connect-port
                             :direct-connect-protocol direct-connect-protocol
                             :direct-connect-username direct-connect-join-username}}
                {:fx/type :label
                 :style {:-fx-text-fill "red"}
                 :text (str
                         (when-let [error (:direct-client login-error)]
                           (str "Error: " error)))}]}}
            {:fx/type :tab
             :closable false
             :graphic {:fx/type :label
                       :text "Host"}
             :content
             {:fx/type :v-box
              :children
              [
                {:fx/type :h-box
                 :alignment :center-left
                 :children
                 [{:fx/type :label
                   :text "Username: "}
                  {:fx/type :pane
                   :h-box/hgrow :always}
                  {:fx/type :text-field
                   :text-formatter
                   {:fx/type :text-formatter
                    :filter direct-connect-username-filter
                    :value-converter :default
                    :value (str direct-connect-host-username)
                    :on-value-changed {:event/type :spring-lobby/assoc
                                       :key :direct-connect-host-username}}}]}
                #_
                {:fx/type :h-box
                 :alignment :center-left
                 :children
                 [{:fx/type :label
                   :text "Password: "}
                  {:fx/type :pane
                   :h-box/hgrow :always}
                  {:fx/type :text-field
                   :text (str direct-connect-password)
                   :on-text-changed {:event/type :spring-lobby/assoc
                                     :key :direct-connect-password}}]}
                {:fx/type :h-box
                 :alignment :center-left
                 :children
                 [{:fx/type :label
                   :text "Port: "}
                  {:fx/type :pane
                   :h-box/hgrow :always}
                  {:fx/type :text-field
                   :text-formatter
                   {:fx/type :text-formatter
                    :value-converter :integer
                    :value direct-connect-port
                    :on-value-changed {:event/type :spring-lobby/assoc
                                       :key :direct-connect-port}}}]}
                {:fx/type :h-box
                 :alignment :center-left
                 :children
                 [{:fx/type :label
                   :text "Protocol: "}
                  {:fx/type :combo-box
                   :value direct-connect-protocol
                   :items [:http]
                   :on-value-changed {:event/type :spring-lobby/assoc
                                      :key :direct-connect-protocol}
                   :button-cell protocol-cell
                   :cell-factory
                   {:fx/cell-type :list-cell
                    :describe protocol-cell}}]}
                {:fx/type :button
                 :style-class ["button" "skylobby-normal"]
                 :text "Host"
                 :disable (or (string/blank? direct-connect-host-username)
                              (contains? server-keys host-server-key))
                 :on-action {:event/type :skylobby.fx.event.direct/host
                             :direct-connect-password direct-connect-password
                             :direct-connect-port direct-connect-port
                             :direct-connect-protocol direct-connect-protocol
                             :direct-connect-username direct-connect-host-username}}
                {:fx/type :label
                 :style {:-fx-text-fill "red"}
                 :text (str
                         (when-let [error (:direct-host login-error)]
                           (str "Error: " error)))}
                {:fx/type :h-box
                 :alignment :center-left
                 :children
                 (concat
                   [
                    {:fx/type :label
                     :text (str " Public IP: ")}]
                   (when direct-connect-public-ip
                     [{:fx/type :text-field
                       :editable false
                       :text (str direct-connect-public-ip)}])
                   [{:fx/type :button
                     :style-class ["button" "skylobby-normal"]
                     :text "Check"
                     :on-action {:event/type :spring-lobby/check-public-ip}}])}]}}]}
          {:fx/type :pane
           :v-box/vgrow :always}]}
        {:fx/type :pane
         :h-box/hgrow :always}]}]}))


(defn main-window-impl
  [{:fx/keys [context]}]
  (let [valid-server-keys (fx/sub-ctx context skylobby.fx/valid-server-keys-sub)
        complex-server-keys (fx/sub-val context u/complex-server-keys)
        show-direct-connect (fx/sub-val context :show-direct-connect)
        show-http (fx/sub-val context :show-downloader)
        show-import (fx/sub-val context :show-importer)
        show-rapid (fx/sub-val context :show-rapid-downloader)
        show-replays (fx/sub-val context :show-replays)
        show-scenarios (fx/sub-val context :show-scenarios-window)
        show-settings (fx/sub-val context :show-settings-window)
        show-singleplayer (fx/sub-val context get-in [:by-server :local :battle :battle-id])
        show-spring-picker (and (fx/sub-val context :show-spring-picker)
                                (not (fx/sub-val context :spring-lobby.main/spring-root-arg)))
        show-tasks (fx/sub-val context :show-tasks-window)
        show-accolades (fx/sub-val context :show-accolades)
        windows-as-tabs (fx/sub-val context :windows-as-tabs)
        tab-ids (concat
                  (when show-spring-picker ["spring"])
                  ["welcome"]
                  (when windows-as-tabs
                    (concat
                      (when show-settings
                        ["settings"])
                      (when show-replays
                        ["replays"])
                      (when show-scenarios
                        ["scenarios"])
                      (when show-http
                        ["http"])
                      (when show-rapid
                        ["rapid"])
                      (when show-import
                        ["import"])
                      (when show-tasks
                        ["tasks"])))
                  (when show-singleplayer ["singleplayer"])
                  (when show-direct-connect ["direct"])
                  (map str complex-server-keys)
                  valid-server-keys)
                  ;#_(when (seq valid-server-keys) ["multi"]))
        tab-id-set (set tab-ids)
        selected-server-tab (fx/sub-val context :selected-server-tab)
        selected-index (or (when (contains? tab-id-set selected-server-tab)
                             (.indexOf ^java.util.List tab-ids selected-server-tab))
                           0)
        needs-focus (fx/sub-val context :needs-focus)
        highlight-tabs-with-new-battle-messages (fx/sub-val context :highlight-tabs-with-new-battle-messages)
        highlight-tabs-with-new-chat-messages (fx/sub-val context :highlight-tabs-with-new-chat-messages)
        selected-tab-main (fx/sub-val context :selected-tab-main)
        selected-tab-channel (fx/sub-val context :selected-tab-channel)
        mute (fx/sub-val context :mute)]
    {:fx/type :v-box
     :style {:-fx-font-size 14}
     :alignment :top-left
     :children
     [
      {:fx/type fx.ext.tab-pane/with-selection-props
       :v-box/vgrow :always
       :props
       {:on-selected-item-changed {:event/type :spring-lobby/selected-item-changed-server-tabs
                                   :selected-tab-channel selected-tab-channel
                                   :selected-tab-main selected-tab-main}
        :selected-index selected-index}
       :desc
       {:fx/type :tab-pane
        :tabs
        (concat
          (when show-spring-picker
            [{:fx/type :tab
              :id "spring"
              :closable true
              :graphic {:fx/type :label
                        :text "Spring"
                        :style {:-fx-font-size 18}}
              :on-close-request {:event/type :spring-lobby/assoc
                                 :key :show-spring-picker
                                 :value false}
              :content
              {:fx/type fx.pick-spring-root/pick-spring-root-view}}])
          [{:fx/type :tab
            :id "welcome"
            :closable false
            :graphic {:fx/type :label
                      :text "Main"
                      :style {:-fx-font-size 18}}
            :content
            {:fx/type fx.welcome/welcome-view
             :v-box/vgrow :always}}]
          (when (and windows-as-tabs show-settings)
            [{:fx/type :tab
              :id "settings"
              :closable true
              :graphic {:fx/type :label
                        :text "Settings"
                        :style {:-fx-font-size 18}}
              :on-close-request {:event/type :spring-lobby/assoc
                                 :key :show-settings-window
                                 :value false}
              :content
              {:fx/type fx.settings/settings-root}}])
          (when (and windows-as-tabs show-replays)
            [{:fx/type :tab
              :id "replays"
              :closable true
              :graphic {:fx/type :label
                        :text "Replays"
                        :style {:-fx-font-size 18}}
              :on-close-request {:event/type :spring-lobby/assoc
                                 :key :show-replays
                                 :value false}
              :content
              {:fx/type fx.replay/replays-root}}])
          (when (and windows-as-tabs show-scenarios)
            [{:fx/type :tab
              :id "scenarios"
              :closable true
              :graphic {:fx/type :label
                        :text "Scenarios"
                        :style {:-fx-font-size 18}}
              :on-close-request {:event/type :spring-lobby/assoc
                                 :key :show-scenarios-window
                                 :value false}
              :content
              {:fx/type fx.scenarios/scenarios-root}}])
          (when (and windows-as-tabs show-http)
            [{:fx/type :tab
              :id "http"
              :closable true
              :graphic {:fx/type :label
                        :text "HTTP Download"
                        :style {:-fx-font-size 18}}
              :on-close-request {:event/type :spring-lobby/assoc
                                 :key :show-downloader
                                 :value false}
              :content
              {:fx/type fx.download/downloader-root}}])
          (when (and windows-as-tabs show-rapid)
            [{:fx/type :tab
              :id "rapid"
              :closable true
              :graphic {:fx/type :label
                        :text "Rapid"
                        :style {:-fx-font-size 18}}
              :on-close-request {:event/type :spring-lobby/assoc
                                 :key :show-rapid-downloader
                                 :value false}
              :content
              {:fx/type fx.rapid/rapid-download-root}}])
          (when (and windows-as-tabs show-import)
            [{:fx/type :tab
              :id "import"
              :closable true
              :graphic {:fx/type :label
                        :text "Importer"
                        :style {:-fx-font-size 18}}
              :on-close-request {:event/type :spring-lobby/assoc
                                 :key :show-importer
                                 :value false}
              :content
              {:fx/type fx.import/importer-root}}])
          (when (and windows-as-tabs show-tasks)
            [{:fx/type :tab
              :id "tasks"
              :closable true
              :graphic {:fx/type :label
                        :text "Tasks"
                        :style {:-fx-font-size 18}}
              :on-close-request {:event/type :spring-lobby/assoc
                                 :key :show-tasks-window
                                 :value false}
              :content
              {:fx/type fx.tasks/tasks-root}}])
          (when show-singleplayer
            [{:fx/type :tab
              :id "singleplayer"
              :closable true
              :graphic {:fx/type :label
                        :text "Singleplayer"
                        :style {:-fx-font-size 18}}
              :on-close-request {:event/type :spring-lobby/dissoc-in
                                 :path [:by-server :local]}
              :content
              {:fx/type fx.battle/battle-view
               :v-box/vgrow :always
               :server-key :local}}])
          (when show-direct-connect
            [{:fx/type :tab
              :id "direct"
              :closable true
              :graphic {:fx/type :label
                        :text "LAN / Direct Connect"
                        :style {:-fx-font-size 18}}
              :on-close-request {:event/type :spring-lobby/dissoc
                                 :key :show-direct-connect}
              :content
              {:fx/type direct-connect-tab}}])
          (mapv
            (fn [server-key]
              ; TODO more than just direct connect
              ; TODO direct connect server vs client
              {:fx/type :tab
               :id (str server-key)
               :closable true
               :graphic {:fx/type :text-flow
                   :children [{:fx/type :label
                         :text (name (:server-type server-key)) 
                         :style {:-fx-font-size 18}}
                         {:fx/type :label
                         :text (str (name (:server-type server-key))
                                    " "
                                    (:username server-key)
                                    "@"
                                    (:hostname server-key)
                                    ":"
                                    (:port server-key))
                         :style {:-fx-font-size 13 :-fx-opacity 0.7}}
                         ]}
               :on-close-request {:event/type :spring-lobby/disconnect
                                  :server-key server-key}
               :content
               {:fx/type fx.battle/battle-view
                :v-box/vgrow :always
                :server-key server-key}})
            complex-server-keys)
          (mapv
            (fn [server-key]
              (let [
                    ignore-users (fx/sub-val context get-in [:ignore-users server-key])
                    my-channels (fx/sub-val context get-in [:by-server server-key :my-channels])
                    ignore-channels-set (->> ignore-users
                                             (filter second)
                                             (map first)
                                             (map u/user-channel-name)
                                             set)
                    classes (if (and (not= server-key selected-server-tab)
                                     (contains? needs-focus server-key)
                                     (or (and highlight-tabs-with-new-battle-messages
                                              (contains? (get needs-focus server-key) "battle")
                                              (not (get-in mute [server-key :battle])))
                                         (and highlight-tabs-with-new-chat-messages
                                              (contains? (get needs-focus server-key) "chat")
                                              (some (fn [channel-name]
                                                      (and
                                                        (contains? my-channels channel-name)
                                                        (not (contains? (get mute server-key) channel-name))
                                                        (not (contains? ignore-channels-set channel-name))
                                                        (or (not show-accolades)
                                                            (not= channel-name (u/user-channel-name "AccoladesBot")))))
                                                    (keys (get-in needs-focus [server-key "chat"]))))))
                                ["tab" "skylobby-tab-focus"]
                                ["tab"])]
                {:fx/type :tab
                 :id (str server-key)
                 :style-class classes
                 :graphic {:fx/type :text-flow
                   :children [{:fx/type :label
                           :text (let [[_ server-config] (fx/sub-val context get-in [:by-server server-key :server])]
                                        (:alias server-config))
                           :style {:-fx-font-size 18}}
                           {:fx/type :label
                           :text (str " (" server-key ")")
                           :style {:-fx-font-size 13 :-fx-opacity 0.7}}
                           ]}
                 :on-close-request {:event/type :spring-lobby/disconnect
                                    :server-key server-key}
                 :content
                 {:fx/type fx.server-tab/server-tab
                  :server-key server-key}}))
            valid-server-keys)
          #_
          (when (seq valid-servers)
            [{:fx/type :tab
              :id "multi"
              :closable false
              :graphic {:fx/type :label
                        :text "All Servers"
                        :style {:-fx-font-size 18}}
              :content
              (merge
                {:fx/type multi-server-tab}
                (select-keys state [:map-details :mod-details]))}]))}}
      {:fx/type fx.bottom-bar/bottom-bar}]}))

(defn main-window [state]
  (tufte/profile {:dynamic? true
                  :id :skylobby/ui}
    (tufte/p :main-window
      (main-window-impl state))))
