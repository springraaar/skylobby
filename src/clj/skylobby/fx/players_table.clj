(ns skylobby.fx.players-table
  (:require
    [clojure.string :as string]
    [skylobby.fx.ext :refer [ext-recreate-on-key-changed]]
    [spring-lobby.fx.font-icon :as font-icon]
    [spring-lobby.util :as u]
    [taoensso.timbre :as log]))


(def allyteam-colors
  {0 "crimson"
   1 "royalblue"
   2 "goldenrod"
   3 "darkseagreen"
   4 "brown"
   5 "pink"
   6 "darkturquoise"
   7 "darkorange"})

(defn players-table
  [{:keys [am-host battle-players-color-allyteam channel-name client host-username players
           scripttags sides singleplayer username]}]
  (let [players-with-skill (map
                             (fn [{:keys [skill skilluncertainty username] :as player}]
                               (let [username-kw (when username (keyword (string/lower-case username)))
                                     tags (some-> scripttags :game :players (get username-kw))
                                     uncertainty (or (try (u/to-number skilluncertainty)
                                                          (catch Exception e
                                                            (log/debug e "Error parsing skill uncertainty")))
                                                     (try (u/to-number (:skilluncertainty tags))
                                                          (catch Exception e
                                                            (log/debug e "Error parsing skill uncertainty")))
                                                     3)]
                                 (assoc player
                                        :skill (or skill (:skill tags))
                                        :skilluncertainty uncertainty)))
                             players)]
    {:fx/type :table-view
     :column-resize-policy :constrained ; TODO auto resize
     :items (->> players-with-skill
                 (sort-by
                   (juxt
                     (comp not :mode :battle-status)
                     (comp :bot :client-status :user)
                     (comp :ally :battle-status)
                     (comp (fnil - 0) u/parse-skill :skill))))
     :style {:-fx-font-size 14
             :-fx-min-height 200
             :-fx-pref-height 200}
     :row-factory
     {:fx/cell-type :table-row
      :describe (fn [{:keys [owner username user]}]
                  {
                   :context-menu
                   {:fx/type :context-menu
                    :items
                    (concat []
                      (when (and (not owner))
                        [
                         {:fx/type :menu-item
                          :text "Message"
                          :on-action {:event/type :spring-lobby/join-direct-message
                                      :username username}}])
                      [{:fx/type :menu-item
                        :text "Ring"
                        :on-action {:event/type :spring-lobby/ring
                                    :client client
                                    :channel-name channel-name
                                    :username username}}]
                      (when (and host-username
                                 (= host-username username)
                                 (-> user :client-status :bot))
                        [{:fx/type :menu-item
                          :text "!help"
                          :on-action {:event/type :spring-lobby/send-message
                                      :client client
                                      :channel-name (u/user-channel host-username)
                                      :message "!help"}}
                         {:fx/type :menu-item
                          :text "!status battle"
                          :on-action {:event/type :spring-lobby/send-message
                                      :client client
                                      :channel-name (u/user-channel host-username)
                                      :message "!status battle"}}
                         {:fx/type :menu-item
                          :text "!status game"
                          :on-action {:event/type :spring-lobby/send-message
                                      :client client
                                      :channel-name (u/user-channel host-username)
                                      :message "!status game"}}])
                      (when (-> user :client-status :bot)
                        [{:fx/type :menu-item
                          :text "!whois"
                          :on-action {:event/type :spring-lobby/send-message
                                      :client client
                                      :channel-name (u/user-channel host-username)
                                      :message (str "!whois " username)}}])
                      [{:fx/type :menu-item
                        :text (str "User ID: " (-> user :user-id))}])}})}
     :columns
     [{:fx/type :table-column
       :text "Nickname"
       :cell-value-factory identity
       :cell-factory
       {:fx/cell-type :table-cell
        :describe
        (fn [{:keys [owner] :as id}]
          (let [not-spec (-> id :battle-status :mode)]
            (merge
              {:text (u/nickname id)
               :style
               (merge
                 {:-fx-text-fill (if (and battle-players-color-allyteam not-spec)
                                     (get allyteam-colors (-> id :battle-status :ally) "white")
                                     "white")}
                 (when not-spec
                   {:-fx-font-weight "bold"}))}
              (when (and username
                         (not= username (:username id))
                         (or am-host
                             (= owner username)))
                {:graphic
                 {:fx/type :button
                  :on-action
                  (merge
                    {:event/type :spring-lobby/kick-battle
                     :client client
                     :singleplayer singleplayer}
                    (select-keys id [:bot-name :username]))
                  :graphic
                  {:fx/type font-icon/lifecycle
                   :icon-literal "mdi-account-remove:16:white"}}}))))}}
      {:fx/type :table-column
       :text "TrueSkill"
       :cell-value-factory identity
       :cell-factory
       {:fx/cell-type :table-cell
        :describe
        (fn [{:keys [skill skilluncertainty]}]
          {:text
           (str skill
                " "
                (when (number? skilluncertainty)
                  (apply str (repeat skilluncertainty "?"))))})}}
      {:fx/type :table-column
       :text "Ally"
       :cell-value-factory identity
       :cell-factory
       {:fx/cell-type :table-cell
        :describe
        (fn [i]
          {:text ""
           :graphic
           {:fx/type ext-recreate-on-key-changed
            :key (u/nickname i)
            :desc
            {:fx/type :combo-box
             :value (str (:ally (:battle-status i)))
             :on-value-changed {:event/type :spring-lobby/battle-ally-changed
                                :client (when-not singleplayer client)
                                :is-me (= (:username i) username)
                                :is-bot (-> i :user :client-status :bot)
                                :id i}
             :items (map str (take 16 (iterate inc 0)))
             :disable (or (not username)
                          (not (or am-host
                                   (= (:username i) username)
                                   (= (:owner i) username))))}}})}}
      {:fx/type :table-column
       :text "Team"
       :cell-value-factory identity
       :cell-factory
       {:fx/cell-type :table-cell
        :describe
        (fn [i]
          {:text ""
           :graphic
           {:fx/type ext-recreate-on-key-changed
            :key (u/nickname i)
            :desc
            {:fx/type :combo-box
             :value (str (:id (:battle-status i)))
             :on-value-changed {:event/type :spring-lobby/battle-team-changed
                                :client (when-not singleplayer client)
                                :is-me (= (:username i) username)
                                :is-bot (-> i :user :client-status :bot)
                                :id i}
             :items (map str (take 16 (iterate inc 0)))
             :disable (or (not username)
                          (not (or am-host
                                   (= (:username i) username)
                                   (= (:owner i) username))))}}})}}
      {:fx/type :table-column
       :text "Color"
       :cell-value-factory identity
       :cell-factory
       {:fx/cell-type :table-cell
        :describe
        (fn [{:keys [team-color] :as i}]
          {:text ""
           :graphic
           {:fx/type ext-recreate-on-key-changed
            :key (u/nickname i)
            :desc
            {:fx/type :color-picker
             :value (u/spring-color-to-javafx team-color)
             :on-action {:event/type :spring-lobby/battle-color-action
                         :client (when-not singleplayer client)
                         :is-me (= (:username i) username)
                         :is-bot (-> i :user :client-status :bot)
                         :id i}
             :disable (or (not username)
                          (not (or am-host
                                   (= (:username i) username)
                                   (= (:owner i) username))))}}})}}
      {:fx/type :table-column
       :text "Status"
       :cell-value-factory identity
       :cell-factory
       {:fx/cell-type :table-cell
        :describe
        (fn [{:keys [battle-status user username]}]
          (let [client-status (:client-status user)
                am-host (= username host-username)]
            {:text ""
             :graphic
             {:fx/type :h-box
              :children
              (concat
                [(cond
                   (:bot client-status)
                   {:fx/type font-icon/lifecycle
                    :icon-literal "mdi-robot:16:grey"}
                   (not (:mode battle-status))
                   {:fx/type font-icon/lifecycle
                    :icon-literal "mdi-magnify:16:white"}
                   (:ready battle-status)
                   {:fx/type font-icon/lifecycle
                    :icon-literal "mdi-account-check:16:green"}
                   am-host
                   {:fx/type font-icon/lifecycle
                    :icon-literal "mdi-account-key:16:orange"}
                   :else
                   {:fx/type font-icon/lifecycle
                    :icon-literal "mdi-account:16:white"})]
                (when (:ingame client-status)
                  [{:fx/type font-icon/lifecycle
                    :icon-literal "mdi-sword:16:red"}])
                (when (:away client-status)
                  [{:fx/type font-icon/lifecycle
                    :icon-literal "mdi-sleep:16:grey"}]))}}))}}
      {:fx/type :table-column
       :text "Spectator"
       :cell-value-factory identity
       :cell-factory
       {:fx/cell-type :table-cell
        :describe
        (fn [i]
          {:text ""
           :graphic
           {:fx/type ext-recreate-on-key-changed
            :key (u/nickname i)
            :desc
            {:fx/type :check-box
             :selected (not (:mode (:battle-status i)))
             :on-selected-changed {:event/type :spring-lobby/battle-spectate-change
                                   :client (when-not singleplayer client)
                                   :is-me (= (:username i) username)
                                   :is-bot (-> i :user :client-status :bot)
                                   :id i}
             :disable (or (not username)
                          (not (or (and am-host (:mode (:battle-status i)))
                                   (= (:username i) username)
                                   (= (:owner i) username))))}}})}}
      {:fx/type :table-column
       :text "Faction"
       :cell-value-factory identity
       :cell-factory
       {:fx/cell-type :table-cell
        :describe
        (fn [i]
          {:text ""
           :graphic
           {:fx/type ext-recreate-on-key-changed
            :key (u/nickname i)
            :desc
            {:fx/type :combo-box
             :value (->> i :battle-status :side (get sides) str)
             :on-value-changed {:event/type :spring-lobby/battle-side-changed
                                :client (when-not singleplayer client)
                                :is-me (= (:username i) username)
                                :is-bot (-> i :user :client-status :bot)
                                :id i
                                :sides sides}
             :items (->> sides seq (sort-by first) (map second))
             :disable (or (not username)
                          (not (or am-host
                                   (= (:username i) username)
                                   (= (:owner i) username))))}}})}}
      #_
      {:fx/type :table-column
       :editable false
       :text "Rank"
       :cell-value-factory (comp u/to-number :rank :client-status :user)
       :cell-factory
       {:fx/cell-type :table-cell
        :describe (fn [rank] {:text (str rank)})}}
      {:fx/type :table-column
       :text "Country"
       :cell-value-factory (comp :country :user)
       :cell-factory
       {:fx/cell-type :table-cell
        :describe (fn [country] {:text (str country)})}}
      {:fx/type :table-column
       :text "Bonus"
       :cell-value-factory identity
       :cell-factory
       {:fx/cell-type :table-cell
        :describe
        (fn [i]
          {:text ""
           :graphic
           {:fx/type ext-recreate-on-key-changed
            :key (u/nickname i)
            :desc
            {:fx/type :text-field
             :disable (not am-host)
             :text-formatter
             {:fx/type :text-formatter
              :value-converter :integer
              :value (int (or (:handicap (:battle-status i)) 0))
              :on-value-changed {:event/type :spring-lobby/battle-handicap-change
                                 :client (when-not singleplayer client)
                                 :is-bot (-> i :user :client-status :bot)
                                 :id i}}}}})}}]}))
