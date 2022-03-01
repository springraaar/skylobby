(ns skylobby.fx.event.direct
  (:require
    [skylobby.direct :as direct]
    [skylobby.direct.client :as direct.client]
    [skylobby.fs :as fs]
    [skylobby.util :as u]
    [taoensso.sente :as sente]
    [taoensso.timbre :as log]))


(defn add-methods
  [multifn state-atom] ; TODO need to move event handler out of spring-lobby ns
  (defmethod multifn ::host
    [{:keys [direct-connect-password direct-connect-port direct-connect-username]}]
    (swap! state-atom assoc-in [:login-error :direct-host] nil)
    (future
      (try
        (let [server-key {:server-type :direct
                          :protocol :skylobby
                          :host true
                          :hostname "localhost"
                          :port direct-connect-port
                          :username direct-connect-username}
              server-close-fn (direct/start-direct-connect state-atom server-key {:direct-connect-port direct-connect-port})
              {:keys [by-spring-root spring-isolation-dir]} @state-atom
              {:keys [engine-version map-name mod-name]} (get by-spring-root (fs/canonical-path spring-isolation-dir))
              server-data {:battle {:battle-id :direct
                                    :scripttags {"game" {"startpostype" 1}}
                                    :users {direct-connect-username
                                            {:battle-status
                                             {:ally 0
                                              :id 0
                                              :mode true
                                              :ready true
                                              :side 0}
                                             :team-color (u/random-color)}}}
                           :battles {:direct
                                     {:host-username direct-connect-username
                                      :battle-map map-name
                                      :battle-modname mod-name
                                      :battle-version engine-version}}
                           :password direct-connect-password
                           :server-close-fn server-close-fn
                           :username direct-connect-username}]
          (if server-close-fn
            (swap! state-atom update-in [:by-server server-key] merge server-data)
            (swap! state-atom update :by-server dissoc server-key)))
        (catch Exception e
          (log/error e "Error starting direct connect server")))))
  (defmethod multifn ::join
    [{:keys [direct-connect-ip direct-connect-port direct-connect-username]}]
    (swap! state-atom assoc-in [:login-error :direct-client] nil)
    (future
      (try
        (let [server-key {:server-type :direct
                          :protocol :skylobby
                          :host false
                          :hostname direct-connect-ip
                          :port direct-connect-port
                          :username direct-connect-username}
              ;; Serializtion format, must use same val for client + server:
              packer :edn
              {:keys [ch-recv send-fn] :as client}
              (sente/make-channel-socket-client!
                "/chsk"
                nil ; ?csrf-token
                {
                 :host direct-connect-ip
                 :port direct-connect-port
                 :protcol :http
                 :type   :ws
                 :packer packer
                 :wrap-recv-evs? false})
              msg-handler (direct.client/event-msg-handler state-atom server-key)
              client-close-fn (sente/start-client-chsk-router! ch-recv msg-handler)
              client-close-fn (fn []
                                (send-fn [::direct.client/close])
                                (client-close-fn))
              data {:battle {:battle-id :direct
                             :users {}}
                    :battles {:direct {}}
                    :client client
                    :client-close-fn client-close-fn
                    :username direct-connect-username}]
          (swap! state-atom assoc-in [:by-server server-key] data))
        (catch Exception e
          (log/error e "Error joining direct connect"))))))
