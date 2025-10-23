(ns skylobby.task
  (:require
    [taoensso.timbre :as log]))


(set! *warn-on-reflection* true)


(def index-tasks
  #{:spring-lobby/refresh-engines
    :spring-lobby/refresh-mods
    :spring-lobby/refresh-maps})

(def resource-tasks
  #{
    :spring-lobby/map-details
    :spring-lobby/mod-details
    :spring-lobby/update-cached-minimaps})

(def download-tasks
  #{
    :spring-lobby/download-and-extract
    :spring-lobby/download-springfiles
    :spring-lobby/extract-7z
    :spring-lobby/import
    :spring-lobby/http-downloadable})

(def rapid-tasks
  #{
    :spring-lobby/rapid-download
    :spring-lobby/update-rapid
    :spring-lobby/update-rapid-packages
    :spring-lobby/delete-corrupt-rapid})

(def task-kinds
  [:spring-lobby/index-task
   :spring-lobby/resource-task
   :spring-lobby/download-task
   :spring-lobby/rapid-task
   :spring-lobby/other-task])

(defn task-kind [{:spring-lobby/keys [task-type]}]
  (cond
    (contains? index-tasks task-type) :spring-lobby/index-task
    (contains? resource-tasks task-type) :spring-lobby/resource-task
    (contains? download-tasks task-type) :spring-lobby/download-task
    (contains? rapid-tasks task-type) :spring-lobby/rapid-task
    :else :spring-lobby/other-task))

(defn print-task-totals-by-kind [state]
  (let [tasks-by-kind (:tasks-by-kind @state)
        index-count (count (:spring-lobby/index-task tasks-by-kind))
        resource-count (count (:spring-lobby/resource-task tasks-by-kind))
        download-count (count (:spring-lobby/download-task tasks-by-kind))
        rapid-count (count (:spring-lobby/rapid-task tasks-by-kind))
        other-count (count (:spring-lobby/other-task tasks-by-kind))]
    (log/info (str "Queued tasks : index=" index-count " resource=" resource-count " download=" download-count " rapid=" rapid-count " other=" other-count))))

(defn add-task-state [state task]
  (if task
    (let [task-kind (task-kind task)]
      (log/info "Adding task" (pr-str task) "to" task-kind)
      (update-in state [:tasks-by-kind task-kind]
        (fn [tasks]
          (set (conj tasks task)))))
    (do
      (log/warn "Attempt to add nil task" task)
      state)))

(defn task-matches? [existing-task new-task]
  (let [existing-keys (set (keys existing-task))
        new-keys (set (keys new-task))
        common-keys (clojure.set/intersection existing-keys new-keys)]
    (every? 
      (fn [k] 
        (= (get existing-task k) 
           (get new-task k))) 
      common-keys)))

; add single task, but check and skip duplicates (thread-safe)
(defn add-task! [state-atom task]
  (swap! state-atom 
    (fn [state]
      (let [task-kind (task-kind task)
        existing-tasks (get-in state [:tasks-by-kind task-kind])
        task-exists? (some #(task-matches? % task) existing-tasks)]
        (if task-exists?
          (do 
            (log/debug "Task already exists, skipping duplicate" (pr-str task))
            state)
          (add-task-state state task)))))
  (print-task-totals-by-kind state-atom))

(defn add-multiple-tasks [tasks-by-kind new-tasks]
  (reduce-kv
    (fn [m k new-tasks]
      (update m k (fn [existing]
                    (set (concat new-tasks existing)))))
    tasks-by-kind
    (group-by task-kind new-tasks)))

; add list of tasks, regardless of existing tasks, for now (thread safe)
(defn add-tasks! [state-atom new-tasks]
  (log/info "Adding tasks" (pr-str new-tasks))
  (swap! state-atom update :tasks-by-kind add-multiple-tasks new-tasks)
  (print-task-totals-by-kind state-atom))


(defn all-tasks [{:keys [current-tasks tasks-by-kind]}]
  (->> tasks-by-kind
       (mapcat second)
       (concat (vals current-tasks))
       (filter some?)
       doall))
