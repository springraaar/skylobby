(ns skylobby.fx.maps
  (:require
    [cljfx.api :as fx]
    [cljfx.ext.node :as fx.ext.node]
    [clojure.java.io :as io]
    [clojure.string :as string]
    [skylobby.fs :as fs]
    skylobby.fx
    [skylobby.fx.ext :refer [ext-recreate-on-key-changed]]
    [skylobby.fx.font-icon :as font-icon]
    [skylobby.fx.sub :as sub]
    [skylobby.fx.tooltip-nofocus :as tooltip-nofocus]
    [skylobby.resource :as resource]
    [skylobby.util :as u]
    [taoensso.tufte :as tufte]))


(set! *warn-on-reflection* true)


(def maps-window-width 690)
(def maps-window-height 800)


(def map-browse-image-size 162)
(def map-browse-box-height 224)


(def known-maps
  ["Altair_Crossing_V4"
   "Avalanche 3.4"
   "Nuclear Winter Bar 1.2"
   "Quicksilver Remake 1.24"
   "Red Comet Remake 1.8"])


(def dice-options
  (take 6 (iterate inc 0)))


(defn spring-root-path-sub [_context spring-isolation-dir]
  (fs/canonical-path spring-isolation-dir))

(defn dice-icon-sub [context]
  (let [
        now (or (fx/sub-val context :now) 0)
        dice-icon (str "mdi-dice-" (inc (nth dice-options (mod now (count dice-options)))) ":16:white")]
    dice-icon))

(defn maps-view-impl
  [{:fx/keys [context]
    :keys [action-disable-rotate disable flow map-name on-value-changed spring-isolation-dir suggest text-only]
    :or {flow true}}]
  (let [
        {:keys [maps maps-by-name spring-root-path]} (fx/sub-ctx context sub/spring-resources spring-isolation-dir)
        selected-map (:file (get maps-by-name map-name))
        selected-map-file (:file selected-map)
        on-value-changed (or on-value-changed
                             {:event/type :spring-lobby/assoc-in
                              :path [:by-spring-root spring-root-path :map-name]})
        dice-icon (fx/sub-ctx context dice-icon-sub)]
    (merge
      {:fx/type (if flow :flow-pane :h-box)}
      (when-not flow {:alignment :center-left})
      {:children
       (concat
         [{:fx/type :label
           :alignment :center-left
           :text " Map: "}]
         (if (empty? maps)
           (if suggest
             (let [downloadables (vals (fx/sub-val context :downloadables-by-url))
                   http-download (fx/sub-val context :http-download)
                   http-download-tasks (fx/sub-ctx context skylobby.fx/tasks-of-type-sub :spring-lobby/http-downloadable)]
               [{:fx/type :flow-pane
                 :children
                 (mapv
                   (fn [map-name]
                     (let [downloadable (->> downloadables
                                             (filter (partial resource/could-be-this-map? map-name))
                                             first)
                           download (get http-download (:download-url downloadable))
                           running (:running download)
                           task (->> http-download-tasks
                                     (filter (comp (partial resource/same-resource-filename? downloadable) :downloadable))
                                     seq)]
                       {:fx/type :button
                        :text (cond
                                running (u/download-progress download)
                                task "Qeueued..."
                                :else
                                (str "Get " map-name))
                        :disable (boolean (or (not downloadable) running task))
                        :on-action {:event/type :spring-lobby/add-task
                                    :task {:spring-lobby/task-type :spring-lobby/http-downloadable
                                           :downloadable downloadable
                                           :spring-isolation-dir spring-isolation-dir}}}))
                   known-maps)}])
             [{:fx/type :label
               :text " No maps"}])
           (let [
                 filtered-map-names (fx/sub-ctx context sub/filtered-maps spring-isolation-dir)]
             [
              {:fx/type ext-recreate-on-key-changed
               :key (str [map-name text-only])
               :desc
               (if text-only
                 {:fx/type :label
                  :text map-name}
                 {:fx/type :combo-box
                  :prompt-text (or map-name " < pick a map > ")
                  :value map-name
                  :items filtered-map-names
                  :disable (boolean disable)
                  :on-value-changed on-value-changed
                  :button-cell
                  (fn [map-name]
                    {:text (str map-name)})
                  :cell-factory
                  {:fx/cell-type :list-cell
                   :describe
                   (fn [map-name]
                     {:text (str map-name)})}
                  :on-key-pressed {:event/type :spring-lobby/maps-key-pressed}
                  :on-hidden {:event/type :spring-lobby/dissoc
                              :key :map-input-prefix}})}]))
         [{:fx/type :button
           :text ""
           :on-action {:event/type :spring-lobby/show-maps-window
                       :on-change-map on-value-changed}
           :graphic
           {:fx/type font-icon/lifecycle
            :icon-literal "mdi-magnify:16:white"}}
          {:fx/type fx.ext.node/with-tooltip-props
           :props
           {:tooltip
            {:fx/type tooltip-nofocus/lifecycle
             :show-delay skylobby.fx/tooltip-show-delay
             :text "View map file"}}
           :desc
           {:fx/type :button
            :on-action {:event/type :spring-lobby/desktop-browse-dir
                        :file (or selected-map-file
                                  (fs/file spring-isolation-dir "maps"))}
            :graphic
            {:fx/type font-icon/lifecycle
             :icon-literal "mdi-folder:16:white"}}}]
         (when (seq maps)
           [{:fx/type fx.ext.node/with-tooltip-props
             :props
             {:tooltip
              {:fx/type tooltip-nofocus/lifecycle
               :show-delay skylobby.fx/tooltip-show-delay
               :text "Random map"}}
             :desc
             {:fx/type :button
              :disable (boolean disable)
              :on-action {:event/type :spring-lobby/random-map
                          :maps maps
                          :on-value-changed on-value-changed}
              :graphic
              {:fx/type font-icon/lifecycle
               :icon-literal dice-icon}}}])
         [{:fx/type fx.ext.node/with-tooltip-props
           :props
           {:tooltip
            {:fx/type tooltip-nofocus/lifecycle
             :show-delay skylobby.fx/tooltip-show-delay
             :text "Reload maps"}}
           :desc
           {:fx/type :button
            :disable (boolean
                       (or
                         (seq
                           (fx/sub-ctx context skylobby.fx/tasks-of-type-sub :spring-lobby/refresh-maps))
                         (seq
                           (fx/sub-ctx context skylobby.fx/tasks-of-type-sub :spring-lobby/clear-map-details))))
            :on-action {:event/type :spring-lobby/add-task
                        :task {:spring-lobby/task-type :spring-lobby/clear-map-details
                               :spring-root spring-isolation-dir
                               :map-resource selected-map}}
            :graphic
            {:fx/type font-icon/lifecycle
             :icon-literal "mdi-refresh:16:white"}}}]
        (when action-disable-rotate
          [{:fx/type fx.ext.node/with-tooltip-props
            :props
            {:tooltip
             {:fx/type tooltip-nofocus/lifecycle
              :show-delay skylobby.fx/tooltip-show-delay
              :text "Disable map rotation"}}
            :desc
            {:fx/type :button
             :on-action action-disable-rotate
             :graphic
             {:fx/type font-icon/lifecycle
              :icon-literal "mdi-lock:16:white"}}}]))})))


(defn maps-view [state]
  (tufte/profile {:dynamic? true
                  :id :skylobby/ui}
    (tufte/p :maps-view
      (maps-view-impl state))))


(defn maps-window-impl
  [{:fx/keys [context]
    :keys [screen-bounds]}]
  (let [
        show-maps (fx/sub-val context :show-maps)]
    {:fx/type :stage
     :showing (boolean show-maps)
     :title (str u/app-name " Maps")
     :icons skylobby.fx/icons
     :on-close-request {:event/type :spring-lobby/dissoc
                        :key :show-maps}
     :on-shown {:event/type :app/popup-window-shown-center}
     :width (skylobby.fx/fitwidth screen-bounds maps-window-width)
     :height (skylobby.fx/fitheight screen-bounds maps-window-height)
     :min-width maps-window-width
     :min-height maps-window-height
     :scene
     {:fx/type :scene
      :stylesheets (fx/sub-ctx context skylobby.fx/stylesheet-urls-sub)
      :root
      (if show-maps
        (let [
              filter-maps-name (fx/sub-val context :filter-maps-name)
              on-change-map (fx/sub-val context :on-change-map)
              server-key (fx/sub-ctx context skylobby.fx/selected-tab-server-key-sub)
              spring-root (fx/sub-ctx context sub/spring-root server-key)
              filtered-map-names (fx/sub-ctx context sub/filtered-maps spring-root filter-maps-name)
              details-by-map-name (fx/sub-ctx context sub/details-filtered-by-map-name spring-root filter-maps-name)
              total-files (count filtered-map-names)
              total-maps (->> details-by-map-name
                              (vals)
                              (map #(or (:map-name-wv %) (:map-name %)))
                              (distinct)
                              (count))
              total-size (->> filtered-map-names 
                              (map #(get-in details-by-map-name [% :map-file-size-b] 0)) 
                              (reduce +))]
          {:fx/type :v-box
           :children
           [{:fx/type :h-box
             :alignment :center-left
             :style {:-fx-font-size 16}
             :children
             (concat
               [{:fx/type :label
                 :text " Filter: "}
                {:fx/type :text-field
                 :text (str filter-maps-name)
                 :prompt-text "Filter by name, etc."
                 :on-text-changed {:event/type :spring-lobby/assoc
                                   :key :filter-maps-name}
                 :tooltip
                    {:fx/type tooltip-nofocus/lifecycle
                     :show-delay skylobby.fx/tooltip-show-delay
                     :text "Example: \"ridge s>10 s<=20 a:john\" matches maps containing \"ridge\" in name\nwith size > 10x10 and <= 20x20 and author containing \"john\""}}
                ]
               (when-not (string/blank? filter-maps-name)
                 [{:fx/type fx.ext.node/with-tooltip-props
                   :props
                   {:tooltip
                    {:fx/type tooltip-nofocus/lifecycle
                     :show-delay skylobby.fx/tooltip-show-delay
                     :text "Clear filter"}}
                   :desc
                   {:fx/type :button
                    :on-action {:event/type :spring-lobby/dissoc
                                :key :filter-maps-name}
                    :graphic
                    {:fx/type font-icon/lifecycle
                     :icon-literal "mdi-close:16:white"}}}])
               [{:fx/type :region
                 :h-box/hgrow :always}
                {:fx/type :label
                 :text (str total-maps " maps (" total-files " files | "
                            (format "%.2f" (/ total-size 1024.0 1024.0 1024.0)) " GB)  ")
                 :style {:-fx-margin-left 10
                         :-fx-text-fill :lightgray}}]
           
               )}
            {:fx/type :scroll-pane
             :fit-to-width true
             :content
             {:fx/type :flow-pane
              :vgap 5
              :hgap 5
              :padding 5
              :children
              (mapv
                (fn [map-name]
                   (let [map-details (get details-by-map-name map-name {})]
                   {:fx/type :button 
                   :style
                   {:-fx-min-width map-browse-image-size
                    :-fx-max-width map-browse-image-size
                    :-fx-min-height map-browse-box-height
                    :-fx-max-height map-browse-box-height}
                   :on-action {:event/type :spring-lobby/map-window-action
                               :on-change-map (assoc on-change-map :map-name map-name :value map-name)}
                   :tooltip
                   {:fx/type tooltip-nofocus/lifecycle
                    :show-delay skylobby.fx/tooltip-show-delay
                    :content-display :top
                    :graphic
                    {:fx/type :v-box
                     :spacing 5
                     :style {
                        :-fx-max-width (+ u/minimap-size 10)
                        :-fx-max-height (* u/minimap-size 1.8)
                        }
                     :children
                     (concat
                       [{:fx/type :image-view
                         :image {:url (-> map-name fs/minimap-image-cache-file io/as-url str)
                         :background-loading true}
                         :preserve-ratio true
                         :style
                         {:-fx-min-width u/minimap-size
                          :-fx-max-width u/minimap-size
                          :-fx-min-height u/minimap-size
                          :-fx-max-height u/minimap-size}}
                        {:fx/type :label
                         :text map-name
                         :alignment :center
                         :wrap-text true
                         :style {:-fx-font-size 16  
                                 :-fx-font-weight :bold}}
                        {:fx/type :label
                         :text (let [width (:map-width map-details)
                           height (:map-height map-details)]
                           (if (or (nil? width) (nil? height))
                             "Size: ?"
                             (str "Size: " width " x " height)))
                         :style {:-fx-font-size 14}}
                        {:fx/type :label
                         :text (str "Author: " (get-in map-details [:map-author] "?") )
                         :wrap-text true
                         :style {:-fx-font-size 14}}
                        {:fx/type :label
                         :text (let [file-size (get-in map-details [:map-file-size-b] 0)]
                          (if (zero? file-size)
                            "File size: ?"
                            (str "File size: " (format "%.2f" (/ file-size 1024.0 1024.0)) " MB")))
                         :style {:-fx-font-size 14}}
                        {:fx/type :label
                         :text (str "Description: " (get-in map-details [:map-description] "?") )
                         :wrap-text true
                         :style {:-fx-font-size 13 :-fx-wrap-text true}
                         }
                        ])}}
                   :graphic
                   {:fx/type :v-box
                    :alignment :center
                    :children
                    [
                     {:fx/type :pane
                      :v-box/vgrow :always}
                     {:fx/type :image-view
                      :image {:url (-> map-name fs/minimap-image-cache-file io/as-url str)
                              :background-loading true
                              :preserve-ratio true
                              :requested-width map-browse-image-size
                              :requested-height map-browse-image-size}
                      :fit-width map-browse-image-size
                      :fit-height map-browse-image-size
                      :preserve-ratio true}
                     {:fx/type :pane
                      :v-box/vgrow :always}
                     {:fx/type :label
                      :style {:-fx-font-size 12}
                      :text (str " " map-name)
                      :wrap-text true}
                     {:fx/type :label
                      :alignment :center-left
                      :text (when-let [{:keys [map-width map-height]} map-details]
                            (if (and map-width map-height)
                              (str map-width " x " map-height)
                              "?"))
                      :wrap-text true}
                     ]}}))
                (fx/sub-ctx context sub/filtered-maps spring-root filter-maps-name))}}]})
        {:fx/type :pane
         :pref-width maps-window-width
         :pref-height maps-window-height})}}))

(defn maps-window [state]
  (tufte/profile {:dyanmic? true
                  :id :skylobby/ui}
    (tufte/p :maps-view
      (maps-window-impl state))))
