(ns skylobby.fx
  (:require
    [cljfx.api :as fx]
    [cljfx.css :as css]
    [clojure.java.io :as io]
    [skylobby.fs :as fs]
    [skylobby.resource :as resource]
    [skylobby.util :as u]
    [taoensso.timbre :as log])
  (:import
    (javafx.stage Screen)
    (org.apache.commons.collections4 ComparatorUtils)))


(set! *warn-on-reflection* true)


(def tooltip-show-delay [200 :ms])
(def tooltip-show-delay-slow [500 :ms]) ; some things are jarring otherwise


(def case-insensitive-natural-comparator
  (.thenComparing
    String/CASE_INSENSITIVE_ORDER
    ComparatorUtils/NATURAL_COMPARATOR))


(def divider-positions
  (atom {}))
(def window-states
  (atom {}))

(defn window-changed [window k v]
  (swap! window-states update window
    (fn [window-state]
      (if (and (:maximized window-state)
               (not= :maximized k))
        window-state ; don't save size if maximized
        (assoc window-state k v)))))

(defn add-maximized-listener [window-key ^javafx.stage.Stage node]
  (let [maximized-property (.maximizedProperty node)]
    (.addListener maximized-property
      (reify javafx.beans.value.ChangeListener
        (changed [_this _observable _old-value new-value]
          (window-changed window-key :maximized new-value))))))

(defn add-divider-listener [^javafx.scene.control.SplitPane node divider-key]
  (let [dividers (.getDividers node)]
    (when-let [^javafx.scene.control.SplitPane$Divider divider (first dividers)]
      (.addListener (.positionProperty divider)
        (reify javafx.beans.value.ChangeListener
          (changed [_this _observable _old-value new-value]
            (swap! divider-positions assoc divider-key new-value)))))))

(def monospace-font-family
  (if (fs/windows?)
    "Consolas"
    "monospace"))


;; ── Design tokens ────────────────────────────────────────────────
;; Type scale (px). base = default body text.
(def type-scale
  {:xs 12 :sm 13 :base 14 :md 16 :lg 18 :xl 22 :xxl 28})

;; Spacing scale (px), addressed by step number.
(def space-scale
  {:1 4 :2 8 :3 12 :4 16 :5 24 :6 32})

;; ── Per-theme surface/colour ramps ───────────────────────────────
;; surface-0 = app background (deepest), surface-3 = raised/hover/selected.
(def black-ramp
  {:surface-0 "rgb(18,18,18)"
   :surface-1 "rgb(28,28,28)"
   :surface-2 "rgb(40,40,40)"
   :surface-3 "rgb(54,54,54)"
   :border    "rgb(64,64,64)"
   :focus     "rgb(120,120,130)"
   :selection "rgb(72,72,72)"
   :selection-unfocused "rgb(55,55,55)"
   :text-on-dark  "rgb(228,228,228)"
   :text-on-light "rgb(20,20,20)"
   :text-2    "rgb(150,150,150)"
   :row-odd   "rgb(30,30,30)"
   :row-even  "rgb(20,20,20)"
   :thumb     "rgb(90,90,90)"
   :thumb-hover "rgb(110,110,110)"
   :tab-highlight "#ffd700"
   :tab-selected-accent "rgb(160,160,160)"
   :icon "rgb(225,225,225)"})

(def grey-ramp
  {:surface-0 "rgb(60,60,60)"
   :surface-1 "rgb(74,74,74)"
   :surface-2 "rgb(90,90,90)"
   :surface-3 "rgb(104,104,104)"
   :border    "rgb(120,120,120)"
   :focus     "rgb(170,170,170)"
   :selection "rgb(120,120,120)"
   :selection-unfocused "rgb(110,110,110)"
   :text-on-dark  "rgb(240,240,240)"
   :text-on-light "rgb(20,20,20)"
   :text-2    "rgb(200,200,200)"
   :row-odd   "rgb(70,70,70)"
   :row-even  "rgb(60,60,60)"
   :thumb     "rgb(120,120,120)"
   :thumb-hover "rgb(150,150,150)"
   :tab-highlight "#ffd700"
   :tab-selected-accent "rgb(160,160,160)"
   :icon "rgb(235,235,235)"})

(def light-ramp
  {:surface-0 "rgb(220,220,220)"
   :surface-1 "rgb(240,240,240)"
   :surface-2 "rgb(252,252,252)"
   :surface-3 "rgb(255,255,255)"
   :border    "rgb(200,200,200)"
   :focus     "rgb(90,110,140)"
   :selection "rgb(208,224,244)"
   :selection-unfocused "rgb(225,225,225)"
   :text-on-dark  "rgb(240,240,240)"
   :text-on-light "rgb(20,20,20)"
   :text-2    "rgb(90,90,90)"
   :row-odd   "rgb(236,236,236)"
   :row-even  "rgb(246,246,246)"
   :thumb     "rgb(150,150,150)"
   :thumb-hover "rgb(120,120,120)"
   :tab-highlight "#ffd700"
   :tab-selected-accent "rgb(100,100,100)"
   :icon "rgb(70,70,70)"})

;; ── Theme author API ─────────────────────────────────────────────
;; Override these in custom-css.edn (loaded via the :css sub) to reskin
;; without fighting the global -fx-base cascade:
;;   .root                          app surfaces (-fx-background/-fx-base/...)
;;   .tab / .tab:selected           tab states (separate from buttons)
;;   .tab-header-background         the strip behind tab headers
;;   .skylobby-card                 raised grouped panel (welcome columns, etc.)
;;   .skylobby-secondary            flat/quiet button variant (de-emphasised nav)
;;   .skylobby-primary              prominent call-to-action button (Start Game)
;;   .combo-box / .check-box / .text-field   inputs, fixed normal font size
;;   .button                        buttons only
;;   .table-view                    table background + outer border
;;   .table-view .column-header-background   table header strip
;;   .table-row-cell                table border lines
;;   .table-row-cell:odd / :even    row striping
;;   .separator:horizontal .line / :vertical .line   region separators
;;   .ikonli-font-icon              all icon glyphs (theme-appropriate colour)
;; Custom CSS still wins over these built-ins (it is appended last in
;; stylesheet-urls), so this list is the stable set of handles.

(defn theme-data
  "Build a cljfx.css style-data map from a surface/colour ramp.
   Element selectors are deliberately separate (tabs are NOT styled via the
   button selector) so theme authors can target each independently."
  [{:keys [surface-0 surface-1 surface-2 surface-3 border focus
           selection selection-unfocused text-on-dark text-on-light text-2
           row-odd row-even thumb thumb-hover tab-selected-accent icon]}]
  {".root"
   {:-fx-base surface-1
    :-fx-accent surface-3
    :-fx-background surface-0
    :-fx-control-inner-background surface-2
    :-fx-selection-bar-non-focused selection-unfocused
    :-fx-focus-color focus
    :-fx-faint-focus-color "transparent"
    :-fx-text-base-color (str "ladder(-fx-base, " text-on-dark " 49%, " text-on-light " 50%)")}
   ;; Tabs — own selectors, separate from .button
   ".tab"
   {:-fx-base surface-2
    :-fx-background surface-1
    :-fx-accent "rgb(0,0,0)"}
   ".tab:selected"
   {:-fx-background surface-0
    :-fx-accent tab-selected-accent}
   ".tab-header-background"
   {:-fx-background-color surface-0}
   ".skylobby-card"
   {:-fx-background-color surface-2
    :-fx-background-radius "6"
    :-fx-border-color border
    :-fx-border-radius "6"
    :-fx-border-width "1"
    :-fx-padding "16"}
   ;; Buttons — own selector, subtle rounding for definition
   ".button"
   {:-fx-background-radius "3"}
   ".ikonli-font-icon"
   {:-fx-icon-color icon}
   ".skylobby-secondary"
   {:-fx-background-color "transparent"
    :-fx-border-color "transparent"}
   ".skylobby-secondary:hover"
   {:-fx-background-color surface-2}
   ".skylobby-primary"
   {:-fx-base "#ffd700"
    :-fx-background "#ffd700"}
   ; A disabled CTA (e.g. Start Game while not synced) must not read as a
   ; live gold button - fall back to a normal neutral control surface.
   ".skylobby-primary:disabled"
   {:-fx-base surface-2
    :-fx-background surface-2}
   ;; Region separators / borders
   ".separator:horizontal .line"
   {:-fx-border-color (str border " transparent transparent transparent")
    :-fx-border-width "1"}
   ".separator:vertical .line"
   {:-fx-border-color (str "transparent " border " transparent transparent")
    :-fx-border-width "1"}
   ;; Inputs / text areas
   ".styled-text-area"
   {:-fx-background-color surface-0}
   ".text-field"
   {:-fx-prompt-text-fill text-2}
   ;; Scroll bars
   ".scroll-bar .thumb"
   {:-fx-background-radius "2, 1"
    :-fx-background-insets "3, 4"}
   ".scroll-bar:vertical .thumb"
   {:-fx-background-color (str thumb ",linear-gradient(to right,derive(-fx-base,30%),derive(-fx-base,-30%))")}
   ".scroll-bar:horizontal .thumb"
   {:-fx-background-color (str thumb ",linear-gradient(to bottom,derive(-fx-base,30%),derive(-fx-base,-30%))")}
   ".scroll-bar:vertical .thumb:hover"
   {:-fx-background-color (str thumb-hover ",linear-gradient(to right,derive(-fx-base,50%),derive(-fx-base,-10%))")}
   ".scroll-bar:horizontal .thumb:hover"
   {:-fx-background-color (str thumb-hover ",linear-gradient(to bottom,derive(-fx-base,50%),derive(-fx-base,-10%))")}
   ".combo-box .arrow-button .arrow,.scroll-bar .increment-arrow,.scroll-bar .decrement-arrow"
   {:-fx-background-color text-2}
   ;; Links
   ".doc-link, .hyperlink"
   {:-fx-text-fill "rgb(70,70,255)"
    :-fx-cursor "hand"
    :-fx-underline true}
   ".doc-link:hover, .hyperlink:hover"
   {:-fx-text-fill "rgb(70,130,255)"}
   ;; Tables — background + a dedicated border-line treatment
   ".table-view"
   {:-fx-background-color surface-1
    :-fx-border-color border
    :-fx-border-width "1"}
   ".table-view .column-header-background"
   {:-fx-background-color surface-2}
   ".table-view .column-header .label"
   {:-fx-text-fill "-fx-text-base-color"
    :-fx-font-weight "bold"}
   ".table-row-cell"
   {:-fx-border-color (str "transparent transparent " border " transparent")
    :-fx-table-cell-border-color border}
   ".table-row-cell:odd"
   {:-fx-background-color row-odd}
   ".table-row-cell:even"
   {:-fx-background-color row-even}
   ".table-row-cell:odd:selected,.table-row-cell:even:selected"
   {:-fx-background-color selection}
   ".table-row-cell:odd:empty, .table-row-cell:even:empty"
   {:-fx-background-color "transparent"}})

;; Theme-specific extras merged over the generated base.
(def grey-style-data
  (merge (theme-data grey-ramp)
         {".check-box" {:-fx-mark-color "white"}
          ".skylobby"
          {"-tab" {"-focus" {:-fx-background (:tab-highlight grey-ramp)
                             :-fx-base (:tab-highlight grey-ramp)}}}}))

(def black-style-data
  (merge (theme-data black-ramp)
         {".skylobby"
          {"-chat" {"-user-list" {:-fx-text-fill "lightgrey"}}
           "-tab" {"-focus" {:-fx-background (:tab-highlight black-ramp)
                             :-fx-base (:tab-highlight black-ramp)}}}}))

(def light-style-data
  (merge (theme-data light-ramp)
         {".scroll-bar:vertical .thumb"
          {:-fx-background-color "rgb(150,150,150),linear-gradient(to right,derive(-fx-base,-10%),derive(-fx-base,-50%))"}
          ".scroll-bar:horizontal .thumb"
          {:-fx-background-color "rgb(150,150,150),linear-gradient(to bottom,derive(-fx-base,-10%),derive(-fx-base,-50%))"}
          ".scroll-bar:vertical .thumb:hover"
          {:-fx-background-color "rgb(255,255,255),linear-gradient(to right,derive(-fx-base,+30%),derive(-fx-base,-10%))"}
          ".scroll-bar:horizontal .thumb:hover"
          {:-fx-background-color "rgb(255,255,255),linear-gradient(to bottom,derive(-fx-base,+30%),derive(-fx-base,-10%))"}
          ".skilluncertainty0" {:-fx-text-fill "rgb(0, 0, 0)"}
          ".skilluncertainty1" {:-fx-text-fill "rgb(50, 40, 30)"}
          ".skilluncertainty2" {:-fx-text-fill "rgb(80, 60, 40)"}
          ".skilluncertainty3" {:-fx-text-fill "rgb(120, 80, 40)"}
          ".combo-box-popup .list-cell:selected"
          {:-fx-background-color "rgb(240, 240, 240)" :-fx-text-fill "rgb(10, 10, 10)"}
          ".combo-box-popup .list-cell:hover"
          {:-fx-background-color "rgb(210,210,210)" :-fx-text-fill "rgb(20, 20, 20)"}
          ".menu-item:hover"
          {:-fx-background-color "rgb(230,230,230)" :-fx-text-fill "rgb(10, 10, 10)"}
          ".skylobby"
          {"-chat"
           {"-message"     {:-fx-fill "black"}
            "-username-ex" {:-fx-fill "rgb(20,140,140)"}
            "-message-ex"  {:-fx-fill "rgb(20,140,140)"}
            "-user-list"   {:-fx-text-fill "black"}}
           "-console"
           {"-message" {:-fx-fill "black"}}
           "-tab" {"-focus" {:-fx-background (:tab-highlight light-ramp)
                             :-fx-base (:tab-highlight light-ramp)}}}}))
(def default-style-data black-style-data)

(def style-presets
  {
   "black" black-style-data
   "grey" grey-style-data
   "light" light-style-data})


(def default-style
  (css/register ::default black-style-data))

; so that themes can override
(def default-classes
  {
   ; text-fill restores theme-aware colour for nodes whose base style-class
   ; (label/hyperlink/...) was replaced when the typography class was applied
   ".skylobby-h1"
   {:-fx-font-size 22
    :-fx-text-fill "-fx-text-base-color"}
   ".skylobby-h2"
   {:-fx-font-size 18
    :-fx-text-fill "-fx-text-base-color"}
   ".skylobby-body"
   {:-fx-font-size 16
    :-fx-text-fill "-fx-text-base-color"}
   ".skylobby-caption"
   {:-fx-font-size 13
    :-fx-text-fill "-fx-text-base-color"}
   ; Inputs render at one consistent "normal" size app-wide. A direct rule on
   ; the control beats any font-size inherited from a heading-sized container,
   ; so combos/checkboxes/fields stay uniform regardless of where they sit.
   ".combo-box"
   {:-fx-font-size 16}
   ".combo-box-popup .list-cell"
   {:-fx-font-size 16}
   ".check-box"
   {:-fx-font-size 16}
   ".text-field"
   {:-fx-font-size 16}
   ".skilluncertainty0"
   {:-fx-text-fill "rgb(255, 255, 255)"}
   ".skilluncertainty1"
   {:-fx-text-fill "rgb(255, 230, 220)"}
   ".skilluncertainty2"
   {:-fx-text-fill "rgb(255, 220, 200)"}
   ".skilluncertainty3"
   {:-fx-text-fill "rgb(255, 190, 140)"}
   ".styled-text-area"
   {:-fx-background-color "rgb(50, 50, 50)"}
   ".hyperlink"
   {:-fx-text-fill "rgb(128, 128, 128)"}
   ".skylobby"
   {"-players"
    {"-multiplayer"
     {:-fx-font-size 14
      "-nickname"
      {:-fx-font-size 16}}
     "-replay"
     {:-fx-font-size 14
      "-nickname"
      {:-fx-font-size 16}}
     "-singleplayer"
     {:-fx-font-size 20
      "-nickname"
      {:-fx-font-size 22}}}
    "-tab"
    {"-focus"
     {:-fx-background "#ffd700"
      :-fx-base "#ffd700"}}
    "-chat"
    {
     "-info"
     {:-fx-fill "grey"}
     "-time"
     {:-fx-fill "grey"}
     "-username"
     {:-fx-fill "royalblue"};"rgb(136,190,190)"} ; kept royalblue but it's a bit too close to the new hyperlink color
     "-username-me"
     {:-fx-fill "orange"}
     "-username-ex"
     {:-fx-fill "cyan"}
     "-username-join"
     {:-fx-fill "grey"}
     "-username-leave"
     {:-fx-fill "grey"}
     "-message"
     {:-fx-fill "white"}
     "-message-ex"
     {:-fx-fill "cyan"}
     "-message-url"
     {; https://stackoverflow.com/a/4774037
      :-fx-fill "rgb(70,70,255)"
      :-fx-cursor "hand"
      :-fx-underline true}
     "-message-url:hover"
     {:-fx-fill "rgb(70,130,255)"}
     "-message-highlight"
     {:-fx-fill "red"}
     "-message-irc-00"
     {:-fx-fill "rgb(255,255,255)"}
     "-message-irc-01"
     {:-fx-fill "rgb(255,255,255)"}
     "-message-irc-02"
     {:-fx-fill "rgb(0,0,127)"}
     "-message-irc-03"
     {:-fx-fill "rgb(0,147,0)"}
     "-message-irc-04"
     {:-fx-fill "rgb(255,0,0)"}
     "-message-irc-05"
     {:-fx-fill "rgb(127,0,0)"}
     "-message-irc-06"
     {:-fx-fill "rgb(156,0,156)"}
     "-message-irc-07"
     {:-fx-fill "rgb(252,127,0)"}
     "-message-irc-08"
     {:-fx-fill "rgb(255,255,0)"}
     "-message-irc-09"
     {:-fx-fill "rgb(0,252,0)"}
     "-message-irc-10"
     {:-fx-fill "rgb(0,147,147)"}
     "-message-irc-11"
     {:-fx-fill "rgb(0,255,255)"}
     "-message-irc-12"
     {:-fx-fill "rgb(0,0,252)"}
     "-message-irc-13"
     {:-fx-fill "rgb(255,0,255)"}
     "-message-irc-14"
     {:-fx-fill "rgb(127,127,127)"}
     "-message-irc-15"
     {:-fx-fill "rgb(210,210,210)"}
     "-input"
     {:-fx-font-size 15}}
    "-console"
    {
     "-time"
     {:-fx-fill "grey"}
     "-source-server"
     {:-fx-fill "goldenrod"}
     "-source-client"
     {:-fx-fill "royalblue"}
     "-message"
     {:-fx-fill "white"}}
    "-spring-log"
    {
     "-err"
     {:-fx-fill "red"}
     "-out"
     {:-fx-fill "white"}}}})

(def default-classes-css
  (css/register ::default-classes default-classes))

(def current-css-filename "skylobby-current.css")

(defn file-backed-css
  "Register style-data under id, and also write the compiled CSS to a file in the
  app root, returning the registered map with ::css/url pointing at that file.
  JavaFX resolves relative url(...) references against the stylesheet's own
  location, so this lets custom CSS reference images (e.g. a welcome background)
  by paths relative to the app root, alongside custom-css.edn. A content-hash
  URL fragment is appended so hot-reload still triggers a reapply (JavaFX's file
  handler ignores the fragment when loading)."
  [id style-data]
  (let [registered (css/register id style-data)]
    (try
      (let [^java.io.File out (fs/file (fs/app-root) current-css-filename)]
        (spit out (slurp (::css/url registered)))
        (assoc registered ::css/url (str (.toURL (.toURI out)) "#" (hash style-data))))
      (catch Exception e
        (log/error e "Error writing file-backed CSS, falling back to in-memory")
        registered))))

(defn stylesheet-urls [css]
  [
   (str (::css/url default-classes-css))
   (str (::css/url (or css default-style)))])

(defn stylesheet-urls-sub [context]
  (stylesheet-urls (fx/sub-val context :css)))

(defn all-tasks-sub [context]
  (->> (fx/sub-val context :tasks-by-kind)
       (mapcat second)
       (concat (vals (fx/sub-val context :current-tasks)))
       (filter some?)
       doall))

(defn tasks-by-type-sub [context]
  (group-by :spring-lobby/task-type (fx/sub-ctx context all-tasks-sub)))

(defn tasks-of-type-sub [context task-type]
  (->> (fx/sub-ctx context all-tasks-sub)
       (filterv (comp #{task-type} :spring-lobby/task-type))))

(defn valid-servers-sub [context]
  (u/valid-servers (fx/sub-val context :by-server)))

(defn valid-server-keys-sub [context]
  (u/valid-server-keys (fx/sub-val context :by-server)))


(defn welcome-server-key-sub [context]
  (u/server-key {:server-url (fx/sub-val context (comp first :server))
                 :username (fx/sub-val context :username)}))

(defn selected-server-data-sub [context]
  (get (fx/sub-val context :by-server)
       (u/server-key {:server-url (first (fx/sub-val context :server))
                      :username (fx/sub-val context :username)})))

(defn selected-tab-server-key-sub [context]
  (let [selected-server-tab (fx/sub-val context :selected-server-tab)
        by-server-keys (fx/sub-val context (comp keys :by-server))]
    (or (->> by-server-keys
             (filter #{selected-server-tab})
             first)
        :local)))

(defn auto-servers-sub [context]
  (->> (fx/sub-val context :servers)
       (filterv (comp :auto-connect second))))

(defn server-key-set-sub [context]
  (fx/sub-val context (comp set keys :by-server)))

(defn auto-servers-not-connected-sub [context]
  (let [server-keys (fx/sub-ctx context server-key-set-sub)
        logins (fx/sub-val context :logins)]
    (->> (fx/sub-ctx context auto-servers-sub)
         (filter (comp :auto-connect second))
         (map (fn [[server-url _server-data]]
                (u/server-key
                  {:server-url server-url
                   :username (-> logins (get server-url) :username)})))
         (filter some?)
         (remove (fn [server-key] (contains? server-keys server-key)))
         doall)))


(defn map-details-sub [context map-key]
  (resource/cached-details (fx/sub-val context :map-details) map-key))

(defn mod-details-sub [context mod-key]
  (resource/cached-details (fx/sub-val context :mod-details) mod-key))

(defn replay-details-sub [context k]
  (get (fx/sub-val context :replay-details) k))

(defn spring-root-sub [context server-url]
  (let [servers (fx/sub-val context :servers)]
    (or (-> servers (get server-url) :spring-isolation-dir)
        (fx/sub-val context :spring-isolation-dir))))

(defn spring-root-resources-sub [context server-url]
  (let [spring-root (fx/sub-ctx context spring-root-sub server-url)]
    (resource/spring-root-resources spring-root (fx/sub-val context :by-spring-root))))

(defn spring-resources-sub [context spring-root]
  (resource/spring-root-resources spring-root (fx/sub-val context :by-spring-root)))

(defn selected-replay-sub [context]
  (or (get (fx/sub-val context :parsed-replays-by-path) (fs/canonical-path (fx/sub-val context :selected-replay-file)))
      (get (fx/sub-val context :online-bar-replays) (fx/sub-val context :selected-replay-id))))

(defn battle-channel-sub
  ([context server-key]
   (battle-channel-sub context server-key (fx/sub-val context get-in [:by-server server-key :battle :battle-id])))
  ([context server-key battle-id]
   (let [
         channel-name (fx/sub-val context get-in [:by-server server-key :battles battle-id :channel-name])]
     (or channel-name
         (str "__battle__" battle-id)))))


(def icons
  [(str (io/resource "icon16.png"))
   (str (io/resource "icon32.png"))
   (str (io/resource "icon48.png"))])
   ;(str (io/resource "icon64.png"))])
   ;(str (io/resource "icon128.png"))
   ;(str (io/resource "icon256.png"))
   ;(str (io/resource "icon512.png"))
   ;(str (io/resource "icon1024.png"))])


(def min-width 256)
(def min-height 256)


(defn get-screen-bounds []
  (let [screens (Screen/getScreens)
        xy (reduce
             (fn [{:keys [min-x min-y max-x max-y]} ^javafx.stage.Screen screen]
               (let [bounds (.getVisualBounds screen)]
                 {:min-x (if min-x (min (.getMinX bounds) min-x) (.getMinX bounds))
                  :min-y (if min-y (min (.getMinY bounds) min-y) (.getMinY bounds))
                  :max-x (if max-x (max (.getMaxX bounds) max-x) (.getMaxX bounds))
                  :max-y (if max-y (max (.getMaxY bounds) max-y) (.getMaxY bounds))}))
             {}
             screens)]
    (assoc xy
           :width (if (and (:min-x xy) (:max-x xy))
                    (- (:max-x xy) (:min-x xy))
                    min-width)
           :height (if (and (:min-y xy) (:max-y xy))
                     (- (:max-y xy) (:min-y xy))
                     min-height))))

(defn screen-bounds-fallback [screen-bounds]
  (or screen-bounds
      (do
        (log/warn (ex-info "stacktrace" {}) "No screen-bounds, performance issue here")
        (get-screen-bounds))))

(defn fitx
  ([screen-bounds]
   (fitx screen-bounds nil))
  ([screen-bounds setting]
   (let [screen-bounds (screen-bounds-fallback screen-bounds)]
     (max
       (min
         (or setting
             (when (and (:min-x screen-bounds) (:max-x screen-bounds))
               (quot (+ (:min-x screen-bounds)
                        (- (:max-x screen-bounds) min-width))
                     2))
             Integer/MIN_VALUE)
         (or (:max-x screen-bounds) 0))
       (or (:min-x screen-bounds) 0)))))

(defn fity
  ([screen-bounds]
   (fity screen-bounds nil))
  ([screen-bounds setting]
   (let [screen-bounds (screen-bounds-fallback screen-bounds)]
     (max
       (min
         (or setting
             (when (and (:min-y screen-bounds) (:max-y screen-bounds))
               (quot (+ (:min-y screen-bounds)
                        (- (:max-y screen-bounds) min-height))
                     2))
             Integer/MIN_VALUE)
         (or (:max-y screen-bounds) 0))
       (or (:min-y screen-bounds) 0)))))

(defn fitwidth
  ([screen-bounds default]
   (fitwidth screen-bounds nil default))
  ([screen-bounds setting default]
   (let [screen-bounds (screen-bounds-fallback screen-bounds)]
     (max
       (min
         (or setting default min-width)
         (or (:width screen-bounds) default))
       min-width))))

(defn fitheight
  ([screen-bounds default]
   (fitheight screen-bounds nil default))
  ([screen-bounds setting default]
   (let [screen-bounds (screen-bounds-fallback screen-bounds)]
     (max
       (min
         (or setting default min-height)
         (or (:height screen-bounds) default))
       min-height))))
