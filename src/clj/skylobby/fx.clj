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

;; Injected by spring-lobby at startup: a (fn [server-key width]) that pushes the
;; measured available width of the auto-filling battle minimap into reactive app
;; state. nil (no-op) until set, so this namespace stays decoupled from *state.
(def minimap-width-impl nil)

(defn add-minimap-width-listener
  "Report node's live width (rounded to 8px steps to limit re-render churn) for
   server-key's auto-filling battle minimap, via minimap-width-impl."
  [server-key ^javafx.scene.layout.Region node]
  (let [width-property (.widthProperty node)
        report (fn [w]
                 (when (and minimap-width-impl (pos? w))
                   (minimap-width-impl server-key
                     (int (* 8 (Math/round (/ (double w) 8.0)))))))]
    (.addListener width-property
      (reify javafx.beans.value.ChangeListener
        (changed [_this _observable _old-value new-value]
          (report new-value))))
    (report (.getValue width-property))))

;; A real, well-hinted monospace face reads far better than the bare generic
;; "monospace" keyword, which renders thin/low-contrast on macOS and Linux.
;; This is a font-family preference list: JavaFX uses the first installed
;; family (Menlo on macOS, Consolas on Windows, DejaVu/Liberation on Linux) and
;; falls back to the generic keyword if none are present.
(def monospace-font-family
  "Menlo, Consolas, 'DejaVu Sans Mono', 'Liberation Mono', monospace")


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
   :thumb     "rgb(120,120,120)"
   :thumb-hover "rgb(155,155,155)"
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
;; Full theme-author reference: docs/theming.md (enumerates every selector,
;; ramp key, and the custom-css.edn workflow). The list below is a quick
;; curated subset of the most common handles.
;; Override these in custom-css.edn (loaded via the :css sub) to reskin
;; without fighting the global -fx-base cascade:
;;   .root                          app surfaces (-fx-background/-fx-base/...)
;;   .tab / .tab:selected           tab states (separate from buttons)
;;   .tab-header-background         the strip behind tab headers
;;   .skylobby-card                 raised grouped panel (welcome columns, etc.)
;;   .skylobby-screen-*             per-screen root hooks (welcome/settings/battle/server/spring-picker/replays/scenarios/downloads/rapid/import/tasks)
;;   .skylobby-secondary            flat/quiet button variant (de-emphasised nav)
;;   .skylobby-primary              prominent call-to-action button (Start Game)
;;   .combo-box / .check-box / .text-field   inputs, fixed normal font size
;;   .skylobby-form-label           fixed-width left-aligned form label column
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
   ;; Tooltips render in their own popup window, so they don't inherit the
   ;; app surfaces and otherwise keep modena's hardcoded dark background.
   ;; Text fill is laddered off the tooltip's own surface so it stays
   ;; readable on both light and dark themes. -fx-text-base-color carries the
   ;; same ladder so child labels inside a tooltip :graphic (e.g. the battle
   ;; list tooltip) inherit it instead of modena's default light text.
   ".tooltip"
   {:-fx-background-color surface-2
    :-fx-text-fill (str "ladder(" surface-2 ", " text-on-dark " 49%, " text-on-light " 50%)")
    :-fx-text-base-color (str "ladder(" surface-2 ", " text-on-dark " 49%, " text-on-light " 50%)")
    :-fx-background-radius "6"
    :-fx-border-color border
    :-fx-border-radius "6"
    :-fx-border-width "1"}
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
   ;; Scroll bars - thicker, higher-contrast thumbs so they read clearly
   ".scroll-bar .thumb"
   {:-fx-background-radius "3, 2"
    :-fx-background-insets "1, 2"}
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
   ;; Sliders - give the track and handle a clear, visible treatment
   ".slider .track"
   {:-fx-background-color surface-3
    :-fx-background-radius "3"}
   ".slider .thumb"
   {:-fx-background-color thumb-hover
    :-fx-background-radius "8"
    :-fx-padding "7"}
   ".slider:hover .thumb,.slider .thumb:pressed"
   {:-fx-background-color focus}
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
          ; chat/console sit on the deepest surface (grey) by default, which is
          ; low-contrast under dark text in light theme - lift to near-white
          ".styled-text-area"
          {:-fx-background-color "rgb(250,250,250)"}
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
;; Catppuccin Mocha - a blue-tinted dark theme (https://catppuccin.com).
(def catppuccin-mocha-ramp
  {:surface-0 "rgb(24,24,37)"      ; mantle
   :surface-1 "rgb(30,30,46)"      ; base
   :surface-2 "rgb(49,50,68)"      ; surface0
   :surface-3 "rgb(69,71,90)"      ; surface1
   :border    "rgb(88,91,112)"     ; surface2
   :focus     "rgb(137,180,250)"   ; blue
   :selection "rgb(69,71,90)"      ; surface1
   :selection-unfocused "rgb(49,50,68)" ; surface0
   :text-on-dark  "rgb(205,214,244)" ; text
   :text-on-light "rgb(30,30,46)"     ; base
   :text-2    "rgb(166,173,200)"   ; subtext0
   :row-odd   "rgb(30,30,46)"      ; base
   :row-even  "rgb(24,24,37)"      ; mantle
   :thumb     "rgb(108,112,134)"   ; overlay0
   :thumb-hover "rgb(127,132,156)" ; overlay1
   :tab-highlight "rgb(249,226,175)" ; yellow
   :tab-selected-accent "rgb(180,190,254)" ; lavender
   :icon "rgb(205,214,244)"})

(def catppuccin-mocha-style-data
  (merge (theme-data catppuccin-mocha-ramp)
         {".skylobby"
          {"-chat" {"-user-list" {:-fx-text-fill "rgb(205,214,244)"}}
           "-tab" {"-focus" {:-fx-background (:tab-highlight catppuccin-mocha-ramp)
                             :-fx-base (:tab-highlight catppuccin-mocha-ramp)}}}}))

;; Catppuccin Latte - the light Catppuccin variant (https://catppuccin.com).
(def catppuccin-latte-ramp
  {:surface-0 "rgb(220,224,232)"   ; crust
   :surface-1 "rgb(230,233,239)"   ; mantle
   :surface-2 "rgb(239,241,245)"   ; base
   :surface-3 "rgb(255,255,255)"   ; raised / white
   :border    "rgb(172,176,190)"   ; surface2
   :focus     "rgb(30,102,245)"    ; blue
   :selection "rgb(202,216,252)"   ; blue-tinted highlight
   :selection-unfocused "rgb(204,208,218)" ; surface0
   :text-on-dark  "rgb(239,241,245)" ; base (light fallback)
   :text-on-light "rgb(76,79,105)"   ; text (the dark text actually shown)
   :text-2    "rgb(108,111,133)"   ; subtext0
   :row-odd   "rgb(239,241,245)"   ; base
   :row-even  "rgb(230,233,239)"   ; mantle
   :thumb     "rgb(156,160,176)"   ; overlay0
   :thumb-hover "rgb(140,143,161)" ; overlay1
   :tab-highlight "rgb(223,142,29)" ; yellow
   :tab-selected-accent "rgb(114,135,253)" ; lavender
   :icon "rgb(76,79,105)"})        ; text (dark icons for a light theme)

;; Light theme: like light-style-data, chat/console/inputs need explicit
;; contrast overrides so the dark-theme defaults (white text, washed-out
;; thumbs) don't bleed through on a light background.
(def catppuccin-latte-style-data
  (merge (theme-data catppuccin-latte-ramp)
         {".scroll-bar:vertical .thumb"
          {:-fx-background-color "rgb(156,160,176),linear-gradient(to right,derive(-fx-base,-10%),derive(-fx-base,-50%))"}
          ".scroll-bar:horizontal .thumb"
          {:-fx-background-color "rgb(156,160,176),linear-gradient(to bottom,derive(-fx-base,-10%),derive(-fx-base,-50%))"}
          ".scroll-bar:vertical .thumb:hover"
          {:-fx-background-color "rgb(124,127,147),linear-gradient(to right,derive(-fx-base,+30%),derive(-fx-base,-10%))"}
          ".scroll-bar:horizontal .thumb:hover"
          {:-fx-background-color "rgb(124,127,147),linear-gradient(to bottom,derive(-fx-base,+30%),derive(-fx-base,-10%))"}
          ".skilluncertainty0" {:-fx-text-fill "rgb(0, 0, 0)"}
          ".skilluncertainty1" {:-fx-text-fill "rgb(50, 40, 30)"}
          ".skilluncertainty2" {:-fx-text-fill "rgb(80, 60, 40)"}
          ".skilluncertainty3" {:-fx-text-fill "rgb(120, 80, 40)"}
          ".combo-box-popup .list-cell:selected"
          {:-fx-background-color "rgb(204,208,218)" :-fx-text-fill "rgb(76,79,105)"}
          ".combo-box-popup .list-cell:hover"
          {:-fx-background-color "rgb(220,224,232)" :-fx-text-fill "rgb(76,79,105)"}
          ".menu-item:hover"
          {:-fx-background-color "rgb(220,224,232)" :-fx-text-fill "rgb(76,79,105)"}
          ".styled-text-area"
          {:-fx-background-color "rgb(239,241,245)"}
          ".skylobby"
          {"-chat"
           {"-message"     {:-fx-fill "rgb(76,79,105)"}
            "-username-ex" {:-fx-fill "rgb(23,146,153)"}
            "-message-ex"  {:-fx-fill "rgb(23,146,153)"}
            "-user-list"   {:-fx-text-fill "rgb(76,79,105)"}}
           "-console"
           {"-message" {:-fx-fill "rgb(76,79,105)"}}
           "-tab" {"-focus" {:-fx-background (:tab-highlight catppuccin-latte-ramp)
                             :-fx-base (:tab-highlight catppuccin-latte-ramp)}}}}))

;; Dark Brown - a warm dark theme inspired by the Beyond All Reason in-game
;; HUD: charcoal-brown panels with orange/red glowing borders and amber-gold
;; accents.
(def dark-brown-ramp
  {:surface-0 "rgb(18,15,12)"       ; deepest warm charcoal
   :surface-1 "rgb(28,23,18)"       ; base panel
   :surface-2 "rgb(42,34,26)"       ; card / hover
   :surface-3 "rgb(58,46,34)"       ; raised / selected
   :border    "rgb(120,70,35)"      ; warm orange-tinted edge
   :focus     "rgb(230,120,40)"     ; orange focus ring
   :selection "rgb(80,55,30)"
   :selection-unfocused "rgb(55,42,26)"
   :text-on-dark  "rgb(235,225,210)" ; warm off-white
   :text-on-light "rgb(25,20,15)"
   :text-2    "rgb(180,150,110)"    ; muted amber
   :row-odd   "rgb(30,25,20)"
   :row-even  "rgb(22,18,14)"
   :thumb     "rgb(140,95,55)"
   :thumb-hover "rgb(180,120,65)"
   :tab-highlight "rgb(240,170,50)" ; amber-gold
   :tab-selected-accent "rgb(225,110,40)" ; orange
   :icon "rgb(235,180,110)"})       ; warm amber icons

(def dark-brown-style-data
  (merge (theme-data dark-brown-ramp)
         {".skylobby"
          {"-chat" {"-user-list" {:-fx-text-fill "rgb(235,225,210)"}}
           "-tab" {"-focus" {:-fx-background (:tab-highlight dark-brown-ramp)
                             :-fx-base (:tab-highlight dark-brown-ramp)}}}}))

;; Light Brown - the light counterpart to Dark Brown: warm sand/tan surfaces
;; with dark-brown text and the same orange/amber accents.
(def light-brown-ramp
  {:surface-0 "rgb(214,198,170)"    ; deep sand (crust)
   :surface-1 "rgb(226,212,186)"    ; base panel
   :surface-2 "rgb(236,224,202)"    ; card / hover
   :surface-3 "rgb(248,240,224)"    ; raised / near-white sand
   :border    "rgb(170,120,70)"     ; warm brown edge
   :focus     "rgb(200,100,30)"     ; orange focus ring
   :selection "rgb(232,200,150)"
   :selection-unfocused "rgb(222,206,180)"
   :text-on-dark  "rgb(245,235,215)" ; light fallback
   :text-on-light "rgb(60,40,20)"   ; dark brown (text actually shown)
   :text-2    "rgb(120,90,55)"      ; muted brown
   :row-odd   "rgb(236,224,202)"
   :row-even  "rgb(226,212,186)"
   :thumb     "rgb(170,140,100)"
   :thumb-hover "rgb(150,120,80)"
   :tab-highlight "rgb(210,130,30)" ; amber-orange
   :tab-selected-accent "rgb(200,100,30)" ; orange
   :icon "rgb(80,55,30)"})          ; dark brown icons for a light theme

;; Light theme: chat/console/inputs need explicit contrast overrides so the
;; dark-theme defaults (white text, washed-out thumbs) don't bleed through.
(def light-brown-style-data
  (merge (theme-data light-brown-ramp)
         {".scroll-bar:vertical .thumb"
          {:-fx-background-color "rgb(170,140,100),linear-gradient(to right,derive(-fx-base,-10%),derive(-fx-base,-50%))"}
          ".scroll-bar:horizontal .thumb"
          {:-fx-background-color "rgb(170,140,100),linear-gradient(to bottom,derive(-fx-base,-10%),derive(-fx-base,-50%))"}
          ".scroll-bar:vertical .thumb:hover"
          {:-fx-background-color "rgb(150,120,80),linear-gradient(to right,derive(-fx-base,+30%),derive(-fx-base,-10%))"}
          ".scroll-bar:horizontal .thumb:hover"
          {:-fx-background-color "rgb(150,120,80),linear-gradient(to bottom,derive(-fx-base,+30%),derive(-fx-base,-10%))"}
          ".skilluncertainty0" {:-fx-text-fill "rgb(0, 0, 0)"}
          ".skilluncertainty1" {:-fx-text-fill "rgb(50, 40, 30)"}
          ".skilluncertainty2" {:-fx-text-fill "rgb(80, 60, 40)"}
          ".skilluncertainty3" {:-fx-text-fill "rgb(120, 80, 40)"}
          ".combo-box-popup .list-cell:selected"
          {:-fx-background-color "rgb(232,200,150)" :-fx-text-fill "rgb(60,40,20)"}
          ".combo-box-popup .list-cell:hover"
          {:-fx-background-color "rgb(226,212,186)" :-fx-text-fill "rgb(60,40,20)"}
          ".menu-item:hover"
          {:-fx-background-color "rgb(226,212,186)" :-fx-text-fill "rgb(60,40,20)"}
          ".styled-text-area"
          {:-fx-background-color "rgb(248,240,224)"}
          ".skylobby"
          {"-chat"
           {"-message"     {:-fx-fill "rgb(60,40,20)"}
            "-username-ex" {:-fx-fill "rgb(150,75,15)"}
            "-message-ex"  {:-fx-fill "rgb(150,75,15)"}
            "-user-list"   {:-fx-text-fill "rgb(60,40,20)"}}
           "-console"
           {"-message" {:-fx-fill "rgb(60,40,20)"}}
           "-tab" {"-focus" {:-fx-background (:tab-highlight light-brown-ramp)
                             :-fx-base (:tab-highlight light-brown-ramp)}}}}))

;; HardHacker - base16 scheme (dark, pink-accented).
;; https://tinted-theming.github.io/tinted-gallery/#base16-hardhacker
(def hardhacker-ramp
  {:surface-0 "#211e2a"  ; base00
   :surface-1 "#2c2737"  ; base01
   :surface-2 "#3f3951"  ; base02
   :surface-3 "#6e6780"  ; base03
   :border    "#8a829e"  ; base04
   :focus     "#95a6f4"  ; base0D (blue)
   :selection "#3f3951"  ; base02
   :selection-unfocused "#2c2737" ; base01
   :text-on-dark  "#e4dee9" ; base05
   :text-on-light "#211e2a" ; base00
   :text-2    "#8a829e"  ; base04
   :row-odd   "#2c2737"  ; base01
   :row-even  "#211e2a"  ; base00
   :thumb     "#6e6780"  ; base03
   :thumb-hover "#8a829e" ; base04
   :tab-highlight "#ebde76" ; base0A (yellow)
   :tab-selected-accent "#ff79c6" ; base0E (pink)
   :icon "#e4dee9"})     ; base05

(def hardhacker-style-data
  (merge (theme-data hardhacker-ramp)
         {".skylobby"
          {"-chat" {"-user-list" {:-fx-text-fill (:text-on-dark hardhacker-ramp)}}
           "-tab" {"-focus" {:-fx-background (:tab-highlight hardhacker-ramp)
                             :-fx-base (:tab-highlight hardhacker-ramp)}}}}))

;; Sakura - base16 scheme by Misterio77 (a soft pink LIGHT theme). Sakura
;; only supplies three light background steps, so the raised surface, borders,
;; selection and scroll thumbs are derived dusty-rose tints.
;; https://tinted-theming.github.io/tinted-gallery/#base16-sakura
(def sakura-ramp
  {:surface-0 "#e0ccd1"  ; base02 (deepest bg)
   :surface-1 "#f8e2e7"  ; base01
   :surface-2 "#feedf3"  ; base00 (main bg)
   :surface-3 "#fff6fa"  ; derived near-white pink (raised)
   :border    "#dcc0c7"  ; derived dusty-rose divider
   :focus     "#006e93"  ; base0D (blue)
   :selection "#f1dae0"  ; derived pink selection tint
   :selection-unfocused "#e0ccd1" ; base02
   :text-on-dark  "#feedf3" ; base00 (light fallback)
   :text-on-light "#564448" ; base05 (dark text actually shown)
   :text-2    "#755f64"  ; base03 (muted)
   :row-odd   "#feedf3"  ; base00
   :row-even  "#f8e2e7"  ; base01
   :thumb     "#cdaeb5"  ; derived
   :thumb-hover "#b8949c" ; derived
   :tab-highlight "#c29461" ; base0A (gold)
   :tab-selected-accent "#5e2180" ; base0E (purple)
   :icon "#564448"})     ; base05 (dark icons for a light theme)

;; Light theme: chat/console/inputs need explicit contrast overrides so the
;; dark-theme defaults (white text, washed-out thumbs) don't bleed through.
(def sakura-style-data
  (merge (theme-data sakura-ramp)
         {".scroll-bar:vertical .thumb"
          {:-fx-background-color "#cdaeb5,linear-gradient(to right,derive(-fx-base,-10%),derive(-fx-base,-50%))"}
          ".scroll-bar:horizontal .thumb"
          {:-fx-background-color "#cdaeb5,linear-gradient(to bottom,derive(-fx-base,-10%),derive(-fx-base,-50%))"}
          ".scroll-bar:vertical .thumb:hover"
          {:-fx-background-color "#b8949c,linear-gradient(to right,derive(-fx-base,+30%),derive(-fx-base,-10%))"}
          ".scroll-bar:horizontal .thumb:hover"
          {:-fx-background-color "#b8949c,linear-gradient(to bottom,derive(-fx-base,+30%),derive(-fx-base,-10%))"}
          ".skilluncertainty0" {:-fx-text-fill "rgb(0, 0, 0)"}
          ".skilluncertainty1" {:-fx-text-fill "rgb(50, 40, 30)"}
          ".skilluncertainty2" {:-fx-text-fill "rgb(80, 60, 40)"}
          ".skilluncertainty3" {:-fx-text-fill "rgb(120, 80, 40)"}
          ".combo-box-popup .list-cell:selected"
          {:-fx-background-color "#f1dae0" :-fx-text-fill "#564448"}
          ".combo-box-popup .list-cell:hover"
          {:-fx-background-color "#f8e2e7" :-fx-text-fill "#564448"}
          ".menu-item:hover"
          {:-fx-background-color "#f8e2e7" :-fx-text-fill "#564448"}
          ".styled-text-area"
          {:-fx-background-color "#feedf3"}
          ".skylobby"
          {"-chat"
           {"-message"     {:-fx-fill "#564448"}
            "-username-ex" {:-fx-fill "#1d8991"}
            "-message-ex"  {:-fx-fill "#1d8991"}
            "-user-list"   {:-fx-text-fill "#564448"}}
           "-console"
           {"-message" {:-fx-fill "#564448"}}
           "-tab" {"-focus" {:-fx-background (:tab-highlight sakura-ramp)
                             :-fx-base (:tab-highlight sakura-ramp)}}}}))

(def default-style-data black-style-data)

(def style-presets
  {
   "black" black-style-data
   "grey" grey-style-data
   "light" light-style-data
   "catppuccin-mocha" catppuccin-mocha-style-data
   "catppuccin-latte" catppuccin-latte-style-data
   "dark-brown" dark-brown-style-data
   "light-brown" light-brown-style-data
   "hardhacker" hardhacker-style-data
   "sakura" sakura-style-data})

;; Per-theme tint for the welcome banner (skylobby_banner.png). The banner's
;; dominant hue is ~216 (blue); these JavaFX :color-adjust values rotate that
;; toward each theme's accent (hue is -1..1 = -180..180 degrees) and lift
;; brightness for light themes. A preset absent from this map (incl. the
;; neutral dark "black"/"grey" and "custom") leaves the original banner, whose
;; blue starfield already suits them. The metallic logo text is near-grey so it
;; stays silver while the starfield takes the hue. Match is approximate by design.
(def banner-color-adjust
  {"light"               {:brightness 0.3 :saturation -0.1}
   "catppuccin-mocha"    {:saturation 0.08}
   "catppuccin-latte"    {:brightness 0.3 :saturation -0.05}
   "dark-brown"          {:hue 0.93 :saturation 0.1}
   "light-brown"         {:hue 0.93 :brightness 0.3 :saturation 0.1}
   "hardhacker"          {:hue 0.61 :saturation 0.15}
   "sakura"              {:hue 0.34 :brightness 0.3 :saturation 0.05}})

(defn banner-effect
  "JavaFX :color-adjust effect prop map tinting the welcome banner to match the
  given style preset, or nil to leave the banner untinted."
  [css-preset]
  (when-let [adj (get banner-color-adjust css-preset)]
    (assoc adj :fx/type :color-adjust)))


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
   ; form rows: a fixed-width left-aligned label column so inputs line up
   ; regardless of label text length
   ".skylobby-form-label"
   {:-fx-min-width 150
    :-fx-alignment "center-left"}
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
