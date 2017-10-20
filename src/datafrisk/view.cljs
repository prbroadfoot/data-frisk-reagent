(ns datafrisk.view
  (:require [cljs.pprint :refer [pprint]]
            [reagent.core :as r]
            [datafrisk.util :as u]
            [cljs.reader :refer [read-string]]))

(declare DataFrisk)

(def styles
  {:shell {:backgroundColor "#FAFAFA"
           :fontFamily "Consolas,Monaco,Courier New,monospace"
           :fontSize "12px"
           :z-index 9999}
   :strings {:color "#4Ebb4E"}
   :keywords {:color "purple"}
   :numbers {:color "blue"}
   :nil {:color "red"}
   :shell-visible-button {:backgroundColor "#4EE24E"}})

(defn ErrorIcon []
  [:svg {:viewBox "0 0 30 42" :width "100%" :height "100%"}
   [:path {:fill "darkorange"
           :stroke "red"
           :stroke-width "2"
           :d "M15 3
           Q16.5 6.8 25 18
           A12.8 12.8 0 1 1 5 18
           Q13.5 6.8 15 3z"}]
   [:circle {:cx 15 :cy 32 :r 7 :fill "yellow"}]])

(defn ErrorText [text]
  [:div {:style {:fontSize "0.7em"
                 :display "flex"
                 :align-items "center"
                 :color "red"}} text])

(defn ExpandButton [{:keys [expanded? path emit-fn]}]
  [:button {:style {:border 0
                    :padding "5px 4px 5px 2px"
                    :textAlign "center"
                    :backgroundColor "transparent"
                    :width "20px"
                    :height "20px"
                    :cursor "pointer"}
            :onClick #(emit-fn (if expanded? :contract :expand) path)}
   [:svg {:viewBox "0 0 100 100"
          :width "100%" :height "100%"
          :style {:transition "all 0.2s ease"
                  :transform (when expanded? "rotate(90deg)")}}
    [:polygon {:points "0,0 0,100 100,50" :stroke "black"}]]])

(def button-style {:padding "1px 3px"
                   :cursor "pointer"
                   :background-color "white"})

(defn ExpandAllButton [emit-fn data]
  [:button {:onClick #(emit-fn :expand-all data)
            :style (merge button-style
                          {:borderTopLeftRadius "2px"
                           :borderBottomLeftRadius "2px"
                           :border "1px solid darkgray"})}
   "Expand"])

(defn CollapseAllButton [emit-fn data]
  [:button {:onClick #(emit-fn :collapse-all)
            :style
            (merge button-style
                   {:borderTop "1px solid darkgray"
                    :borderBottom "1px solid darkgray"
                    :borderRight "1px solid darkgray"
                    :borderLeft "0"})}
   "Collapse"])

(defn CopyButton [emit-fn data]
  [:button {:onClick #(emit-fn :copy data)
            :style (merge button-style
                          {:borderTopRightRadius "2px"
                           :borderBottomRightRadius "2px"
                           :borderTop "1px solid darkgray"
                           :borderBottom "1px solid darkgray"
                           :borderRight "1px solid darkgray"
                           :borderLeft "0"})}
   "Copy"])

(defn NilText []
  [:span {:style (:nil styles)} (pr-str nil)])

(defn StringText [data]
  [:span {:style (:strings styles)} (pr-str data)])

(defn KeywordText [data]
  [:span {:style (:keywords styles)} (str data)])

(defn NumberText [data]
  [:span {:style (:numbers styles)} data])

(defn KeySet [keyset]
  [:span
   (->> keyset
        (sort-by str)
        (map-indexed
          (fn [i data] ^{:key i} [:span
                                  (cond (nil? data) [NilText]
                                        (string? data) [StringText data]
                                        (keyword? data) [KeywordText data]
                                        (number? data) [NumberText data]
                                        :else (str data))]))
        (interpose " "))])

(defn Node [{:keys [data path emit-fn swappable metadata-paths]}]
  [:div {:style {:display "flex"}} (cond
          (nil? data)
          [NilText]

          (string? data)
          (if swappable
            [:input {:type "text"
                     :default-value (str data)
                     :on-change
                     (fn string-changed [e]
                       (emit-fn :changed path (.. e -target -value)))}]
            [StringText data])

          (keyword? data)
          (if swappable
            [:input {:type "text"
                     :default-value (name data)
                     :on-change
                     (fn keyword-changed [e]
                       (emit-fn :changed path (keyword (.. e -target -value))))}]
            [KeywordText data])

          (object? data)
          (str data " " (.stringify js/JSON data))

          (number? data)
          (if swappable
            [:input {:type "number"
                     :default-value data
                     :on-change
                     (fn number-changed [e]
                       (emit-fn :changed path (js/Number (.. e -target -value))))}]
            [NumberText data])
          :else
          (str data))
   (when-let [errors (:error (get metadata-paths path))]
     [ErrorText (str "\u00A0 " errors)])])

(defn expandable? [v]
  (or (map? v) (seq? v) (coll? v)))

(defn CollectionSummary [{:keys [data]}]
  (cond (map? data) [:div {:style {:flex "0 1 auto"}}
                     [:span "{"]
                     [KeySet (keys data)]
                     [:span "}"]]
        (set? data) [:div {:style {:flex "0 1 auto"}} [:span "#{"]
                     (str (count data) " items")
                     [:span "}"]]
        (or (seq? data)
            (vector? data)) [:div {:style {:flex 1}}
                             [:span (if (vector? data) "[" "(")]
                             (str (count data) " items")
                             [:span (if (vector? data) "]" ")")]]))

(defn change-panel-path [{:keys [event emit-fn path]}]
  (.stopPropagation event)
  (emit-fn :change-panel-path path)
  (when (.-ctrlKey event)
    (emit-fn :copy path)))

(defn KeyValNode [{[k v] :data :keys [path metadata-paths emit-fn swappable]}]
  (let [path-to-here (conj path k)
        expandable-node? (and (expandable? v)
                              (not (empty? v)))
        metadata (get metadata-paths path-to-here)
        expanded? (:expanded? metadata)
        highlight? (:highlight? metadata)]
    [:div {:style {:display "flex"
                   :flex-flow "column"}
           :on-click #(change-panel-path
                       {:event %, :emit-fn emit-fn, :path (conj path k)})}
     [:div {:style {:display "flex"}}
      [:div {:style {:flex "0 0 20px"}}
       (when expandable-node?
         [ExpandButton {:expanded? expanded?
                        :path path-to-here
                        :emit-fn emit-fn}])]
      [:div {:style {:flex "0 1 auto"}}
       [:div {:style {:display "flex"
                      :flex-flow "row"}}
        [:div {:style {:flex "0 1 auto"}}
         [Node {:data k}]]
        [:div {:style {:flex "0 1 auto" :paddingLeft "4px"}}
         (if (expandable? v)
           [CollectionSummary {:data v}]
           [Node {:data v
                  :swappable swappable
                  :path path-to-here
                  :metadata-paths metadata-paths
                  :emit-fn emit-fn}])]]]]
     (when expanded?
       [:div {:style {:flex "1"
                      :background-color (when highlight? "#fff9db")}}
        [DataFrisk {:hide-header? true
                    :data v
                    :swappable swappable
                    :path path-to-here
                    :metadata-paths metadata-paths
                    :emit-fn emit-fn}]])]))

(defn ListVecNode [{:keys [data path metadata-paths emit-fn swappable hide-header?]}]
  (let [metadata (get metadata-paths path)
        expanded? (:expanded? metadata)]
    [:div {:style {:display "flex"
                   :flex-flow "column"}
           :on-click #(change-panel-path
                       {:event %, :emit-fn emit-fn, :path path})}
     (when-not hide-header?
       [:div {:style {:display "flex"}}
        (when (:error metadata)
          [:div {:style {:margin-left "-1em"
                         :width "1em"
                         :height "1.2em"}}
           [ErrorIcon]])
        [ExpandButton {:expanded? expanded?
                       :path path
                       :emit-fn emit-fn}]
        [:div {:style {:flex "0 1 auto"}}
         [:span (if (vector? data) "[" "(")]
         (str (count data) " items")
         [:span (if (vector? data) "]" ")")]]])
     (when expanded?
       [:div {:style {:flex "0 1 auto" :padding "0 0 0 20px"}}
        (when (:error metadata)
          [:div {:style {:paddingBottom "4px"}}
           [ErrorText (:error metadata)]])
        (map-indexed (fn [i x] ^{:key i} [DataFrisk {:data x
                                                     :swappable swappable
                                                     :path (conj path i)
                                                     :metadata-paths metadata-paths
                                                     :emit-fn emit-fn}]) data)])]))

(defn SetNode [{:keys [data path metadata-paths emit-fn swappable hide-header?]}]
  (let [metadata (get metadata-paths path)
        expanded? (:expanded? metadata)]
    [:div {:style {:display "flex"
                   :flex-flow "column"}
           :on-click #(change-panel-path
                       {:event %, :emit-fn emit-fn, :path path})}
     (when-not hide-header?
       [:div {:style {:display "flex"}}
        (when (:error metadata)
          [:div {:style {:margin-left "-1em"
                         :width "1em"
                         :height "1.2em"}}
           [ErrorIcon]])
        [ExpandButton {:expanded? expanded?
                       :path path
                       :emit-fn emit-fn}]
        [:div {:style {:flex "0 1 auto"}}
         [:span "#{"]
         (str (count data) " items")
         [:span "}"]]])
     (when expanded?
       [:div {:style {:flex "0 1 auto" :paddingLeft "20px"}}
        (when (:error metadata)
          [:div {:style {:paddingBottom "4px"}}
           [ErrorText (:error metadata)]])
        (map-indexed (fn [i x] ^{:key i} [DataFrisk {:data x
                                                     :swappable swappable
                                                     :path (conj path i)
                                                     :metadata-paths metadata-paths
                                                     :emit-fn emit-fn}]) data)])]))

(defn MapNode [{:keys [data path metadata-paths emit-fn hide-header?] :as all}]
  (let [metadata (get metadata-paths path)
        expanded? (:expanded? metadata)
        highlight? (:highlight? metadata)]
    [:div {:style {:display "flex"
                   :flex-flow "column"}
           :on-click #(change-panel-path
                       {:event %, :emit-fn emit-fn, :path path})}
     (when-not hide-header?
       [:div {:style {:display "flex"}}
        (when (:error metadata)
          [:div {:style {:margin-left "-1em"
                         :width "1em"
                         :height "1.2em"}}
           [ErrorIcon]])
        [ExpandButton {:expanded? expanded?
                       :path path
                       :emit-fn emit-fn}]
        [:div {:style {:flex "0 1 auto"}}
         [:span (str "{")]
         [KeySet (keys data)]
         [:span "}"]]])
     (when expanded?
       [:div {:style {:flex "0 1 auto" :paddingLeft "20px"
                      :background-color (when highlight? "#fff9db")}}
        (when (:error metadata)
          [:div {:style {:paddingBottom "4px"}}
           [ErrorText (:error metadata)]])
        (->> data
             (sort-by (fn [[k _]] (str k)))
             (map-indexed (fn [i x] ^{:key i} [KeyValNode (assoc all :data x)])))])]))

(defn DataFrisk [{:keys [data emit-fn path] :as all}]
  (cond (map? data) [MapNode all]
        (set? data) [SetNode all]
        (or (seq? data) (vector? data)) [ListVecNode all]
        (satisfies? IDeref data) [DataFrisk (assoc all :data @data)]
        :else [:div {:style {:paddingLeft "20px"}
                     :on-click #(change-panel-path
                                 {:event %, :emit-fn emit-fn, :path path})}
               [Node all]]))

(defn conj-to-set [coll x]
  (conj (or coll #{}) x))

(defn expand-all-paths [root-value current-expanded-paths]
  (loop [remaining [{:path [] :node root-value}]
         expanded-paths current-expanded-paths]
    (if (seq remaining)
      (let [[current & rest] remaining
            current-node (if (satisfies? IDeref (:node current)) @(:node current) (:node current))]
        (cond (map? current-node)
              (recur
                (concat rest (map (fn [[k v]] {:path (conj (:path current) k)
                                               :node v})
                                  current-node))
                (assoc-in expanded-paths [(:path current) :expanded?] true))

              (or (seq? current-node)
                  (vector? current-node)
                  (set? current-node))
              (recur
                (concat rest (map-indexed (fn [i node] {:path (conj (:path current) i)
                                                        :node node})
                               current-node))
                (assoc-in expanded-paths [(:path current) :expanded?] true))

              :else
              (recur
                rest
                (if (coll? current-node)
                  (assoc-in expanded-paths [(:path current) :expanded?] true)
                  expanded-paths))))
      expanded-paths)))

(defn copy-to-clipboard [data]
  (let [pretty (with-out-str (pprint data))
        textArea (.createElement js/document "textarea")]
    (doto textArea
      ;; Put in top left corner of screen
      (aset "style" "position" "fixed")
      (aset "style" "top" 0)
      (aset "style" "left" 0)
      ;; Make it small
      (aset "style" "width" "2em")
      (aset "style" "height" "2em")
      (aset "style" "padding" 0)
      (aset "style" "border" "none")
      (aset "style" "outline" "none")
      (aset "style" "boxShadow" "none")
      ;; Avoid flash of white box
      (aset "style" "background" "transparent")
      (aset "value" pretty))

    (.appendChild (.-body js/document) textArea)
    (.select textArea)

    (.execCommand js/document "copy")
    (.removeChild (.-body js/document) textArea)))

(defn collapse-all [metadata-paths]
  (u/map-vals #(assoc % :expanded? false) metadata-paths))

(defn expand-to-path [path data current-expanded-paths]
  (let [path-found? (-> (get-in data (drop-last path))
                        (contains? (last path)))]
    (cond
      (empty? path)
      {}

      path-found?
      (let [paths (reductions conj [] path)
            metadata-paths (reduce #(assoc %1 %2 {:expanded? true})
                                   {}
                                   paths)]
        (-> metadata-paths
            (assoc-in [path :highlight?] true)))

      :else
      current-expanded-paths)))

(defn emit-fn-factory [state-atom id swappable]
  (fn [event & args]
    (case event
      :expand (swap! state-atom assoc-in [:data-frisk id :metadata-paths (first args) :expanded?] true)
      :expand-all (swap! state-atom update-in [:data-frisk id :metadata-paths] (partial expand-all-paths (first args)))
      :expand-to-path (swap! state-atom update-in [:data-frisk id :metadata-paths] (partial expand-to-path (first args) (second args)))
      :contract (swap! state-atom assoc-in [:data-frisk id :metadata-paths (first args) :expanded?] false)
      :collapse-all (swap! state-atom update-in [:data-frisk id :metadata-paths] collapse-all)
      :copy (copy-to-clipboard (first args))
      :changed (let [[path value] args]
                 (if (seq path)
                   (swap! swappable assoc-in path value)
                   (reset! swappable value)))
      :change-panel-path (swap! state-atom assoc-in [:panel-path] (first args)))))

(defn vector-str [s]
  (let [s (if (.startsWith s "[") s (str "[" s))
        s (if (.endsWith s "]") s (str s "]"))]
    s))

(defn PathInput [emit-fn data]
  (let [v (r/atom "")]
    (fn []
      [:input {:style {:width "300px" :padding "2px" :margin-right "5px"}
               :type "text"
               :placeholder "enter a path..."
               :value @v
               :on-change #(let [new-val (-> % .-target .-value)]
                             (reset! v new-val)
                             (try
                               (emit-fn :expand-to-path (read-string (vector-str new-val)) data)
                               (catch js/Error e)))}])))

(defn ModalPanel [{:keys [emit-fn data panel-path]}]
  (let [show? (r/atom false)]
    (fn [{:keys [emit-fn data panel-path]}]
      (if @show?
        [:div
         {:style {:position "fixed" :top "10px" :right "10px"
                  :padding "10px"
                  :background-color "rgb(240, 240, 240)"
                  :display "flex"
                  :flex-direction "column"
                  :align-items "flex-end"}}
         [:div
          [PathInput emit-fn data]
          [:a {:style { :font-size "16px" :cursor "pointer"}
               :on-click #(swap! show? not)} "✖"]]
         [:p {:style {:padding "0" :margin "2px" :margin-top "5px"}}
          (str @panel-path)]]

        [:button {:style (merge button-style
                                {:borderTop "1px solid darkgray"
                                 :borderBottom "1px solid darkgray"
                                 :borderRight "1px solid darkgray"
                                 :borderLeft "0"})
                  :on-click #(swap! show? not)} "Panel"]))))

(defn get-data-frisk [state-atom]
  (:data-frisk @state-atom))

(defn get-panel-path [state-atom]
  (:panel-path @state-atom))

(defn Root [data id state-atom]
  (let [data-frisk @(r/track get-data-frisk state-atom)
        swappable (when (satisfies? IAtom data)
                    data)
        emit-fn (emit-fn-factory state-atom id swappable)
        metadata-paths (get-in data-frisk [id :metadata-paths])
        panel-path (r/track get-panel-path state-atom)]
    [:div
     [:div {:style {:padding "4px 2px"}}
      [ExpandAllButton emit-fn data]
      [CollapseAllButton emit-fn]
      [CopyButton emit-fn data]
      [ModalPanel {:emit-fn emit-fn, :data data, :panel-path panel-path}]]
     [:div {:style {:flex "0 1 auto"}}
      [DataFrisk {:data data
                  :swappable swappable
                  :path []
                  :metadata-paths metadata-paths
                  :emit-fn emit-fn}]]]))

(defn VisibilityButton
  [visible? update-fn]
  [:button {:style {:border 0
                    :backgroundColor "transparent" :width "20px" :height "20px"}
            :onClick update-fn}
   [:svg {:viewBox "0 0 100 100"
          :width "100%" :height "100%"
          :style {:transition "all 0.2s ease"
                  :transform (when visible? "rotate(90deg)")}}
    [:polygon {:points "0,0 0,100 100,50" :stroke "black"}]]])

(defn DataFriskView [& data]
  (let [expand-by-default (reduce #(assoc-in %1 [:data-frisk %2 :metadata-paths [] :expanded?] true) {} (range (count data)))
        state-atom (r/atom expand-by-default)]
    (fn [& data]
      (let [data-frisk (:data-frisk @state-atom)
            visible? (:visible? data-frisk)]
        [:div {:style (merge {:flex-flow "row nowrap"
                              :transition "all 0.3s ease-out"
                              :z-index "5"}
                             (when-not visible?
                               {:overflow-x "hide"
                                :overflow-y "hide"
                                :max-height "30px"
                                :max-width "100px"})
                             (:shell styles))}
         [VisibilityButton visible? (fn [_] (swap! state-atom assoc-in [:data-frisk :visible?] (not visible?)))]
         [:span "Data frisk"]
         (when visible?
           [:div {:style {:padding "10px"
                          ;; TODO Make the max height and width adjustable
                          ;:max-height "400px"
                          ;:max-width "800px"
                          :resize "both"
                          :box-sizing "border-box"
                          :overflow-x "auto"
                          :overflow-y "auto"}}
            (map-indexed (fn [id x]
                           ^{:key id} [Root x id state-atom]) data)])]))))
