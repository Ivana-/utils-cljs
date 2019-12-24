(ns charts.svg
  (:require [reagent.core :as r]
            [clojure.string :as str]))

(defn chart [{:keys [objects style padding-percent]}]
  (let [state (r/atom nil)

        padding-percent (or padding-percent 5)
        ; norm (fn [x limit] (* x limit))
        norm (fn [x limit] (let [p (/ padding-percent 100)]
                             (Math/round (+ (* x limit (- 1 p p)) (* limit p)))))
        norm-x #(norm % (:area-width @state))
        norm-y #(norm % (:area-height @state))

        get-normalized-coords (fn [e]
                                (let [{:keys [dx dy area-node area-width area-height]} @state
                                      area-bounds (.getBoundingClientRect area-node)
                                      ; x (/ (- (.-clientX e) (.-left area-bounds) (.-clientLeft area-node) dx) area-width)
                                      ; y (/ (- (.-clientY e) (.-top area-bounds)  (.-clientTop area-node)  dy) area-height)
                                      offset-x (* 0.01 padding-percent area-width)
                                      offset-y (* 0.01 padding-percent area-height)
                                      base-x (- area-width (* 2 offset-x))
                                      base-y (- area-height (* 2 offset-y))
                                      x (/ (- (.-clientX e) offset-x (.-left area-bounds) (.-clientLeft area-node) dx) base-x)
                                      y (/ (- (.-clientY e) offset-y (.-top area-bounds)  (.-clientTop area-node)  dy) base-y)]
                                  [x y]))

        handle-mouse-move (fn mouse-move [e]
                            (when-let [on-drag (:on-drag @state)]
                              (let [{:keys [dragging-id]} @state
                                    [x y] (get-normalized-coords e)]
                                (when dragging-id (on-drag dragging-id x y)))))

        handle-mouse-up  (fn mouse-up [e]
                           ;;(prn "up")
                           (let [{:keys [dragging-id on-drag-end]} @state
                                 [x y] (get-normalized-coords e)]
                             (swap! state dissoc :dragging-id :on-drag-start :on-drag :on-drag-end)
                             (.removeEventListener js/document "mousemove" handle-mouse-move)
                             (.removeEventListener js/document "mouseup" mouse-up)
                             (when on-drag-end (on-drag-end dragging-id x y))))

        handle-mouse-down (fn [e]
                             ;;(prn "down")
                            (.removeAllRanges (.getSelection js/window))
                            (let [bounds (-> e .-target .getBoundingClientRect)
                                  dx (- (.-clientX e) (/ (+ (.-left bounds) (.-right bounds)) 2))
                                  dy (- (.-clientY e) (/ (+ (.-top bounds) (.-bottom bounds)) 2))
                                  dragging-id (-> e .-currentTarget (.getAttribute "data-id"))
                                  [x y] (get-normalized-coords e)]
                              (swap! state assoc :dragging-id dragging-id :dx dx :dy dy)
                              (.addEventListener js/document "mousemove" handle-mouse-move)
                              (.addEventListener js/document "mouseup" handle-mouse-up)
                              (when-let [on-drag-start (:on-drag-start @state)]
                                (on-drag-start dragging-id x y))))

        handle-window-resize (fn [e] (swap! state assoc :window-resized? true))

        mouse-down (fn [params]
                     (fn [e]
                       (swap! state merge (select-keys params [:on-drag-start :on-drag :on-drag-end]))
                       (handle-mouse-down e)))

        process-object (fn [[obj-type {:keys [on-drag-start on-drag on-drag-end] :as params} :as object]]
                         (when object
                           [obj-type
                            (cond-> (case obj-type
                                      :circle (-> params (update :cx norm-x) (update :cy norm-y))
                                      :polyline (update params :points #(->> %
                                                                             (map (fn [{:keys [x y]}] (str (norm-x x) "," (norm-y y))))
                                                                             (str/join " ")))
                                      params)
                              (or on-drag-start on-drag on-drag-end)
                              (-> (assoc :onMouseDown (mouse-down params))
                                  (dissoc :on-drag-start :on-drag :on-drag-end)))]))]

    (r/create-class
     {:component-did-mount (fn [this]
                             ;  (prn "component-did-mount")
                             (.addEventListener js/window "resize" handle-window-resize)
                             (swap! state assoc :area-node (r/dom-node this)))

      :component-will-unmount (fn [this]
                                (.removeEventListener js/window "resize" handle-window-resize))

      :component-did-update (fn [prev-props prev-state]
                              ; (prn "component-did-update")
                              (when-let [root (:area-node @state)]
                                (let [bounds (.getBoundingClientRect root)]
                                  (swap! state assoc
                                         :window-resized? false
                                         :area-width (- (.-right bounds)  (.-left bounds))
                                         :area-height (- (.-bottom bounds) (.-top bounds))))))

      :reagent-render (fn [{:keys [objects]}]
                        ; (prn "reagent-render")
                        (let [processed-objects (keep process-object objects)]
                          [:div.svg-container {:style {:width "100%" :height "100%"
                                                       :overflow :hidden}}
                           [:svg {; :width area-width :height area-height
                                  ; :viewBox (str/join " " [top-left-x top-left-y width height])
                                  :width "100%" :height "100%"
                                  :style style}
                            (when-not (empty? processed-objects) (into [:<>] processed-objects))]]))})))
