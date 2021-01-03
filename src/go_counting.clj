(ns go-counting
  {:author "Seçkin KÜKRER"
   :todos [{:prio :high :content "Clear the code, it's too verbose."}]}
  (:require [clojure.set]))

(def ^:private internal-representation
  (let [check-coordinate-fn (fn check-coordinate-fn
                              [graph coordinate]
                              (get graph coordinate))
        is-node-type? (fn [node-type graph coordinate]
                        (= node-type ((check-coordinate-fn graph coordinate) :owner)))
        owner-representation {:black :black :white :white :empty :empty}
        ->graph (fn ->graph
                  [graph-text]
                  (letfn [(->x [n i] (mod n i))
                          (->y [n i] (quot n i))
                          (->owner [stone-representation]
                            (case stone-representation
                              \B (owner-representation :black)
                              \W (owner-representation :white)
                              (owner-representation :empty)))]
                    (let [x (-> graph-text first count)]
                      (apply merge
                             {}
                             (map #(let [coordinate [(->x %2 x) (->y %2 x)]]
                                     (identity {coordinate
                                                {:coordinate coordinate
                                                 :owner (->owner %)}}))
                                  (apply str graph-text) (range))))))
        get-neighborhoods-fn (fn get-neighborhoods-fn [graph coordinate]
                               (letfn [(generate-neighborhood-coordinate [v]
                                         (vec ((juxt #(update-in % [0] dec)
                                                     #(update-in % [0] inc)
                                                     #(update-in % [1] inc)
                                                     #(update-in % [1] dec)) v)))
                                       (validate-neighborhood-coordinates [graph coordinates]
                                         (filter (partial check-coordinate-fn graph) coordinates))]
                                 (->> coordinate
                                      generate-neighborhood-coordinate
                                      (validate-neighborhood-coordinates graph))))
        connected-empty-nodes-fn (fn get-connected-empty-nodes
                                   ([graph coord] (let [get-neighborhoods (partial get-neighborhoods-fn graph)
                                                        is-node-empty? (partial is-node-type?
                                                                                (-> owner-representation :empty)
                                                                                graph)]
                                                    (if (is-node-empty? coord)
                                                      (get-connected-empty-nodes graph
                                                                                 (set [coord])
                                                                                 (get-neighborhoods coord))
                                                      [])))
                                   ([graph traversed-coords neighborhoods]
                                    (let [get-neighborhoods (partial get-neighborhoods-fn graph)
                                          is-node-empty? (partial is-node-type?
                                                                  (-> owner-representation :empty)
                                                                  graph)]
                                      (let [new-neighborhoods (clojure.set/difference
                                                               (set (filter is-node-empty?
                                                                            neighborhoods))
                                                               traversed-coords)]
                                        (if (empty? new-neighborhoods)
                                          (clojure.set/union traversed-coords new-neighborhoods)
                                          (get-connected-empty-nodes graph
                                                                     (clojure.set/union traversed-coords new-neighborhoods)
                                                                     (mapcat get-neighborhoods (vec new-neighborhoods))))))))
        territory-fn (fn [graph coord]
                       (if (check-coordinate-fn graph coord)
                         (let [connected-nodes (connected-empty-nodes-fn
                                                graph
                                                coord)
                               possible-territory-owners (distinct
                                                          (filter (partial not= :empty)
                                                                  (map
                                                                   #(-> (check-coordinate-fn graph %) :owner)
                                                                   (mapcat
                                                                    (partial get-neighborhoods-fn graph)
                                                                    connected-nodes))))]
                           {:stones (set connected-nodes)
                            :owner (if (= 1 (count possible-territory-owners))
                                     (owner-representation (first possible-territory-owners))
                                     nil)})
                         (throw (new Exception (str "Wrong coordinate of node!"
                                                    "\nCoordinate: " coord)))))
        update-node-fn (fn update-node [graph coordinate & args]
                         (apply update-in graph coordinate args))
        territories-fn (fn f
                         ([graph-text]
                          (let [graph (->graph graph-text)]
                            (f graph true)))
                         ([graph _]
                          (if-let [first-node (first (filter #(= [:empty nil]
                                                                 ((juxt :owner :field-owner) (check-coordinate-fn graph %))) (keys graph)))]
                            (let [node-datas (territory-fn graph first-node)
                                  new-graph-data (map vector
                                                      (node-datas :stones)
                                                      (repeat (node-datas :owner)))
                                  new-graph (reduce (fn [graph [coordinate owner]]
                                                      (update-node-fn graph
                                                                      [coordinate] assoc :field-owner (get {:black :black :white :white nil :empty} owner)))
                                                    graph
                                                    new-graph-data)]
                              (f new-graph true))
                            ((fn [graph]
                               (apply merge (map (fn [[k v]]
                                                   {k (set (map (comp :coordinate second) v))})
                                                 (clojure.set/rename-keys
                                                  (select-keys
                                                   (-> (group-by (fn [v] (get (second v) :field-owner false)) graph)
                                                       (update-in [:empty] (fnil identity #{}))
                                                       (update-in [:white] (fnil identity #{}))
                                                       (update-in [:black] (fnil identity #{})))
                                                   [:empty :black :white])
                                                  {:empty :null-territory
                                                   :black :black-territory
                                                   :white :white-territory}))))
                             graph))))]
    {:owner-representation owner-representation
     :graph-converter-fn ->graph
     :is-node-type is-node-type?
     :is-node-empty? (partial is-node-type? (owner-representation :empty))
     :update-node update-node-fn
     :check-coordinate-fn check-coordinate-fn
     :get-neighborhoods-fn get-neighborhoods-fn
     :get-connected-empty-nodes-fn connected-empty-nodes-fn
     :territory-fn territory-fn
     :territories-fn territories-fn}))

(defn- territory* [graph-converter-fn territory-fn graph-text-representation-data coordinate]
  (-> graph-text-representation-data
      (graph-converter-fn)
      (territory-fn coordinate)))

(def territory (partial territory*
                        (internal-representation :graph-converter-fn)
                        (internal-representation :territory-fn)))

(def territories (internal-representation :territories-fn))
