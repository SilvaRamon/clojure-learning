(ns clojure-learning.conway-game-of-life
  (:use clojure.pprint))

(defn build-world
  [rows cols]
  (vec (take rows (repeatedly #(vec (take cols (repeatedly (fn [] (rand-int 2)))))))))

;(def world (atom (build-world 30 60)))
(def world (atom [[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0]
                  [0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0]
                  [0 1 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                  [0 1 1 0 0 0 0 0 0 0 0 1 0 0 0 1 0 1 1 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                  [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]]))

(defn alive?
  [x y field]
  (= 1 (get-in field [y x])))

(defn count-neighborhood
  [x y field]
  (let [left       (alive? (dec x) y field)
        right      (alive? (inc x) y field)
        up         (alive? x (dec y) field)
        up-left    (alive? (dec x) (dec y) field)
        up-right   (alive? (inc x) (dec y) field)
        down       (alive? x (inc y) field)
        down-left  (alive? (dec x) (inc y) field)
        down-right (alive? (inc x) (inc y) field)
        result     [left right up up-left up-right down down-left down-right]]
    (count (remove false? result))))

(defn kill-cell!
  [x y field]
  (swap! field assoc-in [y x] 0))

(defn revive-cell!
  [x y field]
  (swap! field assoc-in [y x] 1))

(defn remain-alive?
  [x y field]
  (let [neighbors (count-neighborhood x y field)]
    (and (alive? x y field) (or (= neighbors 2) (= neighbors 3)))))

(defn dies?
  [x y field]
  (let [neighbors (count-neighborhood x y field)]
    (and (alive? x y field) (or (< neighbors 2) (> neighbors 3)))))

(defn revive-by-reproduction?
  [x y field]
  (let [neighbors (count-neighborhood x y field)]
    (and (not (alive? x y field)) (= neighbors 3))))

(defn print-world
  [field]
  (dotimes [y (count field)]
    (dotimes [x (count (first field))]
      (if (alive? x y field)
        (print "@")
        (print " ")))
    (println "")))

(defn generations!
  [field]
  (let [rows (count field)
        cols (count (first field))
        new-field (atom (vec (repeat rows (vec (repeat cols 0)))))]
    (dotimes [y rows]
      (dotimes [x cols]
        (cond
          (dies? x y field) (swap! new-field assoc-in [y x] 0)
          (remain-alive? x y field) (swap! new-field assoc-in [y x] 1)
          (revive-by-reproduction? x y field) (swap! new-field assoc-in [y x] 1))))
    @new-field))

(defn -main
  []
  (dotimes [gen 3000]
    (println "Generation:" gen "world [ x:" (count @world) "y:" (count (first @world)) "]")
    (print-world @world)
    (reset! world (generations! @world))
    (Thread/sleep 80)
    (print (str (char 27) "[2J"))
    ))

(-main)