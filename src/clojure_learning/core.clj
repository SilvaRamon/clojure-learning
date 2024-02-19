(ns clojure-learning.core
  (:use [clojure.pprint :only [pprint]]))

(defn random-distinct-int [n]
  (take n (distinct (repeatedly #(rand-int 100)))))

(def numbers (atom {:deck (random-distinct-int 15), :left [], :middle [], :right []}))

(defn move-from-to [from to]
  (when (not (empty? (from @numbers)))
    (let [n (first (from @numbers))]
      (swap! numbers update-in [from] rest)
      (swap! numbers update-in [to] conj n))))

(defn take-five []
  (dotimes [_ 5]
    (do (move-from-to :deck :left)
        (move-from-to :deck :middle)
        (move-from-to :deck :right))))

(defn print-decks []
  (do
    (dissoc @numbers :deck)
    (pprint @numbers)))

(defn start-round []
  (take-five)
  (print-decks))

(defn choose-option [option]
  (cond
    (= option "l")
      (do
        (swap! numbers assoc :deck (concat (:deck @numbers) (:right @numbers) (:left @numbers) (:middle @numbers)))
        (swap! numbers assoc :left [] :middle [] :right []))
    (= option "m")
      (do
        (swap! numbers assoc :deck (concat (:deck @numbers) (:left @numbers) (:middle @numbers) (:right @numbers)))
        (swap! numbers assoc :left [] :middle [] :right []))
    (= option "r")
      (do
        (swap! numbers assoc :deck (concat (:deck @numbers) (:middle @numbers) (:right @numbers) (:left @numbers)))
        (swap! numbers assoc :left [] :middle [] :right [])))
  (start-round))

(start-round)
(println "Choose l, m or r")
(let [option (read-line)] (choose-option option))
(println "Choose l, m or r")
(let [option (read-line)] (choose-option option))
(println "Choose l, m or r")
(let [option (read-line)] (choose-option option))

(println (str "Guess: " (get (:middle @numbers) 2)))