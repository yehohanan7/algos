algos.sandbox
  (:require [clojure.set :as sql]))

(def ascii (map char (range 65 (+ 65 26))))

(defn rand-str [sz alphabet]
  (apply str (repeatedly sz #(rand-nth alphabet))))

(def rand-sym #(symbol (rand-str %1 %2)))

(def rand-key #(keyword (rand-str %1 %2)))

(defn rand-vec [& generators]
  (into [] (map #(%) generators)))


(defn rand-map [sz kgen vgen]
  (into {} (repeatedly sz #(rand-vec kgen vgen))))



;;Invent sourcing example

(defn valid? [event]
  (boolean (:result event)))


(defn effect [{:keys [ab h] :or {ab 0 h 0}} event]
  (let [ab (inc ab) h (if (= :hit (:result event)) (inc h) h)
        avg (double (/ h ab))]
    {:ab ab :h h :avg avg}))

(defn apply-effect [state event]
  (if (valid? event)
    (effect state event)
    state))


(def effect-all #(reduce apply-effect %1 %2))

(def events (repeatedly 100 (fn [] (rand-map 1 #(-> :result)
                                             #(if (< (rand-int 10) 3) :hit :out)))))

(def fx-timeline #(reductions apply-effect %1 %2))
(fx-timeline {} (take 3 events))


(def PLAYERS #{{:player "Nick", :ability 32}
               {:player "Matt", :ability 26}
               {:player "Ryan", :ability 19}})

(defn lookup [db player]
  (first (sql/select #(= name (:player %)) db)))


(defn update-stats [db event]
  (let [player (lookup db (:player event))
        less-db (sql/difference db #{player})]
    (conj less-db (merge player (effect player event)))))


(defn commit-event [db event]
  (dosync (alter db update-stats event)))


(defn rand-event [{ability :ability}]
  (let [able (numerator ability)
        max  (denominator ability)]
    (rand-map 1
              #(-> :result)
              #(if (< (rand-int max) able)
                 :hit
                 :out))))
