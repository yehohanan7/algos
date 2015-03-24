(ns algos.sorting)


(defn insert [xs x]
  (cond
    (empty? xs) (list x)
    (< x (first xs)) (conj xs x)
    :else (conj (insert (rest xs) x) (first xs))))

(defn insertion-sort [sorted xs]
  (if
      (empty? xs) sorted
      (insertion-sort (insert sorted (first xs)) (rest xs))))

(defn first<=? [xs ys]
  (<= (first xs) (first ys)))


(defn merge-it [left right]
  (cond
    (empty? left) right
    (empty? right) left
    (first<=? left right) (cons (first left) (merge (rest left) right))
    :else (cons (first right) (merge left (rest right)))))

(defn split [xs]
  (split-at (/ (count xs) 2) xs))

(defn merge-sort [[x & *xs :as xs]]
  (if (empty? *xs) xs
      (let [[left right] (split xs)]
        (merge-it (merge-sort left) (merge-sort right)))))

(println (insertion-sort (list) (list 7 6 1 2 3 5))) 
