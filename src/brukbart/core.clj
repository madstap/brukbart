(ns brukbart.core
  (:require [clojure.edn :as edn]))

(defn one? "Returns true if x is one, else false"
  [x] (== 1 x))

;; Use medley
(defn map-keys [f m]
  (into (empty m)
        (map vector (map f (keys m)) (vals m))))

;; Use medley
(defn map-vals [f m]
  (into (empty m)
        (map vector (keys m) (map f (vals m)))))

;; Use medley
(defn filter-keys [pred m]
  (into (empty m) (filter (fn [[k _]] (pred k))) m))

;; Use medley
(defn filter-vals [pred m]
  (into (empty m) (filter (fn [[_ v]] (pred v))) m))

;; Use medley
(defn ffilter ; find-first
  "Like (first (filter pred coll))"
  [pred coll]
  (reduce (fn [_ x]
            (when (pred x) (reduced x)))
          nil coll))

(defn find-indices
  "Returns a sequence of indices i where (pred (nth coll i))"
  [pred coll]
  (keep-indexed (fn [i x] (when (pred x) i)) coll))

(defn reduce-indexed
  ([f init coll] (reduce-kv f init (vec coll))))

(defn find-index
  "Returns the first index i where (pred (nth coll i))"
  [pred coll]
  (reduce-indexed (fn [_ i x] (when (pred x) (reduced i))) nil coll))

;; deep-merge implementation from circleCI's frontend
(defn deep-merge* [& maps]
  (let [f (fn [old new]
            (if (and (map? old) (map? new))
              (merge-with deep-merge* old new)
              new))]
    (if (every? map? maps)
      (apply merge-with f maps)
      (last maps))))

(defn deep-merge
  "Merge nested maps. At each level maps are merged left to right. When all
  maps have a common key whose value is also a map, those maps are merged
  recursively. If any of the values are not a map then the value from the
  right-most map is chosen.
  E.g.:
  user=> (deep-merge {:a {:b 1}} {:a {:c 3}})
  {:a {:c 3, :b 1}}
  user=> (deep-merge {:a {:b 1}} {:a {:b 2}})
  {:a {:b 2}}
  user=> (deep-merge {:a {:b 1}} {:a {:b {:c 4}}})
  {:a {:b {:c 4}}}
  user=> (deep-merge {:a {:b {:c 1}}} {:a {:b {:e 2 :c 15} :f 3}})
  {:a {:f 3, :b {:e 2, :c 15}}}
  Each of the arguments to this fn must be maps:
  user=> (deep-merge {:a 1} [1 2])
  AssertionError Assert failed: (and (map? m) (every? map? ms))
  Like merge, a key that maps to nil will override the same key in an earlier
  map that maps to a non-nil value:
  user=> (deep-merge {:a {:b {:c 1}, :d {:e 2}}}
                     {:a {:b nil, :d {:f 3}}})
  {:a {:b nil, :d {:f 3, :e 2}}}"
  [& maps]
  (let [maps (remove nil? maps)]
    (assert (every? map? maps))
    (apply merge-with deep-merge* maps)))

;; -----------------
;; Macros

(defmacro it->
  "Like clojure.core/as-> with name bound to the symbol it"
  {:style/indent 1}
  [expr & forms]
  `(as-> ~expr ~'it ~@forms))

(defmacro forv
  "Eager for comprehension that returns a vector instead of a lazy-seq."
  {:style/indent 1}
  [seq-exprs body-exprs]
  `(vec (for ~seq-exprs ~body-exprs)))

(defmacro for-map
  "Like for but eager and takes a key expression and a value expression to make a map."
  {:style/indent 1}
  [seq-exprs key-exprs value-exprs]
  `(into {} (for ~seq-exprs [~key-exprs ~value-exprs])))

(defmacro vars->map
  "Takes a variable number of vars and makes a map of keyword of the var name to var value.

  For example:
  (def a 10)
  (def b 20)

  (vars->map a b) ;=> {:a 10 :b 20}"
  [& vars]
  (for-map [v vars] (keyword v) v))


(comment

  (def a 10)

  (def b 20)

  (macroexpand-1 '(vars->map a b))

  )
