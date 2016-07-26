(ns brukbart.core
  (:require [clojure.spec :as s]
            [brukbart.dbg :refer [p< pp<]]
            [clojure.spec.test :as stest]
            [clojure.spec.gen :as gen]))

(defn one? "Returns true if x is one, else false"
  [x] (== 1 x))

(defn two? "Returns true if x is two, else false"
  [x] (== 2 x))

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
(defn ffilter
  "Like (first (filter pred coll))"
  [pred coll]
  (reduce (fn [_ x]
            (when (pred x) (reduced x)))
          nil coll))

(defn find-indices
  "Returns a sequence of indices i where (pred (nth coll i))
  Returns a transducer when called without a collection."
  ([pred]
   (keep-indexed (fn [i x] (when (pred x) i))))
  ([pred coll]
   (sequence (find-indices pred) coll)))

(defn reduce-indexed
  "Reduce with index, expects a function with arity [acc index x]"
  [f init coll]
  (reduce-kv f init (vec coll)))

(s/fdef reduce-indexed
  :args (s/cat :reducing-fn
               (s/fspec :args
                        (s/cat :accumulator any?
                               :index nat-int?
                               :input any?))
               :init any?
               :coll seqable?))

(defn find-index
  [pred coll]
  (reduce-indexed (fn [_ i x] (when (pred x) (reduced i))) nil coll))

(def ^:private unpack-keysets
  "Takes a map that may have sets as keys and unpacks the sets.
    {#{1 2} :one-or-two, 3 :three} => {1 :one-or-two, 2 :one-or-two, 3 :three}"
  (partial into {}
           (mapcat (fn [[k-or-ks v]]
                     (if (set? k-or-ks)
                       (for [k k-or-ks] [k v])
                       [[k-or-ks v]])))))

(s/def ::keyset-map
  (s/map-of (s/or :index nat-int?
                  :set (s/coll-of nat-int?))
            any?))

(defn interpose-indexed
  "Returns a seq of the elements of coll with sep interposed at the indices provided.
  Returns a transducer when called without a collection."
  ([sep indices]
   (let [indices (set indices)]
     (fn [rf]
       (let [i (volatile! -1)]
         (fn
           ([] (rf))
           ([result] (rf result))
           ([result input]
            (if (indices (vswap! i inc))
              (-> result (rf sep) (rf input))
              (rf result input))))))))
  ([sep indices coll]
   (sequence (interpose-indexed sep indices) coll)))

(s/fdef interpose-indexed
  :args (s/cat :sep any?
               :indices (s/coll-of nat-int?)
               :coll (s/? seqable?)))

(defn inject-indexed
  "Takes a map of indices or sets of indices to values, injects the values at indices.
  Returns a transducer when called without a collection."
  ([index->x]
   (let [i->x (unpack-keysets index->x)]
     (fn [rf]
       (let [curr-i (volatile! -1)]
         (fn
           ([] (rf))
           ([result] (rf result))
           ([result input]
            (let [i (vswap! curr-i inc)
                  x (i->x i ::none)]
              (if-not (= ::none x)
                (-> result (rf x) (rf input))
                (rf result input)))))))))
  ([index->x coll]
   (sequence (inject-indexed index->x) coll)))

(s/fdef inject-indexed
  :args (s/cat :index->x ::keyset-map
               :coll (s/? seqable?)))

(defn replace-indexed
  "Given a map of indices to replacements,
  replaces the items at the indices with their replacements.
  Returns a transducer when called without a collection."
  ([index->x]
   (let [i->x (unpack-keysets index->x)]
     (map-indexed
      (fn [i x]
        (let [replacement (i->x i ::none)]
          (if (= ::none replacement)
            x
            replacement))))))
  ([index->x coll]
   (sequence (replace-indexed index->x) coll)))

(s/fdef replace-indexed
  :args (s/cat :index->x ::keyset-map
               :coll (s/? seqable?)))

(defn find-index
  "Returns the first index i where (pred (nth coll i))"
  [pred coll]
  (reduce-indexed (fn [_ i x] (when (pred x) (reduced i))) nil coll))

(defn find-indices
  "Returns a sequence of the indices where (pred (nth coll i))
  Returns a transducer when called without a collection."
  ([pred]
   (keep-indexed (fn [i x] (when (pred x) i))))
  ([pred coll]
   (sequence (find-indices pred) coll)))

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
  Like merge, a key that maps to nil will override the same key in an earlier
  map that maps to a non-nil value:
  user=> (deep-merge {:a {:b {:c 1}, :d {:e 2}}}
                     {:a {:b nil, :d {:f 3}}})
  {:a {:b nil, :d {:f 3, :e 2}}}"
  [& maps]
  (if-let [maps (remove nil? maps)]
    (apply merge-with deep-merge* maps)
    {}))

(s/fdef deep-merge :args (s/* (s/nilable map?)), :ret map?)

(defn month-keys
  "Three letter keys for the months of the year.
  Takes an optional lang parameter (:en :pt :no), default :en"
  ([] (month-keys :en))
  ([lang]
   (case lang
     :en [:jan :feb :mar :apr :mai :jun :jul :aug :sep :oct :nov :dec]
     :pt [:jan :fev :mar :abr :mai :jun :jul :ago :set :out :nov :dec]
     :no [:jan :feb :mar :apr :mai :jun :jul :aug :sep :okt :nov :des]
     (month-keys))))

;; Taken from the clojurescript compiler.
;; https://github.com/clojure/clojurescript/blob/ce25dce6c332f395e550d6b572f76e3482b1acec/src/main/clojure/cljs/util.cljc#L212
(defn levenshtein
  "The the minimum number of single-element edits needed to
   transform str1 into str2.
  Works with sequences."
  [str1 str2]
  (let [str1 (seq str1)
        str2 (seq str2)
        lev (fn [f s1 s2]
              (cond
                (empty? s1) (count s2)
                (empty? s2) (count s1)
                :else (let [cost (if (= (first s1) (first s2)) 0 1)]
                        (min (inc (f f (rest s1) s2))
                             (inc (f f s1 (rest s2)))
                             (+ cost (f f (rest s1) (rest s2)))))))
        lev' (memoize lev)]
    (lev' lev' str1 str2)))

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
  "Like for but eager and takes a key expression and a value expression to make a map.
  Equal keys are treated as if repeatedly assoced."
  {:style/indent 1}
  [seq-exprs key-exprs value-exprs]
  `(into {} (for ~seq-exprs [~key-exprs ~value-exprs])))

(defmacro vars->map
  "Takes a variable number of symbols that resolve to some value
  returns a map of (keyword (name symbol)) -> value.

  (def a 10)
  (let [b 20]
    (vars->map foo/a b)) ;=> {:a 10 :b 20}
  "
  [& vars] (for-map [v vars] (keyword (name v)) v))

(s/fdef vars->map
  :args (s/* symbol?)
  :ret (s/map-of simple-keyword? symbol?)
  :fn #(->> [(:args %) (vals (:ret %)) (keys (:ret %))]
         (map (partial map name)) (map set) (apply =)))

(defmacro for-loop
  "Imperative for-loop. Runs untill check is false.

  (for-loop [i 0, (< i 10), (inc i)]
    (println i))"
  {:style/indent 1}
  [[sym init check change :as params] & steps]
  `(loop [~sym ~init value# nil]
     (if ~check
       (let [new-value# (do ~@steps)]
         (recur ~change new-value#))
       value#)))

(s/fdef for-loop
  :args (s/cat :loop-params
               (s/and vector?
                      (s/cat :loop-variable symbol?
                             :init-value any?
                             :check-expr any?
                             :change-expr any?))
               :loop-body
               (s/+ any?)))

(comment
  (stest/instrument)

  )
