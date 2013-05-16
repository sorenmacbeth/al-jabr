(ns al-jabr.core
  (:import [com.twitter.algebird
            AveragedValue DecayedValue HyperLogLogMonoid HLL
            MinHasher32 DecayedValue$ AveragedGroup$]))

(defn type-sym [algebird-type]
  (symbol (str algebird-type "$/MODULE$")))

(defn class-sym [algebird-type]
  (symbol (str "com.twitter.algebird." algebird-type "$")))

(defprotocol Semigroup
  (plus [l r]))

(defmacro defsemigroups
  [m]
  `(do ~@(for [r (map class-sym (vals m))]
           `(import ~(symbol r)))
       (extend-protocol Semigroup
         ~@(mapcat (fn [[clojure-type algebird-type]]
                     `[~clojure-type
                       (~'plus [l# r#] (.plus ~(type-sym algebird-type) l# r#))])
             m))))

;; ## Java Primitives

(defsemigroups
  {nil NullGroup
   Boolean JBoolField
   Integer JIntRing
   Short JShortRing
   Long JLongRing
   Float JFloatField
   Double JDoubleField
   String StringMonoid})

;; ## Clojure Types

(extend-protocol Semigroup
  nil
  (plus [l r] r)
  
  clojure.lang.IPersistentMap
  (plus [l r] (merge-with plus l r))

  clojure.lang.IPersistentList
  (plus [l r] (concat l r))
  
  clojure.lang.IPersistentVector
  (plus [l r] (into [] (concat l r)))

  clojure.lang.IPersistentSet
  (plus [l r] (into l r))

  clojure.lang.Ratio
  (plus [l r] (+ l r)))

(defn monoid
  "Returns a monoid function for types implementing
   the `Semigroup` protocol with the supplied `zero-fn`"
  [zero-fn]
  (fn
    ([] (zero-fn))
    ([l r] (plus l r))))

(def num-monoid (monoid (constantly 0)))
(def string-monoid (monoid str))
(def map-monoid (monoid hash-map))
(def vector-monoid (monoid vector))
(def set-monoid (monoid hash-set))
(def fn-monoid (monoid (fn [] identity)))
(def ratio-monoid (monoid (constantly 0)))

;; ## AveragedValue

(extend-protocol Semigroup
  AveragedValue
  (plus [l r]
    (.plus AveragedGroup$/MODULE$ l r)))

(def averaged-monoid (monoid (fn [] #(.zero AveragedGroup$/MODULE$))))

;; ## DecayedValue

(defn decayed-monoid [^double epsilon]
  (let [monoid (.monoidWithEpsilon DecayedValue$/MODULE$ epsilon)]
    (fn
      ([]
         (.zero monoid))
      ([^DecayedValue l ^DecayedValue r]
         (.plus monoid l r)))))

;; ## HyperLogLog

(defn mk-hyper-log-log
  "Make a `HyperLogLogMonoid` instance with the supplied
   error percent `err-percent`."
  [err-percent]
  (let [log2 (fn [x] (/ (Math/log x) (Math/log 2.0)))
        bits (->> (- (log2 104) (log2 err-percent)) Math/ceil (* 2))]
    (HyperLogLogMonoid. bits)))

(defn ->hll
  "Apply function `f` to `x`, which should return a byte array.
   The resulting byte array is passed into `monoid` which creates
   a `SparseHLL` instance."
  [^HyperLogLogMonoid monoid f x]
  (.create monoid (f x)))

(defn hll-monoid
  "Create a hyper log log monoid function from the supplied
   HyperLogLogMonoid instance `monoid`."
  [^HyperLogLogMonoid monoid]
  (fn
    ([]
       (.zero monoid))
    ([^HLL l ^HLL r]
       (.plus monoid l r))))

;; ## MinHasher

(defn mk-minhasher [t n]
  (MinHasher32. t n))

(defn mh-monoid [^MinHasher32 monoid]
  (fn
   ([]
      (.zero monoid))
   ([^bytes l ^bytes r]
      (.plus monoid l r))))
