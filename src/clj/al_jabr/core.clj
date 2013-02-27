(ns al-jabr.core
  (:import [com.twitter.algebird DecayedValue$ AveragedGroup$ HyperLogLogMonoid]))

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
  clojure.lang.IPersistentMap
  (plus [l r] (merge-with plus l r))

  clojure.lang.IPersistentVector
  (plus [l r] (into [] (concat l r)))

  clojure.lang.IPersistentSet
  (plus [l r] (into l r))

  clojure.lang.IFn
  (plus [l r] (comp r l))

  clojure.lang.Ratio
  (plus [l r] (+ l r)))

(defn monoid [zero-fn]
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

(defrecord AveragedValue [^long count ^double value])

(extend-protocol Semigroup
  AveragedValue
  (plus [l r]
    (let [v (.plus AveragedGroup$/MODULE$
                   (com.twitter.algebird.AveragedValue. (:count l) (:value l))
                   (com.twitter.algebird.AveragedValue. (:count r) (:value r)))]
      (AveragedValue. (.count v) (.value v)))))

(def averaged-monoid (monoid #(let [m (.zero AveragedGroup$/MODULE$)]
                                (AveragedValue. (.count m) (.value m)))))

;; ## DecayedValue

;; stateful monoid must be done outside of the `Semigroup` protocol
(defrecord DecayedValue [^double value ^double scaled-time])

(defn decayed-monoid [^double epsilon]
  (let [monoid (.monoidWithEpsilon DecayedValue$/MODULE$ epsilon)]
    (fn
      ([]
         (let [v (.zero monoid)]
           (DecayedValue. (.value v) (.scaledTime v))))
      ([l r]
         (let [v (.plus monoid
                        (.apply DecayedValue$/MODULE$ (:value l) (:scaled-time l))
                        (.apply DecayedValue$/MODULE$ (:value r) (:scaled-time r)))]
           (DecayedValue. (.value v) (.scaledTime v)))))))

;; ## HyperLogLog

;; TODO: bijection between scala and clojure collections maybe?

(defrecord SparseHLL [bits maxrhow])
(defrecord DenseHLL [bits v])

(defn hll-bijection [v]
  (cond
   (instance? com.twitter.algebird.DenseHLL v) (DenseHLL. (.bits v) (.v v))
   (instance? com.twitter.algebird.SparseHLL v) (SparseHLL. (.bits v) (.maxRhow v))
   (instance? SparseHLL v) (com.twitter.algebird.SparseHLL. (:bits v) (:maxrhow v))
   (instance? DenseHLL v) (com.twitter.algebird.DenseHLL. (:bits v) (:v v))))

(defn hll-monoid [bits]
  (let [monoid (HyperLogLogMonoid. bits)]
    (fn
      ([]
         (let [v (.zero monoid)]
           (hll-bijection v)))
      ([l r]
         (let [v (.plus monoid
                        (hll-bijection l)
                        (hll-bijection r))]
           (hll-bijection v))))))