(ns al-jabr.core)

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

(defsemigroups
  {nil NullGroup
   Boolean JBoolField
   Integer JIntRing
   Short JShortRing
   Long JLongRing
   Float JFloatField
   Double JDoubleField
   String StringMonoid})

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

(comment
  (num-monoid) ;;=> 0
  (num-monoid 10 11) ;; => 21
  )
