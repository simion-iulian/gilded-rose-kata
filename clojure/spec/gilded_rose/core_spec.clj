(ns gilded-rose.core-spec
(:require [clojure.test :refer :all]
          [clojure.spec.alpha :as s]
          [clojure.spec.gen.alpha :as gen]
          [gilded-rose.core :as c]))

(def pass-name "Backstage passes to a TAFKAL80ETC concert")

(s/def ::name #{"+5 Dexterity Vest"
                "Aged Brie"
                "Elixir of the Mongoose"
                "Sulfuras, Hand Of Ragnaros"
                "Backstage passes to a TAFKAL80ETC concert"})

(s/def ::sell-in (s/int-in -3 21))
(s/def ::quality (s/int-in 0 82))
(s/def ::item    (s/keys :req-un [::name ::sell-in ::quality]))

(s/def ::no-name-item   (s/keys :req-un [::sell-in ::quality]))



(defn ranged-sample
  [sample-size start end item-key]
  (gen/sample
    (gen/fmap
      #(assoc {} item-key %)
      (gen/choose start end))
    sample-size))


;Create a generator with monotonously increasing sell-in value
(def backstage-pass-all-sell-in-range
  (sort #(compare (:sell-in %1)
                  (:sell-in %2))
        (map #(assoc % :name pass-name)
             (map #(into {} %)
                  (partition 2
                             (interleave
                               (ranged-sample 50 1 11 :sell-in)
                               (ranged-sample 50 45 50 :quality)))))))

(def backstage-pass-triple-increase-sample
  (sort #(compare (:quality %1)
                  (:quality %2))
        (map #(assoc % :name pass-name)
             (map #(into {} %)
                  (partition 2
                             (interleave
                               (ranged-sample 15 1 4 :sell-in)
                               (ranged-sample 15 45 50 :quality)))))))

(def backstage-pass-double-increase-sample
  (sort #(compare (:quality %1)
                  (:quality %2))
        (map #(assoc % :name pass-name)
             (map #(into {} %)
                  (partition 2
                             (interleave
                               (ranged-sample 15 1 4 :sell-in)
                               (ranged-sample 15 45 50 :quality)))))))

(deftest check-current-inventory-large-sample
  (is (= "foo"  (:name (first (c/update-quality [(c/item "foo" 0 0)])))))
  (is (s/valid? (s/coll-of ::item) c/current-inventory))
  (is (s/valid? (s/coll-of ::item) (gen/sample (s/gen ::item) 1000))))

(defn update-on-all
  [items n]
  (reduce (fn [x _] (c/update-quality x))
          items
          (range n)))

(defn- max-sell-in
  [items]
  (reduce
    max (map :sell-in items)))

(defn difference-on [item-sample]
  (map #(reduce - %)
       (partition 2 (interleave
                      (map :quality (c/update-quality item-sample))
                      (map :quality item-sample)))))

(deftest backstage-triple-increases
  (let [update-difference-on-double-sample (difference-on backstage-pass-double-increase-sample)
        update-difference-on-triple-sample (difference-on backstage-pass-triple-increase-sample)
        required-difference (fn [x] (<= 0 x 3))]
    (is (every? required-difference update-difference-on-double-sample))
    (is (every? required-difference update-difference-on-triple-sample))
    ))

(deftest backstage-passes-ordered-consumption
  (is (every? #(<= (:quality %) 50)
              (update-on-all backstage-pass-all-sell-in-range
                             (dec (max-sell-in backstage-pass-all-sell-in-range)))))
  (is (every? #(and (< (:sell-in %) 0)
                     (= (:quality %) 0))
              (update-on-all backstage-pass-all-sell-in-range
                             (inc (max-sell-in backstage-pass-all-sell-in-range))))))