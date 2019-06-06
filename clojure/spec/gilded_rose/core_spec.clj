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

(s/def ::backstage-pass (s/and ::item
                               #(<= 0 (:sell-in %) 11)
                               #(<= 30 (:quality %) 50)))

(s/def ::backstage-pass-triple-increase (s/and ::backstage-pass
                               #(<=  1 (:sell-in %) 4)
                               #(<= (:quality %) 50)))

(s/def ::backstage-pass-double-increase (s/and ::backstage-pass
                                               #(<=  5 (:sell-in %) 9)
                                               #(<= (:quality %) 50)))

;Create a generator with monotonously increasing sell-in value
(def backstage-pass-all-sell-in-range
  (sort #(compare (:sell-in %1)
                  (:sell-in %2))
        (map #(assoc % :name pass-name)
             (gen/sample
               (s/gen ::backstage-pass)
               50))))

(def backstage-pass-triple-increase-sample
  (sort #(compare (:quality %1)
                  (:quality %2))
        (map #(assoc % :name pass-name)
             (gen/sample
               (s/gen ::backstage-pass-triple-increase)
               8))))

(def backstage-pass-double-increase-sample
  (sort #(compare (:quality %1)
                  (:quality %2))
        (map #(assoc % :name pass-name)
             (gen/sample
               (s/gen ::backstage-pass-double-increase)
               8))))


(deftest check-current-inventory
  (is (= "foo"  (:name (first (c/update-quality [(c/item "foo" 0 0)])))))
  (is (s/valid? (s/coll-of ::item) c/current-inventory))
  (is (s/valid? (s/coll-of ::item) (gen/sample (s/gen ::item) 50000))))

(defn update-on-all
  [items n]
  (reduce (fn [x _] (c/update-quality x))
          items
          (range n)))

(defn- max-sell-in
  [items]
  (reduce max (map :sell-in items)))

(deftest backstage-double-increases
  (is (every? (fn [x] (<= 0 x 2)) (map #(reduce - %) (partition 2 (interleave
                                                                    (map :quality (c/update-quality
                                                                                    backstage-pass-double-increase-sample))
                                                                    (map :quality backstage-pass-double-increase-sample)))))))

(deftest backstage-triple-increases
  (is (every? (fn [x] (<= 0 x 3)) (map #(reduce - %) (partition 2 (interleave
                                         (map :quality (c/update-quality backstage-pass-triple-increase-sample))
                                         (map :quality backstage-pass-triple-increase-sample)))))))

(deftest backstage-passes-order-test
  (is (every? #(<= (:quality %) 50)
              (update-on-all backstage-pass-all-sell-in-range
                             (dec (max-sell-in backstage-pass-all-sell-in-range)))))
  (is (every? #(and (< (:sell-in %) 0)
                     (= (:quality %) 0))
              (update-on-all backstage-pass-all-sell-in-range
                             (inc (max-sell-in backstage-pass-all-sell-in-range))))))