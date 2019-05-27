(ns gilded-rose.core-spec
(:require [clojure.test :refer :all]
          [clojure.spec.alpha :as s]
          [clojure.spec.gen.alpha :as gen]
          [gilded-rose.core :as c]))

(s/def ::name #{"+5 Dexterity Vest"
                "Aged Brie"
                "Elixir of the Mongoose"
                "Sulfuras, Hand Of Ragnaros"
                "Backstage passes to a TAFKAL80ETC concert"})

(s/def ::sell-in (s/int-in -5 25))
(s/def ::quality (s/int-in 0 90))
(s/def ::item    (s/keys :req-un [::name ::sell-in ::quality]))

(def test-backstage-pass
  {:name "Backstage passes to a TAFKAL80ETC concert", :sell-in 10, :quality 33})


(defn- first-stage-backstage-pass
  []
  (c/update-quality [test-backstage-pass]))

(def current-inventory
  '( {:name "+5 Dexterity Vest", :sell-in 9, :quality 19}
     {:name "Aged Brie", :sell-in 1, :quality 1}
     {:name "Elixir of the Mongoose", :sell-in 4, :quality 6}
     {:name "Sulfuras, Hand Of Ragnaros", :sell-in -1, :quality 80}
     {:name "Backstage passes to a TAFKAL80ETC concert", :sell-in 14, :quality 21}))

(deftest check-current-inventory
  (is (= current-inventory (c/update-current-inventory)))
  (is (= "foo"  (:name (first (c/update-quality [(c/item "foo" 0 0)])))))
  (is (s/valid? (s/coll-of ::item) current-inventory))
  (is (s/valid? (s/coll-of ::item) (gen/sample (s/gen ::item) 50000))))

(deftest backstage-passes-order-test
  (is (= 35 (:quality (first (first-stage-backstage-pass)))))
  (is (= 37 (:quality (first (reduce (fn [x _] (c/update-quality x))
                                     (first-stage-backstage-pass)
                                     (range 1))))))
  (is (= 39 (:quality (first (reduce (fn [x _] (c/update-quality x))
                                     (first-stage-backstage-pass)
                                     (range 2))))))
  (is (= 46 (:quality (first (c/update-quality [(-> test-backstage-pass
                                                    (assoc :sell-in 5)
                                                    (assoc :quality 43))])))))
  (is (= 50 (:quality (first (reduce (fn [x _] (c/update-quality x))
                                     [(-> test-backstage-pass
                                          (assoc :sell-in 5)
                                          (assoc :quality 43))]
                                     (range 5))))))
  (is (= -1 (:sell-in (first (reduce (fn [x _] (c/update-quality x))
                                    [(-> test-backstage-pass
                                         (assoc :sell-in 5)
                                         (assoc :quality 43))]
                                    (range 6))))))
  (is (= 0 (:quality (first (reduce (fn [x _] (c/update-quality x))
                                     [(-> test-backstage-pass
                                          (assoc :sell-in 5)
                                          (assoc :quality 43))]
                                     (range 6)))))))