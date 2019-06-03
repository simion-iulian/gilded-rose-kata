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
                               #(<= -1 (:sell-in %) 11)
                               #(<= 30 (:quality %) 50)))

;Create a generator with monotonously increasing sell-in value
(def backstage-pass-sample
  (sort #(compare (:sell-in %1)
                  (:sell-in %2))
        (map #(assoc % :name pass-name)
             (gen/sample
               (s/gen ::backstage-pass)
               50))))

(def backstage-pass
  {:name "Backstage passes to a TAFKAL80ETC concert", :sell-in 10, :quality 33})


(defn- first-stage-backstage-pass
  []
  (c/update-quality [backstage-pass]))

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

(defn update-on-all
  [items n]
  (reduce (fn [x _] (c/update-quality x))
          items
          (range n)))

(defn- update-quality-repeatedly
  [items n]
  (first (update-on-all items n)))

(defn- max-sell-in
  [items]
  (reduce max (map :sell-in items)))

;Increasing by 1
;Increasing by 2
;Increasing by 3

(deftest backstage-passes-order-test
  (is (= 35 (:quality (first (first-stage-backstage-pass)))))
  (is (= 37 (:quality (update-quality-repeatedly (first-stage-backstage-pass) 1))))
  (is (= 39 (:quality (update-quality-repeatedly (first-stage-backstage-pass) 2))))
  (is (= 46 (:quality (first (c/update-quality [(-> backstage-pass
                                                    (assoc :sell-in 5)
                                                    (assoc :quality 43))])))))
  (is (= 50 (:quality (update-quality-repeatedly [(-> backstage-pass
                                                      (assoc :sell-in 5)
                                                      (assoc :quality 43))]
                                                 5))))
  (is (every?  #(<= (:quality %) 50)
               (update-on-all backstage-pass-sample (dec (max-sell-in backstage-pass-sample)))))
  (is (every?  #(and (< (:sell-in %) 0)
                     (= (:quality %) 0))
               (update-on-all backstage-pass-sample (inc (max-sell-in backstage-pass-sample))))))