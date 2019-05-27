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

(s/def ::sell-in integer?)
(s/def ::quality (s/and integer? #(> % 0)))

(s/def ::item (s/keys :req-un [::name ::sell-in ::quality]))


(gen/sample (s/gen ::item) 10000)


(def current-inventory
  '( {:name "+5 Dexterity Vest", :sell-in 9, :quality 19}
     {:name "Aged Brie", :sell-in 1, :quality 1}
     {:name "Elixir of the Mongoose", :sell-in 4, :quality 6}
     {:name "Sulfuras, Hand Of Ragnaros", :sell-in -1, :quality 80}
     {:name "Backstage passes to a TAFKAL80ETC concert", :sell-in 14, :quality 21}))

(deftest check-current-inventory
  (is (= current-inventory (c/update-current-inventory)))
  (is (= "foo" (:name (first (c/update-quality [(c/item "foo" 0 0)])))))
  (is (s/valid? (s/coll-of ::item) current-inventory))
  (is (s/valid? (s/coll-of ::item) (gen/sample (s/gen ::item) 10000))))
