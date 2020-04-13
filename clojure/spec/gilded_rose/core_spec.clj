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
(s/def ::item (s/keys :req-un [::name ::sell-in ::quality]))

(s/def ::no-name-item (s/keys :req-un [::sell-in ::quality]))

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
                               (ranged-sample 15 1 5 :sell-in)
                               (ranged-sample 15 45 50 :quality)))))))

(def backstage-pass-double-increase-sample
  (sort #(compare (:quality %1)
                  (:quality %2))
        (map #(assoc % :name pass-name)
             (map #(into {} %)
                  (partition 2
                             (interleave
                               (ranged-sample 15 6 10 :sell-in)
                               (ranged-sample 15 45 50 :quality)))))))

(deftest check-current-inventory-large-sample
  (is (= "foo" (:name (first (c/update-quality [(c/item "foo" 0 0)])))))
  (is (s/valid? (s/coll-of ::item) c/current-inventory))
  (is (s/valid? (s/coll-of ::item) (gen/sample (s/gen ::item) 20000))))

(defn update-on-all
  [items n]
  (reduce (fn [x _] (c/update-quality x))
          items
          (range n)))

(defn- max-sell-in
  [items]
  (reduce
    max (map :sell-in items)))

(defn- max-quality
  [items]
  (reduce
    max (map :quality items)))

(defn difference-on [item-sample]
  (map #(reduce - %)
       (partition 2 (interleave
                      (map :quality (c/update-quality item-sample))
                      (map :quality item-sample)))))


;;Add test cases close to the edge at 50
(deftest backstage-increases
  (testing "that the increases are right for their respective periods"
    (let [update-difference-on-double-sample (difference-on backstage-pass-double-increase-sample)
          update-difference-on-triple-sample (difference-on backstage-pass-triple-increase-sample)]
      (is (every? #(<= 0 % 2) update-difference-on-double-sample))
      (is (every? #(<= 0 % 3) update-difference-on-triple-sample))
      )))

(deftest backstage-passes-ordered-consumption
  (testing "that the quality never goes over 50 when past sell in"
    (is (every? #(<= (:quality %) 50)
              (update-on-all
                backstage-pass-all-sell-in-range
                (dec (max-sell-in backstage-pass-all-sell-in-range))))))
  (testing "that quality goes to 0 when past sell in"
    (is (every? #(and (< (:sell-in %) 0)
                      (= (:quality %) 0))
              (update-on-all
                backstage-pass-all-sell-in-range
                (inc (max-sell-in backstage-pass-all-sell-in-range)))))))

(def aged-brie-sample
  (sort #(compare (:quality %1)
                  (:quality %2))
        (map #(assoc % :name "Aged Brie")
             (gen/sample
               (s/gen (s/and ::no-name-item
                             #(< (:quality %) 50)))
               20))))

(deftest aged-brie-checks
  (testing "that aged brie increases quality as it gets older"
    (is (=   [{:name "Aged Brie" :quality 25 :sell-in 2}]
           (c/update-quality
             [{:name "Aged Brie" :quality 24 :sell-in 3}]))))

  (testing "that aged brie quality increases even after sell-in date"
    (is (=   [{:name "Aged Brie" :quality 22 :sell-in -6}]
           (c/update-quality
             [{:name "Aged Brie" :quality 21 :sell-in -5}]))))

  (testing "that aged brie quality stops at 50"
    (is (=  49 (max-quality
                 (update-on-all
                  aged-brie-sample
                  (inc (max-sell-in aged-brie-sample))))))))

(update-on-all aged-brie-sample 3)

;; Add property test for legendary itema

;; Add property test for ordinary itema

;; Add propery test for the new item