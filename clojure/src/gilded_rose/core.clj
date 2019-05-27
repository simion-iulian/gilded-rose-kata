(ns gilded-rose.core)

(defn is-backstage-pass? [item]
  (= "Backstage passes to a TAFKAL80ETC concert" (:name item)))

(defn is-aged-brie? [item]
  (= (:name item) "Aged Brie"))

(defn past-sell-in? [item]
  (< (:sell-in item) 0))

(defn increase-quality [item val]
  (update item :quality #(+ val %)))

(defn quality-to-zero
  [item]
  (assoc item :quality 0))


(defn decrease-quality
  [item val]
  (update item :quality #(- val %)))

(defn backstage-double-increase?
  [item]
  (and
    (>= (:sell-in item) 5)
    (< (:sell-in item) 10)))

(defn backstage-triple-increase?
  [item]
  (and (>= (:sell-in item) 0)
       (< (:sell-in item) 5)))

(defn update-sell-in
  [item]
  (update item :sell-in dec))

(defn normal-item?
  [item]
  (or (= "+5 Dexterity Vest" (:name item))
      (= "Elixir of the Mongoose" (:name item))))

(defn is-legendary?
  [item]
  (= "Sulfuras, Hand of Ragnaros" (:name item)))

(defn update-quality
  [items]
  (map (fn [item]
         (cond
           (past-sell-in? item)
           (cond
             (is-backstage-pass? item)
             (quality-to-zero item)

             (normal-item? item)
             (decrease-quality item 2)

             :else
             item)

           (is-backstage-pass? item)
           (cond
             (and (backstage-triple-increase? item)
                  (<= (:quality item) 47))
             (increase-quality item 3)

             (and (backstage-double-increase? item)
                  (<= (:quality item) 48))
             (increase-quality item 2)

             (< (:quality item) 50)
             (increase-quality item 1)

             :else
             item)

           (and (is-aged-brie? item)
                (< (:quality item) 50))
           (increase-quality item 1)

           (normal-item? item)
           (update item :quality dec)

           :else item))

       (map update-sell-in (remove is-legendary? items))))

(defn item [item-name, sell-in, quality]
  {:name item-name, :sell-in sell-in, :quality quality})

(def ^:private current-inventory
  [
   (item "+5 Dexterity Vest" 10 20)
   (item "Aged Brie" 2 0)
   (item "Elixir of the Mongoose" 5 7)
   (item "Sulfuras, Hand Of Ragnaros" 0 80)
   (item "Backstage passes to a TAFKAL80ETC concert" 15 20)])

(defn update-current-inventory[]
  (update-quality current-inventory))
