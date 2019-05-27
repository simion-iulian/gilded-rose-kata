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

(defn first-backstage-increase-value?
  [item]
  (and
    (>= (:sell-in item) 5)
    (< (:sell-in item) 10)))

(defn second-backstage-increase-value?
  [item]
  (and (>= (:sell-in item) 0)
       (< (:sell-in item) 5)))

(defn update-sell-in
  [item]
  (update item :sell-in dec))

(defn update-quality
  [items]
  (map
    (fn[item] (cond
                ;Backstage pass goes to 0 past sell in
                (and (past-sell-in? item)
                     (is-backstage-pass? item))
                (quality-to-zero item)


                (or (is-aged-brie? item)
                    (is-backstage-pass? item))
                ;Backstage pass increases with 2 between 10 and 5days before sell-in
                (if (and (is-backstage-pass? item)
                         (first-backstage-increase-value? item))
                  (increase-quality item 2)
                  ;Backstage pass increases with 3 between 10 and 5days before sell-in
                  (if (and (is-backstage-pass? item)
                           (second-backstage-increase-value? item))
                    (increase-quality item 3)
                    (if (< (:quality item) 50)
                      (increase-quality item 1)
                      item)))

                ;Backstage pass is already checked to go to 0 above.
                (past-sell-in? item)
                (if (is-backstage-pass? item)
                  (quality-to-zero item)
                  (if (or (= "+5 Dexterity Vest" (:name item))
                          (= "Elixir of the Mongoose" (:name item)))
                    (decrease-quality item 2)
                    item))

                (or (= "+5 Dexterity Vest" (:name item))
                    (= "Elixir of the Mongoose" (:name item)))
                (update item :quality dec)

                :else item))
  (map (fn [item]
         (if (not= "Sulfuras, Hand of Ragnaros" (:name item))
           (update-sell-in item)
           item))
       items)))

(defn item [item-name, sell-in, quality]
  {:name item-name, :sell-in sell-in, :quality quality})

(def ^:private current-inventory[
   (item "+5 Dexterity Vest" 10 20)
   (item "Aged Brie" 2 0)
   (item "Elixir of the Mongoose" 5 7)
   (item "Sulfuras, Hand Of Ragnaros" 0 80)
   (item "Backstage passes to a TAFKAL80ETC concert" 15 20)])

(defn update-current-inventory[]
  (update-quality current-inventory))
