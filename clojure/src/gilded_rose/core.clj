(ns gilded-rose.core)

(defn is-backstage-pass? [item]
  (= "Backstage passes to a TAFKAL80ETC concert" (:name item)))

(defn is-aged-brie? [item]
  (= (:name item) "Aged Brie"))

(defn past-sell-in? [item]
  (< (:sell-in item) 0))

(defn update-quality-twice [item]
  (merge item {:quality (inc (inc (:quality item)))}))

(defn update-quality
  [items]
  (map
    (fn[item] (cond
                (and (past-sell-in? item)
                     (is-backstage-pass? item))
                (merge item {:quality 0})



                (or (is-aged-brie? item)
                    (is-backstage-pass? item))
                (if (and (is-backstage-pass? item)
                         (>= (:sell-in item) 5)
                         (< (:sell-in item) 10))
                  (update-quality-twice item)
                  (if (and (is-backstage-pass? item)
                           (>= (:sell-in item) 0)
                           (< (:sell-in item) 5))
                    (merge item {:quality (inc (inc (inc (:quality item))))})
                    (if (< (:quality item) 50)
                      (merge item {:quality (inc (:quality item))})
                      item)))



                (past-sell-in? item)
                (if (is-backstage-pass? item)
                  (merge item {:quality 0})
                  (if (or (= "+5 Dexterity Vest" (:name item))
                          (= "Elixir of the Mongoose" (:name item)))
                    (merge item {:quality (- (:quality item) 2)})
                    item))


                (or (= "+5 Dexterity Vest" (:name item))
                    (= "Elixir of the Mongoose" (:name item)))
                (merge item {:quality (dec (:quality item))})

                :else item))
  (map (fn [item]
         (if (not= "Sulfuras, Hand of Ragnaros" (:name item))
           (merge item {:sell-in (dec (:sell-in item))})
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
