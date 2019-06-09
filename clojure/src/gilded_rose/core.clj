(ns gilded-rose.core)

(defn backstage-pass?
  [item]
  (= "Backstage passes to a TAFKAL80ETC concert" (:name item)))

(defn aged-brie?
  [item]
  (= (:name item) "Aged Brie"))

(defn past-sell-in?
  [item]
  (<= (:sell-in item) 0))

(defn increase-quality
  [item val]
  (if (< (:quality item) 50)
    (update item :quality #(+ % val))
    item))

(defn quality-to-zero
  [item]
  (assoc item :quality 0))

(defn decrease-quality
  [item val]
  (update item :quality #(- % val)))

(defn double-increase?
  [item]
  (<= (:sell-in item) 10))

(defn triple-increase?
  [item]
  (<= (:sell-in item) 5))

(defn update-sell-in
  [item]
  (update item :sell-in dec))

(defn normal-item?
  [item]
  (some #{(:name item)} #{"+5 Dexterity Vest"
                          "Elixir of the Mongoose"}))

(defn legendary?
  [item]
  (= "Sulfuras, Hand of Ragnaros" (:name item)))

(defn conjured?
  [item]
  (= "Conjured" (:name item)))

(defn- process-past-sell-in
  [item]
  (cond
    (backstage-pass? item)
    (quality-to-zero item)

    (normal-item? item)
    (decrease-quality item 2)

    :else
    item))

(defn- process-back-stage-pass
  [item]
  (cond
    (triple-increase? item)
    (if (<= (:quality item) 47)
      (increase-quality item 3)
      (assoc item :quality 50))

    (and (double-increase? item)
         (<= (:quality item) 48))
    (increase-quality item 2)

    (< (:quality item) 50)
    (increase-quality item 1)

    :else
    item))

(defn- process-item
  [item]
  (cond
    (aged-brie? item)
    (increase-quality item 1)

    (past-sell-in? item)
    (process-past-sell-in item)

    (backstage-pass? item)
    (process-back-stage-pass item)

    ;(conjured? item)
    ;(decrease-quality item 2)

    (normal-item? item)
    (decrease-quality item 1)

    :else item))

(defn update-quality
  [items]
  (map update-sell-in
       (map process-item (remove legendary? items))))

(defn item [item-name, sell-in, quality]
  {:name item-name, :sell-in sell-in, :quality quality})

(def current-inventory
  [
   (item "+5 Dexterity Vest" 10 20)
   (item "Aged Brie" 2 0)
   (item "Elixir of the Mongoose" 5 7)
   (item "Sulfuras, Hand Of Ragnaros" 0 80)
   (item "Backstage passes to a TAFKAL80ETC concert" 15 20)])

(defn update-current-inventory[]
  (update-quality current-inventory))
