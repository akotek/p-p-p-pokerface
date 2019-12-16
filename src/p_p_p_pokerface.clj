(ns p-p-p-pokerface)

(def mapping
  {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (mapping fst))
    ))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (let [rank_freq (vals (frequencies (map rank hand)))]
    (or (= rank_freq (seq [3 2]))
        (= rank_freq (seq [2 3])))))

(defn two-pairs? [hand]
  (let [rank_freq (vals (frequencies (map rank hand)))]
    (or (and
          (= (count rank_freq) 3)
          (= (max rank_freq) 2)
          (four-of-a-kind? hand)))))                        ;also a two-pair

(defn straight? [hand]
  ; check ascendancy by comparing range from first to last
  (let [sorted_ranks (sort (map rank hand))
        ascending? (fn [x] (= x (range (first x) (inc (last x)))))]
    (or
      (ascending? sorted_ranks)
      (ascending? (sort (replace {14 1} sorted_ranks))))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    (high-card? hand) 0
    :else             -1)
  )
