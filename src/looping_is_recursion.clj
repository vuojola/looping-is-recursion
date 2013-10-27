(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
    :else false))

(defn find-first-index [pred a-seq]
  (loop [remaining-seq a-seq
         n 0]
    (cond
      (empty? remaining-seq) nil
      (pred (first remaining-seq)) n
      :else (recur (rest remaining-seq) (inc n)))))

(defn avg [a-seq]
  (loop [remaining-seq a-seq
         n 0
         sum 0]
    (if (empty? remaining-seq)
      (/ sum n)
      (recur (rest remaining-seq) (inc n) (+ sum (first remaining-seq))))))

(defn toggle [a-set elem]
  (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)))

(defn parity [a-seq]
  (loop [remaining-seq a-seq
         parity-set #{}]
    (if (empty? remaining-seq)
      parity-set
      (recur (rest remaining-seq) (toggle parity-set (first remaining-seq))))))
    

(defn fast-fibo [n]
  (cond
    (= 0 n) 0
    (= 1 n) 1
    :else
      (loop [i 2
             f-1-val 1
             f-val 1]
        (if (= i n)
          f-val
          (recur (inc i) f-val (+ f-1-val f-val))))))

(defn cut-at-repetition [a-seq]
  (loop [repetition (conj [] (first a-seq))
         remaining-seq (rest a-seq)]
    (if (or (empty? remaining-seq) (= (get repetition 0) (first remaining-seq)))
      repetition
      (recur (conj repetition (first remaining-seq)) (rest remaining-seq)))))
