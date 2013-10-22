(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc k]
                 (if (zero? k)
                   acc
                   (recur (* acc base) (dec k))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2))
    true
   (or (empty? seq1) (empty? seq2))
    false
   (not (= (first seq1) (first seq2)))
    false
   :else
    (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [n 0
         seq1 a-seq]
    (cond
     (empty? seq1)
      nil
     (pred (first seq1))
      n
     :else
      (recur (inc n) (rest seq1)))))

(defn avg [a-seq]
  (loop [sum 0
         n 0
         seq1 a-seq]
    (if (empty? seq1)
      (/ sum n)
      (recur (+ sum (first seq1))
             (inc n)
             (rest seq1)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [seq1 a-seq
         set1 #{}]
    (if (empty? seq1)
      set1
      (recur (rest seq1)
             (toggle set1 (first seq1))))))

(defn fast-fibo [n]
  (loop [k 1
         f-n 1
         f-n1 0]
    (cond
     (= n 0)
      0
     (= k n)
      f-n
     :else
      (recur (inc k) (+ f-n f-n1) f-n))))

(defn cut-at-repetition [a-seq]
  (loop [seen-set #{}
         build-vect []
         seq1 a-seq]
    (cond
     (empty? seq1)
      build-vect
     (contains? seen-set (first seq1))
      build-vect
     :else
      (recur (conj seen-set (first seq1))
             (conj build-vect (first seq1))
             (rest seq1)))))
