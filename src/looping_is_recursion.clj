(ns looping-is-recursion)

(defn power [base exp]
  (let [pwr (fn [curr base exp]
              (if (zero? exp)
                curr
                (recur (* curr base) base (dec exp))))]
      (pwr 1 base exp)))

(defn last-element [a-seq]
  (let [last-e (fn [b-seq size]
              (if (== size 1)
                (first b-seq)
                (recur (rest b-seq) (dec size))))]
      (if (empty? a-seq) nil
        (last-e a-seq (count a-seq)))))

(defn seq= [seq1 seq2]
  (let [help (fn [sq1 sq2 size]
             (cond
               (zero? size) true
               (= (first sq1) (first sq2)) (recur (rest sq1) (rest sq2) (dec size))
               :else false))]
    (if (not(= (count seq1) (count seq2)))
      false
      (help seq1 seq2 (count seq1)))))

(defn find-first-index [pred a-seq]
  (loop [idx 0
         size (count a-seq)
         sq a-seq]
    (cond
      (zero? size) nil
      (pred (first sq)) idx
      :else (recur (inc idx) (dec size) (rest sq)))))

(defn avg [a-seq]
  (loop [sum 0
         how-many 0
         size (count a-seq)
         sq a-seq]
    (if (zero? size) (/ sum how-many)
      (recur (+ sum (first sq)) (inc how-many) (dec size) (rest sq)))))

(defn my-toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [size (count a-seq)
         st (set [])
         sq a-seq]
    (if (zero? size)
      st
      (recur (dec size) (my-toggle st (first sq)) (rest sq)))))

(defn fast-fibo [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (loop [f-n1 0
                 f-n 1
                 x 1
                 s n]
            (if (= x n) f-n
              (recur f-n (+ f-n1 f-n) (inc x) s)))))

(defn cut-at-repetition [a-seq]
  (loop [sq a-seq
         st (set nil)
         ans (vec nil)
         size (count a-seq)]
    (if (or (contains? st (first sq)) (= size 0))  ans
      (recur (rest sq) (conj st (first sq)) (conj ans (first sq)) (dec size)))))

