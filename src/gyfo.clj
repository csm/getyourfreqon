(ns gyfo)

(defn pad-to-even
  [^String s & {:keys [pad-char] :or {pad-char \space}}]
  (if (odd? (.length s))
    (str pad-char s)
    s))

(defn pad-to
  ([^String s n] (pad-to s n \space))
  ([^String s n pad-char]
   (if (<= n (.length s))
     s
     (recur (str pad-char s) n pad-char))))

(defn unhex-nybble
  [n]
  (let [n (int n)]
    (cond
      (<= (int \0) n (int \9)) (- n (int \0))
      (<= (int \a) n (int \f)) (+ 10 (- n (int \a)))
      (<= (int \A) n (int \F)) (+ 10 (- n (int \A))))))

(defn hex->bytes
  [^String hex]
  (-> hex
      (.replaceAll "\\s+" "")
      (pad-to-even :pad-char \0)
      (->> (partition 2)
           (map (fn [[u l]]
                  (.byteValue
                    (bit-or (bit-shift-left (unhex-nybble u) 4)
                            (unhex-nybble l)))))
           (byte-array))))

(defn bytes->binary
  [b]
  (reduce str (map (fn [b] (pad-to (Integer/toBinaryString (bit-and b 0xFF)) 8 \0)) b)))

(defn hex->binary
  [h]
  (bytes->binary (hex->bytes h)))

(comment
  ; repeated sequences of bits, with a clear pattern:
  ; 000057c0  c6 0c b2 63 06 48 b9 f6  b5 ad 6b 5a d6 b5 ad 6b  |...c.H....kZ...k|
  ; 000057d0  5a d6 b5 ad 6b 5a d6 b5  ad 6b 5a d6 b5 ad 6b 5a  |Z...kZ...kZ...kZ|
  ; 000057e0  d6 b5 ad 6b 5a d6 b5 ad  6b 5a d6 b5 ad 6b 5a d6  |...kZ...kZ...kZ.|
  (hex->binary "6b 5a d6 ad")
  ; => "01101011010110101101011010101101"

  ; 00036080  dd bb 76 ed db b7 6e dd  bb 76 ed db b7 6e dd bb  |..v...n..v...n..|
  (hex->binary "dd bb 76 ed db b7 6e")
  ; => "11011101101110110111011011101101110110111011011101101110"

  ; repeated bytes (with maybe some lost bits, so things get shifted):
  ;000076f0  d0 d9 2d 92 cd 88 bd bd  bd bd bd bd bd bd bd bd  |..-.............|
  ;00007700  bd bd bd bd bd bd bd bd  bd bd bd bd bd bd bd bd  |................|
  ;*
  ;00013070  bd bd bd bd bd bd bd bd  bd bd bd bd bd bd bb 7b  |...............{|
  ;00013080  7b 7b 7b 7b 7b 7b 7b 7b  7b 7b 7b 7b 7b 7b 7b 7b  |{{{{{{{{{{{{{{{{|
  ;*
  ;00013e70  7b 7b 7b 7b 7b 7b 7b 7b  7b 7b 7b 7b 7b 7b 76 f6  |{{{{{{{{{{{{{{v.|
  ;00013e80  f6 f6 f6 f6 f6 f6 f6 f6  f6 f6 f6 f6 f6 f6 f6 f6  |................|
  ;*
  ;00015070  f6 f6 f6 f6 f6 f6 f6 f6  f6 f6 f6 f6 f6 f6 ed ed  |................|
  ;00015080  ed ed ed ed ed ed ed ed  ed ed ed ed ed ed ed ed  |................|
  ;*
  ;00015670  ed ed ed ed ed ed ed ed  ed ed ed ed ed ed db db  |................|
  ;00015680  db db db db db db db db  db db db db db db db db  |................|
  ;*
  ;00015c70  db db db db db db db db  db db db db db db b7 b7  |................|
  ;00015c80  b7 b7 b7 b7 b7 b7 b7 b7  b7 b7 b7 b7 b7 b7 b7 b7  |................|
  ;*
  ;00016270  b7 b7 b7 b7 b7 b7 b7 b7  b7 b7 b7 b7 b7 b7 6f 6f  |..............oo|
  ;00016280  6f 6f 6f 6f 6f 6f 6f 6f  6f 6f 6f 6f 6f 6f 6f 6f  |oooooooooooooooo|
  ;*
  ;00016870  6f 6f 6f 6f 6f 6f 6f 6f  6f 6f 6f 6f 6f 6e de de  |ooooooooooooon..|
  ;00016880  de de de de de de de de  de de de de de de de de  |................|
  ;*
  ;00016e70  de de de de de de de de  de de de de de dd bd bd  |................|
  ;00016e80  bd bd bd bd bd bd bd bd  bd bd bd bd bd bd bd bd  |................|
  ;*
  ;00017470  bd bd bd bd bd bd bd bd  bd bd bd bd bd bb 7b 7b  |..............{{|
  ;00017480  7b 7b 7b 7b 7b 7b 7b 7b  7b 7b 7b 7b 7b 7b 7b 7b  |{{{{{{{{{{{{{{{{|
  ;*
  ;00017a70  7b 7b 7b 7b 7b 7b 7b 7b  7b 7b 7b 7b 7b 76 f6 f6  |{{{{{{{{{{{{{v..|
  ;00017a80  f6 f6 f6 f6 f6 f6 f6 f6  f6 f6 f6 f6 f6 f6 f6 f6  |................|
  ;*
  ;00018070  f6 f6 f6 f6 f6 f6 f6 f6  f6 f6 f6 f6 f6 ed ed ed  |................|
  ;00018080  ed ed ed ed ed ed ed ed  ed ed ed ed ed ed ed ed  |................|
  ;*
  ;0001ea70  ed ed ed ed ed ed ed ed  ed ed ed ed ed db db db  |................|
  ;0001ea80  db db db db db db db db  db db db db db db db db  |................|
  ;*
  (print
    (hex->binary "bd") \newline
    (hex->binary "7b") \newline
    (hex->binary "76") \newline
    (hex->binary "ed") \newline
    (hex->binary "db") \newline
    (hex->binary "b7") \newline
    (hex->binary "6f") \newline
    (hex->binary "6e") \newline
    (hex->binary "de") \newline
    (hex->binary "dd") \newline)
  "")
