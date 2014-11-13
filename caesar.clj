(require '[clojure.string :as str])

;; Cesar decryption
;; Find the char equivalent to e by looking for the char with
;; max occurence in a text. Then permutate all chars

;; Contract: char-occurence : string -> sequence of number
;; Purpose:  Find number of occurence of all characters in a text
;; Example:  (char-occurence "wonderfultext") =>
;; => [0 0 0 1 2 1 0 0 0 0 0 1 0 1 1 0 0 1 0 2 1 0 1 1 0 0]
(defn char-occurence
  [text]
  (reduce []))

(def alphabet (repeat 26 0))

(def text
"In cryptography, a Caesar cipher, also known as Caesar's cipher, the shift cipher, Caesar's code or Caesar shift, is one of the simplest and most widely known encryption techniques. It is a type of substitution cipher in which each letter in the plaintext is replaced by a letter some fixed number of positions down the alphabet. For example, with a left shift of 3, D would be replaced by A, E would become B, and so on. The method is named after Julius Caesar, who used it in his private correspondence.The encryption step performed by a Caesar cipher is often incorporated as part of more complex schemes, such as the Vigenère cipher, and still has modern application in the ROT13 system. As with all single alphabet substitution ciphers, the Caesar cipher is easily broken and in modern practice offers essentially no communication security.")

(defn char-to-index
  [char]
  (let [idx (int (first (str/lower-case (str char))))]
    (if (and (> idx 96) (< idx 123))
      (- idx 97)
      -1)))         

(defn index-to-char
  [idx]
  (if (and (> idx -1) (< idx 26))
    (char (+ idx 97))
    \ ))

;; Contract: inc-char-occurence : sequence char -> sequence
;; Purpose: increment occurence of a character in the alphabet
(defn inc-char-occurence
  [alphabet char]
  (let [idx (char-to-index char)]
    (if (= idx -1)
      alphabet
      (map-indexed (fn [idx itm]
                     (if (= idx (char-to-index char))
                       (inc itm)
                       itm))
                   alphabet))))

;; Contract: char-occurence : sequence string -> sequence
;; Purpose: count occurence of each character in a text
(defn char-occurence
  [alphabet text]
  (loop [remaining-text text
         result alphabet]
    (if (str/blank? remaining-text)
      result
      (recur (subs remaining-text 1) (inc-char-occurence result (first remaining-text))))))

;; Contract: max-index : sequence -> number
;; Purpose: return index of highest number in a sequence
(defn max-index
  [alphabet]
  (loop [idx 0
         max-idx 0
         max-occurence (first alphabet)
         [current-occurence & remaining] alphabet]
    (if (empty? remaining)
      max-idx
      (if (< max-occurence current-occurence)
        (recur (inc idx) idx current-occurence remaining)
        (recur (inc idx) max-idx max-occurence remaining)))))

(defn char-encrypt
  [character permutation]
  (let [idx (char-to-index character)]
    (if (= idx -1)
      character
      (char (+ 97 (mod (+ idx permutation) 26))))))

(defn text-encrypt
  [text permutation]
  (loop [final-txt ""
         remaining text]
    (if (str/blank? remaining)
      final-txt
      (let [current-char (first remaining)]
        (recur (str final-txt (char-encrypt current-char permutation)) (subs remaining 1))))))

(defn text-decrypt
  [text]
  (let [permutation (max-index (char-occurence alphabet text))]
    (text-encrypt text (+ 4 (* -1 permutation)))))
