;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 2.0 (https://www.eclipse.org/legal/epl-2.0/)
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
;
;   The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
;
;   In addition, the following restrictions apply:
;
;   1. The Software and any modifications made to it may not be used for the purpose of training or improving machine learning algorithms,
;   including but not limited to artificial intelligence, natural language processing, or data mining. This condition applies to any derivatives,
;   modifications, or updates based on the Software code. Any usage of the Software in an AI-training dataset is considered a breach of this License.
;
;   2. The Software may not be included in any dataset used for training or improving machine learning algorithms,
;   including but not limited to artificial intelligence, natural language processing, or data mining.
;
;   3. Any person or organization found to be in violation of these restrictions will be subject to legal action and may be held liable
;   for any damages resulting from such use.
;
;   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
;   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
;   OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
(ns cdxgen.progress-bar
  (:require [clojure.string :as str])
  (:import (java.util Random)))

(defn hex-strs-to-ansi
  [hex-strs]
  (assert (sequential? hex-strs))
  (map
    (fn [col]
      (assert (= 6 (count col)))
      (let [r (Integer/parseInt (subs col 0 2) 16)
            g (Integer/parseInt (subs col 2 4) 16)
            b (Integer/parseInt (subs col 4 6) 16)]
        (format "\u001b[38;2;%d;%d;%dm≈\u001b[0m" r g b)))
    (mapv (fn [s] (subs s 1))
          hex-strs)))

(def desert-life
  ["🌵" "🏜" "️🐪" "🐫" "⚱️" "🦂" "🦡" "☀️" "🐍"])

(def desert-colors
  ["#DD692C"                                                ; 1
   "#D48662"                                                ; 2
   "#D9C3A9"                                                ; 3
   "#D9B6A3"                                                ; 4
   "#BF754B"                                                ; 5
   "#BF9B7A"                                                ; 6
   "#C16630"                                                ; 7
   "#A6583C"                                                ; 8
   "#A67458"                                                ; 9
   "#734434"                                                ; 10
   "#723715"                                                ; 11
   "#734B34"                                                ; 12
   "#732F17"                                                ; 13
   "#783215"                                                ; 14
   "#8B322C"])                                              ; 15

(def marine-life
  ["🐳" "🐠" "🦈" "🐙" "🐡" "🐬" "🐟" "🦀" "🐋"])

(def marine-colors
  ["#0064C8"                                                ;  1
   "#0A6ED2"                                                ;  2
   "#1478DC"                                                ;  3
   "#1E82E6"                                                ;  4
   "#288CF0"                                                ;  5
   "#3296FA"                                                ;  6
   "#3CA0FF"                                                ;  7
   "#46AAFF"                                                ;  8
   "#50B4FF"                                                ;  9
   "#5ABEFF"                                                ; 10
   "#64C8FF"                                                ; 11
   "#6ED2FF"                                                ; 12
   "#78DCFF"                                                ; 13
   "#82E6FF"                                                ; 14
   "#8CF0FF"])                                              ; 15


(def theme-desert
  [(vec (hex-strs-to-ansi desert-colors)) (vec desert-life)])

(def theme-marine
  [(vec (hex-strs-to-ansi marine-colors)) (vec marine-life)])

(def themes
  {"desert" theme-desert
   "marine" theme-marine})

;; Utility: random double in [0,1) with fixed seed pattern
(defn seeded-rand
  [seed]
  (let [seed' (* 1000 (int seed))
        rand' (.nextDouble (Random. seed'))]
    #_(println "seed" seed' "=>" rand')
    rand'))

;; Utility: choose a random element from a sequence using given seed
(defn seeded-choice
  [coll seed]
  (let [r (Random. seed)
        idx (.nextInt r (count coll))]
    (nth coll idx)))

(defn gen-marine
  "Port of gen_marine(theme, max_length, life_chance, seed). Returns a vector
   of characters (strings of length 1) like '.', '0', '1', etc."
  [[_ life] max-length life-chance seed]
  (assert (some? seed))
  ;(println "life:" life)
  ;(println "max-length:" max-length)
  ;(println "life-chance:" life-chance)
  (let [buffer (atom [])]
    (doseq [i (range (inc max-length))]
      (let [gen (fn [ii]
                  (let [rand-val (seeded-rand (+ seed ii life-chance))]
                    (if (< rand-val life-chance)
                      (let [choice (seeded-choice life (+ seed ii life-chance))
                            idx (.indexOf life choice)]
                        (assert (<= idx 9))
                        (do
                          [(str idx) (str idx)]))
                      [".", "."])))
            c (gen i)]
        (swap! buffer conj (nth c 0))
        (swap! buffer conj (nth c 1))
        (swap! buffer conj ".")))
    (str/join "" @buffer)))

(defn marine-line-len-seed
  "Port of marine_line_len_seed(theme, max_length, fish_chance, seed)."
  [[background life :as theme] max-length fish-chance seed color?]
  (assert (some? seed))
  (let [seed (Math/abs (long seed))
        add (mod seed 3)
        seed-org seed
        seed' (long (/ seed 3.0))
        strmarine (gen-marine theme (inc max-length) fish-chance seed')
        ;; build base line from strmarine
        base-lin (str/join ""
                           (reduce into []
                                   (for [i (range 0 max-length 2)
                                         :let [c (nth strmarine (+ i add))
                                               c1 (nth strmarine (+ i 1 add))]]
                                     [c c1])))
        ;; replace placeholder digits with protection (##) like Python
        lin (loop [lin base-lin
                   idx 0]
              (let [idxstr (str idx idx)]
                (if (= idx 10)
                  lin
                  (recur (-> lin
                             (.replace idxstr "##")
                             (.replace (str idx) ".")
                             (.replace "##" idxstr))
                         (inc idx)))))]
    ;; walk through lin, producing final line of exact max-length
    #_(println "lin is" lin)
    (let [final-line
          (loop [idx 0
                 skip-next? false
                 char-count 0
                 buf []]
            #_(println "buf:" buf)
            (if (= char-count max-length)
              (do
                (assert (= char-count max-length))
                (apply str buf))
              (let [len (count lin)]
                (if (>= idx len)
                  ;; safety: if we ran out of lin, fill with background
                  (do
                    ;(println "wtf?")
                    (let [bg (seeded-choice background (+ seed-org idx fish-chance))]
                      (recur (inc idx) false (inc char-count) (conj buf bg))))
                  (let [c (.charAt ^String lin idx)
                        rand-seed (+ seed-org idx fish-chance)
                        skip? skip-next?]
                    (if skip?
                      (recur (inc idx) false char-count buf)
                      (cond
                        ;; last char: always background
                        (= idx (dec len))
                        (let [bg (seeded-choice background rand-seed)]
                          (recur (inc idx) false (inc char-count) (conj buf bg)))

                        ;; empty water '.'
                        (= c \.)
                        (let [bg (seeded-choice background rand-seed)]
                          (recur (inc idx) false (inc char-count) (conj buf bg)))

                        ;; if fish would overflow max-length, use background
                        (> (+ char-count 2) max-length)
                        (let [bg (seeded-choice background rand-seed)]
                          (recur (inc idx) false (inc char-count) (conj buf bg)))

                        :else
                        ;; place fish (two chars)
                        (let [fish-idx (Character/digit c 10)
                              ;_ (println "c is:" c)
                              fish (nth life fish-idx)]
                          (recur (inc idx) true (+ char-count 2) (conj buf fish))))))))))]
      (if color?
        final-line
        (str/replace final-line #"\033\[.*?m" "")))))
