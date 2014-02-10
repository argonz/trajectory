

;; creating positions - for the some manner 
;; so this is how this is how - we will have 
;; 
  
;; generating star - raster setting - which is so shitty all around :)
;; that's gonna around :)
;; I'm dead if it's the thing :) - we will see - more have to :)


;; simplest floodings - manhatten flooding - great 
(defn env->rst [e]
  (let [[d0 d1] (:dim e)]
    (to-array-2d 
     (for [i0 (range d0)]
       (for [i1 (range d1)]
         (if (ipos-env->obs? [i0 i1] e)
           -1 
           0))))))

(defn rst->dim [r]
  [(count r) (count (aget r 0))])
(defn pos-rst->val [[p0 p1] r]
  (aget r p0 p1))
(defn pos->mnht-neigh-poss [[p0 p1]]
  [[(inc p0) p1]
   [(dec p0) p1]
   [p0 (inc p1)]
   [p0 (dec p1)]])
(defn pos-rst->mnht-neighs [p r]
  (map pos-rst->val (pos->mnht-neigh-poss p) (repeat r)))
(defn pos-rst->positive-neighs? [p r]
  (some pos? (pos-rst->mnht-neighs p r)))

(defn rst->mnht-flood-step [r]
  (let [[d0 d1] (rst->dim r)]
    (to-array-2d
     (for [i0 (range d0)]
       (for [i1 (range d1)]                
        
         (let [v (aget r i0 i1)]
           (cond (pos? v) (inc v)
                 (neg? v) v
                 ;; if zero checking around 
                 true (if (pos-rst->positive-neighs? p r)
                        (inc v)
                        v))))))))
;; flooding 
(defn raster-pos->inc-flood-till-reach [rs pos]
  (loop [rs rs]
    (if (zero? (raster-pos->val rs pos))
      (recur (raster->flood-inc-raster-step rs))
      rs)))
(defn raster-pos0-pos1->flood-raster [rs pos0 pos1]
  (raster-pos->inc-flood-till-reach (raster-is->replace rs pos0 1)
                                    pos1))

(defn rst->mnht-flooding-till-reach [r] 
  
          
          
  
(defn mnht-flood? [p e]
  (





;; ;; CHECKBOARD RASTER 
;; ;; it's not pos it's axis 
;; (defn pos-step->axis-min [p pmin step]
;;   (let [n (int (/ (- p pmin) step))]
;;      (- p (* n step))))
;; (defn pos-step->axis-max [p pmax step]
;;   (let [n (int (/ (- pmax p) step))]
;;     (+ p (* n step))))
;;  ;; getting the axis - so cool 
;; (defn pos-minmax-step->axis-range [p pmin pmax step]
;;   (let [amin (pos-step->axis-min p pmin step)
;;         amax (pos-step->axis-max p pmax step)]
;;     (range amin amax step)))
;; (defn pos-minmax-step->axis-ranges [pos dim step]
;;   (let [[p0 p1] pos
;;         [d0 d1] dim 
;;         p0axis (pos-minmax-step->axis-range p0 0 d0 step)
;;         p1axis (pos-minmax-step->axis-range p1 0 d1 step)]
;;     [p0axis p1axis]))

;; ;; raster - 2d list of positions 
;; (defn pos-dim-step->raster-poss [pos dim step]
;;   (let [[p0as p1as] (pos-minmax-step->axis-ranges pos dim step)]
;;     (loop [[p0 & p0rs] p0as
;;            ret []]
;;       (if p0 
;;         (recur p0rs (conj ret (map vector (repeat p0) p1as)))
;;         ret))))
;; (defn raster-env->raster-typs [rs env]
;;   (loop [[p0s & p0rs] rs
;;          ret []]
;;     (if p0s
;;       (recur p0rs (conj ret (poss-env->typs-under? p0s env)))
;;       ret)))

;; ;; env - we are around - so we are around .. 
;; (defn raster-env->typs [raster env]
;;   (poss-env->typs-under? raster env))
;; (defn env-pos-step->raster-typs [env pos step]
;;   (let [rs (pos-dim-step->raster-poss pos (:dim env) step)
;;         ts (raster-env->raster-typs rs env)]
;;     [rs ts]))

;; ;; flooding
;; ;; it might be - using hashes - because of the shit around :O

;; ;; adding the shit around - that's gonna be around 
;; ;; so that's around 


;; ;; of we go through that can be easier - bu
;; (defn raster-init [d0 d1]
;;   (vec (repeat d0 (vec (repeat d1 0)))))
;; (defn poss->raster-init [poss]
;;   (raster-init (count poss) (count (first poss))))
;; (defn raster-replace [rs i0 i1 e]
;;   (assoc rs i0 (assoc (nth rs i0) i1 e)))
;; (defn raster-is->replace [rs [p0 p1] e]
;;   (assoc rs p0 (assoc (nth rs p0) p1 e)))
;; (defn raster-pos->val [rs [p0 p1]]
;;    (nth (nth rs p0) p1))
;; (defn nths [coll [i0 & ir]]
;;   (if ir 
;;     (nths (nth coll i0) ir)
;;     (nth coll i0)))
;; (defn raster-poss->vals [rs poss]
;;   (map raster-pos->val (repeat rs) poss))
;; (defn raster->dim [rs]
;;   [(count rs) (count (first rs))])

;; ;; doing the flooding 
;; ;; flood patterns 
;; (def neigh-dirs [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])
;; (def neigh-inv-dists [0.7 1 0.7 1 1 0.7 1 0.7])
;; (def neigh-dir-inv-dists (map vector neigh-dirs neigh-inv-dists))
;; ;; (map println neigh-dir-inv-dists)

;; ;; (def neigh-dirs
;; ;;   (doall (for [i0 [-1 0 1]
;; ;;                i1 [-1 0 1]]
;; ;;            [i0 i1])))
;; ;; (def neigh-inv-dists 
;; ;;   (for [v (doall neigh-dirs)]
;; ;;     (/ 1.0 (vec->len v))))


;; ;; ONLY RASTER - you ale the leadel :)
;; (defn is-dim-valid? [[i0 i1] [d0 d1]]
;;   (and (< -1 i0 d0)
;;        (< -1 i1 d1)))
;; (defn is-typ-valid? [is rs]
;;   (not (neg? (nths rs is))))
;; (defn is-valid? [is rs dim]
;;   (if (is-dim-valid? is dim)
;;     (is-typ-valid? is rs)))
 
  
;; ;; getting an is -
;; ;; 1| loop through
;; (defn is->neigh-iss [is]
;;   (map vec+vec neigh-dirs (repeat is)))
;; (defn is->valid-neigh-is [is rs dim]
;;   (filter #(is-valid? % rs dim) (is->neigh-iss is)))
;; (defn is->dim-valid-neigh-is [is dim]
;;   (filter #(is-dim-valid? % dim) (is->neigh-iss is)))

;; ;; ds - distances 
;; (defn iss->highest-dist [iss ds rs dim]
;;   (loop [[is0 & ir] iss
;;          [d0 & dr] ds
;;          ret []]
;;     (if is0 
;;       (if (is-valid? is0 rs dim)
;;         (if (pos? (nths rs is0))
;;           (recur ir dr (conj ret d0))
;;           (recur ir dr (conj ret 0)))
;;         (recur ir dr ret))
;;       (apply max ret))))
;; ;; getting the new inced val
;; (defn is->inced-is [is rs dim]
;;   (let [v (nths rs is)]
;;     (cond (pos? v) (inc v)
;;           (neg? v) v
;;           true (+ v (iss->highest-dist (is->neigh-iss is) neigh-inv-dists rs dim)))))
;; (defn raster->flood-inc-raster-step [rs]
;;   (let [dim (raster->dim rs)
;;         [im0 im1] dim]
;;     (vec (for [i0 (range 0 im0)]
;;            (vec (for [i1 (range 0 im1)]
;;                   (is->inced-is [i0 i1] rs [im0 im1]))))))) 

;; ;; HAVING NORMAL MANHATTEN FLOODING - that's gonna be allright and that's so long 
;; (defn is->mnht-inced-is [is rs dim]
;;   (let [v (nths rs is)]
;;     (cond (neg? v) v
;;           (
;;           (pos? v) (inc v)
          
;;           true (+ v (iss->highest-dist (is->neigh-iss is) neigh-inv-dists rs dim)))))
;; (defn raster->mnht-flood-inc-raster-step [rs]
;;   (let [dim (raster->dim rs)
;;         [im0 im1] dim]
;;     (vec (for [i0 (range 0 im0)]
;;            (vec (for [i1 (range 0 im1)]
;;                   (is->inced-is [i0 i1] rs [im0 im1]))))))) 




          
;; ;; (defn pos->neigh-poss [pos]
;; ;;   (map vec+vec neigh-dirs (repeat pos)))
;; ;; (defn poss->remove-outbound [poss [d0 d1]]
;; ;;   (remove (fn [[p0 p1]] (or (or (< p0 0) (<= d0 p0))
;; ;;                             (or (< p1 0) (<= d1 p1))))
;; ;;           poss))
;; ;; (defn pos->valid-neigh-poss [pos dim]
;; ;;   (poss->remove-outbound (pos->neigh-poss pos) dim)) 
;; ;; (defn pos->valid-neigh-vals [rs pos dim]
;; ;;   (remove neg? (raster-poss->vals rs (pos->valid-neigh-poss pos dim))))
;; ;; ;; (defn raster-neighs-sum [rs pos dim]
;; ;; ;;   (reduce + (raster-poss->vals rs (pos->valid-neigh-poss pos dim))))
;; ;; (defn raster-neighs-sum [rs pos dim]
;; ;;   (reduce + (pos->valid-neigh-vals rs pos dim)))
;; ;; (defn raster-non-neg-neighs-num [rs pos dim]
;; ;;   (count (filter pos? (pos->valid-neigh-vals rs pos dim))))

;; ;; ;; adding the num of neighbours 
;; ;; (defn raster-pos->inc-if-neigh [rs pos dim]
;; ;;   (let [v (raster-pos->val rs pos)]
;; ;;     (if (neg? v)                          ;if not increable 
;; ;;       v
;; ;;       (+ v (raster-non-neg-neighs-num rs pos dim)))))

;; ;; flooding with grace or sort of 
;; ;; (defn raster-inc-flood-step [rs]
;; ;;   (let [[im0 im1] (raster->dim rs)]
;; ;;     (vec (for [i0 (range 0 im0)]
;; ;;            (vec (for [i1 (range 0 im1)]
;; ;;                   (raster-pos->inc-if-neigh rs [i0 i1] [im0 im1])))))))
;; ;; (defn raster-pos->inc-flood-till-reach [rs pos]
;; ;;   (loop [rs rs]
;; ;;     (if (zero? (raster-pos->val rs pos))
;; ;;       (recur (raster-inc-flood-step rs))
;; ;;       rs)))
;; ;; (defn raster-pos0-pos1->flood-raster [rs pos0 pos1]
;; ;;   (raster-pos->inc-flood-till-reach (raster-pos->replace rs pos0 1)
;; ;;                                     pos1))


;; (defn raster-pos->inc-flood-till-reach [rs pos]
;;   (loop [rs rs]
;;     (if (zero? (raster-pos->val rs pos))
;;       (recur (raster->flood-inc-raster-step rs))
;;       rs)))
;; (defn raster-pos0-pos1->flood-raster [rs pos0 pos1]
;;   (raster-pos->inc-flood-till-reach (raster-is->replace rs pos0 1)
;;                                     pos1))


;; ;; WE HAVE TO CHANGE THE STUFF AROUND ... 
;; ;; uptracing - from a flood map 
;; (defn raster-pos->highest-uptrace-neigh-pos [rs pos]
;;   (let [dim (raster->dim rs)
;;         iss (conj (is->dim-valid-neigh-is pos dim) pos)
;;         vs (raster-poss->vals rs iss)]
;;     (nth iss (.indexOf vs (apply max vs)))))
;; (defn raster-pos->uptrace-poss [rs pos]
;;   (loop [p pos
;;          ret [pos]]
;;     (let [np (raster-pos->highest-uptrace-neigh-pos rs p)]
;;       (if (vec=vec? p np)
;;         ret 
;;         (recur np (conj ret np))))))


;; ;; flood and having the uptrace 
;; (defn raster-pos0-pos1->flood-poss [rs pos0 pos1]
;;   (raster-pos->uptrace-poss (raster-pos0-pos1->flood-raster rs pos0 pos1)
;;                             pos1))



;; ;; TODO 
;; ;; if -1 -> then it's not flooding
;; ;; having the  flooding -> than tracing back !!! 
;; ;; 

;; ;; it should have hexagons - ohh fuck that :) 
;; ;; but whatever 

;; ;; TODO 
;; ;; !| FINDING INDEX 
;; ;; finding index - shortest dist
;; (defn poss->all-indexes [poss]
;;   (for [i0 (range (count poss))
;;         i1 (range (count (first poss)))]
;;     [i0 i1]))
;; (defn poss-pos->is-index-of-shortest [poss pos]
;;   (let [is (poss->all-indexes poss)
;;         ds (doall (map #(vec-dist (nths poss %) pos) is))

;;         mind (apply min ds)
;;         mini (.indexOf ds mind)]

;;     (nth is mini)))

;; ;; setting minus1 shitty-shittiness :)
;; (defn poss-raster->minusp-for-obst [rs ts]
;;   (let [is (poss->all-indexes rs)]
;;     (loop [[i & ir] is
;;            rs rs]
;;       (if i
;;         (if (some #(= % :obst) (nths ts i))
;;           (recur ir (raster-is->replace rs i -1))
;;           (recur ir rs))
;;         rs))))

;; ;; having trace .. 
;; (defn env-pos->raster->flood-trace [env pos0 pos1 step]
;;   (let [poss (pos-dim-step->raster-poss pos0 (:dim env) step)

;;         ts (raster-env->raster-typs poss env)
;;         rs (poss->raster-init poss)
;;         rs (poss-raster->minusp-for-obst rs ts)       
;; ;;        i (doall (map println rs))

;;         ;; indexes
;;         ipos0 (poss-pos->is-index-of-shortest poss pos0)
;;         ipos1 (poss-pos->is-index-of-shortest poss pos1)]

;; ;;    (println pos0 pos1)
;; ;;    (println ipos0 ipos1)
;;     (map #(nths poss %) (raster-pos0-pos1->flood-poss rs ipos0 ipos1))))

;; ;; so it's fucking working .. ohh yeah :) 
;; (def r (raster-init 6 6))
;; (raster-pos0-pos1->flood-poss r [1 1] [4 2])
;; ;;(raster-neighs-sum r [4 4] [6 6])

;; (def rs (raster-init 6 6))


;; (def r2 (raster->flood-inc-raster-step (raster->flood-inc-raster-step (raster-is->replace r [2 2] 1))))
;; (raster->flood-inc-raster-step (raster-is->replace r [2 2] 1))
;; (raster-pos->uptrace-poss r2 [0 5])



          
        
;; ;; ohh faszomba :) 

;;   ;; (loop [[r0s & r0rs] rs]
;;   ;;   (if r0s
;;   ;;     (loop [[r1s & r1rs] r0s]
;;   ;;       (if

;;   ;;          (def rs (raster-replace (raster-init 10 10) 2 2 1))
