

;; creating positions - for the some manner 
;; so this is how this is how - we will have 
;; 

;; generating star - raster setting - which is so shitty all around :)
;; that's gonna around :)
;; I'm dead if it's the thing :) - we will see - more have to :)


;; CHECKBOARD RASTER 
;; it's not pos it's axis 
(defn pos-step->axis-min [p pmin step]
  (let [n (int (/ (- p pmin) step))]
     (- p (* n step))))
(defn pos-step->axis-max [p pmax step]
  (let [n (int (/ (- pmax p) step))]
    (+ p (* n step))))
;; getting the axis - so cool 
(defn pos-minmax-step->axis-range [p pmin pmax step]
  (let [amin (pos-step->axis-min p pmin step)
        amax (pos-step->axis-max p pmax step)]
    (range amin amax step)))
(defn pos-minmax-step->axis-ranges [pos dim step]
  (let [[p0 p1] pos
        [d0 d1] dim 
        p0axis (pos-minmax-step->axis-range p0 0 d0 step)
        p1axis (pos-minmax-step->axis-range p1 0 d1 step)]
    [p0axis p1axis]))

;; raster - 2d list of positions 
(defn pos-dim-step->raster-poss [pos dim step]
  (let [[p0as p1as] (pos-minmax-step->axis-ranges pos dim step)]
    (loop [[p0 & p0rs] p0as
           ret []]
      (if p0 
        (recur p0rs (conj ret (map vector (repeat p0) p1as)))
        ret))))
(defn raster-env->raster-typs [rs env]
  (loop [[p0s & p0rs] rs
         ret []]
    (if p0s
      (recur p0rs (conj ret (poss-env->typs-under? p0s env)))
      ret)))

;; env - we are around - so we are around .. 
(defn raster-env->typs [raster env]
  (poss-env->typs-under? raster env))
(defn env-pos-step->raster-typs [env pos step]
  (let [rs (pos-dim-step->raster-poss pos (:dim env) step)
        ts (raster-env->raster-typs rs env)]
    [rs ts]))

;; flooding
;; it might be - using hashes - because of the shit around :O

;; of we go through that can be easier - bu
(defn raster-init [d0 d1]
  (vec (repeat d0 (vec (repeat d1 0)))))
(defn raster->num-raster [rs]
  (raster-init (count rs) (count (first rs))))
(defn raster-replace [rs i0 i1 e]
  (assoc rs i0 (assoc (nth rs i0) i1 e)))
(defn raster-pos->val [rs [p0 p1]]
  (nth (nth rs p0) p1))
(defn raster-poss->val [rs poss]
  (map raster-pos->val (repeat rs) poss))
(defn raster->dim [rs]
  [(count rs) (count (first rs))])

;; doing the flooding 
(defn neigh-dirs []
  (for [i0 [-1 0 1]
        i1 [-1 0 1]]
    [i0 i1]))
(defn pos->neigh-poss [pos]
  (map vec+vec (neigh-dirs) (repeat pos)))
(defn poss->remove-outbound [poss [d0 d1]]
  (remove (fn [[p0 p1]] (or (or (< p0 0) (<= d0 p0))
                            (or (< p1 0) (<= d1 p1))))
          poss))
(defn pos->valid-neigh-poss [pos dim]
  (poss->remove-outbound (pos->neigh-poss pos) dim))
(defn raster-neighs-sum [rs pos dim]
  (reduce + (raster-poss->vals rs (pos->valid-neigh-poss pos dim))))

                              
;; THE SUBVEC VARIATION
;; (defn raster-axis-neigh-range [i imax]
;;   (filter #(< -1 % imax) (range (- i 1) (+ i 2))))
;; (defn raster-axis-neigh-subvec [rs is]
;;   (subvec rs (first is) (+ (last is) 1)))
;; (defn raster-neighs [rs i0 i1 im0 im1]
;;   (let [r0s (raster-axis-neigh-range i0 im0)
;;         r1s (raster-axis-neigh-range i1 im1)]
;;     (reduce into (map #(raster-axis-neigh-subvec (nth rs %) r1s) r0s))))
;; (defn raster-neighs-sum [rs i0 i1 im0 im1]
;;   (reduce + (raster-neighs rs i0 i1 im0 im1)))

;; normal flooding 
(defn raster-pos->if-neigh-inc [rs pos dim]
  (if (pos? (raster-neighs-sum rs pos dim))
    (inc (raster-pos->val rs pos))
    0))
(defn raster-inc-flood-step [rs]
  (let [[im0 im1] (raster->dim rs)]
    (vec (for [i0 (range 0 im0)]
           (vec (for [i1 (range 0 im1)]
                  (raster-pos->if-neigh-inc rs [i0 i1] [im0 im1])))))))
(defn raster-pos->inc-flood-till-reach [rs pos]
  (loop [rs rs]
    (if (zero? (raster-pos->val rs pos))
      (recur (raster-inc-flood-step rs))
      rs)))

;; flooding with obstacles - great
(defn raster-pos->if-obst-neigh-inc [rs vs pos dim]
  (if (not (= (raster-pos->val vs pos) :obst))
    (raster-pos->if-neigh-inc rs pos dim)))
(defn raster-obst-inc-flood-step [rs vs]
  (let [[im0 im1] (raster->dim rs)]
    (vec (for [i0 (range 0 im0)]
           (vec (for [i1 (range 0 im1)]
                  (raster-pos->if-obst-neigh-inc rs [i0 i1] [im0 im1])))))))
(defn raster-pos->obst-inc-flood-till-reach [rs vs pos]
  (loop [rs rs]
    (if (zero? (raster-pos->val rs pos))
      (recur (raster-obst-inc-flood-step rs vs))
      rs)))


;; uptracing - from a flood map 
(defn raster-pos->highest-neigh-pos [rs pos]
  (let [dim (raster->dim rs)
        ps (pos->valid-neigh-poss pos dim) 
        vs (raster-poss->vals rs ps)]
    (nth ps (.indexOf vs (apply max vs)))))    
(defn raster-pos->uptrace-pos [rs pos]
  (loop [p pos
         ret [pos]]
    (let [np (raster-pos->highest-neigh-pos rs p)]
      (if (vec=vec? p np)
        ret 
        (recur np (conj ret np))))))
  

(defn env-pos->raster->flood-trace [env pos step]
  (let [[rs ts] (env-pos-step->raster-typs env pos step)
        rs 
        ;; you should check and set others too 
    (raster-pos->uptrace-pos
    
(defn raster-pos->uptrace-from-pos [rs pos]
  (
         


;; so it's fucking working .. ohh yeah :) 
(def r (raster-replace (raster-init 6 6) 2 2 1))
(raster-neighs r 0 0 6 6)
(raster-neighs-sum r 4 4 6 6)
(def r2 (raster-inc-flood-step (raster-inc-flood-step r)))
(raster-pos->uptrace-pos r2 [0 5 ])
          
        
;; ohh faszomba :) 

  (loop [[r0s & r0rs] rs]
    (if r0s
      (loop [[r1s & r1rs] r0s]
        (if

           (def rs (raster-replace (raster-init 10 10) 2 2 1))






;; THE TESTING PHASE - GOD HELP US :))
;; (pos-env-step->raster-typs [11 11] e0 20)

;; I SHOULD DO - THE FLOODIGN 

;; so we are under - we are going more :)
;; testing the shit 
        
  
;; (defn values-step->values 
;; (defn step->poss [p0 p1 step dim]
;;   (loop [p p0
;;          ret []]
;;     (if (< p p1)
;;       (recur (vec+vec p step) (conj ret p))
;;       ret)
         

;; (defn phys->posrast [phys step]
;;   (dim-step->rast-poss (:dim phys) step))
;; (defn posrast-phys->valrast [pss phys val-func]
;;   (loop [[ps0 & psr] pss
;;          rs []]
;;     (if ps0
;;       ;; creating a row
;;       (recur psr (conj rs (doall (map val-func ps0 (repeat phys)))))
;;       ;; returning the rast
;;       rs)))
;; (defn phys->rast-posvals [phys step val-func]
;;   (let [pss (phys->posrast phys step)
;;         vss (posrast-phys->valrast pss phys val-func)]
;;     (doall (map (fn [ps vs] 
;;                   (doall (map (fn [p v] {:pos p :val v})
;;                               ps vs)))
;;                 pss 
;;                 vss))))
;; (defn phys->posvals-rast-assocs [phys step val-func] 
;;   (reduce into (phys->rast-posvals phys step val-func)))

;; (defn pos-phys->obst-val [pos phys]
;;   (if (pos-rects-under? pos (:obsts phys))
;;     1 
;;     0))
;; (defn phys->obst-pos-assocrast [p step]
;;   (phys->posvals-rast-assocs p step pos-phys->obst-val))




;; ;; RADIANT RASTER 

 
;; ;; STAR POSITIONS
;; ;; adding multiple steps to the positions 
;; (defn poss-diff-vec [p0 step n]
;;   (loop [p p0
;;          n n
;;          r []]
;;     (let [np (pos+pos p step)]
;;       (if (zero? n)
;;         (conj r np)
;;         (recur np (dec n) (conj r np))))))

;; (defn pos-vec-n->poss [p0 vec n]
;;   (loop [p p0
;;          n n
;;          r []]
;;     (let [np (pos+pos p vec)]
;;       (if (zero? n)
;;         (conj r np)
;;         (recur np (dec n) (conj r np))))))  
;; (defn pos-dir-n->poss [p0 dir len n]
;;   (

;; (defn poss-vec [p0 dir dst n]
;;   (vec-len->vec dir 
  
;; ;; (defn star-steps [step-dis]
;; ;;   (map (fn [r] [(float (Math/sin r)) (float (Math/cos r))])
;; ;;        (map * (range 0 1 1/8) (repeat Math/PI))))

;; ;;   (let [ds [-1 -0.5 0 0.5 1]]
;; ;;     (for [p0 ds
;; ;;           p1 ds]
;; ;;       (pos->norm [p0 p1]))

 
;; ;; creating a raster - ohh that's great - we will see more :) 
;; ;; RAST - create a 2d array :O - creating to nums 
;; (defn dim-step->rast-row-poss [d1 step p0]
;;   (loop [vs []
;;          p1 (/ step 2.0)]
;;     (if (< p1 d1)
;;       (recur (conj vs [p0 p1]) (+ p1 step))
;;       vs)))
;; (defn dim-step->rast-poss [[d0 d1] step]
;;   (loop [vs []
;;          p0 (/ step 2.0)]
;;     (if (< p0 d0)
;;       (recur (conj vs (dim-step->rast-row-poss d1 step p0)) (+ p0 step))
;;       vs)))
