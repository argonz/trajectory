

;; creating positions - for the some manner 
;; so this is how this is how - we will have 
;;

;; having typed 

;; array 
;; aget aset alength aclone amap areduce 
;; (set! *warn-on-reflection* true)    
;; (set! *warn-on-reflection* false)    
(defn arr2d->arr2d-double ^"[[D" [arr]
  (into-array (Class/forName "[D")
              (map double-array arr)))
(defn arr2d->arr2d-int ^"[[I" [arr]
  (into-array (Class/forName "[I")
              (map int-array arr)))
(defn arr2d-double [d0 d1]
  (make-array Double/TYPE d0 d1))
(defn arr2d-int [d0 d1]
  (make-array Integer/TYPE d0 d1))

;; aux
(defn arr2d-aclone [a2d]
  (into-array (map aclone a2d)))
(defn arr2d-print [a2d]
  (let [[d0 d1] (arr2d->dims a2d)]
    (doseq [i0 (range d0)]
      (println (map #(format "%.2f" %) (vec (aget a2d i0)))))))

(defn arr2d-ipos->val [a2d [i0 i1]]
  (aget a2d i0 i1))
(defn arr2d-ipos->set! [a2d [i0 i1] v]
  (aset a2d i0 i1 v)
  a2d)


;; generating star - raster setting - which is so shitty all around :)
;; that's gonna around :)
;; I'm dead if it's the thing :) - we will see - more have to :)
(defn raster->dim [rs]
  [(count rs) (count (first rs))])
(defn dim->raster [[d0 d1]]
  (to-array-2d (repeat d0 (repeat d1 0))))
(defn ipos-rs->val [[i0 i1] r]
  (aget r i0 i1))
(defn iposs-rs->vals [is r]
  (for [i is] (ipos-rs->val i r)))
(defn rs-ipos-set! [rs [i0 i1] val]
  (aset rs i0 i1 val)
  rs)

(defn ipos-dim->valid? [[i0 i1] [d0 d1]]
  (and (<= 0 i0) (< i0 d0)
       (<= 0 i1) (< i1 d1)))
(defn ipos-rs->valid? [[i0 i1] rs]
  (let [[d0 d1] (raster->dim rs)]
    (and (<= 0 i0) (< i0 d0)
         (<= 0 i1) (< i1 d1))))
(defn iposs-rs->valid-iposs [is rs]
  (filter #(ipos-rs->valid? % rs) is))


;; adding up rasters 
(defn arr2d->multiply-arr2d [a2d m]
  (let [[d0 d1] (arr2d->dims a2d)
        ret (arr2d-aclone a2d)]
    (doseq [i0 (range d0)
            i1 (range d1)]
      (aset ret i0 i1 (* (aget a2d i0 i1) m)))
    ret))
(defn arr2d-pow! [a2d e]
  (let [[d0 d1] (arr2d->dims a2d)]
    (doseq [i0 (range d0)
            i1 (range d1)]
      (let [v (Math/pow (aget a2d i0 i1) e)]
        (aset a2d i0 i1 v)))
    a2d))
(defn arr2d-pow-pos! [a2d e]
  (let [[d0 d1] (arr2d->dims a2d)]
    (doseq [i0 (range d0)
            i1 (range d1)]
      (let [v (aget a2d i0 i1)]
        (if (pos? v)
          (aset a2d i0 i1 (Math/pow v e)))))
    a2d))
(defn arr2d-aset-plus! [arr2d i0 i1 v]
  (let [nv (+ (aget arr2d i0 i1) v)]
    (aset arr2d i0 i1 nv)))
(defn arr2d-add-arr2d! 
  ;; positioned - corner 
  ([a2d0 a2d1 ipos]
     (let [dim0 (arr2d->dims a2d0)
           dim1 (arr2d->dims a2d1)
           [d0 d1] dim1]
       (doseq [i0 (range d0)
               i1 (range d1)]
         (let [ip (vec+vec ipos [i0 i1])
               [ip0 ip1] ip]
           (if (ipos-dim->valid? ip dim0)
             (arr2d-aset-plus! a2d0 (first ip) (second ip) (aget a2d1 i0 i1)))))
       a2d0))
  ;; without pos 
  ([a2d0 a2d1] 
     (let [[d0 d1] (arr2d->dims a2d0)]
       (doseq [i0 (range d0)
               i1 (range d1)]
         (arr2d-aset-plus! a2d0 i0 i1 (aget a2d1 i0 i1))))
     a2d0))
(defn arr2d-add-arr2d-centered [a2d0 a2d1 ipos]
  (let [dim1 (arr2d->dims a2d1)
        hipos (map #(int (/ % 2.0)) dim1)]
    (arr2d-add-arr2d! a2d0 a2d1 (vec-vec ipos hipos))))

;; (print-arr2d (normal-pdf2d-unittop-raster 2 2 2))
;; (print-arr2d (arr2d-add-arr2d (arr2d-double 8 8)
;;                               (normal-pdf2d-unittop-raster 2 2 2)
;;                               [-1 -1]))
;; (print-arr2d (arr2d-add-arr2d-centered (arr2d-double 8 8)
;;                                        (normal-pdf2d-unittop-raster 2 2 2)
;;                                        [3 3]))


;; we want to create a pdf function 
(defn normal-pdf [x mean std]
  (* (/ 1.0 (* std (Math/sqrt (* 2 Math/PI))))
     (Math/pow Math/E (- (/ (Math/pow (- x mean) 2)
                            (* 2 (Math/pow std 2)))))))


;; we are creating a normal - looking-like - pdf - and will see.. 
;; std-cutoff - for how long we create the raster .. 
;; rpu - real per unit 
;; this should be more around - that's how it is - how this could  be 
;; [0 0] - should be the center of it - and that's around - how its going to be 
(defn normal-pdf2d-raster [std n-std rpu]
  (let [d (Math/ceil (/ (* n-std std) rpu))
        [d0 d1] (repeat 2 (+ 1 (* 2 d)))
        rpu-half (vec->len [(/ rpu 2) (/ rpu 2)])
        ret (arr2d-double d0 d1)]
    
    ;; do sequence 
    (doseq [i0 (range (- d) (inc d))
            i1 (range (- d) (inc d))]    
      (let [v (* (vec->len [i0 i1]) rpu)]
        ;; computing the density 
        (aset ret (+ i0 d) (+ i1 d) (normal-pdf v 0 std))))
    ;; return 
    ret))
;; (normal-pdf2d-raster 10 1 5)
(defn normal-pdf2d-unittop-raster [std n-std rpu]
  (let [p2d (normal-pdf2d-raster std n-std rpu)
        nrm (/ 1.0 (normal-pdf 0 0 std))
        [d0 d1] (arr2d->dims p2d)]

    (doseq [i0 (range d0)
            i1 (range d1)]
      (let [v (* nrm (aget p2d i0 i1))]
        (aset p2d i0 i1 v)))

    p2d))
;; (raster->show (normal-pdf2d-unittop-raster 10 2 2) [255 0 0] 8)
;;      (normal-pdf (- (vec->len (ipos-rpu->pos [i0 i1] rpu)) mean (/ rpu 2.0)) mean std))))

;; go through - the activator - the inhibitor -
;; 0 - nothing
;; pos - activator
;; neg - inhibitor
;; adding up the raster - which is imp
(defn state->act-inh-sum-state [s0 act-arr2d inh-arr2d]
;;  (arr2d-print s0)
  (let [s1 (arr2d-aclone s0)
        [d0 d1] (arr2d->dims s0)]

    ;; activities
    (doseq [i0 (range d0)
            i1 (range d1)]    
      (let [v (aget s0 i0 i1)]
        (cond (pos? v) (arr2d-add-arr2d-centered 
                        s1 
                        (arr2d->multiply-arr2d act-arr2d v)
                        [i0 i1])
              (neg? v) (arr2d-add-arr2d-centered 
                        s1 
                        (arr2d->multiply-arr2d inh-arr2d v)
                        [i0 i1])
              (= 0) nil)))
    ;; enforcing obs
    (doseq [i0 (range d0)
            i1 (range d1)]    
      (if (= (aget s0 i0 i1) -1)
        (aset s1 i0 i1 -1)))
    ;; returning 
    s1))
;; (defn state->act-inh-till-reach-ipos [s0 ipos act-arr2d inh-arr2d]
;;   (let [[i0 i1] ipos]
;;     (loop [s s0]
;;       (println  (aget s i0 i1))
;;       (if (pos? (aget s i0 i1))
;;         s
;;         (recur (state->act-inh-sum-state s act-arr2d inh-arr2d))))))
(defn state-ipos-ipos->act-inh-till-reach [s0 ipos0 ipos1 act-arr2d inh-arr2d]
  (loop [s (arr2d-ipos->set! s0 ipos1 1.0)]
    (println (arr2d-ipos->val s ipos1))
    (if (pos? (arr2d-ipos->val s ipos0))
      s
      (recur (state->act-inh-sum-state s act-arr2d inh-arr2d)))))


(defn gauss-test [rpu]
  (let [pos0 (pos-rpu->ipos [4000 8900] rpu)
        pos1 (pos-rpu->ipos [3000 2900] rpu)
        ras (env->scaled-obs-raster m0 rpu)
        ret (state-pos-pos->act-inh-till-reach 
             ras pos0 pos1 
             (normal-pdf2d-unittop-raster 300 2.44 rpu)
             (normal-pdf2d-unittop-raster 140 2.44 rpu))]
    
    (raster->show (arr2d-pow-pos! ret 0.5)  [255 0 0] 8)))



  

(defn ipos->mnht-neigh-iposs [[p0 p1]]
  [[(inc p0) p1]
   [(dec p0) p1]
   [p0 (inc p1)]
   [p0 (dec p1)]])
(defn ipos-rst->mnht-neighs [p r] 
  (remove nil?
          (for [[i0 i1] (ipos->mnht-neigh-iposs p)]
            (try (aget r i0 i1)
                 (catch Exception e nil)))))
(defn ipos-rst->mnht-pos-neighs? [p rs dim]
  (loop [[i & ir] (ipos->mnht-neigh-iposs p)]
    (if i
      (if (ipos-dim->valid? i dim)
        (let [[i0 i1] i]
          (if (pos? (aget rs i0 i1))
            true
            (recur ir)))
        (recur ir))
      false)))


;; MNHT FLOODING 
;; (defn rst->mnht-flood-step [rs0]
;;   (let [[d0 d1] (array-2d->dims rs0)
;;         rs1 (make-array Integer/TYPE d0 d1)]

;;     (doseq [i0 (range d0)
;;             i1 (range d1)]                
        
;;          (let [v0 (aget rs0 i0 i1)
;;                v1 (cond (pos? v0) (inc v0)
;;                         (neg? v0) v0
;;                         true (if (ipos-rst->positive-neighs? [i0 i1] rs0)
;;                                (inc v0)
;;                                v0))]
;;            (aset rs1 i0 i1 (int v1))))
;;     rs1))
(defn rst->mnht-flood-step [rs0]
  (let [dim (arr2d->dims rs0)
        [d0 d1] dim]

    (to-array-2d 
    (for [i0 (range d0)]
      (for [i1 (range d1)]              

         (let [v0 (aget rs0 i0 i1)]
           (cond (pos? v0) (inc v0)
                 (neg? v0) v0
                 true (if (ipos-rst->mnht-pos-neighs? [i0 i1] rs0 dim)
                        (inc v0)
                        v0))))))))

(defn rst->mnht-flood-till-reach [rs ipos]
  (loop [rs rs]
    (if (zero? (ipos-rs->val ipos rs))
      (recur (rst->mnht-flood-step rs))
      rs)))
(defn rst-pos0-pos1->mnht-flood [rs pos0 pos1]
  (rst->mnht-flood-till-reach (rs-ipos-set! rs pos1 1)
                              pos0))

(defn rst->nonminus-minmax-inverse [rs]
  (let [min-val (apply min (filter nonneg? (arr2d->vals rs)))
        max-val (apply max (filter nonneg? (arr2d->vals rs)))
        [d0 d1] (arr2d->dims rs)]

    (to-array-2d
     (for [i0 (range d0)]
       (for [i1 (range d1)]
         (let [v (aget rs i0 i1)]
           (if (nonneg? v) 
             (- max-val (- v min-val))
             v)))))))
    




;; UPTRACING - FROM A FLOOD MAP 
(defn raster-pos->highest-uptrace-neigh-pos [rs pos]
  (let [dim (raster->dim rs)
        iss (conj (iposs-rs->valid-iposs pos rs) pos)
        vs (iposs-rs->vals iss rs)]
    (nth iss (.indexOf vs (apply max vs)))))
(defn raster-pos->uptrace-poss [rs pos]
  (loop [p pos
         ret [pos]]
    (let [np (raster-pos->highest-uptrace-neigh-pos rs p)]
      (if (vec=vec? p np)
        ret 
        (recur np (conj ret np))))))

;; flood and having the uptrace 
;; (defn raster-pos0-pos1->flood-poss [rs pos0 pos1]
;;   (raster-pos->uptrace-poss (raster-pos0-pos1->flood-raster rs pos0 pos1)
;;                             pos1))



;; viewing raster 
(defn raster->max [rs]
  (let [[d0 d1] (raster->dim rs)]
    (apply max (for [i0 (range d0)
                     i1 (range d1)]
                 (aget rs i0 i1)))))
(defn raster->rgbs [rs rgb-max]
  (let [[d0 d1] (arr2d->dims rs)
        vmax (raster->max rs)
        rgb-unit (vec*scl rgb-max (/ 1.0 vmax))]

    (to-array-2d 
     (for [i0 (range d0)]
       (for [i1 (range d1)]
         (let [v (aget rs i0 i1)]
           (cond (pos? v) (map int (vec*scl rgb-unit v))
                 (neg? v) [0 0 0]
                 true [255 255 255])))))))
(defn raster->show 
  ([rs rgb-max scl] (rgbs->show (raster->rgbs rs rgb-max) scl))
  ([rs rgb-max] (raster->show rs rgb-max 1)))

;; trying an example here 
(rgbs->show (raster->rgbs (rst-pos0-pos1->mnht-flood (dim->raster [22 22]) [7 7] [19 16]) [255 0 0]) 12) 
;; THIS IS COOL 








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
