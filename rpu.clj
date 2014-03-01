

;; axis checking 
(defn axis-out-bound? [p d]
  (or (< p 0) (<= d p)))
(defn ipos-out-bound? [pos dim]
  (some identity (map axis-out-bound? pos dim)))
(defn iposs->in-bounds [is dim]
  (remove #(ipos-out-bound? % dim) is))

;; raster position
;; 0 -> [0 .. rpu]
;; 2 -> [2rpu .. 3rpu] .. ranges 
(defn pos-rpu->ipos [p rpu]
  (map #(int %) (vec*scl p (/ 1.0 rpu))))
(defn ipos-rpu->pos-range [i rpu]
  [(* i rpu) (* (inc i) rpu)])
(defn ipos-rpu->pos [i rpu]
  (vec*scl (vec+vec i [0.5 0.5]) rpu))
(defn dim-rpu->idim [dim rpu]
  (map #(int (Math/ceil (/ % rpu))) dim))    

(defn poss-rpu->iposs [ps rpu]
  (map pos-rpu->ipos ps (repeat rpu)))
(defn iposs-rpu->poss [is rpu]
  (map ipos-rpu->pos is (repeat rpu)))
                             

;; distance 
(defn ipos-ipos-rpu->dist [i0 i1 rpu]
  (vec-dist (ipos-rpu->pos i0 rpu) (ipos-rpu->pos i1 rpu)))


;; getting average-tile-value 
(defn pos-rpu->corner-iposs [p rpu]
  (let [[f0 f1] (vec*scl p (/ 1.0 rpu))
        [j0 j1] (map int [f0 f1])
        
        i0s (if (< (- f0 j0) 0.5)
              [(dec j0) j0]
              [j0 (inc j0)])
        i1s (if (< (- f1 j1) 0.5)
              [(dec j1) j1]
              [j1 (inc j1)])]

    (for [i0 i0s
          i1 i1s]
      [i0 i1])))
(defn pos-rpu->corner-iposs-with-dist [pos rpu]
  (let [iposs (pos-rpu->corner-iposs pos rpu)
        diss (for [ip iposs]
               (vec-dist pos (ipos-rpu->pos ip rpu)))]
    [iposs diss]))
(defn pos-ras-rpu->wavg-val [pos rs rpu]
  (let [[iposs diss] (pos-rpu->corner-iposs-with-dist pos rpu)
        vs (for [[i0 i1] iposs] (aget rs i0 i1))
        ds (for [d diss] (/ 1.0 (+ d 0.000001)))]
    (wavg vs ds)))
;; HAVING THE WEIGHTED AVERAGE HERE - that's would be :) 

;; SCALING 
(defn ipos-rpu->pos-ltc-rbc [i rpu]
  (let [rhalf (/ rpu 2.004)
        pos (ipos-rpu->pos i rpu)]
    [(vec-vec pos [rhalf rhalf])
     (vec+vec pos [rhalf rhalf])]))
(defn pos-ltc-rbc->iposs [ltc rbc rpu]
  (let [[i00 i01] (pos-rpu->ipos ltc rpu)
        [i10 i11] (pos-rpu->ipos rbc rpu)]
    (for [i0 (range i00 (inc i10))
          i1 (range i01 (inc i11))]
      [i0 i1])))  

(defn ipos0-rpu0->rpu1-iposs-tile [ipos0 rpu0 rpu1]
  (let [[ltc rbc] (ipos-rpu->pos-ltc-rbc ipos0 rpu0)]
    (pos-ltc-rbc->iposs ltc rbc rpu1)))

;; SCALING OBSS etc.. - doing it wrong 
;; there is something -> create tiles but that's it 
;; something - map to something        
(defn ipos0-rpu0->rpu1-ipos [i0 rpu0 rpu1]
  (pos-rpu->ipos (ipos-rpu->pos i0 rpu0) rpu1))
(defn dim0-rpu0->rpu1-dim [dim0 rpu0 rpu1]
  (let [rhalf0 (/ rpu0 2)
        rhalf1 (/ rpu1 2)

        pdim0 (vec*scl (vec+vec dim0 [rhalf0 rhalf0]) rpu0)
        pdim1 (map #(int (Math/ceil %)) (vec*scl pdim0 (/ 1.0 rpu1)))]
    pdim1))


