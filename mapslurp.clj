
(require 'incanter.core)
(require 'incanter.stats)
(incanter.stats/sample-normal 1)

;; using the rgbs ...
(defn rgbs->obss [rgbs]
  (to-array-2d
   (for [i0 (range (count rgbs))]
     (for [i1 (range (count (aget rgbs 1)))]
       (if (= (aget rgbs i0 i1) [255 255 255])
         0
         1)))))

        
;; that's pretty much around 
;; (def rgbs (pth->rgb-coll (str "/home/files/prog/clj/guarp/data/mappics/" 
;;                               "mappic80x90_pn8_an6_th330.png")))
;; (aget rgbs 69 69) 


;; we will have a new - special kind of environment - holo ere.. 
;; whenever a position is asked -> is go through the scaling step .. 
;;                                 and that's around ... 

;; real-per-unit -> real-in 1 map 
(defn mappth->env [pth rpu]             ;real-per-unit 
  (let [rgbs (pth->rgbs pth)
        dim [(count rgbs) (count (aget rgbs 0))]]
    {:rgbs rgbs
     :obss (rgbs->obss rgbs)
     :dim dim
     :rpu rpu}))


;; OUT OF DIM
;; checking of some position - suddenly out of bound - or similar ... 
(defn axis-out-bound? [p d]
  (or (< p 0) (<= d p)))
(defn ipos-out-bound? [pos dim]
  (some identity (map axis-out-bound? pos dim)))
(defn iposs->in-bounds [is dim]
  (remove #(ipos-out-bound? % dim) is))

;; list penalties - how it should be 
;; eg.. there could be other ones .. 
;; so long 
;; POSITION - center of tile !!! 
;; [4 7] 100rpu -> [450 750] in real!! 
(defn pos-rpu->ipos [p rpu]
;;  (map #(Math/round %) (vec*scl p (/ 1.0 rpu))))
;;  MINDIG INT lesz -> mert 0.5 -re mappel real :O
  (map #(int %) (vec*scl p (/ 1.0 rpu))))
(defn ipos-rpu->pos [i rpu]
  (vec*scl (vec+vec i [0.5 0.5]) rpu))


(defn pos->ipos [p e]
  (pos-rpu->ipos p (:rpu e)))
(defn ipos->pos [i e]
  (ipos-rpu->pos i (:rpu e)))
(defn env-pos->out-of-bound? [p e]
  (ipos-out-bound? (pos->ipos p e)))


;; assumption - end of map -> out of bounds (wall everywhere..)
(defn ipos-env->obs? [[p0 p1] e]
  (pos? (aget (:obss e) p0 p1)))
(defn pos-env->obs? [p e]
  (let [pos (pos->ipos p e)]
    (if (ipos-out-bound? pos (:dim e))
      true                              
      (if (ipos-env->obs? pos e)
        true 
        false))))

(defn iposs-env->obs? [ps e]
  (some identity (map ipos-env->obs? ps (repeat e))))
(defn poss-env->obs? [ps e]
  (some identity (map pos-env->obs? ps (repeat e))))


;; getting average-tile-value 
(defn pos-ras-rpu->corner-iposs [p rs rpu]
  (let [[f0 f1] (vec*scl p (/ 1.0 rpu))
        [j0 j1] (map int [f0 f1])
        
        i0s (if (< (- f0 j0) 0.5)
              (range (dec j0) j0)
              (range j0 (inc j0)))
        i1s (if (< (- f1 j1) 0.5)
              (range (dec j1) j1)
              (range j1 (inc j1)))]

    (for [i0 i0s
          i1 i1s]
      [i0 i1])))
(defn pos-ras-rpu->corner-iposs-with-dist [pos rs rpu]
  (let [iposs (pos-ras-rpu->corner-iposs pos rs rpu)
        diss (for [ip iposs]
               (vec-dist pos (ipos-rpu->pos ip rpu)))]
    [iposs diss]))
(defn pos-ras-rpu->wavg-val [pos rs rpu]
  (let [[iposs diss] (pos-ras-rpu->corner-iposs pos rs rpu)
        vs (for [[i0 i1] iposs] (aget rs i0 i1))
        ds (for [d diss] (/ 1.0 (+ d 0.00000001)))]
    (wavg vs diss)))
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


(defn env->obs-iposs [e]
  (let [[d0 d1] (arr2d->dims (:obss e))]
    (for [i0 (range d0)
          i1 (range d1)
          :when 
          (ipos-env->obs? [i0 i1] e)]
      [i0 i1])))
(defn env-rpu1->obs-iposs1-tiles [e rpu1]
  (reduce concat 
          (map #(ipos0-rpu0->rpu1-iposs-tile % (:rpu e) rpu1)
               (env->obs-iposs e))))



;; raster closeness - having other values - that's quite much there 
;; the weighted average of the value.. 
(defn env-raster->set-minus-obst [e rs]
  (let [[d0 d1] (arr2d->dims rs)]
    (doseq [i0 (range d0)
            i1 (range d1)]
      (if (ipos-env->obs? [i0 i1] e)
        (aset rs i0 i1 -1)))
    rs))
(defn env->obst-minus-raster [e]
  (env-raster->set-minus-obst e (env->raster e) ))
(defn env->flood-raster [e ipos0 ipos1]
  (rst-pos0-pos1->mnht-flood (env->obst-minus-raster e) ipos0 ipos1))



;; SCALING THE RASTER 
(defn env->raster [e]
  (apply arr2d-double (arr2d->dims (:obss e))))

;; scale integer results - like 0.25 0.5 2 3
(defn env->scaled-obs-raster [e rpu1] 
  (let [rpu0 (:rpu e)
        dim0 (:dim e)
        ras0 (:obss e)

        dim1 (dim0-rpu0->rpu1-dim (:dim e) rpu0 rpu1)
        [d0 d1] dim1 
        ras1 (arr2d-double d0 d1)]      ;for some reasons it starts with zero 

    ;; setting zeros 
    (doseq [i0 (range d0)
            i1 (range d1)]
      (aset ras1 i0 i1 0.0))
    ;; setting obstacles
    (doseq [[i0 i1] (env-rpu1->obs-iposs1-tiles e rpu1)]
      (aset ras1 i0 i1 -1.0))
    
    ;; returning 
    ras1))            



;; we have a map here 
;; where the starting-ending positions are 
;; [40 89]  [30 29]
;; (view-map-pos0-pos1 m0 [40 89] [30 29])
(def m0 (mappth->env (str "/home/files/prog/clj/guarp/data/mappics/" 
                          "mappic80x90_pn8_an6_th330.png")
                     100))
;; sometimes doesn't work with complete numbers 
;; (raster->show (env->scaled-obs-raster m0 51) [255 0 0] 2)
;; (raster->show (env->scaled-obs-raster m0 100) [255 0 0] 8)
;; (raster->show (env->scaled-obs-raster m0 180) [255 0 0] 8)
;; (raster->show (env->scaled-obs-raster m0 200) [255 0 0] 8)
;; (raster->show (env->scaled-obs-raster m0 300) [255 0 0] 8)
;; (raster->show (env->scaled-obs-raster m0 340) [255 0 0] 8)
;; (raster->show (env->scaled-obs-raster m0 400) [255 0 0] 8)
(defn gauss-test [rpu]
  (let [pos0 (pos-rpu->ipos [4000 8900] rpu)
        pos1 (pos-rpu->ipos [3000 2900] rpu)
        ras (env->scaled-obs-raster m0 rpu)
        ret (state-pos-pos->act-inh-till-reach 
             ras pos0 pos1 
             (normal-pdf2d-unittop-raster 340 2.14 rpu)
             (normal-pdf2d-unittop-raster 140 2.14 rpu))]

    (raster->show (arr2d-pow-pos! ret 0.5)  [255 0 0] 8)
    ret))
(def r0 (gauss-test 144))
(def r1 (gauss-test 244))

(selector-8dirs-traj [4000 7900] [3000 2900] 100 r1 244)


;;    (raster->show ret [255 0 0] 8)))

;; should be find some around 

(arr2d-print (normal-pdf2d-unittop-raster 300 2.44 300))
(arr2d-print (normal-pdf2d-unittop-raster 300 2.44 100))
(arr2d-print (normal-pdf2d-unittop-raster 210 1.8 300))
;; (gauss-test 280)
;; (gauss-test 340)




;; (defn env->costrs [e]
;;   {:mnht-flood (
;; (def f0 (env->flood-raster m0 [40 89] [30 29]))
;; (def fi0 (rst->nonminus-minmax-inverse f0))

;; (def di0 (env->obs-inv-dist-rs m0 1.68))
;; (def dn0 (env->obs-dist-rs m0))


;; (env->flood-raster m0 [40 89] [30 29])
;; (rgbs->show (raster->rgbs (env->flood-raster m0 [40 89] [30 29]) [1 0 0])
;;             4)

;; ;; creating the inverse - so we are around .. 
;; (raster->show (rst->nonminus-minmax-inverse f0) [0 255 0] 4)
;; (raster->show f0 [0 255 0] 4)
;; (raster->show f0 [0 255 0] 4)


;; (simulating-selector [30 29] (selector0-f fi0 dn0) 10)



;; ;; that's around 
;; (defn env->img [e]
;;   (rgbs->img (:rgbs e)))                ;this could be - later - for image  
;; (defn env->show [e]
;;   (rgbs->show (:rgbs e) 4))



;; ;; ;; all right - this is cool 
;; ;; (pos-env->obst? [-1 500] m0)

;; ;; ;; we will have something around - with this - that's quite much about 
;; ;; (rgbs->show (:rgbs m0) 4)
;; ;; (last (rgbs->vobjs (:rgbs m0) 1))

;; ;; (vobjs->show (rgbs->vobjs (:rgbs m0) 4))
;; ;; (vobjs->max-rbc-dim (rgbs->vobjs (:rgbs m0) 1))


(defn view-map-pos0-pos1 [m pos0 pos1]
  (let [scl 4
        p0 (rgb-point->scaled-vrect pos0 scl [0 255 0])
        p1 (rgb-point->scaled-vrect pos1 scl [255 0 0])]
    (vobjs->show (conj (rgbs->vobjs (:rgbs m) scl) p0 p1))))

;; ;; ;; that's one valid goal - should be others around 
;; ;; ;; slurping the maps .. we will see .. just now see the 
(view-map-pos0-pos1 m0 [40 89] [30 29])


;; (def tr0 (simulating-selector-until-reach [40 89] [30 29] (selector0-f fi0 dn0) 100))

;; (vobjs->show (concat (rgbs->vobjs (:rgbs m0) 4)
;;                      (map (fn [p] (rgb-point->scaled-vrect p 4 [0 255 0])) tr0)))









