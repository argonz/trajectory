

;; obstacle distances .. 
(defn env->obs-iposs [e]
  (let [[d0 d1] (raster->dim (:obss e))]
    (for [i0 (range d0)
          i1 (range d1)
          :when (ipos-env->obs? [i0 i1] e)]
      [i0 i1])))

(defn vec-vecs->dists [v0 vs]
  (for [v vs]
    (vec-dist v0 v)))
(defn ipos->min-dist [i0 is]
  (apply min (map vec-dist (repeat i0) is)))
  
(defn env->obs-dist-rs [e]
  (let [dim (raster->dim (:obss e))
        [d0 d1] dim

        rs (dim->raster dim)
        obs-iposs (env->obs-iposs e)]

    (to-array-2d 
     (for [i0 (range d0)]
       (for [i1 (range d1)]
         (ipos->min-dist [i0 i1] obs-iposs))))))
(defn env->obs-inv-dist-rs [e exp-param]
  (array-2d-map (fn [d] (/ 1.0 (+ 1.0 (Math/pow d exp-param))))
                (env->obs-dist-rs e)))


;; testing 
;; (raster->show (env->obs-dist-rs m0) [0 0 255] 6)
;; (raster->show (env->obs-inv-dist-rs m0 1.68) [0 0 255] 6)




      
        
    
             