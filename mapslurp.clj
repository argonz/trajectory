

;; but around - we will use something different 
(defn img->rgb-coll [img]
  (doall (for [i0 (range (.getWidth img))]
           (for [i1 (range (.getHeight img))]
             (let [c (new Color (.getRGB img i0 i1))]
               ;; so that getting the resulting out .
               [(.getRed c) (.getGreen c) (.getBlue c)])))))
(defn pth->rgbs [pth]
  (to-array-2d (img->rgb-coll (path->img pth))))
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

;; scl -> 1 in real -> in map

(defn mappth->env [pth scl]
  (let [rgbs (pth->rgbs pth)
        dim [(count rgbs) (count (aget rgbs 0))]]
    {:rgbs rgbs
     :obss (rgbs->obss rgbs)
     :dim dim
     :scl scl}))


;; OUT OF DIM
;; checking of some position - suddenly out of bound - or similar ... 
(defn axis-out-bound? [p d]
  (or (< p 0)
      (< d p)))
(defn pos-out-bound? [pos dim]
  (some identity (map axis-out-bound? pos dim)))


;; list penalties - how it should be 
;; eg.. there could be other ones .. 
;; so long 
(defn pos->ressed-map-int [p scl]
  (map #(Math/round %) (vec*scl p scl)))
(defn env-pos->out-of-bound? [p e]
  (pos-out-bound? (pos->ressed-map-int p (:scl e))))

(defn ipos-env->obs? [[p0 p1] e]
  (pos? (aget (:obss e) p0 p1)))

(defn pos-env->obst? [p e]
  (let [pos (pos->ressed-map-int p (:scl e))]
    ;;(println pos)
    (if (pos-out-bound? pos (:dim e))
      true                              
      (if (ipos-env->obs? pos e)
        true 
        false))))


;; that's around 
(defn env->img [e]
  (rgbs->img (:rgbs e)))                ;this could be - later - for image 
(defn env->show [e]
  (rgbs->show (:rgbs e) 4))


(def m0 (mappth->env (str "/home/files/prog/clj/guarp/data/mappics/" 
                          "mappic80x90_pn8_an6_th330.png")
                     0.1))

;; all right - this is cool 
(pos-env->obst? [-1 500] m0)



;; (defn pth->env [pth]
;;   (pth->rgb-coll pth)






