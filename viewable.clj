
 
;; adding - shape properties - latter creating for other :) 
(defn set-col [o col]
  (assoc o :col col))
(defn set-shp [o shp]
  (assoc o :shp shp))
(defn set-typ [o typ]
  (assoc o :typ typ))

;; so it's around - I will have a history of shit :) - great :)
;; so the big picture - is that I will have some 

;; we have only rectangles at the moment - so we have ltc - rbc - great 
(defn obj-field->scale-vector [v field scl]
  (assoc v field (doall (map * (get v field) (repeat scl)))))
(defn obj-fields->scale-vector [v fields scl]
  (reduce #(obj-field->scale-vector %1 %2 scl) v fields))
(defn env-obj->scale-corners [o unit-pix]
  (obj-fields->scale-vector o [:ltc :rbc] unit-pix))
(defn env-obj->scale-pos-siz [o unit-pix]
  (obj-fields->scale-vector o [:pos :siz] unit-pix))
(defn env-obj->scale-corners-pos-siz [o unit-pix]
  (-> o 
      (env-obj->scale-corners unit-pix)
      (env-obj->scale-pos-siz unit-pix)))

;; setting object
(defn env-obst->disp-obj [o unit-pix]
  (-> o
      (env-obj->scale-corners-pos-siz unit-pix)
      (assoc :col [200 100 0]
             :shp :rect)))
(defn env-agnt->disp-obj [o unit-pix]
  (-> o
      (env-obj->scale-corners-pos-siz unit-pix)
      (assoc :col [255 20 0]
             :shp :rect)))
(defn env-goal->disp-obj [o unit-pix]
  (-> o
      (env-obj->scale-corners-pos-siz unit-pix)
      (assoc :col [0 255 0]
             :shp :rect)))
(defn env-shrt-wayp->disp-obj [o unit-pix]
  (-> o 
      (env-obj->scale-corners-pos-siz unit-pix)
      (assoc :col [24 167 181]
             :shp :rect)))


(defn env-obj->disp-obj [o unit-pix]
  (cond (= (:typ o) :obst) (env-obst->disp-obj o unit-pix)
        (= (:typ o) :agnt) (env-agnt->disp-obj o unit-pix)
        (= (:typ o) :shrt-wayp) (env-shrt-wayp->disp-obj o unit-pix)
        (= (:typ o) :goal) (env-goal->disp-obj o unit-pix)))

;; rendering
(defn env-objs->disp-objs [os unit-pix]
  (doall (map env-obj->disp-obj os (repeat unit-pix))))
(defn env->disp-objs [e unit-pix]
  (env-objs->disp-objs (:objs e) unit-pix))

(defn env->disp-dim [e unit-pix]
  (vec*scl (:dim e) unit-pix))


;; CREATE IMG 
;; viewable objects - only rects although :O 
(defn img-vobjs->draw! [img vobjs]
  (let [g (img->graphics img)]
    (doseq [o vobjs]
      (grph->fill-rect! g (:ltc o) (:siz o) (:col o)))
    img))      
(defn env->img [e upix]
  (let [vdim (env->disp-dim e upix)
        vobs (env->disp-objs e upix)

        img (img-init vdim)]
    (img-vobjs->draw! img vobs)))

(defn env->show-in-panel [e upix]
  (img->show-in-panel! (env->img e upix)))
    


        