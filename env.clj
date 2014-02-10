
;; THE ENV - so we are 


;; CRAETE RAND POS - type objects :) 
(defn rand-pos [[d0 d1]]
  [(rand d0) (rand d1)])
(defn rand-rect-siz 
  ([siz] (rand-rect-siz siz 0.1))
  ([siz min] (vec+vec (vec*scl siz min)
                      (rand-pos (vec-vec siz (vec*scl siz min))))))

;; OBJ RAND INIT - in env 
(defn obj-dim->rand-pos [o dim]
  (assoc o :pos (rand-pos dim)))
(defn obj-siz->rand-siz [o siz]
  (assoc o :siz (rand-rect-siz siz)))

(defn pos-siz-dim->rect-obj [pos siz dim]
  (-> {:pos pos :siz siz}
      (obj-pos-siz->set-corners)
      (obj-dim->set-in-bound dim)))     
(defn dim-siz->rand-rect-obj [dim siz]
  (pos-siz-dim->rect-obj (rand-pos dim) 
                         (rand-rect-siz siz)
                         dim))


;; OBSTACLE - which is a different breed 
(defn rect->obst [r]
  (assoc r :typ :obst))
(defn obst-init-by-corners [ltc rbc]
  (rect->obst (obj-init-by-corners ltc rbc)))
(defn obst-init-by-pos-siz [pos siz]
  (rect->obst (obj-init-by-pos-siz pos siz)))

(defn obst-rand-init [dim siz]
  (rect->obst (dim-siz->rand-rect-obj dim siz)))
(defn obsts-rand-init [n dim siz]
  (repeatedly n #(obst-rand-init dim siz)))





;; PHYS - a something like a world :) 
;; (phys-init [10 10] 5 [5 5])
(defn empty-env-init [dim]
  (env-init dim))
(defn env-add-obsts [e obsts]
  (env-into-objs e obsts))
;; (defn env-add-rand-obsts [e dim n siz]
;;   (into-obsts e (obsts-rand-init n dim siz))) 
;; (defn env-init-with-obsts [dim obsts]
;;   (-> (env-init)
;;       (set-dim dim)
;;       (into-obsts obsts)))

;; (defn init-env-with-obsts [dim n siz]
;;   (env-init-with-obsts dim 
;;   (-> (env-init)
;;       (add-dim dim)
;;       (env-add-rand-obsts dim n siz))))



           

;; testing 
;; (dim-siz->rand-rect-obj [10 10] [2 2])
;; (obsts-rand-init 2 [5 5] [2 2])
