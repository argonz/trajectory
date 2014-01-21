
(use 'clojure.math.numeric-tower)
(require 'quil.core)

;; MECHANICS - 
;; 1| objects - having pos - typ -
;; 2| list-of-objects -
;; 3| calling list-of-objects 
;; the update - refreshing - remapping and all that 
;; every object have an update function 
;;              me + world -> new-pos 
;; world-mechanics - you have it here - great 
;;                


;; MECHANIC/LOGIC - creating these fields 
(defn vec+vec [p0 p1]
  (map + p0 p1))
(defn vec-vec [p0 p1]
  (map - p0 p1))
(defn vec*scl [p s]
  (map * p (repeat s)))
(defn vec->len [p]
  (Math/pow (reduce + (map #(Math/pow % 2) p)) 0.5))
(defn vec->norm [p]
  (vec*scl p (/ 1.0 (vec->len p) )))
(defn vec-len->vec [p l]
  (vec*scl (vec->norm p) l))

(defn vec<vec [p0 p1 dim]
  (< (nth p0 dim) (nth dim)))
(defn vec>vec [p0 p1 dim]
  (> (nth p0 dim) (nth dim)))


;; OBJECTS 
;; add to objects and so on 
(defn set-pos [o pos]
  (assoc o :pos pos))
(defn set-siz [o siz]
  (assoc o :siz siz))
(defn set-corners [o ltc rbc]
  (assoc o :ltc ltc :rbc rbc))
(defn set-dir [o dir]
  (assoc o :dir dir))
(defn set-spd [o spd]
  (assoc o :spd spd))
(defn set-typ [o typ]
  (assoc o :typ typ))


(defn pos-siz->ltc-rbc [pos siz]
  (let [hsiz (vec*scl siz 0.5)]
    [(vec-vec pos hsiz)
     (vec+vec pos hsiz)]))
(defn ltc-rbc->pos-siz [ltc rbc]
  (let [siz (vec-vec rbc ltc)
        hsiz (vec*scl siz 0.5)]
    [(vec+vec ltc hsiz)
     siz]))

(defn obj-pos-siz->set-corners [o]
  (let [[ltc rbc] (pos-siz->ltc-rbc (:pos o) (:siz o))]
    (assoc o :ltc ltc :rbc rbc)))
(defn obj-ltc-rbc->set-pos-siz [o]
  (let [[pos siz] (ltc-rbc->pos-siz (:ltc o) (:rbc o))]
    (assoc o :siz siz :pos pos)))

(defn objs-pos-siz->set-corners [os]
  (doall (map obj-pos-siz->set-corners os)))
(defn objs-ltc-rbc->set-pos-siz [os]
  (doall (map obj-ltc-rbc->set-pos-siz os)))

(defn obj-init-by-pos-siz [pos siz]
  (-> {:pos pos :siz siz}
      (obj-pos-siz->set-corners)))
(defn obj-init-by-corners [ltc rbc]
  (-> {:ltc ltc :rbc rbc}
      (obj-ltc-rbc->set-pos-siz)))      


 
;; POS CHECKING 
;; detecting - if a corner or so - is under some stuff :)
(defn pos-ltc-rbc-under? [pos ltc rbc]
  (let [[p0 p1] pos
        [l0 l1] ltc
        [r0 r1] rbc]
    ;; (println pos ltc rbc     (and (< l0 p0 r0)
    ;;                                (< l1 p1 r1)))
    (and (< l0 p0 r0)
         (< l1 p1 r1))))
(defn pos-obj-under? [pos r]
  (pos-ltc-rbc-under? pos (:ltc r) (:rbc r)))
(defn pos-objs-under? [pos rs]
  (filter #(pos-obj-under? pos %) rs))
  

;; OUT OF DIM
;; checking of some position - suddenly out of bound - or similar ... 
(defn axis-out-bound? [p d]
  (or (< p 0)
      (< d p)))
(defn axis-set-in-bound [p d]
  (cond (< p 0) 0
        (< d p) d                     ;we allow the top - it's not possible
        true p))
(defn pos-out-bound? [pos dim]
  (some identity (map axis-out-bound? pos dim)))
(defn pos-set-in-bound [pos dim]
  (map axis-set-in-bound pos dim))



;; UPDATING  
;; obj -> new-obj  -   UPDATING THE STATE - and that's it fuckers :)
(defn obj-env->upd [o e]
  (if (:upd-f o)
    ((:upd-f o) o e)
    o))
(defn objs-env->upds [os e]
  (doall (map obj-env->upd os e)))

;; pos - translation 
(defn pos-dir->new-pos [pos dir spd upd-t]
  (vec+vec pos (vec*scl dir (* spd upd-t))))
(defn obj-env->upd-pos [o e]
  (if (:dir o)
    (assoc o :pos (pos-dir->new-pos (:pos o) (:dir o) (:spd o) (:upd-t e)))
    o))
(defn objs-env->upd-pos [os e]
  (doall (map obj-env->upd-pos os (repeat e))))

;; set - in bound 
(defn obj-dim->set-in-bound [o dim]
  (assoc o :pos (pos-set-in-bound (:pos o) dim)))
(defn obj-env->set-in-bound [o e]
  (obj-dim->set-in-bound o (:dim e)))
(defn objs-env->set-in-bound [os e]
  (doall (map obj-env->set-in-bound os (repeat e))))

;; checking for collision 
(defn obj-obj-under? [o0 o1]
  (or (pos-obj-under? (:ltc o0) o1)     ;if one of them under - both of them 
      (pos-obj-under? (:rbc o0) o1)))
(defn obj-objs->objs-under [o os]
  (loop [[o0 & or] os
         ret []]
    (if o0
      (if (obj-obj-under? o o0)
        (recur or (conj ret o0))
        (recur or ret))
      (if (empty? [])
        nil
        (conj ret o)))))      
(defn objs->collide-lists [os]
  (loop [[o0 & or] os
         ret []]
    (if o0
      (let [cl (obj-objs->objs-under o0 or)]
        (if cl
          (recur or (conj ret cl))
          (recur or ret)))
      ret)))

;; checking for anything? 
(defn pos-env->objs-under? [pos env]
  (if (pos-out-bound? pos (:dim env))
    :out 
    (pos-objs-under? pos (:objs env))))
(defn pos-env->typ-under? [pos env]
  (let [unders (pos-env->objs-under? pos env)]
    (if (= unders :out)
      unders
      (if (empty? unders)
        nil
        (map :typ unders)))))
(defn poss-env->typs-under? [poss env]
  (doall (map pos-env->typ-under? poss (repeat env))))
  


;; THE ENV - collection of objs 
;; ENV - list of stuff - 
;;     - having collision detection - maybe
;;     - UPDATE - DIFFERENCE - update functions - and so 
;; display - should deal + random generating functions - and all that aroud 
;;         - we will have that around 
(defn env-init [dim]
  {:dim dim
   :objs []})
(defn env-conj-obj [e o]
  (assoc e :objs (conj (:objs e) o)))
(defn env-into-objs [e os]
  (assoc e :objs (into (:objs e) os)))

(defn env->collide-lists [e]
  (objs->collide-lists (:objs e)))
;; thats great - you now have all that stupid shit around :) 
;; you will just have to make - some around - which would be good 

;; doing the looping - setting all the new states - ooohhh crap monami 
(defn objs-env->full-upd [objs e]
  (-> (:objs e)
      (objs-env->upds e)
      (objs-env->upd-pos e)
      (objs-pos-siz->set-corners)
      (objs-env->set-in-bound e)
      (objs-pos-siz->set-corners)))
(defn env->upd-env [e]
  (assoc e :objs (objs-env->full-upd (:objs e) e)))
(defn env->n-upd-hist [e0 n]
  (loop [es (list e0)
         n n] 
    (if (zero? n)
      es
      (recur (cons (env->upd-env (first es)) es) 
             (dec n)))))

;; (defn env->upd-env [e]
;;   (let [os0 (:objs e)
;;         os1-upd (objs-env->upds os0 e)
;;         os1-mov (objs-env->upd-pos os1-upd e)
;;         os1-bnd (objs-env->set-in-bound os1-mov e)]
;;     ;; should be collision detection here - fuck that :)
;;     (assoc e :objs os-1bnd
        



;; (defn env->n-update-env [e]
;;   )

