


;; auxiliary functions 
(defn ras-rpu->obst-poss [ras rpu]
  (let [[d0 d1] (arr2d->dims ras)]
    (for [i0 (range d0)
          i1 (range d1)
          :when (= (int (aget ras i0 i1)) -1)]
      (ipos-rpu->pos [i0 i1] rpu))))
(defn pos-ras->obst-dists [pos ras rpu]
  (let [[d0 d1] (arr2d->dims ras)]
    (for [i0 (range d0)
          i1 (range d1)
          :when (= (int (aget ras i0 i1)) -1)]
      (vec-dist (ipos-rpu->pos [i0 i1] rpu)
                pos))))
(defn pos-obst-poss->obst-dists [pos oposs]
  (for [p oposs] (vec-dist pos p)))
   
(defn pos-ras->min-obst-dist [pos ras rpu]
  (apply min (pos-ras->obst-dists pos ras rpu)))
(defn pos-oposs->min-odist [pos oposs]
  (apply min (pos-obst-poss->obst-dists pos oposs)))

;; (defn poss-ras->obst-dists [poss ras rpu]
;;   (for [p poss] (pos-ras->min-obst-dist p ras rpu))
(defn poss-ras->obst-dists [poss ras rpu]
  (let [oposs (ras-rpu->obst-poss ras rpu)]
    (for [p poss] (pos-oposs->min-odist p oposs))))
         
          

;; the robot going through - so that's how around :) 
;; so that's really under the stuff how that's going on 
(defn pos0-pos1-pos2->deg [p0 p1 p2]
  (let [v0 (vec-vec p1 p0)
        v1 (vec-vec p2 p1)]
    (vec2d-vec2d->deg v0 v1)))
(defn poss->degs [ps0]
  (loop [ps ps0
         ret []]
    (let [[p0 p1 p2 & pr] ps]
      (if p2 
        (recur (rest ps) (conj ret (pos0-pos1-pos2->deg p0 p1 p2)))
        ret))))

(defn pos0-pos1-pos2->deglen [p0 p1 p2] 
  (let [v0 (vec-vec p1 p0)
        v1 (vec-vec p2 p1)]
    [(vec2d-vec2d->deg v0 v1) 
     (+ (vec-len v0) (vec-len v1))]))
(defn poss->deglens [ps0]
    (loop [ps ps0
         ret []]
    (let [[p0 p1 p2 & pr] ps]
      (if p2 
        (recur (rest ps) (conj ret (pos0-pos1-pos2->deglen p0 p1 p2)))
        ret))))

;; the lens of going shitt 
(defn poss->lens [ps0]
  (loop [ps ps0
         ret []]
    (let [[p0 p1 & pr] ps]
      (if p1 
        (recur (rest ps) (conj ret (vec-dist p0 p1)))
        ret))))

;; we will have the easiest cost function 
(defn odist->cost [d u-obst e-obst]
  (if (= d 0) 
    1.0
    (/ 1 (+ 1 (Math/pow (/ d u-obst) e-obst)))))
(defn jerk->cost [deg u-deg e-deg]
  (Math/pow (/ deg u-deg) e-deg))
(defn deglen->cost [deg len u-deg e-deg]
  (* (Math/pow (/ deg u-deg) e-deg) len))
(defn len->cost [len u-len]
  (/ len u-len))


;; params 
(defn traj-params-init [u-len u-deg e-deg u-obst e-obst]
  {:u-len u-len
   :u-deg u-deg
   :e-deg e-deg
   :u-obst u-obst
   :e-obst e-obst})
(defn traj->odst-deg-cost [poss ras rpu ps]
  (let [odsts (poss-ras->obst-dists poss ras rpu)
        pdegs (poss->degs poss)
        plens (poss->lens poss)]

    (reduce +
            (concat (for [p plens] (len->cost p (:u-len ps)))
                    (for [d pdegs] (jerk->cost d (:u-deg ps) (:e-deg ps)))
                    (for [p odsts] (odist->cost p (:u-obst ps) (:e-obst ps)))))))

(defn traj->odst-deglen-cost [poss ras rpu ps]
  (let [odsts (poss-ras->obst-dists poss ras rpu)
        pdeglens (poss->deglens poss)
        plens (poss->lens poss)]

    (println (distinct (map first pdeglens)))
    (reduce +
            (concat (for [p plens] (len->cost p (:u-len ps)))
                    (for [[d l] pdeglens] 
                      (deglen->cost d l (:u-deg ps) (:e-deg ps)))
                    (for [p odsts] (odist->cost p (:u-obst ps) (:e-obst ps)))))    
))


;; 1| generate population
;; 2| generate evolution - that's great 
;;    it could be used - differential evolution - but also the splining putting in
;; we will be around :) 

;; that's really would be more - that's going to the same 
;; the splining 

;; generating difference vector 
;; v0 v1 -> diff   -> put splining on the diff? 
;;     -> randomize on the applied diff - so that where added - its not so much 
;;        -> we will have some here :) 
 

;; so we are around - so that's quite couchy 
;; okay I have something seen - to have more shit around - that's going 






