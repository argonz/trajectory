




;; okay around - so 

;; finding trajectories -> and I can shove it .. motherfucker :) 
;; so that's really something 
    
;; so pushing and searching for trajectories .. how this could be ??? 
;; ohh god how this could be - this will be there 


;; comptuing the new direction 
;; (defn pos-wayps->dir [pos ws rng exp-dist] ;e-dist is the exponent
;;   (let [ws (wayps->wayps-in-range ws rng)]
;;     (if (empty? ws)
;;       (vec-vec (first 
;; )))))

;; selecting waypoints - in nlen radious or so 
;; selecting weighted waypoints - not active any more - that's pretty much more :) 
;; we will see more about this - I'm pretty sure 
(defn wayps->wayps-in-range [ws rng]
  (loop [[w0 & wr] ws
         dst 0
         ret []]

    (if w0
      (let [[w1 & w0r] wr]
        (if w1 

          (let [d (vec-dist w0 w1)]
            (if (< (+ d dst) rng)
              (recur wr (+ d dst) (conj ret w0))
              ret))
          ret))
      ret)))
(wayps->wayps-in-range [[1 1] [2 2] [3 3] [4 4] [5 5]] 1)
(wayps->wayps-in-range [[1 1] [2 2] [3 3] [4 4] [5 5]] 5)
;; so we are really under? :)
(defn pos-poss->wavg-dir [pos0 ps exp-dst]
  (let [dirs (map vec-vec->dir (repeat pos0) ps)
        ds (map vec-vec->dist ps (repeat pos0))
        ws (map #(/ 1 (+ 1 (Math/pow % exp-dst))) ds)]
    (vecs->wavg dirs ws)))
;; that's stupid around we will have more - we should move more 
;; so just follow the first trajectory point - with some turning radii
;; actually that's it - no turning radii? 


;; if we decide to evolve trajectories - you will end up several stupid mistakes
;; at the end - it's always around 
(defn rand-diff [n scl]
  (take n (repeatedly #(* scl (- (rand) 1)))))
(defn traj->window-averagize [ts0 n]
  (loop [ts ts0
         ret []] 
    (take n ts) 
  (take ts))
;; should have the monte-carlo too that's pretty much what's going on .. 
;; so we are under around - we will see more here and then :) 
;; this is the way it's over and under ..




;; 2| creating a trajectory .. so we are around :) 
;; if we have a head model - ohh god - that's all kind of troubles - that's really 
(defn wayps->following-traj [ws0 len]
  (loop [pos (first ws0)
         ws (rest ws0)
         ts []]
    ;; creating a mixture of whatever - the old one - and the new one - 
    ;; and that's more around - that's how it is all the classes we should be
    ;; I mean we can see more around .. :O - we will have that 
    (let [[w0 & wr] ws]
      (if w0 
        (if (< (vec-vec->dist pos w0) len)
          (recur w0 wr (conj ts (vec pos)))
          (recur (vec+vec pos (vec*scl (vec-vec->dir pos w0) len)) 
                 ws 
                 (conj ts (vec pos))))
        ts))))

;; so this is stupid - none the less ..
;; but if I can sit through there - without doing something else - that's pretty 
(defn wayps->following-momentum-traj [ws0 len w-per-len]
  (loop [pos (first ws0)
         ws (rest ws0)
         dir (vec-vec->dir (first ws0) (first (rest ws0)))
         ts []]
  
    (let [[w0 & wr] ws]
      (if w0 
        (let [ndir (vecs->wavg [dir (vec-vec->dir pos w0)]
                               [1.0 (* len w-per-len)])
              npos (vec*scl (vec-vec->dir pos w0) len)
              nts (conj ts npos)]

          (if (< (vec-vec->dist pos w0) len)
            (recur npos wr ndir nts)
            (recur npos ws ndir nts)))
        ts))))


;; normal following is better - I suppose now ... - so that's the ugly truth of things .. that's really going on - that's quite much around :) - we will see :) 
(wayps->following-traj [[0 0] [3 4] [3 7] [8 8]] 0.4)
;; (wayps->following-momentum-traj [[0 0] [3 4] [3 7] [8 8]] 0.4 20) 

;; so this is all aorund - what we have here
;; so that's gonna be allright - that's how this is going around 
;; 

;; so long around -  we will see more - and that's going to be more about :) 
;; we will see more how this can be done - that's pretty much more :)
;; what we will actually doing around - that's how this should be more :)
;; belittleing yourself - that's stupid 
;;                      -> you create hidden surprise - but thats stupid ..
;;     so long so thats quite much around - that's going to be more around ... 
;; so that we are around - shitty fuckface - we are done and I don't want to

;; knowing the m0 - shitty stuff from here and around - we are very good 
(defn test0 []
  (let [rpu0 100
        ras0 (env->scaled-obs-raster m0 rpu0)
        ;; len u-len u-deg e-deg u-obst e-obst
        ps (traj-params-init 1 4 3.2 10 2.2)
    
        pos0 [4000 7900] 
        pos1 [3000 2900]
        poss (wayps->following-traj [pos0 pos1] 32)
        iposs (poss-rpu->iposs poss rpu0)]
 ;; [[0 0] [3 4] [3 7] [8 8]] 0.4)
 ;;        poss (wayps->following-traj [[0 0] [3 4] [3 7] [8 8]] 0.4)
   (raster-and-trajs->show ras0 [255 0 0] [iposs [0 255 0]] 8)
    (traj->odst-deglen-cost poss ras0 rpu0 ps)))

;; how the point can be modulated and so on - that's gonna be around 
;; we will see more how this is going to be more 



;; what could be my evolutionary system :) 
;; 1| creating a population of trajectories ... 
;;    where and and new point is there - hooray :) - lenize .. 

;; THERE ARE CASES - when it's not a number - that's pretty around and much :)
;; we will see 


;; you need some kind of prominences that are going around - and we will be more for that, hooray for that kind of disclosure 

;; so you would need some kind of modulation - how this can be aroun 


(test0)

        


