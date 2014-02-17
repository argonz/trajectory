

;; raster - looking over - selecting several of which is good 
(defn ipos->8neigh-dirs [[p0 p1]]
  (for [i0 (range -1 2)
        i1 (range -1 2)
        :when (not (and (= i0 0) (= i1 0)))]
    [i0 i1]))
;; (defn ipos-dirs->iposs [ipos dirs]
;;   (map vec+vec (repeat ipos) dirs))
(defn ipos->8neigh-iposs [ipos dim]
  (filter #(ipos-dim->valid? % dim)
          (map vec+vec (repeat ipos) (ipos->8neigh-dirs ipos))))


;; gradient looker :) 
;; robot - facing to some direction 
;; direction facet - 
;; should be rotate - these directions 
;; gathher gradient in those points 
;; choose the biggest :) - and that's all .. 

;; I don't need that good - at the moment .. I just nne
;; (defn rob-state [pos dir steps]
;;   (pos 
;; THIS WILL BE GOOD :D 
;; ROTATION - DIRECTION - DEGREE - AND SO :D


;; 8 direction simple robot .. 
;; last-dir - last state? 
(defn dirs-8dirs [len]
  (for [i (range 8)]
    (let [r (* Math/PI 2 i (/ 1.0 8.0))]
      [(* (Math/cos r) len)
       (* (Math/sin r) len)])))
(dirs-8dirs 4)
(defn selector-8dirs [pos len ras rpu]
  (let [dirs (dirs-8dirs len)
        poss (for [d dirs]
               (vec+vec pos d))
        vs (for [p poss]
             (pos-ras-rpu->wavg-val p ras rpu))]
    
    (nth poss (.indexOf vs (apply max vs)))))
(defn selector-8dirs-while [pos0 pos1 len ras rpu]
  (loop [p pos0
         ret []]
    (if (< (vec-dist p pos1) len)
      (conj ret p)
      (recur (selector-8dirs p len ras rpu) (conj ret p)))))
      
             

        
        
        



;; robot - doing head directions and so ... - ohh great ... 
;;       -> should be done there - and looking 

;; for the manhatten - there should be more
;; -> there should be the inverse - so that's pretty much around 

;; selectors 
;; poss x cost-maps -> which to move 
;; costs - is a big all kind of stuff 
;; no combined - that would be awful?? - might be - don't know .. 
;; we will see more about - them that's quite much around .. 


(/ 1 (+ 1 (Math/pow 3.8 1.2)))
;; selector function
;; ipos -> which ipos to take output 
(defn selector0-f [mnht-rs odist-rs] 
  (let [dim (array-2d->dims odist-rs)]

    ;; this is getting the position - and it will be later like that .
    (fn [ipos] 
      (let [;; js (ipos->8neigh-dirs ipos)
            ;; is (ipos-dirs->iposs ipos dim)
            is (ipos->8neigh-iposs ipos dim)

            ms (iposs-rs->vals is mnht-rs) ;the manhatten distance to goal
            ds (iposs-rs->vals is odist-rs) ;the distance from obstacles 
            
            ums (vec->unitize ms)

            ;; combined scores
            cs (map (fn [u d] (if (< d 2) 
                                -1      ;automatic rejection 
                                (+ (- (* 0.8 u))
                                   (- (* 1.9 ;adding distance somehow
                                         (/ 1 (+ 1 (Math/pow d 1.2))))))))
                    ums
                    ds)]
        (println (map vector is cs ums ds))

        ;; returning the index of the direction 
        (nth is (.indexOf cs (apply max cs)))))))
                                

;; DIRECTION WILL BE BETTER :O
;; (defn pos-selector->next-pos [pos sf]
;;   (let [dir (sf pos)]
;;     (println dir)
;;     (vec+vec pos dir)))
(defn pos-selector->next-pos [pos sf]
  (sf pos))
(defn simulating-selector [pos sf n]
  (loop [pos pos
         n n
         traj [pos]]
    (println "step: " pos traj)
    (if (zero? n)
      traj
      (let [npos (pos-selector->next-pos pos sf)]
        (recur npos (dec n) (conj traj npos))))))
(defn simulating-selector-until-reach [pos0 pos1 sf n]
  (loop [pos pos0
         n n
         traj [pos0]]

    (println "step " n "th: " pos traj)
    (if (or (vec=vec? pos pos1)
            (zero? n))
      traj
      (let [npos (pos-selector->next-pos pos sf)]
        (recur npos (dec n) (conj traj npos))))))
             
            
;; (defn env->  