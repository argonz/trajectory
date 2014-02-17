



;; scoring - we will need some scoring - and showing shortest paths 
;; they are around - that will be more around 



;; or just simply list of trajectories 
(defn stat-init [pos dir]
  {:pos pos
   :dir dir})

;; scoring functions - that's around - that's all wrong ..
;; s0-s1->val - that's the real scoring around .. that's all around
;; all of them [0 .. 1]  -  and weighted afterward .. 
(defn euclid-dist-f [pos]
  (fn [s0 s1] (let [d0 (vec-dist (:pos s0) pos)
                    d1 (vec-dist (:pos s1) pos)]
                (- d0 d1)))) ;; so is this closing the distance - the gap ..

;; (defn score-funcs 
;; we want to create a simulation - how that could be ..





;; choosing with same energies - ?? - maybe time how much/further they can go.. 
;; that's pretty much around .. that's going to be more - we are around .. 
  

