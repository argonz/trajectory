

;; this is the path-raster allocation suffocating 
;; pos+direction 
(defn pdr-init [pos dir]
  {:pos pos :dir dir})

;; clockwise the direction 
(def dirs-clockwise 
  (vec (for [i (range 8)]
         [(Math/sin (* i (/ (* 2 Math/PI) 8.0)))
          (Math/cos (* i (/ (* 2 Math/PI) 8.0)))])))
(def dirs-ints 
  (vec (map (fn [[i0 i1]] [(Math/round i0) (Math/round i1)]) dirs-clockwise)))

;; distances 
(def dists-clockwise 
  (vec (reduce into (repeat 4 [1 (Math/sqrt 2)])))) 

;; flooding - is sending the path - the total cost function around 
;; it's really around 


;; creating a functional raster something :) 
(defn pdr-raster-init [[d0 d1]]
  (for [i0 (range d0)]
    (for [i1 (range d1)]
      ;; adding eight - pdr-init 
      (repeat 8 (pdr-init)))))

;; searching without - obstacle penalty .. that's quite around 
;; how the activity propagates - I think it's just that .. 

;; we are starting from 0 
;; obstacle 
;; 1| checking if the axis is valid?
;; 2| if we are at somwhere - you will go there 

;; this is around - what could be more ... 
;; 

;; the simple cost of going diagonal and so on 



;; we have this connectivity ring 
;; same direction - is 1 
;; 1 less         - 0.8
;; fullturn       - 0.2
;; this might be some kind of :O

;; how to add up - cost function for the 8? :O


