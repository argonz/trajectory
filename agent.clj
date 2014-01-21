

(defn agnt-init-by-pos [pos]
  (-> (obj-init-by-pos-siz pos [1 1])
      (assoc :typ :agnt)))
(defn goal-init-by-pos [pos]
  (-> (obj-init-by-pos-siz pos [1 1])
      (assoc :typ :goal)))



  
