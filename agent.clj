

(defn agnt-init-by-pos [pos]
  (-> (obj-init-by-pos-siz pos [1 1])
      (assoc :typ :agnt)))
(defn goal-init-by-pos [pos]
  (-> (obj-init-by-pos-siz pos [1 1])
      (assoc :typ :goal)))
(defn shrt-wayp-init [pos]
    (-> (obj-init-by-pos-siz pos [1 1])
      (assoc :typ :shrt-wayp)))

;; getting - specific tpyes - for the trajectories and all sorts of fun :) 
(defn env->agnt-pos [e]
  (first (env-typ->poss e :agnt)))
(defn env->goal-pos [e]
  (first (env-typ->poss e :goal)))


;; we have the agnt-goal 
(defn env->agnt-goal-poss [e step]
  (env-pos->raster->flood-trace e (env->agnt-pos e) (env->goal-pos e) step))
(defn env->agnt-goal->shrt-wayps [e step]
  (map shrt-wayp-init (env->agnt-goal-poss e step)))

;; env 
(defn env-add-shortest-wayps [e step]
  (env-into-objs e (env->agnt-goal->shrt-wayps e step)))

