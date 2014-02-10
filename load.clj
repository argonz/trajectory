
(load-file "/home/mate/clj/masd/masd.clj")
(load-file "/home/mate/clj/masd/vec.clj")
(load-file "/home/mate/clj/masd/disp.clj")


(load-file "/home/mate/clj/cog/src/mechanics.clj")
;; (load-file "/home/mate/clj/cog/src/env.clj")
;; instead of env -> we have mapslurp - and that's creating something terrible .. 

(load-file "/home/mate/clj/cog/src/viewable.clj")
(load-file "/home/mate/clj/cog/src/raster.clj")
(load-file "/home/mate/clj/cog/src/agent.clj")
(load-file "/home/mate/clj/cog/src/mapslurp.clj")



;; ;; testing 
(defn env0 []
  (let [e (env-init [100 100])
        o0 (obst-init-by-corners [20 20] [80 40])
        o1 (obst-init-by-corners [20 60] [80 80])
        a0 (agnt-init-by-pos [90 90])
        g0 (goal-init-by-pos [16 72])

        e (env-into-objs e [o0 o1 a0 g0])]
    e))
(def e0 (env0))

;; there is a router - now we need a system - like doing something? :O



(defn test0 []
  (let [e (env0)
        upix 4]
    (env->show-in-panel e upix)))  
(test0)

(defn test1 []
  (let [e (env0)
        upix 4
        e (env-add-shortest-wayps e 3)]
    (println (map :pos (env->agnt-goal->shrt-wayps e0 1.7)))
    (env->show-in-panel e upix)))
(test1)

    
;;     vobs))

        
        

    
    
   
    
;; (def dim [100 100])
;; (dim-siz->rand-rect-obj [10 10] [2 2])