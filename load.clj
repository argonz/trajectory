


(load-file "/home/mate/clj/cog/src/mechanics.clj")
(load-file "/home/mate/clj/cog/src/env.clj")
(load-file "/home/mate/clj/cog/src/disp.clj")
(load-file "/home/mate/clj/cog/src/viewable.clj")
(load-file "/home/mate/clj/cog/src/agent.clj")
(load-file "/home/mate/clj/cog/src/raster.clj")




;; ;; testing 
(defn env0 []
  (let [e (env-init [100 100])
        o0 (obst-init-by-corners[20 20] [40 80])
        o1 (obst-init-by-corners [60 20] [80 80])
        a0 (agnt-init-by-pos [30 90])
        g0 (goal-init-by-pos [72 16])

        e (env-into-objs e [o0 o1 a0 g0])]
    e))
(def e0 (env0))
(pos-env-step->raster-typs [90 50] e0 12)


(defn test0 []
  (let [e (env0)
        upix 4]
;;    (env->img e upix)))
    (env->show-in-panel e upix)))
    
(test0)
    
;;     vobs))

        
        

    
    
   
    
;; (def dim [100 100])
;; (dim-siz->rand-rect-obj [10 10] [2 2])