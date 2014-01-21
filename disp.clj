
;; displaying shit - that's it 
(import
  'javax.imageio.ImageIO
  'java.io.File

  'java.awt.image.BufferedImage
  'java.awt.Color
  'java.awt.Polygon
  'java.awt.Dimension

  'javax.swing.JFrame
  'javax.swing.JPanel)


;; sending boxes - that's really around 
;; EASIEST - I don't want to suck with this shit - ohh right!! :)
(defn grph->set-color! [g [c0 c1 c2]]
  (. g setColor (new Color c0 c1 c2)))
(defn grph->fill-rect! [g [x y] [w h] col]
  (grph->set-color! g col)
  (. g fillRect x y w h))
(defn grph->fill-oval! [g [x0 y0] [c0 c1] col]
  (grph->set-color! g col)
  (. g fillOval x0 y0 c0 c1))
(defn grph->fill-circle! [g [x0 y0] rad col]
  (grph->set-color! g col)
  (. g fillOval x0 y0 rad rad))
(defn grph->fill-pixel! [g [x0 y0] col]
  (grph->set-color! g col)
  (. g drawLine x0 y0 x0 y0))

(defn grph->draw-line! [g [x0 y0] [x1 y1] col]
  (grph->set-color! g col)
  (. g drawLine x0 y0 x1 y1))


;; creating image 
(defn img-init [[w h]]
  (do (new BufferedImage w h BufferedImage/TYPE_INT_RGB)))
(defn img->graphics [img]
  (do (. img createGraphics)))
(defn img->save-file [img name]
  (ImageIO/write img "png" (new File name)))

;; creating showing - panel 
(defn frame-init->panel [[w h]]
  (let [panel (new JPanel)
        frame (new JFrame)]
    (do 
      (. panel setPreferredSize (new Dimension w h))
    
      (. frame add panel)
      (. frame pack)
      (. frame setVisible true)

      panel)))
(defn panel-img->render-img! [panel img]
  (do (Thread/sleep 80)
      (. (. panel getGraphics) drawImage img 0 0 panel)))
      ;; (. panel revalidate)))


;; so fucking cool!!! :) 
(defn img->show-in-panel! [img]
  (let [p (frame-init->panel [(. img getWidth) (. img getHeight)])]
    (do (panel-img->render-img! p img))))
    

