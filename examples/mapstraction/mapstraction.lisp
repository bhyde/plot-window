(in-package #:plot-window)

(define-javascript-library -open-layers ()
  "http://openlayers.org/api/OpenLayers.js"
  (boundp -open-layers))

(define-javascript-library mapstraction-top ()
  "https://raw.github.com/mapstraction/mxn/master/source/mxn.js"
  (boundp mxn))

(define-javascript-library mapstraction-core (mapstraction-top)
  "https://raw.github.com/mapstraction/mxn/master/source/mxn.core.js"
  (boundp (@ mxn -lat-lon-point)))

(define-javascript-library mapstraction-openlayers (-open-layers mapstraction-core)
  "https://raw.github.com/mapstraction/mxn/master/source/mxn.openlayers.core.js"
  (loop
     for p in (chain mxn util (get-available-providers))
     when (string= "openlayers" p) do (return t)))


(defun mapabstraction-example ()
  (ps-eval-in-client
    (chain ($ :body) (empty))
    (chain ($ :body) (append "<div id='map' height='40em' width='40em'/>"))
    (lg "hm1")
    (with-js-libraries (mapstraction-openlayers)
      (lg "hm3")
        (with-delay (1000)
          (let ((map (new (chain mxn
                                 (-mapstraction "map" "openlayers"))))
                (latlon (new (chain mxn
                                    (-lat-lon-point 
                                     6.4500S0 3.3833S0
                                     ; -1.2833S0 36.8167S0
                                     )))))
            (chain map
                   (set-center-and-zoom latlon 12)))))))

