(in-package #:plot-window)

(define-javascript-library -open-layers ()
  "http://openlayers.org/api/OpenLayers.js")

(define-javascript-library mapstraction-top ()
  "https://raw.github.com/mapstraction/mxn/master/source/mxn.js")

(define-javascript-library mapstraction-core (mapstraction-top)
  "https://raw.github.com/mapstraction/mxn/master/source/mxn.core.js")

(define-javascript-library mapstraction-openlayers (-open-layers mapstraction-core)
  "https://raw.github.com/mapstraction/mxn/master/source/mxn.openlayers.core.js")

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

