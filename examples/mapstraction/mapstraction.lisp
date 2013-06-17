(in-package #:plot-window)

(declare-javascript-library -open-layers ()
  :url "http://openlayers.org/api/OpenLayers.js"
  :loaded-p (boundp -open-layers))

(declare-javascript-library mapstraction-top ()
  :url "https://raw.github.com/mapstraction/mxn/master/source/mxn.js"
  :loaded-p (boundp mxn))

(declare-javascript-library mapstraction-core (mapstraction-top)
  :url "https://raw.github.com/mapstraction/mxn/master/source/mxn.core.js"
  :loaded-p (boundp (@ mxn -lat-lon-point)))

(declare-javascript-library mapstraction-openlayers (-open-layers mapstraction-core)
  :url "https://raw.github.com/mapstraction/mxn/master/source/mxn.openlayers.core.js"
  :loaded-p (loop
               for p in (chain mxn util (get-available-providers))
               when (string= "openlayers" p) do (return t)))

(defun mapabstraction-example ()
  (ps-eval-in-client
    (chain ($ :body) (empty))
    (chain ($ :body) (append "<div id='map' height='40em' width='40em'/>"))
    ;((@ dw lg) "hm1")
    (with-javascript-modules (mapstraction-openlayers)
      ; ((@ dw lg) "hm3")
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

