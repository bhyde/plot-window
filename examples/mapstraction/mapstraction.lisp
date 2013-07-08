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
    (flet ((map-lat-lon (lat lon)
             (let ((new-element (append ($ "<div/>" (create :id "map" :height "400px" :width "600px"
                                                            :borde "1px")))))
               (chain dw (insert-element new-element))
               (with-javascript-modules (mapstraction-openlayers)
                 (with-delay (10)
                   (let ((map (new (chain mxn
                                          (-mapstraction (aref new-element 0) "openlayers"))))
                         (latlon (new (chain mxn (-lat-lon-point lat lon)))))
                     (chain map (set-center-and-zoom latlon 12))))))))
      (map-lat-lon
       ;; 6.4500S0 3.3833S0 ;; Lagos
       -1.2833S0 36.8167S0 ;; Nairobi
       ))))

(defun map-point (lat lon)
  (setf lat (coerce lat 'single-float))
  (setf lon (coerce lon 'single-float))
  (ps-eval-in-client*
   `(let ((new-element ($ "<div/>" (create :id "map" :height "400px" :width "600px"
                                                   :border "1px")))
          (pt (new (chain mxn (-lat-lon-point ,lat ,lon)))))
      (flet ((fill-in-map ()
               (with-javascript-modules (mapstraction-openlayers)
                 (with-delay (10)
                   (let ((map (new (chain mxn
                                          (-mapstraction (aref new-element 0) "openlayers")))))
                     (chain map (set-center-and-zoom pt 12)))))))
        (chain dw (insert-element new-element (lambda () (fill-in-map))))))))

(defparameter *cities* 
  '(("Lagos"6.4500S0 3.3833S0)
    ("Nairobi" -1.2833S0 36.8167)
    ("Boston, MA"        +42.35670 -71.05690)
    ("London,England"    +51.50000  -0.11670)
    ("Los_Angeles,CA"    +34.05420  -118.24100)
    ("Tokyo,Japan"       +35.68330  +139.73330)
    ("Kyoto,Japan"       +35.00000  +135.76670)
    ("Chicago, IL"       +41.87440  -87.63940)
    ("Shanghai,China"    +31.23300  +121.45000)
    ("Mexico_City,Mexico" +19.4667  -99.14990)
    ("Sao_Paulo,Brazil"  -23.56660  -46.63320)
    ("Bogota, Colombia"  +4.63330   -74.09990)
    ("Bombay, India"     +18.96670  +72.83330)))

(defun map-random-city ()
  (let ((c (alexandria:random-elt *cities*)))
    (print c)
    (map-point (second c) (third c))))
