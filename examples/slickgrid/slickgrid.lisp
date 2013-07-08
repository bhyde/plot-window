(in-package #:plot-window)



(declare-javascript-library jquery-event-drag (jquery)
  :url "http://mleibman.github.io/SlickGrid/lib/jquery.event.drag-2.2.js"
  :loaded-p (boundp (@ j-query fn drag)))

(declare-javascript-library slick-core (jquery-event-drag)
  :url "http://mleibman.github.io/SlickGrid/slick.core.js"
  :loaded-p (boundp -slick))

(declare-javascript-library slick-grid (slick-core)
  :url "http://mleibman.github.io/SlickGrid/slick.grid.js"
  :loaded-p (boundp (@ -slick -grid)))

(defun slickgrid-example-1 ()
  (ps-eval-in-client
    (with-css ("http://mleibman.github.io/SlickGrid/slick.grid.css") ; note a
      (with-javascript-modules (slick-grid)
        (let* ((new-element ($ "<div/>" 
                               (create :style
                                       "padding:4px;margin:3px;border:1px black solid;background: whitesmoke;width:300;height:150;")))
               (data (loop for i from 0 below 400 collect (create
                                                         :a i
                                                         :b (random 10)
                                                         :c (random 10)
                                                         :d (random 10))))
               (columns (list (create :id "a" :name "Ape" :field :a)
                              (create :id "b" :name "Bob" :field :b :sortable true)
                              (create :id "c" :name "Cat" :field :c)
                              (create :id "d" :name "Dog" :field :d)))
               (options (create 'enable-cell-navigation true
                                'enable-column-reorder false)))
          (flet ((bang () (chain -slick (-grid new-element data columns options))))
            (chain dw (insert-element new-element bang)))
          )))))

;; note a with-css is currently defined in syntax-highlter example :(
