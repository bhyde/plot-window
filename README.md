Need to plot some data from Common Lisp?  This will solve your problem
by making a pretty plot in your web browser, effectively using that
browser as a display for your chart.  It uses a cute Javascript
plotting widget known as flot.  But it can do much more; you can push
arbitrary content to a webpage, and by using other Javascript widgets
lots of other things are possible; including maps, rich text editors,
syntax highlighting, etc. etc.

First: clone this repository (to ~/.quicklisp/local-projects for example),
then having assured that ASDF can find it (say by resetting ASDF's source respository `(asdf:clear-source-registry)`)

This plot-window requires a patched version of cl-interpol (until changes are merged in cl-interpol). The patched cl-interpol can be found at https://github.com/mmaul/cl-interpol.git. 

The plot-window can be loaded and ran as so:
```common-lisp
> (ql:quickload "plot-window")
> (in-package :pw)
> (pw:initialize-application)
```

Second: Visit http://localhost:8765/ in a quality web browser. I've only tried chrome.
The resulting page becomes your REPL's display window.

Third: Load up an example, in this case a plot widget:
```common-lisp
> (ql:quickload "plot-window-flot")`
> (flot-example-1)
> (plot (loop for i below 50 collect (list i (random 20))))
```

You can clear the window with `(clear-display-window)`, and you can add single elements to the page using `(pw:add-element "<p>Hi there</p>")`

A number of examples are in the example's subdirectory.  Each of these has it's on asd.  You may load them all via `(ql:quickload "plot-window-examples")`.  And once they are all loaded you can run a little demo: `(ql:demo t)`

Magic
-----

The chart is drawn by [flot](http://www.flotcharts.org/), a javascript
library.  The page is rendered via -Hunchentoot.  The dynamic updating
is done via websockets (with the help of
[clws](http://www.cliki.net/clws)).  A tangle of javascript glues it
all together, and that's implemented using parenscript.  Various
standard libraries on the javascript and common lisp side are used:
[Hunchentoot](http://weitz.de/hunchentoot/),
[Parenscript](http://common-lisp.net/project/parenscript/), jQuery,
etc.
