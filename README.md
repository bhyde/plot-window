Need to plot some data from Common Lisp?  This will solve your problem
by making a pretty plot in your web browser, effectively using that
browser as a display for your chart.  It uses a cute Javascript
plotting widget known as flot.  But it can do much more; you can push
arbitrary content to a webpage, and by using other Javascript widgets
lots of other things are possible; including maps, rich text editors,
syntax highlighting, etc. etc.

First: load and run this:
```common-lisp
> (ql:quickload "plot-window")
> (cl-user:initialize-application)
```

Second: Visit http://localhost:8765/ in a quality web browser. I've only tried chrome.
The resulting page becomes your plot display.  

Third: Load up an example, in this case a plot widget: `(ql:quickload "plot-window-flot")`

```common-lisp
> (pw:plot (loop for i below 50 collect (list i (random 20))))
```

You can clear the window with `(ps:clear-display-window)`, and you can add single elements to the page using `(ps:add-element "<p>Hi there</p>")`

A number of examples are in the example's subdirectory.  Each of these has it's on asd.  You may load them all via `(ql:quickload "plot-window-examples")`.  And once they are all loaded you can run a little demo: `(ql:demo t)`

Magic
-----

The chart is drawn by [flot](http://www.flotcharts.org/), a javascript
library.  The page is rendered via Hunchentoot.  The dynamic updating
is done via websockets (with the help of
[clws](http://www.cliki.net/clws)).  A tangle of javascript glues it
all together, and that's implemented using parenscript.  Various
standard libraries on the javascript and common lisp side are used:
[Hunchentoot](http://weitz.de/hunchentoot/),
[Parenscript](http://common-lisp.net/project/parenscript/), jQuery,
etc.
