Need to plot some data from Common Lisp?  This will solve your problem by making a pretty plot
in your web browser, effectively using that browser as a display for your chart.

First: load and run this:
```common-lisp
> (ql:quickload "plot-window")
> (cl-user:initialize-application)
```

Second: Visit http://localhost:8765/ in a quality web browser. I've only tried chrome.
This becomes your plot display.  To start a random example plot is shown.

Third: Plot your data.

```common-lisp
> (pw:plot (loop for i below 50 collect (list i (random 20))))
```

Magic
-----

The chart is drawn by (flot)[http://www.flotcharts.org/], a javascript library.  The page is rendered via Hunchentoot.  The dynamic updating is done via websockets (with the help of [clws](http://www.cliki.net/clws)).  A tangle of javascript glues it all together, and that's implemented using parenscript.  Various standard libraries on the javascript and common lisp side are used: [Hunchentoot](http://weitz.de/hunchentoot/), Parenscript, jQuery, etc.

