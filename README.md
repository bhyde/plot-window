Introduction
============

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

