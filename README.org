* Introduction
Need to plot some data from Common Lisp?

To load and run this first do...
```common-lisp
> (ql:quickload "plot-window")
> (cl-user:initialize-application)
```
Visit http://localhost:8765/ in a quality web browser, I've only tried chrome.
This becomes your plot display, a random example plot is shown to start.

The function md:plot will revise what's showing, i.e.

```common-lisp
> (pw:plot (loop for i below 50 collect (list i (random 20))))
```

