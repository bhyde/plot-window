`plot-window` provides Javascript code and content push capability to a web browser using websockets. `plot-window` goes a step further by providing tooling for using and interacting with Javascript and Javascript libraries. Javscript interaction is done usin parenscript providing a holistic interaction between Lisp and the browser. 

 Examples have been provided for working with the following libraries:
 * flot - Plotting
 * d3js - Plotting
 * mapstraction - Openstreet Maps Mapping
 * nice-edit - Rich text editor
 * syntax-highlighter - Syntax highlighting

Getting Started
---------------

First: clone this repository (to ~/.quicklisp/local-projects for example),
then having assured that ASDF can find it (say by resetting ASDF's source respository `(asdf:clear-source-registry)`)

`plot-window` requires a modified version of cl-interpol (until changes are merged in cl-interpol). The patched cl-interpol can be found at https://github.com/mmaul/cl-interpol.git and the modified version can be found on the `use-interpol-reader-outside-of-reader` branch. 
```
git clone https://github.com/mmaul/cl-interpol.git
cd cl-interpol
git checkout use-interpol-reader-outside-of-reader
```

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

Generated Javascript modules are stored in the static directory this location set with the parameter `*where-to-store-js-module-files*`. It is set by default to the ASDF system relative path of `plot-window`.

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
