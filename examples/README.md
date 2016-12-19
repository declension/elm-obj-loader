# Install
**This example requires features from webgl 2.0.0!**
This means installing this package is a bit tricky. I think the easiest way is to clone `elm-community/webgl`, checkout branch `2.0` and use `elm_self_publish.py elm-community/webgl .` to install the correct version.

There is probably a better way...


# Running
Due to a bug in elm reactor, the obj file will be served as an html page instead of plain/text. This screws up the parsing.
Using another file server works, e.g. `python -m http.server`
