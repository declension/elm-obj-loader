# Install
**This example requires features from webgl 2.0.0!**
This means installing this package is a bit tricky.   
I think the easiest way is to clone `elm-community/webgl`,   
checkout branch `2.0` and use [elm_self_publish.py](https://github.com/NoRedInk/elm-ops-tooling) to install the correct version.   
`elm_self_publish.py elm-community/webgl .`

There is probably a better way...


# Running
Due to [a bug in elm reactor](https://github.com/elm-lang/elm-reactor/issues/217), the obj file will be served as an html page instead of `plain/text`. This screws up the parsing.

Using another file server works, e.g.
using [elm-live](https://github.com/tomekwi/elm-live) or `python -m http.server`
