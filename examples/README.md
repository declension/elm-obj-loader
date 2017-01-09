# Running
Due to [a bug in elm reactor](https://github.com/elm-lang/elm-reactor/issues/217), the obj file will be served as an html page instead of `plain/text`. This screws up the parsing.

Using another file server works, e.g.
using [elm-live](https://github.com/tomekwi/elm-live) or `python -m http.server`
