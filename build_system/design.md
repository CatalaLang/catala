

clerk test ->
  ① compile required module dependencies, then:
  --backend interpret (default)
    * run inline tests
    * discard outline (legacy) tests !?
    * interpret scopes marked `#[test]`
  --backend BACK
    * implicit --autotest
    * compile to binary using BACK
    * execute the generated binary


clerk build -> (no longer low-level passthrough ?)
  TARGET is
    - .catala_?? -> copy
    - object -> adequate backend
    - exe -> build deps + link
  -n / --dry-run -> generate clerk.ninja but don't run clerk


targets:
  _build/SRCDIR/SRC.catala_??
  _build/SRCDIR/ocaml/SRC.*
  _build/SRCDIR/c/SRC.*
  _build/SRCDIR/java/SRC.*
  _build/SRCDIR/python/SRC.*
  _build/SRCDIR/BACKEND/SRC.exe ?
