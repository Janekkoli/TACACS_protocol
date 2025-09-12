# How make "virtual environment" and build this project?

1. `opam switch create .`

2. `opam repository set-url coq-released https://coq.inria.fr/opam/released`
3. `opam update`
4. `opam pin add . -y`
5. `dune build`
6. There should be executable files: `./_build/default/server/server.exe` and `./_build/default/client/client.exe`. They are not windows executable just normal linux ones.
