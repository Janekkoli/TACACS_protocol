# How make "virtual environment" and build this project?

1. `opam switch create .`

2. `opam repository set-url coq-released https://coq.inria.fr/opam/released`
3. `opam update`
4. `opam pin add . -y`
5. Now its for some reason important to have tacacs_extracted.ml file in theories/ and .ml file has to match .mli file or .mli file shouldnt exist
6. `dune build`
7. There should be executable files: `./_build/default/server/server.exe` and `./_build/default/client/client.exe`. They are not windows executable just normal linux ones.
