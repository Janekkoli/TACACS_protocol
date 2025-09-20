# How create enviroment and build this project?

1. `opam switch create tacacs_project  ocaml-base-compiler.5.3.0`
2. `eval $(opam env --switch=tacacs_project)`
3. `opam install .`
4. `dune build`
5. There should be executable files: `./_build/default/server/server.exe`, `./_build/default/server2/server2.exe` `./_build/default/client/client.exe`. They are not windows executable just normal linux ones.

# Usage:

## Server:

There are 2 server programs:

`./_build/default/server/server.exe` and `./_build/default/server2/server2.exe`

The first is single-threaded, the second is multi-threaded. Both work automatically and don't require any runtime input. There is a small database and some randomisation used to determine server responses. Each request is logged on standard output.


## Client:

There is one client program:

`./_build/default/client/client.exe`

The client is more interactive. First, you choose between 4 types of connection - 3 of them are from TACACS documentation and 4th one is extra, working like 2nd, but with a superuser request (only working for alice).

After that, you need to enter user credentials. You can check them in the server file or use 2 common ones:

**username:** *eve* **password:** *12345* **- normal user**

**username:** *alice* **password:** *secret* **- admin**

Then you need to enter a line - this argument will be sent to the server, but it will be ignored if it is a valid integer.

Each connection type has its own sequence of requests; you have some control over how many of each will be sent. Some requests will ask you for additional information.

Note that sometimes there will be a response "no response, try again", which is randomised. Sometimes the client will ask you if you want to try again, but for Logout and Slipoff, accordingly to the documentation, it won't, which may look like an error.