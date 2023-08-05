# Brassica architecture

(Note: Brassica used to use a rather complicated architecture involving linking Haskell to C++.
I abandoned this approach since it proved very difficult to maintain on Windows,
  but in the meantime I linked to this document as a reference on how to do it.
Though it is now outdated,
  you can still read that version of the document <https://github.com/bradrn/brassica/blob/v0.1.0/ARCHITECTURE.md>.)

The desktop version of Brassica has two components, which broadly follow a client/server architecture.
The logic and CLI are written in Haskell, while the GUI is written in C++.
The GUI runs the CLI with a `--server` argument:
  this places it into a ‘server’ mode
  where it accepts JSON requests on stdin and outputs JSON responses on stdout.
The requests and responses are undocumented,
  but should be trivial to infer from `./app/Server.hs` and `./gui/brassica-gui/brassicaprocess.cpp`.

The only real subtlety here is synchronisation.
The server uses incremental parsing,
  and outputs a response as soon as a full and valid JSON object is seen.
The response is followed by the control character U+0017 (‘End Transmission Block’).
The GUI reads from the server’s stdout until this character is seen,
  at which point a complete JSON response has been received for it to decode.

The web version, by necessity, takes a different approach.
It is possible to compile the CLI to WASM,
  but difficulties in async IO support make it difficult to use in the browser
  (see e.g. <https://github.com/bjorn3/browser_wasi_shim/issues/14>).
Thus, the web version bypasses the CLI entirely.
Instead, `./gui/brassica-interop-wasm/` contains a library which is compiled to a WASI reactor.
The website in `./gui/brassica-web/` then calls into this reactor.
