# Brassica changelog

## v0.2.0

- Allow lexeme sequences in categories using `{…}` syntax
- Allow backreferences to occur in the environment
- Allow user to choose separator used between multiple results (previously a space)
- Internal refactor: category expansion is now separate from parsing
- Add `--version` command-line option
- Store `MultiZipper` data in a `Vector` rather than a linked list (for performance)
- Bugfix: subtraction now removes all subtracted graphemes
- Store paradigm builder output in a tree data structure, allowing a more compact output format
- Documented abstract features in paradigm builder (previously present but undocumented)

## v0.1.1

- Rewrote executables with a client/server architecture for better Windows support.
  The library remains unchanged.

## v0.1.0

- Add new syntax with `#` in lexicon to create word boundaries which can be manipulated by sound changes
- Web interface greatly improved using WebAssembly
- Allow synchronising scroll positions in GUI between input and output textboxes
- Add timeout to desktop GUI to abort long-running computations
- Allow category backreferencing with `@n` before category
- Allow forcing nondeterminism with `@?` before category
- Add ‘input→output’ format for output words
- Change default output format for MDF input to wordlist in CLI
- Add ‘or environments’ with syntax ‘target / replacement / env1 / env2 / env3 / …’.
  Former rule exceptions with similar syntax have been changed to syntax ‘target / replacement / env // exception’.
- Improve documentation
