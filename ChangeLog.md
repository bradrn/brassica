# Brassica changelog

## Unreleased changes

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
