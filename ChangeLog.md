# Brassica changelog

## Unreleased changes

### Behaviour

- Bugfix: nested categories are now matched up correctly between target and replacement
- Bugfix: Brassica no longer freezes with rules where the target is entirely optional
- Wildcard symbols can now be used in the replacement of a rule
- Brassica now applies sound changes to words in parallel, giving a significant speedup on multi-core machines (though not in a webpage)
- Combining diacritics are now grouped with their base characters as multigraphs
- New `extra` directive allows specifying characters which should never be replaced through all category redefinitions

### Code

- `optparse-applicative` lower bound tightened to 0.17.1
- `Brassica.SoundChange.Apply.Internal.applyOnce` now returns a `RuleStatus` value with more detailed information about the rule application, which is now used by `Brassica.SoundChange.Apply.Internal.setupForNextApplication`
- `Wildcard` and `Kleene` no longer have `OneOf 'Target 'Env` constraint
- `OneOf` type family is no longer used and has been removed
- `Target` and `Environment` `LexemeType`s have been unified as `Matched`
- `Brassica.SoundChange.Frontend.Internal.parseTokeniseAndApplyRules` now takes another argument specifying how to map over the parse output,
    allowing it to be run both on a single core and in parallel depending on the provided function
- `Brassica.SoundChange.Tokenise.tokeniseWord` (and related functions) now handle combining diacritics as mentioned above
- `Brassica.SoundChange.Types.Directive` has a new constructor `ExtraGraphemes` for the `extra` directive, with corresponding changes in parsing and expansion
- `Brassica.SoundChange.Category.extend` has been renamed to `extendCategories`, and now requires pattern-matching on a `Categories` directive before use
- `Brassica.SoundChange.Frontend.Internal.parseTokeniseAndApplyRules` no longer implements rule expansion,
   allowing it to take place only once without needing to be repeated for each rule application.

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
