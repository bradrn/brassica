# Brassica changelog

## Unreleased

### Behaviour

- Bugfix: etymologies are now added in all situations when processing an MDF file
- Bugfix: desktop version correctly warns about unsaved changes when opening a file
- New `report` directive allows for reporting intermediate results in input→output mode
- Whitespace is now preserved when displaying sound change output on desktop and web
- Improved handling of `~` after grapheme:
  - It is now allowed inside a category block
  - It is now disallowed in category names and in `extra` declarations (where it would be meaningless)
- Category intersection now produces a category with graphemes in the same order as the last category mentioned,
  rather than the first
- New support for phonetic features:
  - Features can be written as maximal `$name#ident(values)` after another lexeme
  - Categories named `+Feature` and `-Feature` get special interpretation with intersections and subtractions
  - New category syntax: `&` before category name forces union interpretation
      even for category names beginning with `+`/`-`

### Code

- CMakeLists for desktop GUI now track Haskell binary dependency correctly
- Changes resulting from addition of `report`:
  - New constructor `ReportS` added to `Brassica.SoundChange.Types.Statement`
  - New functions `Brassica.SoundChange.Apply.Internal.applyChangesWithReports`,
      `Brassica.SoundChange.Apply.Internal.applyChangesWithChangesWithReports`
  - A constructor `ReportWord` has been added to `Brassica.SoundChange.Apply.Internal.LogItem`,
      while its former record fields `input` and `output` have been replaced with functions
      `logInput` and `logOutput`.
  - `Brassica.SoundChange.Frontend.Internal` has been restructured to allow display of intermediate results
  - New function `Brassica.SoundChange.Tokenise.joinComponents` added to assist with said restructure
- Changes resulting from addition of features:
  - New constructor `Feature` added to `Brassica.SoundChange.Types.Lexeme`
  - New type `Brassica.SoundChange.Apply.Internal`

## 0.3.0

### Behaviour

- Bugfix: nested categories are now matched up correctly between target and replacement
- Bugfix: Brassica no longer freezes with rules where the target is entirely optional
- Bugfix: Brassica no longer crashes when a rule refers to nonexistent categories
- Wildcard symbols can now be used in the replacement of a rule
- Brassica now applies sound changes to words in parallel, giving a significant speedup on multi-core machines (though not in a webpage)
- New `extra` directive allows specifying characters which should never be replaced through all category redefinitions
- Improved placement of etymologies in MDF output
- Target and replacement can now be separated by `->`
- New `filter` directive allows removing unwanted results
- An improved heuristic for avoiding infinite loops in epenthesis rules,
    such that e.g. `/h/a_a` yields `aaaa`→`ahahaha` rather than previous unexpected *`ahaaha`
- New `-??` flag to allow for per-occurrence sporadicity
- `categories` directive can now be specified `noreplace`
    to prevent replacement of unknown graphemes with U+FFFD (�)
- Improved method for highlighting words ‘different to last run’
    (now using the Myers diff algorithm)
- Added syntax highlighting for flags
- Sound change rules can now be specified on the command-line using new `--eval` or `-e` flag
- Add CLI option to highlight words different to input
- Currently open files shown in title of desktop window
- Desktop application warns when closing with unsaved changes
- Improve user interface for file management in desktop paradigm builder
- New CLI for paradigm builder: program name `brassica-pb`

### Code

- `optparse-applicative` lower bound tightened to 0.17.1
- Bugfix: `Brassica.SoundChange.Apply.applyRuleStr` is no longer seriously broken
- `Brassica.SoundChange.Apply.Internal.applyOnce` now returns a `RuleStatus` value with more detailed information about the rule application, which is now used by `Brassica.SoundChange.Apply.Internal.setupForNextApplication`
- `Wildcard` and `Kleene` no longer have `OneOf 'Target 'Env` constraint
- `OneOf` type family is no longer used and has been removed
- `Target` and `Environment` `LexemeType`s have been unified as `Matched`
- `Brassica.SoundChange.Frontend.Internal.parseTokeniseAndApplyRules` now takes another argument specifying how to map over the parse output,
    allowing it to be run both on a single core and in parallel depending on the provided function
- `Brassica.SoundChange.Types.Directive` has a new constructor `ExtraGraphemes` for the `extra` directive, with corresponding changes in parsing and expansion
- `Brassica.SoundChange.Category.extend` has been renamed to `extendCategories`, and now requires pattern-matching on a `Categories` directive before use
- `Brassica.SoundChange.Frontend.Internal.parseTokeniseAndApplyRules` no longer implements rule expansion,
   allowing it to take place only once without needing to be repeated for each rule application.
- MDF support has been comprehensively rewritten:
  - `Brassica.MDF` has been removed
  - New module `Brassica.SFM.SFM` implements generic support for SIL Standard Field Marker hierarchies
  - New module `Brassica.SFM.MDF` describes the standard and alternate MDF hierarchies,
    and other necessary utilities for working with MDF documents
  - Some rewrites to `Brassica.SoundChange.Frontend.Internal` to account for the new architecture
- New type `Brassica.SoundChange.Types.Filter`, resulting in other changes:
  - New `FilterS` constructor added to `Brassica.SoundChange.Types.Statement`
  - `LogItem` and `PWordLog` (in `Brassica.SoundChange.Apply.Internal`) now use `Maybe PWord`
    to show cases where a word was deleted
  - Corresponding changes to parsing, expansion and application
- Rule sporadicity is now represented by a dedicated type, `Brassica.SoundChange.Types.Sporadicity`
- `Categories` constructor (in `Brassica.SoundChange.Types.Directive`)
    now has an extra field for `noreplace` directive
- Add useful function `Brassica.Paradigm.Apply.depth`

## v0.2.0

- Allow grapheme to begin with star
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
