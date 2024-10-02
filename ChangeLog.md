# Brassica changelog

## Unreleased

### Behaviour

(Note: breaking changes are boldfaced.)

- Bugfixes:
  - Etymologies are now added in all situations when processing an MDF file
  - Desktop version correctly warns about unsaved changes when opening a file
  - Rules such as `[p f] / [f h] / (a) _ #` are no longer applied twice to the same grapheme
      (as a consequence of changes to rule application, see below)
  - `noreplace` categories now tokenise multigraphs as expected
- Documentation (user guides and Haddocks) have been comprensively rewritten
- New support for phonetic features:
  - Features can be written as maximal `$name#ident(values)` after another lexeme
  - Categories named `+Feature`, `-Feature` and `+Feature+Value`
      get special interpretation with intersections and subtractions
  - New category syntax: `&` before category name forces union interpretation
      even for category names beginning with `+`/`-`
  - Change to category syntax: first element of category is always interpreted as union
      (**technically a breaking change**, but unlikely in existing sound changes)
  - Syntax `&Feature` after set operation (`&`/`+`/`-`) to include both `-Feature` and `+Feature`
      (i.e. all graphemes with a setting for that feature)
  - Declaration `auto <FeaturalCategory>` in category definition block
      can be used to treat said feature autosegmentally
      whenever a grapheme in the selected category is mentioned
- Other changes to sound change syntax:
  - `report` directive allows for reporting intermediate results in input→output mode
  - Graphemes followed by `~` are now allowed inside a category block,
      but **disallowed in category names and in `extra` declarations** (where they would be meaningless)
  - **Category intersection now produces a category with graphemes
      in the same order as the last category mentioned, rather than the first**
  - Optional elements or categories can now be prefixed by `%` to make them match greedily
  - New backreference syntax `@#id` allows categories to be matched by ID rather than number
  - `extra` graphemes are now always taken into account for multigraph tokenisation
      (**technically a breaking change**, but unlikely in existing sound changes)
  - Within a category, single graphemes inside braces are now treated the same as single graphemes outside braces
      (**technically a breaking change**, but unlikely to change existing sound changes)
- Changes to rule application algorithm:
  - **Sound change applications can now overlap,
      such that the replacement from one application can be used as the environment for the next**
  - New flag `-no` allows for reverting to the previous behaviour (non-overlapping applications)
  - **Make RTL and LTR application symmetric,
      by reimplementing the former as LTR application with words and rules reversed:
      consequently category correspondences, backreferences etc. are also reversed**
- Improvements to graphical interface:
  - Whitespace is now preserved when displaying sound change output on desktop and web
  - New keyboard shortcuts in desktop GUI:
      Ctrl+Enter to apply rules, Ctrl+Tab to toggle between rules and words textboxes
  - Some labels have been changed to be more descriptive:
    ‘Different to input’ to ‘Any rule applied’, and ‘Wordlist’ to ‘Wordlist + glosses’
  - Improvements to output from ‘Report rules apply’: most importantly,
    comments are no longer included next to rules, and
    outputs are no longer aligned across input words (greatly increasing responsiveness on desktop)
  - User can now choose which MDF hierarchy to use for dictionaries
  - Web version brought closer to parity with desktop version
      by adding MDF support and an option to ‘synchronise scroll positions’
  - When ‘view results live’ is selected, results are updated on changes to all controls,
      not just changes to words or sound changes
  - Add ‘Edit’ menu to desktop GUI, including ‘Find’ dialog box
  - Add ‘Help’ menu to desktop GUI
  - Fields of web GUI can now be initialised using URL query parameters
    (`r` for rules, `w` for words)
  - Add ‘select all’ buttons to web GUI
  - Improve display of web application controls on small screens
  - Sound changes editor on web version has been rewritten to work around bug with combining diacritics
  - Add option for ‘input→output’ display format to include glosses and whitespace

### Code

- CMakeLists for desktop GUI now track Haskell binary dependency correctly
- Renamed `Brassica.SoundChange.Category` to `Brassica.SoundChange.Expand`
- Renamed constructor `Brassica.SoundChange.Types.DirectiveS` to `Brassica.SoundChange.Types.DeclS` 
- Changes resulting from addition of `report`:
  - New constructor `ReportS` added to `Brassica.SoundChange.Types.Statement`
  - A constructor `ReportWord` has been added to `Brassica.SoundChange.Apply.Internal.LogItem`,
      while its former record fields `input` and `output` have been replaced with functions
      `logInput` and `logOutput`.
  - `Brassica.SoundChange.Frontend.Internal` has been restructured to allow display of intermediate results
  - New function `Brassica.SoundChange.Tokenise.joinComponents` added to assist with said restructure
- Changes resulting from addition of features:
  - New constructors `Feature` and `Autosegment` added to `Brassica.SoundChange.Types.Lexeme`
    (the latter primarily for internal use)
  - New type `Brassica.SoundChange.Apply.Internal`
  - New constructor `DefineAuto` added to `Brassica.SoundChange.Types.CategoryDefinition`
  - Changes in category expansion:
      new type `Brassica.SoundChange.Expand.AutosegmentDef`,
      and new constructor `InvalidDerivedValue`
        added to `Brassica.SoundChange.Expand.ExpandError`
- Changes resulting from new rule application algorithm:
  - New constructore `PrevEnd` added to `Brassica.SoundChange.Apply.Internal.RuleTag`
  - New field `nonOverlappingTarget` added to `Brassica.SoundChange.Types.Flags`
- New constructors `GreedyOptional` and `GreedyCategory` for `Brassica.SoundChange.Types.Lexeme`
- `Brassica.SoundChange.Frontend.Internal.InputLexiconFormat`
    now depends on new type `Brassica.SoundChange.Frontend.Internal.MDFHierarchy`
- Word boundaries are now simply ordinary graphemes `"#"`:
    `Brassica.SoundChange.Types.Grapheme` is now a type synonym for `[Char]`
- Expansion now results in sound changes with declaration type `GraphemesList`
    rather than `[Grapheme]`
- API of `Brassica.SoundChanges.Apply` (and `Brassica.SoundChanges.Apply.Internal`) has been refactored:
  - `LogItem` no longer stores redundant information on input words
  - `PWordLog` is now based on `LogItem`
  - Former functions `applyRule` and `applyStatement`
      have been renamed to `applyRuleMZ` and `applyStatementMZ`;
  - Former functions `applyRuleWithLogs` and `applyStatementWithLog`
      have been renamed to `applyRule` and `applyStatement`
  - Function `applyRuleWithLog` has been removed
  - Set of sound change application functions `applyChanges*`
      has been replaced with a single function `applyChanges` returning a `PWordLog`,
      plus a set of functions to extract various outputs from it
- New constructor `WordsWithProtoOutputPreserve` for `Brassica.SoundChange.Frontend.Internal.OutputMode`
- Remove now-unnecessary pattern `Brassica.SoundChange.Types.Boundary`
- In parsing modules, re-export whole module `Text.Megaparsec.Error`
    rather than only re-exporting the single function `errorBundlePretty`
- Category elements are now represented as simply `[Lexeme category a]`
    (synonym `Brassica.SoundChanges.Types.CategoryElement`),
    without any special case for single-grapheme elements
- `concatWithBoundary` is now no longer re-exported from `Brassica.SoundChange.Tokenise`

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
