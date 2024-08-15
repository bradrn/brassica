<!-- -*-GFM-*- -->

# Brassica: reference

This document descibes the syntax and application of Brassica sound changes.
If you’re new to Brassica, you probably want to read the [getting started guide](Getting-Started.md) instead.

## Elements of a sound change file

A Brassica sound change file comprises a list of **statements**,
  separated by one or more newlines.
Each statement can be:
- a [sound change](#sound-change-rules);
- a [category definition block](#category-definition-block);
- a [filter](#filters); or
- an [extra graphemes declaration](#extra-graphemes).

Brassica processes statements in order from the top of the file to the bottom.
For each input word, a statement may make modifications to the word,
  possibly changing the number of output words.
Alternately a statement can change how Brassica interprets following rules:
  for instance, by defining new categories.

## Sound change rules

The bulk of this document is dedicated to describing **sound change rules**.
These are the most common type of statement in an ordinary Brassica sound change description.

### Overall structure and interpretation

A sound change rule has the following general structure,
  where `(...)` indicates that the section within the brackets is optional,
  and `(...)*` indicates that the section within the brackets can be repeated one or more times:
```
(flag)* target / replacement (/ environment)* (// exception)
```
The first slash in a sound change may be replaced by `→` or `->` with no change in meaning.

These components are as follows:
- A rule begins with zero or more space-separated [**flags**](#flags)
    which change the way in which the rule is applied.
- The **target** denotes the part of the input word to be replaced with the replacement.
  It consists of zero or more lexemes (see below).
- The **replacement** denotes what should be substituted for the target in the output word.
  Like the target, it consists of zero or more lexemes.
- The **environment**s specify the situations in which the target will be replaced by the replacement,
    and any **exception** specifies a situation in which the target must not be replaced by the replacement.
  Each has the form `before _ after`,
    where `before` and `after` each comprise zero or more lexemes (like the target).

Each **lexeme** represents a certain part of an input or output word.
Lexemes must be separated by spaces except when omitting the spaces is unambiguous.

In the target, environment and exception,
  a lexeme describes a part of the input word to be **matched** by Brassica.
In the replacement,
  a lexeme denotes a part of the output word which is **produced** by Brassica.
Lexemes in the replacement can **take their value** from lexemes in the target,
  to produce complex sound changes where parts of the input word are copied or altered in the output word.

The available lexeme types are as follows:
- One or more letters, matching a single [**grapheme**](#graphemes) or a **predefined category**
  - The **word boundary** marker `#` is treated as a special kind of grapheme
- A list of graphemes within square brackets, representing a **category** of graphemes
  - `@n` (where `n` is a number) before a category denotes a category **backreference**
  - `@?` before a category denotes **multiple outputs**
- A sequence of lexemes within parentheses, representing **optional** lexemes
- `>`, representing a **geminate** grapheme
- `^`, representing a **wildcard** matching any number of graphemes until another lexeme
- `*`, the Kleene star, representing **repetition** of a lexeme
- Within the replacement only:
  - `~`, representing a **discarded** category
  - `/`, representing **metathesis** of the target
For more on each of these lexemes, see below.

(Note that the underscore is *not* a lexeme: it is a separator between lexemes.
As such it cannot occur everywhere that other lexemes can.)

To understand the sound change application process, consider some arbitrary input word.
This word is composed of a list of **graphemes** (either single letters or multigraphs),
  starting and finishing with word boundary markers.
Brassica then applies a sound change of the form `target / replacement / before _ after` as follows:

1. The application process begins at the initial word boundary.
2. Test the target and environment:
   1. Test each lexeme in `before` to see if it matches each next grapheme in the input word.
      If they all match, move forward to after the matched graphemes.
   2. Test each lexeme in `target` to see if it matches each next grapheme in the input word,
        recording any information which might be needed for the replacement.
      If they all match, and move forward to after the matched graphemes.
   3. Test each lexeme in `after` to see if it matches each next grapheme in the input word.
      If they all match, move forward to after the matched graphemes.
3. Did all of the above lexemes match successfully?
   - If any did not match, go back to the position before the environment,
       move forward by one grapheme if possible, and try again from step **2**.
   - If they all matched, delete the target, and replace it with the new graphemes specified by the `replacement`
       (creating multiple output words if necessary).
     Then move to the grapheme after the replacement and go back to step **2**.

A particularly important consequence of this rule application process is that *environments can overlap, but targets never can*.
For instance, compare the two rules below:
```
C V C V / C V́ C V
V / V́ / C _ C V
```
On a word of the form `CVCV`, these two rules behave identically.
However, they behave differently on a word of the form `CVCVCV`:
- The target of the first rule matches the first four graphemes, yielding `CV́CVCV`.
  After this, application moves on to the remaining graphemes after the target, namely `CV`.
  Therefore the rule cannot apply further and the final result is `CV́CVCV`.
- The target of the second rule matches the first `V`, surrouded by the environment `C _ C V` as expected.
  After this, application moves on to the remaining graphemes after the target, which are now `CVCV`.
  The rule can again match the first `V` surrounded by `C _ C V`.
  After this target the remaining graphemes are `CV`, and the rule cannot apply further:
    the final result is `CV́CV́CV`.
The behaviour of this first rule can be useful to implement e.g. alternating stress patterns.
On the other hand, it can cause difficulties with implementng other sound changes such as vowel harmony.

### Flags

The rule application process described above can be modified by **flags** at the beginning of a rule.
Brassica supports the following flags:

- `-rtl` alters the rule application process to begin at the right edge of the word and move backwards,
    rather than starting at the left edge and moving forward as described above.
  (The default behaviour can be explicitly requested with the flag `-ltr`, which currently has no effect.)
- `-1` causes a rule to be applied at most once per word, by halting the application process after the first replacement.
- `-?` yields a sporadic rule: in addition to the expected result, the input word is preserved as a second output word.
- `-??` yields a rule which is sporadic per-occurrence:
    two outputs are produced for each application site, where the rule has been applied at that site in one output, and has not been applied in the other output.
  In total, for *n* application sites this yields up to 2*ⁿ* outputs.
- `-x` suppresses output highlighting in the GUI.
  For details see the [Getting Started guide](Getting-Started.mdw).

Flags can be combined (when not incompatible with each other) by adding spaces between them.
For instance, a rule beginning with `-rtl -1` will be applied only once, at the rightmost possible position in the word.
The order of flags does not matter.
For incompatible flags (`-ltr`/`-rtl`, or `-?`/`-??`) the behaviour is left unspecified.

### Graphemes

As mentioned previously, words are represented as lists of **graphemes**.
A grapheme corresponds to one or more Unicode characters.
Graphemes are the basic unit of sound change application in Brassica:
  in particular, Brassica will never split up a multiple-character grapheme (**multigraph**),
  although a sound change rule may explicitly replace it with single-character graphemes.

Sound change application begins with input words being **tokenised** into graphemes.
Tokenisation depends on whether the sound change file contains any [category definition blocks](#category-definition-block).
If it does not, tokenisation will never form multigraphs:
  each character (including combining marks) is treated as its own grapheme.

However, if a category definition block is present,
  the first such block will be scanned for multigraphs by Brassica.
Any multigraphs listed in that block will be tokenised as a single grapheme if present in any input words.

After tokenisation, multigraphs can be introduced or removed by sound change rules.
For instance, a sound change `t h / th` will convert
  a sequence of the two graphemes ⟨t⟩ and ⟨h⟩
  to the single grapheme ⟨th⟩.
The reverse would be `th / t h`, which works as expected.

The **word boundary** marker `#` is treated as a special kind of grapheme.
For each sound change application,
  word boundaries are automatically inserted at the beginning and end of every input word
Thus rules such as `k / / _ #` or `v / b / # _` will work as expected.

However, if the input contains any instances of `#`,
  these will also be treated as explicit word boundaries.
In addition to the usual sound changes with word boundaries in the environment,
  these internal word boundaries can be deleted or inserted with rules
  such as `# / / _` (which deletes all internal word boundaries).

(It can be particularly useful to choose a grapheme to act as a marker of boundary type.
For instance, one could distinguish a clitic boundary as `#=#` and a word boundary as `##`.
Two boundary markers are used such that both the preceding and following graphemes
  are adjacent to word boundaries as expected by sound changes.)

### Categories

A **category** is a list of graphemes or lexemes which should be treated in the same way.
In brief, a category can match or produce any of its constituent elements.

The simplest way to define a category is by placing a list of space-separated graphemes in square brackets:
  for instance, `[p t ch k]` is a category which contains the four graphemes ⟨p⟩, ⟨t⟩, ⟨ch⟩ and ⟨k⟩.
As a special kind of grapheme, word boundaries are also allowed in a category.
A category may include other lexemes, or sequences of zero or more lexemes, if they are written between curly braces `{}`.

Categories can also be predefined in a [category definition block](#category-definition-block).
Such a block associates graphemes with categories:
   afterwards, whenever one of those graphemes is seen in a sound change rule, it is replaced with the corresponding category.
This allows more convenient reuse of categories such as ‘all vowels’ or ‘all consonants’ when they are used by many different rules.

Predefined categories can be combined with each other or with other graphemes or lexemes.
This is done by mentioning them in a subsequent category.
By default, this yields the **concatenation** of the two categories.
If the second category is prefixed by `+`, it creates the **intersection** of the categories;
  if it is prefixed by `-`, the result is the **subtraction** of the second category.

For example, consider a category definition block
  which defines `Stop` as the category `[p t k]`, and `Labial` as `[m p f]`.
Then:
- `[Stop Labial]` is the category `[d p t k m p f]`
- `[Stop +Labial]` is the category `[p]` (the intersection of the two categories: they have only ⟨p⟩ in common)
- `[Stop -Labial]` is the category `[t k]` (all `Stop`s which are not `Labial`)

Multiple categories are combined left to right.
For instance, `[m Stop]` would be `[m p t k]`, so `[m Stop -Labial]` would be `[m p]`.

Each category is therefore, ultimately, a list of lexemes (which will usually be single graphemes).
In the description below, the **index** of a lexeme in a category will refer to its position in the list:
  thus, first, second, third, etc.

A category will match an input word if and only if
  at least one of its constituent graphemes or lexemes match that input.
For each category matched in the target,
  the index of its matched element(s) will be be saved for use in the replacement.

Each category in the replacement can take its value from a category matched in the target.
When this occurs, the replacement category produces the lexeme(s)
  which are at the same index as that of the lexeme(s) which were matched by the corresponding target category.
If the replacement category is too short to have any element at that index,
  the Unicode replacement character U+FFFF (�) is inserted instead.
If more than one element was matched in the input category, multiple output words will be produced,
  for each corresponding element in the output category.

In the absence of backreferences (explained below),
  Brassica matches replacement categories with target categories from left to right.
The first category encountered in the replacement takes its value from the first category matched in the target;
  the second category in the replacement takes its value from the second in the target;
  and so on.

If there are more categories in the replacement than in the target,
  some replacement categories may not correspond to any category in the target.
In this case Brassica creates **multiple output words**: one for each element of the category.
This behaviour can also be forced for any category in the replacement, by writing `@?` before the category.

There are two ways to control the matching of categories between the target and replacement.
Firstly, the **discard** character `~` causes the next target category to be ignored in the replacement.
Effectively, it acts like a replacement category which never produces output.
For instance, `[a b] [a b] [a b] / [x y] ~ [x y]` applied to input word `aba` produces output `xx`.

Secondly, a **backreference** can be placed before a category in any part of a rule.
A backreference takes the form `@n`, where `n` can be any number greater than 0.

The meaning of a backreference depends on its position in the rule.
In the replacement, a backreference specifies which target category corresponds to the replacement category.
If each target category is given a number starting from 1, counting from left to right,
  a replacement category backreferenced to `@n` will take its index from the `n`th category in the target.
As with `@?`, such backreferenced categories are skipped when matching up other target and replacement categories.

When matching, a backreference instead **constrains** which category elements can be matched.
Taking a backreference in the target as an example, this operates as follows:
1. Categories in the target are numbered from 1 as previously described.
2. If a target category is encountered with a backreference `@n`:
   - If the `n`th target category has been matched,
       retrieve the matched index of that category,
       and attempt to match the lexeme(s) at that same index of the current category.
   - Otherwise, fail.
Similar descriptions apply to the environment and exception.

### Lexemes for repetition

The remaining lexemes describe different kinds of repetition:
  parentheses for optional lexemes (0× or 1× repetition),
  `>` for gemination (2× repetition),
  `*` for arbitrary repetition, `^` for wildcard matching and `\` for metathesis.

Blocks of **optional lexemes** can be expressed by surrounding the lexemes in question with parentheses.
When matching an optional block, two matches are attempted:
  one in which the optional lexemes are matched, and another in which the optional lexemes are ignored.
(If both matches succeed, more than one output word may be produced.)

Each optional block in the replacement takes its value from an optional block in the target,
  in a similar manner to categories.
If an optional block in the target matched successfully,
  the corresponding optional lexemes in the replacement will produce graphemes as usual.
But if the optional block in the target did not match,
  no graphemes will be produced by the corresponding optional lexemes in the replacement.
If an optional block in the replacement does not correspond to any optionals in the target,
  two outputs will be produced corresponding to both of those cases.

**Gemination** can be expressed with `>`.
In the target, environment or exception, this always matches a grapheme which is the same as the last one matched.
This is the case even between the target and another rule part:
  for instance, `[a e] / i / _ >` will take `aa` to `ia`, and `ee` to `ie`, but `ae` or `ea` will not change.

Similarly, in the replacement, `>` produces a grapheme
  which is the same as the previous grapheme which was produced.
At the beginning of the replacement there is no previous grapheme,
  so `>` in this situation will produce no output graphemes.

**Arbitrary repetition** can be expressed by following a lexeme with `*`
  (also known as the **Kleene star**).
When matching, a lexeme followed by `*` is matched repeatedly, as many times as possible (zero or more).
In the target, the number of times it matched is saved.

In the replacement, a lexeme followed by `*` produces graphemes repeatedly.
Similarly to categories and optional lexemes,
  the number of times the lexeme is repeated is taken from a corresponding Kleene star in the target.
If there is no corresponding Kleene star, no output is produced by this lexeme.

The character `^` before another lexeme denotes a **wildcard** which can match or produce any graphemes.
When matching, this matches zero or more graphemes, until the following lexeme can match.
In the target, the matched graphemes are saved for use in the replacement.

As with the Kleene star, the graphemes produced by a wildcard in the replacement
  depend on what was matched in the target.
Each wildcard in the replacement produces the same graphemes
  which were matched by the corresponding wildcard in the target, if there is one.

Finally, **metathesis** is denoted by `\` in the replacement only.
This simply takes the graphemes matched by the whole target,
  and produces those in reverse order.

## Category definition block

### Overall structure and interpretation

A **category definition block** is introduced by writing `categories` on its own line,
  and ended by writing `end` on its own line.
The first line can also contain `new` before `categories`, and/or `noreplace` after `categories`
  (both separated by a space).

Between the first and the last lines, one can write **category definitions**.
A category definition contains a grapheme, followed by `=` and then a category.
Any category description which can go between square brackets in a sound change rule
  may also follow `=` in a category definition.

(A category definition block can also contain [feature definitions](#feature-definitions), described below.)

Each category definition defines the grapheme before the `=` as a synonym for the category after the `=`.
This definition remains in effect for all statements following the category definition, unless:
- Some following category definition gives a new definition for the same grapheme
    (in a different category definition block or in the same one)
- A following category definition block is introduced with `new`,
    after which point all previous category definitions are removed.

A category definition block has the side-effect of **filtering unknown graphemes** from input words.
For each category definition block, Brassica maintains a list of all graphemes mentioned so far, comprising:
- Those mentioned on the right-hand side of category definitions in that block
- Those similarly mentioned in all previous category blocks,
    up to and including the last block introduced with `new categories`
- Any [extra graphemes](#extra-graphemes) mentioned before the category block
Then, when a word encounters a category definition block
  (after having been modified by zero or more sound changes or other statements),
  any graphemes not in this list are replaced by U+FFFF (�).
This behaviour can be disabled by introducing the category definition block with `noreplace`.

(Additionally the first category block in the file influences tokenisation into multigraphs,
  as [described above](#graphemes).)

### Feature definitions

Category definition blocks can also include **feature definitions**.
Each feature definition has the following form:
```
feature (C₁ =) <category₁> / C₂ = <category₂> (/ Cₙ = <categoryₙ>)*
```
Where parentheses enclose optional elements, an asterisk denotes repetition zero or more times,
  each `C` can be any grapheme name, and each `<category>` is a category.
In what follows, a subscript `ₙ` (or superscript `ⁿ`) represents some arbitrary number.

Let each `<categoryₙ>` have elements `cₙ¹`, `cₙ²`, etc.
Then, a feature definition as above creates the same category definitions as if the following had been written:
```
(C₁ = <category₁>)
C₂ = <category₂>
(Cₙ = <categoryₙ>)*

(c₁ⁿ = c₁ⁿ c₂ⁿ (cₙⁿ)*)*
```

For a concrete example, consider:
```
feature V = a e i o u / Vhigh = á é í ó ú / Vlow = à è ì ò ù
```
This creates the following category definitions:
```
V = a e i o u
Vhigh = á é í ó ú
Vlow = à è ì ò ù

a = a á à
e = e é è
i = i í ì
o = o ó ò
u = u ú ù
```
The key is the last set of category definitions:
  after this point, every mention of `a` will match any of `[a á à]`, and so on.

Occasionally this last behaviour might be unwanted:
  one might want to refer to the **base grapheme** (say, `a`) alone.
This can be accomplished by following it with a tilde: `a~`.

## Other statements

### Filters

A **filter** is a statement which deletes output words matching some criterion.
A filter is defined by writing `filter`, followed by a sequence of lexemes as in the target of a rule.
The filter will then remove from the output any words which match those lexemes.

Filters are most useful together with rules which can create multiple outputs.
A filter may be used to delete unwanted output words
  (for instance, those which violate some phonological constraint)
  while keeping wanted outputs.

### Extra graphemes

An **extra graphemes** declaration takes the form of `extra` followed by a list of graphemes.
It defines a list of graphemes which are not part of any category
  (and hence not mentioned in any category definition block),
  but nonetheless should never be replaced by U+FFFF by any category definition block.
Each `extra` declaration overwrites any previous one.
