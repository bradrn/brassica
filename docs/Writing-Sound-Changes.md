<!-- -*-GFM-*- -->

# Writing sound changes in Brassica

## Summary table of contents

1. [What is Brassica?](#what-is-brassica)
2. [Sound change files](#sound-change-files)
3. [Basic sound changes](#basic-sound-changes)
4. [Advanced usage](#advanced-usage)
5. [Phonetic features](#phonetic-features)

## What is Brassica?

Brassica is a **sound change applier**:
  a computer program which can simulate the evolution of words as they are affected by sound changes.
Brassica defines a syntax for representing sound changes in a format which is computer-understandable,
  to allow their simulation.

This guide will focus on **how to write common sound changes in Brassica format**.
It does not aim to comprehensively cover every feature of Brassica:
  for that, refer to the [reference](Reference.md).

> **Notational conventions in this guide**:
> - Sound changes or parts of sound changes are written in code formatting (`like this`)
> - Input or output words, or parts of words, are written in angled brackets (⟨like this⟩)
> - Example sound changes are specified in code blocks,
>     followed by comments showing examples of rule application to representative words
>
> Additionally, all examples are automatically tested and validated
>   against the current version of Brassica.
> Therefore each example is completely self-contained
>   (all relevant categories are defined within the example, etc.).

## Sound change files

A Brassica sound change file lists a set of sound changes which Brassica should apply to input words.
Each sound change is specified on its own line,
  using the syntax described below.
Sound changes are applied to input words in order, from top to bottom.

A sound change file can also include other elements besides sound changes.
The most important are [category definition blocks](predefining categories),
  which allow sets of phonemes to be defined before use.
Other elements will be introduced below as they become relevant.

Additionally, a semicolon `;` can be inserted at any point in a sound change file.
Any text from the semicolon to the end of the line is ignored.
This text is called a **comment**.

Sound changes are most often applied to **words files**,
  which list a set of words to evolve.
Words files have the following format:

- Each line can contain zero, one or more input words.
- If multiple words are on the same line, they must be separated by whitespace.
- A **gloss** can be included by surrounding it with square brackets `[like this]`.
  The text within the square brackets is not affected by sound changes.

As an alternative input format,
  Brassica also supports dictionaries in the MDF format.
For more on using this format, refer to
  [Coward & Grimes (2000)](http://downloads.sil.org/legacy/shoebox/MDF_2000.pdf).
  
## Basic sound changes
  
### Unconditional sound changes

The simplest sound changes have the following form:
```
target / replacement
```
Such a rule will replace every instance of the ‘target’ by the ‘replacement’.
The target and replacement (and other parts of more complicated rules)
  are composed of sequences of **lexemes**, separated by a space.

In the simplest rules, both the target and the replacement are a single letter (or **grapheme**).
For instance, a rule to unconditionally change ⟨θ⟩ to ⟨f⟩
  (as found in some English dialects)
  can be specified as follows:
```brassica
θ / f

; θɪn → fɪn
; mɪθs → mɪfs
; pɑːθ → pɑːf
; ðɪs → ðɪs (no change)
```

(Note: the first slash in a rule can be replaced by `→` or `->` if you prefer.)

The target and replacement can contain multiple graphemes.
However, as mentioned earlier, **they must have spaces between them**.
(This is important! See the section below on [multigraphs](#multigraphs) for details.)
For instance, the following rule changes ⟨ki⟩ to ⟨čə⟩:
```brassica
k i / č ə

; kita → čəta
; akiə → ačəə
```

### Conditional sound changes

Conditional sound changes take the following form:
```
target / replacement / before _ after
```
Like unconditional sound changes, such a rule will replace the ‘target’ with the ‘replacement’.
However, this will only happen when the target is preceded by ‘before’ and followed by ‘after’.
The section `before _ after` is called the **environment** of the soudn change.
As with the target and replacement,
  the ‘before’ and ‘after’ portions of the environment are each a list of lexemes.

The simplest environment is a single underscore.
(That is, an environment where neither `before` nor `after` is present.)
This is exactly the same as leaving the environment out altogether:
  it makes the rule unconditional, by constraining neither `before` nor `after`.

For a more interesting example, the following rule:
```brassica
t / s / i _ i

; iti → isi
; keritim → kerisim
; kita → kita (no change)
; koti → koti (no change)
```
…replaces ⟨t⟩ with ⟨s⟩ only when it’s between two ⟨i⟩s.

A common kind of condition is that a sound change should occur at a **word boundary**.
Word boundaries may be specified as `#`.
For instance, this rule changes ⟨n⟩ to ⟨ŋ⟩ at the end of a word:
```brassica
n / ŋ / _ #

; pan → paŋ
; ana → ana (no change)
; nap → nap (no change)
```

A rule may take place in any of **multiple environments**.
Specify this by adding more slashes to the end of the rule:
```
target / replacement / before1 _ after1 / before2 _ after2 / …
```
Such a sound change will take place when the target is between ‘before1’ and ‘after1’,
  or when it is between ‘before2’ and ‘after2’, or so on.

For instance, the following rule changes ⟨i⟩ to ⟨ə⟩ at the beginning or at the end of a word:
```brassica
i / ə / # _ / _ #

; ine → əne
; nin → nin (no change)
; eni → enə
; ini → ənə
```

### Deletion and epenthesis

A sound change of **deletion** may be specified by leaving the replacement blank.
This causes the whole target to be deleted (i.e. replaced with blank) in the specified environment,
  or unconditionally if no environment is specified.
This can be used to specify sound changes of **syncope**, **apocope**, **elision** and so on.

For instance, this rule deletes word-final ⟨i⟩:
```brassica
i / / _ #

; doti → dot
; kai → ka
; kaip → kaip (no change)
```

On the other hand, if the target is blank, **epenthesis** takes place.
The replacement is inserted in every position where the environment holds.
For instance, this rule inserts ⟨ʔ⟩ between any two adjacent ⟨a⟩s:
```brassica
/ ʔ / a _ a

; paa → paʔa
; aapee → aʔapee
; anapee → anapee (no change)
```

### Categories of sounds

Many sound changes affect several different graphemes in similar ways.
Brassica handles these using **categories**.
A category is a list of graphemes to be treated similarly.
In a rule, a category can take on the value of any grapheme listed within it.

(More complicated cases can be handled using [phonetic features](#phonetic-features), described below.)

A category can be specified **inline** by listing a set of graphemes between square brackets.
For instance, `[a e i o u]` is the category containing graphemes ⟨a⟩, ⟨e⟩, ⟨i⟩, ⟨o⟩ or ⟨u⟩.
Thus, the following rule will insert ⟨ʔ⟩ between any two vowels (assuming a five-vowel system):
```brassica
/ ʔ / [a e i o u] _ [a e i o u]

; aapee → aʔapeʔe
; kuinofa → kuʔinofa
; kaoete → kaʔoʔete
```

Categories can also contain **word boundaries**.
For instance, the following rule works to delete ⟨ʔ⟩ before a stop or the end of a word:
```brassica
ʔ / / _ [p t k b d g #]

; naʔpe → nape
; ɨnoʔ → ɨno
; ʔaneke → ʔaneke (no change)
; naʔap → naʔap (no change)
```

### Gemination

A repeated or geminate grapheme is represented by the special symbol `>`.
When used in the target or environment, it matches exactly the same grapheme as the last one matched.
Thus, the following sound change degeminates all stops,
  by deleting any stop which occurs before itself:
```brassica
[p t k b d g] / / _ >

; abba → aba
; tekkal → tekal
; atdi → atdi (no change)
```

In the replacement, it instead repeats the previous grapheme.
This behaviour is particularly useful when
  [the replacement contains a category](#categories-in-the-replacement).
For instance, this sound change geminates word-final voiceless stops:
```
[p t k] / [p t k] > / _ #

; tap → tapp
; etek → etekk
```

### Predefining categories

In many cases, a category such as `[a e i o u]` will be used repeatedly across many rules.
Such categories can be **predefined**  to give them a name, like so:
```brassica
categories
C = m n ŋ p t k b d g f s v z r l w y
V = a e i o u
end

/ ʔ / V _ V

; aapee → aʔapeʔe
; kuinofa → kuʔinofa
; kaoete → kaʔoʔete
```
Here, `C` is defined to be synonymous with `[m n ŋ p t k b d g f s v z r l w y]`,
  and `V` is defined to be synonymous with `[a e i o u]`.
They are defined in a **category definition block**,
  beginning with `categories` on its own line and ending with `end` on its own line.

Particularly long categories can be defined by combining smaller ones, like so:
```
categories
Nasal = m n ŋ
Stop = p t k b d g
Fric = f s v z
Approx = r l w y
C = Nasal Stop Fric Approx
V = a e i o u
end
```
This has the same effect as the previous example.
`C` is defined as the elements of `Nasal`, followed by the elements of `Stop`, and so on.
Categories and graphemes can be mixed: for instance, `[Approx i u #]` is a valid category.

(Categories can be combined in other ways too: see [later](#combining-categories) for more.)
  
In normal usage, every grapheme used in a ruleset should belong to at least one category.
To enforce this, whenever Brassica encounters a category block,
  it replaces any grapheme not listed in the block with the Unicode replacement character �.
This behaviour is particularly useful in cases where sound changes dramatically change the phoneme inventory of a language.
In such cases, multiple category blocks can be used,
  with each corresponding to the inventory at a particular point in time, such as in the following example:
  
```
categories
C = p t k m n f s x r w y
V = a ɛ e i ɔ o u
end

; various sound changes

; further sound changes which alter the vowel inventory

categories
V = a e ŏ o i ŭ u
end
```

In the above example, the later definition of `V` replaces the earlier definition of `V`.
If the output from the sound changes includes a sound outside the expected final inventory,
  the second category block will cause the unexpected sound to be replaced by �, immediately highlighting the mistake.

In some cases the phonology will have changed so dramatically that all categories will need to be replaced entirely.
For this case, Brassica allows the category block to be introduced with `new categories` rather than `categories`.
Such a declaration will clear all pre-existing category assignments.

Furthermore, don’t forget that you can write rules before the first category definition!
This is a good place to do operations such as deromanisation.
Otherwise, Brassica would replace any unknown graphemes in the romanisation with the replacement character �.

On occasion, you might want to use special characters which do not fit into any category.
You can specify this by listing them after the special directive `extra`, before the first category definition.
This will indicate to Brassica that it should never replace these characters by �.
Thus, for instance, a sound change which makes used of morpheme and clitic boundaries might specify:

```
extra - =
```

Finally, if you dislike this behaviour of replacing unknown graphemes by �,
  you can add the instruction `noreplace` after `categories`.
(This can still be combined with `new` before `categories`.)
In this case Brassica will simply leave unknown graphemes unchanged.
This is not recommended, as it makes mistakes harder to find!

### Categories in the replacement

Consider the following set of sound changes:
```
b / m / _ #
d / n / _ #
g / ŋ / _ #
```
These, collectively, convert voiced stops to nasals when word-final.
Each sound change has the same form, differing only in the precise graphemes which are present.

These three sound changes can be condensed to one, by using a category in the replacement:
```brassica
[b d g] / [m n ŋ] / _ #

; anab → anam
; tedped → tedpen
; egog → egoŋ
; idrek → idrek (no change)
```
Here, the grapheme represented by the replacement category
  depends on which grapheme was matched in the target.
The first element `b` of the target category is replaced by the first element `m` of the replacement category,
  and similar for their second elements (`d` / `n`) and third elements (`g` / `ŋ`).
  
To represent more complex sound changes,
  **more than one category** can be used in a single sound change,
In this case,
  each category in the replacement takes its value from a different category in the target:
  the first in the replacement corrresponds to the first in the target,
  the second in the replacement corresponds to the second in the target,
  and so on.
For instance, the following (rather contrived) rule
  will replace stop–nasal sequences with nasal–stop sequences,
  preserving places of articulation:
```brassica
[p t k] [m n ŋ] / [m n ŋ] [p t k]

; akme → aŋpe
; tne → nte
; apŋ → amk
```
Expressed without categories, this would require a list of nine sound changes.

(Note that if the replacement contains more categories than the target,
  Brassica will detect the ambiguity and produce [multiple results](#multiple-results).)

On occasion it’s useful to replace one grapheme by many.
For instance, you might want to replace ⟨ē⟩ by ⟨yə⟩ and ⟨ō⟩ by ⟨wə⟩.
This can be accomplished by surrounding the replacements with braces:
```brassica
[ē ō] / [{y ə} {w ə}]

; atēpe → atyəpe
; ōmonē → wəmonyə
```
(It would be incorrect to use `[yə wə]`, as that would create undesired [multigraphs](#multigraphs).)

Braces may also contain zero lexemes.
Thus, the following sound change replaces `b` with `p` and `d` with `t` at the end of a word,
  but replaces `g` by nothing:
```brassica
[b d g] / [p t {}] / _#

; abed → abet
; dadag → dada
```

### Romanisation

Unlike some sound change applies, Brassica has no special provisions for romanisation conventions.
This is because they can be written as ordinary sound changes.
For instance, the ‘Latin to Portuguese’ sound changes
  specify at the end `-x l j / lh`, to convert ⟨lj⟩ to ⟨lh⟩ as required by Portuguese spelling.

Note that the previous rule begins with `-x`.
This is an example of a **flag**:
  an element which changes something about the rule application process.
In this case, it suppresses rule highlighting in the graphical interface.
It is suggested to add `-x` before every romanisation rule,
  or any other such ‘unimportant’ sound changes:
  refer to the document on [Using Brassica](./Using-Brassica.md) for more information.

## Advanced usage

### Multigraphs

Often, two letters are used to represent a single sound, as in ⟨sh⟩ or ⟨ng⟩.
In Brassica this is called a **multigraph**.

By default Brassica makes a separate grapheme for every letter or character in an input word.
You can convert sequences of letters into multigraphs using ordinary sound changes,
  after which all sound changes will treat those multigraphs as single, indivisible graphemes:
```brassica
s h / sh
s / z

; asa → aza
; asha → asha
```

Note how, after the first rule here (`s h / sh`),
  Brassica treats `sh` as its own grapheme.
It is distinct from a sequence of two graphemes `s`+`h`,
  and therefore is not affected by the next sound change `s / z`.
  
Incidentally, be careful to include spaces between graphemes which are *not* multigraphs!
If you accidentally write a rule such as `ā / ay`, it will produce odd results,
  since Brassica will interpret the replacement ⟨ay⟩ as a single grapheme.
Writing `ā / a y` avoids this issue.

By default, Brassica tokenises input words into single-letter graphemes:
  ⟨bath⟩ becomes `b`+`a`+`t`+`h`, and ⟨enga⟩ becomes `e`+`n`+`g`+`a`,
  even if subsequent rules create multigraphs `th` and `ng`.
However, this behaviour changes if any multigraphs are listed
  in the **first category block** in the sound changes file.
(They can also be listed in an `extra` graphemes declaration
  if you don’t feel like writing out a whole category block.)
In this case, Brassica will assume that those multigraphs are also present in the input,
  and will create those multigraphs in the input without needing any extra rules.
Brassica encourages a style where each category block lists all relevant phonemes,
  so this will usually result in correct tokenisation.
  
Thus, in the following example, ⟨ch sh⟩ are tokenised as single graphemes
  because they are listed in a category block.
They are thus treated as single units by the following sound changes:
```brassica
categories
C = p t ch k b d j g f s sh h r l w y

V = a e i o u
end

h / sh / _ i
C C / C >

; achi → achi (no change)
; ahi → ashi
; yashfo → yashsho
```

Some languages have a romanisation in which a multigraph and the corresponding letter sequence are both used for different consonants.
This can be observed e.g. in some Australian languages which use ⟨ng⟩ for /ŋ/ but ⟨n⟩+⟨g⟩ for /nɡ/.
Often the latter is represented ⟨n’g⟩ or ⟨n.g⟩ in text, as seen for instance in the language name Ngan’gityemerri /ŋanɡicemeri/.
Such a situation can be dealt with in Brassica using a rule which deletes the separator, such as `’ / / _`.
A sequence such as ⟨n’g⟩ will be tokenised to `n`+`’`+`g`, after which this rule will remove the separator leaving `n`+`g` as desired,
  while a sequence such as ⟨ng⟩ will be straightforwardly tokenised to the multigraph `ng`.
  
An inverse rule, like `/ . / _`, can be useful for debugging purposes.
This inserts a ⟨.⟩ in every possible position between and around graphemes:
```brassica
s h / sh
/ . / _

; abc → .a.b.c.
; ashc → .a.sh.c.
```
This clearly shows the presence of multigraphs.

### Backreferences

Sometimes, categories in the target must be duplicated, deleted, or rearranged in a different order.
For instance, consider a sound change which converts V₁ʔC to V₁ʔV₁C.
This change cannot be written as `V ʔ / V ʔ V / _ C`,
  because the second `V` in the replacement corresponds to no category in the target.

For these situations, Brassica provides **identifier backreferences**.
Specify these as `@#identifier` before a category, where `identifier` can be any name
  (though shorter ones are often easier to read).
Every category given the same identifier must take on a corresponding value.

With this syntax, the previous sound change becomes:
```brassica
categories
C = p t k ʔ
V = a e i o u
end

@#v V ʔ / @#v V ʔ @#v V / _ C

; aʔpe → aʔape
; kaʔetuʔpeʔ → kaʔetuʔupeʔ
```
All categories given the identifier `v` take on the same value as the category in the target.

More complicated combinations are possible.
For instance, the following rule implements nasal assimilation:
```brassica
[m n ŋ] / @#stop [m n ŋ] / _ @#stop [b d g]

; anbe → ambe
; aŋde → ande
; amge → aŋge
```
The identity of the original nasal is ignored;
  instead, its value is taken from the following stop.
  
Backreferences can also appear in the target.
For instance, this sound change converts homorganic nasal-stop clusters into prenasalised stops,
  while leaving heterorganic clusters alone:
```brassica
@#pren [m n ŋ] @#pren [p t k] / @#pren [ᵐb ⁿd ᵑg]

; ampule → aᵐbule
; ŋkane → ᵑgane
; mkana → mkana (no change)
; npenpe → npenpe (no change)
```

(Brassica versions before 1.0.0 used less capable **numeric backreferences**.
These are described [in the reference manual](Reference.md#category-backreferences).)

### Category discards

A particularly common case is for a category in the target to be absent in the replacement.
This can be achieved using the **category discard** `~` in the replacement.
For instance, consider a rule of monophthongisation
  in which the first vowel deletes and the second centralises.
(For instance, ⟨ai⟩ would turn into ⟨ɨ⟩.)
We might attempt to write this as follows:
```
[a e i o u] [a e i o u] / [ɐ ə ɨ ə ɨ]
```
This, of course, does not work as expected:
  the *first* category in the input corresponds to `[ɐ ə ɨ ə ɨ]` in the output.
⟨ai⟩ would become ⟨ɐ⟩, rather than the desired ⟨ɨ⟩.
A tilde can be used to get the desired sound change:
```brassica
[a e i o u] [a e i o u] / ~ [ɐ ə ɨ ə ɨ]

; ai → ɨ
; oe → ə
```

### Optional lexemes

Part of a sound change may be made optional by surrounding it with parentheses, like so:
```brassica
categories
C = m n p t k
V = a e i o u
end

a / e / _ (C) i

; mai → mei
; maki → meki
; mapti → mapti (no change)
```
Here, `(C)` can correspond to either nothing, or a single consonant.

Optional lexemes can also be included in the replacement.
Like categories, these are matched up one-to-one with optional lexemes in the target.
An optional lexeme in the replacement will be included in the output
  only if the corresponding lexeme in the target was matched:

Thus, this sound change converts sequences of alveolar+⟨i⟩ to palatal+schwa,
  while converting an intervening ⟨y⟩ to ⟨i⟩ if there is one:
```brassica
[t d s] (y) i / [ch j sh] (i) ə

; ti → chə
; dyi → jiə
; sai → sai (no change)
```

Warning: be careful when including optional categories in the environment!
In a rule like `Alv (Aprx) [i e] / Alv [ɨ ə]`,
  the category `[ɨ ə]` in the replacement will correspond to the *optional* category `Aprx` when the latter is matched.
The [discard](#category-discards) is useful to fix this: `Alv (Aprx) [i e] / Alv (~) [ɨ ə]`.
Alternatively, you can use a named backreference: `Alv (Aprx) @#vowel [i e] / Alv @#vowel [ɨ ə]`.

### Repeated elements

A grapheme or category may be **repeated** by following it with `*`
  (traditionally called the [Kleene star](https://en.wikipedia.org/wiki/Kleene_star)).
This acts as a generalisation of optionality:
  the lexeme before the star may be matched zero, one, two or more times, as many times as possible.
  
For example, a vowel umlaut rule raising ⟨e⟩ to ⟨i⟩ before another ⟨i⟩ may be implemented as follows:
```brassica
categories
C = m n p t k b d g f s v z r l w y
V = a e i o u
end

e / i / _ C* i

; ei → ii
; emi → imi
; ekti → ikti
; espri → ispri
```
The environment states that the sound change should take place
  whenever ⟨e⟩ occurs before ⟨i` with *any number* of intervening consonants, written `C*`.
  
A Kleene star may also appear in the replacement,
  as long as one appears in the target too.
In this case, it repeats the previous element
  the same number of times as it was matched in the target.
For instance, the following sound change implements a rule of umlaut,
  where word-final ⟨i⟩ is deleted causing the previous vowel to be raised:
```brassica
categories
C = m n p t k b d g f s v z r l w y
V = a e i o u
end

[a e o u] C* i / [ä ë ö ü] C* / _ #

; ai → ä
; oni → ön
; enti → ënt
; astki → ästk
```

A different type of wildcard is provided by the **wildcard**,
  written as `^` before a lexeme (usually a category or grapheme).
This corresponds to an unlimited number of any kind of grapheme,
  until the first point at which the lexeme following the wildcard matches the input.

For example, the following implements a rule of **regressive vowel harmony**.
Two categories of vowels are defined, and vowels change category if the other category is seen anywhere later in the word:
```brassica
categories
C = p t k b d g m n s l
V = a e i o u
V̈ = ä ë ï ö ü
end

V / V̈ / _ ^V̈
V̈ / V / _ ^V

; atebegidë → ätëbëgïdë
; änasëpëlöna → anasepelona
```
([Later](#phonetic-features) we will see how to write this sound change with only one rule.)

Here, the wildcard expresses the desired sound change:
  ‘change the vowel if there is a vowel of this type *anywhere later in the word*’,

Note that the target `_` does not qualify as something that can be modified by wildcards or stars!
That is, you cannot write `_*` or `^_`.
Of course, the former makes no sense, since it would imply matching the target multiple times in an environment.
The latter is easier to imagine, but as of Brassica version 1.0.0, it too is disallowed.

### Multiple results

Sound changes in Brassica can be written to have more than one result.
This can be useful in a few different ways.

The most common situation is that of **sporadic** sound changes.
Such sound changes apply only to a subset of words, with no clear conditioning factor.
In Brassica, you can simulate these using a rule with two outputs:
  one where the rule has been applied, and one where the rule has not been applied.
Create these rules by adding the flag `-?` to the beginning of the rule.

For instance, in the following example,
  the second rule simulates the *meet*–*meat* merger in English,
  which applied to most but not all English words with */eː/:

```brassica
[e i] ː / [eː iː]    ; make multigraphs first
-? eː / iː
eː / ɛi

; miːt → miːt         ‘meet’ (no change)
; meːt → mɛit/miːt    ‘meat’ (Modern English takes second output: underwent merger)
; greːt → grɛit/griːt ‘great’ (Modern English takes first output: did not undergo merger)
```

Otherwise, it is possible to produce multiple results using categories.
If the replacement has categories which cannot be matched up to any target categories,
  Brassica will produce *all possible replacements* simultaneously.
Using another example from English,
  the following sound changes summarise the history of close back vowels from Middle to Modern English:
```brassica
[o u] ː / [oː uː]    ; make multigraphs first
oː / [uː ʊ ʌ]
u / [ʌ ʊ]

; moːðər → muːðər/mʊðər/mʌðər  ; ‘mother’ (Modern English takes third output)
; tuf → tʌf/tʊf                ; ‘tough’ (Modern English takes second output)
```

This behaviour can also be forced by writing `@?` before the category.
For instance, `[i u] / @? [i u]` will output both ⟨i⟩ and ⟨u⟩ for any high vowel in the input.

Similarly, optional lexemes in the replacement will produce two results
  if they cannot be matched with optionals in the target.
In one result, the optional lexemes will be present; in the other, they will be absent.

On occasion, Brassica may produce two results when a rule is ambiguous.
This most often happens with optional lexemes.
For instance, consider the rule `o (m) / u (ŋ)`, applied to the word ⟨tom⟩.
Because the letter ⟨m⟩ is optional,
  Brassica could take the target to include the single letter ⟨o⟩ (replaced with ⟨u⟩),
  or the two letters ⟨om⟩ (replaced with ⟨uŋ⟩).
Thus there will be two output words, ⟨tum⟩ and ⟨tuŋ⟩.

To prevent this situation, you can place a `%` before the optional to form a **greedy optional**.
Such an optional will ‘greedily’ match the largest target possible to produce only a single result, like so:
```brassica
o %(m) / u (ŋ)

; tom → tuŋ
; to → tu
```

If many optional rules are used, the result may contain a large number of output words.
The sound change file may contain **filters** to remove all results matching a certain condition.
For instance, specifying `filter V V` will remove any word which matches `V V`:
  that is, any word with a sequence of two vowels.

(Note that filters will remove words
  even if those words have only a single result — so be careful when using them!)

### Sound changes between words

Sometimes it can be useful to apply sound changes across two or more words at once.
For instance:

- Sandhi and liaison processes act across adjacent words
- Resyllabification can cause a consonant to move across a word boundary
- Cliticisation and affixisation are associated with the deletion of a phonological boundary

To model these processes, you can explicitly connect words together in the lexicon
  with the word-boundary marker `#`:
  for instance, English ‘law and order’ may be `loː#ænd#oːdɐ`.
By default, each element between `#`s undergoes sound changes independently,
  as if it was a separate word.
However, word boundaries can be included in the target or replacement,
  to respectvely merge or split these connected words.
  
For instance, the following rule implements English intrusive /ɹ/,
  by inserting ⟨ɹ⟩ at the beginning of words as appropriate:
```brassica
categories
; only list relevant graphemes
C = l n d ɹ
V = æ ɐ ɑː ə iə oː
end

/ ɹ / [ə iə ɑː oː] # _ V

; loː#ænd#oːdɐ → loː#ɹænd#oːdɐ
```

If you want, you can even remove all interword boundaries in the lexicon with `# / / _`,
  if such a thing is useful.

There is a useful trick to represent **different types of boundary**.
This is to represent boundaries as `# <something> #`,
  where the `<something>` is a different marker for each boundary types.
For instance, you might choose to use `# = #` for clitics, and `# W #` for ordinary word boundaries.
Then, the following rule merges clitics with their hosts, but keeps words separate:

```brassica
# = # / / _

; ə#=#klɪtɪk#=#ɪz#W#mɜːdʒd → əklɪtɪkɪz#W#mɜːdʒd
; bʌt#W#wɜːdz#=#ɑː#W#kɛpt#W#sɛpɹət → bʌt#W#wɜːdzɑː#W#kɛpt#W#sɛpɹət 
```

### Iterative sound changes

Iterative sound changes are ones which apply repeatedly across a word.
Examples include:

- Rhythmic stress assignment
- Spreading and harmony processes
- Alternating dissimilation

In Brassica, the basic principle of iterative sound changes is that
  **targets never overlap with each other**.
However, environments may overlap with targets, as well as with themselves.

Starting with a simple case, consider a rule of trochaic **stress assignment**:
  that is, a rule which stresses every first syllable.
Representing stressed vowels with an acute accent,
  this can be stated as follows:
```brassica
categories
C = p t k b d g
Vu = a e i o u
Vs = á é í ó ú
end

C* Vu C* Vu C* / C* Vs C* Vu C*

; pat → pat
; patbek → pátbek
; patbeko → pátbeko
; patbekoda → pátbekóda
; patbekodadet → pátbekódadet
```

Considering the last listed example ⟨patbekodadet⟩, the rule is iteratively applied as follows:

1. The target first applies at the beginning of the word, matching ⟨patbek⟩.
   The replacement specifies that this should be replaced with ⟨pátbek⟩.
   The word is now ⟨pátbekodadet⟩.
2. Because targets never overlap with each other, the target next matches ⟨odad⟩.
   This is replaced with ⟨ódad⟩.
   The word is now ⟨pátbekódadet⟩.
3. The remaining graphemes left in the word are ⟨et⟩.
   The target cannot match this material, so the sound change is finished.

Note that the last syllable in this word remains unstressed,
  because the target is defined to match bisyllabic feet only.
Similarly, the monosyllabic word ⟨pat⟩ is not stressed.
If this is undesired, the rule can be changed to match monosyllabic feet, as `C* Vu C* %(Vu C*) / C* Vs C* (Vu C*)`.
(A [greedy optional](#multiple-results) is necessary to make the target unambiguous.)

(Alternatively, a rule can be added to stress monosyllabic words only,
  for instance `Vu / Vs / # C* _ C* #`.)

The above rule naturally pauses and resumes stress assignment
  if any stressed vowels are present in the input.
For instance, consider the word ⟨patbekódadet⟩:

1. As before, the target first matches ⟨patbek⟩, yielding ⟨pátbekódadet⟩.
2. The remaining material is ⟨kódadet⟩.
   The first vowel cannot be included in the target,
     because the target requires two unstressed syllables.
   Therefore the target can only match ⟨dadet⟩,
     yielding the final word ⟨pátbekódádet⟩.
     
This can be used to implement stress rules where e.g. certain feet are always stressed.
 
For a second example, consider a rule of progressive, unbounded **vowel harmony**.
If the two harmonic sets are `[a e i o u]` and `[ä ë ï ö ü]`,
  this can be implemented as follows:
  
```brassica
categories
C = p t k b d g m n ŋ f s h
V = a e i o u
V̈ = ä ë ï ö ü
end

V̈ / V / V C* _
V / V̈ / V̈ C* _

; atemïnösë → ateminose
; ätemïnösë → ätëmïnösë
```

[Later](#phonetic-features) we will see how to write this sound change with only one rule.

Considering the first example listed above (namely ⟨atemïnosë⟩),
  the first rule applies to it as follows:

1. The target first matches ⟨ï⟩, after environment ⟨em⟩.
   This target is replaced with ⟨i⟩.
   The word is now ⟨ateminösë⟩.
2. The new graphemes ⟨in⟩ can now form the environment for the next target ⟨ö⟩,
     allowing it to be replaced with ⟨o⟩.
   The word is now ⟨ateminosë⟩.
3. Similarly, the new graphemes ⟨os⟩ form the environment for the next target ⟨ë⟩,
     to be replaced with ⟨e⟩.
   The final word is ⟨ateminose⟩.
   
Observe how this works:
  as each non-harmonic vowel is replaced with a harmonic vowel,
  it forms a new environment from which the sound change application can be continued.

Not all iterative sound changes are applied left-to-right:
  some require **right-to-left** application.
To achieve this, add the `-rtl` flag at the beginning of a rule.
(For completeness, `-ltr` is also provided to explicitly specify the default left-to-right direction.)
Thus, compare the following rule of right-to-left trochaic stress assignment
  to the previous example of left-to-right stress assignment:
  
```brassica
categories
C = p t k b d g
Vu = a e i o u
Vs = á é í ó ú
end

-rtl C* Vu C* Vu C* / C* Vs C* Vu C*

; pat → pat
; patbek → pátbek
; patbeko → patbéko
; patbekoda → pátbekóda
; patbekodadet → patbékodádet
```

In right-to-left application, **all parts of sound change application are reversed**.
Matches start at the right edge of the word, and continue to the left edge.
Within each rule part, too, lexemes are interpreted from right to left.
This affects category matching, such that
  e.g. `-rtl [a b] [x y] / [X Y]` will yield ⟨bx⟩→⟨X⟩, rather than ⟨bx⟩→⟨Y⟩.
It also affects wildcards:
  for instance, reading the environment from right to left,
  `-rtl V̈ / V / ^V _` replaces `V̈` by `V` when preceded by `V` anywhere before it in the word.
(This gives an alternate way to implement vowel harmony.)

Finally, it can be useful to do the opposite of iterative sound changes:
  to apply a sound change **once only** within a word.
This is done using the flag `-1`.
For instance, the following rules together assign primary stress
  to the rightmost long vowel in a word if there is one,
  or to the leftmost vowel otherwise:
```brassica
categories
C = p t k b d g
Vu = a i u aa ii uu
Vs = á í ú áá íí úú
end

-rtl -1 [aa ii uu] / [áá íí úú]
-1 Vu / Vs
Vs / Vu / _ ^Vs   ; destress all except rightmost of conflicting stress assignments

; katigupa → kátigupa
; katiigupa → katíígupa
; katiigupaa → katiigupáá
```

### Debugging

It can sometimes be difficult to find and fix misbehaving sound changes.
Brassica provides a few tools to help with this task.

Firstly, when dealing with a large set of sound changes,
  it can be useful to see **intermediate results**.
There are two ways to do this:

- To see results at a specific point between sound changes,
    write the word `report` on a line of its own (or multiple times at different points).
  Then, if you select the output format ‘Input→output’,
    the output word at each `report` point will be presented alongside the final output.
- To see the effect of all sound changes at once,
    select the button to ‘Report rules applied’.
  This will show the evolution of every word in the input as it evolves through every sound change.

For more on these, see the guide to [Using Brassica](./Using-Brassica.md).

It can be more difficult to debug single sound changes which are misbehaving.
However, there are some tricks you can use:

- Adding `/ . / _` inserts ⟨.⟩ in every possible position between and around graphemes.
  This more clearly shows the graphemes making up each word,
    and makes it obvious if unexpected multigraphs are present
    (as mentioned [above](#multigraphs)).
- To see the elements of a category, you can write `. / Category`,
    then include ⟨.⟩ as an input word.
  The output for that word will be a list of results, one for each element of `Category` in order.
  This is useful for diagnosing problems with categories which have unexpected elements,
    or with elements in an unexpected order.
    
(Of course, you can substitute any convenient grapheme for ⟨.⟩ above.)

Beyond these, the best strategy to fix problematic rules
  is to test different input words until the problem becomes clear.

## Phonetic features

Very many sound changes involve a change in some phonetic feature,
  such that phonemes change in one or two features while maintaining others.
For instance, voiceless phonemes might become voiced in certain environments,
  or vowels might attain high or low tone.

Brassica has several tools which work together to represent such phonetic features:

- Lists of graphemes can be specified by combining categories with different operations
- Features can be defined as sets of opposing categories, then transferred between graphemes
- Features can be declared ‘autosegmental’ to automatically transfer them from target to replacement

The following sections explain how these work.

### Combining categories

[Earlier](#predefining-categories) it was mentioned
  that predefined categories can be combined by writing them together in a rule.
This can be seen, for instance, by using the category-printing trick mentioned [above](#debugging):

```brassica
extra .

categories
Nasal = m n ŋ
Stop = p t k b d g
Fric = f s v z
Approx = r l w y
C = Nasal Stop Fric Approx
end

. / C

; . → m/n/ŋ/p/t/k/b/d/g/f/s/v/z/r/l/w/y
```

However, there is more than one way to combine categories.
Brassica provides three different **category operations**,
  distinguished by an operator placed before the category name:

- `[First +Second]` is **intersection**:
    the result contains all elements of `Second` (in order) which are also elements of `First`.
- `[First -Second]` is **subtraction**:
    the result contains all elements of `First` (in order), except for those which are elements of `Second`.
    - Subtraction can also remove a single grapheme from a category: `[First -grapheme]`.
- `[First Second]` is **union**: all elements of `First` followed by all elements of `Second`.
  For completeness, this can also be written with an operator as `[First &Second]`
    (which will become useful later with [featural categories](#featural-categories)).

By judiciously combining categories, many sound changes become easier to write and read.
For instance, the following rule changes voiceless stops to voiced ones intervocalically:

```brassica
categories
Nasal = m n ŋ
Stop = p t k b d g
Fric = f s v z
Approx = r l w y
C = Nasal Stop Fric Approx

Voice = m n ŋ b d g v z r l w y

V = a e i o u
end

[Stop -Voice] / [Stop +Voice] / V _ V

; atepke → adepke
; tapagu → tabagu
```

In order to work correctly,
  the categories in such rules must contain corresponding graphemes in the same order.
In this case, `[Stop -Voice]` expands out to `[p t k]`,
  and similarly `[Stop +Voice]` expands to `[b d g]`,
  so they match and the rule works as expected.
More care would be needed if, say, `Stop` were to contain `[t k b d g]`:
  then the rule would become `[t k] / [b d g] / V _ V` and behave unexpectedly.
([Featural categories](#featural-categories) are designed to make such problems easier to avoid.)

For another example, consider a rule of progressive nasal harmony
  in which voiced stops become nasal following a nasal consonant,
  until interrupted by ⟨h⟩.
This sound change can be implemented as follows:

```brassca
categories
C = p t k b d g m n ŋ f s h
Nasl = m n ŋ
VStp = b d g

V = a e i o u
end

VStp / Nasl / Nasl [C V -h]* _

; mate → mate (no change)
; matebede → matemene
; matebehede → matenehede
```

Here, `[C V -h]*` matches an indefinite number of consonants or vowels — except ⟨h⟩ —
  between a nasal and a following voiced stop.
Thus, the voiced stop is nasalised only when ⟨h⟩ does not intervene between it and the nasal,
  giving the desired behaviour.

### Featural categories

Often it is necessary to define sets of related sounds.
For instance, in the last section, the first example used a category `Voice`
  to define opposing sets of unvoiced and voiced sounds.

To make this easier, Brassica lets you define **features**
  by creating categories with specific names, as follows:

- A **binary feature** can be defined by listing two categories named `-Feature` and `+Feature`.
  (`Feature` can be replaced with any feature name you want.)
  The **feature values** are called `-` and `+`.
- A **multivalent feature** can be defined by listing two or more categories
    of the form `+Feature+Value`.
  `Feature` is a single feature name shared by all categories comprising the feature,
    while `Value` is different for each category.
  Each `Value` names a different feature value.

For the most part, these categories behave as ordinary categories.
For instance, the following rule devoices voiced consonants word-finally:
```brassica
categories
C = p t k f s b d g v z

-Voice = p t k f s
+Voice = b d g v z

V = a e i o u
end

+Voice / -Voice / _ C* #

; apat → apat (no change)
; gubeb → gubep
; tavz → tafs
```

Observe the structure of the feature definition in the previous example.
Each category making up the ‘Voice’ feature has **the same number of elements**.
This is a key requirement for all feature definitions,
  and ensures that the graphemes within them can be paired up logically:
  ⟨p⟩ with ⟨b⟩, ⟨t⟩ with ⟨d⟩, and so on.

What happens to a feature when some graphemes cannot be paired up in this manner?
For instance, if ⟨m⟩ and ⟨n⟩ were added to `C` in the previous example,
  they would be voiced consonants with no voiceless counterpart.
There are several possible ways to deal with this situation:

1. Corresponding voiceless graphemes ⟨m̥⟩ and ⟨n̥⟩ could be added to the list of consonants.
   Later rules could specify, say, `[+Voice -m -n]` to exclude the undesired graphemes.
2. Rather than adding new phonemes, some special grapheme (say, ⟨?⟩) could be listed
     as the `-Voice` counterparts of ⟨m⟩ and ⟨n⟩.
   Again later rules will need to specify categories such as `[+Voice -m -n]`,
     but it will be more obvious if any mistake is made.
3. Instead of *adding* new graphemes to `-Voice`,
     instead ⟨m n⟩ could be *removed* from `+Voice`.
   This means that `+Voice` would no longer match all voiced phonemes;
     on the other hand, `-Voice` and `+Voice` now contain only phonemes which exist and have voiced/voiceless counterparts.

There is no single best choice of methods here.
It depends on which trade-offs make a given set of sound changes easiest to write.
(However, the author suggests that method (2) may be most widely useful.)

For another example, the following set of rules define three levels of stress for vowels,
  with the exception of /ə ɨ/ which cannot receive primary stress.
Primary stress is assigned to the rightmost possible vowel, and trochaic feet are constructed before it.
Method (2) above is used to deal with the primary-stressed correspondent for ⟨ə ɨ⟩:
```brassica
categories
C = m n p t k b d g f s v z r l y w

+Str+None = a e i o u ə ɨ
+Str+Sec  = à è ì ò ù ə̀ ɨ̀
+Str+Pri  = á é í ó ú ? ?

V = +Str+None &+Str+Sec &+Str+Pri
; or V = &&Str - see below
end

-rtl -1 [+Str+None -ə -ɨ] / +Str+Pri
-rtl C* +Str+None C* +Str+None C* / C* +Str+Sec C* +Str+None C* / _ // ^+Str+Pri _

; dukɨtipəta → dùkɨtìpətá
; dukɨtipetə → dukɨ̀tipétə
; dukɨtipətə → dùkɨtípətə
```

The previous examples contained some examples of [category combining](#combining-categories).
This syntax is one of the ways in which featural categories differ from ordinary categories, as follows:

- Ordinarily, `[Category +Feature]` and `[Category -Feature]`
    would be interpreted as (respectively) intersection and union with a category named `Feature`.
  But if the appropriate featural categories are defined,
    the former is instead treated as an intersection with `+Feature`,
    and the latter as an intersection with `-Feature`.
- The former can also be written using the usual syntax as `[Category ++Feature]` and `[Category +-Feature]`:
  Similarly, union and subtraction with a featural category are written as, respectively,
    `[Category &+Feature]` and `[Category -+Feature]`.
- After a category operator, the special syntax `&Feature` means ‘all graphemes in `-Feature` and `+Feature`’
    (for binary features only).
  Thus, `[&&Feature]` contains all graphemes listed with a setting for ‘Feature’,
    and similarly for `[Category +&Feature]` and `[Category -&Feature]`.

### Feature syntax

Featural categories like those in the previous section
  form the basis for **feature syntax**.
In its simplest form, a feature in a sound change is written `something$Feature`.
Here `something` can be any lexeme, but will most often be a category,
  while `Feature` is the name of a previously defined feature.
  
When encountered in a sound change rule, `something$Feature` acts almost like a ‘copy–paste’ operation.
In the target, it inspects the last grapheme matched by `something`,
  and records its value with respect to that ‘Feature’
  (e.g. for a binary ±Voiced feature, whether it is in `-Voiced` or `+Voiced`).
Then, in the replacement, a corresponding `somethingelse$Feature` will retrieve that feature value,
  and re-apply it to the last grapheme of `somethingelse`,
  such that the result has the same feature value as the original `something`.
  
Therefore, feature syntax works by pairing up occurrences in the target and replacement,
  similarly to categories.
Each `$Feature` in the target corresponds to a `$Feature` in the replacement,
  reading from left to right (or right to left for `-rtl` rules).

This may be easiest to understand via example.
The following rule implements a sound change of regressive **voicing assimilation** in clusters:

```brassica
categories
C = m n p t k f s b d g v z

-Voice = p t k f s ? ?
+Voice = b d g v z m n

V = a e i o u
end

C C$Voice / C$Voice C

; apgedse → abgetse
; boknaint → bognai?t
```

This sound change works as follows:

1. The target matches any consonant `C` followed by any other consonant `C`.
   The voicing status `$Voice` of the second consonant is recorded.
2. The two target consonants are replaced by the first consonant `C` followed by the second consonant `C`.
   The `$Voice` saved in the target is re-applied to the second consonant,
     causing it to take on the voicing value of the original second consonant.

The second example word above contained the cluster ⟨nt⟩.
The rule as specified thus attempts to devoice ⟨n⟩.
This is impossible within this phonology,
  so instead the rule inserts ⟨?⟩ as specified in the definition of `-Voice`.

What happens if the relevant grapheme is in neither `-Voice` nor `+Voice`?
Such a grapheme is called **indeterminate** with respect to ±Voice.
In the target, it will create multiple results, one for each feature value (so two in this case).
In the replacement, it will remain the same no matter what value is applied to it.

Features can also use **backreferences**.
In this case they are written as `$Feature#identifier`.
They behave similarly to [category backreferences](#backreferences):
  across the target, replacement and environment, every feature with the same backreference
  must take on the same value.
The following example uses backreferences to implement a rule of vowel harmony,
  where vowels agree in ±Back with the first vowel in the word:

```brassica
categories
C = m n p t k f s b d g v z

-Back = ä ö ü
+Back = a o u

Active = &&Back
Neutral = e i
V = Active Neutral
end


Active / Active$Back#harmony / Active$Back#harmony [C Neutral]* _

; pezöman → pezömän
; tanetiküvä → tanetikuva
```

In the previous rule, `Active` vowels receive the same `Back`ness value
  as the last `Active` vowel matched in the environment.
The `#harmony` identifier is used to link the two feature references together.
(Note that any name could have been used for the identifier:
  this author thinks that `#harmony` reads well.)
  
Finally, feature matching can be **negated** in the replacement by adding a `-` after the `$`.
For instance, consider a case where vowel tone is produced from consonant voicing:
  in this case, low tone (`-High`) should be produced from voiced consonants (`+Voice`) and *vice versa*.
This can be written as follows:

```brassica
categories
C = m n p t k f s b d g v z

-Voice = p t k f s ? ?
+Voice = b d g v z m n

-High = a e i o u
+High = á é í ó ú
V = &&High
end

V / V$-High#voice / C$Voice#voice _

; akageme → akágeme
; sunasopke → súnasópké
```

### Autosegmental features

Often a feature behaves *autosegmentally*,
  undergoing sound changes independant of the segment to which it is attached.
Common examples include stress and tone.
Although Brassica does not have true autosegmental tiers,
  it can invoke the feature system to make such sound changes easier to write.
  
This is perhaps best explained by example.
Consider the following:

```brassica
categories
C = m n p t k f s b d g v z w y

+Tone+Low = à è ì ò ù
+Tone+Mid = a e i o u
+Tone+High = á é í ó ú

auto +Tone+Mid

V = a e i o u
end

e / i / V _

; toéna → toína
; mákòesi → mákòisi
; taetèp → taitèp
; púèza → púìza
```

The key line here is `auto +Tone+Mid`.
This declares the ‘Tone’ feature as autosegmental,
  and redefines the five graphemes listed in `+Tone+Mid` as being **autosegmental graphemes**.
Taking ⟨e⟩ as an example, those graphemes henceforth behave as follows:

- In the target (and replacement and exception), a mention of `e`
    can correspond to any of ⟨e è é⟩.
  It also saves the ‘Tone’ value of the grapheme it matched,
    exactly as if it had been followed by an explicit feature `$Tone`.
- In the replacement, it looks at the corresponding feature value for ‘Tone’ in the target
    (again behaving like `$Tone`).
  It then inserts the corresponding grapheme from ⟨e è é⟩ into the output word.

Thus, in the sound change `e / i / V _`, the first `e` can match any of ⟨e è é⟩,
  and similarly with ⟨i ì í⟩.
The correct output grapheme is determined by matching the feature ‘Tone’, as described above.
The net effect is as if tone exists on its own tier, independent of the change in the vowel phoneme.

As for `V`, it was defined as `[a e i o u]` *after* the autosegment declaration.
Thus, `V` includes ⟨à⟩, ⟨á⟩, ⟨è⟩, ⟨é⟩, etc., in the same way that `e` includes ⟨è⟩ and ⟨é⟩.
And feature matching works here too:
  thus `V / a` would successfully convert ⟨tíkù⟩ to ⟨tákà⟩,
  neutralising the vowels while preserving the feature of ‘Tone’.

(Note that declaring `V = &&Tone` as in previous examples would *not* have this effect.
The various `+Tone` categories were declared before `auto +Tone+Mid` was specified,
  so they do not have any autosegmental behaviour.
`+Tone+Mid` contains only the five graphemes ⟨a e i o u⟩, and nothing more.)

Some care is needed when using autosegmental graphemes in the replacement.
If there is no corresponding feature in the target for them to use,
  they will produce all possible results at once (as mentioned in the previous section).
For instance, continuing the previous example,
  `y / i / C _ #` applied to ⟨katy⟩ would produce the three results ⟨kati katì katí⟩,
  because Brassica has no way to know what tone the output should be.
To expliticly specify the grapheme itself, write it with a following tilde:
  thus `y / i~ / C _ #` yields ⟨katy⟩→⟨kati⟩.
(The other possibilities `y / í` and `y / ì` work as you would expect.)

Similar issues arise when using multiple autosegments in the target and replacement,
  as in the following example:

```brassica
categories
C = m n p t k f s b d g v z w y

-High = a e i o u
+High = á é í ó ú

auto -High

V = a e i o u
end

[i u] a / [y w] e

; kiana → kyena
; súatep → swétep
; piáb → pyeb
```

Here, both `[i u]` and `a` are autosegmental graphemes with respect to ±High.
However, in the replacement, only `e` behaves autosegmentally:
  `y` and `w` are both consonants, and as such cannot take tone.
Since features are matched up from left to right,
  `e` therefore receives tone from `[i u]`.

If this behaviour is undesired, it can be changed in two ways.
One way is to run the rule [right-to-left](#iterative-sound-changes),
  thereby causing features to be associated right-to-left as well.
But a more flexible solution is to use explicit feature backreferences:
  `[i u] a$High#a / [y w] e$High#a` (taking `a` as the identifier to use).

Of course, one might want the result to have high tone when *either* of the input graphemes have high tone.
As of Brassica version 1.0.0, this is not possible in a single rule,
  but it can be accomplished by transfering high tone to the first vowel beforehand:

```brassica
categories
C = m n p t k f s b d g v z w y

-High = a e i o u
+High = á é í ó ú

auto -High

V = a e i o u
end

[i~ u~] / [í ú] / _ á
[i u] a / [y w] e

; kiana → kyena
; súatep → swétep
; piáb → pyéb
```

A final note: **at most one feature** can be defined as autosegmental for any given grapheme.
Thus, if a language has e.g. both stress and tone,
  they would need to be combined into a single feature to be treated as simultaneously autosegmental.
Fortunately, such cases are rare.
