<!-- -*-GFM-*- -->

# Brassica: getting started

## What is Brassica?

Brassica is a **sound change applier**:
  a computer program which can simulate the evolution of words as they are affected by sound changes.
Brassica provides
  a consistent format to specify sound changes,
  a mechanism for applying those changes 
  
You can use Brassica via three different interfaces:
- The **graphical interface** (online at https://bradrn.com/brassica or available as a standalone desktop program),
    letting you interactively write and apply sound changes.
- The **command-line interface** (or ‘CLI’, available as a standalone program)
    supports batch processing of dictionaries and other large files.
- The **library** which powers Brassica (available [on Hackage](https://hackage.haskell.org/package/brassica))
    can be used independently or integrated into other software.

This guide will focus on the graphical interface.
However, all these interfaces use the same sound change syntax and apply it in the same way.

(Note that help for the CLI is available with the command-line option `--help`,
  and help for the library is available at the Hackage link.)
  
Brassica also contains a **paradigm builder** to build families of words.
See the end of this document for a [guide to the paradigm builder](#paradigm-builder).

## Writing sound changes

### Basic rules

The simplest sound change rules look like this:
```
target / replacement
```
This rule will replace every instance of the `target` by the `replacement`.

In the simplest rules, both the target and the replacement are a single letter (or **grapheme**).
For instance, a rule to unconditionally change ⟨θ⟩ to ⟨f⟩ (as found in some English dialects) can be specified as follows:
```
θ / f
```
This rule will change, say, ⟨θɪn⟩ to ⟨fɪn⟩.

> **Note:** the first slash in a rule can be replaced by `→` or `->` if you prefer.

The target and replacement can contain multiple graphemes.
However, **they must have spaces between them**.
(This is important! See the section below on [multigraphs](#multigraphs) for details.)
For instance, the following rule changes ⟨ki⟩ to ⟨č⟩:
```
k i / č
```

Most sound changes, however, are not unconditional.
Instead, they take place only in a specific **environment**.
Brassica can represent such sound changes as follows:
```
target / replacement / before _ after
```
This rule will replace `target` by `replacement` only when preceded by `before` and followed by `after`.
Note that `before` or `after` or both may be absent.
  
The simplest environment is a single underscore.
(That is, an environment where neither `before` nor `after` is present.)
This is exactly the same as leaving the environment out altogether:
  it constrains neither `before` nor `after`.

For a more interesting example, the following rule:
```
t / s / i _ i
```
…will replace ⟨t⟩ with ⟨s⟩ only when it’s between two ⟨i⟩s.

**Word boundaries** may be specified in the environment as `#`.
For instance, this rule changes ⟨n⟩ to ⟨ŋ⟩ at the end of a word:
```
n / ŋ / _ #
```

Either the target or the replacement may be left blank.
If the replacement is blank, the target is **deleted** (i.e. replaced with nothing) in every position where the environment holds.
For instance, this rule deletes word-final ⟨i⟩:
```
i / / _ #
```
This can be used to specify sound changes of **syncope**, **apocope**, **elision** and so on.

On the other hand, if the target is blank, the replacement is inserted in every position where the environment holds.
For instance, this rule inserts ⟨ʔ⟩ between any two adjacent ⟨a⟩s:
```
/ ʔ / a _ a
```
This can be used to implement rules of **epenthesis**.

Plain-text **comments** can be added by beginning them with a semicolon.
Brassica will ignore any text after the semicolon.
This can be useful for explaining what a complex rule does, for instance:
```
; replace i with y before a
i / y / _ a
```
In this example the comment is on its own line.
A comment may also be added after a rule if necessary:
```
i / y / _ a    ; replace i with y before a
```

A rule may take place in any of **multiple environments**.
Specify this by adding more slashes to the end of the rule:
```
target / replacement / before1 _ after1 / before2 _ after2 / …
```
For instance, the following rule will delete ⟨ʔ⟩ when it occurs at the beginning or at the end of a word:
```
ʔ / / _ # / # _
```

### Categories

Many sound changes affect several different graphemes in similar ways.
Brassica handles these using **categories**.
In a rule, a category will match any of the graphemes listed within it.

Specify a category by listing a set of graphemes between square brackets.
For instance, `[a e i o u]` will match any of ⟨a⟩, ⟨e⟩, ⟨i⟩, ⟨o⟩ or ⟨u⟩.
This means the following rule will insert ⟨ʔ⟩ between any two vowels (assuming a five-vowel system):
```
/ ʔ / [a e i o u] _ [a e i o u]
```
And this rule will delete any consonant occurring immediately before another consonant:
```
[b c d f g h j k l m n p q r s t v w x y z] / / _ [b c d f g h j k l m n p q r s t v w x y z]
```
(We will soon see [how to make these rules easier to write](#defining-categories).)


Categories become particularly useful when converting between two sets of graphemes.
For instance, we might want to convert voiced stops to nasals when word-final:
```
b / m / _ #
d / n / _ #
ɡ / ŋ / _ #
```
Obviously this gets tedious for long sets of graphemes!

Brassica simplifies these cases by allowing categories in the replacement, like so:
```
[b d ɡ] / [m n ŋ] / _ #
```
Here, the category in the replacement takes its value from the category in the target.
The listed graphemes are matched up one-to-one:
  the first grapheme listed in the target category will be replaced by the first in the replacement category,
  the second grapheme in the target will be replaced by the second in the replacement,
  and so on.

In the environment, categories can include **word boundaries**.
For instance, the following rule works to delete ⟨ʔ⟩ before a stop or the end of a word:
```
ʔ / / _ [p t k b d g #]
```

Sometimes it’s useful to match or replace a **sequence** of zero or more graphemes within a category.
Do this by surrounding those graphemes with curly braces `{`/`}`, as in:
```
[e o] / [{j ə} {w ə}]
[{ŋ g} ŋ] → [ŋ {}]
```
The first rule here will replace ⟨e⟩ by ⟨jə⟩ and ⟨o⟩ by ⟨wə⟩.
The second rule acts to change ⟨ŋg⟩ to ⟨ŋ⟩, while deleting any single ⟨ŋ⟩.

> **Note:** this second rule would be difficult to write without using braces!
> If it had been written as two rules in the following order:
> ```
> ŋ g / ŋ
> ŋ /
> ```
> ⟨ŋg⟩ would be changed to ⟨ŋ⟩, only for that to be deleted by the second rule.
>
> If the other order had been chosen:
> ```
> ŋ /
> ŋ g / ŋ
> ```
> Then all instances of ⟨ŋ⟩ would be deleted by the first rule, such that the second rule could never apply.

You can also use **more than one category** at a time.
In this case each category in the replacement is matched with a category in the target:
  the first in the replacement with the first in the target,
  the second in the replacement with the second in the target,
  and so on.
For instance, the following (rather contrived) rule will replace stop–nasal sequences with nasal–stop sequences,
  preserving places of articulation (e.g. ⟨km⟩ → ⟨ŋp⟩):
```
[p t k] [m n ŋ] / [m n ŋ] [p t k]
```

In more complex cases it is convenient to **ignore** a category in the target.
For instance, consider a rule of monophthongisation
  in which the first vowel deletes and the second centralises.
(For instance, ⟨ai⟩ would turn into ⟨ɨ⟩.)
We might attempt to write this as follows:
```
[a e i o u] [a e i o u] / [ɐ ə ɨ ə ɨ]
```
But this will not work as expected:
  the *first* category in the input corresponds to `[ɐ ə ɨ ə ɨ]` in the output,
  leaving the second category to be deleted.
This can be resolved using the special **discard** symbol `~` in the replacement,
  which corresponds to one category in the input, but produces no output:
```
[a e i o u] [a e i o u] / ~ [ɐ ə ɨ ə ɨ]
```
One could read this rune in English as:
  ‘delete the first vowel, and centralise the second vowel’ — exactly what we want!


### Defining categories

It is annoying to specify the same categories over and over again.
Thus Brassica allows one to **give names to predefined categories**.

To define a category, specify it in a **category block** before any rule which uses it:
```
categories
C = p t k m n f s x r w y
V = a e i o u
end
```
After this definition,
  any reference to `C` will act the same as `[p t k m n f s x r w y]`,
  and any reference to `V` will act the same as `[a e i o u]`.
For instance, `/ ʔ / V _ V` will insert a glottal stop between adjacent vowels.

Often it’s convenient to build up bigger categories from smaller ones.
Do this by listing category names within the definition of another category:
```
categories
Nasl = m n ŋ
Stop = p t k
Fric = f s x
Approx = w l y
C = Nasl Stop Fric Approx
```
The last line has the same effect as writing `C = m n ŋ p t k f s x w l y`.

(Incidentally, this shows that category names can be multiple letters.
More on this [below](#multigraphs).)

In normal usage, the named categories should cover all the phonemes in the relevant languages and sound changes.
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

; various sound changes, altering the vowel inventory

categories
V = a e ŏ o i ŭ u
end
```
In the above example, the later definition of `V` replaces the earlier definition of `V`.
If the output from the sound changes includes a sound outside the expected final inventory,
  the second category block will cause the unexpected sound to be replaced by �,
  immediately highlighting the mistake.

In some cases the phonology will have changed so dramatically
  that it’s easier to **replace all the categories at once**.
Do this by introducing the category block with `new categories` rather than just `categories`.
This will remove all the pre-existing category assignments.

Furthermore, don’t forget that you can write rules before the first category definition!
This is a good place to do operations such as **deromanisation**.
Otherwise, Brassica would replace any unknown graphemes in the romanisation with the replacement character �.

On occasion, you might want to define some **special characters** which don’t fit into any category.
You can specify this by listing them after the special directive `extra`, before the first category definition.
This will indicate to Brassica that it should never replace these characters by �.
Thus, for instance, some sound changes which make used of morpheme and clitic boundaries might specify:
```
extra - =
```

Finally, if you dislike this behaviour of replacing unknown graphemes by �,
  you can add the instruction `noreplace` after `categories`.
(This can still be combined with `new` before `categories`.)
In this case Brassica will simply leave unknown graphemes unchanged.
Note that this is not recommended, as it makes mistakes harder to find!

> **Note:** For the remainder of this guide, I will assume that `C` and `V` have been defined
>   as the set of all consonants and all vowels respectively.

### Multigraphs

So far, all of the examples have contained graphemes consisting of only one letter each.
But it’s often useful to use **multigraphs** such as ⟨ng⟩, ⟨th⟩, ⟨aa⟩ or ⟨eqh⟩.
Although these are composed of multiple letters,
  each represents a single sound and should be treated as a single unit.

Brassica understands that letters with no intervening space should be considered multigraphs.
For instance, the following rule will convert any ⟨t⟩+⟨h⟩ sequences in the input into a single multigraph ⟨th⟩:
```
t h / th
```
Note that Brassica treats a multigraph like ⟨th⟩ as its own grapheme,
  distinct from a sequence of two graphemes ⟨t⟩+⟨h⟩.
Thus rules such as `h / / _` will not affect ⟨th⟩,
  while rules such as `th / s` will not affect sequences of ⟨t⟩+⟨h⟩.
Similarly, rules involving categories will treat multigraphs as single elements:
```
C C / ~ C / _   ; will convert ⟨athke⟩→⟨ake⟩, no change in ⟨athe⟩→⟨athe⟩
```

> **Note:** in fact, the multi-letter category names seen above are considered multigraphs by Brassica.
> Any valid grapheme may be used as a category name.

Incidentally, **be careful to include spaces** between graphemes which are *not* multigraphs!
If you accidentally write a rule such as `ā / ay`, it will produce odd results, since Brassica will interpret the replacement ⟨ay⟩ as a single grapheme.
Use `ā / a y` instead to avoid this issue.

When no categories are defined, Brassica never groups letters together into multigraphs in input words.
For instance, ⟨bath⟩ becomes `b`+`a`+`t`+`h`, and ⟨enga⟩ becomes `e`+`n`+`g`+`a`.
In this situation it is possible to explicitly introduce multigraphs in sound changes, for instance:
```
t h / th
n g / ng
```

However, when categories are defined, Brassica will assume that the **first category block** includes all multigraphs which may be present in the input.
Thus multigraphs will be recognised in the input *only if* those multigraphs are listed in the first category block.
Because Brassica encourages a style where each category block contains every grapheme used at that point in rule application,
  this will usually result in multigraphs being created correctly.

Some languages have a romanisation in which a multigraph and the corresponding letter sequence are both used for different consonants.
For instance, some Australian languages use ⟨ng⟩ for /ŋ/ but ⟨n⟩+⟨g⟩ for /nɡ/.
Often the latter is represented ⟨n’g⟩ or ⟨n.g⟩ in text, as seen for instance in the language name Ngan’gityemerri /ŋanɡicemeri/.
Such a situation can be dealt with in Brassica using a rule which removes the separator, such as `’ / / _`.
A sequence such as ⟨n’g⟩ will then be tokenised to `n`+`’`+`g`, after which this rule will remove the separator leaving `n`+`g` as desired.
By contrast, a sequence such as ⟨ng⟩ will be straightforwardly tokenised to the multigraph `ng`.

### Miscellaneous useful elements

The symbol `>` represents **gemination**.
When used in the target or environment, it matches exactly the same grapheme as the last one matched:
```
[b d g] > / [p t k]   ; converts ⟨bb⟩→⟨p⟩, ⟨dd⟩→⟨t⟩, ⟨gg⟩→⟨k⟩
V / / C > _ #         ; converts ⟨amme⟩→⟨amm⟩, ⟨kappa⟩→⟨kapp⟩
C / / _ >             ; converts ⟨amme⟩→⟨ame⟩,  ⟨kappa⟩→⟨kapa⟩
```
When used in the replacement, it repeats the previous grapheme:
```
C / C > / _ #         ; converts ⟨ap⟩→⟨app⟩
```

**Metathesis** can be accomplished by specifying `\` in the replacement.
This will reverse the graphemes in the target:
```
Aprx Stop / \         ; converts ⟨ayke⟩→⟨akye⟩, ⟨nawbe⟩→⟨nabwe⟩
```

One or more elements in the target or environment can be made **optional** by surrounding them with parentheses:
```
a / e / _ (C) i       ; converts ⟨ai⟩→⟨ei⟩, ⟨ami⟩→⟨emi⟩
Alv (y) i / Pal ə / _ ; converts ⟨ti⟩→⟨čə⟩, ⟨nyi⟩→⟨ñə⟩
```
Optional elements can also be included in the replacement.
Like categories, these are matched up one-to-one with optional elements in the target.
An optional element in the replacement will be included in the output
  only if the corresponding element in the target was matched:
```
Alv (y) i / Pal (i) ə / _   ; converts ⟨ti⟩→⟨čə⟩, ⟨nyi⟩→⟨ñjə⟩
```
Warning: be careful when including optional categories in the environment!
In a rule like `Alv (Aprx) [i e] / Alv [ɨ ə]`,
  the category `[ɨ ə]` in the replacement will correspond to the *optional* category `Aprx` when the latter is matched.
To avoid this, use the discard character: `Alv (Aprx) [i e] / Alv (~) [ɨ ə]`.

Sometimes, you may want to prevent a rule from applying in an exceptional situation.
Do this by adding an **exception** to the rule, with the following syntax:
```
target / replacement / environments… // exception
```
The `exception` has the same form as the other environments,
  with an underscore to represent the target.
For instance, to change ⟨k⟩ to ⟨g⟩ in all situations except at the beginning of a word, one can write:
```
k / g / _ // # _
```
Note that there is no space between the double slashes `//` which mark an exception.

To control various aspects of rule application, one can place **flags** before the rule, separated by a space.
Available flags include `-x` to suppress rule highlighting,
  `-?` to apply rules sporadically and `-rtl` to apply rules right-to-left.
Flags will be described below in the relevant sections.

### Sporadic rules and multiple results

Sometimes we might want a sound change rule where more than one output is possible.
Brassica provides several means to simulate these.

Probably the most common situation is that of **sporadic** rules,
  which unpredictably apply to some words but not others.
Mark these in Brassica by placing the flag element `-?` at the very beginning of the rule.
Brassica will then generate two output words for this rule:
  one in which the rule has been applied,
  and another in which the rule has not applied.
These outputs are separated by a slash by default
  (configurable with the ‘Multiple result separator’ textbox in the graphical interface).

For instance, consider the following rule:
```
-? i / / a _ #
```
This sporadically deletes ⟨i⟩ after ⟨a⟩ at the end of a word.
Applied to the word ⟨kanai⟩, this produces the two outputs ⟨kanai⟩ and ⟨kana⟩.
You can then inspect and compare these results to determine which is preferable.

For even more sporadicity the flag `-??` can be used instead.
This applies the rule *per-occurrence*, not *per-word*.
Thus, for instance, the rule `-? o / u` applied to ⟨koko⟩ will produce the two outputs ⟨koko⟩ and ⟨kuku⟩:
  changing it to `-?? o / u` will add two more outputs to these, ⟨koku⟩ and ⟨kuko⟩,
  where the rule has been applied to one occurrence of ⟨o⟩ but not the other.

For greater control over the number of results, one can use categories or optional elements in the target.
As mentioned above, these are normally matched one-to-one with corresponding elements in the target.
However, if an output element is found with no corresponding input element, Brassica will instead produce *all possible* outputs.

For example, the following two rules summarise the history of close back vowels from Middle to Modern English:
```
oː / [uː ʊ ʌ]
u / [ʌ ʊ]
```
(Note that the real evolution is [somewhat more complex](https://en.wikipedia.org/wiki/Phonological_history_of_English_close_back_vowels).)
Thus, the word ⟨foːt⟩ ‘foot’ would produce the three outputs ⟨fuːt/fʊt/fʌt⟩,
  of which the second happens to correspond to the modern English pronunciation.
Similarly, ⟨moːðər⟩ ‘mother’ yields ⟨muːðər/mʊðər/mʌðər⟩, where the third output is seen in most modern English dialects.

Rarely, Brassica will produce multiple results even when this is not explicitly specified.
This occurs when it detects that there are multiple valid ways to apply a rule to a word.

This situation comes up most often with optional elements in the target or environment.
For instance, if the rule `o (m) / u (ŋ)` is applied to the word ⟨tom⟩, then both ⟨tum⟩ and ⟨tuŋ⟩ are possible outputs,
  depending on whether the target is taken to include the ⟨m⟩ or not.

However, more subtle cases also occur.
For instance, consider the following rule:
```
ʔ / / [# ʔ] _
```
There are two different ways to apply this rule to the word ⟨ʔʔan⟩.
On one hand, we can notice that the very first `ʔ` is at the beginning of the word, so it can be deleted;
  this then brings the second `ʔ` to the beginning of the word, where it too can be deleted to give the result ⟨an⟩.
On the other hand, we can notice that there are two `ʔ`s in a row, so by this rule the second can be deleted;
  proceeding on to the rest of the word gives the result ⟨ʔan⟩.
In this situation, Brassica will report both ⟨an⟩ and ⟨ʔan⟩, to account for all possibilities.

### Backreferences

Many more complex rules can be expressed using **backreferences**.
These take the form `@n`, where `n` can be any positive number, and are placed before categories to modify their meaning.

A backreference in the replacement indicates that the following category should be associated with a specific category in the target.
If the categories in the target are numbered from left to right, starting from 1,
  backreference `@n` corresponds to target category number `n`.

For a simple example, consider the following nasal assimilation rule:
```
[m n ŋ] [p t k] / @2 [m n ŋ] @2 [b d g]
```
Here, `@2` in the replacement indicates that
  both replacement categories take their values from the *second* category listed in the target
  (namely `[p t k]`).
That is, a nasal followed by `p` will always be replaced by `mb`;
  similarly a nasal followed by `t` will always yield `nd`, and one followed by `k` will always yield `ŋg`.

A more complex example (similar to a sound change [attested in Hawu](https://www.jstor.org/stable/23321852)) is as follows:
```
[i u] C V / ə @2 C @1 [i u]
```
Here, two vowels are swapped around a consonant, with the first being reduced to ⟨ə⟩.
The target consists of three categories in sequence.
The replacement starts with ⟨ə⟩, then takes the consonant matching the *second* category in the target (i.e. the original intervocalic consonant),
  followed by the element of `[i u]` corresponding to the *first* category in the target (i.e. the original preconsonantal vowel).
The backreferences act to swap the two categories, accomplishing the metathesis.

If using backreferences in the target, it is recommended to use them consistently:
  add a backreference to *every* category mentioned the target.
This isn’t actually required, but it can help to make your sound changes easier to read and write.

Backreferences can also be used in the target and replacement.
Here they have a different meaning: they match **repeated category elements**.
A backreferenced category in the target or replacement is constrained to match the element
  which corresponds to the element matched by the category it references.

If this seems a bit abstract, the following example may clarify the meaning:
```
[m n ŋ] @1 [p t k] / [b d g]
```
The target of this rule will only match homorganic consonant clusters.
The category `[p t k]` backreferences to the *first* category in the target, namely `[m n ŋ]`:
  thus, the graphemes matched by each category must correspond.
This target will match ⟨mp⟩, ⟨nt⟩ and ⟨ŋk⟩, but not (say) ⟨mt⟩ or ⟨ŋp⟩.

Another example, showing a backreference in the environment:
```
ʔ / / [# C] V _ @2 V
```
This rule deletes a glottal stop between identical vowels, at the beginning of a word or after a consonant.
The identity of the vowels is enforced by a backreference:
  the second vowel must correspond to the *second* category in the environment, namely the first vowel.
(Note that this backreference extends across the target!
This is completely acceptable.)

An important restriction:
  a backreference in the target or environment must come *after* the category it references.
In other words, you can’t reference a category which hasn’t been mentioned yet (reading from left to right).
Of course, a category cannot refer to itself either, since that would make no sense.

### Other features

Other, more advanced features supported by Brassica include:

- Other flags to influence the details of rule application
- Wildcards `^` and lexeme repetition `*`
- Filters for dealing with large numbers of multiple results
- Feature definitions to represent suprasegmentals

For details on these, consult the [reference guide](./Reference.md).


## Using the graphical interface

### Basic usage

The graphical interface appears as follows (on desktop):

![Image of Brassica GUI](./gui-interface.png)

Enter your sound change rules in the leftmost textbox, and your input words in the textbox second from the left.
Then press the ‘Apply’ button to view the result of applying your rules to your lexicon.
Alternately, you can select ‘Report rules applied’:
  this will display a table showing the evolution of each word as it was modified by the sound changes.

Below these buttons, there are areas to select:
- Whether to highlight any output words:
    ‘different to last run’ will highlight output words which have changed since the last run,
    while ‘different to input’ will highlight output words which have undergone any sound changes.
- The input format: this can be either a wordlist (optionally including [glosses](#input-and-output-formats)), or a dictionary in [MDF format](http://downloads.sil.org/legacy/shoebox/MDF_2000.pdf)
- The output format: see below for details.

Finally, there are some other options to control the interface:
- ‘View results live’ will cause the output to update live as you type.
  (Warning: this can be slow!)
- ‘Synchronise scroll positions’ will attempt to synchronise the scroll positions of the boxes which display the input and the output words,
    such that the same words are shown in both.
- ‘Multiple result separator’ allows you to change the character used to separate multiple output words arising from a single input word
    (as described [above](#sporadic-rules-and-multiple-results) for details).

### Input and output formats

Brassica can currently take input in two formats:
  as a list of words, or as an MDF dictionary file.
For each of these inputs it can produce output in several different formats.

The default input format is a **plain-text wordlist**.
(It is expected that this will have file extension `.lex`.)
Each line of the input can contain zero, one or more words, separated by spaces.
Brassica will apply sound changes independently to each word in the wordlist.

A wordlist can also contain **glosses**.
Write these between angle brackets, `[like this]`.
When Brassica encounters a gloss, it will not apply any sound changes:
  the gloss will be preserved unchanged in the output (if it is not omitted).
  
Brassica can also process **MDF dictionary files**.
(It is expected that this will have file extension `.txt` or `.mdf`.)
These are used by other programs such as [SIL Toolbox](https://software.sil.org/toolbox/).

> **Note:** For details on the MDF format itself, see [Coward and Grimes’s original manual](https://web.archive.org/web/20211008115829/http://downloads.sil.org/legacy/shoebox/MDF_2000.pdf),
>   or [SIL’s MDF documentation](http://www.fieldlinguiststoolbox.org/MDFDocumentation.zip) formatted for use with SIL Toolbox.

To use MDF input files with the desktop interface, the ‘MDF file’ option must be enabled.
This option will automatically be enabled when an MDF file is opened using the ‘Open lexicon’ dialog box.
MDF files can also be processed when using Brassica in batch mode from the command-line,
  by supplying the `--mdf` command-line argument.

In MDF files, Brassica will process all fields with values in the ‘vernacular’ language.
This includes such MDF fields as `\lx` ‘lexeme’, `\se` ‘subentry’, `\cf` ‘cross-reference’ and `\xv` ‘example’.
All other fields will be unaffected by sound changes.
Note that after rule application, the resulting MDF file still needs to be inspected and modified,
  to account for semantic changes, differences in sample sentences and other variations which Brassica is unable to simulate.

For both of the input formats mentioned above, the following output formats are available:

- **Wordlist** output will produce the same format as a wordlist input.
  For MDF input, the output will include only the vernacular words of the input.
- **Input→output** format will list the input and output forms of every word processed by Brassica.

For MDF inputs, two additional output formats are available:

- **MDF** output produces output in the MDF format, preserving the structure of the MDF input.
- **MDF input with etymologies** produces MDF output as above,
    but additionally inserts etymological `\et` and `\eg` fields containing the original lexeme and/or its gloss.
    
For an example of this last option, consider the sound change `b o / u`, applied to the following MDF input file:
```
\lx bonab
\ge woman
\lf Ant
\lv bokap
\le man
\se bonabo
\ge girl
\dt 21/Apr/2022
```
The output with etymologies inserted is:
```
\lx unab
\ge woman
\lf Ant
\lv ukap
\le man
\se unau
\ge girl
\et *bonabo
\eg girl
\dt 21/Apr/2022
```
Further modifications can then be made to the output while keeping the etymological information for reference.
(Note that Brassica adds an initial asterisk to all etymons;
  this is not currently configurable, but can easily be removed by hand.)

### Output highlighting

Brassica’s interactive interface lets you highlight output words which satisfy various conditions.
These options are located in the ‘output highlighting’ box near the right of the window.

The default option is ‘No highlighting’.
Select **Different to last run** to highlight all words where
  the current application of the rules gave a different output to the last application of the rules.
This option can be useful when investigating the effect of a particular rule on the output:
  while this option is enabled, adding and removing the rule will highlight all words which are affected by this rule.

Select **Different to input** to highlight all words which have been affected by at least one rule.
This option can be useful when prototyping a new set of sound changes.
For instance, a conlanger might want to make sure that every word is highlighted,
  indicating that at least one sound change has applied to every word.

With this latter option, some rules can cause excess highlighting.
This can occur, for instance, with rules which handle romanisation conventions or syllable boundaries.
These rules tend to alter a large portion of the input, without representing any real sound change.

For these cases, Brassica provides the `-x` flag.
If a word has only been affected by rules marked with `-x`, that word will not be highlighted.
That is, for the purposes of output highlighting, Brassica treats `-x` rules as if they had caused no change.
It is recommended to annotate rules with `-x` when they cause only cosmetic change,
  such that only rules corresponding to actual phonetic change will trigger output highlighting.
(This also makes it easier to see which rules are important or not!)

## Paradigm builder

Brassica includes an inbuilt paradigm builder.
It may be accessed using the ‘Tools⇒Paradigm Builder’ menu item in the graphical interface,
  by visiting <https://bradrn.com/brassica/builder.html>,
  or by running `brassica-pb` from the command-line.

The graphical interface of the paradigm builder consists of three textboxes and a button.
The leftmost textbox contains a description of the paradigm using the syntax described below.
The middle textbox contains a list of roots on which the paradigm should be built.
Each root should be specified on its own line.
When the ‘Build’ button (at the bottom right on the desktop version) is pressed,
  the rightmost textbox will contain the output of the paradigm builder,
  which can then be copied and pasted into another window or application.
A menu at the top of the window provides options to open and save the paradigm and the lexicon.

A basic paradigm definition is fairly simple.
Each line of the paradigm definition specifies a *grammatical feature* — a set of mutually exclusive affixes.
These are given as a list, separated by spaces.
As is usual in templatic morphology, each prefix and suffix is assigned a slot.
For prefixes, `-1` is the slot closest to the root, `-2` is the slot before that, and so on.
For suffixes, `1` is the slot closest to the root, `2` is the slot after that, and so on.
Thus each affix is either:

- A prefix in slot *n*, specified as `-n.prefix`;
- A suffix in slot *n*, specified as `n.suffix`;
- An empty affix, specified as `()`;
- A combination of other affixes, specified as `(affix1 affix2 ...)`; or
- An *abstract feature*, specified simply as `featurename` (more on this below).

Thus an example of a basic paradigm definition would be as follows:

```
() 2.en
1.wim () 1.soo 1.aa
-2.zhaa -2.woo -2.yaa
-1.zh -1.w -1.y
```

The output will then iterate through all combinations of these affixes.
For instance, applying the paradigm above to the root `kood` gives:

```
zhaazhkoodwim zhaawkoodwim zhaaykoodwim
woozhkoodwim woowkoodwim wooykoodwim
yaazhkoodwim yaawkoodwim yaaykoodwim

zhaazhkood zhaawkood zhaaykood
woozhkood woowkood wooykood
yaazhkood yaawkood yaaykood

zhaazhkoodsoo zhaawkoodsoo zhaaykoodsoo
woozhkoodsoo woowkoodsoo wooykoodsoo
yaazhkoodsoo yaawkoodsoo yaaykoodsoo

zhaazhkoodaa zhaawkoodaa zhaaykoodaa
woozhkoodaa woowkoodaa wooykoodaa
yaazhkoodaa yaawkoodaa yaaykoodaa


zhaazhkoodwimen zhaawkoodwimen zhaaykoodwimen
woozhkoodwimen woowkoodwimen wooykoodwimen
yaazhkoodwimen yaawkoodwimen yaaykoodwimen

zhaazhkooden zhaawkooden zhaaykooden
woozhkooden woowkooden wooykooden
yaazhkooden yaawkooden yaaykooden

zhaazhkoodsooen zhaawkoodsooen zhaaykoodsooen
woozhkoodsooen woowkoodsooen wooykoodsooen
yaazhkoodsooen yaawkoodsooen yaaykoodsooen

zhaazhkoodaaen zhaawkoodaaen zhaaykoodaaen
woozhkoodaaen woowkoodaaen wooykoodaaen
yaazhkoodaaen yaawkoodaaen yaaykoodaaen
```

(You can also tick the option ‘Each word on its own line’, if you dislike placing multiple words on a single line.)

Note that in the paradigm described above, all affixes on each line are assigned to the same slot.
This is common in paradigms, so the paradigm builder has a shortcut syntax for this situation,
  in which the slot is specified at the beginning of the line:

```
() 2.en
1 wim () soo aa
-2 zhaa woo yaa
-1 zh w y
```

Any slots specified for later affixes will override the slot at the beginning of the line.

Also, each grammatical feature can be given a name, which must be followed by `=`:

```
NEG = () 2.en
1 TA = wim () soo aa
-2 ABS = zhaa woo yaa
-1 ERG = zh w y
```

If a grammatical feature is given a name, it can subsequently be used in a *condition*.
This is useful in situations where two different grammatical features do not co-occur.
For instance, it might be desirable to ensure that a verb with the antipassive cannot take an ergative cross-referencing marker:

```
NEG = () 2.en
ANTIP = () 1.el
2 TA = wim () soo aa
when (ANTIP not 1.el) -2 ABS = zhaa woo yaa
-1 ERG = zh w y
```

A condition takes the form `when (<feature name> <operator> <affix>)`,
  where `<operator>` is either `is` or `not`.
The output will contain the following feature only in words where the condition is satisfied;
  in all other words that feature will be absent.

Earlier, brief reference was made to *abstract features*.
These are similar to affixes, but are not associated with any one slot.
Instead, one or more abstract features can be *mapped* to an affix,
  by writing on a separate line:
```
feature1 feature2 … > affix
```
The `affix` will then be included in the result when all of the specified features are present.
This allows for implementing fusional morphology, for instance the Latin first declension:
```
NOM VOC ACC GEN DAT ABL
SG PL
NOM SG > 1.a
NOM PL > 1.ae
VOC SG > 1.a
VOC PL > 1.ae
ACC SG > 1.am
ACC PL > 1.ās
GEN SG > 1.ae
GEN PL > 1.ārum
DAT SG > 1.ae
DAT PL > 1.īs
ABL SG > 1.ā
ABL PL > 1.īs
```
When applied to, for instance, `puell`, this produces:
```
puella puellae
puella puellae
puellam puellās
puellae puellārum
puellae puellīs
puellā puellīs
```
