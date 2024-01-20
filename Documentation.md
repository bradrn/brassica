<!-- -*-GFM-*- -->

# Brassica documentation

## Using Brassica

Brassica may be used as an interactive application in two ways:
  either using the standalone desktop program, or using the [online version](http://bradrn.com/brassica/index.html).
Both of these graphical interaces work in the same way and are organised very similarly, though the online interface supports fewer features than the desktop one.

The desktop application appears as follows:

![Image of Brassica GUI](./gui-interface.png)

For basic usage, enter your rules in the leftmost textbox and your lexicon in the textbox second from the left,
  using the syntax described below.
Then press the ‘Apply’ button to view the result of applying your rules to your lexicon.
Alternately, select ‘Report rules applied’ to view how each individual rule affects each word in the lexicon.
Below these are options to control the output format; these are detailed below.
Finally, enabling the checkbox labeled ‘View results live’ causes the results to update live as you type.
This can be extremely useful when prototyping new rules, but is very slow for longer rulesets.
For this reason, live updating is disabled by default.

Brassica also offers a command-line interace.
This is designed to support batch processing of dictionaries and other large files.
Invoke Brassica on the command-line by running `path/to/brassica path/to/your/rules.bsc`.
By default `brassica` takes words as input on stdin, and outputs to stdout the results of applying the selected ruleset to these words.
However these defaults can be changed by means of the `--in`/`-i` and `--out`/`-o` command-line arguments respectively.
For more details on using the command-line interface, run `brassica --help`.

The library which powers Brassica can also be used directly as a Haskell package.
View the documentation at https://hackage.haskell.org/package/brassica.

## Basic rule syntax

### Writing simple rules

The basic form of a sound change rule in Brassica is as follows:
```
target / replacement / before _ after
```
This will replace `target` by `replacement` only when preceded by `before` and followed by `after`.
The entire `before _ after` section is called the ‘environment’.
Note that `before` or `after` (or both) may be absent:
  in that case there are no constraints on what must precede or follow the target.

If you prefer, the initial `/` can be replaced by `→`, as in standard linguistic notation:
```
target → replacement / before _ after
```
However this guide does not use that variant.

In the most basic rules, the target and replacement are a single letter.
For instance, a rule to replace ⟨θ⟩ by ⟨f⟩ unconditionally (as found in some English dialects) can be specified as follows:
```
θ / f / _
```
Such unconditional rules are common.
Brassica provides a shortcut syntax for this situation, in which unconditional rules may be specified by simply leaving off the environment:
```
θ / f
```
Brassica refers to elements like `θ` or `f` as *graphemes*.

A target, replacement or environment can contain multiple graphemes.
However, **they must have spaces between them**.
(This is important!
See the section below on [multigraphs](#multigraphs) for details.)
For instance, the following rule changes ⟨ki⟩ to ⟨č⟩:
```
k i / č
```

Word boundaries may be specified in the environment as `#`.
For instance, this rule changes ⟨n⟩ to ⟨ŋ⟩ at the end of a word:
```
n / ŋ / _ #
```

Either the target or the replacement may be left blank.
If the replacement is blank, the target is deleted (i.e. replaced with nothing) in every position where the environment holds.
For instance, this rule deletes word-final ⟨i⟩:
```
i / / _ #
```

On the other hand, if the target is blank, the replacement is inserted in every position where the environment holds.
For instance, this rule inserts ⟨ʔ⟩ between any two adjacent ⟨a⟩s:
```
/ ʔ / a _ a
```

Plain-text *comments* can be added by beginning them with a semicolon.
These can be useful for explaining what a complex rule does, for instance:

```
; replace i with y before a
i / y / _ a
```
Comment are usually added on their own line, as in the above example.
They may also go on the same line as a rule, after that rule (though this is not recommended):
```
i / y / _ a    ; replace i with y before a
```

A rule may have more than one environment, in which case it applies if any of the environments match.
For instance, the following rule deletes ⟨ʔ⟩ at the beginning or the end of a word:
```
ʔ / / _ # / # _
```

### Categories

It is extremely common for a sound change to affect several different graphemes at once.
Brassica handles this case using *categories*, consisting of a list of graphemes within square brackets.
In a rule, a category will match any of the graphemes within it.

For instance, `[a e i o u]` will match any of ⟨a⟩, ⟨e⟩, ⟨i⟩, ⟨o⟩ or ⟨u⟩.
This means the following rule will insert ⟨ʔ⟩ between any two vowels (assuming a five-vowel system):
```
/ ʔ / [a e i o u] _ [a e i o u]
```
And this rule will delete any consonant occurring immediately before another
  (assuming the relatively small consonant inventory of the Papuan language Maybrat):
```
[p t k m n f s x r w y] / / _ [p t k m n f s x r w y]
```

Often we want to convert each of a set of graphemes to another, related grapheme.
For instance, we might want to convert voiceless stops to nasals when word-final:
```
p / m / _ #
t / n / _ #
k / ŋ / _ #
```
Obviously this gets tedious for long sets of graphemes!
Thus, Brassica lets you use categories for the same purpose.
If a category appears in the replacement of a rule, it will take its value from a corresponding category in the target.
The first grapheme listed in the target category will be replaced by the first in the replacement category,
  the second grapheme in the target will be replaced by the second in the replacement,
  and so on.
Thus the following single rule does the same as the three rules above:
```
[p t k] / [m n ŋ] / _ #
```

In the environment only, a word boundary can be included within a category.
Thus, to delete ⟨ʔ⟩ before a stop or the end of a word, the following rule works:
```
ʔ / / _ [p t k b d g #]
```

Sometimes you might want to match a sequence of zero or more graphemes.
Do this in a category by surrounding them with curly brackets `{`/`}`, as in:
```
[eː oː] / [{j ə} {w ə}]
[{ŋ g} ŋ] → [ŋ {}] / _#
```

You can also use multiple categories in the replacement.
In this case each is matched up one-to-one with a category in the target:
  the first in the replacement with the first in the target,
  the second in the replacement with the second in the target,
  and so on.
For instance, the following (rather contrived) rule will replace stop–nasal sequences with nasal–stop sequences,
  but preserving places of articulation (e.g. ⟨km⟩ → ⟨ŋp⟩):
  
```
[p t k] [m n ŋ] / [m n ŋ] [p t k]
```

More complex rules may require ignoring a category in the target entirely.
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
This can be resolved using the special symbol `~`,
  which corresponds to one category in the input, but produces no output:
```
[a e i o u] [a e i o u] / ~ [ɐ ə ɨ ə ɨ]
```
As English, this rule can be read as: ‘delete the first vowel, and centralise the second vowel’ — exactly what we want!


### Defining categories

It can be inconvenient to repeatedly type out the same categories over and over again.
Thus Brassica allows the use of *predefined categories*.
To define a category, specify it in a *category block* before any rule which uses it:

```
categories
C = p t k m n f s x r w y
V = a e i o u
end
```

After this definition, any reference to e.g. `V` will act the same as `[a e i o u]`.
(Indeed, Brassica itself sees no difference whatsoever between the two!)
For the remainder of this guide, I will assume that `C` and `V` have been defined
  to be the set of all consonants and all vowels respectively.

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

### Operations on categories

It is possible to reference predefined categories in later category expressions.
For instance, a category matching both consonants and vowels can be written as `[C V]`.
Similarly, a category matching all consonants, plus `i` and `u` can be written as `[C i u]`.
This is referred to as finding the *union* of the relevant categories.

There are two other ways of combining categories.
The *intersection* of two categories can be found using the syntax `[C +D]`.
This matches all members of `C` which are also members of `D`.
The *subtraction* of two categories can be found using the syntax `[C -D]`.
This matches all members of `C` which are not also members of `D`.

The operations above can be combined, in which case Brassica applies them from left to right.
For instance, consider the following predefined categories:

```
categories
Stop = p t k b d g
Affr = c j
Fric = f s š x v z ž ɣ
Aprx = w r l y
Nasl = m n ñ ŋ

Lab = p b f v w m
Alv = t d s z l r n
Pal = c j š ž y ñ
Vel = k g x ɣ ŋ

Vcd = b d g j v z ž ɣ w r l y m n ñ ŋ

C = p t k b d g c j f s š x v z ž ɣ w r l y m n ñ ŋ
V = a e i o u
end
```

(Incidentally this also provides an example of multi-letter category names. More on those later.)

With these categories, the expression `[Affr Fric +Pal +Vcd Aprx -r]` is interpreted as follows:

- `[Affr Fric]` is the same as `[c j f s š x v z ž ɣ]`
- `[Affr Fric +Pal]` is `[c j š ž]`
- `[Affr Fric +Pal +Vcd]` is `[j ž]`
- `[Affr Fric +Pal +Vcd Aprx]` is `[j ž w r l y]`
- `[Affr Fric +Pal +Vcd Aprx -r]` is `[j ž w l y]`.

That is, `[Affr Fric +Pal +Vcd Aprx -r]` is just a rather long and complex way of writing `[j ž w l y]`.
  
These sort of expressions can be particularly useful in writing sound changes in a ‘featural’ way.
For instance, with the categories defined above,
  intervocalic fricative voicing can be written as `[Fric -Vcd] / [Fric +Vcd] / V _ V`,
  and nasalisation of an initial voiced stop in a stop cluster can be written as `[Stop +Vcd] / Nasl / _ Stop`.
  
Be careful to ensure that the target and replacement categories always have the same number of elements!
For instance, you might be tempted to express regressive voicing assimilation in consonant clusters as follows:
  
```
[C -Vcd] / [C +Vcd] / _ [C +Vcd]
[C +Vcd] / [C -Vcd] / _ [C -Vcd]  ; incorrect!
```

However, the second rule here is incorrect, as is obvious if you expand it out:

```
[b d g j v z ž ɣ w r l y m n ñ ŋ] / [p t k c f s š x] / _
```

Here the nasals and approximants have no voiceless variants;
  thus a word such as ⟨kayte⟩ will result in an output ⟨ka�te⟩.
This makes intuitive sense: the categories never did define a voiceless counterpart of /y/!
Assuming we do *not* want nasals and approximants to undergo this change,
  it is easiest to simply exclude such consonants from the rule using the subtraction operator:
  
```
[C -Vcd] / [C +Vcd] / _ [C +Vcd]
[C +Vcd -Nasl -Aprx] / [C -Vcd] / _ [C -Vcd]
```

All of these category operations can also be used in predefined categories.
Thus, in the previous example, the `C` category could have been written as follows:

```
categories
C = Stop Affr Fric Aprx Nasl
end
```

And a category for voiceless consonants could have been written as follows:

```
categories
Vls = C -Vcd
end
```

In general, this syntax is recommended, as it prevents your categories from getting out of sync if you change them.

### Multigraphs

So far, all of the examples have contained graphemes consisting of only one letter each.
However it is often desirable to use *multigraphs* such as ⟨ng⟩, ⟨th⟩, ⟨aa⟩ or ⟨eqh⟩.
But multigraphs can be problematic when combined with other rules:
  for instance, a rule such as `h / / _` will wrongly convert ⟨th⟩ to ⟨t⟩.
Thus many other sound change applies struggle with multigraphs.

By contrast, Brassica allows rules to contain true multigraphs.
Specify multigraphs by omitting the space between letters.
For instance, the following rule will convert any `t`+`h` sequences in the input into a single multigraph `th`.
```
t h / th
```

Note that Brassica treats a multigraph like `th` as its own grapheme,
  distinct from a sequence of two graphemes `t`+`h`!
Thus rules such as `h / / _` will not affect multigraphs,
  while rules such as `th / s` will not affect sequences of single-letter graphemes.
Similarly, rules involving categories, gemination and metathesis will treat multigraphs as single elements:

```
C C / ~ C / _                      ; will convert ⟨athke⟩→⟨ake⟩ (but *⟨athe⟩→⟨ahe⟩ will not occur)
[f s th x] / [f s th x] > / _ #    ; will convert ⟨bath⟩→⟨bathth⟩
y [m n ng] / \ / _ V               ; will convert ⟨ynga⟩→⟨ngya⟩
```

Also, any valid grapheme may be used as a category name.
This explains why the examples above were able to use category names with multiple letters.

Incidentally, be careful to include spaces between graphemes which are *not* multigraphs!
If you accidentally write a rule such as `ā / ay`, it will produce odd results, since Brassica will interpret the replacement ⟨ay⟩ as a single grapheme.
Use `ā / a y` instead to avoid this issue.

By default, Brassica tokenises input words into single-letter graphemes:
  ⟨bath⟩ becomes `b`+`a`+`t`+`h`, and ⟨enga⟩ becomes `e`+`n`+`g`+`a`,
  even if subsequent rules use multigraphs `th` and `ng`.
The user would then need to use rules such as `t h / th` to introduce multigraphs.
However Brassica assumes that any multigraphs listed in the first category block
  will be present in the input.
Thus the input will be tokenised into multigraphs *only if* those multigraphs are listed in the first category block.
Because Brassica encourages a style where each category block contains every grapheme used at that point in rule application,
  this will usually result in correct tokenisation.

Some languages have a romanisation in which a multigraph and the corresponding letter sequence are both used for different consonants.
This can be observed e.g. in some Australian languages which use ⟨ng⟩ for /ŋ/ but ⟨n⟩+⟨g⟩ for /nɡ/.
Often the latter is represented ⟨n’g⟩ or ⟨n.g⟩ in text, as seen for instance in the language name Ngan’gityemerri /ŋanɡicemeri/.
Such a situation can be dealt with in Brassica using a rule which removes the separator, such as `’ / / _`.
A sequence such as ⟨n’g⟩ will be tokenised to `n`+`’`+`g`, after which this rule will remove the separator leaving `n`+`g` as desired,
  while a sequence such as ⟨ng⟩ will be straightforwardly tokenised to the multigraph `ng`.

### Miscellaneous useful elements

Several useful elements have not been covered yet.
The symbol `>` represents *gemination*.
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

Metathesis can be accomplished by specifying `\` as the replacement.
This will reverse the target:

```
Aprx Stop / \         ; converts ⟨ayke⟩→⟨akye⟩, ⟨nawbe⟩→⟨nabwe⟩
```

One or more elements in the target or replacement can be made optional by surrounding them with parentheses:

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

Sometimes rules can apply over long distances.
For instance, progressive vowel harmony changes vowels depending on the preceding vowel, irrespective of how many consonants intervene.
Such rules can be written in Brassica using the *wildcard* `*`.
When placed after another element, it will match as many repetitions of that element as possible.
Thus, if the two vowel classes are called e.g. `V` and `V̈`, progressive vowel harmony can be written as follows:

```
V / V̈ / V̈ C* _       ; converts ⟨ämpte⟩→⟨ämptë⟩
```

(Note: this rule will not work for words of more than one vowel!
This is a known issue with Brassica, relating to the fact that in such cases the target and replacement overlap.
For now, a suitable fix is to simply repeat this rule a number of times.)

The `^` character represents another type of wildcard.
If placed before another element, it will match any number of repetitions of *any* letter until it can match that element.
This can be used to implement retrogressive vowel harmony:

```
V / V̈ / _ ^V̈          ; converts ⟨amptë⟩→⟨ämptë⟩
```

Both `*` and `^` can be used in the replacement of a rule.
`*` repeats its replacement the same number of times it was matched:

```
V C* V̈ / V̈ C* V       ; converts ⟨amptë⟩→⟨ämpte⟩
```

While `^` repeats all the graphemes it matched:

```
V ^V̈ / V̈ ^V        ; converts ⟨amptë⟩→⟨ämpte⟩
```


One often wants to include a *gloss* in the list of input words.
Such glosses should not be affected by sound changes.
This can be achieved by surrounding the gloss in square brackets.
Any part of the input surrounded by square brackets will be ignored by all sound changes.


## Advanced features

### Sound changes across words

Sometimes it can be useful to apply sound changes across two or more words at once.
For instance:

- Sandhi and liaison processes act across adjacent words
- Resyllabification can cause a consonant to move across a word boundary
- Cliticisation and affixisation are associated with the deletion of a phonological boundary

To model these processes, you can explicitly connect words together in the lexicon with the word-boundary marker `#`:
  for instance, `sītā#etam`, `loː#ænd#oːdɐ`, or `grɑ̃t#ɔm`.
In most situations these are treated as entirely separate words.
However, for these words you can reference the word boundary `#` in the target or replacement, or in the middle of an environment.
For instance, a selection of Sanskrit vowel sandhi rules (taken from <https://ubcsanskrit.ca/lesson3/sandhicharts.html>)
  can be modeled as follows:
```
[a ā] # [a ā] / ā
[i ī] # [i ī] / ī
ṛ # / r / _ V
ai / ā / _ # [a ā]
; and so on
```
And English intrusive /r/ can be represented as:
```
/ r / [ə iə ɑː oː] # _ V
```
You can even do `# / / _` to remove all interword boundaries in the lexicon, if such a thing is useful.

One trick that can be useful is to add some kind of marker between two words to reflect their relationship,
  while using word boundaries to keep it separate from the word.
For instance, clitics might be represented using a multigraph `CL`:
  with English words this would look like `ði#CL#kæt` or `tʉː#CL#gəʊ`.
That allows manipulations such as:
```
V / ə / _ # CL  ; reduce cliticised vowels
```
Or:
```
# CL # / / _    ; affixisation of clitics
```

### Controlling rule application

By default, Brassica applies rules from left to right.
Thus a rule such as `a / b / _ b` will convert an input like ⟨aaaab⟩ to ⟨aaabb⟩ rather than ⟨bbbbb⟩:
  Brassica traverses the input from left to right, so only ‘sees’ the `b` immediately after the last `a`,
  and cannot go back and change the `a` which is now followed by a new `b`.
More realistically, this problem can occur in rules such as vowel harmony or stress assignment.

Problems such as these can be solved by adding ‘flags’ to rules.
A *flag* in Brassica is an element at the beginning of the rule preceded by `-` and followed by whitespace.
Here, the relevant flag is `-rtl`: when placed at the beginning of a rule, it changes rule application to be right-to-left.
A rule such as `-rtl a / b / _ b` will correctly convert ⟨aaaab⟩→⟨bbbbb⟩.
If desired, the flag `-ltr` can also be used to explicitly indicate left-to-right rule application,
  even though this is already the default.

Another useful flag is `-1`.
This flag will tell Brassica to only apply the following rule one time, then stop.
Thus `a / b` will convert ⟨aaaaa⟩→⟨bbbbb⟩, while `-1 a / b` will convert ⟨aaaaa⟩→⟨baaaa⟩.
This flag is particularly useful in rules of primary stress assignment
  (for rules like, ‘assign primary stress to the first syllable’).

In some situations it might be desirable to apply a rule normally *except* when a specific condition applies.
Such exceptions can be specified by including two slashes after the environment,
  then specifying the exceptional environment.
For instance, a rule to degeminate consonants everywhere *except* intervocalically might be written as follows:

```
C / / _ > // V _ V
```

(Note that some SCAs place restrictions on what can be used in an exception clause.
Brassica has no such restriction.
Any element, even optional elements or wildcards, can be used in an exception in Brassica.)

### Backreferences

Recall that Brassica matches up each category in the replacement to the corresponding category in the target,
  going in order from left to right.
Thus, a rule `[p t k] [m n ŋ] / [m n ŋ] [p t k]` will change ⟨pŋ⟩ to ⟨mk⟩ and ⟨tm⟩ to ⟨np⟩.
We can picture this as follows, with the first category in the replacement associated to the first category in the target, and similarly for the second categories:
```
[p t k] [m n ŋ] / [m n ŋ] [p t k]
───┬─── ───╥───   ───┬─── ───╥───
   │       ║         │       ║
   │       ╚═════════╪═══════╝
   └─────────────────┘
```
(NB. If the picture looks wonky, try copying it into Notepad or a similar text editing application.)

However, this behaviour can be inconvenient.
For instance, consider the cross-linguistically common sound change converting V₁ʔC to V₁ʔV₁C.
Though not impossible, this is difficult to write using only the syntax covered above.
A rule like `V ʔ / V ʔ V / _ C` fails, since the second `V` in the replacement corresponds to no category in the target:
```
V ʔ / V ʔ V / _ C
┬     ┬   ┬
└─────┘   │
          ?
```

‘Backreferences’ provide a way to express such rules.
To create a backreference, write <code>@<i>n</i></code> before a category, where <code><i>n</i></code> is any number greater than 0.
This indicates to Brassica that it should associate that category with the <code><i>n</i></code>th category in the target.
Thus, we can write the desired rule as `V ʔ / @1 V ʔ @1 V / _ C`,
  explicitly associating both categories in the replacement with the first category in the target:
```
V ʔ / V ʔ V / _ C
┬     ┬   ┬
├─────┘   │
└─────────┘
```

Of course, we are not limited to using the same category on both sides.
For instance, the following rule will cause a nasal to assimilate in place of articulation to a following voiced stop,
  by associating the nasal category in the replacement with the stop category in the target:
```
[m n ŋ] [b d g] / @2 [m n n] @2 [b d g]
───┬─── ───╥───   ─────╥──── ─────╥────
           ╠═══════════╝          ║
           ╚══════════════════════╝
```

Backreferences can also be used in the target or environment, in which case they match repeated or corresponding graphemes.
For instance, consider a rule deleting ⟨ə⟩ between identical consonants.
This may be written:
```
C ə @1 C / C @1 C
┬   ──┬─   ┬ ──┬─
├─────┘    │   │
├──────────┘   │
└──────────────┘
```
(Note the use of backreferences in the replacement too!
Otherwise the second `C` in the replacement would not be associated with anything in the target.)

Similarly, consider a rule converting homorganic nasal + voiceless stop sequences to prenasalised stops
  (e.g. ⟨nt⟩→⟨ⁿd⟩, but ⟨mt⟩ doesn’t change).
This may be written as:
```
[m n ŋ] @1 [p t k] / [ᵐb ⁿd ⁿg]
───┬─── ─────┬────   ─────┬────
   ├─────────┘            │
   └──────────────────────┘
```

### Sporadic rules and multiple results

Sometimes we might want a sound change rule with more than one possible output.
The most common situation where this is useful is that of *sporadic rules*:
  sound changes which unpredictably apply to some words but not others.
In Brassica, these can be simulated by placing the flag `-?` before the rule.
For rules with this flag, two outputs are generated:
  one for the situation in which the rule has been applied,
  and one for the situation in which it has not been applied.
(In the output wordlist, these are displayed separated by a single space.)
For instance, the rule `-? i / / a _ #` applied to the word ⟨kanai⟩ will produce the two outputs ⟨kanai⟩ and ⟨kana⟩.
This allows us to manually inspect both outputs, so we can decide which one we prefer for later usage.

We can also produce rules with two or more different outputs, where all the outputs have undergone some change.
One way of doing this involves categories or optional elements in the replacement which cannot be matched with any in the target.
As mentioned above, categories or optional elements used in the replacement are matched one-to-one with corresponding elements in the target.
However, if an output element is found with no corresponding input element, Brassica will instead produce *all possible* outputs.
This allows us to, for instance, summarise the history of close back vowels from Middle to Modern English as:
```
oː / [uː ʊ ʌ]
u / [ʌ ʊ]
```
(though the reality is [somewhat more complex](https://en.wikipedia.org/wiki/Phonological_history_of_English_close_back_vowels)).
This behaviour can also be forced by adding `@?` before a category:
  thus `[i u] / @? [i u]` will output both ⟨i⟩ and ⟨u⟩ for any high vowel in the input.

Rarely, Brassica will produce multiple results even when this is not explicitly specified.
This occurs when it detects that there are multiple valid ways to apply a rule to a word.
This situation comes up most often with optional elements in the target or environment.
For instance, if the rule `o (m) / u (ŋ)` is applied to the word ⟨tom⟩, then both ⟨tum⟩ and ⟨tuŋ⟩ are possible outputs,
  depending on whether ⟨m⟩ is included in the target or not.
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

### Features

Sound changes often manipulate suprasegmental features such as stress or tone.
An effective way of simulating this in Brassica is by treating phonemes with different features as separate graphemes.
For instance, unstressed and stressed vowels could be defined as follows:

```
categories
Vu = a e i o u
Vs = á é í ó ú

V = Vu Vs
end
```

This allows a rule for initial stress, say, to be written as `-1 Vu / Vs`.
However, this approach is problematic in that a grapheme such as `i`
  will now only refer to the unstressed phone.
To match both stressed and unstressed phones as is usually desired,
  one would need to write `[i í]`.
This is both verbose and easy to forget.
It *would* be possible to give each combination its own category name:

```
categories
A = a á
E = e é
I = i í
O = o ó
U = u ú
end
```

But this is tedious to type and not much less verbose.
Thus Brassica provides support for *features*.
After a feature is declared within a category block,
  one *base* grapheme can be used to match a collection of many *modified* graphemes.
Using features, the definitions above can be rewritten as

```
categories
feature Vu = a e i o u / Vs = á é í ó ú

V = Vu Vs
end
```

After this declaration, the grapheme `a` will be processed as if `[a á]` had been written,
  and similarly for the other graphemes listed in `Vu`.
To refer to the grapheme `a` alone, without matching `á`, Brassica provides the special syntax `a~`.
Note that this only comes into effect *after* the declaration:
  if a category such as `Front = e i` had been defined *before* the declaration,
  it would only match the unstressed phonemes.
Note also that each base grapheme has now effectively been redefined as a category:
  as such, a rule such as `i [k s] / [č š]` will no longer work as desired (because `[c š]` would be matched up with `i` i.e. `[i í]`),
  and would need to be written with the discard character as `i [k s] / ~ [č š]`.

More formally, the syntax for feature declaration is:

```
; NB this must be within a category definition block

feature (BaseCategory =) <category declaration> / ModifiedOne = <category declaration> (/ ModifiedTwo = <category declaration> / ...)
```

The base category name is optional, and any number of modified categories may be defined.
Each category specified in the declaration must have the same number of elements.

## User interface

### Output highlighting

Brassica’s interactive interfaces contain options to highlight output words which satisfy various conditions.
These options are located in the ‘output highlighting’ box near the right of the window.
The highlighting is updated every time the rules are applied.
(That is, every time the ‘apply’ button is pressed;
  or, with live previewing enabled, every time the input or rules are changed.)

The default option is ‘No highlighting’.
Select ‘Different to last run’ to highlight all words where
  the current application of the rules gave a different output to the last application of the rules.
This option can be useful when investigating the effect of a particular rule on the output:
  while this option is enabled, adding and removing the rule will highlight all words which are affected by this rule.

Select ‘Different to input’ to highlight all words which have been affected by at least one rule.
This option can be useful when prototyping a new set of sound changes:
  when conlanging, the ideal situation is one in which every word is affected by at least one rule,
  i.e. every word is highlighted.

Note that when ‘Different to input’ selected, some rules can cause undesirable highlighting.
For instance, it is often useful to use rules which remove romanisation conventions, mark syllable boundaries or similar.
Such rules tend to alter a large portion of the input;
  all those words are then highlighted even though, linguistically speaking, they have undergone no change.
For this reason, Brassica provides the `-x` flag.
If a word has only been affected by rules marked with `-x` rules, that word will not be highlighted.
That is, `-x` rules are treated by Brassica as effectively causing no change for the purposes of output highlighting.
It is recommended to annotate rules with `-x` when they cause only cosmetic change,
  so only rules corresponding to actual phonetic change will trigger output highlighting.

### MDF dictionaries

All examples above have involved input given as a simple list of words.
However, Brassica also allows input to be given in ‘Multi-Dictionary Formatter’ (MDF) notation,
  as generated by dictionary-editing programs such as [SIL Toolbox](https://software.sil.org/toolbox/).
(For details on the MDF format itself, see [Coward and Grimes’s original manual](https://web.archive.org/web/20211008115829/http://downloads.sil.org/legacy/shoebox/MDF_2000.pdf),
  or [SIL’s MDF documentation](http://www.fieldlinguiststoolbox.org/MDFDocumentation.zip) formatted for use with SIL Toolbox.)

To use MDF input files with the desktop interface, the ‘MDF file’ option must be enabled.
This option will automatically be enabled when an MDF file is opened using the ‘Open lexicon’ dialog box.
Similarly, enabling the option labeled ‘Wordlist + glosses’ will make Brassica treat the input as a simple wordlist with glosses.
MDF files can also be processed when using Brassica in batch mode from the command-line:
  this can be done by supplying the `--mdf` command-line argument.
  
Brassica has three output modes which are available when an MDF file is given as input.
On the desktop interface, Brassica will produce an MDF file as output when the ‘MDF output’ button is enabled.
‘MDF output with etymologies’ is similar, but will insert additional etymological information into the MDF output;
  see below for details on this.
(The corresponding command-line argument is `--etymons`.)
By contrast, when the ‘Wordlist’ button (or `--wordlist` command-line argument) is enabled, output is produced in quite a different format:
  instead of an MDF file, the output is a simple list of every derived word (similar to the output produced from non-MDF inputs).
This last option can be very useful when quickly experimenting with sound changes.
  
In MDF mode, Brassica will only apply sound changes to ‘vernacular language’ field values.
This includes such MDF fields as `\lx` ‘lexeme’, `\se` ‘subentry’, `\cf` ‘cross-reference’ and `\xv` ‘example’.
All other fields will be unaffected by sound changes.
Furthermore, glosses between square brackets are not recognised in MDF mode.
However, aside from these changes, Brassica operates in precisely the same way as previously detailed for wordlist inputs.
Note that after rule application, the resulting MDF file will usually need to be inspected and modified somewhat,
  to account for semantic changes, differences in sample sentences and other variations which Brassica is unable to simulate.

As previously mentioned, Brassica also allows etymological information to be inserted into the MDF files it produces.
This can be accomplished by selecting the ‘MDF output with etymologies’ button.
When this is selected, Brassica will add `\et` ‘etymology’ and `\eg` ‘etymology gloss’ fields for every main entry and subentry,
  which retain every input word and gloss exactly as they were in the input, before the sound changes were applied.
Thus, for instance, if the sound change `b o / u` is applied to the following MDF input file:

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
\et *bonab
\eg woman
\se unau
\ge girl
\et *bonabo
\eg girl
\dt 21/Apr/2022
```
Further modifications can then be made to the output while keeping the etymological information for reference.
(Note that Brassica adds an initial asterisk to all etymons;
  this is not currently configurable, but can easily be removed by hand.)

## Paradigm builder

Brassica includes an inbuilt paradigm builder.
It may be accessed using the ‘Tools⇒Paradigm Builder’ menu item in the graphical interface,
  or by visiting <https://bradrn.com/brassica/builder.html>.

The paradigm builder consists of three textboxes and a button.
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
