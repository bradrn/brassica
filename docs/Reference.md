<!-- -*-GFM-*- -->

# Brassica: reference

This document descibes the syntax and application of Brassica sound changes.
If you’re new to Brassica, you probably want to read the [getting started guide](Getting-Started.md) instead.

```
Note: example statements will be specified throughout in code blocks such as these.
Their results will be specified as comments like:
; input word → output word.

Examples will assume the following category definitions:

categories noreplace
C = m n p t ch k b d j g f s sh h v z r l w y

-Stress = a e i o u
+Stress = á é í ó ú

auto -Stress

V = &&Stress
end
```

## Overall structure

### Statements

A Brassica sound change file comprises a list of **statements**,
  separated by one or more newlines.
Each statement can be:
- a [sound change](#sound-change-rules);
- a [category definition block](#category-definition-block);
- a [filter](#filters);
- an [extra graphemes declaration](#extra-graphemes); or
- a [report instruction](#reporting).

Additionally, **comments** may be specified as text after a semicolon.
Commented text is ignored when processing sound changes.

### Phases of processing

Brassica processes a sound change file by taking it through several phases:

1. Firstly, the file is **parsed**
     converting the written text into Brassica’s internal representation of sound changes.
2. Next those sound changes undergo **expansion**.
   Category definition blocks are removed,
     while graphemes which are given definitions in those blocks are replaced by categories or autosegments.
3. Finally the sound changes can be **applied** to an input word.
   Each statement is applied to the input word in order from top to bottom,
     producing zero, one or more output words.
   The output of each statement is used as the input to the next.

Additionally, before sound changes can be applied to file containing words,
  the words file must first be **tokenised** as follows:

1. Words to be affected by sound changes are distinguished from surrounding
     whitespace, glosses, field markers and other elements which should not be modified.
2. These words are then divided into **graphemes**.
   Each grapheme is a sequence of one or more Unicode characters,
     treated as a single unit for the purposes of sound changes.

Tokenisation is determined by
  the first [category definition block](#category-definition-block) in the sound changes file,
  and/or the first [extra graphemes declarations](#extra-graphemes).
Any **multigraphs** (graphemes with more than one character)
  which are listed in those two declarations
  are tokenised in words as a single grapheme.
All other characters are tokenised as single-character graphemes.
If multiple tokenisations are possible,
  Brassica will form the longest possible grapheme at each point in the word,
  from left to right.

### Word highlighting

The graphical frontends of Brassica provide an option to highlight output words when ‘Any rule applied’.
This highlighting occurs for all words which satisfy one of the following conditions:

- At least one sound change has successfully matched,
    where the input word has different graphemes to the output word,
    and the sound change was not flagged with `-x`.
- At least one grapheme in the word was replaced by a category definition block.

There is also an option to highlight output words when ‘Specific rule (-h) applied’.
This is similar to the previous option, but only considers sound changes flagged with `-h`:
  any other sound changes are considered irrelevant for highlighting purposes.

## Sound changes

### Sound change syntax

A sound change rule has the following syntax
  (using [EBNF as defined in the W3C XML standard](https://www.w3.org/TR/REC-xml/#sec-notation)):
```ebnf
SoundChange ::= Flag* Lexeme* ("/" | "→" | "->") Lexeme* ("/" Environment)* ("//" Environment)?
Flag ::= "-ltr" | "-rtl" | "-1" | "-?" | "-??" | "-x" | "-no"
Environment ::= Lexeme* "_" Lexeme*
```
The components of a sound change are named as follows:
- The **target** comprises the lexemes before the first separator (`/` or `→` or `->`).
  It denotes the part of the input word to be replaced with the replacement.
- The **replacement** comprises the lexemes after the first separator.
  It denotes what should be substituted for the target in the output word.
- Each **environment** comprises two lists of lexemes separated by an underscore (denoting the target).
  These specify the situations in which the target will be replaced by the replacement.
- An environment after `//` is an **exception**, specifying a situation in which the target may not be replaced by the replacement.

Each **lexeme** represents a certain part of an input or output word.
Lexemes are defined as follows:
```ebnf
Lexeme ::= Grapheme  [wfc: defined below]
         | InlineCategory
         | "@" [1-9] S (InlineCategory | Grapheme)
         | "@#" Grapheme S (InlineCategory | Grapheme)
         | "(" (S Grapheme)+ S ")"
         | ">"
         | "^" S Lexeme
         | Lexeme S "*"
         | Lexeme S "$" "-"? Grapheme ("#" Grapheme)? FeatureSpec?
         | "%" (InlineCategory | Grapheme)     [wfc: only in target or environment]
         | "%(" (S Grapheme)+ S ")"            [wfc: only in target or environment]
         | "@?" S (InlineCategory | Grapheme)  [wfc: only in replacement]
         | "~"                                 [wfc: only in replacement]
         | "\"                                 [wfc: only in replacement]

InlineCategory ::= "[" InnerCategory "]"
InnerCategory ::= (Grapheme | "+" Grapheme | "-" Grapheme | "&" Grapheme)+

FeatureSpec = "(" S (Grapheme ("~" Grapheme)+)* S ")"
```
Consecutive lexemes or graphemes must be separated by whitespace,
  except when omitting the whitespace is unambiguous.
Within a lexeme, `S` above denotes a place where whitespace can optionally be inserted.

The lexeme types listed above are as follows:

- A **grapheme**, indicating zero or more letters
  - **Word boundaries**, represented as `#`, are treated in many ways as a special kind of grapheme
  - Some graphemes may be treated as **predefined categories** or **autosegments**
      if a category definition block is present
- An **inline category**, indicating a set of graphemes or lexemes
- A **numeric backreference**, refering back to a previous category by position
- An **identifier backreference**, labelling or refering back to a category by identifier
- **Optional** lexemes, denoting a list of one or more lexemes to be optional
- A **geminate**, indicating duplication of the previous grapheme
- A **wildcard**, representing any number of graphemes until a following lexeme
- A **Kleene star**, representing **repetition** of a lexeme zero or more times
- A **phonetic feature**, indicating lists of graphemes grouped by common properties
- A **greedy category**, indicating a category which can be matched at most one time
- A **greedy optional**, indicating a set of graphemes which can be matched zero or one times but not both
- A **multiple outputs** declaration, indicating a category which specifies multiple outputs
- A **discard**, indicating a category which is absent in the output
- **Metathesis**, indicating the reverse of the target graphemes

See below for further details on each lexeme type.

### Sound change application

A sound change is applied to an input word using two basic operations
  (which will be defined later for each lexeme type):

- A lexeme can be **matched** to a portion of the input word.
  If the match succeeds, it can record information about the input word for later use.
- A lexeme can **produce** graphemes in the output word,
    possibly using information taken from matches in the target.

Matching occurs in the target, environment(s) and exception of a sound change.
Production occurs in the replacement of a sound change.

The sound change application algorithm is similar to that described by [Howard (1973:53-63)](https://dspace.mit.edu/bitstream/handle/1721.1/12982/26083289-MIT.pdf#page=70).
Simplifying slightly, for the usual case of left-to-right application, rule application occurs as follows:

1. Add word boundary graphemes `#` to the beginning and end of the word.
2. Initialise the current position to the beginning of the word.
3. If the sound change has an exception, and that exception matches the current position, go to the next position.
4. For each environment in the sound change, or for the environment `_` if no environment is specified:
    1. Match each lexeme before the underscore to the input word in sequence, from left to right.
    2. Match each lexeme in the target to the input word, in the same manner.
    3. Match each lexeme after the underscore to the input word, in the same manner.
5. If any of the previous matches did not succeed,
     reset the position to one grapheme after the original position, and go back to step **3**.
6. Otherwise:
   1. Produce the replacement graphemes by concatenating the graphemes
        produced by each lexeme in the replacement, from left to right.
   2. Find the graphemes in the input word which were matched by the target, and replace them with the new replacement graphemes.
   3. Move to the position at the start of the replaced graphemes and go back to step **3**.
7. When the position reaches the end of the word,
     remove the initial and final word boundary graphemes,
     and finish by returning the result as the output word.
 
At any point a matching or production operation may produce **multiple results**.
In this case, the application algorithm continues for each result independently.

The operation of this algorithm can be affected by the presence of rule flags, as follows:

- The `-ltr` flag does nothing (but explicitly specifies the usual left-to-right operation as above).

- The `-rtl` flag applies the rule from right to left.
  Rule application starts at the right edge of the rule and moves left, rather than *vice versa*.
  Similarly, lexemes are traversed from right to left within the sound change rule.

  (Note that this flips the usual meaning of lexemes such as `>` and wildcards!)
  
- `-1` causes rule application to terminate after the first successful match.

- `-no` applies a rule with *no overlap*:
    the target of one application may not be used as the environment of the next.
  This is accomplished by modifying step **6.3** above to move to the end of the replacement,
    rather than its start.

- `-?` causes the whole rule application to produce one additional result,
    which is identical to the input word.

- `-??` causes an additional result to be produced for each successful replacement,
    which is identical to the word before that replacement.

```brassica
C -Stress C V / C +Stress C V

; pa → pa (no change)
; pati → páti
; patiku → pátiku
; patikupu → pátikúpu
```

```brassica
-rtl C -Stress C V / C +Stress C V

; pa → pa (no change)
; pati → páti
; patiku → patíku
; patikupu → pátikúpu
```

```brassica
-1 -rtl C -Stress C V / C +Stress C V

; pa → pa (no change)
; pati → páti
; patiku → patíku
; patikupu → patikúpu
```

```brassica
e / i / i C _

; mide → midi
; midese → midisi
; midesenetake → midisinitake
```

```brassica
-no e / i / i C _

; mide → midi
; midese → midise
; midesenetake → midisenetake
```

### Graphemes

As mentioned [previously](#phases-of-processing), words are represented as lists of graphemes.
These graphemes form the basic unit of sound change in Brassica.
In particular, Brassica will never split up a multigraph
  except when specifically directed to do so by a sound change
  (e.g. `sh / s h`).
  
A grapheme is specified as a sequence of one or more non-whitespace characters.
Any printable character may occur within a category,
  with the exception of the reserved characters `#[](){}>\\→/_^%~*@$;`.
However, a grapheme may begin with `*`.

Within a sound change or category, a grapheme matches only that grapheme exactly.
It similarly will produce itself in the replacement.

Depending on a previous category definition block,
  a grapheme may be replaced by a category or autosegment during [expansion](#phases-of-processing).
To prevent this, the grapheme may be followed by a tilde `~`.
The tilde is not considered part of the grapheme itself.
  
The **word boundary** marker `#` is treated as a special grapheme.
As mentioned in the previous section,
  it is inserted at the beginning and end of every word before sound change application begins.
Otherwise, it receives no special treatment.

```brassica
sh / y / _ #

; as → as (no change)
; ah → ah (no change)
; ash → ay
; anish → aniy
; shash#shash → shay#shay
```


### Categories

A **category** is a list of graphemes or lexemes (**elements**)
  which are to be treated similarly within a sound change.
In brief, a category can match or produce any of its constituent elements.

Categories can be written in two ways in a sound change.
An **inline category** is written by placing a list of space-separated elements between square brackets.
Each element can be a single grapheme,
  or a sequence of zero or more lexemes placed between braces.
  
Alternatively, a category can be **predefined** in a [category definition block](#category-definition-block).
This associates graphemes with categories.
The process of [expansion](#phases-of-processing) will then
  replace each grapheme with the corresponding category,
  whenever one of those graphemes is mentioned.
  
(Note: as mentioned earlier, this guide assumes that `C`, `V`, `+Stress` and `-Stress` are predefined.)

Predefined categories can be combined with each other or with other graphemes or lexemes.
This is done by mentioning them in a subsequent category.
Category combinations are interpreted from left to right,
  using the first element of the category as the initial state of the category
  (no matter whether it is a predefined category, a grapheme or a list of lexemes).
The **category operation** can be specified by placing a character before the category name:

- `&Category` acts to **concatenate** `Category` with the preceding category.
  The result contains all the elements already present in the category,
    followed by all the elements of the second category.
- `+Category` acts to **intersect** `Category` with the preceding category.
  The result contains every element of the preceding category which is also present in `Category`,
    in the same order as `Category`.
- `-Category` acts to **subtract** `Category` with the preceding category.
  The result contains every element of the preceding category, preserving their order,
    except for those which are present in `Category`.

([Autosegments](#phonetic-features-and-autosegments) have somewhat special behaviour
  with intersection and subtraction: see the relevant section for details.)

If no character is present before the category name,
  the chosen operation depends on the category name:

- If the category name begins with `+` or `-`, intersection is used.
- Otherwise, concatenation is used.

Three more operations are defined for use with [phonetic features](#phonetic-features):
  
- `&&Feature` is equivalent to `&-Feature &+Feature`
- `+&Feature` is equivalent to intersection with the category defined by `[&+Feature &-Feature]`
- `-&Feature` is equivalent to subtraction with the category defined by `[&+Feature &-Feature]`

```brassica
. / [&&Stress]

; . → a/e/i/o/u/á/é/í/ó/ú
```

By these operations, each category is expanded to ultimately become a list of graphemes and lexemes.
In the description below, the **index** of a category element will refer to its position in the list:
  thus, first, second, third, etc.
  
An unmodified category (without backreferences of any kind) will match an input word
  if and only if at least one of its constituent graphemes or lexemes match that input.
When this occurs in the target, the index of the matched element
  is appended to a **list of matched category indices** maintained by the target.

If multiple elements of a target category can match, multiple results are produced, one for each element.
This behaviour can be disabled using a **greedy category**, written by prefixing the category with `%`.
Such a category will match at most one element,
  traversing its elements from left to right.

Each category in the replacement can take its value from a category matched in the target.
In the absence of backreferences, each category reads the next index in turn
  from the list of matched category indices gathered in the target,
  starting at the first item of that list.
The lexeme produced is that which is at the same index in the replacement category
  as that of the lexeme which were matched by the corresponding target category.
If the replacement category has no element at that index,
  the Unicode replacement character U+FFFD (�) is produced instead.

If there are more categories in the replacement than in the target,
  some replacement categories may not be able to read an index from the list of matched category indices.
In this case Brassica creates multiple output words: one for each element of the category.
Additionally, the previous behaviour can be forced for any category in the replacement
  by writing `@?` before the category, to create a lexeme with **multiple outputs**.
  
The **discard** character `~` in the replacement acts as a category which never produces output.
It causes one matched category index to be skipped,
  such that the next category without a backreference takes its value from the next category in the target.

```brassica
ə / [a~ e~]

; kəm → kam/kem
; kəmə → kama/kame/kema/keme
```

```brassica
[a b] [a b] [a b] / [x y] ~ [x y]

; aaa → xx
; aba → xx
; aab → xy
; abb → xy
; baa → yx
; bba → yx
; bab → yy
; bbb → yy
```

### Category backreferences

Backreferences allow explicitly specifying the correspondence between categories.
Two forms of backreference exist: identifier backreferences, and numeric backreferences.

An **identifier backreference** is written as `@#id` followed by a category,
  where the identifier `id` can be any grapheme except `#` which is not followed by a tilde.
Identifier backreferences extend over the entirety of a rule:
  any two categories with the same identifier
  must match or produce an element at the same index as each other.

```brassica
@#example [p t k] / ʔ / @#example [p t k] _ @#example [u i a]

; ppu → pʔu
; tti → tʔi
; kka → kʔa
; pta → pta (no change)
; kpu → kpu (no change)
```

```brassica
@#first [a b] [a b] @#second [a b] / @#first [x y] @#second [x y]

; aaa → xx
; aba → xx
; aab → xy
; abb → xy
; baa → yx
; bba → yx
; bab → yy
; bbb → yy
```

```brassica
@#stop [p t k] / @#stop [p t k] @#stop [f s x] / _ @#stop [i i u]

; api → apfi
; apu → apu (no change)
; ati → atsi
; atu → atu (no change)
; aki → aki (no change)
; aku → akxu
```

A **numeric backreference** is written as `@n` followed by a category,
  where `n` can be any number greater than 0.
Unlike identifier backreferences,
  the meaning of a numeric backreference depends on which sound change part it is in:
  
- In the target, a numeric backreference `@n Category`
    refers to the `n`th element of the list of matched category indices in the target.
  The `n`th element of `Category` must then match successfully.
- In an environment or exception, a backreference has the same meaning,
    except that it refers to the list of matched category indices
    in the environment or exception in which the backreference is placed.
  The scope of backreferences extends across the underscore representing the target.
- In the replacement, a numeric backreference `@n Category`
    refers to the `n`th element of the list of matched category indices in the *target* (not the replacement).
  The `n`th element of `Category` is then produced.
  
Note that, as lexemes are traversed from right to left in a sound change flagged as `-rtl`,
  numeric backreferences naturally operate in the same order in such rules.
  
```brassica
[m n ŋ] [b d g] / @2 [m n ŋ] @2 [b d g]

; anbe → ambe
; aŋde → ande
; amge → aŋge
```

### Optional lexemes

A sequence of lexemes can be made **optional** by surrounding them with parentheses.
When matching optional lexemes, two matches are attempted:
  one in which the optional lexemes are matched, and another in which the optional lexemes are ignored.
The whole match succeeds if either of these matches succeed.

Similarly to categories, a **list of matched optionals** is maintained by the target.
Each list element specifies whether the optional lexemes were matched or ignored.
If both matches succeeded, two output words are produced, one for each possibility.
A **greedy optional** supresses this by taking only the output word where the lexemes were matched.

Each optional block encountered in the replacement reads the next value from the list of matched optionals.
If that value states that the corresponding target optional is matched,
  then the lexemes in the replacement optional produce their graphemes as usual.
If it states otherwise, the replacement optional is ignored and produces no graphemes.
If there are more optionals in the replacement than there are in the target,
  then two results are produced for each optional which cannot read its value from the target:
  one where no graphemes are produced, and one where graphemes are produced.

```brassica
a / e / _ (C) i

; ai → ei
; ami → emi
; ammi → ammi (no change)
```

```brassica
[t d s] (y) i / [ch j sh] (i) ə

; ti → chə
; dyi → jiə
; sai → sai (no change)
```

```brassica
V V / V (ʔ) V

; ae → ae/aʔe
; iʔu → iʔu (no change)
```

### Unlimited repetition

Brassica contains two lexemes which can be used to represent unlimited repetition.
These are the wildcard `^` and the Kleene star `*`.

A **wildcard** lexeme takes the form `^l`, where `l` is any lexeme.
This matches zero or more graphemes, followed by `l`
  (at the first location where it is able to successfully match the input).
The match fails if `l` cannot be matched successfully at any point before the next word boundary.

As with categories and optionals, a **list of matched wildcards** is maintained by the target.
Each list element contains the graphemes matched by each wildcard in the target.
Each wildcard `^l` in the replacement
  then produces the same graphemes matched by the corresponding wildcard in the target,
  followed by producing the graphemes which result from `l`.
  
A **Kleene star** lexeme takes the form `l*`, where `l` is again any lexeme.
This matches zero or more repetitions of `l`.
As many repetititions as possible are matched.
This lexeme never fails to match.

A **list of matched Kleene stars** is maintained by the target.
Each list element specifies the number of times which the corresponding Kleene star lexeme was matched.
Each Kleene star `l*` in the replacement
  then produces the graphemes corresponding to `l`
  the same number of times as the corresponding Kleene star in the target was matched.
  
Note that the wildcard and Kleene star are opposites in important ways:

|          | Wildcard             | Star                           |
|----------|----------------------|--------------------------------|
| Matches… | Arbitrary graphemes  | Specific lexeme                |
| …until…  | Given lexeme matches | Given lexeme no longer matches |
  

```brassica
-Stress / +Stress / _ C* #

; eta → etá
; etap → etáp
; etaymbs → etáymbs
```

```brassica
[b d] / [m n] / _ ^ [m n]

; abenet → amenet
; adepitekem → anepitekem
```

```brassica
[a i u] ^[ä ï ü] / [ä ï ü] ^[a i u]

; antï → änti
; antepï → äntepi
```

### Phonetic features and autosegments

A phonetic feature defines lists of graphemes which correspond to each other.
As with categories, phonetic features may be inline or predefined.

An **inline feature** is specified as `l$Name(…)`, where:

- `l` is any lexeme (the **base lexeme** of the feature)
- `Name` is a grapheme which is not `#` or followed by `~`,
- `…` is a list of space-separated **correspondence sets**,
    each of which is a tilde-separated list of graphemes.
  Each correspondence set in a feature should have the same number of graphemes.

A **predefined feature** is specified as `l$Name`:
  that is, the same syntax, but without any explicitly specified feature values.
Instead, the feature values are taken from predefined categories, as follows:

- If categories named `-Name` and `+Name` exist,
    the correspondence sets consist of
    the first element of `-Name` with the first element of `+Name`,
    the second element of `-Name` with the second element of `+Name`,
    and so on.
- If there are two or more categories whose names begin with `+Name+`,
    the correspondence sets consist of
    the first elements of all these categories, then their second elements, and so on.
  The order of graphemes within a correspondence set is not explicitly specified,
    but is guaranteed to be consistent both within a feature and across features.
  (As of Brassica 1.0.0 they are arranged in ascending alphabetical order of the category names.)

The **values** of a feature are taken to cross-cut their correspondence sets.
A feature has two values if it has two elements in each correspondence,
  three values if it has three elements in each correspondence, and so on.
For instance, the inline feature `l$Voicing(p~b t~d k~g)` has two values,
  the first consisting of {`p`,`t`,`k`}, and the second consisting of {`b`,`d`,`g`}.
  
When matching a feature, the lexeme `l` is first matched.
Then, the last grapheme matched by `l` is compared to each of the values of that feature.
Any values which contain that grapheme are recorded.
If the grapheme is not contained in any values of that feature,
  a value of ‘indeterminate’ is recorded.

The target maintains one **list of matched features**
  for each distinct feature name which is mentioned.
For each feature in the target,
  the matched feature value is appended to the corresponding list.

Each feature in the replacement then takes its value
  from the list of matched features in the target,
  in the following way:
  
1. Graphemes are produced as normal from the base lexeme.
2. The next matched feature value is taken from the list of matched features
     which corresponds to the feature name.
3. The last grapheme produced by the base lexeme is altered
     to correspond to the matched feature value, as follows:
   - If the grapheme is already in the corresponding feature value of the replacement feature,
       no change occurs.
   - If it is in a different feature value of the replacement feature to that feature value which was matched,
       the appropriate member of its correspondence set is selected,
       such that the result has the same feature value as that which was matched.
   - If the matched feature value was ‘indeterminate’, multiple output words are produced,
       one for each member of its correspondence set.

As with categories, a feature may also be given an **identifier**,
  as `#id` immediately following the feature name.
Again the `id` may be any grapheme except `#` which is not followed by a tilde.
Two features with the same identifier always match or produce corresponding feature values.

A feature may be **negated** by writing `-` immediately after `$`.
This reverses the interpretation of the feature, as follows:
- In the replacement, it produces every grapheme of that correspondence set
    with a *different* feature value to that which was matched.
- When matching with an identifier, it can match any grapheme
    with a *different* feature value to that of the first grapheme matched with that identifier.
- Otherwise it has no effect when matching.

In addition, category definition blocks may cause graphemes to be defined as
  **autosegmental** with respect to a feature.
Effectively, an autosegmental grapheme acts as a feature with a single correspondence set:

- When matching, it matches any element of that correspondence set.
  In the target it appends the corresponding feature value to the appropriate list of matched features.
- In the replacement, it produces the element of its correspondence set
    which has the appropriate feature value.

Autosegmental graphemes can be included in a category definition,
  in which case they retain their autosegmental meaning as of the time of definition.

When an autosegmental grapheme is within a category,
  the set of graphemes which it can match or produce
  can additionally be affected by category operations.
If a grapheme is excluded by union or intersection when matching,
  that grapeme cannot be matched.
If a grapheme is similarly excluded in the replacement, the behaviour is more complex:
  if a situation occurs in which that grapheme would otherwise be produced,
  Brassica instead acts as if the feature were indeterminate,
  producing one result for each non-excluded output grapheme subsumed under the autosegment.

```brassica
C$Voice(p~b t~d k~g) C / C C$Voice(p~b t~d k~g)

; apte → apte (no change)
; apde → apte
; agpe → agbe
; anta → anta/anda
```

```brassica
V / V$Stress#spread / V$Stress#spread _

; táene → táéne
; sióna → siona
```

```brassica
a / e  ; note 'a' and 'e' are defined autosegmental with respect to $Stress

; tana → tene
; tána → téne
; taná → tené
; táná → téné
```

```brassica
; examples of autosegmental category operations:
; note that ±Long contain autosegmental graphemes

new categories noreplace
C = m n p t ch k b d j g f s sh h v z r l w y

+Tone+Low  = à ì ù àà ìì ùù
+Tone+Mid  = a i u aa ii uu
+Tone+High = á í ú áá íí úú

auto +Tone+Mid

-Long = a i u
+Long = aa ii uu

V = a i u aa ii uu
end

-x -Long > / +Long  ; fix multigraphs - automated test doesn't recognise these ones

[+Tone+High +Long] / [+Tone+High -Long]
[+Tone+Low -Long] / [+Tone+Low +Long]

. / [+Tone+High +Long]  ; check category membership also
, / [+Tone+High -Long]

; . → áá/íí/úú
; , → á/í/ú
; táásìnu → tásììnu
; tásììnuu → tásììnuu (no change)
```

```brassica
categories
+POA+Lab = p b f v
+POA+Alv = t d s z
+POA+Pal = ch j sh r
end

; stress test of negated features: heterorganic POA randomisation
C$-POA#poa / C$-POA#poa / C$POA#poa _

; apse → apse/apshe
; arbe → arde/arbe
; apfe → apfe (no change)
; adke → adke (no change)
```

### Other lexemes

The following lexemes exist in addition to those mentioned above:

- **Gemination** is represented by `>`.
  This matches or produces the same grapheme as that which was last matched or produced.

- **Metathesis** is represented by `\` in the replacement only.
  It produces in reversed order the graphemes which were matched in the target.

```brassica
C / / _ >

; atte → ate
; oshshe → oshe
```

```brassica
C ʔ / \ / V _

; namʔe → naʔme
; kanatʔ → kanaʔt
```

## Category definition block

### Overall structure and interpretation

A category definition block has the following syntax:

```ebnf
CategoryDefinitionBlock ::=
    "new"? "categories" "noreplace"? N (CategoryDefinition N)* "end"

CategoryDefinition ::= Grapheme "=" InnerCategory
                     | "auto" Grapheme
                     | "feature" FeatureDefinition
                     
FeatureDefinition ::=
    "feature" (Grapheme "=")? InnerCategory "/" (Grapheme "=" InnerCategory)+
```

(Where `N` represents an operating system-dependent line terminator,
  and `Grapheme` and `InnerCategory` are as defined [above](#sound-change-syntax).
Spaces must be included between elements except around `=`.)

A category definition block contains a list of definitions, each of which can be:

- a **category definition**, containing a grapheme,
    followed by `=` and then a [category](#categories);
- an **autosegment definition**, specifying the name of a category
    from which to produce [autosegments](#phonetic-features-and-autosegments); or
- a **feature definition**, listing a set of category definitions from which to create featural categories.

Each kind of definition introduces one or more graphemic **names**,
  each of which is associated with a single lexeme as its **value**.
For category definitions, the value is the category defined after the equals sign.
For other definitions, see below.

As with sound changes, category definitions are traversed in order from top to bottom.
When each definition is encountered, it is added to the global set of definitions,
  overwriting a previous definition with the same name if one exists.
However, if a category definition block begins with the word `new`,
  all previous definitions are removed before category definition block is processed.

During [expansion](#phases-of-processing),
  whenever a grapheme is encountered which is the name of a definition active at that point,
  that grapheme is replaced with its value.
This occurs in all sound changes, category definitions and filters.
As mentioned [earlier](#graphemes),
  this replacement can be disabled by following the grapheme with a tilde `~`.

During sound change application, category definition blocks act to filter graphemes.
Brassica collects the graphemes
  which are mentioned in every definition value
  active at the point immediately following the category definition block,
  as well as those mentioned in any previous [extra graphemes declarations](#extra-graphemes).
Then, for each word, the graphemes in that word are compared to this list of collected graphemes.
Any grapheme which is not in the list is replaced by U+FFFD (�).
This behaviour can be disabled by introducing the category definition block with `noreplace`.

Additionally, the first category block in the file influences tokenisation into multigraphs,
  as [described above](#phases-of-processing).

### Autosegment definitions

An autosegmental definition takes the form `auto`, followed by a category name.
The category name should be that of a category
  which is available for use as a [predefined feature](#phonetic-features-and-autosegments):
  that is, it should have form `-Name` or `+Name`, or begin with `+Name+`.

Each grapheme in the listed category
  is given a definition as [autosegmental](#phonetic-features-and-autosegments).
The correspondence set of that autosegmental grapheme contains
  all the graphemes of the other categories with the same feature name.

Note that, as with other kinds of definitions,
  no more than one autosegmental definition can be given for any given grapheme.
An attempt to create nested autosegmental definitions will cause an error, as of Brassica 1.0.0.

For a concrete example, consider:
```
+Tone+High = á é í ó ú
+Tone+Mid = a e i o u
+Tone+Low = à è ì ò ù

auto +Tone+Mid
```
This will define the elements of `+Tone+Mid` as autosegmental, as follows:
- `a` is defined as autosegmental with correspondence set {`á`, `à`, `a`}
- `e` is defined as autosegmental with correspondence set {`é`, `è`, `e`}
- `i` is defined as autosegmental with correspondence set {`í`, `ì`, `i`}
- `o` is defined as autosegmental with correspondence set {`ó`, `ò`, `o`}
- `u` is defined as autosegmental with correspondence set {`ú`, `ù`, `u`}

### Feature definitions

Note: **feature definitions are deprecated**
  in favour of [autosegment definitions](#autosegment-definitions).

To expand on the previous EBNF definition,
  a feature definition has the following form:
```
feature (C₁ =) <category₁> (/ Cₙ = <categoryₙ>)+
```
Here, a subscript `ₙ` (or superscript `ᵐ`) is used to represent some arbitrary number of elements in a list.

Let each `<categoryₙ>` have elements `cₙ¹`, `cₙ²`, etc., up to `cₙᵐ`
Then, a feature definition as above creates the same category definitions as if the following had been written:
```
C₁ = <category₁>
(Cₙ = <categoryₙ>)+

c₁¹ = c₁¹ cₙ¹+
(c₁ᵐ = c₁ᵐ cₙᵐ+)+
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

## Other statements

### Filters

A **filter** is a statement which deletes output words matching some criterion.
A filter is defined by writing `filter`, followed by a sequence of lexemes as in the target of a rule.
The filter will then remove from the output any words which match those lexemes.

```brassica
filter V V

; kane → kane (no change)
; kaene → (deleted)
```

### Extra graphemes

An **extra graphemes** declaration takes the form of `extra` followed by a list of graphemes.
It defines a list of graphemes which are not part of any category
  (and hence not mentioned in any category definition block),
  but nonetheless should never be replaced by U+FFFD by any category definition block.
Each `extra` declaration overwrites any previous one.

(See [category definition blocks](#category-definition-block) for further details.)

### Reporting

A **report** instruction is simply the word `report` placed on a line of its own.
This will cause Brassica to record all output words at that point in the rules file.
These intermediate results will be displayed
  if selecting ‘Input→output’ format in a graphical interface,
  or `--show-input` in the CLI.
