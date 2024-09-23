<!-- -*-GFM-*- -->

# Brassica paradigm builder

## Using the paradigm builder

Brassica includes a **paradigm builder** for creating inflectional paradigm sets.
It may be accessed from any of [Brassica’s interfaces](./Using-Brassica.md#versions-of-brassica),
  as follows:

- On desktop, open the ‘Tools⇒Paradigm Builder’ menu item
- On web, visit <https://bradrn.com/brassica/builder.html>,
- On the command-line, run `brassica-pb`
- In the Haskell library, use the modules under `Brassica.Paradigm`

On desktop and web, the paradigm builder consists of three textboxes and a button.
The leftmost textbox contains a **paradigm definition**, using the syntax described below.
The middle textbox contains a list of **roots** on which the paradigm should be built.
Each root should be specified on its own line.
When the **Build** button is pressed,
  the rightmost textbox will contain the output of the paradigm builder,
  which can then be copied and pasted into another window or application.
A menu at the top of the window provides options to open and save the paradigm and the lexicon.

## Paradigm definitions

A basic paradigm definition is fairly simple.
An example is as follows:

```
() 2.en
1.wim () 1.soo 1.aa
-2.zhaa -2.woo -2.yaa
-1.zh -1.w -1.y
```

Each line of the paradigm definition specifies a *grammatical feature* — a set of mutually exclusive affixes.
These are given as a list, separated by spaces.
As is usual in templatic morphology, each prefix and suffix is assigned a **slot number**.
For prefixes, `-1` is the slot closest to the root, `-2` is the slot before that, and so on.
For suffixes, `1` is the slot closest to the root, `2` is the slot after that, and so on.
Thus each affix can be:

- A **prefix** in slot *n*, specified as `-n.prefix`;
- A **suffix** in slot *n*, specified as `n.suffix`;
- A **zero affix**, specified as `()`.

Two further options are available:

- A **combination** of other affixes, specified as `(affix1 affix2 ...)`; or
- An **abstract feature**, specified simply as `featurename` (more on this below).

The output will then iterate through all valid combinations of these affixes,
  formatted with spaces and newlines between them.
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

In the paradigm described above, note that all affixes on the same line are assigned the same slot.
The paradigm builder has a shortcut syntax for this situation,
  in which the slot is specified at the beginning of the line:

```
() 2.en
1 wim () soo aa
-2 zhaa woo yaa
-1 zh w y
```

If any affix later in the line specifies its own slot, that overrides the slot at the beginning of the line.

Grammatical features can be given **names**.
These are placed at the beginning of the line after a slot number, followed by `=`:

```
NEG = () 2.en
1 TA = wim () soo aa
-2 ABS = zhaa woo yaa
-1 ERG = zh w y
```

If a grammatical feature is given a name, it can subsequently be used in a **condition**.
This specifies where two different grammatical features can or cannot co-occur.
A condition takes the form `when (<feature name> <operator> <affix>)`,
  where `<operator>` is either `is` or `not`.
The output will contain the following feature only in words where the condition is satisfied;
  in all other words that feature will be absent.

For instance, in the following example,
  a verb with the antipassive never takes an ergative cross-referencing marker:

```
NEG = () 2.en
ANTIP = () 1.el
2 TA = wim () soo aa
when (ANTIP not 1.el) -2 ABS = zhaa woo yaa
-1 ERG = zh w y
```


Earlier, brief reference was made to **abstract features**.
These are similar to affixes, but are not associated with any one slot.
Instead, one or more abstract features can be **mapped** to an affix,
  by writing on a separate line:
```
feature1 feature2 … > affix
```
The `affix` will then be included in the result whenever all of the specified features are present.
This allows for implementing fusional morphology.

For instance, the following example describes the Latin first declension:
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
The first two lines describe the grammatical features in question.
The other lines map those features to concrete suffixes.
When applied to, for instance, `puell`, this produces:
```
puella puellae
puella puellae
puellam puellās
puellae puellārum
puellae puellīs
puellā puellīs
```
