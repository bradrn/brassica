# Brassica

[![Hackage](https://img.shields.io/hackage/v/brassica.svg?logo=haskell)](https://hackage.haskell.org/package/brassica)

Brassica is a new sound change applier.
Its features include:

- Can be used interactively both [online](https://bradrn.com/brassica/index.html) and as a desktop application, or non-interactively in batch mode on the command-line or as a [Haskell library](https://hackage.haskell.org/package/brassica)
- Natively supports the MDF dictionary format, also used by tools including [SIL Toolbox](https://software.sil.org/toolbox/) and [Lexique Pro](https://software.sil.org/lexiquepro/)
- First-class support for multigraphs
- Easy control over rule application: apply sound changes sporadically, right-to-left, between words, and in many more ways
- Live preview and control over output highlighting let you try out sound changes quickly and easily
- Highlight and visualise results in numerous ways
- Category operations allow phonetic rules to be written in both featural and character-based ways
- Support for ‘features’ lets rules easily manipulate stress, tone and other suprasegmentals
- Comes with a [paradigm builder](https://bradrn.com/brassica/builder.html) for quickly investigating inflectional and other patterns
- Rich syntax for specifying phonetic rules, including wildcards, optional elements and more

And many more!

See the [documentation](./Documentation.md) for details on Brassica usage.

Download Brassica from the [releases page](https://github.com/bradrn/brassica/releases/latest).
Alternately, try it online at http://bradrn.com/brassica.
As of the time of writing prebuilt binaries exist for Windows and Linux.
Instructions for building from source are available at [`BUILDING.md`](./BUILDING.md).

![Image of Brassica with some example sound changes](https://raw.githubusercontent.com/bradrn/brassica/v0.1.0/gui-interface-example.png)
