# Brassica

Brassica is a new sound change applier.
Its features include:

- Can be used interactively both [online](http://bradrn.com/brassica/index.html) and as a desktop application, or non-interactively in batch mode on the command-line or as a Haskell library
- Natively supports the MDF dictionary format, also used by tools including [SIL Toolbox](https://software.sil.org/toolbox/) and [Lexique Pro](https://software.sil.org/lexiquepro/)
- First-class support for multigraphs
- Easy control over rule application: apply sound changes sporadically, right-to-left, and in many more ways
- Live preview and control over output highlighting allows fast iteration through rules
- Category operations allow phonetic rules to be written in both featural and character-based ways
- Support for ‘features’ lets rules easily manipulate stress, tone and other suprasegmentals
- Comes with a paradigm builder for quickly investigating inflectional and other patterns
- Rich syntax for specifying phonetic rules, including wildcards, optional elements and more

And many more!

See the [documentation](./Documentation.md) for details on Brassica usage.

Download Brassica from the [releases page](https://github.com/bradrn/brassica/releases/latest).
Alternately, try it online at http://bradrn.com/brassica.
As of the time of writing prebuilt binaries exist only for Windows.
Instructions for building from source are available at [`BUILDING.md`](./BUILDING.md).

![Image of Brassica with some example sound changes](https://raw.githubusercontent.com/bradrn/brassica/v0.0.3/gui-interface-example.png)
