# Brassica architecture
(plus some general advice on calling statically linked Haskell from C)

## Overview

At a high level Brassica is divided into three components:

- **Haskell**: core logic and CLI executable
- **Haskell/C interop**: FFI declarations compiled to static library
- **C++**: GUI (in Qt), calls into Haskell code

Brassica is structured in this way out of a necessity to use Qt:
  all GUI libraries with good Haskell bindings also have terrible support for custom keyboards,
  which was a key requirement for Brassica.
On the other hand, by the time I discovered this, the core logic had already been written in Haskell.
Hence, the logic is written in Haskell, the GUI is written in C++,
  and the latter is compiled into an executable which calls the former.

## Haskell

The Haskell code is organised in a fairly standard manner: a library with accompanying executable.
All core logic resides within the library component,
  while the executable `brassica` adds a CLI interface.
The library is also intended to be usable as a standalone package
  (though as of the time of writing it has not yet been uploaded to Hackage).
For this purpose the most important modules are `Brassica.SoundChange`, which gives a high-level interface,
  and `Brassica.SoundChange.Parse` and `Brassica.SoundChange.Apply`, giving a lower-level interface to the sound change code.

## Haskell/C interop

This component (in `./gui/brassica-interop`) consists of FFI wrappers around the pure Haskell code,
  along with some logic for easier integration with the GUI.
(I should probably move this logic into the library component at some point.)

The design and building of this component is perhaps somewhat unusual compared to most other online examples of Haskell/C interop.
Whereas most other examples focus on calling C from Haskell, this is a Haskell component called by a C program.
(Well, C++ really, but it works the same way given that C++ is a near-superset of C.)
Broadly speaking, three methods are available to accomplish this:
  interprocess communication, dynamic linking and static linking.
The first can be brittle and the second is unavailable on Windows,
  so this component is built as a static library to be linked into C.
This is accomplished using GHC options `-stubdir stub -o brassica-interop.a -static -optl-static -staticlib`.
(Explanation: `-stubdir stub` places stub header files in a directory `stub`,
  `-o brassica-interop.a` names the output static library,
  and I’m not quite sure exactly what all of `-static -optl-static -staticlib` do,
  or even if they’re all necessary.)
This component consists only of one module `BrassicaInterop.hs` containing foreign exports,
  because GHC [seems to choke](https://www.reddit.com/r/haskell/comments/mlsjmm/ghcexe_cant_apply_o_to_multiple_source_files/)
    on creating a static library with two or more modules.

The exact design of the foreign interface also deserves comment.
Only a limited set of types may be passed between C and Haskell with any degree of ease:
  numeric types, booleans, strings, and to some extent arrays and pointers (`Ptr` and `StablePtr`).
Accordingly, the exported functions take and return only these types.
In particular, all results from the SCA are passed back to C having already been formatted into `CString`s.
(Specifically, they are rendered into HTML for viewing with a `QTextEdit`.)
Due to the heavily multilingual nature of Brassica,
  care is taken to interpret and create all strings in UTF-8 using `GHC.Foreign` —
  the `CString` functions in `Foreign.C` are not sufficient for this purpose.
For passing more complex types back and forth, `StablePtr` is helpful.
`StablePtr`s become opaque void pointers on the C side,
  so any manipulation, construction or pattern matching needs to be done in Haskell functions exported to C.
Mutation is accomplished using a `StablePtr` containing an `IORef`.
Tuples can’t be passed back to C,
  so the easiest way to return multiple values is to allocate a pointer in C,
  pass it to Haskell as a `Ptr`, and let the Haskell code mutate it;
  however this proved to be unnecessary for Brassica.
There are probably more principled ways to accomplish all this,
  but `StablePtr`s (as well as `Ptr`s in other programs) proved sufficient for my purposes.

### C++

The C++ component (`./gui/brassica-gui`) is for the most part a fairly standard Qt program.
The main curiosities concern linking with the Haskell static library.
In addition to linking with `brassica-interop.a`,
  I also found it important to link `ws2_32`, `psapi`, `dbghelp` and `winmm`.
(Thanks to @bgamari and /u/Illustrious-Bet-4548 for help with this!)
Additionally the compiler must be able to find the stub header file and `HsFFI.h` (for me in `<ghc-directory>/../lib/include`),
All this is implemented in the CMake build config,
  using `stack path --compiler-bin` to find the appropriate paths.

Once all the above is done correctly,
  using Haskell functions from C++ is as easy as using C++ functions.
The program must start by calling `hs_init(&argc, &argv)` and finish by calling `hs_exit()` (both from `HsFFI.h`),
  but otherwise there are few to no restrictions.
In theory one should call `hs_free_stable_ptr(HsStablePtr sp)` after finishing with a `StablePtr` so that it can be garbage collected;
  on the other hand, Brassica doesn’t do this, and it doesn’t seem to have caused any problems yet.
(Though I’m sure it’s only a matter of time…)
In larger programs I have found it useful to wrap up each Haskell type into a C++ class holding a reference to the relevant `StablePtr`.
The class is then responsible for calling the appropriate Haskell functions and freeing the pointer after use,
  insulating the rest of the code from these requirements.
However, since Brassica mostly passes simple types like `CString`s, this is not necessary here.
