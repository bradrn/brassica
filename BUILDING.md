# Building Brassica

## Windows

To build the command-line interface alone, you can use the [Haskell Stack](https://docs.haskellstack.org/en/stable/README/).
Simply run `mkdir bin`, then `stack install brassica --local-bin-path bin`.
The command-line executable `brassica.exe` should now be present in `./bin`.
The equivalent Cabal commands should also work.

The GUI interface is more complex to build.
First install the GUI library [Qt](https://www.qt.io/).
To build the Haskell–C++ interoperation library, run `stack build brassica-interop`.
This should produce files `./gui/brassica-interop/brassica-interop.a` and `./gui/brassica-interop/stub/BrassicaInterop_stub.h`.
Next locate the copy of Ninja which came with Qt (on my machine it is at `C:\Qt\Tools\Ninja\`).
Now run the following commands, substituting the appropriate paths as required:

```
> cd gui/brassica-gui
> set PATH=%PATH%;C:\Qt\Tools\Ninja\
> C:\Qt\Tools\CMake_64\bin\cmake.exe -S . -B ../build -G Ninja -DCMAKE_BUILD_TYPE:String=Release -DQT_QMAKE_EXECUTABLE:STRING=C:/Qt/5.15.1/mingw81_64/bin/qmake.exe -DCMAKE_PREFIX_PATH:STRING=C:/Qt/5.15.1/mingw81_64 -DCMAKE_C_COMPILER:STRING=C:/Qt/Tools/mingw810_64/bin/gcc.exe -DCMAKE_CXX_COMPILER:STRING=C:/Qt/Tools/mingw810_64/bin/g++.exe
> cd ../build
> ninja
> copy brassica-gui.exe ..\..\bin
> cd ../..
```

(Alternately, you can simply open `./gui/brassica-gui` in Qt Creator and copy the executable to `./bin`.
But this method gives more control over where all the files go.)

Running the commands above should give an executable `./bin/brassica-gui.exe`.
However, this will not yet run as a standalone program.
To copy the required dynamic libraries and other files, run the following command
  (again substituting the appropriate path):

```
> cd bin
> C:\Qt\5.15.1\mingw81_64\bin\windeployqt.exe brassica-gui.exe
> cd ..
```

Now you should be able to run the GUI interface using `./bin/brassica-gui.exe`.
(To create an installer from these files, an [NSIS](https://nsis.sourceforge.io/Main_Page) installer file is provided in `installer.nsi`.)

## Online version

Building the online version of Brassica is more difficult as it uses GHCJS and Reflex.
Accordingly the [Nix](https://nixos.org/) package manager is required for building.
First, install Nix if it isn’t present already.
Next ensure that [`reflex-platform`](https://github.com/reflex-frp/reflex-platform) has been cloned as a submodule of this repository;
  if not then run `git submodule init && git submodule update`.
Then run `reflex-platform/try-reflex` to download and install GHCJS and Reflex.
(Warning: this can take a while.)
When finished that command should drop into a Nix shell, which can be immediately `exit`ed from.
After this, run either of the following two commands to build:
```bash
# to build as a warp-enabled webserver with GHC, into ./dist-ghc/:
nix-shell -A shells.ghc --run "cabal --project-file=cabal-web-ghc.project --builddir=dist-ghc build brassica-web"
# to build as a webpage with GHCJS, into ./dist-ghcjs/:
nix-shell -A shells.ghcjs --run "cabal --project-file=cabal-web-ghcjs.project --builddir=dist-ghcjs build brassica-web"
```
For more details consult the [`reflex-platform` project development guide](https://github.com/reflex-frp/reflex-platform/blob/ac66356c8839d1dc16cc60887c2db5988a60e6c4/docs/project-development.rst).

## Other platforms

I have not yet attempted to build Brassica outside Windows,
  however I anticipate that the above instructions or a variant thereof should work successfully.
If you manage to build Brassica on another platform,
  please tell me how you did it so I can add it to this document!
