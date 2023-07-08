# Building Brassica

## Windows

To build Brassica, it is recommended to use [Cabal](https://www.haskell.org/cabal/).
This can be installed most easily using [GHCup](https://www.haskell.org/ghcup/).

To build the command-line interface run the following command:
```
> cabal install exe:brassica --installdir=bin
```
The command-line executable `brassica.exe` should now be present in `./bin`.
(Note that depending on your version of GHC, you may have to remove `brassica-web` from the `cabal.project`,
  since its dependency `reflex-platform` requires a fairly old version of `base` to compile.)

The GUI interface is more complex to build.
First install the GUI library [Qt](https://www.qt.io/).
To build the Haskell–C++ interoperation library, run `cabal build brassica-interop`.
This should produce files `./gui/brassica-interop/brassica-interop.a` and `./gui/brassica-interop/stub/BrassicaInterop_stub.h`.
Next the C++ project at `./bin/brassica-gui.exe` need to be built.
This is perhaps easiest to do using [Qt Creator](https://www.qt.io/product/development-tools).
Alternately, it can be built from the command line using the following commands,
  substituting the appropriate paths for CMake, Qt, Ninja and the build directory as required:
```
> cd gui/brassica-gui
> set PATH=%PATH%;C:\Qt\Tools\Ninja\
> C:\Qt\Tools\CMake_64\bin\cmake.exe -S . -B ../build -G Ninja -DCMAKE_BUILD_TYPE:String=Release -DQT_QMAKE_EXECUTABLE:STRING=C:/Qt/5.15.1/mingw81_64/bin/qmake.exe -DCMAKE_PREFIX_PATH:STRING=C:/Qt/5.15.1/mingw81_64 -DCMAKE_C_COMPILER:STRING=C:/Qt/Tools/mingw810_64/bin/gcc.exe -DCMAKE_CXX_COMPILER:STRING=C:/Qt/Tools/mingw810_64/bin/g++.exe
> cd ../build
> ninja
```
[`windeployqt`](https://doc.qt.io/qt-5/windows-deployment.html) can then be used to copy over the appropriate dynamic libraries etc.

For deployment, move the two executables `brassica.exe` and `brassica-gui.exe` to the folder `./bin`;
  also move the files copied by `windeployqt` to the same place.
The [NSIS](https://nsis.sourceforge.io/Main_Page) installer file provided in `installer.nsi` can then be used to create an installer.

## Linux


The same procedure as on Windows should suffice to build the CLI, and the Haskell–C++ interoperation library.
After this, it is simple to build the GUI from the command-line:
```
$ cmake -S . -B ../build
$ cd ../build
$ make
```
Using Qt Creator is more difficult, as GHCup usually installs GHC outside the global PATH.
It may thus be necessary to edit the `CMakeLists.txt` to hard-code the path to `ghc`
  (on my machine at `/home/<user>/.ghcup/bin/ghc`),
  then regenerate the CMake configuration using ‘Build→Clear CMake Configuration’ followed by ‘Build→Run CMake’.
Either way, an executable `brassica-gui` should be generated in the appropriate directory.

The situation is slightly more complicated if the GUI interface needs to be distributed more widely.
Cabal hard-codes the [RUNPATH](https://en.wikipedia.org/wiki/Rpath) of the executable
  so that it can find its Haskell dependencies elsewhere in the filesystem.
To fix this, the libraries must be distributed alongside the executable, and the RUNPATH altered to refer to their location.
I find [`patchelf`](https://github.com/NixOS/patchelf) (available from many distros) useful for the latter purpose.
The following commands deploy the executable in `./deploy`, and its required libraries in `./deploy/lib`:

```
$ mkdir deploy
$ cp path/to/brassica-gui deploy
$ cd deploy
$ mkdir lib
$ cp path/to/brassica-interop/libbrassica-interop.so lib
$ ldd brassica-gui | grep ghc | sed -E 's/\s+[a-zA-Z0-9.-]+ => ([^ ]+) .+/\1/' | xargs -I{} cp "{}" lib
$ patchelf --set-rpath '$ORIGIN/lib' brassica-gui
$ ls lib | xargs -I{} patchelf --set-rpath '$ORIGIN' '{}'
```

It should then be possible to distribute `./deploy` freely.

## Online version

Building the online version of Brassica is slightly more difficult as it requires the WebAssembly backend of GHC.
You will also need [Wizer](https://github.com/bytecodealliance/wizer) to pre-initialise the WASM binary,
  and [npm](https://www.npmjs.com/) for JavaScript package management.

Follow the instructions at [ghc-wasm-meta](https://gitlab.haskell.org/ghc/ghc-wasm-meta) to install this version.
Then:

1. In `./gui/brassica-interop-wasm`, run the following commands:
   ```
   wasm32-wasi-cabal build brassica-interop-wasm
   wizer --allow-wasi --wasm-bulk-memory true "$(wasm32-wasi-cabal list-bin -v0 brassica-interop-wasm)" -o "./dist/brassica-interop-wasm.wasm"
   ```
   This will create a file `./gui/brassica-interop-wasm/dist/brassica-interop-wasm.wasm` containing the WASM binary.
2. In `./gui/brassica-web`, run the following commands to copy the required files into `./gui/brassica-web/static`:
   ```
   npm install
   npx webpack
   cp ../brassica-interop-wasm/dist/brassica-interop-wasm.wasm static/
   cp -r ../../examples/ static/
   ```
3. To test Brassica, you can now run a webserver in `./gui/brassica-web/static`,
     for instance using `python -m http.server`.

## Other platforms

I have not yet attempted to build Brassica outside Windows or Linux,
  however I anticipate that the above instructions or a variant thereof should work successfully.
If you manage to build Brassica on another platform,
  please tell me how you did it so I can add it to this document!
