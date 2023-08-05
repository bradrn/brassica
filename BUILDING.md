# Building Brassica

## Desktop

Brassica can be built using much the same process on both Windows and Linux.
(And probably Mac, though I haven’t tried it there.)
First, build the command-line interface with [Cabal](https://www.haskell.org/cabal/) (which can be installed using [GHCup](https://www.haskell.org/ghcup/)):
```
cabal build exe:brassica
```

Next, build the GUI interface, in `./gui/brassica-gui`.
Brassica uses [Qt](https://www.qt.io/),
  and relies on [CMake](https://cmake.org/) for building.
The CMake configuration uses `cabal` to find the location of the command-line executable,
  so make sure `cabal` is on your PATH first.
Especially on Windows, it may be easiest to open the project in [Qt Creator](https://www.qt.io/product/development-tools) and use that for building.

Alternately, you can use CMake directly.
On Linux this is simple:
```
cd gui/brassica-gui
cmake -S . -B ../build
cd ../build
make
```
On Windows it is more involved, since you need to set the appropriate paths, substituting `<version>` with your version of Qt:
```
cd gui/brassica-gui
set PATH=%PATH%;C:\Qt\Tools\Ninja\
C:\Qt\Tools\CMake_64\bin\cmake.exe -S . -B ../build -G Ninja -DCMAKE_BUILD_TYPE:String=Release -DQT_QMAKE_EXECUTABLE:STRING=C:/Qt/<version>/mingw81_64/bin/qmake.exe -DCMAKE_PREFIX_PATH:STRING=C:/Qt/<version>/mingw81_64 -DCMAKE_C_COMPILER:STRING=C:/Qt/Tools/mingw810_64/bin/gcc.exe -DCMAKE_CXX_COMPILER:STRING=C:/Qt/Tools/mingw810_64/bin/g++.exe
cd ../build
ninja
```

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
