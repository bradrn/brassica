# Building Brassica

## Desktop

### Building

Brassica can be built using much the same process on both Windows and Linux.
(And probably Mac, though I haven’t tried it there.)
First, build the command-line interface with [Cabal](https://www.haskell.org/cabal/) (which can be installed using [GHCup](https://www.haskell.org/ghcup/)):
```
cabal build exe:brassica
```

Next, build the GUI interface, in `./gui/brassica-gui`.
Brassica uses [Qt](https://www.qt.io/) version 6,
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
On Windows it is more involved, since you need to set the appropriate paths, substituting `<version>` as appropriate:
```
cd gui/brassica-gui
set PATH=%PATH%;C:\Qt\Tools\Ninja\;C:\Qt\Tools\mingw1120_64\bin
C:\Qt\Tools\CMake_64\bin\cmake.exe -S . -B ../build -G Ninja -DCMAKE_BUILD_TYPE:String=Release -DQT_QMAKE_EXECUTABLE:STRING=C:/Qt/<version>/mingw81_64/bin/qmake.exe -DCMAKE_PREFIX_PATH:STRING=C:/Qt/<version>/mingw_64 -DCMAKE_C_COMPILER:STRING=C:/Qt/Tools/mingw<version>/bin/gcc.exe -DCMAKE_CXX_COMPILER:STRING=C:/Qt/Tools/mingw<version>/bin/g++.exe
cd ../build
ninja
```

### Deployment

On Linux, the preferred deployment method is using [AppImages](https://appimage.org/).
First download [linuxdeploy](https://github.com/linuxdeploy/linuxdeploy) and [linuxdeploy-plugin-qt](https://github.com/linuxdeploy/linuxdeploy-plugin-qt).
Then run `cabal build`, `cmake` and `make` as described above.
Then, to build the AppImage, run:
```
linuxdeploy-x86_64.AppImage --executable ./brassica-gui --appdir AppDir -d ../brassica-gui/brassica.desktop -i ../brassica-gui/brassica.png --plugin qt
cp ./brassica AppDir/usr/bin
linuxdeploy-x86_64.AppImage --appdir AppDir --output appimage
```
(You may need to `export QMAKE=/usr/bin/qmake6` if you’re using a distribution such as Debian where Qt 5 is the default.)

Similarly the CLI can be deployed to an AppImage with something like:
```
linuxdeploy-x86_64.AppImage --executable brassica --appdir AppDirCLI path/to/brassica-gui/brassica.desktop -i path/to/brassica-gui/brassica.png --output appimage
```

On Windows, first copy `brassica.exe` and `brassica-gui.exe` into `.\deploy`, and `.\examples` into `.\deploy\examples`.
Then use [`windeployqt`](https://doc.qt.io/qt-6/windows-deployment.html) on `brassica-gui.exe` to copy over the relevant files for Qt.
Finally, use [NSIS](https://nsis.sourceforge.io/Main_Page) with the given `installer.nsi` to generate an installer.

## Online version

Building the online version of Brassica is slightly more difficult as it requires the WebAssembly backend of GHC.
It is easiest to get this using GHCup’s [WASM cross bindists](https://www.haskell.org/ghcup/guide/#cross-support).
You will also need [Wizer](https://github.com/bytecodealliance/wizer) to pre-initialise the WASM binary,
  and [npm](https://www.npmjs.com/) for JavaScript package management.

Then:

1. In `./gui/brassica-interop-wasm`, run the following commands:
   ```
   cabal build --project-file=cabal-wasm.project brassica-interop-wasm
   wizer --allow-wasi --wasm-bulk-memory true "$(cabal --project-file=cabal-wasm.project list-bin -v0 brassica-interop-wasm)" -o "./dist/brassica-interop-wasm.wasm"
   ```
   This will create a file `./gui/brassica-interop-wasm/dist/brassica-interop-wasm.wasm` containing the WASM binary.

   (Note: it can also be convenient to set `CABAL_DIR` so that WASM packages are installed to a different location.)

2. In `./gui/brassica-web`, run `mkdir dist; ./cpfiles` to copy the asset files into `dist`.
   Note that you’ll need to redo this every time you update a static file (HTML or CSS or WASM)!
3. Install JavaScript dependencies using `npm install`.

   Now you can use [webpack](https://webpack.js.org/) to bundle the JavaScript files.
   For instance, use `npx webpack serve` to run a development server,
     or `npx webpack --mode=production` to prepare a release.
