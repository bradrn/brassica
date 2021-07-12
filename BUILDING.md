# Building Brassica

## Windows

To build the command-line interface alone, you will need the [Haskell Stack](https://docs.haskellstack.org/en/stable/README/).
(Cabal should also work, but I highly recommend using Stack, especially if you intend to go on and build the GUI.)
Simply run `mkdir bin`, then `stack install brassica --local-bin-path bin`.
The command-line executable `brassica.exe` should now be present in `./bin`.

The GUI interface is more complex to build.
First install the GUI library [Qt](https://www.qt.io/).
To build the Haskell–C++ interoperation library, run `stack build brassica-interop`.
This should produce files `./gui/brassica-interop/brassica-interop.a` and `./gui/brassica-interop/stub/BrassicaInterop_stub.h`.
Next locate the copy of Ninja which came with Qt (on my machine it is at `C:\Qt\Tools\Ninja\`).
Now run the following commands, substituting the appropriate paths as required:

```
> cd gui/brassica-gui
> set PATH=%PATH%;C:\Qt\Tools\Ninja\
> C:\Qt\Tools\CMake_64\bin\cmake.exe -S . -B ../../bin -G Ninja -DCMAKE_BUILD_TYPE:String=Release -DQT_QMAKE_EXECUTABLE:STRING=C:/Qt/5.15.1/mingw81_64/bin/qmake.exe -DCMAKE_PREFIX_PATH:STRING=C:/Qt/5.15.1/mingw81_64 -DCMAKE_C_COMPILER:STRING=C:/Qt/Tools/mingw810_64/bin/gcc.exe -DCMAKE_CXX_COMPILER:STRING=C:/Qt/Tools/mingw810_64/bin/g++.ex
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

## Other platforms

I have not yet attempted to build Brassica outside Windows,
  however I anticipate that the above instructions or a variant thereof should work successfully.
If you manage to build Brassica on another platform,
  please tell me how you did it so I can add it to this document!
