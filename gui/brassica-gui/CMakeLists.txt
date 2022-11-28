cmake_minimum_required(VERSION 3.5)

project(brassica-gui LANGUAGES CXX)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(CMAKE_AUTOUIC ON)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(Qt6 COMPONENTS Widgets Core5Compat REQUIRED)

set(BRASSICA_INTEROP_DIR ${CMAKE_CURRENT_LIST_DIR}/../brassica-interop/ CACHE PATH "Path to /gui directory of Brassica source code")
if(WIN32)
    find_program(HAS_GHCUP "ghcup")
    if(HAS_GHCUP)
        execute_process(COMMAND ghcup whereis -d ghc OUTPUT_VARIABLE GHC_BIN_DIR OUTPUT_STRIP_TRAILING_WHITESPACE)
    else() # try Stack if no GHCup
        execute_process(COMMAND stack path --compiler-bin OUTPUT_VARIABLE GHC_BIN_DIR OUTPUT_STRIP_TRAILING_WHITESPACE)
    endif()
    set(GHC_INCLUDE_DIR ${GHC_BIN_DIR}/../lib/include CACHE PATH "Path to includes directory of GHC")
elseif(LINUX)
    execute_process(COMMAND /home/bradrn/.ghcup/bin/ghc --print-libdir    OUTPUT_VARIABLE GHC_LIB_DIR OUTPUT_STRIP_TRAILING_WHITESPACE)
    execute_process(COMMAND /home/bradrn/.ghcup/bin/ghc --numeric-version OUTPUT_VARIABLE GHC_VERSION OUTPUT_STRIP_TRAILING_WHITESPACE)
    set(GHC_INCLUDE_DIR ${GHC_LIB_DIR}/include CACHE PATH "Path to includes directory of GHC")
endif()

include_directories(${BRASSICA_INTEROP_DIR}/stub ${GHC_INCLUDE_DIR})

if(LINUX)
    add_library(brassica-interop SHARED IMPORTED)
    set_target_properties(brassica-interop PROPERTIES IMPORTED_LOCATION ${BRASSICA_INTEROP_DIR}/libbrassica-interop.so)
endif()

add_executable(brassica-gui WIN32
  main.cpp
  mainwindow.cpp
  mainwindow.h
  ruleshighlighter.cpp
  ruleshighlighter.h
  paradigmwindow.cpp
  paradigmwindow.h
  settingsdialog.cpp
  settingsdialog.h
  settings.cpp
  settings.h
  resources.rc
#  brassicaprocess.h
#  brassicaprocess.cpp
)

if(WIN32)
    target_link_libraries(brassica-gui PRIVATE ${BRASSICA_INTEROP_DIR}/brassica-interop.a Qt::Widgets Qt::Core5Compat ws2_32 psapi dbghelp winmm)
elseif(LINUX)
    target_link_libraries(brassica-gui PRIVATE brassica-interop ${GHC_LIB_DIR}/rts/libHSrts-ghc${GHC_VERSION}.so Qt::Widgets Qt::Core5Compat)
endif()
