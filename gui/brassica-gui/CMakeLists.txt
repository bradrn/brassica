cmake_minimum_required(VERSION 3.5)

project(brassica-gui LANGUAGES CXX)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(CMAKE_AUTOUIC ON)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(QT NAMES Qt6 Qt5 COMPONENTS Widgets REQUIRED)
find_package(Qt${QT_VERSION_MAJOR} COMPONENTS Widgets REQUIRED)

set(BRASSICA_INTEROP_DIR ${CMAKE_CURRENT_LIST_DIR}/../brassica-interop/ CACHE PATH "Path to /gui directory of Brassica source code")
execute_process(COMMAND stack path --compiler-bin OUTPUT_VARIABLE GHC_BIN_DIR OUTPUT_STRIP_TRAILING_WHITESPACE)
set(GHC_INCLUDE_DIR ${GHC_BIN_DIR}/../lib/include CACHE PATH "Path to includes directory of GHC")

include_directories(${BRASSICA_INTEROP_DIR}/stub ${GHC_INCLUDE_DIR})

add_executable(brassica-gui
  main.cpp
  mainwindow.cpp
  mainwindow.h
  ruleshighlighter.cpp
  ruleshighlighter.h
#  brassicaprocess.h
#  brassicaprocess.cpp
)

target_link_libraries(brassica-gui PRIVATE ${BRASSICA_INTEROP_DIR}/brassica-interop.a Qt${QT_VERSION_MAJOR}::Widgets ws2_32 psapi dbghelp winmm)
