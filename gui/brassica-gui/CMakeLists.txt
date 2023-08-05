cmake_minimum_required(VERSION 3.5)

project(brassica-gui LANGUAGES CXX)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(CMAKE_AUTOUIC ON)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(Qt6 COMPONENTS Widgets Core5Compat REQUIRED)
qt_standard_project_setup()

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
  brassicaprocess.h
  brassicaprocess.cpp
)

target_link_libraries(brassica-gui
    PRIVATE Qt::Widgets Qt::Core5Compat)

if(WIN32)
    execute_process(COMMAND cabal exec -- where brassica OUTPUT_VARIABLE BRASSICA_CLI OUTPUT_STRIP_TRAILING_WHITESPACE)
    cmake_path(NATIVE_PATH CMAKE_CURRENT_BINARY_DIR CMAKE_CURRENT_BINARY_DIR_WIN)
    add_custom_command(
        TARGET brassica-gui
        POST_BUILD
        COMMAND copy ${BRASSICA_CLI} "${CMAKE_CURRENT_BINARY_DIR_WIN}\\brassica"
        DEPENDS ${BRASSICA_CLI})
elseif(UNIX)
    execute_process(COMMAND cabal exec -- which brassica OUTPUT_VARIABLE BRASSICA_CLI OUTPUT_STRIP_TRAILING_WHITESPACE)
    add_custom_command(
        TARGET brassica-gui
        POST_BUILD
        COMMAND cp ${BRASSICA_CLI} "${CMAKE_CURRENT_BINARY_DIR}/brassica"
        DEPENDS ${BRASSICA_CLI})
endif()

install(TARGETS brassica-gui
    BUNDLE DESTINATION .
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
