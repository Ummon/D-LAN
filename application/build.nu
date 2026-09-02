#!/usr/bin/env nu

# By default will build everything and clean all previous build.
#
# See the build-all subcommand.
def main [] {
    main build-all --clean
}

# Build everything, it will not clean by default.
def "main build-all" [
    --clean # Clean all previous compiled files.
] {
    print "=== BUILD ALL ==="
    main translations
    main compile --clean=$clean
    main run-tests
    main make-setup
}

# Update the .ts translation files which can be edited with Qt Linguist.
#
# It will then generate the compiled files .qm.
# If you have edited a ts file, re-run this subcommand to update the .qm files.
def "main translations" [] {
    print "=== TRANSLATIONS ==="

    configure

    # Extracts the strings from the sources into the .ts files ('update_translations' is
    # the global target created by the 'qt_add_lupdate' calls in CMakeLists.txt) then
    # compiles them into .qm files (built in 'build/release').
    cmake --build build/release --target update_translations
    cmake --build build/release --target dlan_translations

    for $project in [GUI Core] {
        mkdir ($project)/output/debug/languages
    }

    cp build/release/*gui*.qm GUI/output/debug/languages
    cp build/release/*core*.qm Core/output/debug/languages

    mkdir Setups/Windows/setup_bundle/languages
    cp build/release/*gui*.qm Setups/Windows/setup_bundle/languages
    cp build/release/*core*.qm Setups/Windows/setup_bundle/languages
}

def "main compile" [
    --clean # Clean all previous compiled files.
] {
    print "=== COMPILATION ==="

    update_version

    # To force to recompile the Common/Version.rs and DialogAbout.
    # rm -f build/release/GUI/CMakeFiles/DLanGUI.dir/__/Common/version.rc.obj
    # rm -f build/release/Core/CMakeFiles/DLanCore.dir/__/Common/version.rc.obj
    # rm -f build/release/GUI/CMakeFiles/DLanGUI.dir/DialogAbout.cpp.obj

    configure
    cmake --build build/release
}

def configure [] {
    cmake -S . -B build/release -G Ninja -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_C_COMPILER=clang -DCMAKE_RC_COMPILER=llvm-rc
}

def update_version [] {
    cd Tools
    nu update_version.nu
}

def "main run-tests" [] {
    print "=== RUN TESTS ==="

    let exe_extension = ".exe" # No extension on Linux.

    let tests = [
        build/release/output/TestsCommon
        build/release/output/TestsFileManager
        build/release/output/TestsPeerManager
        build/release/output/TestsNetworkListener
    ]

    for $test in $tests {
        print $"Executing ($test)"
        do {
            cd ($test | path dirname)
            ./($test | path basename)
        }
    }

    print "All tests finished successfully"
}

def "main make-setup" [] {
    print "=== MAKE SETUP ==="

    cd Setups/Windows
    mkdir setup_bundle

    cp ../../build/release/output/D-LAN.Core.exe setup_bundle
    cp ../../build/release/output/D-LAN.GUI.exe setup_bundle
    cp ../../build/release/output/PasswordHasher.exe setup_bundle

    cd setup_bundle
    cp C:/Qt/Tools/llvm-mingw1706_64/bin/libwinpthread-1.dll .

    mkdir styles
    cp -r ../../../styles/* styles/
    cp -r ../../../GUI/resources/emoticons .

    windeployqt.exe --no-translations  PasswordHasher.exe D-LAN.Core.exe D-LAN.GUI.exe

    cd ..

    iscc windows_setup.iss
}
