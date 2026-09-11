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

    let release_directory = get_release_directory
    if $release_directory == null {
        print "Compilation aborted"
        return
    }

    print $"Release directory: ($release_directory)"

    update_version

    # To force to recompile the Common/Version.rs and DialogAbout.
    # rm -f build/release/GUI/CMakeFiles/DLanGUI.dir/__/Common/version.rc.obj
    # rm -f build/release/Core/CMakeFiles/DLanCore.dir/__/Common/version.rc.obj
    # rm -f build/release/GUI/CMakeFiles/DLanGUI.dir/DialogAbout.cpp.obj

    cmake --build $release_directory --parallel

    if $clean {
        cmake --build $release_directory --target clean
    }

    cmake --build $release_directory
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
        build/release/output/TestsLogManager
        build/release/output/TestsFileManager
        build/release/output/TestsFilePool
        build/release/output/TestsHashCache
        build/release/output/TestsPeerManager
        build/release/output/TestsUploadManager
        build/release/output/TestsDownloadManager
        build/release/output/TestsNetworkListener
        build/release/output/TestsRemoteCoreController
        build/release/output/TestsRemoteControlManager
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

    match $nu.os-info.name {
        "window" => { make_windows_setup }
        "linux" => { make_linux_app_image }
        other => { print $"Unsupported OS: $other" }
    }
}

def make_windows_setup [] {
    let release_directory = get_release_directory
    if $release_directory == null {
        print "Setup building aborted: can't find the release directory"
        return
    }

    cd Setups/Windows
    mkdir setup_bundle

    cp ../../$release_directory/output/D-LAN.Core.exe setup_bundle
    cp ../../$release_directory/output/D-LAN.GUI.exe setup_bundle
    cp ../../$release_directory/output/PasswordHasher.exe setup_bundle

    cd setup_bundle
    cp C:/Qt/Tools/llvm-mingw1706_64/bin/libwinpthread-1.dll .

    mkdir styles
    cp -r ../../../styles/* styles/
    cp -r ../../../GUI/resources/emoticons .

    windeployqt.exe --no-translations  PasswordHasher.exe D-LAN.Core.exe D-LAN.GUI.exe

    cd ..

    iscc windows_setup.iss
}

def make_linux_app_image [] {
    # TODO
}

def get_release_directory [] {
    let release_directories = ls build | where name =~ Release
    if ($release_directories | is-empty) {
        print "Cannot find the release directory, try to configure a release build with Qt Creator"
        return null
    }

    $release_directories | first | get name
}