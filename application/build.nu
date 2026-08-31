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

# Update .ts translation files which can be updated with Qt Linguist.
#
# It will then generate the compiled files .qm.
# If you have edited the ts file, re-run this subcommand to update the .qm files.
def "main translations" [] {
    print "=== TRANSLATIONS ==="

    let langs = [fr es ru de ko it]
    let lupdate_cmd = match $nu.os-info.name {
        "linux" => "lupdate-pro",
        _ => "lupdate-pro.exe"
    }

    let langs = [fr es ru de]
    for $lang in $langs {
        ^$lupdate_cmd Core.pro -ts translations\d_lan_core.($lang).ts
        ^$lupdate_cmd Common/RemoteCoreController/RemoteCoreController.pro GUI.pro -ts translations\d_lan_gui.($lang).ts
    }

    for $project in [GUI Core] {
        mkdir ($project)/output/debug/languages
    }

    cd translations

    rm --force *.qm

    lrelease *.ts

    cp *gui*.qm ../GUI/output/debug/languages
    cp *core*.qm ../Core/output/debug/languages

    mkdir ../Setups/Windows/setup_bundle/languages
    cp *gui*.qm ../Setups/Windows/setup_bundle/languages
    cp *core*.qm ../Setups/Windows/setup_bundle/languages
}

def "main compile" [
    --clean # Clean all previous compiled files.
] {
    print "=== COMPILATION ==="

    update_version

    # To force to recompile the Common/Version.rs and DialogAbout.
    rm -f Core/.tmp/release/version_res.o
    rm -f GUI/.tmp/release/version_res.o
    rm -f GUI/.tmp/release/DialogAbout.o

    let projects = [
        Common
        Common/TestsCommon
        Common/LogManager
        Common/RemoteCoreController
        Core/HashCache
        Core/FileManager
        Core/FileManager/TestsFileManager
        Core/PeerManager
        Core/PeerManager/TestsPeerManager
        Core/UploadManager
        Core/DownloadManager
        Core/NetworkListener
        Core/ChatSystem
        Core/RemoteControlManager
        Core
        GUI
        Tools/PasswordHasher
    ]

    let nb_proc = sys cpu | length

    let os_config = (match $nu.os-info.name {
        "linux" => {
            {qmake_cmd: "qmake6",
                make_cmd: "make",
                spec: "linux-g++",
                makefile: "Makefile"}
        },
        _ => {
            {qmake_cmd: "qmake",
                make_cmd: "mingw32-make.exe",
                spec: "win32-clang-g++",
                makefile: "Makefile.Release"}
        }
    })

    for $project in $projects {
        let project_name = $project | split row '/' | last
        print "----------"
        print $"Compiling ($project)..."

        do {
            cd $project
            print $"Generating make file..."
            ^$os_config.qmake_cmd $"($project_name).pro" -r -spec $os_config.spec "CONFIG+=release"
            if $clean {
                print $"Cleaning..."
                ^$os_config.make_cmd -f $os_config.makefile clean -j($nb_proc)
            }
            print $"Compiling..."
            ^$os_config.make_cmd -f $os_config.makefile -w -j($nb_proc)
        }
    }
}

def update_version [] {
    cd Tools
    nu update_version.nu
}

def "main run-tests" [] {
    print "=== RUN TESTS ==="

    let exe_extension = ".exe" # No extension on Linux.

    let tests = [
        Common/TestsCommon/output/release/TestsCommon
        Core/FileManager/TestsFileManager/output/release/TestsFileManager
        Core/PeerManager/TestsPeerManager/output/release/TestsPeerManager
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

    cp ../../Core/output/release/D-LAN.Core.exe setup_bundle
    cp ../../GUI/output/release/D-LAN.GUI.exe setup_bundle
    cp ../../Tools/PasswordHasher/output/release/PasswordHasher.exe setup_bundle

    cd setup_bundle
    cp C:/Qt/Tools/llvm-mingw1706_64/bin/libwinpthread-1.dll .

    mkdir styles
    cp -r ../../../styles/* styles/
    cp -r ../../../GUI/resources/emoticons .

    windeployqt.exe --no-translations  PasswordHasher.exe D-LAN.Core.exe D-LAN.GUI.exe

    cd ..

    iscc windows_setup.iss
}
