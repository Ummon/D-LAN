#!/usr/bin/env nu

def main [--clean] {
    main build-all --clean=$clean
}

def "main build-all" [--clean] {
    print "=== BUILD ALL ==="
    main translations
    main compile --clean=$clean
    main run-tests
    main make-setup
}

def "main translations" [] {
    print "=== TRANSLATIONS ==="

    let langs = [fr es ru de]
    for $lang in $langs {
        lupdate-pro.exe Core.pro -ts translations\d_lan_core.($lang).ts
        lupdate-pro.exe Common\RemoteCoreController\RemoteCoreController.pro GUI.pro -ts translations\d_lan_gui.($lang).ts
    }

    for $project in [GUI Core] {
        mkdir ($project)/output/debug/languages
    }

    cd translations

    rm --force *.qm

    lrelease *.ts

    cp *gui*.qm ../GUI/output/debug/languages
    cp *core*.qm ../Core/output/debug/languages
}

def "main compile" [--clean] {
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

    for $project in $projects {
        let project_name = $project | split row '/' | last
        print "----------"
        print $"Compiling ($project)..."

        do {
            cd $project
            if $clean {
                mingw32-make.exe -f Makefile.Release clean -j($nb_proc)
            }
            qmake $"($project_name).pro" -r -spec win32-clang-g++ "CONFIG+=release"
            mingw32-make.exe -f Makefile.Release -w -j($nb_proc)
        }

        # qmake -makefile Common.pro -r "CONFIG+=release"
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
    print "make setup"

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

    # cd ../temp_setup
    # windeployqt.exe --no-translations  PasswordHasher.exe D-LAN.Core.exe D-LAN.GUI.exe
}
