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

    let release_directory = get_release_directory

    # Extracts the strings from the sources into the .ts files ('update_translations' is
    # the global target created by the 'qt_add_lupdate' calls in CMakeLists.txt) then
    # compiles them into .qm files (built in 'build/release').
    cmake --build $release_directory --target update_translations
    cmake --build $release_directory --target dlan_translations

    for $project in [GUI Core] {
        mkdir ($project)/output/debug/languages
    }

    cp ($release_directory)/*gui*.qm GUI/output/debug/languages
    cp ($release_directory)/*core*.qm Core/output/debug/languages

    mkdir Setups/Windows/setup_bundle/languages
    cp ($release_directory)/*gui*.qm Setups/Windows/setup_bundle/languages
    cp ($release_directory)/*core*.qm Setups/Windows/setup_bundle/languages
}

def "main compile" [
    --clean # Clean all previous compiled files.
] {
    print "=== COMPILATION ==="

    let release_directory = get_release_directory

    print $"Release directory: ($release_directory)"

    update_version

    # To force to recompile the Common/Version.rs and DialogAbout.
    # rm -f build/release/GUI/CMakeFiles/DLanGUI.dir/__/Common/version.rc.obj
    # rm -f build/release/Core/CMakeFiles/DLanCore.dir/__/Common/version.rc.obj
    # rm -f build/release/GUI/CMakeFiles/DLanGUI.dir/DialogAbout.cpp.obj

    if $clean {
        cmake --build $release_directory --target clean --parallel
    }

    cmake --build $release_directory --parallel
}

def update_version [] {
    cd Tools
    nu update_version.nu
}

def "main run-tests" [] {
    print "=== RUN TESTS ==="

    let release_directory = get_release_directory

    let exe_extension = ".exe" # No extension on Linux.

    let tests = [
        ($release_directory)/output/TestsCommon
        ($release_directory)/output/TestsSortedList
        ($release_directory)/output/TestsChatSystem
        ($release_directory)/output/TestsLogManager
        ($release_directory)/output/TestsFileManager
        ($release_directory)/output/TestsFilePool
        ($release_directory)/output/TestsHashCache
        ($release_directory)/output/TestsPeerManager
        ($release_directory)/output/TestsUploadManager
        ($release_directory)/output/TestsDownloadManager
        ($release_directory)/output/TestsNetworkListener
        ($release_directory)/output/TestsRemoteCoreController
        ($release_directory)/output/TestsRemoteControlManager
        ($release_directory)/output/TestsDownloadsTreeModel
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
        "windows" => { make_windows_setup }
        "linux" => { make_linux_app_image }
        $other => { print $"Unsupported OS: ($other)" }
    }
}

def make_windows_setup [] {
    let release_directory = get_release_directory

    cd Setups/Windows
    mkdir setup_bundle

    cp ../../($release_directory)/output/D-LAN.Core.exe setup_bundle
    cp ../../($release_directory)/output/D-LAN.GUI.exe setup_bundle
    cp ../../($release_directory)/output/PasswordHasher.exe setup_bundle

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
    let release_directory = get_release_directory
    let release_directory = $release_directory | path expand
    let application_directory = pwd
    let architecture = match $nu.os-info.arch {
        "x86_64" => "x86_64"
        "aarch64" => "aarch64"
        $other => { error make {msg: $"Unsupported AppImage architecture: ($other)"} }
    }

    # Use the same Qt SDK as the selected build, not an unrelated system Qt.
    # QMAKE can override discovery for distribution-specific Qt layouts.
    let qt_directory = (open --raw ($release_directory | path join "CMakeCache.txt")
        | lines | parse "Qt6_DIR:PATH={directory}" | get directory | first)
    let qmake = $env.QMAKE? | default ($qt_directory | path join "../../../bin/qmake" | path expand)
    if not ($qmake | path exists) {
        error make {msg: "Cannot find Qt's qmake. Set QMAKE to the qmake executable matching this build."}
    }

    for executable in [D-LAN.GUI D-LAN.Core] {
        if not ($release_directory | path join "output" $executable | path exists) {
            error make {msg: $"Missing ($executable); build the Release configuration first."}
        }
    }

    # Compile translations without updating the source .ts files.
    cmake --build $release_directory --target dlan_translations
    if $env.LAST_EXIT_CODE != 0 { error make {msg: "Could not build translations; Qt LinguistTools is required."} }
    let translations = glob ($release_directory | path join "d_lan_*.qm")
    if ($translations | is-empty) { error make {msg: "No compiled D-LAN translations found"} }

    let tools_directory = $application_directory | path join "build/appimage-tools" $architecture
    mkdir $tools_directory
    for tool in [linuxdeploy linuxdeploy-plugin-qt] {
        let filename = $"($tool)-($architecture).AppImage"
        let destination = $tools_directory | path join $filename
        if not ($destination | path exists) {
            let download = $"($destination).download"
            curl --fail --location --retry 3 --output $download $"https://github.com/linuxdeploy/($tool)/releases/download/continuous/($filename)"
            if $env.LAST_EXIT_CODE != 0 { error make {msg: $"Could not download ($tool)"} }
            mv -f $download $destination
        }
        chmod +x $destination
        if $env.LAST_EXIT_CODE != 0 { error make {msg: $"Could not make ($tool) executable"} }
    }

    let appdir = $application_directory | path join "build/appimage/D-LAN.AppDir"
    if ($appdir | path exists) { rm -rf $appdir }
    let bin_directory = $appdir | path join "usr/bin"
    mkdir $bin_directory
    for executable in [D-LAN.GUI D-LAN.Core] {
        cp ($release_directory | path join "output" $executable) $bin_directory
    }
    mkdir ($bin_directory | path join "languages")
    cp ...$translations ($bin_directory | path join "languages")
    cp -r styles ($bin_directory | path join "styles")
    cp -r GUI/resources/emoticons ($bin_directory | path join "emoticons")
    mkdir ($appdir | path join "usr/share/licenses/d-lan")
    cp ../COPYING ($appdir | path join "usr/share/licenses/d-lan/COPYING")

    let desktop = $appdir | path join "d-lan.desktop"
    open --raw Setups/Ubuntu/d-lan.desktop
        | str replace "Exec=d-lan-gui" "Exec=D-LAN.GUI"
        | save $desktop
    let icon = $appdir | path join "d-lan.svg"
    cp GUI/resources/icon.svg $icon

    # Include whichever Wayland platform plugins this Qt version provides.
    let plugins = (do { ^$qmake -query QT_INSTALL_PLUGINS } | complete)
    if $plugins.exit_code != 0 { error make {msg: "Could not query Qt plugin directory"} }
    let wayland_plugins = (glob ($plugins.stdout | str trim | path join "platforms/libqwayland*.so")
        | each {|plugin| $plugin | path basename } | str join ";")
    let version = (open --raw Common/Version.h | lines
        | parse '#define VERSION "{version}"' | get version | first)
    let tag = (open --raw Common/Version.h | lines
        | parse '#define VERSION_TAG "{tag}"' | get tag | first)
    let package_version = if ($tag | is-empty) { $version } else { $"($version)-($tag)" }
    let output_directory = $application_directory | path join "Setups/AppImage"
    mkdir $output_directory
    let output = $output_directory | path join $"D-LAN-($package_version)-($architecture).AppImage"
    let linuxdeploy = $tools_directory | path join $"linuxdeploy-($architecture).AppImage"
    let qt_plugin = $tools_directory | path join $"linuxdeploy-plugin-qt-($architecture).AppImage"
    # D-LAN uses SQLite only. Other SDK SQL plugins may need unavailable
    # database client libraries, so exclude them before dependency scanning.
    let sql_exclusions = (glob ($plugins.stdout | str trim | path join "sqldrivers/libqsql*.so")
        | where {|plugin| ($plugin | path basename) != "libqsqlite.so" }
        | each {|plugin| ["--exclude-library" ($plugin | path basename)] } | flatten)

    # Extract-and-run allows packaging on build hosts without FUSE.
    # linuxdeploy includes the AppImage output plugin in its own AppImage.
    with-env {
        QMAKE: $qmake
        EXTRA_PLATFORM_PLUGINS: $wayland_plugins
        APPIMAGE_EXTRACT_AND_RUN: "1"
        VERSION: $package_version
        OUTPUT: $output
    } {
        cd $output_directory
        ^$linuxdeploy --appdir $appdir --executable ($bin_directory | path join "D-LAN.GUI") --executable ($bin_directory | path join "D-LAN.Core") --desktop-file $desktop --icon-file $icon
        if $env.LAST_EXIT_CODE != 0 { error make {msg: "AppImage dependency deployment failed"} }
        ^$qt_plugin --appdir $appdir ...$sql_exclusions
        if $env.LAST_EXIT_CODE != 0 { error make {msg: "Qt plugin deployment failed"} }

        # Qt's GTK integration supplies the desktop palette on Cinnamon/GNOME.
        # linuxdeploy-plugin-qt does not deploy it automatically. Use the same
        # SDK as the application and scan its dependencies at its plugin location.
        let gtk_theme = $plugins.stdout | str trim | path join "platformthemes/libqgtk3.so"
        if ($gtk_theme | path exists) {
            let theme_directory = $appdir | path join "usr/plugins/platformthemes"
            mkdir $theme_directory
            cp $gtk_theme $theme_directory
            ^$linuxdeploy --appdir $appdir --deploy-deps-only ($theme_directory | path join "libqgtk3.so")
            if $env.LAST_EXIT_CODE != 0 { error make {msg: "GTK theme integration deployment failed"} }
        } else {
            print --stderr "Warning: this Qt SDK has no GTK theme plugin; GTK desktop colors may not be available."
        }
        ^$linuxdeploy --appdir $appdir --output appimage
        if $env.LAST_EXIT_CODE != 0 { error make {msg: "AppImage packaging failed"} }
    }
    if not ($output | path exists) { error make {msg: "Packaging did not produce the expected AppImage"} }
    print $"Created ($output)"
}

def get_release_directory [] {
    let release_directories = ls build | where name =~ Release
    if ($release_directories | is-empty) {
        error make {msg:"Cannot find the release directory, try to configure a release build with Qt Creator"}
    }

    $release_directories | first | get name
}
