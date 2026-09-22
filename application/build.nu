#!/usr/bin/env nu

# By default will build everything and clean all previous build.
#
# See the build-all subcommand.
def main [
    --build-dir: path
    --jobs (-j): int = 8
] {
    main build-all --clean --build-dir=$build_dir --jobs=$jobs
}

# Build everything, it will not clean by default.
def "main build-all" [
    --clean # Clean all previous compiled files.
    --build-dir: path # Configured Release build; auto-detected when omitted.
    --jobs (-j): int = 8
] {
    if $nu.os-info.name == "macos" {
        main release-test --clean=$clean --build-dir=$build_dir --jobs=$jobs
        main make-setup --build-dir=$build_dir
        return
    }
    print "=== BUILD ALL ==="
    main translations --build-dir=$build_dir
    main compile --clean=$clean --build-dir=$build_dir --jobs=$jobs
    main run-tests --build-dir=$build_dir
    main make-setup --build-dir=$build_dir
}

# Build and test a configured Release tree without packaging or editing .ts files.
def "main release-test" [
    --build-dir: path # Defaults to the sole Release build under build/.
    --jobs (-j): int = 8 # Maximum parallel build jobs.
    --clean
] {
    let release_directory = get_release_directory $build_dir
    require_tests $release_directory
    main compile --clean=$clean --build-dir=$release_directory --jobs=$jobs
    run_checked cmake --build $release_directory --config Release --target dlan_translations
    main run-tests --build-dir=$release_directory
    print "Release build and tests completed successfully"
}

# Update Common/Version.h:
# - git revision to HEAD
# - Date and time
def "main update-version" [] {
   update_version
}

# Update the .ts translation files which can be edited with Qt Linguist.
#
# It will then generate the compiled files .qm.
# If you have edited a ts file, re-run this subcommand to update the .qm files.
def "main translations" [--build-dir: path] {
    print "=== TRANSLATIONS ==="

    let release_directory = get_release_directory $build_dir

    # Extracts the strings from the sources into the .ts files ('update_translations' is
    # the global target created by the 'qt_add_lupdate' calls in CMakeLists.txt) then
    # compiles them into .qm files (built in 'build/release').
    run_checked cmake --build $release_directory --config Release --target update_translations
    run_checked cmake --build $release_directory --config Release --target dlan_translations

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
    --jobs (-j): int = 8
    --build-dir: path
] {
    if $jobs < 1 { error make {msg: "--jobs must be at least 1"} }
    print "=== COMPILATION ==="

    let release_directory = get_release_directory $build_dir

    print $"Release directory: ($release_directory)"

    # update_version Need to be done manually now.

    # To force to recompile the Common/Version.rs and DialogAbout.
    # rm -f build/release/GUI/CMakeFiles/DLanGUI.dir/__/Common/version.rc.obj
    # rm -f build/release/Core/CMakeFiles/DLanCore.dir/__/Common/version.rc.obj
    # rm -f build/release/GUI/CMakeFiles/DLanGUI.dir/DialogAbout.cpp.obj

    if $clean {
        run_checked cmake --build $release_directory --config Release --target clean --parallel ($jobs | into string)
    }

    run_checked cmake --build $release_directory --config Release --parallel ($jobs | into string)
}

def update_version [] {
    cd Tools
    nu update_version.nu
}

# Use CTest's registered commands, environments, timeouts and working directories.
def "main run-tests" [--build-dir: path] {
    print "=== RUN TESTS ==="
    let release_directory = get_release_directory $build_dir
    require_tests $release_directory
    run_checked ctest --test-dir $release_directory --build-config Release --parallel 1 --output-on-failure --no-tests=error --timeout 300
    print "All tests finished successfully"
}

def "main make-setup" [--build-dir: path] {
    print "=== MAKE SETUP ==="
    match $nu.os-info.name {
        "windows" => { make_windows_setup $build_dir }
        "linux" => { make_linux_app_image $build_dir }
        "macos" => { make_macos_app $build_dir }
        $other => { error make {msg: $"Unsupported OS: ($other)"} }
    }
}

# Package only Apple Silicon slices, including the deployed Qt dependencies.
def make_macos_app [build_dir?: path] {
    let release_directory = get_release_directory $build_dir
    let qt_directory = cache_value $release_directory Qt6_DIR
    if ($qt_directory | is-empty) { error make {msg: "Qt6_DIR is missing from this build's CMake cache."} }
    let qt_sdk = $qt_directory | path join "../../.." | path expand
    run_checked cmake --build $release_directory --config Release --target dlan_translations
    run_checked /bin/bash Setups/macOS/package.sh $release_directory $qt_sdk
}

def windows_openssl_dlls [release_directory: path] {
    # Qt deploys its TLS plugins, but does not reliably copy dynamically loaded
    # OpenSSL DLLs. Also ship libcrypto, used directly for certificate creation.
    let configured_openssl = cache_value $release_directory DLAN_OPENSSL_RUNTIME_DIR
    let openssl_root = cache_value $release_directory OPENSSL_ROOT_DIR
    let openssl_bin = if not ($configured_openssl | is-empty) {
        $configured_openssl | path expand
    } else if not ($openssl_root | is-empty) {
        $openssl_root | path join "bin" | path expand
    } else {
        # FindOpenSSL exposes OPENSSL_CRYPTO_LIBRARY as a result variable, but
        # its Windows cache entries depend on the compiler and configuration.
        let crypto_library = (["OPENSSL_CRYPTO_LIBRARY" "LIB_EAY" "LIB_EAY_RELEASE" "LIB_EAY_DEBUG"]
            | each {|key| cache_value $release_directory $key }
            | where {|value| not ($value | is-empty) and not ($value | str ends-with "-NOTFOUND") }
            | get -o 0 | default "")
        if ($crypto_library | is-empty) {
            error make {msg: "Cannot locate the OpenSSL installation. Set DLAN_OPENSSL_RUNTIME_DIR to its bin directory."}
        }
        $crypto_library | path dirname | path join "../bin" | path expand
    }
    if not ($openssl_bin | path exists) {
        error make {msg: $"OpenSSL runtime directory does not exist: ($openssl_bin). Set DLAN_OPENSSL_RUNTIME_DIR to its bin directory."}
    }
    ["libcrypto*.dll" "libssl*.dll"] | each {|pattern|
        # Windows path separators are glob escapes. Match just the filename in
        # a scoped directory, also keeping metacharacters in paths literal.
        let matches = do { cd $openssl_bin; glob --no-dir $pattern }
        if ($matches | length) != 1 {
            error make {msg: $"Set DLAN_OPENSSL_RUNTIME_DIR to the OpenSSL 3 bin directory: expected one ($pattern) in ($openssl_bin)."}
        }
        $matches | first
    }
}

def make_windows_setup [build_dir?: path] {
    let release_directory = get_release_directory $build_dir
    let openssl_dlls = windows_openssl_dlls $release_directory

    cd Setups/Windows
    mkdir setup_bundle

    for executable in [D-LAN.Core.exe D-LAN.GUI.exe PasswordHasher.exe] {
        cp ($release_directory | path join "output" $executable) setup_bundle
    }

    cd setup_bundle
    cp C:/Qt/Tools/llvm-mingw1706_64/bin/libwinpthread-1.dll .
    cp ...$openssl_dlls .

    mkdir styles
    cp -r ../../../styles/* styles/
    cp -r ../../../GUI/resources/emoticons .

    run_checked windeployqt.exe --force-openssl --no-translations PasswordHasher.exe D-LAN.Core.exe D-LAN.GUI.exe

    cd ..

    run_checked iscc windows_setup.iss
}

def make_linux_app_image [build_dir?: path] {
    let release_directory = get_release_directory $build_dir
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
    let build_time = (open --raw Common/Version.h | lines
        | parse '#define BUILD_TIME "{build_time}"' | get build_time | first)
    let package_version = if ($tag | is-empty) { $version } else { $"($version)-($tag)" }
    let output_directory = $application_directory | path join "Setups/AppImage"
    mkdir $output_directory
    let output = $output_directory | path join $"D-LAN-($package_version)-($build_time)-($architecture).AppImage"
    let linuxdeploy = $tools_directory | path join $"linuxdeploy-($architecture).AppImage"
    let qt_plugin = $tools_directory | path join $"linuxdeploy-plugin-qt-($architecture).AppImage"
    let ssl_library = cache_value $release_directory OPENSSL_SSL_LIBRARY
    if ($ssl_library | is-empty) or not ($ssl_library | path exists) or ($ssl_library | str ends-with ".a") {
        error make {msg: "AppImage packaging requires a shared OpenSSL SSL library (OPENSSL_SSL_LIBRARY)."}
    }
    let tls_plugin = $plugins.stdout | str trim | path join "tls/libqopensslbackend.so"
    if not ($tls_plugin | path exists) { error make {msg: "Qt's OpenSSL TLS plugin is required for the AppImage."} }
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
        ^$linuxdeploy --appdir $appdir --executable ($bin_directory | path join "D-LAN.GUI") --executable ($bin_directory | path join "D-LAN.Core") --library $ssl_library --desktop-file $desktop --icon-file $icon
        if $env.LAST_EXIT_CODE != 0 { error make {msg: "AppImage dependency deployment failed"} }
        ^$qt_plugin --appdir $appdir ...$sql_exclusions
        if $env.LAST_EXIT_CODE != 0 { error make {msg: "Qt plugin deployment failed"} }

        let tls_directory = $appdir | path join "usr/plugins/tls"
        mkdir $tls_directory
        cp $tls_plugin $tls_directory
        ^$linuxdeploy --appdir $appdir --deploy-deps-only ($tls_directory | path join "libqopensslbackend.so")
        if $env.LAST_EXIT_CODE != 0 { error make {msg: "TLS plugin deployment failed"} }

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

# Explicit checking also covers Nushell versions that continue after externals fail.
def --wrapped run_checked [program: string, ...arguments: string] {
    ^$program ...$arguments
    if $env.LAST_EXIT_CODE != 0 {
        error make {msg: $"($program) failed with exit code ($env.LAST_EXIT_CODE)"}
    }
}

def cache_value [directory: path, key: string] {
    open --raw ($directory | path join "CMakeCache.txt")
        | lines | parse '{key}:{type}={value}' | where key == $key
        | get value | get -o 0 | default ""
}

def is_release_directory [directory: path] {
    if not ($directory | path join "CMakeCache.txt" | path exists) { return false }
    let configurations = cache_value $directory CMAKE_CONFIGURATION_TYPES
    if not ($configurations | is-empty) {
        "Release" in ($configurations | split row ";")
    } else {
        (cache_value $directory CMAKE_BUILD_TYPE) == "Release"
    }
}

def require_tests [directory: path] {
    if not ((cache_value $directory DLAN_BUILD_TESTS) =~ '^(?i:ON|TRUE|YES|1)$') {
        error make {msg: "Release testing requires DLAN_BUILD_TESTS=ON. Reconfigure this build with tests enabled."}
    }
}

def get_release_directory [directory?: path] {
    if $directory != null {
        let directory = $directory | path expand
        if not (is_release_directory $directory) {
            error make {msg: $"Not a configured Release build: ($directory)"}
        }
        return $directory
    }
    let directories = (glob "build/*/CMakeCache.txt"
        | each {|cache| $cache | path dirname }
        | where {|directory| is_release_directory $directory })
    if ($directories | is-empty) {
        error make {msg: "No configured Release build found. Configure one with CMake or Qt Creator, or pass --build-dir."}
    }
    if ($directories | length) > 1 {
        error make {msg: $"Multiple Release builds found; select one with --build-dir: ($directories | str join ', ')"}
    }
    $directories | first
}
