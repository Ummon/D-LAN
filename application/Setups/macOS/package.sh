#!/bin/bash
# Invoked by build.nu with a configured Release directory and its Qt SDK.
set -euo pipefail

fail() { echo "macOS packaging: $*" >&2; exit 1; }
[[ $(uname -s) == Darwin ]] || fail "Run this script on macOS."
[[ $# == 2 ]] || fail "Usage: package.sh <release-build> <qt-sdk>"
build_dir=$(cd "$1" && pwd)
qt_dir=$(cd "$2" && pwd)
application_dir=$(cd "$(dirname "$0")/../.." && pwd)
macdeployqt="$qt_dir/bin/macdeployqt"
[[ -x "$macdeployqt" ]] || fail "macdeployqt not found in the selected Qt SDK."

bin_dir="$build_dir/output"
[[ ! -d "$bin_dir/Release" ]] || bin_dir="$bin_dir/Release"
for executable in D-LAN.GUI D-LAN.Core; do
    [[ -x "$bin_dir/$executable" ]] || fail "Build $executable in Release mode first."
    /usr/bin/lipo -verify_arch arm64 "$bin_dir/$executable" || fail "$executable has no arm64 slice."
done

version=$(sed -n 's/^#define VERSION "\([^"]*\)"/\1/p' "$application_dir/Common/Version.h")
tag=$(sed -n 's/^#define VERSION_TAG "\([^"]*\)"/\1/p' "$application_dir/Common/Version.h")
build_time=$(sed -n 's/^#define BUILD_TIME "\([^"]*\)"/\1/p' "$application_dir/Common/Version.h")
[[ "$version" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]] || fail "Invalid VERSION."
[[ "$tag" =~ ^[a-zA-Z0-9._-]*$ && "$build_time" =~ ^[0-9_-]+$ ]] || fail "Invalid version tag/build time."

# Never package a binary carrying a stale version from before Version.h changed.
expected_version="$version${tag:+ $tag} $build_time"
[[ $("$bin_dir/D-LAN.Core" --version) == "$expected_version" ]] || fail "Binary version differs from Version.h; rebuild first."

output_dir="$application_dir/Setups/macOS/Installations"
mkdir -p "$output_dir" "$application_dir/build/macos"
stage=$(mktemp -d "$application_dir/build/macos/package.XXXXXX")
image_mount="$stage/mount"
image_attached=false
cleanup() {
    if $image_attached; then
        if ! diskutil eject "$image_mount"; then
            echo "macOS packaging: Cannot eject $image_mount; preserving $stage for cleanup." >&2
            return
        fi
    fi
    rm -rf "$stage"
}
trap cleanup EXIT
app="$stage/D-LAN.app"
contents="$app/Contents"
resources="$contents/Resources"
mkdir -p "$contents/MacOS" "$resources/languages" "$contents/PlugIns"
cp "$bin_dir/D-LAN.GUI" "$bin_dir/D-LAN.Core" "$contents/MacOS/"
# Keep the optional PasswordHasher tool alongside the core when built.
if [[ -x "$bin_dir/PasswordHasher" ]]; then
    cp "$bin_dir/PasswordHasher" "$contents/MacOS/"
fi
cp -R "$application_dir/styles" "$resources/styles"
cp -R "$application_dir/GUI/resources/emoticons" "$resources/emoticons"
cp "$application_dir/../COPYING" "$resources/COPYING"
cp -R "$application_dir/Setups/macOS/licenses" "$resources/licenses"
shopt -s nullglob
translations=("$build_dir"/d_lan_*.qm)
[[ ${#translations[@]} -gt 0 ]] || fail "No translations found; install Qt LinguistTools and build dlan_translations."
cp "${translations[@]}" "$resources/languages/"

# Use the project's existing 256px icon, converted to Apple's icon container.
iconset="$stage/D-LAN.iconset"
mkdir "$iconset"
sips -s format png "$application_dir/Common/resources/icon.ico" --out "$stage/icon.png" >/dev/null
for size in 16 32 128 256 512; do
    sips -z "$size" "$size" "$stage/icon.png" --out "$iconset/icon_${size}x${size}.png" >/dev/null
done
for size in 16 32 128 256 512; do
    pixels=$((size * 2))
    sips -z "$pixels" "$pixels" "$stage/icon.png" --out "$iconset/icon_${size}x${size}@2x.png" >/dev/null
done
iconutil -c icns "$iconset" -o "$resources/D-LAN.icns"

cp "$application_dir/Setups/macOS/Info.plist" "$contents/Info.plist"
plutil -replace CFBundleShortVersionString -string "$version" "$contents/Info.plist"
plutil -replace CFBundleVersion -string "$version" "$contents/Info.plist"
plutil -insert DLANBuildTime -string "$build_time" "$contents/Info.plist"
plutil -insert DLANVersionTag -string "$tag" "$contents/Info.plist"
printf 'APPL????' > "$contents/PkgInfo"

# Select plugins explicitly: deploying every SQL driver adds unrelated external
# database dependencies. D-LAN uses SQLite. Fusion needs no native style plugin.
plugins=("$qt_dir/plugins/platforms/libqcocoa.dylib" "$qt_dir/plugins/sqldrivers/libqsqlite.dylib"
         "$qt_dir/plugins/tls/libqsecuretransportbackend.dylib"
         "$qt_dir"/plugins/imageformats/*.dylib "$qt_dir"/plugins/iconengines/*.dylib)
extra_executables=()
for executable in "$contents"/MacOS/*; do
    [[ "$executable" == "$contents/MacOS/D-LAN.GUI" ]] || extra_executables+=("-executable=$executable")
done
for plugin in "${plugins[@]}"; do
    [[ -f "$plugin" ]] || fail "Missing Qt plugin: $plugin"
    category=$(basename "$(dirname "$plugin")")
    mkdir -p "$contents/PlugIns/$category"
    destination="$contents/PlugIns/$category/$(basename "$plugin")"
    cp "$plugin" "$destination"
    extra_executables+=("-executable=$destination")
done
# Include each helper so macdeployqt also deploys its linked libcrypto dependency.
# Qt TLS uses Secure Transport here; no OpenSSL TLS plugin/libssl is needed.
"$macdeployqt" "$app" -no-plugins -no-codesign -no-strip -always-overwrite "${extra_executables[@]}"
# Qt looks for qt.conf in the bundle Resources directory for each executable.
printf '[Paths]\nPlugins = PlugIns\n' > "$resources/qt.conf"

# Thin *every* Mach-O, including Qt frameworks/plugins, before signing. Leave the
# SDK and the original build products untouched. Reject any Intel-only library.
machos=()
while IFS= read -r -d '' binary; do
    case $(/usr/bin/file -b "$binary") in
        *Mach-O*) machos+=("$binary") ;;
    esac
done < <(find "$contents" -type f -print0)
[[ ${#machos[@]} -gt 0 ]] || fail "No Mach-O files in the bundle."
for binary in "${machos[@]}"; do
    architectures=$(/usr/bin/lipo -archs "$binary")
    /usr/bin/lipo -verify_arch arm64 "$binary" || fail "Dependency has no arm64 slice: $binary"
    if [[ "$architectures" != arm64 ]]; then
        /usr/bin/lipo "$binary" -thin arm64 -output "$binary.arm64"
        chmod "$(stat -f %Lp "$binary")" "$binary.arm64"
        mv "$binary.arm64" "$binary"
    fi
    # Remove development-machine search paths left by the build/deployment tool.
    while IFS= read -r rpath; do
        case "$rpath" in
            /*) install_name_tool -delete_rpath "$rpath" "$binary" ;;
        esac
    done < <(otool -l "$binary" | awk '/cmd LC_RPATH/ {r=1; next} r && /path / {sub(/^ *path /, ""); sub(/ \(offset.*$/, ""); print; r=0}')
done

# Verify that every dependency resolves inside this bundle or to a system library.
for binary in "${machos[@]}"; do
    install_id=$(otool -D "$binary" | sed -n '2p')
    while IFS= read -r dependency; do
        [[ "$dependency" != "$install_id" ]] || continue
        case "$dependency" in
            /System/Library/*|/usr/lib/*) continue ;;
            @rpath/*) resolved="$contents/Frameworks/${dependency#@rpath/}" ;;
            @executable_path/*) resolved="$contents/MacOS/${dependency#@executable_path/}" ;;
            @loader_path/*) resolved="$(dirname "$binary")/${dependency#@loader_path/}" ;;
            *) fail "Non-relocatable dependency in $binary: $dependency" ;;
        esac
        [[ -f "$resolved" ]] || fail "Unresolved dependency in $binary: $dependency"
    done < <(otool -L "$binary" | tail -n +2 | sed -E 's/^[[:space:]]+//; s/ \(compatibility version.*$//')
done

# Advertise the actual minimum OS across the shipped binaries, never the SDK
# version or an arbitrary lower value that the executable cannot run on.
minimum_os=$(
    for binary in "${machos[@]}"; do
        otool -l "$binary" | awk '/cmd LC_VERSION_MIN_MACOSX/ {old=1} /minos / {print $2} old && /version / {print $2; old=0}'
    done | awk '{split($1,v,"."); n=v[1]*1000000+v[2]*1000+v[3]; if(n>max){max=n; version=$1}} END {print version}'
)
[[ -n "$minimum_os" ]] || fail "Cannot determine minimum macOS version."
# This distribution promises macOS 26.0 compatibility. Do not merely relabel a
# bundle built for a newer OS; rebuild the application/dependencies instead.
if ! awk -v version="$minimum_os" 'BEGIN {split(version,v,"."); exit !(v[1]*1000000+v[2]*1000+v[3] <= 26000000)}'; then
    fail "Bundle requires macOS $minimum_os. Rebuild D-LAN and its dependencies with CMAKE_OSX_DEPLOYMENT_TARGET=26.0."
fi
plutil -replace LSMinimumSystemVersion -string "$minimum_os" "$contents/Info.plist"
plutil -lint "$contents/Info.plist"

# Ad-hoc signing permits local use on Apple Silicon. A Developer ID can be
# supplied for distribution; notarization is a separate credentialed operation.
identity=${DLAN_MACOS_SIGN_IDENTITY:--}
sign_options=(--force --sign "$identity")
if [[ "$identity" != - ]]; then sign_options+=(--options runtime --timestamp); fi
for binary in "${machos[@]}"; do codesign "${sign_options[@]}" "$binary"; done
while IFS= read -r -d '' framework; do codesign "${sign_options[@]}" "$framework"; done < <(find "$contents/Frameworks" -depth -name '*.framework' -type d -print0)
codesign "${sign_options[@]}" "$app"
codesign --verify --deep --strict "$app"

# Check helper loading using only the deployed frameworks before packaging.
[[ $(env -u DYLD_LIBRARY_PATH -u DYLD_FRAMEWORK_PATH "$contents/MacOS/D-LAN.Core" --version) == "$expected_version" ]] || fail "Packaged core failed to start."

# Configure Finder on a writable volume so its background reference remains
# valid when the final image is mounted on another machine.
writable_image="$stage/layout.dmg"
disk_image="$stage/package.dmg"
# Allow room for filesystem metadata and Finder's view settings.
image_size_mb=$(( $(du -sk "$app" | awk '{print $1}') / 1024 * 12 / 10 + 256 ))
modern_diskutil=false
if diskutil image create from --help >/dev/null 2>&1; then
    modern_diskutil=true
    diskutil image create blank --volumeName "D-LAN" --format RAW --size "${image_size_mb}m" "$writable_image"
else
    hdiutil create -volname "D-LAN" -size "${image_size_mb}m" -fs HFS+ -format UDRW "$writable_image"
fi
image_attached=true
if $modern_diskutil; then
    diskutil image attach --nobrowse --mountPoint "$image_mount" "$writable_image"
else
    hdiutil attach -nobrowse -mountpoint "$image_mount" "$writable_image"
fi
ditto "$app" "$image_mount/D-LAN.app"
ln -s /Applications "$image_mount/Applications"
mkdir "$image_mount/.background"
xcrun swift -module-cache-path "$stage/swift-cache" \
    "$application_dir/Setups/macOS/dmg-background.swift" "$image_mount/.background/install.tiff"
osascript "$application_dir/Setups/macOS/dmg-layout.applescript" "$image_mount" \
    || fail "Cannot save the disk image layout. Run in a logged-in desktop session and allow your terminal to control Finder."
[[ -s "$image_mount/.DS_Store" ]] || fail "Finder did not save the disk image layout."
sync
diskutil eject "$image_mount"
image_attached=false

# Convert the configured volume itself, preserving Finder's background alias.
if $modern_diskutil; then
    diskutil image create from --format UDZO "$writable_image" "$disk_image"
else
    hdiutil convert "$writable_image" -format UDZO -o "$disk_image"
fi
if [[ "$identity" != - ]]; then
    codesign --force --sign "$identity" --timestamp "$disk_image"
    codesign --verify --strict "$disk_image"
fi
hdiutil verify "$disk_image"
package="$output_dir/D-LAN-$version${tag:+-$tag}-$build_time-arm64.dmg"
mv -f "$disk_image" "$package"
echo "Created $package (arm64 only, macOS $minimum_os or newer)"
