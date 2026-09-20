-- Coordinates are in logical points and match dmg-background.swift.
on run argv
    set mountPath to item 1 of argv
    set imageFolder to POSIX file mountPath as alias
    tell application "Finder"
        set imageFolder to folder imageFolder
        open imageFolder
        tell container window of imageFolder
            set current view to icon view
            set toolbar visible to false
            set statusbar visible to false
            set pathbar visible to false
            set bounds to {200, 120, 840, 502}
        end tell
        set viewOptions to icon view options of container window of imageFolder
        set arrangement of viewOptions to not arranged
        set icon size of viewOptions to 128
        set text size of viewOptions to 14
        set shows item info of viewOptions to false
        set shows icon preview of viewOptions to false
        set label position of viewOptions to bottom
        set background picture of viewOptions to POSIX file (mountPath & "/.background/install.tiff") as alias
        set position of item "D-LAN.app" of imageFolder to {160, 170}
        set position of item "Applications" of imageFolder to {480, 170}
        close container window of imageFolder
        open imageFolder
        update imageFolder without registering applications
        delay 2
        close container window of imageFolder
    end tell
    -- Finder writes .DS_Store asynchronously after closing the window.
    repeat 30 times
        try
            do shell script "test -s " & quoted form of (mountPath & "/.DS_Store")
            return
        end try
        delay 1
    end repeat
    error "Finder did not write .DS_Store."
end run
