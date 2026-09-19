def main [...extensions: string] {
    let extensions = ["cpp", "h", "ui", "proto", "nu"]

    if (which cloc | is-empty) {
        let count = git ls-files
            | lines
            | where {|f| ($f | path parse | get extension) in $extensions }
            # | inspect # Debug
            | par-each {|f| try { open --raw $f | lines | length } catch { 0 } }
            | math sum
        print $"Number of lines for files ($extensions): ($count)"
        print "You can install cloc and re-run this script to have a more accurate result"
    } else {
        cloc --vcs=git --include-ext=($extensions | str join ",")
    }
}