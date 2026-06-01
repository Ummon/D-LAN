def chunk-hash [file, chunk] {
    let start = $chunk * 64 * 1024 * 1024
    let end = ($chunk + 1) * 64 * 1024 * 1024 - 1
    open --raw $file | bytes at $start..$end | b3sum.exe -l 28
}