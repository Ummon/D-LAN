def main [] {
    build_css
    setup_native_build_env
    gleam dev
}

def "main build" [] {
    setup_native_build_env
    gleam build
}

const style_output = "priv/static/style.css"

def "main watch-css" [] {
    watch_css
}

def "main build-css" [] {
    build_css
}

def "main deploy" [port, host, path, chown_user = ""] {
    build_css
    setup_native_build_env
    gleam test
    gleam export erlang-shipment

    let cross_compiled = $nu.os-info.name == "windows"
    if $cross_compiled {
        cross_compile_nifs_for_linux (ssh -p $port $host uname -m | str trim)

        # The entry point is exported with Windows line endings.
        let entrypoint = "build/erlang-shipment/entrypoint.sh"
        open --raw $entrypoint | str replace --all "\r\n" "\n" | save --force --raw $entrypoint
    }

    # 'rsync' doesn't understand the Windows path separators.
    let files = ls build/erlang-shipment | get name | str replace --all '\' '/'
    rsync -rvz --delete --exclude=d_lan_website.sqlite3 --exclude=config.toml --exclude=/d_lan_website/priv/releases/ -e $'ssh -p ($port)' ...$files $'($host):($path)'
    if $chown_user != "" {
        ssh -p $port $host $'sudo chown -R ($chown_user):($chown_user) ($path)'
    }

    if $cross_compiled {
        # Check that the cross-compiled NIFs can be loaded by the Erlang VM of the host.
        let check = "erl -noshell -pa */ebin -eval '{module, _} = code:ensure_loaded(jargon), {module, _} = code:ensure_loaded(esqlite3_nif), halt().'"
        ssh -p $port $host $"cd ($path) && ($check)"
    }
}

def "main password-hash" [password] {
    setup_native_build_env
    gleam run -m password $password
}

def build_css [] {
    dart-sass scss/style.scss $style_output
}

def watch_css [] {
    dart-sass --watch scss/style.scss $style_output
}

# Prepare the environment to compile the native dependencies (NIFs) on Windows,
# MSYS2 with the packages 'make' and 'mingw-w64-x86_64-gcc' is required:
#  * 'esqlite' is compiled with 'cc'.
#  * 'jargon' is compiled with 'make' and '/mingw64/bin/gcc'. Its makefile needs
#    the Unix 'find' instead of the Windows one and doesn't support spaces in
#    the Erlang paths: their short (8.3) forms are given instead.
def --env setup_native_build_env [] {
    if $nu.os-info.name != "windows" { return }

    let make = which make
    if ($make | is-empty) {
        error make { msg: "'make' not found: MSYS2 with the packages 'make' and 'mingw-w64-x86_64-gcc' is required" }
    }
    let msys_bin = $make.0.path | path dirname
    let mingw_bin = $msys_bin | path dirname --num-levels 2 | path join "mingw64" "bin"
    $env.PATH = $env.PATH | prepend [$mingw_bin $msys_bin]

    let erl_dirs = erlang_dirs
    let short = {|path| ^cygpath --mixed --short-name $path | str trim }
    $env.ERTS_INCLUDE_DIR = do $short $erl_dirs.0
    $env.ERL_INTERFACE_INCLUDE_DIR = do $short ($erl_dirs.1 | path join "include")
    $env.ERL_INTERFACE_LIB_DIR = do $short ($erl_dirs.1 | path join "lib")
}

# Return the directory of the ERTS headers and the directory of the 'erl_interface' library.
def erlang_dirs [] {
    ^erl -noshell -eval "io:format('~ts~n~ts~n', [filename:join([code:root_dir(), lists:concat(['erts-', erlang:system_info(version)]), include]), code:lib_dir(erl_interface)]), halt()." | lines
}

# A shipment exported on Windows contains Windows native libraries (NIFs):
# replace them by Linux ones, cross-compiled with Zig.
# The compilation flags are the ones used by the packages when built on Linux,
# see 'rebar.config.script' of 'esqlite' and 'c_src/Makefile' of 'jargon'.
def cross_compile_nifs_for_linux [arch: string] {
    if $arch not-in ["x86_64" "aarch64"] {
        error make { msg: $"Unsupported host architecture: '($arch)'" }
    }

    # The NIF headers are the same on all platforms except for the size of the type 'long'.
    let include = "build/linux-nif-include"
    let erts_include = erlang_dirs | get 0
    mkdir $include
    for header in ["erl_nif.h" "erl_nif_api_funcs.h" "erl_drv_nif.h"] {
        cp ($erts_include | path join $header) $include
    }
    open --raw ($erts_include | path join "erl_int_sizes_config.h")
        | str replace "SIZEOF_LONG 4" "SIZEOF_LONG 8"
        | save --force ($include | path join "erl_int_sizes_config.h")

    let zig_cc = ["cc" "-target" $"($arch)-linux-gnu.2.28" "-shared" "-fPIC" "-fno-sanitize=undefined" "-I" $include]

    let jargon = "build/packages/jargon"
    let jargon_priv = "build/erlang-shipment/jargon/priv"
    let jargon_sources = ["c_src/jargon.c" "argon2/src/argon2.c" "argon2/src/core.c" "argon2/src/blake2/blake2b.c" "argon2/src/thread.c" "argon2/src/encoding.c" "argon2/src/ref.c"]
    (^zig ...$zig_cc -O3 -std=c99 -finline-functions
        -I ($jargon | path join "argon2/include")
        ...($jargon_sources | each {|source| $jargon | path join $source })
        -lpthread
        -o ($jargon_priv | path join "jargon.so"))
    rm ($jargon_priv | path join "jargon.dll")

    let esqlite = "build/packages/esqlite"
    let esqlite_priv = "build/erlang-shipment/esqlite/priv"
    let esqlite_sources = ["c_src/esqlite3_nif.c" "c_src/sqlite3/sqlite3.c"]
    (^zig ...$zig_cc -Os -std=c11
        -DSQLITE_DQS=0 -DSQLITE_THREADSAFE=1 -DSQLITE_DEFAULT_MEMSTATUS=0 -DSQLITE_DEFAULT_WAL_SYNCHRONOUS=1
        -DSQLITE_LIKE_DOESNT_MATCH_BLOBS -DSQLITE_MAX_EXPR_DEPTH=0 -DSQLITE_OMIT_DEPRECATED
        -DSQLITE_OMIT_PROGRESS_CALLBACK -DSQLITE_USE_ALLOCA -DSQLITE_OMIT_AUTOINIT -DSQLITE_USE_URI
        -DSQLITE_ENABLE_FTS3 -DSQLITE_ENABLE_FTS3_PARENTHESIS -DSQLITE_ENABLE_FTS4 -DSQLITE_ENABLE_FTS5
        -DSQLITE_ENABLE_MATH_FUNCTIONS -DSQLITE_ENABLE_JSON1 -DSQLITE_ENABLE_RTREE -DSQLITE_ENABLE_GEOPOLY
        -I ($esqlite | path join "c_src/sqlite3")
        ...($esqlite_sources | each {|source| $esqlite | path join $source })
        -lpthread -ldl -lm
        -o ($esqlite_priv | path join "esqlite3_nif.so"))
    rm ($esqlite_priv | path join "esqlite3_nif.dll")
}
