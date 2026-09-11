def main [] {
    build_css
    gleam dev
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
    gleam test
    gleam export erlang-shipment

    # Remove these two lines when gleam 1.19 is released
    # See: https://github.com/gleam-lang/gleam/issues/6073
    let entrypoint = open --raw build/erlang-shipment/entrypoint.sh | split row "\n" | skip 3 | str join "\n"
    $entrypoint | save --force build/erlang-shipment/entrypoint.sh

    rsync -rvz --delete --exclude=d_lan_website.sqlite3 --exclude=config.toml --exclude=/d_lan_website/priv/releases/ -e $'ssh -p ($port)' build/erlang-shipment/* ($host):($path)
    if $chown_user != "" {
        ssh -p 9851 $host $'sudo chown -R ($chown_user):($chown_user) ($path)'
    }
}

def "main password-hash" [password] {
    gleam run -m password $password
}

def build_css [] {
    dart-sass scss/style.scss $style_output
}

def watch_css [] {
    dart-sass --watch scss/style.scss $style_output
}
