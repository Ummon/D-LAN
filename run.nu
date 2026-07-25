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
    gleam test
    gleam export erlang-shipment
    rsync -rvz --delete -e $'ssh -p ($port)' build/erlang-shipment/* ($host):($path)
    if $chown_user != "" {
        ssh -p 9851 $host $'sudo chown -R ($chown_user):($chown_user) ($path)'
    }
}

def "main password-hash" [password] {
    gleam run -m password $password
}

def build_css [] {
    dart-sass style.scss $style_output
}

def watch_css [] {
    dart-sass --watch style.scss $style_output
}