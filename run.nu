def main [] {
    build_css
    gleam dev
}

const style_output = "priv/static/style.css"

def "main watch-css" [] {
    watch_css
}

def build_css [] {
    dart-sass style.scss $style_output
}

def watch_css [] {
    dart-sass --watch style.scss $style_output
}