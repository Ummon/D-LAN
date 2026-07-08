let current_date_formatted = date now | format date "%Y-%m-%d_%H-%M"
let current_git_head = (git rev-parse HEAD)
let version_file = "../Common/Version.h"

open $version_file
    | str replace -r 'BUILD_TIME ".*"' ('BUILD_TIME "' + $current_date_formatted + '"')
    | str replace -r 'GIT_VERSION ".*"' ('GIT_VERSION "' + $current_git_head + '"')
    | save -f $version_file