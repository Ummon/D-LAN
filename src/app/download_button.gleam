import app/date
import app/utils
import app/web
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/regexp
import gleam/result
import gleam/string
import gleam/uri
import lustre/attribute as attr
import lustre/element
import lustre/element/html
import simplifile
import translations as tr

/// Returns a release download button as a div element or Nil
/// if no release is found.
/// 'platform' is a folder where the releases are put.
/// For example: "windows".
pub fn element(
  ctx: web.Context,
  platform: String,
) -> Result(element.Element(a), Nil) {
  let platform_formatted = string.capitalise(platform)
  let release_platform_folder = ctx.app.releases_directory <> "/" <> platform
  use filenames <- result.try(
    simplifile.read_directory(release_platform_folder)
    |> result.map_error(fn(_) { Nil }),
  )

  use filename <- result.try(
    filenames
    |> list.filter(fn(f) {
      list.any([".exe", ".dmg", ".deb"], string.ends_with(f, _))
    })
    |> list.sort(string.compare)
    |> list.last,
  )

  let extension = string.slice(filename, string.length(filename) - 3, 3)

  let assert Ok(re) =
    regexp.from_string(case extension {
      "deb" -> "D-LAN-((?:\\d|\\.)+)([^-]*)-(\\d+)-(\\d+)-(\\d+)_.*-(\\w+)\\..*"
      _ -> "D-LAN-((?:\\d|\\.)+)([^-]*)-(\\d+)-(\\d+)-(\\d+).*\\..*"
    })
  let assert [
    regexp.Match(
      submatches: [
        Some(version),
        version_tag,
        Some(year),
        Some(month),
        Some(day),
        ..rest
      ],
      ..,
    ),
  ] = regexp.scan(re, filename)
  // 'archi' isn't used for the moment.
  let archi = case rest {
    [Some(archi)] -> archi
    _ -> "win32"
  }
  let version_full = case version_tag {
    Some(tag) -> version <> " " <> tag
    None -> version
  }

  let assert Ok(file_info) =
    simplifile.file_info(release_platform_folder <> "/" <> filename)

  let assert Ok(month_int) = int.parse(month)
  let released_date = date.month_name(month_int) <> " " <> day <> " " <> year

  // Add a link to the torrent file if it exists.
  let torrent_link = case
    filenames
    |> list.filter(string.ends_with(_, ".torrent"))
    |> list.sort(string.compare)
    |> list.last
  {
    Ok(torrent_file) -> [
      html.a(
        [attr.class("torrent"), attr.href(file_to_url(torrent_file, platform))],
        [tr.download_button_torrent(ctx.lang)],
      ),
    ]
    Error(Nil) -> []
  }

  html.div([attr.class("download " <> extension <> " " <> archi)], [
    html.a(
      [attr.class("installer"), attr.href(file_to_url(filename, platform))],
      [
        html.em([], [
          tr.download_button_download(ctx.lang),
          html.text(" (" <> utils.file_size_mib(file_info.size) <> " MiB)"),
        ]),
        html.br([]),
        tr.download_button_version(ctx.lang, version_full, platform_formatted),
        html.br([]),
        tr.download_button_released(ctx.lang, released_date),
      ],
    ),
    ..torrent_link
  ])
  |> Ok
}

// Returns the url to download a given file for the given platform.
fn file_to_url(filename: String, platform: String) -> String {
  "download/"
  <> uri.percent_encode(platform)
  <> "/"
  <> uri.percent_encode(filename)
}
