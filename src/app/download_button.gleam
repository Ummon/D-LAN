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
    |> latest_release,
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
    |> latest_release
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

/// Returns the most recently built release among 'filenames', or an error if
/// none of them follows the release naming scheme.
///
/// The releases are ordered by the build date embedded in their name and not by
/// the name itself: compared as plain strings, "D-LAN-1.9.0-..." comes after
/// "D-LAN-1.10.0-..." and "D-LAN-1.2.0Beta1-..." after the final release
/// "D-LAN-1.2.0-...".
pub fn latest_release(filenames: List(String)) -> Result(String, Nil) {
  filenames
  |> list.filter_map(fn(f) {
    build_datetime(f) |> result.map(fn(datetime) { #(datetime, f) })
  })
  |> list.max(fn(a, b) { int.compare(a.0, b.0) })
  |> result.map(fn(release) { release.1 })
}

/// Extracts the build date and time embedded in a release filename as a
/// sortable number, for example "D-LAN-1.2.0Beta1-2026-07-10_19-21-Setup.exe"
/// gives 202_607_101_921. Returns an error if the filename doesn't follow the
/// release naming scheme.
fn build_datetime(filename: String) -> Result(Int, Nil) {
  let assert Ok(re) = regexp.from_string("-(\\d+)-(\\d+)-(\\d+)_(\\d+)-(\\d+)")

  use match <- result.try(case regexp.scan(re, filename) {
    [match, ..] -> Ok(match)
    [] -> Error(Nil)
  })
  use parts <- result.try(
    match.submatches
    |> list.try_map(fn(part) {
      part |> option.to_result(Nil) |> result.try(int.parse)
    }),
  )

  case parts {
    [year, month, day, hour, minute] ->
      Ok({ { { year * 100 + month } * 100 + day } * 100 + hour } * 100 + minute)
    _ -> Error(Nil)
  }
}

// Returns the url to download a given file for the given platform.
fn file_to_url(filename: String, platform: String) -> String {
  "download/"
  <> uri.percent_encode(platform)
  <> "/"
  <> uri.percent_encode(filename)
}
