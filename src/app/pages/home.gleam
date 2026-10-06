import app/date
import app/download_button
import app/screenshots
import app/web
import gleam/result
import gleam/time/calendar
import gleam/time/timestamp
import lustre/attribute as attr
import lustre/element
import lustre/element/html
import translations as tr

pub fn page(ctx: web.Context) -> element.Element(a) {
  let download_separator =
    html.img([
      attr.src("static/img/circle.svg"),
      attr.class("download-separator"),
    ])
  html.div([attr.id("content"), attr.class("home")], [
    image_of_the_week(ctx.lang),
    html.h1([], [html.em([], [tr.home_title(ctx.lang)])]),
    html.p([], [tr.home_description(ctx.lang, "features.html")]),
    html.div([attr.class("downloads")], [
      download_button.element(ctx, "Windows") |> result.unwrap(element.none()),
      download_button.microsoft_store_element(ctx),
      download_separator,
      download_button.element(ctx, "Linux") |> result.unwrap(element.none()),
      download_separator,
      download_button.element(ctx, "macOS") |> result.unwrap(element.none()),
    ]),
  ])
}

fn image_of_the_week(lang: tr.Lang) -> element.Element(a) {
  let #(date, _time) =
    timestamp.system_time() |> timestamp.to_calendar(calendar.utc_offset)

  case date.weekday(date) {
    date.Monday ->
      screenshots.image(
        lang,
        "browse",
        tr.gallery_browse(lang),
        tr.gallery_browse_comment(lang),
      )

    date.Tuesday ->
      screenshots.image(
        lang,
        "search",
        tr.gallery_search(lang),
        tr.gallery_search_comment(lang),
      )

    date.Wednesday ->
      screenshots.image(
        lang,
        "download_folders",
        tr.gallery_download_folders(lang),
        tr.gallery_download_folders_comment(lang),
      )

    date.Thursday ->
      screenshots.image(
        lang,
        "download_files",
        tr.gallery_download_files(lang),
        tr.gallery_download_files_comment(lang),
      )

    date.Friday ->
      screenshots.image(lang, "upload", tr.gallery_upload(lang), "")

    // Week-end.
    _ ->
      screenshots.image(
        lang,
        "download_files",
        tr.gallery_download_files(lang),
        tr.gallery_download_files_comment(lang),
      )
  }
}
