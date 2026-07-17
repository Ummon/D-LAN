import app/pages
import app/pages/screenshots
import app/utils
import app/web
import gleam/time/calendar
import gleam/time/timestamp
import lustre/attribute as attr
import lustre/element
import lustre/element/html
import translations as tr

pub fn page(ctx: web.Context, page: String) -> element.Element(a) {
  html.div([attr.id("content"), attr.class("home")], [
    image_of_the_week(ctx.lang),
    html.h1([], [html.em([], [tr.home_title(ctx.lang)])]),
    html.p([], [tr.home_description(ctx.lang, "features.html")]),
    utils.download_button(ctx, "windows"),
    html.div([attr.class("spacer")], []),
  ])
}

fn image_of_the_week(lang: tr.Lang) -> element.Element(a) {
  let #(date, _time) =
    timestamp.system_time() |> timestamp.to_calendar(calendar.utc_offset)

  case utils.weekday(date) {
    utils.Monday ->
      screenshots.image(
        "browse",
        tr.gallery_browse(lang),
        tr.gallery_browse_comment(lang),
      )

    utils.Tuesday ->
      screenshots.image(
        "search",
        tr.gallery_search(lang),
        tr.gallery_search_comment(lang),
      )

    utils.Wednesday ->
      screenshots.image(
        "download_folders",
        tr.gallery_download_folders(lang),
        tr.gallery_download_folders_comment(lang),
      )

    utils.Thursday ->
      screenshots.image(
        "download_files",
        tr.gallery_download_files(lang),
        tr.gallery_download_files_comment(lang),
      )

    utils.Friday -> screenshots.image("upload", tr.gallery_upload(lang), "")

    // Week-end.
    _ ->
      screenshots.image(
        "download_files",
        tr.gallery_download_files(lang),
        tr.gallery_download_files_comment(lang),
      )
  }
}
