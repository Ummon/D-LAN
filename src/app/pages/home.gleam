import app/pages
import app/pages/screenshots
import gleam/time/calendar
import gleam/time/timestamp
import lustre/attribute as attr
import lustre/element
import lustre/element/html
import translations as tr
import utils

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

pub fn page(lang: tr.Lang, page: String) -> element.Element(a) {
  html.div([attr.id("content"), attr.class("home")], [
    image_of_the_week(lang),
    html.h1([], [html.em([], [tr.home_title(lang)])]),
    html.p([], [tr.home_description(lang, "features.html")]),
  ])
  |> pages.main_page(lang, page)
}
