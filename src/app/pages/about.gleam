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
  html.text("about")
}
