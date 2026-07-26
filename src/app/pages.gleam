import gleam/int
import gleam/list
import gleam/time/calendar
import gleam/time/timestamp
import lustre/attribute as attr
import lustre/element
import lustre/element/html
import translations as tr

pub fn main_page(
  content: element.Element(a),
  lang: tr.Lang,
  page: String,
) -> element.Element(a) {
  html.html([attr.lang(tr.to_str(lang))], [
    html.head([], [
      html.meta([
        attr.name("viewport"),
        attr.attribute("content", "width=device-width, initial-scale=1"),
      ]),
      html.title([], tr.title(lang)),
      html.link([attr.rel("shortcut icon"), attr.href("static/favicon.ico")]),
      html.link([attr.rel("stylesheet"), attr.href("static/style.css")]),
      html.link([
        attr.rel("stylesheet"),
        attr.href("static/colorbox/colorbox.css"),
      ]),
      html.script([attr.src("static/js/jquery-3.5.0.min.js")], ""),
      html.script([attr.src("static/colorbox/jquery.colorbox-min.js")], ""),
      html.script([attr.src("static/js/snow.js")], ""),
      html.script([attr.src("static/js/d_lan.js")], ""),
    ]),
    html.body([], [
      html.canvas([attr.id("canvas-menu")]),
      header(lang),
      menu(lang, page),
      html.div([attr.id("content-bg")], [content]),
      footer(),
    ]),
  ])
}

fn header(lang: tr.Lang) {
  html.div([attr.id("header")], [
    languages(lang),
    html.ul([attr.id("external-links")], [
      html.li([], [
        html.a(
          [
            attr.href("http://dev.d-lan.net/projects/pmp/wiki"),
            attr.target("_blank"),
          ],
          [html.text("wiki")],
        ),
      ]),
      html.li([], [
        html.a(
          [
            attr.href("http://dev.d-lan.net/projects/pmp/boards"),
            attr.target("_blank"),
          ],
          [html.text("forums")],
        ),
      ]),
      html.li([], [
        html.a(
          [
            attr.href("https://github.com/Ummon/D-LAN"),
            attr.target("_blank"),
          ],
          [html.text("github")],
        ),
      ]),
      html.li([], [
        html.a([attr.href("donate.html")], [
          tr.header_support_us(lang),
        ]),
      ]),
    ]),
  ])
}

fn languages(current_lang: tr.Lang) -> element.Element(a) {
  html.select(
    [attr.id("langs")],
    tr.all_langs()
      |> list.map(fn(l) {
        html.option(
          [attr.value(tr.to_str(l)), attr.selected(l == current_lang)],
          tr.plain_lang(l),
        )
      }),
  )
}

fn menu(lang: tr.Lang, current_page: String) -> element.Element(a) {
  html.div([attr.id("menu")], [
    html.a([attr.id("logo"), attr.href("/")], [
      html.img([attr.src("static/img/logo.svg"), attr.alt("logo")]),
    ]),
    html.ul(
      [],
      [
        #("home.html", tr.menu_home(lang)),
        #("features.html", tr.menu_features(lang)),
        #("faq.html", tr.menu_faq(lang)),
        #("about.html", tr.menu_about(lang)),
      ]
        |> list.map(fn(entry) {
          let #(p, t) = entry
          html.li([], [
            html.a(
              [
                attr.href(p),
                attr.classes([#("current-page", current_page == p)]),
              ],
              [t],
            ),
          ])
        }),
    ),
  ])
}

fn footer() -> element.Element(a) {
  let #(date, _time) =
    timestamp.system_time() |> timestamp.to_calendar(calendar.utc_offset)
  html.div([attr.id("footer")], [
    html.span([attr.class("copyright")], [
      html.text("copyright 2010-" <> int.to_string(date.year)),
      html.a([attr.href("http://www.gburri.org")], [html.text(" Greg Burri")]),
    ]),
    html.text(" • "),
    html.a([attr.class("admin"), attr.href("/admin.html")], [html.text("admin")]),
  ])
}
