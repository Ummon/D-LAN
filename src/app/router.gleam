import app/web
import gleam/io
import gleam/list
import lustre/attribute as attr
import lustre/element
import lustre/element/html
import translations as tr
import wisp.{type Request, type Response}

/// The HTTP request handler- your application!
///
pub fn handle_request(req: Request, ctx: web.Context) -> Response {
  // Apply the middleware stack for this request/response.
  use req <- web.middleware(req, ctx)

  // Later we'll use templates, but for now a string will do.
  // let body = "<h1>Hello, Joe!</h1>"

  // Return a 200 OK response with the body and a HTML content type.
  // wisp.html_response(body, 200)

  let lang = tr.current_lang(req)
  let page = case wisp.path_segments(req) {
    [p] -> p
    _ -> "home.html"
  }

  case page {
    "home.html" -> home_page(lang, page) |> element_to_response
    _ -> wisp.not_found()
  }
}

fn element_to_response(element: element.Element(a)) -> Response {
  element
  |> element.to_document_string
  |> wisp.html_response(200)
}

// type Page {
//   Home
//   Features
//   Faq
//   About
// }

fn main_page(content: element.Element(a), lang: tr.Lang, page: String) {
  html.html([], [
    html.head([], [
      html.title([], "D-LAN"),
      html.link([attr.rel("shortcut icon"), attr.href("static/favicon.ico")]),
      html.link([attr.rel("stylesheet"), attr.href("static/style.css")]),
      html.link([
        attr.rel("stylesheet"),
        attr.href("colorbox/colorbox.css"),
      ]),
      html.script([attr.src("static/js/jquery-3.5.0.min.js")], ""),
      html.script([attr.src("static/colorbox/jquery.colorbox-min.js")], ""),
      html.script([attr.src("static/js/snow.js")], ""),
      html.script([attr.src("static/js/d_lan.js")], ""),
    ]),
    html.body([], [
      html.canvas([attr.id("canvas-menu")]),
      html.div([attr.id("header")], [
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
        language_ul(lang, page),
      ]),
      html.div([attr.id("content-bg")], [content]),
    ]),
  ])
}

fn language_ul(current_lang: tr.Lang, page: String) -> element.Element(a) {
  html.ul(
    [attr.id("langs")],
    tr.all_langs()
      |> list.map(fn(l) {
        html.li([], [
          html.a(
            [
              attr.href(page <> "?lang=" <> tr.to_str(l)),
              attr.class(case l == current_lang {
                True -> "current-lang"
                False -> ""
              }),
            ],
            [
              html.text(tr.plain_lang(l)),
            ],
          ),
        ])
      }),
  )
}

fn home_page(lang: tr.Lang, page: String) -> element.Element(a) {
  html.div([attr.id("content"), attr.class("home")], [
    html.h1([], [html.em([], [tr.title(lang)])]),
  ])
  |> main_page(lang, page)
}
