import app/pages
import app/pages/about
import app/pages/faq
import app/pages/features
import app/pages/home
import app/web
import lustre/element
import translations as tr
import wisp.{type Request, type Response}

/// The HTTP request handler- your application!
pub fn handle_request(req: Request, app_ctx: web.AppContext) -> Response {
  // Apply the middleware stack for this request/response.
  use req, ctx <- web.middleware(req, app_ctx)

  let page = case wisp.path_segments(req) {
    [p] -> p
    _ -> "home.html"
  }

  case page {
    "home.html" ->
      home.page(ctx)
      |> to_main_page_response(ctx.lang, page)
    "features.html" ->
      features.page(ctx)
      |> to_main_page_response(ctx.lang, page)
    "faq.html" ->
      faq.page(ctx)
      |> to_main_page_response(ctx.lang, page)
    "about.html" ->
      about.page(ctx)
      |> to_main_page_response(ctx.lang, page)
    _ -> wisp.not_found()
  }
}

fn to_main_page_response(
  element: element.Element(a),
  lang: tr.Lang,
  page: String,
) -> Response {
  element
  |> pages.main_page(lang, page)
  |> element_to_response
}

fn element_to_response(element: element.Element(a)) -> Response {
  element
  |> element.to_document_string
  |> wisp.html_response(200)
}
