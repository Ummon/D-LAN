import app/pages/home
import app/web
import lustre/element
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
    "home.html" -> home.page(ctx.lang, page) |> element_to_response
    _ -> wisp.not_found()
  }
}

fn element_to_response(element: element.Element(a)) -> Response {
  element
  |> element.to_document_string
  |> wisp.html_response(200)
}
