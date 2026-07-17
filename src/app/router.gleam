import app/pages/home
import app/web
import lustre/element
import translations as tr
import wisp.{type Request, type Response}

/// The HTTP request handler- your application!
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
    "home.html" -> home.page(lang, page) |> element_to_response
    _ -> wisp.not_found()
  }
}

fn element_to_response(element: element.Element(a)) -> Response {
  element
  |> element.to_document_string
  |> wisp.html_response(200)
}
