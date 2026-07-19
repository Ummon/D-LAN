import gleam/list
import gleam/string
import sqlight
import translations as tr
import wisp

pub type AppContext {
  AppContext(
    static_directory: String,
    releases_directory: String,
    db: sqlight.Connection,
  )
}

pub type Context {
  Context(app: AppContext, lang: tr.Lang)
}

/// The middleware stack that the request handler uses. The stack is itself a
/// middleware function!
///
/// Middleware wrap each other, so the request travels through the stack from
/// top to bottom until it reaches the request handler, at which point the
/// response travels back up through the stack.
///
/// The middleware used here are the ones that are suitable for use in your
/// typical web application.
///
pub fn middleware(
  req: wisp.Request,
  app: AppContext,
  handle_request: fn(wisp.Request, Context) -> wisp.Response,
) -> wisp.Response {
  // Permit browsers to simulate methods other than GET and POST using the
  // `_method` query parameter.
  let req = wisp.method_override(req)

  // Log information about the request and response.
  use <- wisp.log_request(req)

  // Return a default 500 response if the request handler crashes.
  use <- wisp.rescue_crashes

  // Rewrite HEAD requests to GET requests and return an empty body.
  use req <- wisp.handle_head(req)

  // Known-header based CSRF protection for non-HEAD/GET requests
  use req <- wisp.csrf_known_header_protection(req)

  use <- serve_static_cached(req, from: app.static_directory)

  // Set the current language.
  let ctx = Context(app:, lang: tr.current_lang(req))

  // Handle the request!
  let response = handle_request(req, ctx)

  // Set 'lang' cookie only if set by client.
  case req |> wisp.get_query |> list.key_find("lang") {
    Ok(_) ->
      response
      |> wisp.set_cookie(
        req,
        "lang",
        tr.to_str(ctx.lang),
        wisp.PlainText,
        365 * 24 * 60 * 60,
      )
    _ -> response
  }
}

// Serves the files under "/static" with a 'cache-control' header so browsers
// don't re-validate the render-blocking CSS/JS on every navigation, which
// causes a white flash between pages.
fn serve_static_cached(
  req: wisp.Request,
  from directory: String,
  next handler: fn() -> wisp.Response,
) -> wisp.Response {
  let response =
    wisp.serve_static(req, under: "/static", from: directory, next: handler)
  case string.starts_with(req.path, "/static"), response.status {
    True, 200 | True, 304 ->
      wisp.set_header(response, "cache-control", "public, max-age=86400")
    _, _ -> response
  }
}
