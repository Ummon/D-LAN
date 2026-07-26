import app/db
import gleam/http
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import password
import translations as tr
import wisp

pub type Params {
  NoParams
  AdminParams(file: String, month: Int, year: Int)
}

pub type AppContext {
  AppContext(
    static_directory: String,
    releases_directory: String,
    db: db.Db,
    admin_password: String,
    dev_mode: Bool,
  )
}

pub type Context {
  Context(app: AppContext, lang: tr.Lang, is_admin: Bool, params: Params)
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

  // Log information about the request and response, in dev mode only.
  use <- log_request_in_dev(req, app.dev_mode)

  // Return a default 500 response if the request handler crashes.
  use <- wisp.rescue_crashes

  // Rewrite HEAD requests to GET requests and return an empty body.
  use req <- wisp.handle_head(req)

  // Known-header based CSRF protection for non-HEAD/GET requests
  use req <- wisp.csrf_known_header_protection(req)

  use <- serve_static_cached(req, from: app.static_directory)

  use user_status <- handle_auth(req, app)

  let params = extract_params(req)

  // Set the current language.
  let ctx =
    Context(
      app:,
      lang: tr.current_lang(req),
      is_admin: user_status == IsAdmin,
      params:,
    )

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
        cookie_max_age,
      )
    _ -> response
  }
}

type UserStatus {
  IsAdmin
  IsNormalUser
}

const auth_cookie_name = "auth"

// One year.
const cookie_max_age = 31_536_000

/// Check if hashed password is given via POST else try to find it in cookie.
/// Then match the hashed password with the stored hashed password.
fn handle_auth(
  req: wisp.Request,
  app_ctx: AppContext,
  cont: fn(UserStatus) -> wisp.Response,
) -> wisp.Response {
  case wisp.get_cookie(req, auth_cookie_name, wisp.Signed), req.method {
    Ok(cookie), _ if cookie != "" && cookie == app_ctx.admin_password ->
      cont(IsAdmin)

    _, http.Post -> {
      // Parses the body; short-circuits with 400/415 on bad/oversized input.
      use form <- wisp.require_form(req)
      let given_password_is_valid =
        list.key_find(form.values, "password")
        |> result.map(password.verify(_, app_ctx.admin_password))

      case given_password_is_valid {
        Ok(True) ->
          cont(IsAdmin)
          |> wisp.set_cookie(
            req,
            auth_cookie_name,
            app_ctx.admin_password,
            wisp.Signed,
            cookie_max_age,
          )
        Ok(False) -> {
          wisp.log_info("Trying login as admin failed: password doesn't match")
          cont(IsNormalUser)
        }
        Error(Nil) -> cont(IsNormalUser)
      }
    }

    _, _ -> cont(IsNormalUser)
  }
}

fn extract_params(req: wisp.Request) -> Params {
  let find = list.key_find(wisp.get_query(req), _)
  case
    find("file"),
    find("month") |> result.try(int.parse),
    find("year") |> result.try(int.parse)
  {
    Ok(file), Ok(month), Ok(year) -> AdminParams(file:, month:, year:)
    _, _, _ -> NoParams
  }
}

// Logs the request and its response only when running in dev mode, to keep the
// production logs free of one entry per request.
fn log_request_in_dev(
  req: wisp.Request,
  dev_mode: Bool,
  next handler: fn() -> wisp.Response,
) -> wisp.Response {
  case dev_mode {
    True -> wisp.log_request(req, handler)
    False -> handler()
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
