import app/pages
import app/pages/about
import app/pages/admin
import app/pages/donate
import app/pages/faq
import app/pages/features
import app/pages/home
import app/web
import gleam/http/request
import gleam/http/response
import gleam/result
import gleam/string
import gleam/uri
import lustre/element
import simplifile
import translations as tr
import wisp.{type Request, type Response}

/// The HTTP request handler- your application!
pub fn handle_request(req: Request, app_ctx: web.AppContext) -> Response {
  // Apply the middleware stack for this request/response.
  use req, ctx <- web.middleware(req, app_ctx)

  case wisp.path_segments(req) {
    ["download", platform, filename] ->
      serve_release(req, ctx, platform, filename)
    segments -> serve_page(segments, ctx)
  }
}

fn serve_page(segments: List(String), ctx: web.Context) -> Response {
  let page = case segments {
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
    "donate.html" ->
      donate.page(ctx)
      |> to_main_page_response(ctx.lang, page)
    "admin.html" ->
      admin.page(ctx)
      |> to_main_page_response(ctx.lang, page)
    _ -> wisp.not_found()
  }
}

// Serves a file from the releases directory and counts the download.
fn serve_release(
  req: Request,
  ctx: web.Context,
  platform: String,
  filename: String,
) -> Response {
  let segments = {
    use platform <- result.try(uri.percent_decode(platform))
    use filename <- result.try(uri.percent_decode(filename))
    case is_safe_segment(platform) && is_safe_segment(filename) {
      True -> Ok(#(platform, filename))
      False -> Error(Nil)
    }
  }

  case segments {
    Ok(#(platform, filename)) -> {
      let path =
        ctx.app.releases_directory <> "/" <> platform <> "/" <> filename
      case simplifile.file_info(path) |> result.map(simplifile.file_info_type) {
        Ok(simplifile.File) -> {
          // Don't count HEAD requests (rewritten to GET by 'wisp.handle_head')
          // as downloads.
          case request.get_header(req, "x-original-method") {
            Ok("HEAD") -> Nil
            _ ->
              ctx.app.db.increment_download_count(platform <> "/" <> filename)
          }
          wisp.ok()
          |> response.set_header("content-type", "application/octet-stream")
          |> wisp.file_download(named: filename, from: path)
        }
        _ -> wisp.not_found()
      }
    }
    Error(Nil) -> wisp.not_found()
  }
}

// Guards against path traversal: a decoded segment must be a plain file or
// directory name.
fn is_safe_segment(segment: String) -> Bool {
  segment != ""
  && !string.contains(segment, "/")
  && !string.contains(segment, "\\")
  && !string.contains(segment, "..")
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
  wisp.response(200)
  |> wisp.set_header("content-type", "text/html; charset=utf-8")
  |> wisp.string_tree_body(element.to_document_string_tree(element))
}
