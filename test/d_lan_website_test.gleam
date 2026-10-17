import app/date
import app/db
import app/download_button
import app/pages/admin
import app/router
import app/web
import gleam/http
import gleam/list
import gleam/string
import gleam/time/calendar
import gleam/time/timestamp
import lustre/element
import password
import simplifile
import translations as tr
import wisp
import wisp/simulate

import gleeunit

pub fn main() {
  wisp.configure_logger()
  gleeunit.main()
}

pub fn index_test() {
  let app_ctx = web.AppContext("static", "release", db_stub(), "", False)

  let response =
    router.handle_request(simulate.browser_request(http.Get, "/"), app_ctx)

  assert response.status == 200
  assert response.headers == [#("content-type", "text/html; charset=utf-8")]
}

pub fn page_routes_test() {
  let app_ctx = web.AppContext("static", "release", db_stub(), "", False)
  let status = fn(path) {
    router.handle_request(simulate.browser_request(http.Get, path), app_ctx).status
  }

  assert status("/") == 200
  assert status("/home.html") == 200
  assert status("/about.html") == 200
  assert status("/missing") == 404
  assert status("/missing/nested") == 404
  assert status("/about.html/extra") == 404
  assert status("/download/windows") == 404
}

fn admin_html(query: List(#(String, String))) -> String {
  let app_ctx = web.AppContext("static", "release", db_stub(), "", False)
  admin.page(web.Context(app: app_ctx, lang: tr.En, is_admin: True, query:))
  |> element.to_string
}

pub fn admin_navigation_encodes_filename_test() {
  let html =
    admin_html([
      #("file", "windows/D-LAN &#+%.exe"),
      #("month", "1"),
      #("year", "2026"),
    ])
  let file_url = "/admin.html?file=windows%2FD-LAN%20%26%23%2B%25.exe"

  assert string.contains(html, "href=\"" <> file_url <> "\"")
  assert string.contains(
    html,
    "href=\"" <> file_url <> "&amp;month=12&amp;year=2025\"",
  )
  assert string.contains(
    html,
    "href=\"" <> file_url <> "&amp;month=2&amp;year=2026\"",
  )
}

pub fn admin_month_test() {
  // The month and year don't need a file.
  let html = admin_html([#("month", "12"), #("year", "2025")])
  assert string.contains(html, ">2025 Dec<")
  assert string.contains(
    html,
    "href=\"/admin.html?file=all&amp;month=1&amp;year=2026\"",
  )

  // An invalid month is replaced by the current one.
  let #(today, _) =
    timestamp.system_time() |> timestamp.to_calendar(calendar.local_offset())
  let current_month = date.month_name(calendar.month_to_int(today.month))
  assert string.contains(
    admin_html([#("month", "13"), #("year", "2026")]),
    ">2026 " <> current_month <> "<",
  )
}

fn db_stub() {
  db.Db(fn(_) { Nil }, fn() { [] }, fn(_, _, _) { [] }, fn(_, _) { [] })
}

pub fn date_to_str_test() {
  assert date.date_to_str(calendar.Date(2026, calendar.January, 5))
    == "2026-01-05"
  assert date.date_to_str(calendar.Date(2026, calendar.October, 9))
    == "2026-10-09"
  assert date.date_to_str(calendar.Date(2026, calendar.December, 25))
    == "2026-12-25"
}

pub fn parse_date_test() {
  assert date.parse_date("2026-02-03")
    == Ok(calendar.Date(2026, calendar.February, 3))

  assert date.parse_date("2026-13-01") == Error(Nil)
  assert date.parse_date("2026-02") == Error(Nil)
  assert date.parse_date("2026-ab-01") == Error(Nil)
  assert date.parse_date("") == Error(Nil)
}

pub fn date_round_trip_test() {
  let date = calendar.Date(2026, calendar.February, 3)
  assert date.parse_date(date.date_to_str(date)) == Ok(date)
}

pub fn localized_date_test() {
  let released = calendar.Date(2026, calendar.July, 10)
  assert tr.format_date(tr.En, released) == "July 10, 2026"
  assert tr.format_date(tr.Fr, released) == "10 juillet 2026"
  assert tr.format_date(tr.De, released) == "10. Juli 2026"
  assert tr.format_date(tr.Es, released) == "10 de julio de 2026"
  assert tr.format_date(tr.It, released) == "10 luglio 2026"
  assert tr.format_date(tr.Ru, released) == "10 июля 2026 г."
  assert tr.format_date(tr.Ko, released) == "2026년 7월 10일"
  assert tr.format_date(tr.Ja, released) == "2026年7月10日"
  assert tr.format_date(tr.Hi, released) == "10 जुलाई 2026"
  assert tr.format_date(tr.Zh, released) == "2026年7月10日"
  assert tr.format_date(tr.Pt, released) == "10 de julho de 2026"
  assert tr.format_date(tr.Id, released) == "10 Juli 2026"
  assert tr.format_date(tr.Bn, released) == "10 জুলাই 2026"
  assert tr.format_date(tr.Ur, released) == "10 جولائی 2026"
  assert tr.format_date(tr.Ar, released) == "10 يوليو 2026"

  assert tr.format_date(tr.Fr, calendar.Date(2026, calendar.January, 5))
    == "5 janvier 2026"
  assert tr.format_date(tr.Ru, calendar.Date(2026, calendar.December, 25))
    == "25 декабря 2026 г."
}

pub fn direction_test() {
  assert tr.direction(tr.En) == "ltr"
  assert tr.direction(tr.Ur) == "rtl"
  assert tr.direction(tr.Ar) == "rtl"
}

pub fn download_button_release_formats_test() {
  [
    #("windows", "Setup.exe", "<em>windows</em>", "exe"),
    #("mac", "x86_64.dmg", "<em>mac</em> x86_64", "dmg"),
    #("linux", "amd64.deb", "<em>linux</em>", "deb"),
    #("linux", "x86_64.AppImage", "<em>linux</em> x86_64", "AppImage"),
  ]
  |> list.each(fn(release) {
    let #(platform, suffix, platform_text, extension) = release
    let releases_directory = "build/download_button_release_formats_test"
    let directory = releases_directory <> "/" <> platform
    let filename = "D-LAN-1.2.0Beta1-2026-09-08_17-54-" <> suffix
    let path = directory <> "/" <> filename
    let assert Ok(_) = simplifile.create_directory_all(directory)
    let assert Ok(_) = simplifile.write(path, "test installer")
    let ctx =
      web.Context(
        app: web.AppContext("static", releases_directory, db_stub(), "", False),
        lang: tr.En,
        is_admin: False,
        query: [],
      )

    let button = download_button.element(ctx, platform)
    let assert Ok(_) = simplifile.delete_file(path)
    let assert Ok(button) = button
    let html = element.to_string(button)
    assert string.contains(
      html,
      "Version 1.2.0 Beta1 for " <> platform_text <> "<",
    )
    assert string.contains(html, "Released on September 8, 2026")
    assert string.contains(html, "download " <> extension <> " " <> platform)
  })
}

pub fn latest_release_test() {
  // A newer minor version has a lower lexicographical order than an older one.
  assert download_button.latest_release([
      "D-LAN-1.9.0-2027-01-01_10-00-Setup.exe",
      "D-LAN-1.10.0-2027-06-01_10-00-Setup.exe",
    ])
    == Ok("D-LAN-1.10.0-2027-06-01_10-00-Setup.exe")

  // A beta has a higher lexicographical order than its final release.
  assert download_button.latest_release([
      "D-LAN-1.2.0Beta1-2026-07-10_19-21-Setup.exe",
      "D-LAN-1.2.0-2026-08-01_10-00-Setup.exe",
    ])
    == Ok("D-LAN-1.2.0-2026-08-01_10-00-Setup.exe")

  // Two builds of the same version are told apart by their time.
  assert download_button.latest_release([
      "D-LAN-1.1.0Beta15-2012-12-16_16-45-amd64.deb",
      "D-LAN-1.1.0Beta15-2012-12-16_16-22-amd64.deb",
    ])
    == Ok("D-LAN-1.1.0Beta15-2012-12-16_16-45-amd64.deb")

  // Files not following the release naming scheme are ignored.
  assert download_button.latest_release([
      "D-LAN.exe",
      "D-LAN-1.2.0-2026-08-01_10-00-Setup.exe",
    ])
    == Ok("D-LAN-1.2.0-2026-08-01_10-00-Setup.exe")

  assert download_button.latest_release(["D-LAN.exe"]) == Error(Nil)
  assert download_button.latest_release([]) == Error(Nil)
}

pub fn current_lang_test() {
  let lang = fn(accept_language) {
    simulate.request(http.Get, "/")
    |> simulate.header("accept-language", accept_language)
    |> tr.current_lang
  }

  // The qualities are compared, not the order of the entries.
  assert lang("fr;q=0.1,de;q=0.9") == tr.De

  // A parameter may be surrounded by whitespaces and its name is case
  // insensitive.
  assert lang("fr; q=0.1, de; Q=0.9") == tr.De
  assert lang("fr;charset=utf-8;q=0.1, de;q=0.9") == tr.De

  // Language tags are case insensitive and their subtags are ignored.
  assert lang("FR-CH") == tr.Fr
  assert lang("zh-CN") == tr.Zh
  assert lang("pt-BR") == tr.Pt
  assert lang("id-ID") == tr.Id
  assert lang("bn-BD") == tr.Bn
  assert lang("ur-PK") == tr.Ur
  assert lang("ar-EG") == tr.Ar

  // Without a quality an entry has the best one, ties keep the header order.
  assert lang("de,fr") == tr.De
  assert lang("fr,de;q=0.9") == tr.Fr

  // Unknown languages are ignored, English is the default.
  assert lang("zz-ZZ,it") == tr.It
  assert lang("zz") == tr.En
  assert lang("") == tr.En
  assert simulate.request(http.Get, "/") |> tr.current_lang == tr.En
}

pub fn current_lang_unknown_falls_through_test() {
  let request =
    simulate.request(http.Get, "/?lang=zz")
    |> simulate.header("accept-language", "de")

  // An unknown 'lang' variable falls back to the cookie, then to the header.
  assert request
    |> simulate.cookie("lang", "fr", wisp.PlainText)
    |> tr.current_lang
    == tr.Fr
  assert request
    |> simulate.cookie("lang", "zz", wisp.PlainText)
    |> tr.current_lang
    == tr.De
  assert request |> tr.current_lang == tr.De

  assert simulate.request(http.Get, "/?lang=it")
    |> simulate.cookie("lang", "fr", wisp.PlainText)
    |> tr.current_lang
    == tr.It
}

pub fn lang_cookie_set_only_for_known_lang_test() {
  let app_ctx = web.AppContext("static", "release", db_stub(), "", False)
  let lang_cookie = fn(path) {
    router.handle_request(simulate.browser_request(http.Get, path), app_ctx).headers
    |> list.any(fn(header) {
      header.0 == "set-cookie" && string.starts_with(header.1, "lang=")
    })
  }

  assert lang_cookie("/?lang=fr")
  assert !lang_cookie("/?lang=zz")
  assert !lang_cookie("/")
}

pub fn admin_auth_test() {
  let app_ctx =
    web.AppContext("static", "release", db_stub(), password.hash("pw"), False)
  let login = fn(given_password) {
    let req =
      simulate.browser_request(http.Post, "/admin.html")
      |> simulate.form_body([#("password", given_password)])
    #(req, router.handle_request(req, app_ctx))
  }
  let is_admin = fn(response) {
    string.contains(simulate.read_body(response), "class=\"calendar\"")
  }

  // A wrong password shows the login form again.
  let #(_, response) = login("wrong")
  assert !is_admin(response)

  // The right password grants access and sets the authentication cookie, which
  // grants access to the next requests.
  let #(req, response) = login("pw")
  assert is_admin(response)
  let next_response =
    simulate.browser_request(http.Get, "/admin.html")
    |> simulate.session(req, response)
    |> router.handle_request(app_ctx)
  assert is_admin(next_response)

  // Without the cookie, the access is denied.
  assert !is_admin(router.handle_request(
    simulate.browser_request(http.Get, "/admin.html"),
    app_ctx,
  ))
}
