import app/date
import app/db
import app/download_button
import app/router
import app/web
import gleam/http
import gleam/time/calendar
import translations as tr
import wisp
import wisp/simulate

import gleeunit

pub fn main() {
  wisp.configure_logger()
  gleeunit.main()
}

pub fn index_test() {
  let app_ctx =
    web.AppContext(
      "static",
      "release",
      db.Db(fn(_) { Nil }, fn() { [] }, fn(_, _, _) { [] }),
      "",
      False,
    )

  let response =
    router.handle_request(simulate.browser_request(http.Get, "/"), app_ctx)

  assert response.status == 200
  assert response.headers == [#("content-type", "text/html; charset=utf-8")]
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

  // Without a quality an entry has the best one, ties keep the header order.
  assert lang("de,fr") == tr.De
  assert lang("fr,de;q=0.9") == tr.Fr

  // Unknown languages are ignored, English is the default.
  assert lang("zz-ZZ,it") == tr.It
  assert lang("zz") == tr.En
  assert lang("") == tr.En
  assert simulate.request(http.Get, "/") |> tr.current_lang == tr.En
}
