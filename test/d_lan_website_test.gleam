import app/date
import app/db
import app/router
import app/web
import gleam/http
import gleam/time/calendar
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
