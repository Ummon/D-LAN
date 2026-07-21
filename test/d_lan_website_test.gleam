import app/router
import app/web
import gleam/http
import wisp/simulate

import gleeunit

pub fn main() {
  gleeunit.main()
}

pub fn test1() {
  // let appCtx = web.AppContext("", "")
  // let ctx = Context(app:, lang: tr.current_lang(req))

  // let response = router.handle_request(simulate.browser_request(http.Get, "/"))

  // assert response.status == 200

  // assert response.headers == [#("content-type", "text/html; charset=utf-8")]

  // assert simulate.read_body(response) == "<h1>Hello, Joe!</h1>"
  assert True
}
