import app/db
import app/router
import app/web
import config
import gleam/erlang/process
import mist
import wisp
import wisp/wisp_mist

pub fn main() {
  wisp.configure_logger()

  let assert Ok(conf) = config.load_config()
  let assert Ok(db) = db.connect()
  let assert Ok(priv_directory) = wisp.priv_directory("d_lan_website")

  let static_directory = priv_directory <> "/static"
  let releases_directory = priv_directory <> "/releases"

  let app_ctx = web.AppContext(static_directory, releases_directory, db)

  let handler = router.handle_request(_, app_ctx)

  let assert Ok(_) =
    wisp_mist.handler(handler, conf.secret)
    |> mist.new
    |> mist.port(conf.port)
    |> mist.start

  // The web server runs in new Erlang process, so put this one to sleep while
  // it works concurrently.
  process.sleep_forever()
}
