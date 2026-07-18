import app/download_counter
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

  // Here we generate a secret key, but in a real application you would want to
  // load this from somewhere so that it is not regenerated on every restart.
  let secret_key_base = wisp.random_string(64)

  let assert Ok(db) = download_counter.connect()

  let app_ctx =
    web.AppContext(
      static_directory: priv_directory() <> "/static",
      releases_directory: priv_directory() <> "/releases",
      db:,
    )

  let handler = router.handle_request(_, app_ctx)

  let assert Ok(_) =
    wisp_mist.handler(handler, secret_key_base)
    |> mist.new
    |> mist.port(conf.port)
    |> mist.start

  // The web server runs in new Erlang process, so put this one to sleep while
  // it works concurrently.
  process.sleep_forever()
}

fn priv_directory() -> String {
  let assert Ok(priv_directory) = wisp.priv_directory("d_lan_website")
  priv_directory
}
