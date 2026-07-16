import app/router
import app/web
import config
import gleam/erlang/process
import mist
import radiate
import wisp
import wisp/wisp_mist

pub fn main() {
  wisp.configure_logger()

  // Watch 'src' and hot-reload modules when files change.
  let _ =
    radiate.new()
    |> radiate.add_dir("src")
    |> radiate.start()

  let conf = config.load_config()

  // Here we generate a secret key, but in a real application you would want to
  // load this from somewhere so that it is not regenerated on every restart.
  let secret_key_base = wisp.random_string(64)

  let ctx = web.Context(static_directory: static_directory())

  let handler = router.handle_request(_, ctx)

  let assert Ok(_) =
    wisp_mist.handler(handler, secret_key_base)
    |> mist.new
    |> mist.port(conf.port)
    |> mist.start

  // The web server runs in new Erlang process, so put this one to sleep while
  // it works concurrently.
  process.sleep_forever()
}

pub fn static_directory() -> String {
  let assert Ok(priv_directory) = wisp.priv_directory("d_lan_website")
  // io.println("PRIV DIR: " <> priv_directory)
  priv_directory <> "/static"
}
