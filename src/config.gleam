import gleam/io
import gleam/result
import simplifile
import tomlet
import wisp

const default_port = 8089

const config_filename = "config.toml"

pub type Config {
  Config(port: Int, secret: String)
}

pub fn load_config() -> Result(Config, String) {
  use error <- result.try_recover(read_config())
  io.println("Error: " <> error <> ", creating default config file..")
  create_default_config()
}

fn read_config() -> Result(Config, String) {
  use text <- result.try(
    simplifile.read(config_filename)
    |> result.replace_error("Unable to read config file"),
  )
  use doc <- result.try(
    tomlet.parse(text) |> result.replace_error("Unable to parse config"),
  )
  use port <- result.try(
    tomlet.get_int(doc, ["port"])
    |> result.replace_error("Unable to read port"),
  )
  use secret <- result.try(
    tomlet.get_string(doc, ["secret"])
    |> result.replace_error("Unable to read secret"),
  )
  Ok(Config(port:, secret:))
}

fn create_default_config() -> Result(Config, String) {
  use doc <- result.try(
    tomlet.set_int(tomlet.new(), ["port"], default_port)
    |> result.replace_error("Unable to set port"),
  )

  let secret = wisp.random_string(64)
  use doc <- result.try(
    tomlet.set_string(doc, ["secret"], secret)
    |> result.replace_error("Unable to set secret"),
  )

  use _ <- result.try(
    tomlet.to_string(doc)
    |> simplifile.write(config_filename, _)
    |> result.replace_error("Unable to write config file"),
  )

  Ok(Config(port: default_port, secret:))
}
