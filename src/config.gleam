import simplifile
import tom

pub type Config {
  Config(port: Int)
}

pub fn load_config() -> Config {
  // Read configuration file.
  let assert Ok(config_text) = simplifile.read("config.toml")
  let assert Ok(config) = tom.parse(config_text)
  let assert Ok(port) = tom.get_int(config, ["port"])

  Config(port:)
}
