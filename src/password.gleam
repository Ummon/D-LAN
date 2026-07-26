import argus
import argv
import gleam/io
import gleam/string

pub fn hash(password: String) -> String {
  let assert Ok(hashes) =
    argus.hasher()
    |> argus.hash(password, argus.gen_salt())

  hashes.encoded_hash
}

pub fn verify(password: String, hashed_password: String) -> Bool {
  hashed_password != "" && argus.verify(hashed_password, password) == Ok(True)
}

fn print_usage() {
  io.println("Usage: <gleam run -m password> <password>")
}

pub fn main() {
  case argv.load().arguments {
    ["--help"] | ["-h"] -> print_usage()
    [password] -> {
      let password =
        password |> string.remove_prefix("\"") |> string.remove_suffix("\"")
      let h = hash(password)
      io.println("Hash: " <> h)
    }
    _ -> {
      io.println("Uknown argument")
      print_usage()
    }
  }
}
