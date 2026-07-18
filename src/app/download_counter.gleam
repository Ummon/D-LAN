//// Counts file downloads in a SQLite database, keyed by
//// "<platform>/<filename>".

import gleam/dynamic/decode
import gleam/list
import gleam/result
import sqlight.{type Connection}
import wisp

const db_filename = "downloads.sqlite3"

pub fn connect() -> Result(Connection, sqlight.Error) {
  use db <- result.try(sqlight.open("file:" <> db_filename))
  use _ <- result.try(sqlight.exec(
    "CREATE TABLE IF NOT EXISTS downloads (
       file TEXT PRIMARY KEY,
       count INTEGER NOT NULL
     ) STRICT",
    db,
  ))
  Ok(db)
}

pub fn increment(db: Connection, file: String) -> Nil {
  let result =
    sqlight.query(
      "INSERT INTO downloads (file, count) VALUES (?, 1)
       ON CONFLICT (file) DO UPDATE SET count = count + 1",
      db,
      [sqlight.text(file)],
      decode.success(Nil),
    )
  case result {
    Ok(_) -> Nil
    Error(error) ->
      wisp.log_error("Unable to count download: " <> error.message)
  }
}

pub fn count(db: Connection, file: String) -> Int {
  sqlight.query(
    "SELECT count FROM downloads WHERE file = ?",
    db,
    [sqlight.text(file)],
    decode.at([0], decode.int),
  )
  |> result.unwrap([])
  |> list.first
  |> result.unwrap(0)
}
