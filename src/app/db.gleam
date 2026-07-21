//// All persistent data are stored in a SQLite database.
//// - Counts file downloads keyed by ("<platform>/<filename>", date).

import gleam/dynamic/decode
import gleam/result
import sqlight
import wisp

const db_filename = "d_lan_website.sqlite3"

pub type Db {
  Db(increment_download_count: fn(String) -> Nil)
}

pub type Error {
  CantOpenDbFile(String)
  CantCreateDatabase
}

pub fn connect() -> Result(Db, Error) {
  // Result(sqlight.Connection, sqlight.Error) {
  use db <- result.try(
    sqlight.open("file:" <> db_filename)
    |> result.replace_error(CantOpenDbFile(db_filename)),
  )
  use _ <- result.try(
    sqlight.exec(
      "CREATE TABLE IF NOT EXISTS downloads (
      id INTEGER PRIMARY KEY,
      file TEXT,
      date TEXT, -- format: 'YYYY-MM-DD'.
      count INTEGER NOT NULL,
      UNIQUE(file, date)
    ) STRICT;
    ",
      db,
    )
    |> result.replace_error(CantCreateDatabase),
  )
  Ok(Db(increment_download_count: increment_download_count(db, _)))
}

fn increment_download_count(db: sqlight.Connection, file: String) -> Nil {
  let result =
    sqlight.query(
      "INSERT INTO downloads (file, date, count) VALUES (?, date(), 1)
       ON CONFLICT (file, date) DO UPDATE SET count = count + 1",
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
// fn count(db: Connection, file: String) -> Int {
//   sqlight.query(
//     "SELECT count FROM downloads WHERE file = ?",
//     db,
//     [sqlight.text(file)],
//     decode.at([0], decode.int),
//   )
//   |> result.unwrap([])
//   |> list.first
//   |> result.unwrap(0)
// }
