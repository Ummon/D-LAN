//// All persistent data are stored in a SQLite database.
//// - Counts file downloads keyed by ("<platform>/<filename>", date).

import app/utils
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import sqlight
import wisp

const db_filename = "d_lan_website.sqlite3"

pub type Db {
  Db(
    increment_download_count: fn(String) -> Nil,
    get_files: fn() -> List(String),
    get_download_counts: fn(String, String, String) -> List(#(String, Int)),
  )
}

pub type Error {
  CantOpenDbFile(String)
  CantCreateDatabase
}

pub fn connect() -> Result(Db, Error) {
  use db <- result.try(
    sqlight.open("file:" <> db_filename)
    |> result.replace_error(CantOpenDbFile(db_filename)),
  )
  use _ <- result.try(
    sqlight.exec(
      "
CREATE TABLE IF NOT EXISTS downloads (
  id INTEGER PRIMARY KEY,
  file TEXT,
  date TEXT, -- format: 'YYYY-MM-DD'.
  count INTEGER NOT NULL,
  UNIQUE(file, date)
) STRICT;
CREATE INDEX IF NOT EXISTS downloads_date_index ON downloads(date);
CREATE INDEX IF NOT EXISTS downloads_file_index ON downloads(file);
    ",
      db,
    )
    |> result.replace_error(CantCreateDatabase),
  )
  try_import_dets(db)
  Ok(
    Db(
      increment_download_count(db, _),
      fn() { get_files(db) },
      fn(file, start_date, end_date) {
        get_download_counts(db, file, start_date, end_date)
      },
    ),
  )
}

import slate/set

fn try_import_dets(db: sqlight.Connection) -> Nil {
  let date_decoder = {
    use y <- decode.field(0, decode.int)
    use m <- decode.field(1, decode.int)
    use d <- decode.field(2, decode.int)
    decode.success(utils.ymd_to_str(y, m, d))
  }

  case
    set.open(
      "d_lan_downloads_count.dets",
      key_decoder: {
        use file <- decode.field(0, decode.list(decode.int))
        use date <- decode.field(1, date_decoder)
        let assert Ok(file) = codepoints_to_string(file)
        decode.success(#(file, date))
      },
      value_decoder: decode.int,
    )
  {
    Ok(downloads) -> {
      let assert Ok(n) =
        downloads
        |> set.fold(0, fn(acc, k, v) {
          let #(file, date) = k
          case file |> string.ends_with("torrent") {
            True -> {
              wisp.log_info("Ignored: " <> file)
              acc
            }
            False -> {
              let assert Ok(_) =
                sqlight.query(
                  "
INSERT INTO downloads (file, date, count) VALUES (?, ?, ?)",
                  db,
                  [sqlight.text(file), sqlight.text(date), sqlight.int(v)],
                  decode.success(Nil),
                )
              acc + 1
            }
          }
        })
      wisp.log_info(int.to_string(n) <> " rows imported")
      let assert Ok(Nil) = set.close(downloads)
      Nil
    }
    _ -> wisp.log_info("No DETS file to import")
  }
  Nil
}

pub fn codepoints_to_string(chars: List(Int)) -> Result(String, Nil) {
  chars
  |> list.map(string.utf_codepoint)
  |> result.all
  |> result.map(string.from_utf_codepoints)
}

fn increment_download_count(db: sqlight.Connection, file: String) -> Nil {
  let result =
    sqlight.query(
      "
INSERT INTO downloads (file, date, count) VALUES (?, date(), 1)
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

fn get_files(db: sqlight.Connection) -> List(String) {
  sqlight.query(
    "SELECT DISTINCT file FROM downloads ORDER BY date DESC, file",
    db,
    [],
    decode.at([0], decode.string),
  )
  |> rows_or_log("Unable to retrieve files")
}

fn get_download_counts(
  db: sqlight.Connection,
  file: String,
  start_date: String,
  end_date: String,
) -> List(#(String, Int)) {
  let decoder = {
    use date <- decode.field(0, decode.string)
    use count <- decode.field(1, decode.int)
    decode.success(#(date, count))
  }
  sqlight.query(
    "
SELECT date, count FROM downloads
WHERE file = ? AND date >= ? AND date <= ?
ORDER BY date",
    db,
    [sqlight.text(file), sqlight.text(start_date), sqlight.text(end_date)],
    decoder,
  )
  |> rows_or_log("Unable to retrieve counts")
}

/// Returns the rows or logs the error and returns an empty list.
fn rows_or_log(
  result: Result(List(a), sqlight.Error),
  message: String,
) -> List(a) {
  case result {
    Ok(rows) -> rows
    Error(error) -> {
      wisp.log_error(message <> ": " <> error.message)
      []
    }
  }
}
