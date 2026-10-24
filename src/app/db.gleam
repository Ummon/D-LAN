//// All persistent data are stored in a SQLite database.
//// - Counts file downloads keyed by ("<platform>/<filename>", date).

import gleam/dynamic/decode
import gleam/result
import sqlight
import wisp

const db_filename = "d_lan_website.sqlite3"

pub type Db {
  Db(
    increment_download_count: fn(String) -> Nil,
    get_files: fn() -> List(String),
    get_download_counts: fn(String, String, String) -> List(#(String, Int)),
    get_download_counts_total: fn(String, String) -> List(#(String, Int)),
  )
}

pub type Error {
  CantOpenDbFile(String)
  CantSetJournalMode
  CantCreateDatabase
}

pub fn connect() -> Result(Db, Error) {
  use db <- result.try(
    sqlight.open("file:" <> db_filename)
    |> result.replace_error(CantOpenDbFile(db_filename)),
  )
  // Write-ahead logging lets readers and writers work at the same time. This
  // setting is persistent, it's stored in the database file itself.
  use _ <- result.try(
    sqlight.exec("PRAGMA journal_mode = WAL;", db)
    |> result.replace_error(CantSetJournalMode),
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
  // try_import_dets(db)
  Ok(
    Db(
      increment_download_count(db, _),
      fn() { get_files(db) },
      fn(file, start_date, end_date) {
        get_download_counts(db, file, start_date, end_date)
      },
      fn(start_date, end_date) {
        get_download_counts_total(db, start_date, end_date)
      },
    ),
  )
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
    "SELECT file FROM downloads GROUP BY file ORDER BY MAX(date) DESC, file",
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

fn get_download_counts_total(
  db: sqlight.Connection,
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
SELECT date, SUM(count) FROM downloads
WHERE date >= ? AND date <= ?
GROUP BY date
ORDER BY date",
    db,
    [sqlight.text(start_date), sqlight.text(end_date)],
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
