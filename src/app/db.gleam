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

// To read DETS files.
import slate
import slate/set

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

/// Number of rows inserted per SQL statement when importing the dets file. Each
/// row binds three parameters, which keeps a statement far below the SQLite
/// limits on both the number of parameters and the number of rows in a 'VALUES'
/// clause.
const import_batch_size = 100

/// Try to find the dets file from the old website and import it to the new database.
/// All the tuples are inserted by batches of 'import_batch_size' rows within a
/// single transaction, thus the import either fully succeeds or leaves the
/// database untouched.
fn try_import_dets(db: sqlight.Connection) -> Nil {
  let date_decoder = {
    use y <- decode.field(0, decode.int)
    use m <- decode.field(1, decode.int)
    use d <- decode.field(2, decode.int)
    decode.success(utils.ymd_to_str(y, m, d))
  }

  case
    set.open_with_access(
      "d_lan_downloads_count.dets",
      repair: slate.AutoRepair,
      access: slate.ReadOnly,
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
      wisp.log_info("DETS to import found, importing...")
      let assert Ok(rows) =
        downloads
        |> set.fold([], fn(acc, k, v) {
          let #(file, date) = k
          case file |> string.ends_with("torrent") {
            True -> {
              // wisp.log_info("Ignored: " <> file)
              acc
            }
            False -> [#(file, date, v), ..acc]
          }
        })

      let assert Ok(Nil) = sqlight.exec("BEGIN", db)
      rows
      |> list.sized_chunk(import_batch_size)
      |> list.each(insert_downloads(db, _))
      let assert Ok(Nil) = sqlight.exec("COMMIT", db)

      let assert Ok(Nil) = set.close(downloads)
      wisp.log_info("DETS import finished")
      wisp.log_info(
        int.to_string(list.length(rows)) <> " rows imported or updated",
      )
      Nil
    }
    _ -> wisp.log_info("No DETS file to import")
  }
  Nil
}

/// Insert a batch of '#(file, date, count)' rows with a single statement. The
/// count of a row already present in the database is overwritten by the
/// imported one.
fn insert_downloads(
  db: sqlight.Connection,
  rows: List(#(String, String, Int)),
) -> Nil {
  let placeholders =
    list.repeat("(?, ?, ?)", list.length(rows)) |> string.join(", ")
  let values =
    rows
    |> list.flat_map(fn(row) {
      let #(file, date, count) = row
      [sqlight.text(file), sqlight.text(date), sqlight.int(count)]
    })
  let assert Ok(_) =
    sqlight.query(
      "INSERT INTO downloads (file, date, count) VALUES "
        <> placeholders
        <> " ON CONFLICT (file, date) DO UPDATE SET count = excluded.count",
      db,
      values,
      decode.success(Nil),
    )
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
