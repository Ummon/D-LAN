import gleam/float
import gleam/int
import gleam/result
import gleam/string
import gleam/time/calendar.{type Date}
import gleam/time/duration
import gleam/time/timestamp

pub type Weekday {
  Monday
  Tuesday
  Wednesday
  Thursday
  Friday
  Saturday
  Sunday
}

pub fn weekday(date: Date) -> Weekday {
  let m = calendar.month_to_int(date.month)
  let y = case m < 3 {
    True -> date.year - 1
    False -> date.year
  }
  let t = case m {
    1 -> 0
    2 -> 3
    3 -> 2
    4 -> 5
    5 -> 0
    6 -> 3
    7 -> 5
    8 -> 1
    9 -> 4
    10 -> 6
    11 -> 2
    _ -> 4
  }
  let n = { y + y / 4 - y / 100 + y / 400 + t + date.day } % 7
  case n {
    0 -> Sunday
    1 -> Monday
    2 -> Tuesday
    3 -> Wednesday
    4 -> Thursday
    5 -> Friday
    _ -> Saturday
  }
}

pub fn previous_day(date: calendar.Date) -> calendar.Date {
  add_days(date, -1)
}

pub fn next_day(date: calendar.Date) -> calendar.Date {
  add_days(date, 1)
}

pub fn add_days(date: calendar.Date, days: Int) -> calendar.Date {
  let #(date, _) =
    date
    |> to_timestamp
    |> timestamp.add(duration.hours(24 * days))
    |> timestamp.to_calendar(calendar.utc_offset)
  date
}

pub fn nb_days(d1: calendar.Date, d2: calendar.Date) -> Int {
  let #(seconds, _) =
    timestamp.difference(to_timestamp(d1), to_timestamp(d2))
    |> duration.to_seconds_and_nanoseconds
  seconds / 86_400
}

fn to_timestamp(date: calendar.Date) -> timestamp.Timestamp {
  timestamp.from_calendar(
    date,
    calendar.TimeOfDay(12, 0, 0, 0),
    calendar.utc_offset,
  )
}

pub fn parse_date(date: String) -> Result(Date, Nil) {
  case date |> string.split("-") {
    [y, m, d] -> {
      use y <- result.try(int.parse(y))
      use m <- result.try(int.parse(m))
      use d <- result.try(int.parse(d))
      use m <- result.map(calendar.month_from_int(m))
      calendar.Date(y, m, d)
    }
    _ -> Error(Nil)
  }
}

pub fn date_to_str(date: Date) -> String {
  ymd_to_str(date.year, date.month |> calendar.month_to_int, date.day)
}

pub fn ymd_to_str(y: Int, m: Int, d: Int) -> String {
  y |> int.to_string()
  <> "-"
  <> case m < 10 {
    True -> "0"
    False -> ""
  }
  <> m |> int.to_string()
  <> "-"
  <> case d < 10 {
    True -> "0"
    False -> ""
  }
  <> d |> int.to_string()
}

pub fn month_name(month: Int) -> String {
  case month {
    1 -> "Jan"
    2 -> "Feb"
    3 -> "Mar"
    4 -> "Apr"
    5 -> "May"
    6 -> "Jun"
    7 -> "Jul"
    8 -> "Aug"
    9 -> "Sep"
    10 -> "Oct"
    11 -> "Nov"
    _ -> "Dec"
  }
}

// Formats a size in bytes as MiB with two decimals, e.g. "24.53".
pub fn file_size_mib(bytes: Int) -> String {
  let hundredths = float.round(int.to_float(bytes) *. 100.0 /. 1_048_576.0)
  int.to_string(hundredths / 100)
  <> "."
  <> string.pad_start(int.to_string(hundredths % 100), 2, "0")
}
