import gleam/time/calendar.{type Date}

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
