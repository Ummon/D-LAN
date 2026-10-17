import app/date
import app/web
import gleam/int
import gleam/list
import gleam/result
import gleam/time/calendar
import gleam/time/timestamp
import gleam/uri
import lustre/attribute as attr
import lustre/element
import lustre/element/html

const all = "all"

pub fn page(ctx: web.Context) -> element.Element(a) {
  html.div([attr.id("content"), attr.class("admin")], case ctx.is_admin {
    True -> [calendar(ctx)]
    False -> [
      html.form([attr.method("post")], [
        html.label([attr.for("input-password")], [html.text("Password: ")]),
        html.input([
          attr.id("input-password"),
          attr.type_("password"),
          attr.name("password"),
          attr.autocomplete("current-password"),
        ]),
        html.input([attr.type_("submit"), attr.value("Connect")]),
      ]),
    ]
  })
}

fn calendar(ctx: web.Context) -> element.Element(a) {
  let files = ctx.app.db.get_files()

  let #(current_date, _) =
    timestamp.system_time()
    |> timestamp.to_calendar(calendar.local_offset())

  // The file, month and year are given as query parameters, a missing or
  // invalid one is replaced by its default value.
  let param = list.key_find(ctx.query, _)
  let file = param("file") |> result.unwrap(all)
  let year =
    param("year") |> result.try(int.parse) |> result.unwrap(current_date.year)
  let month =
    param("month")
    |> result.try(int.parse)
    |> result.try(calendar.month_from_int)
    |> result.unwrap(current_date.month)

  let first_of_month = calendar.Date(year:, month:, day: 1)
  let first_of_next_month =
    calendar.Date(..date.add_days(first_of_month, 31), day: 1)
  let first_of_prev_month =
    calendar.Date(..date.previous_day(first_of_month), day: 1)

  let first_day = first_grid_day(first_of_month)
  let last_day = first_of_next_month |> date.previous_day |> last_grid_day

  let counts = case file == all {
    True ->
      ctx.app.db.get_download_counts_total(
        date.date_to_str(first_day),
        date.date_to_str(last_day),
      )
    False ->
      ctx.app.db.get_download_counts(
        file,
        date.date_to_str(first_day),
        date.date_to_str(last_day),
      )
  }

  let file_url = "/admin.html?" <> uri.query_to_string([#("file", file)])
  let month_url = fn(first_of_month: calendar.Date) {
    "/admin.html?"
    <> uri.query_to_string([
      #("file", file),
      #("month", int.to_string(calendar.month_to_int(first_of_month.month))),
      #("year", int.to_string(first_of_month.year)),
    ])
  }

  let day_element = fn(day: calendar.Date) {
    let nb_downloads =
      counts |> list.key_find(date.date_to_str(day)) |> result.unwrap(0)
    html.li(
      [
        attr.classes([
          #("current-month", day.month == month),
          #("today", day == current_date),
        ]),
      ],
      [
        html.p([attr.class("day-num")], [html.text(int.to_string(day.day))]),
        html.p([attr.class("download-count")], [
          html.text(int.to_string(nb_downloads)),
        ]),
      ],
    )
  }

  // Counting down while prepending gives the days in increasing order.
  let days =
    int.range(
      from: date.nb_days(first_day, last_day),
      to: -1,
      with: [],
      run: list.prepend,
    )
    |> list.map(fn(d) { day_element(date.add_days(first_day, d)) })

  [
    html.select(
      [attr.id("file")],
      [all, ..files]
        |> list.map(fn(f) {
          html.option([attr.selected(f == file), attr.value(f)], f)
        }),
    ),
    html.div([attr.class("calendar")], [
      html.div([attr.class("month-selector")], [
        html.a([attr.class("prev"), attr.href(month_url(first_of_prev_month))], [
          html.text("🡄"),
        ]),
        html.div([], [
          html.a([attr.href(file_url)], [
            html.text(
              int.to_string(year)
              <> " "
              <> date.month_name(calendar.month_to_int(month)),
            ),
          ]),
        ]),
        html.a([attr.class("next"), attr.href(month_url(first_of_next_month))], [
          html.text("🡆"),
        ]),
      ]),
      html.ul(
        [attr.class("days")],
        ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
          |> list.map(fn(d) { html.li([attr.class("weekday")], [html.text(d)]) })
          |> list.append(days),
      ),
    ]),
  ]
  |> element.fragment()
}

fn first_grid_day(date: calendar.Date) -> calendar.Date {
  case date.weekday(date) {
    date.Monday -> date
    _ -> first_grid_day(date.previous_day(date))
  }
}

fn last_grid_day(date: calendar.Date) -> calendar.Date {
  case date.weekday(date) {
    date.Sunday -> date
    _ -> last_grid_day(date.next_day(date))
  }
}
