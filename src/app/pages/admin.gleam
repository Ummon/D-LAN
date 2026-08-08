import app/date
import app/web
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/time/calendar
import gleam/time/timestamp
import lustre/attribute as attr
import lustre/element
import lustre/element/html

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

  let #(file, year, month) = {
    case ctx.params {
      web.AdminParams(file, month, year) -> #(
        file,
        year |> option.unwrap(current_date.year),
        month |> option.unwrap(current_date.month |> calendar.month_to_int),
      )
      _ -> {
        #(
          files |> list.first |> result.unwrap(""),
          current_date.year,
          current_date.month |> calendar.month_to_int,
        )
      }
    }
  }

  let #(next_month, next_year) = case month {
    12 -> #(1, year + 1)
    m -> #(m + 1, year)
  }

  let #(prev_month, prev_year) = case month {
    1 -> #(12, year - 1)
    m -> #(m - 1, year)
  }

  let first_day =
    first_grid_day(calendar.Date(
      day: 1,
      month: month |> calendar.month_from_int |> result.unwrap(calendar.January),
      year:,
    ))
  let last_day =
    calendar.Date(
      day: 1,
      month: next_month
        |> calendar.month_from_int
        |> result.unwrap(calendar.January),
      year: next_year,
    )
    |> date.previous_day
    |> last_grid_day

  let nb_days = date.nb_days(first_day, last_day)

  let counts =
    ctx.app.db.get_download_counts(
      file,
      date.date_to_str(first_day),
      date.date_to_str(last_day),
    )

  let url_params = fn(month, year) {
    "/admin.html?file="
    <> file
    <> "&month="
    <> int.to_string(month)
    <> "&year="
    <> int.to_string(year)
  }

  [
    html.select(
      [attr.id("file")],
      files
        |> list.map(fn(f) {
          html.option([attr.selected(f == file), attr.value(f)], f)
        }),
    ),
    html.div([attr.class("calendar")], [
      html.div([attr.class("month-selector")], [
        html.a(
          [
            attr.class("prev"),
            attr.href(url_params(prev_month, prev_year)),
          ],
          [
            html.text("🡄"),
          ],
        ),
        html.div([], [
          html.a([attr.href("/admin.html?file=" <> file)], [
            html.text(int.to_string(year) <> " " <> date.month_name(month)),
          ]),
        ]),
        html.a(
          [
            attr.class("next"),
            attr.href(url_params(next_month, next_year)),
          ],
          [html.text("🡆")],
        ),
      ]),
      html.ul(
        [attr.class("days")],
        ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
          |> list.map(fn(d) { html.li([attr.class("weekday")], [html.text(d)]) })
          |> list.append(
            int.range(0, nb_days + 1, [], fn(acc, d) {
              let date = first_day |> date.add_days(d)
              let nb_downloads =
                counts
                |> list.key_find(date.date_to_str(date))
                |> result.unwrap(0)
              let day_element =
                html.li(
                  [
                    attr.classes([
                      #(
                        "current-month",
                        date.month |> calendar.month_to_int == month,
                      ),
                      #("today", date == current_date),
                    ]),
                  ],
                  [
                    html.p([attr.class("day-num")], [
                      html.text(date.day |> int.to_string),
                    ]),
                    html.p([attr.class("download-count")], [
                      html.text(nb_downloads |> int.to_string),
                    ]),
                  ],
                )
              [day_element, ..acc]
            })
            |> list.reverse,
          ),
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
