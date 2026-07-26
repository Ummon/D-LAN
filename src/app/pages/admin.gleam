import app/utils
import app/web
import gleam/int
import gleam/list
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

  let #(file, year, month, day) = case ctx.params {
    web.AdminParams(file, month, year) -> #(file, year, month, 0)
    _ -> {
      let #(current_date, _) =
        timestamp.system_time()
        |> timestamp.to_calendar(calendar.local_offset())
      #(
        files |> list.first |> result.unwrap(""),
        current_date.year,
        current_date.month |> calendar.month_to_int,
        current_date.day,
      )
    }
  }

  let first_day =
    first_grid_day(calendar.Date(
      day: 1,
      month: month |> calendar.month_from_int |> result.unwrap(calendar.January),
      year:,
    ))
  let last_day =
    calendar.Date(
      // Next month.
      day: 1,
      month: {
        month
        |> int.modulo(12)
        |> result.unwrap(0)
      }
      + 1
        |> calendar.month_from_int
        |> result.unwrap(calendar.January),
      year:,
    )
    |> utils.previous_day
    |> last_grid_day

  let nb_days = utils.nb_days(first_day, last_day)

  let counts =
    ctx.app.db.get_download_counts(
      file,
      utils.date_to_str(first_day),
      utils.date_to_str(last_day),
    )

  [
    html.select(
      [],
      files
        |> list.map(fn(f) { html.option([], f) }),
    ),
    html.div([attr.class("calendar")], [
      html.div([attr.class("month-selector")], [
        html.a([attr.class("prev")], [html.text("<")]),
        html.div([], [
          html.text(int.to_string(year) <> " " <> utils.month_name(month)),
        ]),
        html.a([attr.class("next")], [html.text(">")]),
      ]),
      html.ul(
        [attr.class("days")],
        ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
          |> list.map(fn(d) { html.li([attr.class("weekday")], [html.text(d)]) })
          |> list.append(
            int.range(0, nb_days + 1, [], fn(acc, d) {
              let date = first_day |> utils.add_days(d)
              let nb_downloads =
                counts
                |> list.key_find(utils.date_to_str(date))
                |> result.unwrap(0)
              let day_element =
                html.li(
                  [
                    case date.month |> calendar.month_to_int {
                      m if m == month -> attr.class("current-month")
                      _ -> attr.none()
                    },
                    case date.day {
                      d if d == day -> attr.class("today")
                      _ -> attr.none()
                    },
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
  case utils.weekday(date) {
    utils.Monday -> date
    _ -> first_grid_day(utils.previous_day(date))
  }
}

fn last_grid_day(date: calendar.Date) -> calendar.Date {
  case utils.weekday(date) {
    utils.Sunday -> date
    _ -> last_grid_day(utils.next_day(date))
  }
}
