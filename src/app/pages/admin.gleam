import app/web
import lustre/attribute as attr
import lustre/element
import lustre/element/html

pub fn page(ctx: web.Context) -> element.Element(a) {
  html.div([attr.id("content"), attr.class("admin")], case ctx.is_admin {
    True -> [html.text("ADMIN")]
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
