import app/web
import lustre/attribute as attr
import lustre/element
import lustre/element/html
import translations as tr

pub fn page(ctx: web.Context) -> element.Element(a) {
  html.div([attr.id("content"), attr.class("about")], [
    html.a([attr.href("static/D-LAN logo.svg"), attr.class("logo")], [
      html.img([attr.src("static/img/logo_icon.svg")]),
    ]),
    html.p([], [
      tr.about_author(
        ctx.lang,
        html.a([attr.href("http://www.gburri.org")], [html.text("Greg Burri")]),
      ),
    ]),
    html.p([], [
      tr.about_linux(
        ctx.lang,
        html.a([attr.href("mailto:herve.martinet@gmail.com")], [
          html.text("Hervé Martinet"),
        ]),
      ),
    ]),
    html.p([], [
      tr.about_thanks(
        ctx.lang,
        html.text("Maximilien Cuony (\"The Glu\")"),
        html.a([attr.href("http://twitter.com/algorithme")], [
          html.text("Olivier Morel"),
        ]),
      ),
    ]),

    html.h2([], [tr.about_tech(ctx.lang)]),

    html.h3([], [tr.about_tech_used_d_lan_title(ctx.lang)]),
    tr.about_tech_used_d_lan(ctx.lang),

    html.h3([], [tr.about_tech_used_tools_title(ctx.lang)]),
    tr.about_tech_used_tools(ctx.lang),

    html.h3([], [tr.about_tech_used_website_title(ctx.lang)]),
    tr.about_tech_used_website(ctx.lang),
  ])
}
