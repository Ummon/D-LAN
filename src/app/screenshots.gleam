import lustre/attribute as attr
import lustre/element
import lustre/element/html

pub fn image(
  filename: String,
  caption: String,
  comment: String,
) -> element.Element(a) {
  html.div([attr.class("box"), attr.class("gallery")], [
    html.a(
      [
        attr.href("static/img/gallery/" <> filename <> ".png"),
        attr.rel("group"),
        attr.title(case comment == "" {
          True -> caption
          False -> comment
        }),
      ],
      [
        html.img([
          attr.src("static/img/gallery/" <> filename <> "_thumb.png"),
          attr.alt(caption),
        ]),
      ],
    ),
    html.p([], [html.text(caption)]),
  ])
}
