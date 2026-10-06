import lustre/attribute as attr
import lustre/element
import lustre/element/html
import translations as tr

pub fn image(
  lang: tr.Lang,
  filename: String,
  caption: String,
  comment: String,
) -> element.Element(a) {
  let lang_folder = tr.to_str(lang)
  html.div([attr.class("box"), attr.class("gallery")], [
    html.a(
      [
        attr.href(
          "static/img/gallery/" <> lang_folder <> "/" <> filename <> ".webp",
        ),
        attr.rel("group"),
        attr.title(case comment == "" {
          True -> caption
          False -> comment
        }),
      ],
      [
        html.img([
          attr.src(
            "static/img/gallery/"
            <> lang_folder
            <> "/"
            <> filename
            <> "_thumb.webp",
          ),
          attr.alt(caption),
        ]),
      ],
    ),
    html.p([], [html.text(caption)]),
  ])
}
