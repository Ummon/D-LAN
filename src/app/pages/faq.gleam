import app/download_button
import app/web
import gleam/result
import lustre/attribute as attr
import lustre/element
import lustre/element/html
import translations as tr

pub fn page(ctx: web.Context) -> element.Element(a) {
  let release_linux_folder = ctx.app.releases_directory <> "/Linux"
  html.div([attr.id("content"), attr.class("faq")], [
    html.div([attr.class("qa")], [
      html.div([attr.class("question")], [html.h2([], [tr.faq_q1(ctx.lang)])]),
      html.div([attr.class("answer")], [tr.faq_a1(ctx.lang)]),
    ]),
    html.div([attr.class("qa")], [
      html.div([attr.class("question")], [html.h2([], [tr.faq_q2(ctx.lang)])]),
      html.div([attr.class("answer")], [tr.faq_a2(ctx.lang)]),
    ]),
    html.div([attr.class("qa")], [
      html.div([attr.class("question")], [html.h2([], [tr.faq_q3(ctx.lang)])]),
      html.div([attr.class("answer")], [tr.faq_a3(ctx.lang)]),
    ]),
    html.div([attr.class("qa")], [
      html.div([attr.class("question")], [html.h2([], [tr.faq_q4(ctx.lang)])]),
      html.div([attr.class("answer")], [tr.faq_a4(ctx.lang)]),
    ]),
    html.div([attr.class("qa")], [
      html.div([attr.class("question")], [html.h2([], [tr.faq_q5(ctx.lang)])]),
      html.div([attr.class("answer")], [
        tr.faq_a5(
          ctx.lang,
          download_button.latest_release_filename(release_linux_folder)
            |> result.unwrap(""),
        ),
      ]),
    ]),
  ])
}
