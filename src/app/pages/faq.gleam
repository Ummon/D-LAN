import app/web
import lustre/attribute as attr
import lustre/element
import lustre/element/html
import translations as tr

pub fn page(ctx: web.Context) -> element.Element(a) {
  html.div([attr.id("content"), attr.class("faq")], [
    html.div([attr.class("qa")], [
      html.div([attr.class("question")], [html.em([], [tr.faq_q1(ctx.lang)])]),
      html.div([attr.class("answer")], [tr.faq_a1(ctx.lang)]),
    ]),
    html.div([attr.class("qa")], [
      html.div([attr.class("question")], [html.em([], [tr.faq_q2(ctx.lang)])]),
      html.div([attr.class("answer")], [tr.faq_a2(ctx.lang)]),
    ]),
    html.div([attr.class("qa")], [
      html.div([attr.class("question")], [html.em([], [tr.faq_q3(ctx.lang)])]),
      html.div([attr.class("answer")], [tr.faq_a3(ctx.lang)]),
    ]),
    html.div([attr.class("qa")], [
      html.div([attr.class("question")], [html.em([], [tr.faq_q4(ctx.lang)])]),
      html.div([attr.class("answer")], [tr.faq_a4(ctx.lang)]),
    ]),
    html.div([attr.class("qa")], [
      html.div([attr.class("question")], [html.em([], [tr.faq_q5(ctx.lang)])]),
      html.div([attr.class("answer")], [tr.faq_a5(ctx.lang)]),
    ]),
    html.div([attr.class("qa")], [
      html.div([attr.class("question")], [html.em([], [tr.faq_q6(ctx.lang)])]),
      html.div([attr.class("answer")], [tr.faq_a6(ctx.lang)]),
    ]),
  ])
}
