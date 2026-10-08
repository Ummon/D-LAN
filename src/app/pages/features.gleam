import app/screenshots
import app/web
import lustre/attribute as attr
import lustre/element
import lustre/element/html
import translations as tr

pub fn page(ctx: web.Context) -> element.Element(a) {
  let screenshot = fn(filename, caption, comment) {
    screenshots.image(ctx.lang, filename, caption, comment)
  }
  html.div([attr.id("content"), attr.class("features")], [
    html.p([], [
      tr.features_disclaimer(ctx.lang),
    ]),
    html.ul([], [
      html.li([], [tr.features_feat_1(ctx.lang)]),
      html.li([], [tr.features_feat_2(ctx.lang)]),
      html.li([], [tr.features_feat_3(ctx.lang)]),
      html.li([], [tr.features_feat_4(ctx.lang)]),
      html.li([], [tr.features_feat_5(ctx.lang)]),
      html.li([], [tr.features_feat_6(ctx.lang)]),
      html.li([], [tr.features_feat_7(ctx.lang)]),
      html.li([], [tr.features_feat_8(ctx.lang)]),
      html.li([], [tr.features_feat_9(ctx.lang)]),
      html.li([], [tr.features_feat_10(ctx.lang)]),
    ]),
    html.p([], [tr.features_help_us(ctx.lang, "donate.html")]),
    screenshot(
      "browse",
      tr.gallery_browse(ctx.lang),
      tr.gallery_browse_comment(ctx.lang),
    ),
    screenshot(
      "search",
      tr.gallery_search(ctx.lang),
      tr.gallery_search_comment(ctx.lang),
    ),
    screenshot(
      "download_folders",
      tr.gallery_download_folders(ctx.lang),
      tr.gallery_download_folders_comment(ctx.lang),
    ),
    screenshot(
      "download_files",
      tr.gallery_download_files(ctx.lang),
      tr.gallery_download_files_comment(ctx.lang),
    ),
    screenshot("upload", tr.gallery_upload(ctx.lang), ""),
    screenshot("skin", tr.gallery_skin(ctx.lang), ""),
  ])
}
