import app/web
import lustre/attribute as attr
import lustre/element
import lustre/element/html
import translations as tr

pub fn page(ctx: web.Context) -> element.Element(a) {
  html.div([attr.id("content"), attr.class("donate")], [
    html.h2([], [tr.donate_title(ctx.lang)]),
    html.p([], [tr.donate_intro(ctx.lang)]),
    html.div([attr.class("box")], [
      html.a([attr.href("http://www.bitcoin.org")], [
        html.img([
          attr.src("static/img/bitcoin_icon.png"),
          attr.alt("Bitcoin"),
          attr.class("bitcoin"),
        ]),
      ]),
      html.a(
        [attr.href("http://blockchain.info/address/" <> bitcoin_address())],
        [tr.donate_bitcoin_address(ctx.lang)],
      ),
      html.input([
        attr.class("bitcoin-address-field"),
        attr.type_("text"),
        attr.spellcheck(False),
        attr.size("45"),
        attr.readonly(True),
        attr.value(bitcoin_address()),
      ]),
      html.img([
        attr.src("static/img/d_lan_bitcoin_qr_code.png"),
        attr.class("bitcoin-qr-code"),
      ]),
    ]),
  ])
}

fn bitcoin_address() {
  "1Hw2RGLAfhnbXhYPPPR4auSAv9pxVvzwCP"
}
