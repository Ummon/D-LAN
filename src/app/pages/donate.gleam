import app/web
import lustre/attribute as attr
import lustre/element
import lustre/element/html
import translations as tr

pub fn page(ctx: web.Context) -> element.Element(a) {
  let href_attr =
    attr.href("http://blockchain.info/address/" <> bitcoin_address())
  html.div([attr.id("content"), attr.class("donate")], [
    html.h1([], [tr.donate_title(ctx.lang)]),
    html.p([], [tr.donate_intro(ctx.lang)]),
    html.h2([], [tr.donate_buy_me_a_coffee(ctx.lang)]),
    html.div([attr.class("box"), attr.id("buy-me-a-coffee")], [
      html.a([attr.href("https://www.buymeacoffee.com/d_lan")], [
        html.img([
          attr.src(
            "https://img.buymeacoffee.com/button-api/?text=Buy me a coffee&emoji=&slug=d_lan&button_colour=FFDD00&font_colour=000000&font_family=Cookie&outline_colour=000000&coffee_colour=ffffff",
          ),
        ]),
      ]),
    ]),
    html.h2([], [html.text("Bitcoin")]),
    html.div([attr.class("box")], [
      html.a([attr.href("http://www.bitcoin.org")], [
        html.img([
          attr.src("static/img/bitcoin_icon.png"),
          attr.alt("Bitcoin"),
          attr.class("bitcoin"),
        ]),
      ]),
      html.a([href_attr], [tr.donate_bitcoin_address(ctx.lang)]),
      html.input([
        attr.class("bitcoin-address-field"),
        attr.type_("text"),
        attr.spellcheck(False),
        attr.size("42"),
        attr.readonly(True),
        attr.value(bitcoin_address()),
      ]),
      html.a([href_attr], [
        html.img([
          attr.src("static/img/d_lan_bitcoin_qr_code.png"),
          attr.class("bitcoin-qr-code"),
        ]),
      ]),
    ]),
  ])
}

// https://buymeacoffee.com/d_lan

fn bitcoin_address() {
  "1Hw2RGLAfhnbXhYPPPR4auSAv9pxVvzwCP"
}
