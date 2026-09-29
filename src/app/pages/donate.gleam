import app/web
import lustre/attribute as attr
import lustre/element
import lustre/element/html
import translations as tr

pub fn page(ctx: web.Context) -> element.Element(a) {
  let href_attr_bitcoin =
    attr.href("http://blockchain.info/address/" <> bitcoin_address())
  let href_attr_ethereum =
    attr.href(
      "https://www.blockchain.com/explorer/addresses/eth/" <> ethereum_address(),
    )
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
          attr.src("static/img/bitcoin_logo.webp"),
          attr.alt("Bitcoin"),
          attr.class("logo"),
        ]),
      ]),
      html.a([href_attr_bitcoin], [tr.donate_bitcoin_address(ctx.lang)]),
      html.input([
        attr.class("bitcoin-address-field"),
        attr.type_("text"),
        attr.spellcheck(False),
        attr.size("42"),
        attr.readonly(True),
        attr.value(bitcoin_address()),
      ]),
      html.a([href_attr_bitcoin], [
        html.img([
          attr.src("static/img/d_lan_bitcoin_qr_code.png"),
          attr.class("qr-code"),
        ]),
      ]),
    ]),
    html.h2([], [html.text("Ethereum")]),
    html.div([attr.class("box")], [
      html.a([attr.href("http://www.ethereum.org")], [
        html.img([
          attr.src("static/img/ethereum_logo.webp"),
          attr.alt("Ethereum"),
          attr.class("logo"),
        ]),
      ]),
      html.a([href_attr_ethereum], [tr.donate_ethereum_address(ctx.lang)]),
      html.input([
        attr.class("ethereum-address-field"),
        attr.type_("text"),
        attr.spellcheck(False),
        attr.size("42"),
        attr.readonly(True),
        attr.value(ethereum_address()),
      ]),
      html.a([href_attr_ethereum], [
        html.img([
          attr.src("static/img/d_lan_ethereum_qr_code.webp"),
          attr.class("qr-code"),
        ]),
      ]),
    ]),
  ])
}

fn bitcoin_address() {
  "1Hw2RGLAfhnbXhYPPPR4auSAv9pxVvzwCP"
}

fn ethereum_address() {
  "0xC151a5F50c7f35a48bE8CFe0b520266623D0899A"
}
