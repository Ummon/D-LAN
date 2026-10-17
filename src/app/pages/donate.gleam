import app/web
import gleam/list
import lustre/attribute as attr
import lustre/element
import lustre/element/html
import translations as tr

const bitcoin_address = "1Hw2RGLAfhnbXhYPPPR4auSAv9pxVvzwCP"

const ethereum_address = "0xC151a5F50c7f35a48bE8CFe0b520266623D0899A"

pub fn page(ctx: web.Context) -> element.Element(a) {
  html.div(
    [attr.id("content"), attr.class("donate")],
    list.flatten([
      [
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
      ],
      coin_box(
        name: "Bitcoin",
        site_url: "http://www.bitcoin.org",
        logo: "static/img/bitcoin_logo.webp",
        explorer_url: "http://blockchain.info/address/" <> bitcoin_address,
        address: bitcoin_address,
        address_label: tr.donate_bitcoin_address(ctx.lang),
        qr_code: "static/img/d_lan_bitcoin_qr_code.png",
      ),
      coin_box(
        name: "Ethereum",
        site_url: "http://www.ethereum.org",
        logo: "static/img/ethereum_logo.webp",
        explorer_url: "https://www.blockchain.com/explorer/addresses/eth/"
          <> ethereum_address,
        address: ethereum_address,
        address_label: tr.donate_ethereum_address(ctx.lang),
        qr_code: "static/img/d_lan_ethereum_qr_code.webp",
      ),
    ]),
  )
}

/// A title followed by a box showing a cryptocurrency address, with a link to
/// its explorer and its QR code.
fn coin_box(
  name name: String,
  site_url site_url: String,
  logo logo: String,
  explorer_url explorer_url: String,
  address address: String,
  address_label address_label: element.Element(a),
  qr_code qr_code: String,
) -> List(element.Element(a)) {
  [
    html.h2([], [html.text(name)]),
    html.div([attr.class("box")], [
      html.a([attr.href(site_url)], [
        html.img([attr.src(logo), attr.alt(name), attr.class("logo")]),
      ]),
      html.a([attr.href(explorer_url)], [address_label]),
      html.input([
        attr.type_("text"),
        attr.spellcheck(False),
        attr.size("42"),
        attr.readonly(True),
        attr.value(address),
      ]),
      html.a([attr.href(explorer_url)], [
        html.img([
          attr.src(qr_code),
          attr.alt(name <> " QR code"),
          attr.class("qr-code"),
        ]),
      ]),
    ]),
  ]
}
