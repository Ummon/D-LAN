import d_lan_website
import radiate

pub fn main() {
  // Watch 'src' and hot-reload modules when files change (dev only).
  let _ =
    radiate.new()
    |> radiate.add_dir("src")
    |> radiate.start()

  d_lan_website.start(dev_mode: True)
}
