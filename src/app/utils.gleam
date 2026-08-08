import gleam/float
import gleam/int
import gleam/string

// Formats a size in bytes as MiB with two decimals, e.g. "24.53".
pub fn file_size_mib(bytes: Int) -> String {
  let hundredths = float.round(int.to_float(bytes) *. 100.0 /. 1_048_576.0)
  int.to_string(hundredths / 100)
  <> "."
  <> string.pad_start(int.to_string(hundredths % 100), 2, "0")
}
