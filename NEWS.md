# News

## 0.2.1

- Added `h(level, text)`: emits a markdown section heading inside
  `results="asis"` chunks.
- Added `write_tsv(x, path)`: writes a data frame to a TSV file, creating
  parent directories as needed.
- Added `safe_asset_suffix(x)`: sanitizes a string for use as an asset
  filename suffix (keeps `[A-Za-z0-9_.-]`, replaces runs of other characters
  with `_`).

## 0.2.0

- Initial public release with HTML report rendering, panel/section system,
  asset bundling, caching hooks, and CLI helpers.
