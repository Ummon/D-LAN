# Translation review — 2026-09-07

Reviewed all 14 catalogs: German, Spanish, French, Italian, Japanese, Korean,
and Russian, for both Core and GUI. Each language contains 11 active Core
messages and 230 active GUI messages: 1,687 active translations in total.
The six historical `vanished` entries were also checked and left unchanged;
they are excluded from active coverage and compiled catalogs.

## Changes

Corrected 118 translations or empty labels, and cleared `unfinished` on 241
reviewed translations (235 Japanese entries and six other GUI entries).
Counts describe this review, excluding pre-existing working-tree changes.

| Language | Text corrections / filled labels | Unfinished entries resolved |
| --- | ---: | ---: |
| German | 11 | 1 |
| Spanish | 42 | 1 |
| French | 41 | 1 |
| Italian | 2 | 1 |
| Japanese | 2 | 235 |
| Korean | 2 | 1 |
| Russian | 18 | 1 |

Examples include Spanish Pause incorrectly saying Resume, Russian Move to top
incorrectly meaning one position up, German Stop meaning hide, French spelling
and agreement errors, and missing Spanish tooltip newlines and spaces.
Also filled ten empty Form labels, clarified download/upload rates, and
improved inconsistent terminology. Product names and established technical
terms are intentionally retained where appropriate.

## Validation

- All active entries are nonempty and marked finished.
- Fresh Qt `lupdate` extraction into temporary catalogs found the same 11 Core
  and 230 GUI messages; no active keys were missing or stale in any language.
- Placeholder identities and counts, newline counts, trailing spaces, and
  HTML tag preservation checks passed. All files parse as UTF-8 XML.
- All 14 catalogs compiled successfully using Qt `lrelease`.
- The normal `dlan_translations` build target also completed successfully.

`lupdate` emitted parser warnings for an existing literal NUL character in
`Common/Global.cpp:482` and declarations in the external protobuf headers.
`Global.cpp` has no translation calls. These warnings were not changed as part
of the catalog review.

## Remaining limitation

`GUI/StatusBar.cpp:119` chooses between `peer` and `peers` using `nbPeer > 1`.
Russian requires additional plural forms, so counts such as 5 and 21 cannot
all be correct using those two catalog entries. Resolving this requires a
separate source change to use Qt numerus translations, with English fallback
handled as well. The Russian search-result counts already avoid this issue
by using labels such as `файлов: %1`.

This was a source and language review, not native-speaker certification or
an interactive review of every screen. Layout fit and live language refresh
were not tested. Compiled audit catalogs are in the build directory; packaged
or installed copies were not replaced.
