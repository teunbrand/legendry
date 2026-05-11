# Changelog

## legendry (development version)

- New guide
  [`guide_legend_manual()`](https://teunbrand.github.io/legendry/reference/guide_legend_manual.md)
  for user defined keys
  ([\#88](https://github.com/teunbrand/legendry/issues/88))
- New guide
  [`guide_axis_plot()`](https://teunbrand.github.io/legendry/reference/guide_axis_plot.md)
  to place side-plots
  ([\#43](https://github.com/teunbrand/legendry/issues/43))
- New guide
  [`guide_axis_upset()`](https://teunbrand.github.io/legendry/reference/guide_axis_symbols.md)
  to place symbols in upset matrices next to axes. Also comes with
  [`key_upset()`](https://teunbrand.github.io/legendry/reference/key_specialty.md)
  to match ([\#38](https://github.com/teunbrand/legendry/issues/38)).
- New guide
  [`guide_axis_symbols()`](https://teunbrand.github.io/legendry/reference/guide_axis_symbols.md)
  for a more manual version of placing symbols. Also comes with
  [`key_symbols()`](https://teunbrand.github.io/legendry/reference/key_specialty.md)
  to match ([\#38](https://github.com/teunbrand/legendry/issues/38)).
- New key
  [`key_range_rle()`](https://teunbrand.github.io/legendry/reference/key_range.md)
  to create a range annotation based on run-length encoding.
- New arguments `guide_axis_base(subtitle)` and
  `guide_axis_nested(subtitle)` make it easier to title custom axes
  ([\#102](https://github.com/teunbrand/legendry/issues/102))
- New arguments
  `guide_legend_cross(row_title, col_title, subtitle_position)` to
  display subtitles over dimensions
  ([\#79](https://github.com/teunbrand/legendry/issues/79)).
- `compose_stack(side.titles)` also work in non-position guides
  ([\#101](https://github.com/teunbrand/legendry/issues/101))
- Changed the margins and inferred justification of the
  `legendry.axis.subtitle` and `legendry.legend.subtitle` theme
  elements.
- Changed `...` argument in many `key_*()` functions to transfer
  graphical properties like `colour` or `linewidth` to guides. Different
  parts of guides can be targeted by specifying
  `text_colour`/`line_colour`/`rect_colour`/`point_colour`.
- Changed
  [`key_group_split()`](https://teunbrand.github.io/legendry/reference/key_group.md)
  to preserve order of labels better, rather than re-sorting them
  ([\#100](https://github.com/teunbrand/legendry/issues/100)).
- Updated title mechanism to comply with ggplot2#6200
  ([\#104](https://github.com/teunbrand/legendry/issues/104))
- Updated lifecycle of
  [`new_compose()`](https://teunbrand.github.io/legendry/reference/guide-composition.md),
  [`compose_ontop()`](https://teunbrand.github.io/legendry/reference/compose_ontop.md),
  [`compose_stack()`](https://teunbrand.github.io/legendry/reference/compose_stack.md)
  from ‘experimental’ to ‘stable’.
- Fixed a bug where empty titles in legends contributed to spacing.
  Visual tests depending on legendry may see subtle positioning shifts
  ([\#113](https://github.com/teunbrand/legendry/issues/113)).
- Fixed a bug where
  [`guide_legend_group()`](https://teunbrand.github.io/legendry/reference/guide_legend_group.md)
  struggled to merge different scales
  ([\#112](https://github.com/teunbrand/legendry/issues/112)).

## legendry 0.2.4

CRAN release: 2025-09-14

This is a patch release fixing a few bugs.

- Fixed an issue detected by CRAN with regards to undeclared imports
  from S7 ([\#97](https://github.com/teunbrand/legendry/issues/97))
- `scale_x/y_dendro()` will no longer fail to lookup
  [`guide_axis_dendro()`](https://teunbrand.github.io/legendry/reference/guide_axis_dendro.md)
  when legendry is not on the search path
  ([\#94](https://github.com/teunbrand/legendry/issues/94)).

## legendry 0.2.3

CRAN release: 2025-08-18

This is a patch release fixing a few bugs.

- [`guide_colbar()`](https://teunbrand.github.io/legendry/reference/guide_colbar.md)
  properly uses `legend.key.width/height` setting
  ([\#81](https://github.com/teunbrand/legendry/issues/81)).
- [`compose_sandwich()`](https://teunbrand.github.io/legendry/reference/compose_sandwich.md)
  and its wrappers now have a `suppress_labels` argument that controls
  label rendering
  ([\#91](https://github.com/teunbrand/legendry/issues/91)).

## legendry 0.2.2

CRAN release: 2025-05-30

This is a patch release without new features, improvements or bug fixes.
The patch aims to make legendry forwards-compatible with a planned
ggplot2 release.

## legendry 0.2.1

CRAN release: 2025-03-04

This is a patch release with a few bug fixes and a tiny polish.

- Fixed bug in
  [`guide_circles()`](https://teunbrand.github.io/legendry/reference/guide_circles.md)
  used for multiple layers
  ([\#58](https://github.com/teunbrand/legendry/issues/58))
- Fixed bug hindering `position = "inside"` placement
  ([\#42](https://github.com/teunbrand/legendry/issues/42))
- Fixed bug in `theme_guide(key.size, key.height, key.width)`
  ([\#41](https://github.com/teunbrand/legendry/issues/41))
- Complete guides based on a crux composition now render the
  `legend.background` element
  ([\#50](https://github.com/teunbrand/legendry/issues/50))
- A better attempt to honour ggplot2’s mechanism for `<AsIs>` variables
  ([\#45](https://github.com/teunbrand/legendry/issues/45))
- Better alignment of `compose_stack(side.titles)`
  ([\#48](https://github.com/teunbrand/legendry/issues/48))
- Fixed aesthetic standardisation in `override.aes` arguments
  ([\#60](https://github.com/teunbrand/legendry/issues/60))
- Improvements to density and histogram gizmos
  ([\#62](https://github.com/teunbrand/legendry/issues/62)):
  - The default key now depends on the scale type: continuous scales
    invoke
    [`key_sequence()`](https://teunbrand.github.io/legendry/reference/key_specialty.md)
    and binned scales invoke
    [`key_bins()`](https://teunbrand.github.io/legendry/reference/key_specialty.md).
  - When using a binned key in
    [`gizmo_histogram()`](https://teunbrand.github.io/legendry/reference/gizmo_histogram.md),
    the default `hist(breaks)` argument is populated with the key’s
    breaks.
- Fix capping issue with non-canonical rescalers in
  [`primitive_line()`](https://teunbrand.github.io/legendry/reference/primitive_line.md)
  ([\#67](https://github.com/teunbrand/legendry/issues/67))

## legendry 0.2.0

CRAN release: 2024-12-14

This is a small feature release introducing dendrogram scales and a size
guide.

- Added support for dendrograms
  ([\#33](https://github.com/teunbrand/legendry/issues/33)):
  - New scale functions
    [`scale_x_dendro()`](https://teunbrand.github.io/legendry/reference/scale_x_dendro.md)
    and
    [`scale_y_dendro()`](https://teunbrand.github.io/legendry/reference/scale_x_dendro.md).
  - New full guide function:
    [`guide_axis_dendro()`](https://teunbrand.github.io/legendry/reference/guide_axis_dendro.md).
  - New primitive guide function:
    [`primitive_segments()`](https://teunbrand.github.io/legendry/reference/primitive_segments.md)
  - New key functions:
    [`key_segment_manual()`](https://teunbrand.github.io/legendry/reference/key_segments.md),
    [`key_segment_map()`](https://teunbrand.github.io/legendry/reference/key_segments.md)
    and
    [`key_dendro()`](https://teunbrand.github.io/legendry/reference/key_segments.md).
- Added new standalone guide
  [`guide_circles()`](https://teunbrand.github.io/legendry/reference/guide_circles.md)
  ([\#14](https://github.com/teunbrand/legendry/issues/14)).
  - New supporting theme element `legendry.legend.key.margin`.
- Fixed bug where `guide_axis_nested(key = key_range_auto(...))`
  produced duplicated labels
  ([\#31](https://github.com/teunbrand/legendry/issues/31)).

## legendry 0.1.0

CRAN release: 2024-11-01

First release.

Thanks to the following people for catching and reporting early bugs and
mistakes:

- @davidhodge931 ([\#7](https://github.com/teunbrand/legendry/issues/7),
  [\#8](https://github.com/teunbrand/legendry/issues/8),
  [\#9](https://github.com/teunbrand/legendry/issues/9),
  [\#12](https://github.com/teunbrand/legendry/issues/12))
- @luisDVA ([\#18](https://github.com/teunbrand/legendry/issues/18))
- @mthomas-ketchbrook
  ([\#21](https://github.com/teunbrand/legendry/issues/21))
