# legendry (development version)

* New arguments `guide_axis_base(subtitle)` and `guide_axis_nested(subtitle)` 
  make it easier to title custom axes (#102)
* `compose_stack(side.titles)` also work in non-position guides (#101)
* Updated title mechanism to comply with ggplot2#6200 (#104)
* `key_group_split()` tries to preserve order of labels better, rather than 
  re-sorting them (#100)

# legendry 0.2.4

This is a patch release fixing a few bugs.

* Fixed an issue detected by CRAN with regards to undeclared imports from S7 (#97)
* `scale_x/y_dendro()` will no longer fail to lookup `guide_axis_dendro()` when
  legendry is not on the search path (#94).

# legendry 0.2.3

This is a patch release fixing a few bugs.

* `guide_colbar()` properly uses `legend.key.width/height` setting (#81).
* `compose_sandwich()` and its wrappers now have a `suppress_labels` argument 
  that controls label rendering (#91).

# legendry 0.2.2

This is a patch release without new features, improvements or bug fixes. 
The patch aims to make legendry forwards-compatible with a planned ggplot2 
release.

# legendry 0.2.1

This is a patch release with a few bug fixes and a tiny polish.

* Fixed bug in `guide_circles()` used for multiple layers (#58)
* Fixed bug hindering `position = "inside"` placement (#42)
* Fixed bug in `theme_guide(key.size, key.height, key.width)` (#41)
* Complete guides based on a crux composition now render the `legend.background` 
  element (#50)
* A better attempt to honour ggplot2's mechanism for `<AsIs>` variables (#45)
* Better alignment of `compose_stack(side.titles)` (#48)
* Fixed aesthetic standardisation in `override.aes` arguments (#60)
* Improvements to density and histogram gizmos (#62):
    * The default key now depends on the scale type: continuous scales invoke 
    `key_sequence()` and binned scales invoke `key_bins()`.
    * When using a binned key in `gizmo_histogram()`, the default `hist(breaks)`
    argument is populated with the key's breaks.
* Fix capping issue with non-canonical rescalers in `primitive_line()` (#67)

# legendry 0.2.0

This is a small feature release introducing dendrogram scales and a size guide.

* Added support for dendrograms (#33):
    * New scale functions `scale_x_dendro()` and `scale_y_dendro()`.
    * New full guide function: `guide_axis_dendro()`.
    * New primitive guide function: `primitive_segments()`
    * New key functions: `key_segment_manual()`, `key_segment_map()` and 
      `key_dendro()`.
      
* Added new standalone guide `guide_circles()` (#14).
    * New supporting theme element `legendry.legend.key.margin`.

* Fixed bug where `guide_axis_nested(key = key_range_auto(...))` produced 
  duplicated labels (#31).

# legendry 0.1.0

First release.

Thanks to the following people for catching and reporting early bugs and 
mistakes:

* @davidhodge931 (#7, #8, #9, #12)
* @luisDVA (#18)
* @mthomas-ketchbrook (#21)
