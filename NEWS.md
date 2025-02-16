# legendry (development version)

* Fixed bug hindering `position = "inside"` placement (#42)
* Fixed bug in `theme_guide(key.size, key.height, key.width)` (#41)
* Complete guides based on a crux composition now render the `legend.background` 
  element (#50)
* A better attempt to honour ggplot2's mechanism for `<AsIs>` variables (#45)
* Better alignment of `compose_stack(side.titles)` (#48)

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
