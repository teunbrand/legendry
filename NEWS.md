# legendry 0.0.0.9001

Renamed {gguidance} to {legendry}.

Restarted package from scratch, using a different approach.

Thanks to the following people for catching and reporting early bugs:

* @davidhodge931 (#7, #8, #9, #12)

## Mechanism

The crux in legendry is that you combine a 'key', that contains instructions
on which values are represented, with a 'guide' that controls how these values
are represented.

## Full guides

Full guides are guides that you can just drop in the `guides()` function or as
`guide` argument to scales.

* `guide_axis_base()` as an axis guide.
* `guide_colbar()` as a continuous colour/fill guide.
* `guide_colsteps()` as a binned colour/fill guide.
* `guide_colring()` as a continuous colour/fill guide.
* `guide_subtitle()` as a colour/fill guide.

## Gizmos

Gizmos are standalone specialised displays.

* `gizmo_barcap()` for displaying colour gradients with capping options.
* `gizmo_stepcap()` for displaying stepped gradients with capping options.
* `gizmo_density()` for displaying gradient-filled densities.
* `gizmo_histogram()` for displaying gradient-filled histograms.
* `gizmo_grob()` for displaying custom grobs.

## Compositions

Compositions offer ways to combine guides.

* `compose_stack()` for stacking guides.
* `compose_ontop()` for overlaying guides.
* `compose_crux()` for layout out guides in a cross.
* `compose_sandwich()` for flanked guides.

## Primitives

Primitives are the most basic elements that guides use.

* `primitive_line()` for lines.
* `primitive_ticks()` for tick marks.
* `primitive_labels()` for text labels.
* `primitive_spacer()` for empty space.
* `primitive_title()` for text titles.
* `primitive_bracket()` for labelled brackets.
* `primitive_box()` for rectangles.
