# `ggproto` objects in legendry

The legendry package relies on an extension system of ggplot2 through
[`ggproto`](https://ggplot2.tidyverse.org/reference/ggproto.html) class
objects, which allow cross-package inheritance of objects such as geoms,
stats, facets, scales and coordinate systems. For the purpose of making
plots, users are invited to wholly ignore these objects, since
interacting with these objects is preferred through various constructor
functions. The legendry package introduces a new `<Guide>` ggproto class
to support variations on axes, legends and colourbars.

## See also

The documentation over at
[`ggproto`](https://ggplot2.tidyverse.org/reference/ggproto.html).
