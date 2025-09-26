# guide_axis_plot() rejects incompatible plots

    Code
      guide_axis_plot(plot = "foobar")
    Condition
      Error in `guide_axis_plot()`:
      ! `plot` must be a <ggplot> object, not the string "foobar".

---

    Code
      guide_axis_plot(plot + coord_radial())
    Condition
      Error in `guide_axis_plot()`:
      ! The `plot` argument cannot have a non-linear coordinate system.

---

    Code
      guide_axis_plot(plot + facet_wrap(~cyl))
    Condition
      Error in `guide_axis_plot()`:
      ! The `plot` argument cannot have facets of class <FacetWrap>.
      i Only `facet_null()` is supported.

---

    Code
      ggplot_build(wrong_plot)
    Condition
      Error in `transform()`:
      ! Non-linear coordinates do not support `guide_axis_plot()`.

