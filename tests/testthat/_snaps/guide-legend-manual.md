# guide_legend_manual() constructor works as expected

    Code
      guide_legend_manual("foo", layers = list("invalid input"))
    Condition
      Error in `guide_legend_manual()`:
      ! `layers` must be a <list> object with <LayerInstance> elements.
      x layers[[1]] is the string "invalid input"

---

    Code
      guide_legend_manual(c("foo", "bar"), colour = c("red", "green", "blue"))
    Condition
      Error in `guide_legend_manual()`:
      ! Can't recycle `labels` (size 2) to match `colour` (size 3).

