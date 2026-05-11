# Guide composition

**\[experimental\]**

Guide composition is a meta-guide orchestrating an ensemble of other
guides. On their own, a 'composing' guide is not very useful as a visual
reflection of a scale.

## Usage

``` r
new_compose(
  guides,
  args = list(),
  ...,
  available_aes = c("any", "x", "y", "r", "theta"),
  call = caller_env(),
  super = Compose
)
```

## Arguments

- guides:

  A `<list>` of guides wherein each element is one of the following:

  - A `<Guide>` class object.

  - A `<function>` that returns a `<Guide>` class object.

  - A `<character[1]>` naming such a function, without the `guide_` or
    `primitive_` prefix.

- args:

  A `<list>` of arguments to pass to guides that are given either as a
  function or as a string.

- ...:

  Additional parameters to pass on to
  [`new_guide()`](https://ggplot2.tidyverse.org/reference/new_guide.html).

- available_aes:

  A `<character>` giving aesthetics that must match the the guides.

- call:

  A [call](https://rlang.r-lib.org/reference/topic-error-call.html) to
  display in messages.

- super:

  A `<Compose>` class object giving a meta-guide for composition.

## Value

A `<Compose>` (sub-)class guide that composes other guides.

## Details

### Styling options

Below are the
[theme](https://ggplot2.tidyverse.org/reference/theme.html) options that
determine the styling of this guide.

|  |  |  |
|----|----|----|
| **Theme setting** | **Type** | **Description** |
| `legendry.guide.spacing` | [`unit()`](https://rdrr.io/r/grid/unit.html) | Spacing between guides. |

There are no further styling options.

## See also

Other composition:
[`compose_crux()`](https://teunbrand.github.io/legendry/reference/compose_crux.md),
[`compose_ontop()`](https://teunbrand.github.io/legendry/reference/compose_ontop.md),
[`compose_sandwich()`](https://teunbrand.github.io/legendry/reference/compose_sandwich.md),
[`compose_stack()`](https://teunbrand.github.io/legendry/reference/compose_stack.md)

## Examples

``` r
# The `new_compose()` function is not intended to be used directly
my_composition <- new_compose(list("axis", "axis"), super = ComposeStack)

# Is the same as
my_composition <- compose_stack("axis", "axis")
```
