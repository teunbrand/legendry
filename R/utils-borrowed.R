# gt ----------------------------------------------------------------------

# The functions in this chunk are either literally copied, or copied with
# modification from gt. The copyright of these functions belong to the
# gt authors. These functions are borrowed under the MIT licence that
# applies to the gt package and can be found at the link below:
# https://gt.rstudio.com/LICENSE.html

rtl_charsets <- paste(
  hebrew    = "[\U00590-\U005FF]",
  arabic    = "[\U00600-\U006FF]",
  syriac    = "[\U00700-\U0074F]",
  thaana    = "[\U00780-\U007BF]",
  samaritan = "[\U00800-\U0083F]",
  mandaic   = "[\U00840-\U0085F]",
  sep = "|"
)

# end gt ------------------------------------------------------------------
