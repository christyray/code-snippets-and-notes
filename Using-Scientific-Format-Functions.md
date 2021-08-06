Using Scientific Format Functions
================
Christy Pickering
2021-08-06

Here I discuss three functions I wrote to convert numbers to
customizable scientific format for ggplot axis labels, annotations,
legend labels, etc. I also provide examples of how to use each function.

Source code containing the described functions is located
[here](Code/scientific_conversion.R).

# `to_scientific()` Function

## `to_scientific()` Inputs:

  - `x` = the number or vector of numbers to be converted into
    scientific format
  - `digits` = the number of significant figures to display in the
    converted output
  - `max_cut` = above this number, values will be converted into
    scientific format (inclusive)
  - `min_cut` = below this number, values will be converted into
    scientific format (inclusive)
  - `units` = if specified, adds units to the converted output

## `to_scientific()` Output:

  - Outputs numbers between `min_cut` and `max_cut` as expressions of
    plain numbers, with `units` if given
  - Outputs numbers less than `min_cut` or greater than `max_cut` as
    expressions with numbers in scientific format, with `units` if given

Expressions are used because R or `ggplot` can parse the expressions to
display formatted numbers. `NA` values are preserved because `ggplot`
often uses `NA` when the axis limits are not specified.

## `to_scientific()` Function Code:

``` r
to_scientific <- function(x,
                          digits = 2,
                          max_cut = 10^5,
                          min_cut = 10^-3,
                          units = NULL) {
  
  # Convert input to consistent number format
  x <- as.numeric(as.character(x))

  # Fix NA values
  if (any(is.na(x))) {

    # To fix NAs in unspecified positions, find location and remove
    xNA <- x            # Store original input for recovery later
    x <- x[!is.na(x)]   # Remove NA values
  }

  # Select numbers in between min_cut and max_cut and round to specified
  # number of digits
  x[abs(x) < max_cut & abs(x) > min_cut] <- signif(
    x[abs(x) < max_cut & abs(x) > min_cut],
    digits = digits
  )

  # Select numbers outside of range and convert numbers into characters
  # in consistent scientific format
  x[abs(x) >= max_cut | abs(x) <= min_cut] <- formatC(
    x[abs(x) >= max_cut | abs(x) <= min_cut],
    format = "e", digits = digits - 1
  )

  # Replace scientific format zeros with plain zeros
  pattern <- paste(rep(0, digits - 1), sep = "", collapse = "")
  pattern <- paste0("0.", pattern, "e+00")
  x[x == pattern] <- "0"

  # Convert scientific notation into a math expression
  x <- gsub(pattern = "e\\+0?", replacement = " %*% 10^", x)
  x <- gsub(pattern = "e\\-0?", replacement = " %*% 10^-", x)

  # Add units if given
  if (is.character(units)) {
    x <- paste0(x, "~", units)
  }

  if (exists('xNA')) {
    xNA[!is.na(xNA)] <- x
    x <- xNA
  }

  # Convert text into expression format
  x <- parse(text = x)
}
```
