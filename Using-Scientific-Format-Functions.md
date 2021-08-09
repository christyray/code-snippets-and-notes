Using Scientific Format Functions
================
Christy Pickering
2021-08-09

  - [`to_scientific()` Function](#to_scientific-function)

Here I discuss three functions I wrote to convert numbers to
customizable scientific format for ggplot axis labels, annotations,
legend labels, etc. I also provide examples of how to use each function.

Source code containing the described functions is located
[here](Code/scientific_conversion.R).

# `to_scientific()` Function

## Inputs

  - `x` = the number or vector of numbers to be converted into
    scientific format
  - `digits` = the number of significant figures to display in the
    converted output
  - `max_cut` = above this number, values will be converted into
    scientific format (inclusive)
  - `min_cut` = below this number, values will be converted into
    scientific format (inclusive)
  - `units` = if specified, adds units to the converted output

## Output

  - Outputs numbers between `min_cut` and `max_cut` as expressions of
    plain numbers, with `units` if given
  - Outputs numbers less than `min_cut` or greater than `max_cut` as
    expressions with numbers in scientific format, with `units` if given

Outputs are formatted as expressions because R or `ggplot` can parse the
expressions to display formatted numbers. `NA` values are preserved
because `ggplot` often uses `NA` when the axis limits are not specified.

Note: Inputs are rounded before checking if they are in the range for
conversion to scientific format; e.g., \(99999\) will be rounded to
\(1 \times 10^5\), so if the cutoff is \(1 \times 10^5\), it will get
converted to scientific format.

## Function Code

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
  # number of digits (not scientific format)
  x[abs(x) < max_cut & abs(x) > min_cut] <- signif(
    x[abs(x) < max_cut & abs(x) > min_cut],
    digits = digits
  )

  # Select numbers outside of range and convert numbers into characters
  # in consistent scientific format
  # formatC converts to 0.00e+00 format; digits argument gives digits after decimal
  x[abs(x) >= max_cut | abs(x) <= min_cut] <- formatC(
    x[abs(x) >= max_cut | abs(x) <= min_cut],
    format = "e", digits = digits - 1
  )

  # Replace scientific format zeros with plain zeros
  pattern <- paste(rep(0, digits - 1), sep = "", collapse = "")
  pattern <- paste0("0.", pattern, "e+00")    # Creates 0.00e+00 for replacement
  x[x == pattern] <- "0"

  # Convert scientific notation into a math expression
  # Match pattern of e + or - 0 (one or zero 0s), replace with x 10^+ or -
  x <- gsub(pattern = "e\\+0?", replacement = " %*% 10^", x)
  x <- gsub(pattern = "e\\-0?", replacement = " %*% 10^-", x)

  # Add units if given; tilde means space in math expression
  if (is.character(units)) {
    x <- paste0(x, "~", units)
  }
  
  # If the original vector had any NA values, put them back in the same locations
  if (exists('xNA')) {
    xNA[!is.na(xNA)] <- x   # Find locations that are not NA and place x values there
    x <- xNA
  }

  # Convert text into expression format
  x <- parse(text = x)
}
```

## Link to Render LaTeX expressions

<https://github.com/nschloe/purple-pi?activate>
