Using Scientific Format Functions
================
Christy Pickering
2021-08-09

  - [`to_scientific()` Function](#to_scientific-function)
  - [`label_scientific()` Function](#label_scientific-function)
  - [`signif_custom()` Function](#signif_custom-function)
  - [Examples](#examples)
  - [References and Resources](#references-and-resources)

Here I discuss three functions I wrote to convert numbers to
customizable scientific format for ggplot axis labels, annotations,
legend labels, etc. I also provide examples of how to use each function.

Source code containing the described functions is located
[here](Code/scientific_conversion.R).

# `to_scientific()` Function

This function converts a vector of numbers to formatted scientific
notation expressions. It is called by `label_scientific()` to convert
the `ggplot` scale function labels to scientific format.

## Inputs

  - `x` = the number or vector of numbers to be converted into
    scientific format
  - `digits` = the number of significant figures to display in the
    converted output
  - `max_cut` = above this number, values will be converted into
    scientific format (inclusive)
  - `min_cut` = below this number, values will be converted into
    scientific format (inclusive)
  - `common` = specifies if scientific format numbers should share a
    common exponential factor
  - `units` = if specified, adds units to the converted output

## Output

  - Outputs numbers between `min_cut` and `max_cut` as expressions of
    plain numbers, with `units` if given
  - Outputs numbers less than `min_cut` or greater than `max_cut` as
    expressions with numbers in scientific format, with `units` if given

Outputs are formatted as expressions because R or `ggplot` can parse the
expressions to display formatted numbers. `NA` values are preserved
because `ggplot` often uses `NA` when the axis limits are not specified.

## Notes

Inputs are rounded before checking if they are in the range for
conversion to scientific format; e.g., \(99999\) will be rounded to
\(1 \times 10^5\), so if the cutoff is \(1 \times 10^5\), it will get
converted to scientific format.

The `common` argument specifies if the scientific format numbers should
share a common exponential factor. It selects the most commonly
occurring exponential factor to use as the shared factor; if multiple
factors occur equally, it selects the largest. This should only be used
if the numbers are within \(10^4\) of each other.

## Function Code

``` r
to_scientific <- function(x,
                          digits = 2,
                          max_cut = 10^5,
                          min_cut = 10^-3,
                          common = FALSE,
                          units = NULL) {
  
  # Convert input to consistent number format
  x <- as.numeric(as.character(x))
  
  # Fix NA values
  if (any(is.na(x))) {
    
    # To fix NAs in unspecified positions, find location and remove
    x_na <- x # Store original input for recovery later
    x <- x[!is.na(x)] # Remove NA values
  }
  
  # Find indices for plain numbers and scientific format numbers
  plain_idx <- which(abs(x) < max_cut & abs(x) > min_cut)
  sci_idx <- which(abs(x) >= max_cut | abs(x) <= min_cut)
  
  # Select numbers in between min_cut and max_cut and round to specified
  # number of digits (not scientific format)
  x[plain_idx] <- signif(x[plain_idx], digits = digits)
  
  # Select numbers outside of range and convert numbers into characters
  # in consistent scientific format
  # formatC converts to 0.00e+00 format; digits argument gives digits after
  # decimal
  x[sci_idx] <- formatC(x[sci_idx], format = "e", digits = digits - 1)
  
  # If all of the numbers should have a common exponential factor, find most
  # common exponential factor and apply it to all scientific format numbers
  if (common) {
    
    # Find all exponent values: match pattern of single character and any
    # numbers after e; keep just the single character (sign) and numbers,
    # convert to numeric
    exponents <- as.numeric(gsub(
      pattern = ".*e(.{1}[0-9]+)",
      replacement = "\\1",
      x[sci_idx]
    ))
    
    # Find the mode of the exponent values
    ux <- unique(exponents)
    tab <- tabulate(match(exponents, ux))
    most <- ux[tab == max(tab)]
    
    # If there are multiple modes, take the maximum one for the common exponent
    if (length(most) > 1) {
      most <- max(most)
    }
    
    # Difference between exponent and common exponent, used as multiplication
    # factor
    diff <- exponents - most
    
    # Find all significand values; match pattern of all numbers before e; keep
    # the numbers, convert to numeric
    significands <- as.numeric(gsub(
      pattern = "(.*)e.*",
      replacement = "\\1",
      x[sci_idx]
    ))
    
    # Scale significands appropriately for common exponent
    significands <- significands * 10^diff
    
    # Format significands to correct number of digits after decimal point and 
    # remove trailing decimal point
    significands <- formatC(
      significands, 
      digits = digits - 1, 
      format = "f", 
      flag = "#"
    )
    significands <- gsub(pattern = "\\.$", replacement = "", significands)
    
    # Convert exponent to formatted text string for combining with significands
    most <- formatC(
      most,
      digits = ifelse(nchar(abs(most)) == 1, 2, nchar(abs(most))),
      flag = "+0"
    )
    
    # Combine significands, e, and exponents to form scientific notation
    x[sci_idx] <- paste0(significands, "e", most)
  }
  
  # Replace scientific format zeros with plain zeros
  pattern <- paste(rep(0, digits - 1), sep = "", collapse = "")
  pattern <- paste0("0.", pattern, "e\\+[0-9]*$") # Makes pattern to find 0
  x <- gsub(pattern = pattern, replacement = "0", x)
  
  # Convert scientific notation into a math expression
  # First, wrap the significand in quotes to preserve trailing zeros
  # Match pattern of any characters at the start of the string prior to e;
  # replace with the matched characters wrapped in quotes followed by e
  x <- gsub(pattern = "(^.*)e", replacement = "'\\1'e", x)
  
  # Next, replace the "e+00" notation with math expression for "x 10^0"
  # Match pattern of e + or - 0 (one or zero 0s); replace with x 10^+ or -
  x <- gsub(pattern = "e\\+0?", replacement = " %*% 10^", x)
  x <- gsub(pattern = "e\\-0?", replacement = " %*% 10^-", x)
  
  # Add units if given; tilde means space in math expression
  if (is.character(units)) {
    x <- paste0(x, "~", units)
  }
  
  # If the original vector had any NA values, put them back in the same
  # locations
  if (exists("x_na")) {
    x_na[!is.na(x_na)] <- x # Find values that are not NA, place x values there
    x <- x_na
  }
  
  # Convert text into expression format
  x <- parse(text = x)
}
```

# `label_scientific()` Function

This function takes the scale breaks as input and uses the
`to_scientific()` function to convert the scale breaks to formatted
scientific notation.

The labels argument of the scale functions can accept a character vector
providing the names or a function that takes the breaks as input and
returns the labels as output. The axis breaks cannot be accessed as a
named variable, but they are the provided input if a function is used
for the labels argument.

This works because of the “lexical scoping” of R functions - if a value
is not defined inside the function, R looks “one level up” until it
reaches the global environment. Since x does not have a value inside the
label\_scientific() function, it looks outside the function for a value,
and it is provided breaks as input by the scale function.

This setup is based on the [`label_scientific()`
function](https://github.com/r-lib/scales/blob/master/R/label-scientific.R)
in the [`scales` package](https://scales.r-lib.org/reference/index.html)

## Inputs

  - `digits` = the number of significant figures to display in the
    converted output
  - `max_cut` = above this number, values will be converted into
    scientific format (inclusive)
  - `min_cut` = below this number, values will be converted into
    scientific format (inclusive)
  - `common` = specifies if scientific format numbers should share a
    common exponential factor
  - `units` = if specified, adds units to the converted output

## Output

  - Outputs numbers between `min_cut` and `max_cut` as expressions of
    plain numbers, with `units` if given
  - Outputs numbers less than `min_cut` or greater than `max_cut` as
    expressions with numbers in scientific format, with `units` if given

## Function Code

``` r
label_scientific <- function(digits = 2,
                             max_cut = 10^5,
                             min_cut = 10^-3,
                             common = FALSE,
                             units = NULL) {
  
  # Defines a new function that takes x as input, but x was not input into the
  # function or defined inside the function. When this occurs, R looks one level
  # up for the value, so it will find the breaks provided by the ggplot scale
  # function and set those to x. Then it will call to_scientific() with the
  # provided input for x
  function(x) {
    to_scientific(
      x,
      digits = digits,
      max_cut = max_cut,
      min_cut = min_cut,
      common = common,
      units = units
    )
  }
}
```

# `signif_custom()` Function

This function is a combination of the `signif()` and
`ceiling()`/`floor()` functions. It allows numbers to be rounded to a
specified number of significant digits, but to specifically have
`ceiling()` or `floor()` applied for the rounding.

## Inputs

  - `x` = the number or vector of numbers to be rounded to set
    significant figures
  - `digits` = the number of significant figures to round to
  - `option` = the specific rounding function to apply; can use
    `ceiling`, `floor`, `trunc`, or `round`

## Output

  - Outputs numbers rounded to the specified number of `digits`, using
    the function given by `option`

## Function Code

``` r
signif_custom <- function(x, digits = 1, option = ceiling) {
  
  # Convert input to consistent number format
  sci <- formatC(x, format = "e", digits = digits)
  
  # Isolate the specific number representing the exponent
  # Match pattern of single character and any numbers after e; keep just the
  # single character (sign) and numbers, convert to numeric
  exponent <- as.numeric(gsub(
    pattern = ".*e(.{1}[0-9]+)",
    replacement = "\\1",
    sci
  ))
  
  # Isolate the specific number representing the significand
  # Match pattern of all numbers before e; keep the numbers, convert to numeric
  significand <- gsub(pattern = "([0-9]*)e.*", replacement = "\\1", sci)
  
  # Calculate the multiple that should be rounded to
  place <- 1 / (10 ^ (digits - 1))
  
  # Used the specified function to round the significand to the given
  # number of digits
  x <- option(as.numeric(significand) / place) * place
  
  # Multiply by the original exponent
  x <- x * 10 ^ (as.numeric(exponent))
}
```

# Examples

## `to_scientific()`

## `label_scientific()`

To display how these functions can work with `ggplot`, we need to load
the `ggplot2` and `scales` packages. The example data set for plotting
is located [here](Data/ternary.RData).

``` r
# Load necessary packages
library("ggplot2")  # For creating high-quality plots
library("scales")   # For changing labeling of log-scale axes

# Load saved data set for example plots
load("Data/ternary.RData")
```

The plot we will create here is a heat map of antibody ternary complex
formation dependent on the antibody concentration and the rate of the
initial binding step (to form a binary complex).

The first few rows of the `ternary` data set are shown below. The `kon`
and `Ab` columns give the \(\log_{10}()\) of the data – we will correct
the x and y axes to display this correctly with the `scales` package.

``` r
head(ternary)
```

    ## # A tibble: 6 x 3
    ##     kon    Ab    Max
    ##   <dbl> <dbl>  <dbl>
    ## 1    -7  -6   16512.
    ## 2    -7  -6.1 16828.
    ## 3    -7  -6.2 16837.
    ## 4    -7  -6.3 16538.
    ## 5    -7  -6.4 15943.
    ## 6    -7  -6.5 15083.

The base plot displays the data as a heat map, made with `geom_raster()`
and `interpolate = TRUE`. A theme has been applied to the plot to
enlarge the font and fix the plot margins.

``` r
base_plot <- ggplot(ternary, aes(x = Ab, y = kon, fill = Max)) +
  geom_raster(interpolate = TRUE) +
  scale_x_continuous(name = "[BS1] (M)", expand = c(0,0)) +
  scale_y_continuous(
    name = expression(k[On]~Monovalent~Step~(nM^{-1}*s^{-1})),
    expand = c(0,0)
  ) +
  scale_fill_viridis_c(name = "Complexes/Cell", option = "plasma") +
  ggtitle("Base Plot") +
  heatmap_theme +
  guides(fill = guide_colorbar(barheight = unit(4,'inch')))

base_plot
```

<img src="Using-Scientific-Format-Functions_files/figure-gfm/base-plot-1.svg" width="80%" />

Next, we will use functions from the `scales` package to correct the x
and y axes to accurately reflect the underlying data. The
[`label_math()`
function](https://scales.r-lib.org/reference/label_parse.html) adds math
expressions to the axis labels, with `.x` denoting where the data should
be inserted.

``` r
scales_plot <- ggplot(ternary, aes(x = Ab, y = kon, fill = Max)) +
  geom_raster(interpolate = TRUE) +
  scale_x_continuous(
    name = "[BS1] (M)",
    expand = c(0,0),
    labels = label_math(10^.x) # Data will go in the place of .x
  ) +
  scale_y_continuous(
    name = expression(k[On]~Monovalent~Step~(nM^{-1}*s^{-1})),
    expand = c(0,0),
    labels = label_math(10^.x) # Data will go in the place of .x
  ) +
  scale_fill_viridis_c(name = "Complexes/Cell", option = "plasma") +
  ggtitle("Corrected Axes") +
  heatmap_theme +
  guides(fill = guide_colorbar(barheight = unit(4,'inch')))

scales_plot
```

<img src="Using-Scientific-Format-Functions_files/figure-gfm/scales-plot-1.svg" width="80%" />

We can then use the `label_scientific()` function to modify the heat map
legend to show the complexes per cell in scientific notation. No input
is required for the function – the breaks are automatically passed to
the function by `ggplot`.

``` r
sci_plot <- ggplot(ternary, aes(x = Ab, y = kon, fill = Max)) +
  geom_raster(interpolate = TRUE) +
  scale_x_continuous(
    name = "[BS1] (M)",
    expand = c(0,0),
    labels = label_math(10^.x) # Data will go in the place of .x
  ) +
  scale_y_continuous(
    name = expression(k[On]~Monovalent~Step~(nM^{-1}*s^{-1})),
    expand = c(0,0),
    labels = label_math(10^.x) # Data will go in the place of .x
  ) +
  scale_fill_viridis_c(
    name = "Complexes/Cell", 
    option = "plasma",
    labels = label_scientific() # No arguments required
  ) +
  ggtitle("Scientific Notation") +
  heatmap_theme +
  guides(fill = guide_colorbar(barheight = unit(4,'inch')))

sci_plot
```

<img src="Using-Scientific-Format-Functions_files/figure-gfm/sci-plot-1.svg" width="80%" />

Initially, calling `label_scientific()` changed nothing about the heat
map legend – why? Because the default value for `max_cut` is \(10^5\),
so none of the values on the plot were large enough to be converted to
scientific notation. We can change this by specifying a lower cutoff for
`max_cut`.

``` r
sci_plot2 <- ggplot(ternary, aes(x = Ab, y = kon, fill = Max)) +
  geom_raster(interpolate = TRUE) +
  scale_x_continuous(
    name = "[BS1] (M)",
    expand = c(0,0),
    labels = label_math(10^.x) # Data will go in the place of .x
  ) +
  scale_y_continuous(
    name = expression(k[On]~Monovalent~Step~(nM^{-1}*s^{-1})),
    expand = c(0,0),
    labels = label_math(10^.x) # Data will go in the place of .x
  ) +
  scale_fill_viridis_c(
    name = "Complexes/Cell", 
    option = "plasma",
    labels = label_scientific(max_cut = 10) # Can give optional arguments
  ) +
  ggtitle("Scientific Modified") +
  heatmap_theme +
  guides(fill = guide_colorbar(barheight = unit(4,'inch')))

sci_plot2
```

<img src="Using-Scientific-Format-Functions_files/figure-gfm/sci-plot2-1.svg" width="80%" />

By specifying a much smaller maximum cutoff with `max_cut`, we have made
the heat map legend display with scientific notation. Now, we can modify
the specific scale breaks used to tweak the display further.

For heat map scales, it’s generally helpful to set the `limit` argument
a little outside the limits that you want to display. With
[`breaks_extended()` from the `scales`
package](https://scales.r-lib.org/reference/breaks_extended.html), we
can give a desired number of breaks. The function will try to match the
exact number requested, or it will display as close to that number as is
feasible for the plot display.

With these changes, we can also change the number of significant figures
that display with the `digits` argument. `trailing` is `TRUE` by
default, so trailing zeros will be preserved.

``` r
sci_plot3 <- ggplot(ternary, aes(x = Ab, y = kon, fill = Max)) +
  geom_raster(interpolate = TRUE) +
  scale_x_continuous(
    name = "[BS1] (M)",
    expand = c(0,0),
    labels = label_math(10^.x) # Data will go in the place of .x
  ) +
  scale_y_continuous(
    name = expression(k[On]~Monovalent~Step~(nM^{-1}*s^{-1})),
    expand = c(0,0),
    labels = label_math(10^.x) # Data will go in the place of .x
  ) +
  scale_fill_viridis_c(
    name = "Complexes/Cell", 
    option = "plasma",
    limits = c(-1,1.751*10^4), # Sets the legend limits
    breaks = breaks_extended(n = 7), # Creates breaks on the legend
    labels = label_scientific(max_cut = 10, digits = 3) # Optional arguments
  ) +
  ggtitle("Breaks and Limits") +
  heatmap_theme +
  guides(fill = guide_colorbar(barheight = unit(4,'inch')))

sci_plot3
```

<img src="Using-Scientific-Format-Functions_files/figure-gfm/sci-plot3-1.svg" width="80%" />

If desired, we can also use the `common = TRUE` argument to specify a
common exponential factor for all of the numbers in scientific notation.
If an `exponent` is not given, the function will pick the most
frequently occurring exponential factor by default. If there are
multiple modes, it picks the largest one for the common factor.

``` r
sci_plot4 <- ggplot(ternary, aes(x = Ab, y = kon, fill = Max)) +
  geom_raster(interpolate = TRUE) +
  scale_x_continuous(
    name = "[BS1] (M)",
    expand = c(0,0),
    labels = label_math(10^.x) # Data will go in the place of .x
  ) +
  scale_y_continuous(
    name = expression(k[On]~Monovalent~Step~(nM^{-1}*s^{-1})),
    expand = c(0,0),
    labels = label_math(10^.x) # Data will go in the place of .x
  ) +
  scale_fill_viridis_c(
    name = "Complexes/Cell", 
    option = "plasma",
    limits = c(-1,1.751*10^4), # Sets the legend limits
    breaks = breaks_extended(n = 7), # Creates breaks on the legend
    labels = label_scientific(max_cut = 10, digits = 3, common = TRUE)
  ) +
  ggtitle("Common Exponent") +
  heatmap_theme +
  guides(fill = guide_colorbar(barheight = unit(4,'inch')))

sci_plot4
```

<img src="Using-Scientific-Format-Functions_files/figure-gfm/sci-plot4-1.svg" width="80%" />

# References and Resources

  - [Regex
    patterns](https://github.com/rstudio/cheatsheets/raw/master/strings.pdf)
  - [`gsub` and how to keep part of a matching
    pattern](https://stackoverflow.com/questions/37425019/gsub-only-part-of-pattern)
  - [Keep trailing zeros in math
    expression](https://stackoverflow.com/questions/15397789/keeping-trailing-zeroes-with-plotmath)
  - [Lexical scoping in R
    functions](https://adv-r.hadley.nz/functions.html#lexical-scoping)
