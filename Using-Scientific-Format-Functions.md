Using Scientific Format Functions
================
Christy Pickering
2021-08-09

  - [`to_scientific()` Function](#to_scientific-function)
  - [`label_scientific()` Function](#label_scientific-function)
  - [`signif_custom()` Function](#signif_custom-function)
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
    xNA <- x            # Store original input for recovery later
    x <- x[!is.na(x)]   # Remove NA values
  }
  
  # Find indices for plain numbers and scientific format numbers
  plain_idx <- which(abs(x) < max_cut & abs(x) > min_cut)
  sci_idx <- which(abs(x) >= max_cut | abs(x) <= min_cut)
  
  # Select numbers in between min_cut and max_cut and round to specified
  # number of digits (not scientific format)
  x[plain_idx] <- signif(x[plain_idx], digits = digits)
  
  # Select numbers outside of range and convert numbers into characters
  # in consistent scientific format
  # formatC converts to 0.00e+00 format; digits argument gives digits after decimal
  x[sci_idx] <- formatC(x[sci_idx], format = "e", digits = digits - 1)
  
  # If all of the numbers should have a common exponential factor, find most common
  # exponential factor and apply it to all scientific format numbers
  if (common) {
    
    # Find all exponent values: match pattern of single character and any numbers
    # after e; keep just the single character (sign) and numbers, convert to numeric
    exponents <- as.numeric(gsub(
      pattern = ".*e(.{1}[0-9]+)", 
      replacement = '\\1', 
      x[sci_idx]
    ))
    
    # Find the mode of the exponent values
    ux <- unique(exponents)
    tab <- tabulate(match(exponents,ux))
    most <- ux[tab == max(tab)]
    
    # If there are multiple modes, take the maximum one for the common exponent
    if (length(most) > 1) {
      most <- max(most)
    }
    
    # Difference between exponent and common exponent, used as multiplication factor
    diff <- exponents - most
    
    # Find all significand values; match pattern of all numbers before e; keep the 
    # numbers, convert to numeric
    significands <- as.numeric(gsub(
      pattern = "(.*)e.*",
      replacement = '\\1',
      x[sci_idx]
    ))
    
    # Scale significands appropriately for common exponent
    significands <- significands * 10^diff
    
    # Format significands to correct number of significant figures and remove trailing
    # decimal point
    significands <- formatC(significands, digits = 2, format = "g", flag = "#")
    significands <- gsub(pattern = "\\.$", replacement = "", significands)
    
    # Convert exponent to formatted text string for combining with significands
    most <- formatC(
      most, 
      digits = ifelse(nchar(abs(most)) == 1, 2, nchar(abs(most))), 
      flag = "+0"
    )
    
    # Combine significands, e, and exponents to form scientific notation
    x[sci_idx] <- paste0(significands, "e",most)
  }
  
  # Replace scientific format zeros with plain zeros
  pattern <- paste(rep(0, digits - 1), sep = "", collapse = "")
  pattern <- paste0("0.", pattern, "e+00")    # Creates 0.00e+00 for replacement
  x[x == pattern] <- "0"
  
  # Convert scientific notation into a math expression
  # First, wrap the significand in quotes to preserve trailing zeros
  # Match pattern of any characters at the start of the string prior to e; replace
  # with the matched characters wrapped in quotes followed by e
  x <- gsub(pattern = "(^.*)e", replacement = "'\\1'e", x)
  
  # Next, replace the "e+00" notation with math expression for "x 10^0"
  # Match pattern of e + or - 0 (one or zero 0s); replace with x 10^+ or -
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
  # function or defined inside the function. When this occurs, R looks one level up
  # for the value, so it will find the breaks provided by the ggplot scale function
  # and set those to x. Then it will call to_scientific() with the provided input for
  # x
  function(x) to_scientific(
    x,
    digits = digits,
    max_cut = max_cut,
    min_cut = min_cut,
    common = common,
    units = units
  )
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
  # Match pattern of single character and any numbers after e; keep just the single
  # character (sign) and numbers, convert to numeric
  exponent <- as.numeric(gsub(
    pattern = ".*e(.{1}[0-9]+)", 
    replacement = '\\1', 
    sci
  ))
  
  # Isolate the specific number representing the significand
  # Match pattern of all numbers before e; keep the numbers, convert to numeric
  significand <- gsub(pattern = "([0-9]*)e.*", replacement = "\\1", sci)
  
  # Calculate the multiple that should be rounded to
  place <- 1 / (10^(digits - 1))
  
  # Used the specified function to round the significand to the given
  # number of digits
  x <- option(as.numeric(significand) / place) * place
  
  # Multiply by the original exponent
  x <- x * 10^(as.numeric(exponent))
}
```

# References and Resources

  - [Regex
    patterns](https://github.com/rstudio/cheatsheets/raw/master/strings.pdf)
  - [`gsub` and how to keep part of a matching
    pattern](https://stackoverflow.com/questions/37425019/gsub-only-part-of-pattern)
  - [Keep trailing zeros in math
    expression](https://stackoverflow.com/questions/15397789/keeping-trailing-zeroes-with-plotmath)
  - [Lexical scoping in R
    functions](https://adv-r.hadley.nz/functions.html#lexical-scoping)
