
# Description -------------------------------------------------------------

# Three functions that aid in converting numbers to customizable scientific
# format for ggplot axis labels, annotations, legend labels, etc.

# to_scientific: Replaces numbers with formatted scientific notation
# expressions, requires the user to provide a number or vector input

# label_scientific: Replaces numbers with formatted scientific notation
# expressions, specifically formatted for use with ggplot to take the
# default input

# signif_custom: Combination of the signif() and ceiling()/floor()
# functions to allow numbers to be rounded to a specified number of
# significant digits, but to specifically have ceiling() or floor()
# applied for the rounding

# to_scientific() ---------------------------------------------------------

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
    x <- x[!is.na(x)]

    # Flag to add back in later
    flag = TRUE

  } else {
    flag = FALSE
  }

  # Select numbers in between min_cut and max_cut and round to specified
  # number of digits
  x[abs(x) < max_cut & abs(x) >= min_cut] <- signif(
    x[abs(x) < max_cut & abs(x) >= min_cut],
    digits = digits
  )

  # Select numbers outside of range and convert numbers into characters
  # in consistent scientific format
  x[abs(x) >= max_cut | abs(x) < min_cut] <- formatC(
    x[abs(x) >= max_cut | abs(x) < min_cut],
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

  if (flag) {
    xNA[!is.na(xNA)] <- x
    x <- xNA
  }

  # Convert text into expression format
  x <- parse(text = x)
}

# label_scientific() ------------------------------------------------------

# The labels argument of the scale functions can accept a character vector
# providing the names or a function that takes the breaks as input and
# returns the labels as output. Because I cannot access the breaks as a
# named variable, I have to be able to define the optional argument by
# themselves, and then I can take the breaks as input.

# I believe this works because of the "lexical scoping" of R functions -
# if a value is not defined inside the function, R looks "one level up"
# until it reaches the global environment. Since x does not have a value
# inside the label_scientific() function, it looks outside the function
# for a value, and it is provided breaks as input by the scale function.

label_scientific <- function(digits = 2,
                             max_cut = 10^5,
                             min_cut = 10^-3,
                             units = NULL) {

  # Defines a new function that takes x as input, so R will look outside
  # the function for x, and find the breaks provided by scale, then call
  # the existing to_scientific() function with the provided input
  function(x) {
    to_scientific(
      x,
      digits = digits,
      max_cut = max_cut,
      min_cut = min_cut,
      units = units
    )
  }
}

# signif_custom() ---------------------------------------------------------

signif_custom <- function(x, digits = 1, option = ceiling) {

  # Convert input to consistent number format
  sci <- formatC(x, format = "e", digits = digits)

  # Isolate the exponential term from the full number
  exponent <- gsub(pattern = ".*e", replacement = "", sci)

  # Isolate the specific number representing the exponent
  exponent <- gsub(
    pattern = "\\+*0?([0-9]+)",
    replacement = "\\1",
    exponent
  )

  # Isolate the specific number representing the coefficient
  coefficient <- gsub(pattern = "([0-9]*)e.*", replacement = "\\1", sci)

  # Calculate the multiple that should be rounded to
  place <- 1 / (10^(digits - 1))

  # Used the specified function to round the coefficient to the given
  # number of digits
  x <- option(as.numeric(coefficient) / place) * place

  # Multiply by the original exponent
  x <- x * 10^(as.numeric(exponent))
}
