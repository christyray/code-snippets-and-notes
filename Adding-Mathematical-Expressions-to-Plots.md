Adding Mathematical Expressions to R Graphics
================
Christy Pickering
2021-10-11

-   [Functions for Creating
    Expressions](#functions-for-creating-expressions)
-   [`plotmath` Intro](#plotmath-intro)
-   [Adding Mathematical Expressions to Titles and
    Labels](#adding-mathematical-expressions-to-titles-and-labels)
-   [Adding Mathematical Expressions to
    Annotations](#adding-mathematical-expressions-to-annotations)
-   [Using the `scales` Package for
    Labels](#using-the-scales-package-for-labels)
-   [Using LaTeX for Expressions](#using-latex-for-expressions)
-   [Annotations with Multiple Lines](#annotations-with-multiple-lines)
-   [Changing Font of Expressions](#changing-font-of-expressions)
-   [`ggtext` Package for Markdown](#ggtext-package-for-markdown)
-   [References and Resources](#references-and-resources)

# Functions for Creating Expressions

R has two data types that hold what we think of as “expressions”: calls
and expressions. A `call` is a single function or R expression, and the
`expression` data structure is really a vector of expressions. See
[Chapter 18 in Advanced R](https://adv-r.hadley.nz/expressions.html) or
[this Stack Overflow
post](https://stackoverflow.com/questions/20355547/expression-vs-call)
for more information on the distinction.

When the `text` argument of a text-drawing function in R (e.g., labels
on a plot) is a call or an expression, R interprets the argument as a
mathematical expression and uses `plotmath` to format the output.

There are a variety of functions to create a `call` or an `expression`
in R:

-   `quote()` = simply returns its argument as a single R `call`, can be
    any R expression
-   `bquote()` = also returns its argument as a single R `call`, except
    it evaluates any terms wrapped in `.()`
-   `substitute(expr, environment)` = substitutes values into `expr`
    based on the given `environment`, returns a single R `call`
    -   E.g., `substitute(x + y, list(x = 1))` returns the call `1 + y`
-   `expression()` = returns its argument as an `expression` vector (a
    list of `calls`)
-   `parse(text = expr)` = converts a string `expr` to an `expression`
    vector
-   `str2expression(expr)` = a special version of `parse()` that
    functions as `parse(text = expr, keep.source = FALSE)`
-   `deparse(expr)` = turn an unevaluated expression into a character
    string, also works on calls
    -   Note that `deparse()` has a line length of 60 bytes as a
        default; any lines longer than 60 bytes will be added as
        additional elements to a character vector. This is problematic
        for formatting expressions for plots - `deparse1()` added in R
        version 4.0 avoids this issue

In practice, `quote()`, `bquote()`, `expression()`, and
`parse(text = expr)` function very similarly. `bquote()` is the best to
use for single mathematical expressions since it can accept variables as
additional input. `expression()` is useful if you need multiple
expressions (e.g., for axis ticks).

# `plotmath` Intro

`plotmath` is R’s mathematical expression interpreter. It follows some
of the general ideas of formatting math expressions in LaTeX, but it
uses it’s own syntax and is more limited in functionality. The
documentation for `plotmath` is
[here](https://rdrr.io/r/grDevices/plotmath.html) and includes a table
of syntax. The original publication describing `plotmath` is
[here](https://www.tandfonline.com/doi/abs/10.1080/10618600.2000.10474900).

`plotmath` has a long list of included expressions in the documentation;
some common ones that you may want to use:

| Syntax               | Output             |
|----------------------|--------------------|
| `x + y`              | x + y              |
| `x - y`              | x - y              |
| `x / y`              | x / y              |
| `x * y`              | xy                 |
| `x %*% y`            | x x y              |
| `x^{y}`              | Superscript        |
| `x[y]`               | Subscript          |
| `frac(x, y)`         | Fractions          |
| `~`                  | Spacing            |
| `==`                 | Equal Sign         |
| `{}`                 | Invisible Grouping |
| `()`                 | Visible Grouping   |
| `bold()`             | Bold Text          |
| `italic()`           | Italic Text        |
| `atop(line1, line2)` | Two Lines          |

Expressions in R have to be valid in R syntax, so you will occasionally
run into errors. For example, expressions cannot end in an equal sign
and `~` sometimes throws errors or doesn’t add space as expected. This
is due to how R parses certain expressions and operators.

This section is adapted from [this
article](https://trinkerrstuff.wordpress.com/2018/03/15/2246/). As an
example, my goal is to generate this text in my plot title:

<img src='Figures/Math-Expressions/text.png' width='288'><br>

<img src='Figures/Math-Expressions/key.png' width='432'><br>

The code to generate this text will consist of the `bquote()` function,
strings, a math expression, and a variable.

With `bquote()`:

-   Strings are wrapped in quotes
-   Space is added between strings and other components with `~`
-   Math expressions are written using `plotmath` syntax
-   Variables are included with `.(variablename)`

Based on these rules, I can generate the second line of my goal text
with this code.

<img src='Figures/Math-Expressions/bquote1.png' width='432'><br>

To add an additional line, I can format the text as a fraction without a
dividing line using the `atop()` function. For more than two lines of
text, see [Annotations with Multiple
Lines](#annotations-with-multiple-lines) or [`ggtext`
Package](#ggtext-package-for-markdown).

<img src='Figures/Math-Expressions/bquote2.png' width='720'>

# Adding Mathematical Expressions to Titles and Labels

With the complete `bquote()` code, I can add the formatted math
expression to my `ggplot`. For titles, labels, or any text that is not a
`geom`, you can add the expression to the plot by using `bquote()` in
the text location.

For example, here I use my `bquote()` expression in the `ggtitle()`
function to add the math expression to my plot title.

``` r
kd <- 3.06

p <- ggplot(NULL, aes(x = 1, y = 1)) +
  theme(
    plot.title = element_text(hjust = 0.5, family = "Roboto", size = 16),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  ggtitle(bquote(atop("Tocilizumab",K[D] == .(kd) ~ "nM")))

ggsave(
  "Figures/Math-Expressions/title.png", 
  plot = p, 
  device = ragg::agg_png,
  width = 3, height = 1.5, dpi = 600
)
```

<img src='Figures/Math-Expressions/title.png' width='600'><br>

I could also add the formatted expression to an axis label with
`xlab()`/`ylab()` or `scale_x_continuous()`, etc.

``` r
p <- ggplot(NULL, aes(x = 1, y = 1)) +
  theme(
    plot.title = element_blank(),
    axis.title = element_text(hjust = 0.5, family = "Roboto", size = 14),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  scale_x_continuous(name = bquote(atop("Tocilizumab",K[D] == .(kd) ~ "nM")))

ggsave(
  "Figures/Math-Expressions/label.png", 
  plot = p, 
  device = ragg::agg_png,
  width = 3, height = 1.5, dpi = 600
)
```

<img src='Figures/Math-Expressions/label.png' width='600'>

# Adding Mathematical Expressions to Annotations

`geom_text()` and `geom_label()` labels do not accept `expressions` or
`calls` - inputs to `geoms` have to be formatted as a string. This can
be done by either directly formatting the expression as a string or by
using `deparse1()` around the `bquote()` text. Then, use the argument
`parse = TRUE` to have `geom_text()` parse the text to a math
expression.

Note: `deparse()` has a line limit of 60 bytes by default, any lines
longer than 60 bytes will be formatted as additional elements of a
character vector. This is problematic for formatting annotations, so
`deparse1()` added in R version 4 (released April 2020) should be used
instead.

If the expression contains any strings, you can use alternating single
quotes and double quotes to maintain the string components. For example,
`bquote(atop("Tocilizumab",K[D] == .(kd) ~ "nM"))` formatted as a string
becomes `'atop("Tocilizumab",K[D] == .(kd) ~ "nM")'`. Alternatively, you
can wrap the original `bquote()` in `deparse1()`.

[`annotate()`](https://ggplot2-book.org/annotations.html#custom-annotations)
ultimately calls `geom_text()` to create the annotation, so it follows
the same rule of needing a string or `deparse1()` and the argument
`parse = TRUE`. There are additional examples of expressions with
`geom_label()`
[here](https://adventuresindata.netlify.app/post/2019-01-27-plotmath-in-ggplot2/).

``` r
p <- ggplot(NULL, aes(x = 1, y = 1)) +
  ylim(0.8, 1.2) +
  theme(
    plot.title = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  annotate(
    geom = "text", 
    x = 1, y = 1, 
    label = deparse1(bquote(atop("Tocilizumab",K[D] == .(kd) ~ "nM"))), 
    parse = TRUE,
    family = "Roboto", 
    size = 8
  )

ggsave(
  "Figures/Math-Expressions/annotation.png", 
  plot = p, 
  device = ragg::agg_png,
  width = 3, height = 1.5, dpi = 600
)
```

<img src='Figures/Math-Expressions/annotation.png' width='600'>

# Using the `scales` Package for Labels

The [`scales` package](https://scales.r-lib.org/index.html) has many
functions for adjusting axis scales and labels. There are many for
formatting currency, dates, SI units, etc. Here, I’m focusing on the
functions for mathematical expressions.

The `label_parse()` function produces an expression from a string, and
the `label_math()` function constructs expressions from a combination of
a string and an inserted variable `.x`. `label_parse()` is analogous to
`quote()` and `label_math()` is analogous to `bquote()`. They are
helpful for formatting axis labels using *plotted data*, versus
`bquote()` which is easier for more complicated expressions with *named
variables*.

Here, I will use `label_parse()` to convert the text “alpha”, “beta”,
and “gamma” to Greek letter symbols. `label_parse()` will convert the
axis labels to expressions and render the expressions with `plotmath`.
Note that the `ragg` package always uses the system default font for
symbols, regardless of font specified. See [Changing Font of
Expressions](#changing-font-of-expressions) for how to change the symbol
font.

``` r
library("scales")

greek <- as.data.frame(c("alpha", "beta", "gamma"))
names(greek) <- "greek"

p <- ggplot(greek, aes(x = greek, y = 1)) +
  theme(
    plot.title = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(family = "Fira Sans", size = 16),
    axis.text.y = element_blank()
  ) +
  scale_x_discrete(name = NULL, labels = label_parse())

ggsave(
  "Figures/Math-Expressions/parse.png",
  plot = p,
  device = ragg::agg_png,
  width = 3, height = 1, dpi = 600
)
```

<img src='Figures/Math-Expressions/parse.png' width='600'><br>

`label_math()` works similarly to `bquote()` with a slightly different
syntax. Here, the data is `1, 2, 3, 4, 5`, and I want the x-axis labels
to show e^1, e^2, etc. With `bquote()` variables are included using
`.(variablename)`. With `label_math()` variables are included with `.x`,
where `x` is whatever `ggplot` passes for the axis breaks.

``` r
num <- as.data.frame(c(1, 2, 3, 4, 5))
names(num) <- "num"

p <- ggplot(num, aes(x = num, y = 1)) +
  theme(
    plot.title = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(family = "Roboto", size = 16),
    axis.text.y = element_blank()
  ) +
  scale_x_continuous(name = NULL, labels = label_math(e^{-.x}))

ggsave(
  "Figures/Math-Expressions/math.png", 
  plot = p, 
  device = ragg::agg_png,
  width = 3, height = 1, dpi = 600
)
```

<img src='Figures/Math-Expressions/math.png' width='600'>

# Using LaTeX for Expressions

`bquote()` is good for expressions with some math formatting but nothing
incredibly extensive. If you need to render an entire equation with
several terms with complicated formatting, the `latex2exp` package will
be useful.

The `latex2exp` provides the `TeX()` function to convert LaTeX formatted
input to an R expression that can be used in plotting. This section is
based on the [`latex2exp` package
vignette](https://cran.r-project.org/web/packages/latex2exp/vignettes/using-latex2exp.html).

Prior to R version 4.0 (released April 2020), the correct syntax for
formatting LaTeX expressions was `TeX("$\\alpha$")` where any normal `\`
needed to be escaped with an additional `\`. R version 4.0 introduced a
new syntax for specifying raw strings: `r"(string text)"`. This can be
used in the `TeX` function as `TeX(r"($\alpha$)")`.

The output of `TeX()` is an R `expression` and can be used with `ggplot`
just like the `bquote()` and `expression()` functions. If the expression
is too long to fit on one line, you can use `paste0()` to connect
multiple strings.

You can quickly test what a rendered LaTeX expression will look like
with `plot()`. Only a limited subset of LaTeX is supported due to
`plotmath` limitations. The full list is available at the [`latex2exp`
package
vignette](https://cran.r-project.org/web/packages/latex2exp/vignettes/using-latex2exp.html).

``` r
library("latex2exp")

ragg::agg_png(
  "Figures/Math-Expressions/latex_example.png", 
  width = 6, height = 2, 
  units = "in", res = 600
)
plot(
  TeX(paste0(r"(A $\LaTeX$ formula: )",
             r"($\frac{d\lbrack LR \rbrack}{dt} = )",
             r"(k_{on}\lbrack L \rbrack \lbrack R \rbrack )",
             r"(- k_{off}\lbrack RL \rbrack$)"))
)
dev.off()
```

<img src='Figures/Math-Expressions/latex_example.png' width='900'>

``` r
p <- ggplot(NULL, aes(x = 1, y = 1)) +
  theme(
    plot.title = element_text(hjust = 0.5, family = "Roboto", size = 10),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  ggtitle(TeX(paste0(r"(A $\LaTeX$ formula: )",
                     r"($\frac{d\lbrack LR \rbrack}{dt} = )",
                     r"(k_{on}\lbrack L \rbrack \lbrack R \rbrack )",
                     r"(- k_{off}\lbrack RL \rbrack$)")))

ggsave(
  "Figures/Math-Expressions/latex_title.png", 
  plot = p, 
  device = ragg::agg_png,
  width = 3, height = 1.5, dpi = 600
)
```

<img src='Figures/Math-Expressions/latex_title.png' width='600'><br>

Just as with `bquote()`, labels added with `geom_text()`,
`geom_label()`, or `annotate()` have to be passed as text strings.
Instead of using `deparse1()`, we can use the `output = "character"`
argument to get a formatted text string. We then use `geom_text()`,
`geom_label()`, or `annotate()` with the `parse = TRUE` argument.

``` r
p <- ggplot(NULL, aes(x = 1, y = 1)) +
  ylim(0.8, 1.2) +
  theme(
    plot.title = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  annotate(
    geom = "text", 
    x = 1, y = 1, 
    label = TeX(paste0(r"(A $\LaTeX$ formula: )",
                       r"($\frac{d\lbrack LR \rbrack}{dt} = )",
                       r"(k_{on}\lbrack L \rbrack \lbrack R \rbrack )",
                       r"(- k_{off}\lbrack RL \rbrack$)"),
                output = "character"), 
    parse = TRUE,
    family = "Roboto", 
    size = 4
  )

ggsave(
  "Figures/Math-Expressions/latex_annotation.png", 
  plot = p, 
  device = ragg::agg_png,
  width = 3.5, height = 1, dpi = 600
)
```

<img src='Figures/Math-Expressions/latex_annotation.png' width='700'>

# Annotations with Multiple Lines

`plotmath` does not support the newline character `\n`, so adding
multiple lines to a math expression is somewhat tricky. For only two
lines, the `atop(line1, line2)` function will format lines like a
fraction without a dividing line.

For more than two lines, we can use `atop()` multiple times, but
`atop()` will shrink successive lines (since it is formatting them like
fractions). To prevent this, we can set the text to “text style” with
`textstyle()`. Changing the text style affects the spacing between
lines, so we can use `scriptstyle()` or `scriptscriptstyle()` as
necessary to modify the spacing. (Adapted from [this Stack Overflow
post](https://stackoverflow.com/questions/40577023/r-ggplot-annotated-with-atop-using-three-values-and-bgroup).)

Remember that `annotate()` requires labels formatted as strings, so we
need to use `deparse1()` and `parse = TRUE`.

For annotations with multiple lines *without* math formatting, see the
[`ggtext` package](#ggtext-package-for-markdown).

``` r
p <- ggplot(NULL, aes(x = 1, y = 1)) +
  ylim(0.8, 1.2) +
  theme(
    plot.title = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  annotate(
    geom = "text",
    x = 1, y = 1,
    label = deparse1(
      bquote(atop(textstyle("Antibody:"),
                  atop(textstyle("Tocilizumab"),
                       atop(scriptscriptstyle(""),
                            textstyle(K[D] == .(kd) ~ "nM")))))),
    parse = TRUE,
    family = "Roboto",
    size = 6
  )

ggsave(
  "Figures/Math-Expressions/annotation-multiple.png", 
  plot = p, 
  device = ragg::agg_png,
  width = 3, height = 2, dpi = 600
)
```

<img src='Figures/Math-Expressions/annotation-multiple.png' width='600'>

# Changing Font of Expressions

## Font Family

For plot titles, axis titles, axis labels, etc., the font is specified
by the plot `theme()`. The font for any given component can be set with
`element_text(family = "Font Name", size = PT)`. See the [Titles and
Labels](#adding-mathematical-expressions-to-titles-and-labels) for an
example.

`geoms` like `geom_text()`, `geom_label()`, and `annotate()` treat the
font family as an aesthetic, so they do not inherit the font from
`theme()`. The font has to be specified inside the `geom_()` function
with the `family` and `size` arguments, like in
[Annotations](#adding-mathematical-expressions-to-annotations) above.

This all applies expressions generated with the `TeX()` function as
well.

## Font Face

`plotmath` has `bold()`, `italic()`, and `bolditalic()` formatting, but
this formatting does not apply to “symbols.” Exactly what is considered
a symbol is unclear, but it generally does not apply to numbers, Greek
letters, or mathematical symbols.

These functions can also be applied to only a part of the overall
expression.

``` r
p <- ggplot(NULL, aes(x = 1, y = 1)) +
  theme(
    plot.title = element_text(hjust = 0.5, family = "Roboto", size = 16),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  ggtitle(bquote(bolditalic(atop("Tocilizumab",K[D] == .(kd) ~ "nM")))) +
  annotate(
    geom = "text", 
    x = 1, y = 1, 
    label = deparse1(bquote(bold(Bold[Text]) ~ Regular[Text])), 
    parse = TRUE,
    family = "Roboto", 
    size = 6
  )

ggsave(
  "Figures/Math-Expressions/plotmath_bolditalic.png", 
  plot = p, 
  device = ragg::agg_png,
  width = 3, height = 1.5, dpi = 600
)
```

<img src='Figures/Math-Expressions/plotmath_bolditalic.png' width='600'><br>

One method around this problem is to use the `register_font()` function
from the `systemfonts` package. The `register_font()` function allows
the user to specify the location of a font to be used and any OpenType
font features like ligatures and variant characters. The `name` argument
creates a unique name for the font that can then be used in functions
like a font family name.

In this case, we will specify the path to the font file with the desired
weight and italics in the `plain` argument. If we were trying to define
a new font normally, we’d pass the path to the regular font to the
`plain` argument, the path to the bold font to the `bold` argument, and
so on. But here, we are trying to “trick” `plotmath` into thinking the
font is not bold or italic so it will render symbols correctly.

This will change the font for the entire expression, bypassing the
`plotmath` functions. (If you needed different font faces or fonts in
the expression, you could register a different font file to the `bold`
and `italic` arguments of `register_font()` function and then access
them with `bold()` and `italic()` from `plotmath`.)

This is also only compatible with graphics devices from the `ragg` and
`svglite` packages: `agg_png()`, `agg_jpeg()`, `agg_tiff()`, and
`svglite()`. This will not work on Greek letters or some other symbols -
those are set with the system default “Symbol” font, and that font
cannot be changed with the `ragg` device. This will be fixed in a later
release of `ragg`, and the user will be able to use `register_font()` to
set an alternate font to the `symbol` name
([source](https://github.com/r-lib/ragg/issues/90)).

``` r
library("systemfonts")

register_font(
  name = "Roboto Math", 
  plain = "/Users/christypickering/Library/Fonts/Roboto-BlackItalic.ttf",
  features = font_feature(
    ligatures = "standard",
    numbers = "tabular"
  )
)

p <- ggplot(NULL, aes(x = 1, y = 1)) +
  theme(
    plot.title = element_text(hjust = 0.5, family = "Roboto Math", size = 16),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  ggtitle(bquote(atop("Tocilizumab",K[D] == .(kd) ~ "nM"))) +
  annotate(
    geom = "text", 
    x = 1, y = 1, 
    label = deparse1(bquote(bold(Bold[Text]) ~ Regular[Text])), 
    parse = TRUE,
    family = "Roboto Math", 
    size = 6
  )

ggsave(
  "Figures/Math-Expressions/register_font.png", 
  plot = p, 
  device = ragg::agg_png,
  width = 3, height = 1.5, dpi = 600
)
```

<img src='Figures/Math-Expressions/register_font.png' width='600'>

## Symbol Font

With the release of R 4.0.0, the default `x11()`, `png()`, `jpeg()`,
`tiff()`, `bmp()`, `svg()`, `cairo_pdf()`, and `cairo_ps()` devices
accept a `symbolfamily` argument to let the user change the font used
for symbols in an image
([source](https://developer.r-project.org/Blog/public/2020/04/17/changes-to-symbol-fonts-for-cairo-graphics-devices/index.html)).

The `ragg` package uses the system default “Symbol” font and does not
accept alternate fonts. (This will be fixed in a later release - see
[above](#font-face).)

``` r
greek <- as.data.frame(c("alpha", "beta", "gamma"))
names(greek) <- "greek"

p <- ggplot(greek, aes(x = greek, y = 1)) +
  theme(
    plot.title = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(family = "Roboto Slab", size = 16),
    axis.text.y = element_blank()
  ) +
  scale_x_discrete(name = NULL, labels = label_parse())

ggsave(
  "Figures/Math-Expressions/symbol.png",
  plot = p,
  device = png,
  width = 3, height = 1, dpi = 600,
  type = "cairo", symbolfamily = cairoSymbolFont("Roboto Slab")
)
```

<img src='Figures/Math-Expressions/symbol.png' width='600'>

# `ggtext` Package for Markdown

The `ggtext` package provides Markdown and HTML support to `ggplot`. It
is limited by R plotting for text rendering, so it can only support a
limited subset of Markdown and HTML features. It is useful for colors
and rich-text formatting but does not support most math formatting.

The `element_markdown()` function is used with `theme()` to enable
Markdown in plot components. `element_textbox_simple()` is used for
larger amounts of text that require text wrapping. `geom_richtext()`
provides a Markdown-enabled alternative to `geom_text()` and
`geom_label()`. [The documentation](https://wilkelab.org/ggtext/) has
more detail about the functions and options for formatting.

``` r
library("ggtext")

p <- ggplot(greek, aes(x = greek, y = 1)) +
  theme(
    plot.title = element_markdown(hjust = 0.5, size = 12),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  ggtitle(paste0("<span style = 'font-family:Monaco;'>**ggtext**</span> ",
                 "allows for ",
                 "<span style = 'color:#B0413E;'>**rich-text**</span> ",
                 "*formatting*"))

ggsave(
  "Figures/Math-Expressions/ggtext.png", 
  plot = p, 
  device = ragg::agg_png,
  width = 3, height = 1.5, dpi = 600
)
```

<img src='Figures/Math-Expressions/ggtext.png' width='600'>

# References and Resources

-   [`bquote()` for math
    expressions](https://trinkerrstuff.wordpress.com/2018/03/15/2246/)
-   [Expressions in R](https://adv-r.hadley.nz/expressions.html)
-   [Expression
    vs. call](https://stackoverflow.com/questions/20355547/expression-vs-call)
-   [`plotmath`
    documentation](https://rdrr.io/r/grDevices/plotmath.html)
-   [`plotmath`
    publication](https://www.tandfonline.com/doi/abs/10.1080/10618600.2000.10474900)
-   [How `annotate()`
    works](https://ggplot2-book.org/annotations.html#custom-annotations)
-   [Add annotations with math
    expressions](https://r-graphics.org/recipe-scatter-fitlines-text#RECIPE-SCATTER-FITLINES-TEXT)
-   [Math expressions in
    `geom_label()`](https://adventuresindata.netlify.app/post/2019-01-27-plotmath-in-ggplot2/)
-   [`scales` package](https://scales.r-lib.org/index.html)
-   [`latex2exp`
    package](https://cran.r-project.org/web/packages/latex2exp/vignettes/using-latex2exp.html)
-   [Basic LaTeX math
    formatting](http://tug.ctan.org/info/undergradmath/undergradmath.pdf)
-   [R version 4.0
    changes](https://blog.revolutionanalytics.com/2020/04/r-400-is-released.html)
-   [Changing symbol
    font](https://developer.r-project.org/Blog/public/2020/04/17/changes-to-symbol-fonts-for-cairo-graphics-devices/index.html)
-   [`ggtext` package](https://wilkelab.org/ggtext/)
