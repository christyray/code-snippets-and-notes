Adding Mathematical Expressions to R
================
Christy Pickering
2021-08-25

``` r
library(systemfonts)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.3     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
register_variant(
  name = "Roboto Symbol",
  family = "Roboto",
  weight = "bold"
)

anno3 <- data.frame(label = "alpha", x = 1, y = 0.7)
anno3$label <- expression(alpha)

p <- ggplot(NULL, aes(x = 1, y = 1)) +
  ylim(0.6, 1.2) +
  theme(
    text = element_text(family = "Helvetica", size = 20),
    plot.title = element_text(hjust = 0.5, family = "Roboto Symbol"),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
  ) +
  ggtitle(bquote(atop("BS1",K[D]~"= 3.4 nM")))

ggsave(
  "Figures/Math-Expressions/test_ragg.png", 
  plot = p, 
  device = ragg::agg_png,
  width = 6, height = 4, dpi = 300
)
```

<img src='Figures/Math-Expressions/test_ragg.png' width='100'>

# Functions for Creating Expressions

# `plotmath` Intro

# Adding Mathematical Expressions to Titles and Labels

# Adding Mathematical Expressions to Annotations

# Using the `scales` Package for Labels

# Using LaTeX for Expressions

# Annotations with Multiple Lines

# Changing Font of Expressions

# `ggtext` Package for Markdown

# `draw_label()` from `cowplot` Package

# References and Resources
