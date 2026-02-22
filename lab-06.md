Lab 06 - Ugly charts and Simpson’s paradox
================
Saima Arina
02/21/2026

### Load packages and data

``` r
library(tidyverse) 
library(dsbox)
library(mosaicData) 
```

``` r
staff <- read_csv("data/instructional-staff.csv")
```

    ## Rows: 5 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): faculty_type
    ## dbl (11): 1975, 1989, 1993, 1995, 1999, 2001, 2003, 2005, 2007, 2009, 2011
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
staff_long <- staff %>%
  pivot_longer(cols = -faculty_type, names_to = "year") %>%
  mutate(value = as.numeric(value))
```

### Exercise 1

``` r
staff_long %>%
  ggplot(aes(
    x = year,
    y = value,
    group = faculty_type,
    color = faculty_type
  )) +
   geom_line(size = 1.1) +
  labs(
    title = "Trends in Instructional Staff Employment, 1975–2011",
    x = "Year",
    y = "Percent of instructional staff",
    color = "Faculty type"
  ) +
  theme_minimal(base_size = 15)
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once per session.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](lab-06_files/figure-gfm/-%20line%20plot-1.png)<!-- -->

### Exercise 2

To highlight an increase in the proportion of part-time faculty over
time compared to other staff types, I would put the visual emphasis on
part-time faculty through a bolder color and muting the colors of the
other types. I could also increase the line thickness for part-time and
decrease it for the others.

### Exercise 3

…

Add exercise headings as needed.
