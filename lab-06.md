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

``` r
fisheries <- read_csv("data/fisheries.csv")
```

    ## Rows: 216 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): country
    ## dbl (3): capture, aquaculture, total
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

To start with, the 3D pie charts are not visually appealing and do not
provide comparisons equally across countries. Bar charts can replace
them to make the areas and lenghts comparable. Putting capture and
aquaculture side by side within each country and differentated by one
color per production type instead of per country can show comparison and
the total. The rank pattern can be made more obvious by ordering the
countries by either total production or capture. The visuals are also
missing a title, axis labels, and an additional subtitle.

``` r
fisheries_long <- fisheries %>%
  pivot_longer(cols = c(capture, aquaculture), names_to = "type", values_to = "tons")
```

``` r
top_countries <- fisheries_long %>% 
  group_by(country) %>% 
  summarise(total_tons = sum(tons, na.rm = TRUE)) %>% 
  slice_max(total_tons, n = 15) %>% 
  arrange(desc(total_tons)) %>% 
  pull(country)

fisheries_top15 <- fisheries_long %>% 
  filter(country %in% top_countries) %>% 
  mutate(country = factor(country, levels = top_countries))

ggplot(fisheries_top15,
       aes(x = country,
           y = tons,
           fill = type)) +
  geom_col(position = "dodge") +
  labs(
    title = "Capture vs Aquaculture, Top 15 Producing Countries (2016)",
    x = "Country",
    y = "Tons of fish (2016)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](lab-06_files/figure-gfm/-%20plotting-1.png)<!-- -->

### Smokers in Whickham

``` r
data(Whickham)
?Whickham
```

    ## starting httpd help server ... done

``` r
library(performance)
```

### Exercise 1

This study is observational because there was no experimental
manipulation done on the participants. Only observations were recorded
(whether they smoke and their age).

### Exercise 2

``` r
nrow(Whickham)
```

    ## [1] 1314

There are 1,314 observations and each observation is one individual
person from Whickham with their recorded age, smoking status at
baseline, and survival status 20 years later.

### Exercise 3

``` r
ncol(Whickham)
```

    ## [1] 3

There are 3 variables: outcome (categorical), smoker (categorical), and
age (numerical).

``` r
Whickham %>% 
  ggplot(aes(x = smoker)) +
  geom_bar()
```

![](lab-06_files/figure-gfm/-%20variable%20visuals-1.png)<!-- -->

``` r
Whickham %>% 
  ggplot(aes(x = outcome)) +
  geom_bar()
```

![](lab-06_files/figure-gfm/-%20variable%20visuals-2.png)<!-- -->

``` r
Whickham %>% 
  ggplot(aes(x = age)) +
  geom_histogram(binwidth = 5)
```

![](lab-06_files/figure-gfm/-%20variable%20visuals-3.png)<!-- -->

### Exercise 4

I would imagine there to be an association between being a smoker and
having a higher likelihood of death.

### Exercise 5

``` r
Whickham %>%
  count(smoker, outcome)
```

    ##   smoker outcome   n
    ## 1     No   Alive 502
    ## 2     No    Dead 230
    ## 3    Yes   Alive 443
    ## 4    Yes    Dead 139

``` r
Whickham %>%
  ggplot(aes(x = smoker, fill = outcome)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Health outcome by smoking status in Whickham study",
    x = "Smoking status",
    y = "Proportion within smoking group",
    fill = "Outcome"
  ) +
  theme_minimal()
```

![](lab-06_files/figure-gfm/-%20visual%20bar%20chart-1.png)<!-- -->

Conditional Probabilities

Non-smokers: 502 alive, 230 dead = 732 P(Dead\|Non-smoker) = 230/732 =
0.31 or 31% P(Alive\|Non-smoker) = 502/732 = 0.69 or 69%

Smokers: 443 alive, 139 dead = 582 P(Dead\|Smoker) = 139/582 = 0.24 or
24% P(Alive\|Smoker) = 443/582 = 0.76 or 76%

This is conflicting with my initial assumption, since it’s found that
smokers actually have a lower observed death proportion and a higher
survival proportion than non‑smokers.
