Homework 2
================
Christine Hou
2022-10-11

``` r
library(tidyverse)
```

    ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()

``` r
library(nycflights13)
library(dbplyr)
```


    Attaching package: 'dbplyr'

    The following objects are masked from 'package:dplyr':

        ident, sql

### What time of day should you fly if you want to avoid delays as much as possible? Does this choice depend on anything? Season? Weather? Airport? Airline? Find three patterns (“null results” are ok!). Write your results into Rmarkdown. Include a short introduction that summarizes the three results. Then, have a section for each finding. Support each finding with data summaries and visualizations. Include your code when necessary. This shouldn’t be long, but it might take some time to find the things you want to talk about and lay them out in an orderly way.

``` r
flights %>% filter (dep_delay > 0) %>% 
  group_by(hour) %>%
  summarize(avg_delay = mean(dep_delay)) %>%
  arrange(avg_delay)
```

    # A tibble: 19 × 2
        hour avg_delay
       <dbl>     <dbl>
     1     5      15.3
     2     7      24.1
     3     6      24.2
     4     9      29.7
     5     8      29.9
     6    12      32.3
     7    11      32.5
     8    10      32.6
     9    13      33.5
    10    14      37.1
    11    23      38.0
    12    15      38.8
    13    16      43.4
    14    17      45.3
    15    18      46.5
    16    22      46.5
    17    20      49.6
    18    21      50.3
    19    19      51.1

> Generally, I filter the rows wth positive `dep_delay` and group by
> `hour`, then calculate the average delay time to see the minimum
> average delay time. It’s clear that the `avg_delay = 15.3` is the
> shortest when `hour = 1`.
