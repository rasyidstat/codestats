Code Stats
================

Get your R code statistics!

Example
-------

``` r
source("codestats/R_stat.R")
#> Loading tidyverse: ggplot2
#> Loading tidyverse: tibble
#> Loading tidyverse: tidyr
#> Loading tidyverse: readr
#> Loading tidyverse: purrr
#> Loading tidyverse: dplyr
#> Conflicts with tidy packages ----------------------------------------------
#> filter(): dplyr, stats
#> lag():    dplyr, stats
```

``` r
# try on itself
get_stat_r("codestats/R_stat.R")
#> Joining, by = c("file", "filename")
#> $raw
#> # A tibble: 54 x 19
#>    filename               file    id
#>       <chr>              <chr> <int>
#>  1 R_stat.R codestats/R_stat.R     1
#>  2 R_stat.R codestats/R_stat.R     2
#>  3 R_stat.R codestats/R_stat.R     3
#>  4 R_stat.R codestats/R_stat.R     4
#>  5 R_stat.R codestats/R_stat.R     5
#>  6 R_stat.R codestats/R_stat.R     6
#>  7 R_stat.R codestats/R_stat.R     7
#>  8 R_stat.R codestats/R_stat.R     8
#>  9 R_stat.R codestats/R_stat.R     9
#> 10 R_stat.R codestats/R_stat.R    10
#> # ... with 44 more rows, and 16 more variables: txt <chr>, is_fill <dbl>,
#> #   is_comment <dbl>, cnt_char <int>, cnt_char_real <int>,
#> #   cnt_assign <int>, cnt_assign_rev <int>, cnt_pipe <int>,
#> #   cnt_library <int>, cnt_function <int>, cnt_filter <int>,
#> #   cnt_mutate <int>, cnt_select <int>, cnt_groupby <int>,
#> #   cnt_summarise <int>, used_library <list>
#> 
#> $summary
#> # A tibble: 1 x 18
#>                 file filename cnt_blank cnt_fill cnt_comment cnt_char
#>                <chr>    <chr>     <dbl>    <dbl>       <dbl>    <int>
#> 1 codestats/R_stat.R R_stat.R         5       49           8     1928
#> # ... with 12 more variables: cnt_char_real <int>, cnt_assign <int>,
#> #   cnt_assign_rev <int>, cnt_pipe <int>, cnt_library <int>,
#> #   cnt_function <int>, cnt_filter <int>, cnt_mutate <int>,
#> #   cnt_select <int>, cnt_groupby <int>, cnt_summarise <int>,
#> #   used_library <list>
```
