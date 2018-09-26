Homework2
================

Homework 2
==========

Here is an exploration of the *Gapminder* dataset using `ggplot2` and `dplyr`.

### 1. First, the packages must be loaded.

*Gapminder* is the dataset itself, while `tidyverse` is the meta-package, a collection with tools like `ggplot2` for graphics, `tbl_df` for tibble data.frames, and `dplyr` for manipulating data.

``` r
library(gapminder)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.6
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

### 2. What kind of dataset is *Gapminder*?

A "data.frame" of demographic metrics by countries and how they relate to each other over time. The summary function shows that there are 6 columns. Two are composed of frequency counts (`country` and `continent`), and four are numerical quantities (`year`, `lifeExp`, `pop`, and `gdpPercap`). These values span 1704 rows.

``` r
summary(gapminder)
```

    ##         country        continent        year         lifeExp     
    ##  Afghanistan:  12   Africa  :624   Min.   :1952   Min.   :23.60  
    ##  Albania    :  12   Americas:300   1st Qu.:1966   1st Qu.:48.20  
    ##  Algeria    :  12   Asia    :396   Median :1980   Median :60.71  
    ##  Angola     :  12   Europe  :360   Mean   :1980   Mean   :59.47  
    ##  Argentina  :  12   Oceania : 24   3rd Qu.:1993   3rd Qu.:70.85  
    ##  Australia  :  12                  Max.   :2007   Max.   :82.60  
    ##  (Other)    :1632                                                
    ##       pop              gdpPercap       
    ##  Min.   :6.001e+04   Min.   :   241.2  
    ##  1st Qu.:2.794e+06   1st Qu.:  1202.1  
    ##  Median :7.024e+06   Median :  3531.8  
    ##  Mean   :2.960e+07   Mean   :  7215.3  
    ##  3rd Qu.:1.959e+07   3rd Qu.:  9325.5  
    ##  Max.   :1.319e+09   Max.   :113523.1  
    ## 

``` r
class(gapminder)
```

    ## [1] "tbl_df"     "tbl"        "data.frame"

``` r
ncol(gapminder)
```

    ## [1] 6

``` r
nrow(gapminder)
```

    ## [1] 1704

What years does the *Gapminder* dataset consider? It spans 55 years, from 1952 to 2007.

``` r
max(gapminder$year)-min(gapminder$year)
```

    ## [1] 55

Unlike a matrix that is composed only of numerical values, a "data.frame" can have more than one type of data, like integers and doubles. *Gapminder* has both, so it is a "data.frame".

``` r
sapply(gapminder, typeof)
```

    ##   country continent      year   lifeExp       pop gdpPercap 
    ## "integer" "integer" "integer"  "double" "integer"  "double"

### 3. Exploring the life expectancy of Chile over time.

If you plot Chile's life expactancy `lifeExp` over time `year`, and make the points look like maountains, you get the Andes cordillera range!

``` r
filter(gapminder, country == "Chile") %>%
ggplot(aes(year, lifeExp)) +
  geom_point(shape = 2, stroke = 2, size = 4)
```

![](Homework2_files/figure-markdown_github/unnamed-chunk-5-1.png)

We can also compare all the countries that share the Andes mountain range with Chile.

``` r
filter(gapminder, country == "Chile"
       | country == "Argentina"
       | country == "Peru"
       | country == "Bolivia") %>% 
ggplot(aes(year,lifeExp)) +
  geom_point(aes(colour = country), shape = 2, stroke = 2, size = 4)
```

![](Homework2_files/figure-markdown_github/unnamed-chunk-6-1.png)

We can also look at `lifeExp` by itself through "violin-and-jitter" plots.

``` r
filter(gapminder, country == "Chile"
       | country == "Argentina"
       | country == "Peru"
       | country == "Bolivia") %>% 
  ggplot(aes(country, lifeExp)) +
  geom_violin() +
  geom_jitter(alpha=0.2, shape=1, size=5, stroke=2)
```

![](Homework2_files/figure-markdown_github/unnamed-chunk-7-1.png)

In contrast, we can also look at `pop` or population through "violin-and-jitter" plots.

``` r
filter(gapminder, country == "Chile"
       | country == "Argentina"
       | country == "Peru"
       | country == "Bolivia") %>% 
  ggplot(aes(country, pop)) +
  geom_violin() +
  geom_jitter(alpha=0.2, shape=1, size=5, stroke=2)
```

![](Homework2_files/figure-markdown_github/unnamed-chunk-8-1.png)

### 4. Exploring the raw data of Chile's neighbours.

``` r
filter(gapminder, country == c("Chile", "Argentina", "Bolivia", "Peru"))
```

    ## # A tibble: 12 x 6
    ##    country   continent  year lifeExp      pop gdpPercap
    ##    <fct>     <fct>     <int>   <dbl>    <int>     <dbl>
    ##  1 Argentina Americas   1957    64.4 19610538     6857.
    ##  2 Argentina Americas   1977    68.5 26983828    10079.
    ##  3 Argentina Americas   1997    73.3 36203463    10967.
    ##  4 Bolivia   Americas   1962    43.4  3593918     2181.
    ##  5 Bolivia   Americas   1982    53.9  5642224     3157.
    ##  6 Bolivia   Americas   2002    63.9  8445134     3413.
    ##  7 Chile     Americas   1952    54.7  6377619     3940.
    ##  8 Chile     Americas   1972    63.4  9717524     5494.
    ##  9 Chile     Americas   1992    74.1 13572994     7596.
    ## 10 Peru      Americas   1967    51.4 12132200     5788.
    ## 11 Peru      Americas   1987    64.1 20195924     6361.
    ## 12 Peru      Americas   2007    71.4 28674757     7409.

I was born in Chile in 1993, so I'm interested in summary data of the region during that time. *Gapminder* has the year 1992, which is close enough.

``` r
filter(gapminder,
       
       country == "Chile"
       | country == "Argentina"
       | country == "Peru"
       | country == "Bolivia",
       
       year == 1992) %>%
  arrange(desc(gdpPercap)) %>% 
  select(country, lifeExp, pop, gdpPercap) %>% 
  knitr::kable(digits = 0)
```

| country   |  lifeExp|       pop|  gdpPercap|
|:----------|--------:|---------:|----------:|
| Argentina |       72|  33958947|       9308|
| Chile     |       74|  13572994|       7596|
| Peru      |       66|  22430449|       4446|
| Bolivia   |       60|   6893451|       2962|

I guess I can expect to live to 74 years old!
