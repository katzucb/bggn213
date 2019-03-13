Class07 functions and packages
================
Lydia Hernandez
1/30/2019

Functions revisited
-------------------

Load (i.e. **source**) our rescale() function from last day.

``` r
source("http://tinyurl.com/rescale-R")
```

Test this function

``` r
rescale(1:5)
```

    ## [1] 0.00 0.25 0.50 0.75 1.00

``` r
is.numeric(1:5)
```

    ## [1] TRUE

``` r
#rescale(c(1:5), "string")
```

``` r
rescale2(c(1:5), "string")
```

    ## [1] 0.00 0.25 0.50 0.75 1.00

``` r
is.numeric("string")
```

    ## [1] FALSE

``` r
!is.numeric("string")
```

    ## [1] TRUE

``` r
x <- c( 1, 2, NA, 3, NA)
y <- c(NA, 3, NA, 3, 4)
```

``` r
is.na(x)
```

    ## [1] FALSE FALSE  TRUE FALSE  TRUE

``` r
is.na(y)
```

    ## [1]  TRUE FALSE  TRUE FALSE FALSE

``` r
is.na(x) & is.na(y)
```

    ## [1] FALSE FALSE  TRUE FALSE FALSE

``` r
sum(is.na(x) & is.na(y))
```

    ## [1] 1

``` r
# which(is.na(x) & is.na(y))
```

Now take our working snippet and make a first function

``` r
both_na <- function(x,y) {
  # Check for NA elements in both vectors
  sum(is.na(x) & is.na(y))
}
```

``` r
both_na(x,y)
```

    ## [1] 1

``` r
x <- c(NA, NA, NA)
y1 <- c( 1, NA, NA)
y2 <- c( 1, NA, NA, NA)
```

``` r
# What will this return?
both_na(x, y2)
```

    ## Warning in is.na(x) & is.na(y): longer object length is not a multiple of
    ## shorter object length

    ## [1] 3

``` r
#both_na2(x, y2)
```

``` r
# Lets define an example x and y
x <- c( 1, 2, NA, 3, NA)
y <- c(NA, 3, NA, 3, 4)
both_na3(x,y)
```

    ## Found 1 NA's at position(s):3

    ## $number
    ## [1] 1
    ## 
    ## $which
    ## [1] 3
