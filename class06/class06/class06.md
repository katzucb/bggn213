Class 6 R Functions
================
Lydia Hernandez
1/25/2019

File reading (again!)
---------------------

Here we try to use **read.table()** and friends to input some example data into R

Lets insert a code chunk.

``` r
file1 <- "https://bioboot.github.io/bggn213_S18/class-material/test1.txt"
data1 <- read.csv(file1, header = TRUE)
data1
```

    ##   Col1 Col2 Col3
    ## 1    1    2    3
    ## 2    4    5    6
    ## 3    7    8    9
    ## 4    a    b    c

``` r
file2 <- "https://bioboot.github.io/bggn213_S18/class-material/test2.txt"
data2 <- read.table(file2, header = TRUE, sep = "$")
data2
```

    ##   Col1 Col2 Col3
    ## 1    1    2    3
    ## 2    4    5    6
    ## 3    7    8    9
    ## 4    a    b    c

``` r
file3 <- ("https://bioboot.github.io/bggn213_S18/class-material/test3.txt")
data3 <- read.table(file3)
data3
```

    ##   V1 V2 V3
    ## 1  1  6  a
    ## 2  2  7  b
    ## 3  3  8  c
    ## 4  4  9  d
    ## 5  5 10  e

R Functions
-----------

My first function

``` r
add <- function(x, y=1) {
 # Sum the input x and y
 x + y
}
```

Let's use the **add()** function

``` r
add(1)
```

    ## [1] 2

``` r
add(1, 5)
```

    ## [1] 6

``` r
add( c(1,2,3,4))
```

    ## [1] 2 3 4 5

``` r
add( c(1,2,3,4), 4)
```

    ## [1] 5 6 7 8

``` r
#add(1,2,2)
```

``` r
#add(x=1, y="b")
```

Our 2nd function
================

``` r
rescale <- function(x) {
 rng <-range(x)
 (x - rng[1]) / (rng[2] - rng[1])
}
```

``` r
rescale(1:10)
```

    ##  [1] 0.0000000 0.1111111 0.2222222 0.3333333 0.4444444 0.5555556 0.6666667
    ##  [8] 0.7777778 0.8888889 1.0000000

``` r
# How would you get your function to work here?
rescale( c(1,2,NA,3,10) )
```

    ## [1] NA NA NA NA NA

``` r
x <- c(1,2,NA,3,10) 
rng <-range(x)
rng
```

    ## [1] NA NA

``` r
#
```

``` r
rescale2 <- function(x) {
 rng <-range(x, na.rm=TRUE)
 (x - rng[1]) / (rng[2] - rng[1])
}
```

``` r
rescale2( c(1,2,NA,3,10) )
```

    ## [1] 0.0000000 0.1111111        NA 0.2222222 1.0000000

``` r
rescale3 <- function(x, na.rm=TRUE, plot=FALSE) {
 if(na.rm) {
 rng <-range(x, na.rm=na.rm)
 } else {
 rng <-range(x)
 }
 print("Hello")
 answer <- (x - rng[1]) / (rng[2] - rng[1])
 print("is it me you are looking for?")
 if(plot) {
   plot(answer, typ="b", lwd=4)
   print("please don't ever sing again!")
 }
 print("I can see it in ...")
}
```

``` r
rescale3(1:10)
```

    ## [1] "Hello"
    ## [1] "is it me you are looking for?"
    ## [1] "I can see it in ..."

``` r
rescale3(1:10, plot=TRUE)
```

    ## [1] "Hello"
    ## [1] "is it me you are looking for?"

![](class06_files/figure-markdown_github/unnamed-chunk-19-1.png)

    ## [1] "please don't ever sing again!"
    ## [1] "I can see it in ..."

``` r
rescale3(1:10, plot=FALSE)
```

    ## [1] "Hello"
    ## [1] "is it me you are looking for?"
    ## [1] "I can see it in ..."
