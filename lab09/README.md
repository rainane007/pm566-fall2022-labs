Lab 09
================
Yuhong Hu
2022-10-26

\#Problem 2: Before you The following functions can be written to be
more efficient without using parallel:

This function generates a `n x k` dataset with all its entries
distributed poission with mean `lambda`

``` r
set.seed(1235)
fun1 <- function(n = 100, k = 4, lambda = 4) {
  x <- NULL
  
  for (i in 1:n)
    x <- rbind(x, rpois(k, lambda))
  
  return(x)
}
f1<-fun1()
mean(f1)
```

    ## [1] 4.1575

``` r
fun1alt <- function(n = 100, k = 4, lambda = 4) {
  x<-matrix(rpois(n*k, lambda),ncol=4)
}

# Benchmarking
microbenchmark::microbenchmark(
  fun1(),
  fun1alt()
)
```

    ## Warning in microbenchmark::microbenchmark(fun1(), fun1alt()): less accurate
    ## nanosecond times to avoid potential integer overflows

    ## Unit: microseconds
    ##       expr     min      lq      mean   median       uq      max neval cld
    ##     fun1() 159.162 178.719 184.96740 184.3770 188.5385  259.817   100   b
    ##  fun1alt()  12.792  13.694  25.88863  14.2065  14.9650 1142.875   100  a

``` r
d <- matrix(1:16, ncol=4)
d
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    5    9   13
    ## [2,]    2    6   10   14
    ## [3,]    3    7   11   15
    ## [4,]    4    8   12   16
