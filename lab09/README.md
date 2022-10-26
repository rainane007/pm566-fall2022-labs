Lab 09
================
Yuhong Hu
2022-10-26

# Problem 2: Before you

1.  The following functions can be written to be more efficient without
    using parallel:

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
    ##       expr     min       lq      mean   median       uq     max neval cld
    ##     fun1() 161.089 176.6280 183.25893 180.3795 185.8940 296.184   100   b
    ##  fun1alt()  12.997  13.5095  24.17811  14.1450  14.8625 991.011   100  a

something about matrix

``` r
d <- matrix(1:16, ncol=4)
d[2,1]
```

    ## [1] 2

``` r
d[2]
```

    ## [1] 2

``` r
diag(d)
```

    ## [1]  1  6 11 16

``` r
d[c(1,6,11,16)]
```

    ## [1]  1  6 11 16

``` r
cbind(1:4,1:4)
```

    ##      [,1] [,2]
    ## [1,]    1    1
    ## [2,]    2    2
    ## [3,]    3    3
    ## [4,]    4    4

``` r
d[cbind(1:4,1:4)]
```

    ## [1]  1  6 11 16

2.  Find the column max (hint: Checkout the function `max.col()`).

``` r
# Data Generating Process (10 x 10,000 matrix)
set.seed(1234)
M <- matrix(runif(12),ncol=4)
M
```

    ##           [,1]      [,2]        [,3]      [,4]
    ## [1,] 0.1137034 0.6233794 0.009495756 0.5142511
    ## [2,] 0.6222994 0.8609154 0.232550506 0.6935913
    ## [3,] 0.6092747 0.6403106 0.666083758 0.5449748

``` r
x <- matrix(rnorm(1e4), nrow=10)

# Find each column's max value
fun2 <- function(x) {
  apply(x, 2, max)
}

fun2(M)
```

    ## [1] 0.6222994 0.8609154 0.6660838 0.6935913

``` r
fun2alt <- function(x) {
  idx <- max.col(t(x))
  x[cbind(idx,1:4)]
}

fun2alt(M)
```

    ## [1] 0.6222994 0.8609154 0.6660838 0.6935913

``` r
# Benchmarking
microbenchmark::microbenchmark(
  fun2(x),
  fun2alt(x)
)
```

    ## Unit: microseconds
    ##        expr     min       lq      mean   median       uq      max neval cld
    ##     fun2(x) 509.876 538.9655 628.80347 566.5790 620.0225 4752.966   100   b
    ##  fun2alt(x)  70.643  76.0550  91.07986  78.6585  81.9385 1169.115   100  a

# Problem 3: Parallelize everything

We will now turn our attention to non-parametric bootstrapping. Among
its many uses, non-parametric bootstrapping allow us to obtain
confidence intervals for parameter estimates without relying on
parametric assumptions.

The main assumption is that we can approximate many experiments by
resampling observations from our original dataset, which reflects the
population.

This function implements the non-parametric bootstrap:

``` r
my_boot <- function(dat, stat, R, ncpus = 1L) {
  
  # Getting the random indices
  n <- nrow(dat)
  idx <- matrix(sample.int(n, n*R, TRUE), nrow=n, ncol=R)
 
  # Making the cluster using `ncpus`
  cl <- makePSOCKcluster(4)   
  clusterSetRNGStream(cl, 123) # Equivalent to `set.seed(123)`
  # STEP 2: GOES HERE
  clusterExport(cl, c("stat","dat","idx"),envir = environment())

  
    # STEP 3: THIS FUNCTION NEEDS TO BE REPLACES WITH parLapply
  ans <- parLapply(cl,seq_len(R), function(i) {
    stat(dat[idx[,i], , drop=FALSE])
  })
  # ans <- parApply(
  # cl     = cl,
  # X      = X,
  # MARGIN = 2,
  # FUN    = function(x) coef(lm(y ~ x))
  # )
  # 
  # Coercing the list into a matrix
  ans <- do.call(rbind, ans)
  
  # STEP 4: GOES HERE
  ans
}
```

1.  Use the previous pseudocode, and make it work with parallel. Here is
    just an example for you to try:

``` r
# Bootstrap of an OLS
my_stat <- function(d) coef(lm(y ~ x, data=d))

# DATA SIM
set.seed(1)
n <- 500; R <- 1e4

x <- cbind(rnorm(n)); y <- x*5 + rnorm(n)

# Checking if we get something similar as lm
ans0 <- confint(lm(y~x))
ans1 <- my_boot(dat = data.frame(x, y), my_stat, R = R, ncpus = 2L)

# You should get something like this
t(apply(ans1, 2, quantile, c(.025,.975)))
```

    ##                   2.5%      97.5%
    ## (Intercept) -0.1386903 0.04856752
    ## x            4.8685162 5.04351239

``` r
##                   2.5%      97.5%
## (Intercept) -0.1372435 0.05074397
## x            4.8680977 5.04539763
ans0
```

    ##                  2.5 %     97.5 %
    ## (Intercept) -0.1379033 0.04797344
    ## x            4.8650100 5.04883353

``` r
##                  2.5 %     97.5 %
## (Intercept) -0.1379033 0.04797344
## x            4.8650100 5.04883353
```

2.  Check whether your version actually goes faster than the
    non-parallel version:

``` r
system.time(my_boot(dat = data.frame(x, y), my_stat, R = 4000, ncpus = 1L))
```

    ##    user  system elapsed 
    ##   0.080   0.023   0.562

``` r
system.time(my_boot(dat = data.frame(x, y), my_stat, R = 4000, ncpus = 2L))
```

    ##    user  system elapsed 
    ##   0.078   0.021   0.560
