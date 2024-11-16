Iteration
================
Shayne Estill (ske2118)

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## Here’s some lists

``` r
l = list(
  vec_numeric = 1:4,
  unif_sample = runif(100),
  mat = matrix(1:8, nrow = 2, ncol = 4, byrow = TRUE),
  summary = summary(rnorm(1000))
)

l
```

    ## $vec_numeric
    ## [1] 1 2 3 4
    ## 
    ## $unif_sample
    ##   [1] 0.2654789099 0.5846678526 0.2876889857 0.1016927680 0.8089558182
    ##   [6] 0.4511255182 0.1937055932 0.2105193199 0.0146497670 0.4257432672
    ##  [11] 0.7840728879 0.1751582092 0.2661273871 0.0597438272 0.2431670018
    ##  [16] 0.1219088451 0.3554501159 0.4045825670 0.7550282355 0.2960556000
    ##  [21] 0.6418598141 0.6032071428 0.8076509435 0.4287739899 0.5755561029
    ##  [26] 0.0009108286 0.6388227490 0.8686777956 0.3412260872 0.6782401141
    ##  [31] 0.1570311990 0.5315568573 0.8916475251 0.4329898180 0.9837852046
    ##  [36] 0.2869041837 0.8480880011 0.8836953859 0.6108645864 0.9268941565
    ##  [41] 0.5043700091 0.3060875621 0.6438625376 0.5482259062 0.2990433723
    ##  [46] 0.2418123195 0.7037338128 0.9346978068 0.4980688568 0.1213254759
    ##  [51] 0.7234639407 0.7052392086 0.1443216209 0.9995716074 0.0163098455
    ##  [56] 0.1887016406 0.3382371042 0.1071401606 0.2127841348 0.9019450853
    ##  [61] 0.1593600905 0.3736013114 0.7174561822 0.8683797074 0.4812646916
    ##  [66] 0.0804989554 0.7088118359 0.6781469884 0.2858239885 0.9201429638
    ##  [71] 0.8864257950 0.5246672735 0.7707479317 0.8378444146 0.4402894115
    ##  [76] 0.6522945731 0.4618890337 0.3351626752 0.7806060419 0.1027723297
    ##  [81] 0.3159106316 0.6029250245 0.9829527650 0.8580638668 0.7954119677
    ##  [86] 0.1022207821 0.9409610485 0.5711958283 0.4250477974 0.9157897688
    ##  [91] 0.2028626183 0.8044025686 0.1732829511 0.1389562583 0.5332048268
    ##  [96] 0.6803008763 0.5293857839 0.8345728514 0.1019267505 0.3730932018
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.73472 -0.69641 -0.02939 -0.04119  0.58328  3.24944

``` r
l$mat
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8

``` r
l[["mat"]][1,3]
```

    ## [1] 3

``` r
l[[1]]
```

    ## [1] 1 2 3 4

``` r
l[[4]]
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.73472 -0.69641 -0.02939 -0.04119  0.58328  3.24944

Make a list that’s hopefully a bit more useful.

``` r
list_norm = 
  list(
    a = rnorm(20, 0 , 5),
    b = rnorm(20, 4 , 5),
    c = rnorm(20, 0 , 10),
    d = rnorm(20, 4, 10)
  )

list_norm[["a"]]
```

    ##  [1] -2.9729214 -2.1988311 -6.6860091 -0.1046842  2.8328324  1.5110656
    ##  [7] -6.3114215 -0.1772095  3.8038100  3.4999284  2.7275850  2.4328341
    ## [13] -7.2725893 -2.6983912  4.2855197 -1.6178467  5.6001765 -4.1706232
    ## [19] -1.6468783 -2.8726213

Let’s reuse the function we wrote last time.

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  out_df = 
  tibble(
    mean = mean_x, 
    sd = sd_x
  )
  
  return(out_df)
}
```

Let’s use the function to take mean and sd of all samples.

``` r
mean_and_sd(list_norm[["a"]])
```

    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.602  3.85

``` r
mean_and_sd(list_norm[["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.11  4.51

``` r
mean_and_sd(list_norm[["c"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.78  9.08

``` r
mean_and_sd(list_norm[["d"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.218  10.1

\##Use a for loop

Create output list, and run a for loop

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])
  
}

output
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.602  3.85
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.11  4.51
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.78  9.08
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.218  10.1
