---
title: "Chap13_Jan31_2020_HW_Kazu"
author: "Kazu"
date: "1/31/2020"
output: 
  html_document: 
    keep_md: yes
editor_options: 
  chunk_output_type: console
---

# 13. Models with memory


Please find attached csv.  This csv has measurements of tomato internodes and petioles from wildtype (Moneymaker) and various phytochrome mutant lines.  Measurements were made at 3 time points, 21, 28, and 35 days after germination under two treatments, simulated sun and simulated shade.

for today let's focus on day 35.  Also let's focus on total stem length.  So: first

# Q1)
## a) subset the data for day 35

```r
F4phyE<-read_csv("../figure4phyE.csv")
```

```
## Parsed with column specification:
## cols(
##   genotype = col_character(),
##   treatment = col_character(),
##   flat = col_double(),
##   day = col_double(),
##   epi = col_double(),
##   int1 = col_double(),
##   int2 = col_double(),
##   int3 = col_double(),
##   pet1 = col_double(),
##   pet2 = col_double(),
##   pet3 = col_double(),
##   pet4 = col_double()
## )
```

```r
summary(F4phyE)
```

```
##    genotype          treatment              flat            day    
##  Length:264         Length:264         Min.   :1.000   Min.   :21  
##  Class :character   Class :character   1st Qu.:2.000   1st Qu.:21  
##  Mode  :character   Mode  :character   Median :3.000   Median :28  
##                                        Mean   :3.455   Mean   :28  
##                                        3rd Qu.:5.000   3rd Qu.:35  
##                                        Max.   :6.000   Max.   :35  
##       epi             int1             int2             int3       
##  Min.   : 2.73   Min.   : 0.240   Min.   :  0.00   Min.   : 0.000  
##  1st Qu.:11.42   1st Qu.: 2.748   1st Qu.:  0.00   1st Qu.: 0.000  
##  Median :24.59   Median :14.635   Median :  7.58   Median : 2.755  
##  Mean   :28.00   Mean   :18.643   Mean   : 16.03   Mean   : 9.289  
##  3rd Qu.:37.48   3rd Qu.:30.532   3rd Qu.: 25.41   3rd Qu.:15.880  
##  Max.   :87.74   Max.   :71.270   Max.   :104.74   Max.   :56.700  
##       pet1            pet2             pet3            pet4       
##  Min.   : 6.23   Min.   : 0.000   Min.   : 0.00   Min.   : 0.000  
##  1st Qu.:15.19   1st Qu.: 5.577   1st Qu.: 0.00   1st Qu.: 0.000  
##  Median :38.78   Median :28.250   Median :13.19   Median : 3.455  
##  Mean   :37.97   Mean   :32.788   Mean   :24.59   Mean   :13.569  
##  3rd Qu.:52.47   3rd Qu.:53.197   3rd Qu.:48.82   3rd Qu.:27.302  
##  Max.   :91.87   Max.   :96.230   Max.   :98.57   Max.   :72.120
```

```r
F4phyE.day35 <- F4phyE %>% filter(day==35)
summary(F4phyE.day35)
```

```
##    genotype          treatment              flat            day    
##  Length:88          Length:88          Min.   :1.000   Min.   :35  
##  Class :character   Class :character   1st Qu.:2.000   1st Qu.:35  
##  Mode  :character   Mode  :character   Median :3.000   Median :35  
##                                        Mean   :3.455   Mean   :35  
##                                        3rd Qu.:5.000   3rd Qu.:35  
##                                        Max.   :6.000   Max.   :35  
##       epi             int1            int2             int3      
##  Min.   :10.94   Min.   : 3.02   Min.   : 10.75   Min.   : 8.79  
##  1st Qu.:29.08   1st Qu.:28.01   1st Qu.: 23.95   1st Qu.:15.98  
##  Median :37.36   Median :35.12   Median : 36.20   Median :24.72  
##  Mean   :41.40   Mean   :36.50   Mean   : 38.50   Mean   :24.85  
##  3rd Qu.:50.62   3rd Qu.:45.85   3rd Qu.: 48.99   3rd Qu.:31.67  
##  Max.   :87.74   Max.   :71.27   Max.   :104.74   Max.   :56.70  
##       pet1            pet2            pet3            pet4      
##  Min.   :32.52   Min.   : 0.00   Min.   :18.52   Min.   :10.83  
##  1st Qu.:51.14   1st Qu.:53.29   1st Qu.:49.33   1st Qu.:27.41  
##  Median :60.62   Median :65.34   Median :58.30   Median :34.99  
##  Mean   :60.26   Mean   :65.52   Mean   :60.12   Mean   :36.29  
##  3rd Qu.:68.91   3rd Qu.:79.04   3rd Qu.:71.24   3rd Qu.:43.87  
##  Max.   :91.87   Max.   :96.23   Max.   :98.57   Max.   :72.12
```

## b) create a new column "stem_length" that is the sum of epi, int1, int2, and int3

```r
F4phyE.day35.v2 <- F4phyE.day35 %>% mutate(stem_length=epi+int1+int2+int3) # %>% View()
F4phyE.day35.v2
```

```
## # A tibble: 88 x 13
##    genotype treatment  flat   day   epi  int1  int2  int3  pet1  pet2  pet3
##    <chr>    <chr>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
##  1 phyB1/B2 shade         1    35  58.8 40.6   72.3  52.7  42.2  49.6  49.6
##  2 phyB1/B2 shade         1    35  69.6  4.91  56.6  35.5  49.8  34.6  47.6
##  3 phyB1/B2 shade         2    35  70.2 62.6   59.7  31.2  46.3  56.8  47.9
##  4 phyB1/B2 shade         3    35  54.6 38.6   67.6  35.2  39.1  66.6  68.8
##  5 phyB1/B2 shade         3    35  58.2 55.3  105.   40.0  44.8  65.3  60.6
##  6 phyB1/B2 shade         5    35  59.9 58.7   74.8  33.3  36.4  39.3  50.7
##  7 phyB1/B2 sun           1    35  37.4 28.1   32.3  15.4  32.5  39.8  45.4
##  8 phyB1/B2 sun           1    35  66.5 36.1   25.8  19.1  50.5  49.3  49.6
##  9 phyB1/B2 sun           2    35  44.0  3.02  37.0  12.2  36.1  40.6  29.9
## 10 phyB1/B2 sun           3    35  67.7 41.2   30.4  20.6  45.8  53.2  44.3
## # … with 78 more rows, and 2 more variables: pet4 <dbl>, stem_length <dbl>
```

c) although flats are listed as 1-6, flats in sun and shade are separate. Create a new column "flat2" that corrects for this.

```r
F4phyE.day35.v3 <- F4phyE.day35.v2 %>% unite("flat2",c("treatment","flat"),sep="_", remove=FALSE)
F4phyE.day35.v3
```

```
## # A tibble: 88 x 14
##    genotype flat2 treatment  flat   day   epi  int1  int2  int3  pet1  pet2
##    <chr>    <chr> <chr>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
##  1 phyB1/B2 shad… shade         1    35  58.8 40.6   72.3  52.7  42.2  49.6
##  2 phyB1/B2 shad… shade         1    35  69.6  4.91  56.6  35.5  49.8  34.6
##  3 phyB1/B2 shad… shade         2    35  70.2 62.6   59.7  31.2  46.3  56.8
##  4 phyB1/B2 shad… shade         3    35  54.6 38.6   67.6  35.2  39.1  66.6
##  5 phyB1/B2 shad… shade         3    35  58.2 55.3  105.   40.0  44.8  65.3
##  6 phyB1/B2 shad… shade         5    35  59.9 58.7   74.8  33.3  36.4  39.3
##  7 phyB1/B2 sun_1 sun           1    35  37.4 28.1   32.3  15.4  32.5  39.8
##  8 phyB1/B2 sun_1 sun           1    35  66.5 36.1   25.8  19.1  50.5  49.3
##  9 phyB1/B2 sun_2 sun           2    35  44.0  3.02  37.0  12.2  36.1  40.6
## 10 phyB1/B2 sun_3 sun           3    35  67.7 41.2   30.4  20.6  45.8  53.2
## # … with 78 more rows, and 3 more variables: pet3 <dbl>, pet4 <dbl>,
## #   stem_length <dbl>
```


Ultimately you want to know if any of the mutants have a different length from Moneymaker, in sun or in shade, or if the response to shade differs.

```r
dat_list.Q1error<- list(
    stem_length = F4phyE.day35.v3$stem_length,
    genotype = as.integer(factor(F4phyE.day35.v3$genotype,levels=c("Moneymaker", 
                                            "phyB1", 
                                            "phyB2", 
                                            "phyB1/B2", 
                                            "phyEami3", 
                                            "phyEami7"))),
    flat_id = as.integer(factor(F4phyE.day35.v3$flat2,levels=c("shade_1",
                                                            "shade_2",
                                                            "shade_3",
                                                            "shade_4",
                                                            "shade_5",
                                                            "shade_6",
                                                            "sun_1",
                                                            "sun_2",
                                                            "sun_3",
                                                            "sun_4",
                                                            "sun_5",
                                                            "sun_6"))),
    treatment = ifelse(F4phyE.day35.v3$treatment=="sun",0,1)) 
dat_list.Q1<- list(
    stem_length = F4phyE.day35.v3$stem_length,
    genotype = as.integer(factor(F4phyE.day35.v3$genotype,levels=c("Moneymaker", 
                                            "phyB1", 
                                            "phyB2", 
                                            "phyB1/B2", 
                                            "phyEami3", 
                                            "phyEami7"))),
    flat_id = as.integer(factor(F4phyE.day35.v3$flat2,levels=c("shade_1",
                                                            "shade_2",
                                                            "shade_3",
                                                            "shade_4",
                                                            "shade_5",
                                                            "shade_6",
                                                            "sun_1",
                                                            "sun_2",
                                                            "sun_3",
                                                            "sun_4",
                                                            "sun_5",
                                                            "sun_6"))),
    treatment = ifelse(F4phyE.day35.v3$treatment=="sun",1,2)) 
# "sun" is 1, "shade" is 2
dat_list.Q1
```

```
## $stem_length
##  [1] 224.42 166.62 223.74 196.05 258.18 226.65 113.30 147.57  96.18 159.95
## [11] 166.98 172.89  87.43 145.75 190.81 149.81 166.10 175.33  84.03  84.03
## [21] 100.18 117.38  74.19  92.12 128.97 108.65 172.52 162.98 148.48 117.70
## [31]  98.14  94.51  71.72 134.57 110.30 163.71 173.64 125.55 146.97  87.37
## [41]  92.29  51.71 143.75 163.07 166.23 148.02 147.45 172.36  90.35  86.28
## [51]  77.33  91.59 215.01 217.62 264.39 254.22 222.57 200.79 190.93 156.71
## [61] 173.01 200.15 153.91 127.99 158.61 190.66 157.54 197.49 120.22  99.41
## [71] 108.67 102.93 101.29  97.56  64.59  85.01 163.40 151.09 138.83 158.88
## [81] 137.04 166.08  92.17  92.26  96.14  92.84  88.03  95.55
## 
## $genotype
##  [1] 4 4 4 4 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 6 6 6
## [39] 6 6 6 6 6 6 6 6 6 6 6 6 6 6 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3
## [77] 1 1 1 1 1 1 1 1 1 1 1 1
## 
## $flat_id
##  [1]  1  1  2  3  3  5  7  7  8  9  9 11  1  2  5  5  6  6  7  8 11 11 12 12  1
## [26]  1  4  5  5  6  7  7 12  2  2  3  4  5  6  8 10 12  1  2  3  3  4  6  7  9
## [51]  9 10  2  4  4  4  5  6  8 10 10 10 11 12  1  3  4  5  6  6  7  9 10 11 12
## [76] 12  1  2  2  3  3  4  7  8  8  9  9 10
## 
## $treatment
##  [1] 2 2 2 2 2 2 1 1 1 1 1 1 2 2 2 2 2 2 1 1 1 1 1 1 2 2 2 2 2 2 1 1 1 2 2 2 2 2
## [39] 2 1 1 1 2 2 2 2 2 2 1 1 1 1 2 2 2 2 2 2 1 1 1 1 1 1 2 2 2 2 2 2 1 1 1 1 1 1
## [77] 2 2 2 2 2 2 1 1 1 1 1 1
```

```r
str(dat_list.Q1error)
```

```
## List of 4
##  $ stem_length: num [1:88] 224 167 224 196 258 ...
##  $ genotype   : int [1:88] 4 4 4 4 4 4 4 4 4 4 ...
##  $ flat_id    : int [1:88] 1 1 2 3 3 5 7 7 8 9 ...
##  $ treatment  : num [1:88] 1 1 1 1 1 1 0 0 0 0 ...
```

```r
str(dat_list.Q1)
```

```
## List of 4
##  $ stem_length: num [1:88] 224 167 224 196 258 ...
##  $ genotype   : int [1:88] 4 4 4 4 4 4 4 4 4 4 ...
##  $ flat_id    : int [1:88] 1 1 2 3 3 5 7 7 8 9 ...
##  $ treatment  : num [1:88] 2 2 2 2 2 2 1 1 1 1 ...
```

# look the raw data

```r
str(F4phyE.day35.v3)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	88 obs. of  14 variables:
##  $ genotype   : chr  "phyB1/B2" "phyB1/B2" "phyB1/B2" "phyB1/B2" ...
##  $ flat2      : chr  "shade_1" "shade_1" "shade_2" "shade_3" ...
##  $ treatment  : chr  "shade" "shade" "shade" "shade" ...
##  $ flat       : num  1 1 2 3 3 5 1 1 2 3 ...
##  $ day        : num  35 35 35 35 35 35 35 35 35 35 ...
##  $ epi        : num  58.8 69.5 70.2 54.6 58.2 ...
##  $ int1       : num  40.6 4.91 62.61 38.64 55.31 ...
##  $ int2       : num  72.3 56.6 59.7 67.6 104.7 ...
##  $ int3       : num  52.7 35.5 31.2 35.2 40 ...
##  $ pet1       : num  42.1 49.8 46.3 39.1 44.9 ...
##  $ pet2       : num  49.6 34.6 56.8 66.6 65.3 ...
##  $ pet3       : num  49.6 47.6 47.9 68.8 60.6 ...
##  $ pet4       : num  30.5 40.5 28.4 53.2 43.5 ...
##  $ stem_length: num  224 167 224 196 258 ...
```

```r
F4phyE.day35.v3$treatment <-factor(F4phyE.day35.v3$treatment,levels=c("sun","shade"))
F4phyE.day35.v3 %>% View()
F4phyE.day35.v3 %>% ggplot(aes(x=treatment, y=stem_length,color=treatment)) + geom_jitter() + facet_grid(.~genotype)
```

![](Chap13_Jan31_2020_HW_Kazu_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
* Looks there are treatment effects are genotype effects.

# Q2) Fit 3 models, all of which include genotype and treatment
a) do not include flat
## ModelQ1zero
$$
stem_length_i \sim Normal(mu,sigma) \\
mu = \theta + \alpha_{genotype} + \beta_{treatment} \\
Kazu'Q: When\ logit\ should\ be\ used? \\
\theta \sim Normal(0,1) \\
\alpha_{group} \sim Normal(0, 1) \\
\beta_{treatment} \sim Normal(0, 1)
$$

## ModelQ1a
$$
stem_length_i \sim Normal(0,p) \\
logit(p) = \alpha_{genotype} + \beta_{treatment} \\
Kazu'Q: When\ logit\ should\ be\ used? \\

\alpha_{group} \sim Normal(0, 1) \\
\beta_{treatment} \sim Normal(0, 1)
$$



```r
set.seed(15)
# error
mQ1.a.error <- ulam(
    alist(
        stem_length ~ dnorm( 1 , p),
        logit(p) <- a[genotype] + b[treatment],
        b[treatment] ~ dnorm( 0 , 1),
        a[genotype] ~ dnorm(0 , 1)
    ), data=dat_list.Q1error , chains=2 , cores=2 , log_lik=TRUE )
```

```
## Warning in .local(object, ...): some chains had errors; consider specifying
## chains = 1 to debug
```

```
## here are whatever error messages were returned
```

```
## [[1]]
## Stan model '0b1010f6d8f8a8e3bb14c44c4539ee4c' does not contain samples.
## 
## [[2]]
## Stan model '0b1010f6d8f8a8e3bb14c44c4539ee4c' does not contain samples.
## 
## Stan model '0b1010f6d8f8a8e3bb14c44c4539ee4c' does not contain samples.
```

```
## Error in validObject(.Object): invalid class "ulam" object: invalid object for slot "coef" in class "ulam": got class "NULL", should be or extend class "numeric"
```

```r
# no error, why?
mQ1.a <- ulam(
    alist(
        stem_length ~ dnorm( 1 , p),
        logit(p) <- a[genotype] + b[treatment],
        b[treatment] ~ dnorm( 0 , 1),
        a[genotype] ~ dnorm(0 , 1)
    ), data=dat_list.Q1 , chains=2 , cores=2 , log_lik=TRUE )
```

```
## recompiling to avoid crashing R session
```

```r
# 
mQ1.a2.error <- ulam(
    alist(
        stem_length ~ dnorm( 1 , p),
        logit(p) <- a*genotype + b*treatment,
        b ~ dnorm( 0 , 1),
        a ~ dnorm(0 , 1)
    ), data=dat_list.Q1error , chains=2 , cores=2 , log_lik=TRUE )
# Do I need to have intercept? 


coeftab(mQ1.a)
```

```
##      mQ1.a  
## b[1]    8.28
## b[2]    9.21
## a[1]    2.51
## a[2]    3.26
## a[3]    2.53
## a[4]    3.11
## a[5]    2.87
## a[6]    2.81
## nobs      88
```

```r
coeftab(mQ1.a2.error)
```

```
##      mQ1.a2.error
## b       2.26     
## a          9     
## nobs      88
```

```r
compare(mQ1.a, mQ1.a2.error) # why different?
```

```
##                 WAIC       SE    dWAIC      dSE    pWAIC       weight
## mQ1.a2.error 1935730 139098.4  0.00000       NA 1.139618 0.9998514926
## mQ1.a        1935748 139100.7 17.62945 5.943925 1.326543 0.0001485074
```

b) include flat without pooling
* I feel I do understand what pooling is meant....

```r
set.seed(15)
mQ1.b <- ulam(
    alist(
        stem_length ~ dnorm( 1 , p),
        logit(p) <- a[genotype] + g[flat_id] + b[treatment],
        b[treatment] ~ dnorm( 0 , sigma_b ),
        a[genotype] ~ dnorm( a_bar , sigma_a ),
        g[flat_id] ~ dnorm( 0 , 1 ),
        a_bar ~ dnorm( 0 , 1.5 ),
        sigma_a ~ dexp(1),
        sigma_b ~ dexp(1)
    ), data=dat_list.Q1 , chains=2 , cores=2 , log_lik=TRUE )
```

```
## Warning: There were 20 divergent transitions after warmup. Increasing adapt_delta above 0.95 may help. See
## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
```

```
## Warning: Examine the pairs() plot to diagnose sampling problems
```

```
## Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#bulk-ess
```

```r
coeftab(mQ1.b)
```

```
##         mQ1.b  
## b[1]       16.5
## b[2]       17.1
## a[1]       1.35
## a[2]       1.47
## a[3]       1.32
## a[4]       1.43
## a[5]       1.36
## a[6]       1.37
## g[1]       0.07
## g[2]       0.03
## g[3]       0.05
## g[4]       0.04
## g[5]       0.08
## g[6]       0.07
## g[7]       0.09
## g[8]       0.04
## g[9]       0.06
## g[10]      0.11
## g[11]       0.1
## g[12]      0.06
## a_bar      1.19
## sigma_a    0.99
## sigma_b    8.21
## nobs         88
```

c) use a hierarchical model that allows partial pooling across flats

```r
set.seed(15)
mQ1.c <- ulam(
    alist(
        stem_length ~ dnorm( 1 , p),
        logit(p) <- a[genotype] + g[flat_id] + b[treatment],
        b[treatment] ~ dnorm( 0 , sigma_b ),
        a[genotype] ~ dnorm( a_bar , sigma_a ),
        g[flat_id] ~ dnorm( 0 , sigma_g ),
        a_bar ~ dnorm( 0 , 1.5 ),
        sigma_a ~ dexp(1),
        sigma_g ~ dexp(1),
        sigma_b ~ dexp(1)
    ), data=dat_list.Q1 , chains=2 , cores=2 , log_lik=TRUE )
```

```
## Warning: There were 71 divergent transitions after warmup. Increasing adapt_delta above 0.95 may help. See
## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
```

```
## Warning: There were 2 chains where the estimated Bayesian Fraction of Missing Information was low. See
## http://mc-stan.org/misc/warnings.html#bfmi-low
```

```
## Warning: Examine the pairs() plot to diagnose sampling problems
```

```
## Warning: The largest R-hat is 1.09, indicating chains have not mixed.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#r-hat
```

```
## Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#bulk-ess
```

```
## Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#tail-ess
```

```r
coeftab(mQ1.c)
```

```
##         mQ1.c  
## b[1]      16.43
## b[2]      17.37
## a[1]       1.34
## a[2]       1.36
## a[3]       1.33
## a[4]       1.39
## a[5]       1.36
## a[6]       1.35
## g[1]       0.15
## g[2]       0.06
## g[3]       0.07
## g[4]       0.01
## g[5]       0.04
## g[6]       0.12
## g[7]      -0.05
## g[8]      -0.04
## g[9]       0.13
## g[10]      0.13
## g[11]      0.08
## g[12]     -0.03
## a_bar      1.28
## sigma_a    0.75
## sigma_g    0.84
## sigma_b    8.28
## nobs         88
```


# Q3) Compare the models, which is preferred?

```r
compare(mQ1.a,mQ1.b,mQ1.c)
```

```
##          WAIC       SE       dWAIC        dSE      pWAIC       weight
## mQ1.b 1935713 139099.8  0.00000000         NA 0.03872870 5.067722e-01
## mQ1.c 1935713 139099.8  0.05418134 0.02913863 0.03564936 4.932277e-01
## mQ1.a 1935748 139100.7 35.30536918 1.60223370 1.32654282 1.092319e-08
```

# Q4) Using the hierarchical model, make posterior predictions
a) for average cluster
b) for same clusters
c) showing the "marginal" from cluster
d) showing new clusters.

Q5) Reparameterize the model to help with divergent transitions (even if there aren't any)

Q6--optional)
a) Which genotypes differ from MoneyMaker in Sun conditions?
b) Which genotypes differ from MoneyMaker in Shade conditions?
c) Which genotypes differ from MoneyMaker in their response to shade (difference in sun vs shade)?

# sessionInfo()

```r
sessionInfo()
```

```
## R version 3.6.2 (2019-12-12)
## Platform: x86_64-apple-darwin15.6.0 (64-bit)
## Running under: macOS Mojave 10.14.6
## 
## Matrix products: default
## BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] parallel  stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
##  [1] forcats_0.4.0        stringr_1.4.0        dplyr_0.8.3         
##  [4] purrr_0.3.3          readr_1.3.1          tidyr_1.0.0         
##  [7] tibble_2.1.3         tidyverse_1.3.0      reshape2_1.4.3      
## [10] lmerTest_3.1-1       lme4_1.1-21          Matrix_1.2-18       
## [13] rethinking_1.93      dagitty_0.2-2        rstan_2.19.2        
## [16] ggplot2_3.2.1        StanHeaders_2.21.0-1
## 
## loaded via a namespace (and not attached):
##  [1] nlme_3.1-143        matrixStats_0.55.0  fs_1.3.1           
##  [4] lubridate_1.7.4     httr_1.4.1          numDeriv_2016.8-1.1
##  [7] tools_3.6.2         backports_1.1.5     utf8_1.1.4         
## [10] R6_2.4.1            DBI_1.1.0           lazyeval_0.2.2     
## [13] colorspace_1.4-1    withr_2.1.2         tidyselect_0.2.5   
## [16] gridExtra_2.3       prettyunits_1.1.0   processx_3.4.1     
## [19] curl_4.3            compiler_3.6.2      cli_2.0.1          
## [22] rvest_0.3.5         xml2_1.2.2          labeling_0.3       
## [25] scales_1.1.0        mvtnorm_1.0-12      callr_3.4.0        
## [28] digest_0.6.23       minqa_1.2.4         rmarkdown_2.1      
## [31] pkgconfig_2.0.3     htmltools_0.4.0     dbplyr_1.4.2       
## [34] rlang_0.4.2         readxl_1.3.1        rstudioapi_0.10    
## [37] farver_2.0.3        shape_1.4.4         generics_0.0.2     
## [40] jsonlite_1.6        inline_0.3.15       magrittr_1.5       
## [43] loo_2.2.0           Rcpp_1.0.3          munsell_0.5.0      
## [46] fansi_0.4.1         lifecycle_0.1.0     stringi_1.4.5      
## [49] yaml_2.2.0          MASS_7.3-51.5       pkgbuild_1.0.6     
## [52] plyr_1.8.5          grid_3.6.2          crayon_1.3.4       
## [55] lattice_0.20-38     haven_2.2.0         splines_3.6.2      
## [58] hms_0.5.3           zeallot_0.1.0       knitr_1.27         
## [61] ps_1.3.0            pillar_1.4.3        boot_1.3-24        
## [64] codetools_0.2-16    stats4_3.6.2        reprex_0.3.0       
## [67] glue_1.3.1          evaluate_0.14       V8_3.0             
## [70] modelr_0.1.5        vctrs_0.2.1         nloptr_1.2.1       
## [73] cellranger_1.1.0    gtable_0.3.0        assertthat_0.2.1   
## [76] xfun_0.12           broom_0.5.3         coda_0.19-3        
## [79] ellipsis_0.3.0
```

