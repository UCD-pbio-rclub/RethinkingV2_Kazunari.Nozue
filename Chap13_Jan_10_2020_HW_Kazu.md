---
title: "Chap13_Jan_10_2020"
author: "Kazu"
date: "1/10/2020"
output: 
  html_document: 
    keep_md: yes
editor_options: 
  chunk_output_type: console
---
# 13. Models with memory


# practices
# 12E1 
* Which of the following priors will produce more shrinkage in the estimates? (a) αtank ∼
Normal(0, 1); (b) αtank ∼ Normal(0, 2). Answer: (a)
* 
# 12E2 # learning how to draw math formula in [Rmarkdown/LaTeX](https://en.wikibooks.org/wiki/LaTeX/Mathematics)
* original
$$
y_i \sim Binomial(1, p_i) \\
logit(p_i) = \alpha_{group[i]} + \beta x_i \\
\alpha_{group} \sim Normal(0, 10) \\
\beta \sim Normal(0, 1)
$$

* multilevel 
* Is sigma necesary for exponential?
$$
y_i \sim Binomial(1, p_i) \\
logit(p_i) = \alpha_{group[i]} + \beta x_i \\
\alpha_{group} \sim Normal(\bar\alpha, \sigma) \\
\bar\alpha \sim Normal(0, 10) \\
\bar{\alpha} \sim Normal(0, 10) \dots with or without is ignored \\ 
\sigma \sim Exponential(1) \\ 
\beta \sim Normal(0, 1)
$$


# 12E3
* Half Cauchy distribution

```r
library(extraDistr)
```

```
## 
## Attaching package: 'extraDistr'
```

```
## The following object is masked from 'package:purrr':
## 
##     rdunif
```

```
## The following objects are masked from 'package:rethinking':
## 
##     dbern, dlaplace, dpareto, rbern, rlaplace, rpareto
```

```r
?extraDistr::dhcauchy
```

* original
$$
y_i \sim Normal(\mu_i, \sigma) \\
\mu_i = \alpha_{group[i]} + \beta x_i \\
\alpha_{group} \sim Normal(0, 10) \\
\beta \sim Normal(0, 1) \\
\sigma \sim HalfCauchy(0, 2)
$$

* multilevel
$$
y_i \sim Normal(\mu_i, \sigma) \\
\mu_i = \alpha_{group[i]} + \beta x_i \\
\alpha_{group} \sim Normal(\bar\alpha, \sigma_2) \\
\bar\alpha \sim Normal(0, 10) \\
\sigma_2 \sim Exponential(1) \\ 
\beta \sim Normal(0, 1) \\
\sigma \sim HalfCauchy(0, 2)
$$

# 12M1 
* Revisit the Reed frog survival data, data(reedfrogs), and add the predation and size treatment variables to the varying intercepts model. Consider models with either main effect alone, both main effects, as well as a model including both and their interaction. Instead of focusing on inferences about these two predictor variables, focus on the inferred variation across tanks. Explain why it changes as it does across models.

```r
data(reedfrogs) # 13.1. Example: Multilevel tadpoles
d <- reedfrogs
str(d)
```

```
## 'data.frame':	48 obs. of  5 variables:
##  $ density : int  10 10 10 10 10 10 10 10 10 10 ...
##  $ pred    : Factor w/ 2 levels "no","pred": 1 1 1 1 1 1 1 1 2 2 ...
##  $ size    : Factor w/ 2 levels "big","small": 1 1 1 1 2 2 2 2 1 1 ...
##  $ surv    : int  9 10 7 10 9 9 10 9 4 9 ...
##  $ propsurv: num  0.9 1 0.7 1 0.9 0.9 1 0.9 0.4 0.9 ...
```

```r
# make the tank cluster variable
d$tank <- 1:nrow(d)
dat_pred <- list(
    S = d$surv,
    N = d$density,
    tank = d$tank,
    pred = ifelse(d$pred == "no", 0, 1)
    )
dat_pred_size <- list(
    S = d$surv,
    N = d$density,
    tank = d$tank,
    pred = ifelse(d$pred == "no", 0, 1),
    size = ifelse(d$size == "small",0,1)
    )
```

# tank + pred

```r
m13.2.mod.tank.pred <- ulam(
    alist(
        S ~ dbinom( N , p ) ,
        logit(p) <- a[tank] + b*pred,
        a[tank] ~ dnorm( a_bar , a_sigma ) ,
        a_bar ~ dnorm( 0 , 1.5 ) ,
        a_sigma ~ dexp( 1 ),
        b ~ dnorm(0, 1)
), data=dat_pred , chains=4 , log_lik=TRUE ,cores=2) 
```

```
## Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
## Running the chains for more iterations may help. See
## http://mc-stan.org/misc/warnings.html#bulk-ess
```
* error .... why??? Do not use "a.sigma". Use "a_sigma".
* If I use data_pred_size there is an error: Why?
 <!-- SYNTAX ERROR, MESSAGE(S) FROM PARSER: -->
<!-- Variable identifier (name) may not be reserved word -->
<!--     found identifier=size -->
<!-- Variable identifier (name) may not be reserved word -->
<!--     found identifier=size -->
<!--  error in 'model9a66097c2c3_44191844a02ecf95246d72c40b264e24' at line 2, column 7 -->
<!--   ------------------------------------------------- -->
<!--      1: data{ -->
<!--      2:     int size[48]; -->
<!--               ^ -->
<!--      3:     int N[48]; -->
<!--   ------------------------------------------------- -->
<!-- PARSER EXPECTED: <identifier> -->
<!-- Error in stanc(file = file, model_code = model_code, model_name = model_name,  :  -->
<!--   failed to parse Stan model '44191844a02ecf95246d72c40b264e24' due to the above error. -->


```r
precis( m13.2.mod.tank.pred , depth=2 )
```

```
##               mean        sd        5.5%     94.5%     n_eff      Rhat
## a[1]     2.4878531 0.6946843  1.40908803  3.616526 1929.8785 1.0001159
## a[2]     2.9387411 0.7476956  1.78012822  4.140103 1838.7325 0.9986799
## a[3]     1.6963584 0.6222392  0.71364407  2.710271 1666.4165 1.0035206
## a[4]     2.9555714 0.7192675  1.85154236  4.123312 1745.3533 0.9992621
## a[5]     2.4812416 0.6869988  1.40093996  3.581690 1659.8628 0.9985846
## a[6]     2.4399891 0.6888500  1.37591719  3.557012 1862.3065 1.0011646
## a[7]     2.9080286 0.7376659  1.76965297  4.101514 1887.0148 0.9990382
## a[8]     2.4654012 0.6661025  1.43834804  3.556165 1645.1319 1.0025319
## a[9]     2.1725859 0.5751599  1.24065088  3.052987  894.5028 1.0054815
## a[10]    3.5398949 0.6303201  2.54465541  4.541257  827.6570 1.0032734
## a[11]    2.9635426 0.5712430  2.09441414  3.908426 1105.0417 1.0008811
## a[12]    2.7053154 0.5716034  1.81747064  3.649514  905.6668 1.0041692
## a[13]    2.9618703 0.5589404  2.10812128  3.864507 1043.7844 1.0024696
## a[14]    2.4395664 0.5630991  1.54319842  3.335294  768.4703 1.0077175
## a[15]    3.5537812 0.6234557  2.61055935  4.559767  937.6739 1.0025268
## a[16]    3.5183724 0.6110310  2.59843789  4.531993 1081.1937 0.9995002
## a[17]    2.8756395 0.6241736  1.92810433  3.908075 1728.9434 0.9998974
## a[18]    2.5291950 0.5368135  1.69387106  3.448509 1963.8180 1.0037556
## a[19]    2.2519842 0.5283499  1.44090781  3.123076 1920.3548 1.0011899
## a[20]    3.2518110 0.6409357  2.30936826  4.322195 1549.2944 0.9995792
## a[21]    2.5382427 0.5662223  1.67838884  3.477953 2182.2969 0.9993422
## a[22]    2.5490104 0.5636888  1.68731045  3.456136 1893.0646 0.9993886
## a[23]    2.5333984 0.5708450  1.66968232  3.500134 1879.3049 0.9993334
## a[24]    2.0108206 0.5003864  1.22009939  2.834569 2048.3689 1.0003696
## a[25]    1.5230731 0.4830569  0.73590703  2.248621  548.0587 1.0102386
## a[26]    2.4814434 0.4403478  1.76082592  3.161000  521.4887 1.0066136
## a[27]    1.2054315 0.5020006  0.36836516  2.003036  583.9821 1.0083310
## a[28]    1.9432211 0.4544730  1.20413276  2.654834  571.6408 1.0073309
## a[29]    2.4867350 0.4413629  1.77892132  3.196627  456.8797 1.0098852
## a[30]    3.4839678 0.4796026  2.73361227  4.259487  781.0056 1.0028914
## a[31]    1.8042658 0.4759544  1.04152474  2.557510  582.8058 1.0071016
## a[32]    2.0873135 0.4393114  1.38524766  2.801449  519.3857 1.0065922
## a[33]    3.0767284 0.6036083  2.17813976  4.034863 1553.6267 0.9994317
## a[34]    2.7414524 0.5607856  1.88494121  3.709433 1883.9516 1.0003169
## a[35]    2.7544360 0.5412192  1.95111796  3.656682 1651.7512 0.9996288
## a[36]    2.2503003 0.4606751  1.53217446  3.018376 2327.1264 0.9989268
## a[37]    2.2569310 0.4837351  1.50199488  3.069660 2552.0583 0.9995915
## a[38]    3.3976474 0.6555647  2.43252829  4.515272 1729.3091 0.9994248
## a[39]    2.7539603 0.5485566  1.91693666  3.662716 2240.2529 0.9996078
## a[40]    2.4828526 0.5131766  1.72306126  3.343067 1997.6057 0.9993040
## a[41]    0.8747691 0.4884619  0.06746822  1.608733  532.9914 1.0088189
## a[42]    1.8679026 0.4132433  1.19524358  2.502858  445.9535 1.0101191
## a[43]    1.9599705 0.4168222  1.25651608  2.612940  399.9916 1.0119886
## a[44]    2.0631744 0.4104986  1.38235891  2.712025  484.7316 1.0059272
## a[45]    2.8554649 0.4068435  2.19350511  3.494830  447.0322 1.0079486
## a[46]    1.8583259 0.4175077  1.18371594  2.504132  482.8860 1.0073997
## a[47]    3.9617111 0.4709211  3.23106533  4.734763  715.6596 1.0052003
## a[48]    2.3635649 0.4095862  1.70336634  2.994516  478.8076 1.0087284
## a_bar    2.5099122 0.2273638  2.14731916  2.865019  285.0365 1.0148029
## a_sigma  0.8238033 0.1410897  0.61913222  1.062922  657.4144 1.0046104
## b       -2.3922846 0.2823657 -2.83192804 -1.913359  224.1248 1.0204269
```


# tank + pred + size

```r
m13.2.mod.tank.pred.size <- ulam(
    alist(
        S ~ dbinom( N , p ) ,
        logit(p) <- a[tank] + b*pred + c*size,
        a[tank] ~ dnorm( a_bar , a_sigma ) ,
        a_bar ~ dnorm( 0 , 1.5 ) ,
        a_sigma ~ dexp( 1 ),
        b ~ dnorm(0, 1),
        c ~ dnorm(0, 1)
), data=dat_pred_size , chains=4 , log_lik=TRUE ,cores=2) 
```

```
## SYNTAX ERROR, MESSAGE(S) FROM PARSER:
```

```
## Variable identifier (name) may not be reserved word
```

```
##     found identifier=size
```

```
## Variable identifier (name) may not be reserved word
```

```
##     found identifier=size
```

```
##  error in 'model1ad414b68310_c52531a27cfa94b43489ea4a241b6bdc' at line 4, column 7
```

```
##   -------------------------------------------------
```

```
##      2:     int N[48];
```

```
##      3:     int S[48];
```

```
##      4:     int size[48];
```

```
##               ^
```

```
##      5:     int pred[48];
```

```
##   -------------------------------------------------
```

```
## 
```

```
## PARSER EXPECTED: <identifier>
```

```
## Error in stanc(file = file, model_code = model_code, model_name = model_name, : failed to parse Stan model 'c52531a27cfa94b43489ea4a241b6bdc' due to the above error.
```
* error


```r
precis( m13.2.mod.tank.pred.size , depth=2 )
```

```
## Error in precis(m13.2.mod.tank.pred.size, depth = 2): object 'm13.2.mod.tank.pred.size' not found
```



* I forgot to how to deal with interaction terms...


# 12M2
# 12H1






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
##  [1] extraDistr_1.8.11  forcats_0.4.0      stringr_1.4.0      dplyr_0.8.3       
##  [5] purrr_0.3.3        readr_1.3.1        tidyr_1.0.0        tibble_2.1.3      
##  [9] tidyverse_1.3.0    reshape2_1.4.3     lmerTest_3.1-1     lme4_1.1-21       
## [13] Matrix_1.2-18      rethinking_1.93    dagitty_0.2-2      rstan_2.19.2      
## [17] ggplot2_3.2.1      StanHeaders_2.19.0
## 
## loaded via a namespace (and not attached):
##  [1] nlme_3.1-143        matrixStats_0.55.0  fs_1.3.1           
##  [4] lubridate_1.7.4     httr_1.4.1          numDeriv_2016.8-1.1
##  [7] tools_3.6.2         backports_1.1.5     R6_2.4.1           
## [10] DBI_1.1.0           lazyeval_0.2.2      colorspace_1.4-1   
## [13] withr_2.1.2         tidyselect_0.2.5    gridExtra_2.3      
## [16] prettyunits_1.1.0   processx_3.4.1      curl_4.3           
## [19] compiler_3.6.2      cli_2.0.1           rvest_0.3.5        
## [22] xml2_1.2.2          scales_1.1.0        mvtnorm_1.0-11     
## [25] callr_3.4.0         digest_0.6.23       minqa_1.2.4        
## [28] rmarkdown_2.0       pkgconfig_2.0.3     htmltools_0.4.0    
## [31] dbplyr_1.4.2        rlang_0.4.2         readxl_1.3.1       
## [34] rstudioapi_0.10     shape_1.4.4         generics_0.0.2     
## [37] jsonlite_1.6        inline_0.3.15       magrittr_1.5       
## [40] loo_2.2.0           Rcpp_1.0.3          munsell_0.5.0      
## [43] fansi_0.4.0         lifecycle_0.1.0     stringi_1.4.3      
## [46] yaml_2.2.0          MASS_7.3-51.5       pkgbuild_1.0.6     
## [49] plyr_1.8.5          grid_3.6.2          crayon_1.3.4       
## [52] lattice_0.20-38     haven_2.2.0         splines_3.6.2      
## [55] hms_0.5.2           zeallot_0.1.0       knitr_1.26         
## [58] ps_1.3.0            pillar_1.4.3        boot_1.3-24        
## [61] codetools_0.2-16    stats4_3.6.2        reprex_0.3.0       
## [64] glue_1.3.1          evaluate_0.14       V8_2.3             
## [67] modelr_0.1.5        vctrs_0.2.1         nloptr_1.2.1       
## [70] cellranger_1.1.0    gtable_0.3.0        assertthat_0.2.1   
## [73] xfun_0.11           broom_0.5.3         coda_0.19-3
```

