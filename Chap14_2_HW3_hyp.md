Chap14\_2\_HW
================
Kazu
3/13/2020

# test (my rstan had errors so I checked rstan with simple sample from book)

# multilevel tadpoles

``` r
data(reedfrogs)
d <- reedfrogs
str(d)
```

    ## 'data.frame':    48 obs. of  5 variables:
    ##  $ density : int  10 10 10 10 10 10 10 10 10 10 ...
    ##  $ pred    : Factor w/ 2 levels "no","pred": 1 1 1 1 1 1 1 1 2 2 ...
    ##  $ size    : Factor w/ 2 levels "big","small": 1 1 1 1 2 2 2 2 1 1 ...
    ##  $ surv    : int  9 10 7 10 9 9 10 9 4 9 ...
    ##  $ propsurv: num  0.9 1 0.7 1 0.9 0.9 1 0.9 0.4 0.9 ...

``` r
d
```

    ##    density pred  size surv  propsurv
    ## 1       10   no   big    9 0.9000000
    ## 2       10   no   big   10 1.0000000
    ## 3       10   no   big    7 0.7000000
    ## 4       10   no   big   10 1.0000000
    ## 5       10   no small    9 0.9000000
    ## 6       10   no small    9 0.9000000
    ## 7       10   no small   10 1.0000000
    ## 8       10   no small    9 0.9000000
    ## 9       10 pred   big    4 0.4000000
    ## 10      10 pred   big    9 0.9000000
    ## 11      10 pred   big    7 0.7000000
    ## 12      10 pred   big    6 0.6000000
    ## 13      10 pred small    7 0.7000000
    ## 14      10 pred small    5 0.5000000
    ## 15      10 pred small    9 0.9000000
    ## 16      10 pred small    9 0.9000000
    ## 17      25   no   big   24 0.9600000
    ## 18      25   no   big   23 0.9200000
    ## 19      25   no   big   22 0.8800000
    ## 20      25   no   big   25 1.0000000
    ## 21      25   no small   23 0.9200000
    ## 22      25   no small   23 0.9200000
    ## 23      25   no small   23 0.9200000
    ## 24      25   no small   21 0.8400000
    ## 25      25 pred   big    6 0.2400000
    ## 26      25 pred   big   13 0.5200000
    ## 27      25 pred   big    4 0.1600000
    ## 28      25 pred   big    9 0.3600000
    ## 29      25 pred small   13 0.5200000
    ## 30      25 pred small   20 0.8000000
    ## 31      25 pred small    8 0.3200000
    ## 32      25 pred small   10 0.4000000
    ## 33      35   no   big   34 0.9714286
    ## 34      35   no   big   33 0.9428571
    ## 35      35   no   big   33 0.9428571
    ## 36      35   no   big   31 0.8857143
    ## 37      35   no small   31 0.8857143
    ## 38      35   no small   35 1.0000000
    ## 39      35   no small   33 0.9428571
    ## 40      35   no small   32 0.9142857
    ## 41      35 pred   big    4 0.1142857
    ## 42      35 pred   big   12 0.3428571
    ## 43      35 pred   big   13 0.3714286
    ## 44      35 pred   big   14 0.4000000
    ## 45      35 pred small   22 0.6285714
    ## 46      35 pred small   12 0.3428571
    ## 47      35 pred small   31 0.8857143
    ## 48      35 pred small   17 0.4857143

``` r
# make the tank cluster variable
d$tank <- 1:nrow(d)
dat <- list(
    S = d$surv,
    N = d$density,
    tank = d$tank )
```

  - varying intercepts model (= simplelst kind of varying effects)

  
![
Si \\sim Bionial(N\_i,p\_i) \\\\
logit(p\_i) \\sim \\alpha\_{TANK\[i\]} \\\\
\\alpha\_j \\sim Normal(0,1.5),\\ for\\ j=1..48 \\\\
](https://latex.codecogs.com/png.latex?%0ASi%20%5Csim%20Bionial%28N_i%2Cp_i%29%20%5C%5C%0Alogit%28p_i%29%20%5Csim%20%5Calpha_%7BTANK%5Bi%5D%7D%20%5C%5C%0A%5Calpha_j%20%5Csim%20Normal%280%2C1.5%29%2C%5C%20for%5C%20j%3D1..48%20%5C%5C%0A
"
Si \\sim Bionial(N_i,p_i) \\\\
logit(p_i) \\sim \\alpha_{TANK[i]} \\\\
\\alpha_j \\sim Normal(0,1.5),\\ for\\ j=1..48 \\\\
")  

``` r
m13.1 <- ulam(
    alist(
        S ~ dbinom( N , p ),
        logit(p) <- a[tank],
        a[tank] ~ dnorm( 0 , 1.5 )
), data=dat, chains=4, log_lik=TRUE ,cores=2) # cores=2 added by Kazu
precis( m13.1 , depth=2 )
```

    ##              mean        sd         5.5%       94.5%    n_eff     Rhat4
    ## a[1]   1.71081821 0.7525575  0.552830768  2.99803508 3572.914 0.9983410
    ## a[2]   2.37592479 0.8837308  1.076126839  3.89899214 3271.823 0.9987780
    ## a[3]   0.75977601 0.6167351 -0.170595937  1.76499915 5057.672 0.9988798
    ## a[4]   2.38804474 0.8781259  1.109926415  3.84909134 4112.693 0.9983179
    ## a[5]   1.71146952 0.7725213  0.553331772  3.05727567 3253.439 0.9988874
    ## a[6]   1.73103976 0.7282668  0.596507006  2.96984627 3245.924 0.9994321
    ## a[7]   2.40129360 0.8676524  1.136737275  3.86536403 3912.320 1.0002132
    ## a[8]   1.71039572 0.8053275  0.482814748  3.03484000 3963.108 0.9984457
    ## a[9]  -0.37809590 0.6477166 -1.441418338  0.61074592 6547.246 0.9982631
    ## a[10]  1.70600626 0.7551563  0.561972908  2.98866346 3163.308 0.9987291
    ## a[11]  0.75947404 0.6003749 -0.162705576  1.73844181 3770.864 0.9991426
    ## a[12]  0.36403298 0.6141684 -0.603623174  1.37683282 4981.812 0.9990689
    ## a[13]  0.76129964 0.6635656 -0.249077038  1.85169031 4538.517 0.9991873
    ## a[14]  0.02131139 0.6252839 -0.957561137  1.05620414 5471.711 0.9983195
    ## a[15]  1.69390571 0.7681718  0.540244455  2.96715097 5105.161 0.9986275
    ## a[16]  1.71876294 0.7687264  0.555124157  3.01173386 4176.406 0.9991159
    ## a[17]  2.53979344 0.6981843  1.546077181  3.71006823 4433.709 0.9989912
    ## a[18]  2.13518133 0.5880296  1.258871648  3.14326463 4271.425 0.9996748
    ## a[19]  1.81560599 0.5322764  1.019157040  2.74672586 3791.953 0.9986856
    ## a[20]  3.10049111 0.7760021  1.945080018  4.41811142 3479.074 1.0001664
    ## a[21]  2.15402355 0.6280459  1.214016189  3.23840118 3914.803 0.9985017
    ## a[22]  2.15538423 0.6376089  1.188455737  3.20850750 3784.751 0.9986384
    ## a[23]  2.12149874 0.5902317  1.248297123  3.12147069 4281.607 0.9989795
    ## a[24]  1.55262249 0.5225096  0.778790307  2.39685441 3864.071 0.9989304
    ## a[25] -1.08965287 0.4559819 -1.830333925 -0.37631314 5075.634 0.9984663
    ## a[26]  0.07299515 0.3925315 -0.553363243  0.70643047 5258.049 0.9991389
    ## a[27] -1.54441018 0.5174648 -2.401616147 -0.75826741 3877.971 0.9992725
    ## a[28] -0.56085963 0.3969601 -1.210504593  0.04766005 4596.074 0.9988765
    ## a[29]  0.06835756 0.3965242 -0.559699008  0.71247376 4555.572 0.9985355
    ## a[30]  1.31371749 0.4727167  0.577066726  2.10462386 3522.406 0.9983988
    ## a[31] -0.72179111 0.4180127 -1.417884891 -0.08842708 4435.226 0.9988673
    ## a[32] -0.39242912 0.3941398 -1.018154629  0.22222975 4340.737 0.9994083
    ## a[33]  2.84919036 0.6600118  1.891545839  3.99743693 2914.082 1.0002283
    ## a[34]  2.45874334 0.5991201  1.593184680  3.45059174 3029.823 0.9995047
    ## a[35]  2.44877103 0.5724458  1.621632457  3.37369097 3598.439 0.9986800
    ## a[36]  1.90578866 0.4869092  1.153927457  2.75466815 3708.999 0.9993047
    ## a[37]  1.90167266 0.4625416  1.186310488  2.66540614 3830.659 0.9987845
    ## a[38]  3.36126876 0.7716691  2.239500072  4.66403568 2789.964 0.9983332
    ## a[39]  2.47144639 0.5647662  1.616782720  3.43394394 3164.050 0.9990978
    ## a[40]  2.15993802 0.4827634  1.418585385  2.94833771 4325.880 0.9985208
    ## a[41] -1.91099454 0.4833064 -2.722365693 -1.20081119 4501.992 0.9982688
    ## a[42] -0.63042269 0.3503036 -1.217672927 -0.07099649 4889.262 0.9995636
    ## a[43] -0.50941080 0.3482521 -1.068377177  0.05292937 4629.691 0.9985275
    ## a[44] -0.38931852 0.3634023 -0.981714866  0.17698090 4612.779 0.9982774
    ## a[45]  0.50995431 0.3308343 -0.006960504  1.03701527 4276.147 0.9986094
    ## a[46] -0.62749573 0.3409556 -1.188659275 -0.09001666 5136.558 0.9984975
    ## a[47]  1.90353799 0.4846375  1.164429666  2.69004319 4452.775 0.9994662
    ## a[48] -0.07099780 0.3309398 -0.595405268  0.45788112 4404.773 0.9985794

# the end of test1

# varying slope test (varying slope version of practice 13M3, (eg. m.practice13M3.b) had error, so I cheched varying slope codes from book)

## 14.1.1. Simulate the population.

``` r
a <- 3.5
b <- (-1)
sigma_a <- 1
sigma_b <- 0.5
rho <- (-0.7)
# average morning wait time
# average difference afternoon wait time
# std dev in intercepts
# std dev in slopes
# correlation between intercepts and slopes
```

``` r
Mu <- c( a , b )
```

``` r
cov_ab <- sigma_a*sigma_b*rho
Sigma <- matrix( c(sigma_a^2,cov_ab,cov_ab,sigma_b^2) , ncol=2 )
```

``` r
 matrix( c(1,2,3,4) , nrow=2 , ncol=2 )
```

    ##      [,1] [,2]
    ## [1,]    1    3
    ## [2,]    2    4

``` r
sigmas <- c(sigma_a,sigma_b) # standard deviations
Rho <- matrix( c(1,rho,rho,1) , nrow=2 ) # correlation matrix
# now matrix multiply to get covariance matrix
Sigma <- diag(sigmas) %*% Rho %*% diag(sigmas)
```

``` r
 N_cafes <- 20
```

``` r
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
set.seed(5) # used to replicate example
vary_effects <- mvrnorm( N_cafes , Mu , Sigma )
```

``` r
a_cafe <- vary_effects[,1]
b_cafe <- vary_effects[,2]
```

``` r
plot( a_cafe , b_cafe , col=rangi2 ,
    xlab="intercepts (a_cafe)" , ylab="slopes (b_cafe)" )
# overlay population distribution
library(ellipse)
```

    ## 
    ## Attaching package: 'ellipse'

    ## The following object is masked from 'package:rethinking':
    ## 
    ##     pairs

    ## The following object is masked from 'package:graphics':
    ## 
    ##     pairs

``` r
for ( l in c(0.1,0.3,0.5,0.8,0.99) )
    lines(ellipse(Sigma,centre=Mu,level=l),col=col.alpha("black",0.2))
```

![](Chap14_2_HW3_hyp_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

# 14.1.2. Simulate observations.

``` r
set.seed(22)
N_visits <- 10
afternoon <- rep(0:1,N_visits*N_cafes/2)
cafe_id <- rep( 1:N_cafes , each=N_visits )
mu <- a_cafe[cafe_id] + b_cafe[cafe_id]*afternoon
sigma <- 0.5  # std dev within cafes
wait <- rnorm( N_visits*N_cafes , mu , sigma )
d <- data.frame( cafe=cafe_id , afternoon=afternoon , wait=wait )
```

# 14.1.3. The varying slopes model.

``` r
R <- rlkjcorr( 1e4 , K=2 , eta=2 )
dens( R[,1,2] , xlab="correlation" )
```

![](Chap14_2_HW3_hyp_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
m14.1 <- ulam(
    alist(
        wait ~ normal( mu , sigma ),
        mu <- a_cafe[cafe] + b_cafe[cafe]*afternoon,
        c(a_cafe,b_cafe)[cafe] ~ multi_normal( c(a,b) , Rho , sigma_cafe ),
        a ~ normal(5,2),
        b ~ normal(-1,0.5),
        sigma_cafe ~ exponential(1),
        sigma ~ exponential(1),
        Rho ~ lkj_corr(2)
    ) , data=d , chains=4 , cores=2,iter=2000,log_lik=TRUE)
```

    ## Warning: The largest R-hat is NA, indicating chains have not mixed.
    ## Running the chains for more iterations may help. See
    ## http://mc-stan.org/misc/warnings.html#r-hat

    ## Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
    ## Running the chains for more iterations may help. See
    ## http://mc-stan.org/misc/warnings.html#bulk-ess

    ## Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
    ## Running the chains for more iterations may help. See
    ## http://mc-stan.org/misc/warnings.html#tail-ess

``` r
# What is lkj_corr?
?rlkjcorr
?rmultinom # for multi_ormal, correct?
precis(m14.1,depth=3)
```

    ##                     mean           sd       5.5%        94.5%    n_eff
    ## b_cafe[1]     -1.1528491 2.594566e-01 -1.5689643 -0.726676216 5311.481
    ## b_cafe[2]     -0.9055777 2.684926e-01 -1.3477132 -0.477822624 5186.250
    ## b_cafe[3]     -1.9381105 2.738587e-01 -2.3727761 -1.498480736 5318.099
    ## b_cafe[4]     -1.2368937 2.643966e-01 -1.6481091 -0.808740187 5848.196
    ## b_cafe[5]     -0.1400265 2.858324e-01 -0.5904145  0.335820601 4644.990
    ## b_cafe[6]     -1.3023128 2.603857e-01 -1.7142367 -0.891614196 5619.572
    ## b_cafe[7]     -1.0228427 2.631967e-01 -1.4403466 -0.591293188 4655.372
    ## b_cafe[8]     -1.6250820 2.600945e-01 -2.0524407 -1.208007604 5210.595
    ## b_cafe[9]     -1.3037484 2.643438e-01 -1.7213383 -0.878759550 4789.355
    ## b_cafe[10]    -0.9524475 2.745354e-01 -1.3934164 -0.516025272 5588.501
    ## b_cafe[11]    -0.4312815 2.796028e-01 -0.8812637  0.009374481 5253.417
    ## b_cafe[12]    -1.1867001 2.640430e-01 -1.6171523 -0.777957606 5404.970
    ## b_cafe[13]    -1.8120531 2.740525e-01 -2.2576544 -1.380823245 5140.964
    ## b_cafe[14]    -0.9404512 2.680443e-01 -1.3696454 -0.521527043 5527.609
    ## b_cafe[15]    -2.1910149 2.872437e-01 -2.6664111 -1.743392561 4620.798
    ## b_cafe[16]    -1.0442255 2.681419e-01 -1.4664822 -0.619460283 5015.437
    ## b_cafe[17]    -1.2195300 2.595661e-01 -1.6392822 -0.797355341 4856.650
    ## b_cafe[18]    -1.0217511 2.854128e-01 -1.4617336 -0.548511208 5248.514
    ## b_cafe[19]    -0.2617084 2.800224e-01 -0.7006356  0.197699802 3877.688
    ## b_cafe[20]    -1.0668223 2.633080e-01 -1.4831090 -0.645958533 5317.117
    ## a_cafe[1]      4.2170991 1.951479e-01  3.9100313  4.528389333 5530.934
    ## a_cafe[2]      2.1598732 2.023715e-01  1.8337605  2.480643343 4499.554
    ## a_cafe[3]      4.3739745 2.002476e-01  4.0507167  4.684667546 5502.994
    ## a_cafe[4]      3.2443856 2.007948e-01  2.9282023  3.568651088 5300.999
    ## a_cafe[5]      1.8766150 2.127240e-01  1.5363659  2.223453778 4645.961
    ## a_cafe[6]      4.2620285 2.006174e-01  3.9437034  4.583606842 5925.506
    ## a_cafe[7]      3.6155890 2.031205e-01  3.2837107  3.940060706 4634.320
    ## a_cafe[8]      3.9479268 1.960748e-01  3.6326402  4.256886352 4963.025
    ## a_cafe[9]      3.9844573 2.006288e-01  3.6640359  4.306853165 3921.227
    ## a_cafe[10]     3.5611095 2.052378e-01  3.2302523  3.894786625 5049.022
    ## a_cafe[11]     1.9355959 2.057440e-01  1.6114683  2.262955603 5098.571
    ## a_cafe[12]     3.8441864 2.006945e-01  3.5245611  4.164164728 5480.961
    ## a_cafe[13]     3.8861255 2.022600e-01  3.5670714  4.210985690 5282.496
    ## a_cafe[14]     3.1760147 2.019739e-01  2.8564048  3.491355809 5056.537
    ## a_cafe[15]     4.4512884 2.096377e-01  4.1152950  4.796033704 5118.758
    ## a_cafe[16]     3.3901411 2.033662e-01  3.0713344  3.713292281 5196.100
    ## a_cafe[17]     4.2143865 1.959483e-01  3.8972662  4.525633835 5139.204
    ## a_cafe[18]     5.7451428 2.066747e-01  5.4116498  6.072375638 5579.548
    ## a_cafe[19]     3.2494420 2.066317e-01  2.9306201  3.582802591 4489.151
    ## a_cafe[20]     3.7373942 1.969077e-01  3.4242504  4.058604281 5334.635
    ## a              3.6512755 2.165104e-01  3.3133642  3.995736551 4664.869
    ## b             -1.1317579 1.431110e-01 -1.3636911 -0.900919846 4020.801
    ## sigma_cafe[1]  0.9609332 1.607341e-01  0.7356584  1.232217974 3663.188
    ## sigma_cafe[2]  0.5909523 1.263922e-01  0.4177011  0.817871202 3611.232
    ## sigma          0.4740424 2.670136e-02  0.4339308  0.518236410 3939.867
    ## Rho[1,1]       1.0000000 0.000000e+00  1.0000000  1.000000000      NaN
    ## Rho[1,2]      -0.5035704 1.779868e-01 -0.7497557 -0.190689099 4116.002
    ## Rho[2,1]      -0.5035704 1.779868e-01 -0.7497557 -0.190689099 4116.002
    ## Rho[2,2]       1.0000000 7.898412e-17  1.0000000  1.000000000 3512.265
    ##                   Rhat4
    ## b_cafe[1]     0.9993523
    ## b_cafe[2]     0.9995211
    ## b_cafe[3]     1.0002009
    ## b_cafe[4]     0.9995452
    ## b_cafe[5]     0.9995517
    ## b_cafe[6]     0.9993782
    ## b_cafe[7]     1.0000161
    ## b_cafe[8]     1.0005508
    ## b_cafe[9]     0.9998809
    ## b_cafe[10]    0.9995082
    ## b_cafe[11]    0.9999762
    ## b_cafe[12]    0.9994160
    ## b_cafe[13]    0.9999128
    ## b_cafe[14]    0.9993147
    ## b_cafe[15]    1.0008124
    ## b_cafe[16]    0.9998594
    ## b_cafe[17]    0.9994817
    ## b_cafe[18]    0.9993323
    ## b_cafe[19]    1.0003333
    ## b_cafe[20]    0.9993206
    ## a_cafe[1]     0.9996961
    ## a_cafe[2]     0.9998999
    ## a_cafe[3]     1.0001218
    ## a_cafe[4]     0.9999444
    ## a_cafe[5]     0.9992030
    ## a_cafe[6]     0.9994401
    ## a_cafe[7]     0.9994942
    ## a_cafe[8]     1.0009442
    ## a_cafe[9]     0.9996461
    ## a_cafe[10]    0.9999180
    ## a_cafe[11]    0.9997575
    ## a_cafe[12]    0.9999845
    ## a_cafe[13]    0.9998129
    ## a_cafe[14]    0.9994796
    ## a_cafe[15]    1.0008258
    ## a_cafe[16]    1.0007247
    ## a_cafe[17]    0.9998508
    ## a_cafe[18]    0.9995430
    ## a_cafe[19]    1.0001542
    ## a_cafe[20]    0.9998679
    ## a             1.0003918
    ## b             0.9999547
    ## sigma_cafe[1] 0.9992414
    ## sigma_cafe[2] 1.0002973
    ## sigma         1.0003127
    ## Rho[1,1]            NaN
    ## Rho[1,2]      1.0001170
    ## Rho[2,1]      1.0001170
    ## Rho[2,2]      0.9989995

# the end of varying slope test

# new problems (March 20, 2020)

# 1\. 1. Review last weeks material.

# 2\. Update last weeks problems if necessary. Can you fit non-centered models? Are you using multivariate normal distributions where appropriate?

# 4\. Attached are data from an experiment measuring hypocotyl length in ~ 180 natural arabidopsis accessions grown in high and low red:far-red light. We want to know if there are differences in accessions in their length in high R:FR (“H”) and in their response to low R:FR(“L”). Also we want to obtain an estimate for hypocotyl length for each accession in high and low R:FR for downstream GWAS analysis.

# Relevant variables:

  - length – hypocotyl length
  - line – unique ID for each accession (you could also use nativename)
  - light – indicator for high or low RFR
  - exp – two independent experiments were done
  - plate – this is an incomplete block design with a subset (10? 12?)
    of accessions on each plate. Let’s try a variety of increasingly
    complex models:
  - reading data

<!-- end list -->

``` r
At.hyp.dat <- read_csv("hyp.lengths.both.experiments.labels.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   line = col_double(),
    ##   length = col_double(),
    ##   plate = col_character(),
    ##   light = col_character(),
    ##   exp = col_character(),
    ##   nativename = col_character(),
    ##   stockparent = col_character(),
    ##   latitude = col_double(),
    ##   longitude = col_double(),
    ##   site = col_character(),
    ##   region = col_character(),
    ##   country = col_character()
    ## )

``` r
str(At.hyp.dat)
```

    ## Classes 'spec_tbl_df', 'tbl_df', 'tbl' and 'data.frame': 18031 obs. of  12 variables:
    ##  $ line       : num  6901 6901 6901 6901 6901 ...
    ##  $ length     : num  2.01 1.48 1.99 2.74 1.94 ...
    ##  $ plate      : chr  "1AH" "1AH" "1AH" "1AH" ...
    ##  $ light      : chr  "H" "H" "H" "H" ...
    ##  $ exp        : chr  "A" "A" "A" "A" ...
    ##  $ nativename : chr  "Bil-7" "Bil-7" "Bil-7" "Bil-7" ...
    ##  $ stockparent: chr  "CS22579" "CS22579" "CS22579" "CS22579" ...
    ##  $ latitude   : num  63.3 63.3 63.3 63.3 63.3 ...
    ##  $ longitude  : num  18.5 18.5 18.5 18.5 18.5 ...
    ##  $ site       : chr  "Bil" "Bil" "Bil" "Bil" ...
    ##  $ region     : chr  "NSweden" "NSweden" "NSweden" "NSweden" ...
    ##  $ country    : chr  "SWE" "SWE" "SWE" "SWE" ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   line = col_double(),
    ##   ..   length = col_double(),
    ##   ..   plate = col_character(),
    ##   ..   light = col_character(),
    ##   ..   exp = col_character(),
    ##   ..   nativename = col_character(),
    ##   ..   stockparent = col_character(),
    ##   ..   latitude = col_double(),
    ##   ..   longitude = col_double(),
    ##   ..   site = col_character(),
    ##   ..   region = col_character(),
    ##   ..   country = col_character()
    ##   .. )

``` r
# format dat for exp
At.hyp.dat2 <- At.hyp.dat %>% mutate(expnid=ifelse(exp=="A",0L,1L))
# format for light (non index)
At.hyp.dat2 <- At.hyp.dat2 %>% mutate(lightnid=ifelse(light=="H",0L,1L))
# format for line
At.hyp.dat2 <- At.hyp.dat2 %>% mutate(lineid=as.integer(as.factor(line)))
# format dat for plate
At.hyp.dat2 %>% group_by(plate,exp,light,line) %>% summarize(n())
```

    ## # A tibble: 1,303 x 5
    ## # Groups:   plate, exp, light [152]
    ##    plate exp   light  line `n()`
    ##    <chr> <chr> <chr> <dbl> <int>
    ##  1 10AH  A     H      6088    13
    ##  2 10AH  A     H      6899     8
    ##  3 10AH  A     H      6956    21
    ##  4 10AH  A     H      6962    14
    ##  5 10AH  A     H      6963    27
    ##  6 10AH  A     H      6976    13
    ##  7 10AH  A     H      8245    19
    ##  8 10AH  A     H      8312    18
    ##  9 10AH  A     H      8426    21
    ## 10 10AL  A     L      6088    17
    ## # … with 1,293 more rows

``` r
At.hyp.dat2 <- At.hyp.dat2 %>% mutate(plateid = as.integer(as.factor(plate)))
```

1.  No
pooling

<!-- end list -->

``` r
At.hyp.dat2.s <- At.hyp.dat2 %>% dplyr::select(length,lineid,lightnid,expnid,plateid)
str(At.hyp.dat2.s)
```

    ## Classes 'spec_tbl_df', 'tbl_df', 'tbl' and 'data.frame': 18031 obs. of  5 variables:
    ##  $ length  : num  2.01 1.48 1.99 2.74 1.94 ...
    ##  $ lineid  : int  18 18 18 18 18 18 18 163 163 163 ...
    ##  $ lightnid: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ expnid  : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ plateid : int  41 41 41 41 41 41 41 41 41 41 ...

``` r
# only line and lightnid
mAt.hyp.nopooling1 <- ulam(
    alist(
        length ~ dnorm( mu , sigma ),
        mu <- a[lineid] + b[lineid]*lightnid, 
        a[lineid] ~ dnorm(0,1),
        b[lineid] ~ dnorm(0,1),        
        sigma ~ dexp(1)
    ) , data=At.hyp.dat2.s , chains=4 , cores=2,iter=2000,log_lik=TRUE)
```

    ## Warning: There were 87 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
    ## http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded

    ## Warning: Examine the pairs() plot to diagnose sampling problems

1.  scaled normalized length

<!-- end list -->

  - Note: scaled -\> mu chould be centered at zero?

<!-- end list -->

``` r
At.hyp.dat2.s <- At.hyp.dat2.s %>% mutate(lengthscaled = scale(length))
str(At.hyp.dat2.s)
```

    ## Classes 'spec_tbl_df', 'tbl_df', 'tbl' and 'data.frame': 18031 obs. of  6 variables:
    ##  $ length      : num  2.01 1.48 1.99 2.74 1.94 ...
    ##  $ lineid      : int  18 18 18 18 18 18 18 163 163 163 ...
    ##  $ lightnid    : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ expnid      : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ plateid     : int  41 41 41 41 41 41 41 41 41 41 ...
    ##  $ lengthscaled: num [1:18031, 1] -1.56 -1.95 -1.58 -1.02 -1.61 ...
    ##   ..- attr(*, "scaled:center")= num 4.12
    ##   ..- attr(*, "scaled:scale")= num 1.35

``` r
mAt.hyp.nopooling1.scaled <- ulam(
    alist(
        lengthscaled ~ dnorm( mu , sigma ),
        mu <- a[lineid] + b[lineid]*lightnid, 
        a[lineid] ~ dnorm(0,1),
        b[lineid] ~ dnorm(0,1),        
        sigma ~ dexp(1)
    ) , data=At.hyp.dat2.s , chains=4 , cores=2,iter=2000,log_lik=TRUE)
```

    ## Warning: There were 157 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
    ## http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded

    ## Warning: Examine the pairs() plot to diagnose sampling problems

# exp and plate (pending)

``` r
mAt.hyp.nopooling <- ulam(
    alist(
        length ~ dnorm( mu , sigma ),
        mu <- a[lineid] + b[lineid]*lightnid,   + c[line]*expnid, #+ d[line]*plateid,
        a[lineid] ~ dnorm(0,1),
        b[lineid] ~ dnorm(0,1),
        c[lineid] ~ dnorm(0,1),
        d[lineid] ~ dnorm(0,1),
        sigma ~ dexp(1)
    ) , data=At.hyp.dat2.s , chains=4 , cores=2,iter=2000,log_lik=TRUE)
plot(mAt.hyp.nopooling1, depth=2)
```

2.  Partial pooling of intercepts and slopes for line and intercepts for
    plate and experiment, but treat each variable separately (no
    multivariate component). you might also consider adding an
    experiment slope effect

<!-- end list -->

  - try differnt a\_bar and a\_sigma (see Julin’s schript).

  - 
<!-- end list -->

``` r
mAt.hyp.partialpooling <- ulam(
    alist(
        length ~ dnorm( mu , sigma ),
        mu <- a[lineid] + b[lineid]*lightnid, 
        a[lineid] ~ dnorm(a_bar,a_sigma),
        b[lineid] ~ dnorm(0,1),  
        a_bar ~ dnorm(0,0.25),
        a_sigma ~ dexp(1),
        sigma ~ dexp(1)
    ) , data=At.hyp.dat2.s , chains=4 , cores=2,iter=5000,log_lik=TRUE)
```

    ## Warning: There were 1415 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
    ## http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded

    ## Warning: Examine the pairs() plot to diagnose sampling problems

3.  As 2, but use a multivariate normal model for the line slope and
    intercept effects

<!-- end list -->

``` r
mAt.hyp.multi <- ulam(
    alist(
        length ~ normal( mu , sigma ),
        mu <- a_line[lineid] + b_line[lineid]*lightnid ,
        c(a_line,b_line)[lineid] ~ multi_normal( c(a,b) , Rho , sigma_line ),
        a ~ normal(5,2),
        b ~ normal(-1,0.5),
        sigma_line ~ dexp(1),
        sigma ~ dexp(1),
        Rho ~ lkj_corr(2)
    ) , data=At.hyp.dat2.s , chains=4 , cores=2,iter=4000,log_lik=TRUE)
```

    ## Warning: The largest R-hat is NA, indicating chains have not mixed.
    ## Running the chains for more iterations may help. See
    ## http://mc-stan.org/misc/warnings.html#r-hat

    ## Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
    ## Running the chains for more iterations may help. See
    ## http://mc-stan.org/misc/warnings.html#bulk-ess

    ## Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
    ## Running the chains for more iterations may help. See
    ## http://mc-stan.org/misc/warnings.html#tail-ess

  - check Rho

<!-- end list -->

``` r
Rho
```

    ##      [,1] [,2]
    ## [1,]  1.0 -0.7
    ## [2,] -0.7  1.0

  - Check correlation sun length and response (seeing correlation
    between )

<!-- end list -->

``` r
# plot raw data
At.hyp.dat2.summary <- At.hyp.dat2 %>% group_by(lineid,light) %>% summarize(length.mean=mean(length)) %>% spread(key="light",value="length.mean") %>% mutate(response=L-H) 
p1 <- At.hyp.dat2.summary %>% ggplot(aes(x=H,y=response)) + geom_point()
p1 + labs(title="mean raw data for each line")
```

    ## Warning: Removed 4 rows containing missing values (geom_point).

![](Chap14_2_HW3_hyp_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
# extract posterior means of partially pooled estimates
post <- extract.samples(mAt.hyp.multi) # m14.1 instead of m13.1?
```

``` r
a_line <- apply( post$a_line , 2 , mean )
b_line <- apply( post$b_line , 2 , mean )
# post.summary <- 
#   tibble(a_line=a_line,b_line=b_line,lineid=At.hyp.dat2.summary$lineid,light=At.hyp.dat2$light) %>% group_by(lineid,light) %>% summarize(length.mean=mean(length))
#p2 <- At.hyp.dat2.summary %>% ungroup() %>% mutate(H.pred=a_line,response.pred=b_line)  %>% ggplot(aes(x=H,y=response)) + geom_point() + geom_point(aes(x=H.pred,y=response.pred),color="blue")
#p2 

p3 <- At.hyp.dat2.summary %>% ungroup() %>% mutate(H.pred=a_line,response.pred=b_line) %>% 
  ggplot() + geom_point(aes(x=H,y=response)) +
geom_segment(mapping=aes(x=H, y=response, xend=H.pred, yend=response.pred), arrow=arrow(length = unit(0.1, "inches")), size=1, color="blue") 
p3 + labs(title="mAt.hyp.multi")
```

    ## Warning: Removed 4 rows containing missing values (geom_point).

    ## Warning: Removed 4 rows containing missing values (geom_segment).

![](Chap14_2_HW3_hyp_files/figure-gfm/unnamed-chunk-21-2.png)<!-- -->

  - add

<!-- end list -->

4.  As 3, but non-centered

Evaluate and compare the models. Is there evidence of line, treatment,
and line X treatment effects? How does the magnitude of the experiment
and plate effects compare to the line effects?

# sessionInfo()

``` r
sessionInfo()
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
    ##  [1] ellipse_0.4.1        MASS_7.3-51.5        forcats_0.5.0       
    ##  [4] stringr_1.4.0        dplyr_0.8.4          purrr_0.3.3         
    ##  [7] readr_1.3.1          tidyr_1.0.2          tibble_2.1.3        
    ## [10] tidyverse_1.3.0      reshape2_1.4.3       lmerTest_3.1-1      
    ## [13] lme4_1.1-21          Matrix_1.2-18        rethinking_1.95     
    ## [16] dagitty_0.2-2        rstan_2.21.1         ggplot2_3.3.0       
    ## [19] StanHeaders_2.21.0-1
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] nlme_3.1-145        matrixStats_0.55.0  fs_1.3.1           
    ##  [4] lubridate_1.7.4     httr_1.4.1          numDeriv_2016.8-1.1
    ##  [7] tools_3.6.2         backports_1.1.5     utf8_1.1.4         
    ## [10] R6_2.4.1            DBI_1.1.0           colorspace_1.4-1   
    ## [13] withr_2.1.2         tidyselect_1.0.0    gridExtra_2.3      
    ## [16] prettyunits_1.1.1   processx_3.4.2      curl_4.3           
    ## [19] compiler_3.6.2      cli_2.0.2           rvest_0.3.5        
    ## [22] xml2_1.2.2          labeling_0.3        scales_1.1.0       
    ## [25] mvtnorm_1.1-0       callr_3.4.2         digest_0.6.25      
    ## [28] minqa_1.2.4         rmarkdown_2.1       pkgconfig_2.0.3    
    ## [31] htmltools_0.4.0     dbplyr_1.4.2        rlang_0.4.5        
    ## [34] readxl_1.3.1        rstudioapi_0.11     farver_2.0.3       
    ## [37] shape_1.4.4         generics_0.0.2      jsonlite_1.6.1     
    ## [40] inline_0.3.15       magrittr_1.5        loo_2.2.0          
    ## [43] Rcpp_1.0.3          munsell_0.5.0       fansi_0.4.1        
    ## [46] lifecycle_0.2.0     stringi_1.4.6       yaml_2.2.1         
    ## [49] pkgbuild_1.0.6      plyr_1.8.6          grid_3.6.2         
    ## [52] crayon_1.3.4        lattice_0.20-40     haven_2.2.0        
    ## [55] splines_3.6.2       hms_0.5.3           knitr_1.28         
    ## [58] ps_1.3.2            pillar_1.4.3        boot_1.3-24        
    ## [61] codetools_0.2-16    stats4_3.6.2        reprex_0.3.0       
    ## [64] glue_1.3.1          evaluate_0.14       V8_3.0.1           
    ## [67] RcppParallel_4.4.4  modelr_0.1.6        vctrs_0.2.3        
    ## [70] nloptr_1.2.1        cellranger_1.1.0    gtable_0.3.0       
    ## [73] assertthat_0.2.1    xfun_0.12           broom_0.5.5        
    ## [76] coda_0.19-3
