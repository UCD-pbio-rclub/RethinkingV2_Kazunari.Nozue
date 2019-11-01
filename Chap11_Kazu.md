---
title: "Chap11_Kazu"
author: "Kazu"
date: "11/1/2019"
output: 
  html_document: 
    keep_md: yes
---



# 

```r
data(chimpanzees)
d <- chimpanzees
```
* There are four combinations:
* (1) prosoc_left= 0 and condition= 0: Two food items on right and no partner. 
* (2) prosoc_left= 1 and condition= 0: Two food items on left and no partner.
* (3) prosoc_left= 0 and condition= 1: Two food items on right and partner present. 
* (4) prosoc_left= 1 and condition= 1: Two food items on left and partner present.

* let’s build an index variable containing the values 1 through 4, to index the combinations above.

```r
d$treatment <- 1 + d$prosoc_left + 2*d$condition
```

* Now treatment contains the values 1 through 4, matching the numbers in the list above. You can verify by using cross-tabs

```r
 xtabs( ~ treatment + prosoc_left + condition , d )
```

```
## , , condition = 0
## 
##          prosoc_left
## treatment   0   1
##         1 126   0
##         2   0 126
##         3   0   0
##         4   0   0
## 
## , , condition = 1
## 
##          prosoc_left
## treatment   0   1
##         1   0   0
##         2   0   0
##         3 126   0
##         4   0 126
```

```r
m11.1 <- quap(
    alist(
        pulled_left ~ dbinom( 1 , p ) ,
        logit(p) <- a ,
        a ~ dnorm( 0 , 10 )
) , data=d )
```

# 

```r
set.seed(1999)
prior <- extract.prior( m11.1 , n=1e4 )
```

# 

```r
p <- inv_logit( prior$a )
dens( p , adj=0.1 )
```

![](Chap11_Kazu_files/figure-html/R code 11.6-1.png)<!-- -->

# 

```r
m11.2 <- quap(
    alist(
        pulled_left ~ dbinom( 1 , p ) ,
        logit(p) <- a + b[treatment] ,
        a ~ dnorm( 0 , 1.5 ),
        b[treatment] ~ dnorm( 0 , 10 )
) , data=d )
set.seed(1999)
prior <- extract.prior( m11.2 , n=1e4 )
p <- sapply( 1:4 , function(k) inv_logit( prior$a + prior$b[,k] ) )
```

# 

```r
 dens( abs( p[,1] - p[,2] ) , adj=0.1 )
```

![](Chap11_Kazu_files/figure-html/R code 11.8-1.png)<!-- -->

# 

```r
m11.3 <- quap(
    alist(
        pulled_left ~ dbinom( 1 , p ) ,
        logit(p) <- a + b[treatment] ,
        a ~ dnorm( 0 , 1.5 ),
        b[treatment] ~ dnorm( 0 , 0.5 )
    ) , data=d )
set.seed(1999)
prior <- extract.prior( m11.3 , n=1e4 )
p <- sapply( 1:4 , function(k) inv_logit( prior$a + prior$b[,k] ) )
mean( abs( p[,1] - p[,2] ) )
```

```
## [1] 0.09838663
```

#

```r
# prior trimmed data list
dat_list <- list(
    pulled_left = d$pulled_left,
    actor = d$actor,
    treatment = as.integer(d$treatment) )
# particles in 11-dimensional space
m11.4 <- ulam(
    alist(
        pulled_left ~ dbinom( 1 , p ) ,
        logit(p) <- a[actor] + b[treatment] ,
        a[actor] ~ dnorm( 0 , 1.5 ),
        b[treatment] ~ dnorm( 0 , 0.5 )
),
    data=dat_list , chains=4, core=2,log_lik=TRUE) # adding log_lik=TRUE for coomparison (Kazu)
precis( m11.4 , depth=2 )
```

```
##            mean        sd        5.5%       94.5%     n_eff      Rhat
## a[1] -0.4405507 0.3279480 -0.95517191  0.09014720  886.9475 1.0019443
## a[2]  3.9010060 0.7508270  2.80042409  5.16176919 1497.1286 0.9991706
## a[3] -0.7346984 0.3284202 -1.25411334 -0.21160526  893.3338 1.0011085
## a[4] -0.7228660 0.3282111 -1.23577149 -0.19269141  907.4431 1.0056065
## a[5] -0.4340172 0.3271884 -0.95274252  0.09150824  884.2596 1.0030804
## a[6]  0.4913732 0.3368085 -0.05009655  1.01631481  875.3362 1.0021107
## a[7]  1.9625382 0.4106072  1.33700892  2.66246394 1227.9889 1.0001700
## b[1] -0.0484187 0.2865308 -0.49768389  0.40686886  849.2850 1.0045507
## b[2]  0.4656816 0.2798013  0.00559224  0.90061250  755.8780 1.0035313
## b[3] -0.3923914 0.2839955 -0.84038076  0.07368503  836.1496 1.0035351
## b[4]  0.3567186 0.2838486 -0.09372661  0.81153498  868.2254 1.0013881
```

#

```r
post <- extract.samples(m11.4)
p_left <- inv_logit( post$a )
plot( precis( as.data.frame(p_left) ) , xlim=c(0,1) )
```

![](Chap11_Kazu_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

# 

```r
labs <- c("R/N","L/N","R/P","L/P")
plot( precis( m11.4 , depth=2 , pars="b" ) , labels=labs )
```

![](Chap11_Kazu_files/figure-html/R code 11.12-1.png)<!-- -->

# 

```r
diffs <- list(
    db13 = post$b[,1] - post$b[,3],
    db24 = post$b[,2] - post$b[,4] )
plot( precis(diffs) )
```

![](Chap11_Kazu_files/figure-html/R code 11.13-1.png)<!-- -->

# 

```r
pl <- by( d$pulled_left , list( d$actor , d$treatment ) , mean )
pl[1,]
```

```
##         1         2         3         4 
## 0.3333333 0.5000000 0.2777778 0.5555556
```

#

```r
plot( NULL , xlim=c(1,28) , ylim=c(0,1) , xlab="" ,
    ylab="proportion left lever" , xaxt="n" , yaxt="n" )
axis( 2 , at=c(0,0.5,1) , labels=c(0,0.5,1) )
abline( h=0.5 , lty=2 )
for ( j in 1:7 ) abline( v=(j-1)*4+4.5 , lwd=0.5 )
for ( j in 1:7 ) text( (j-1)*4+2.5 , 1.1 , concat("actor ",j) , xpd=TRUE )
for ( j in (1:7)[-2] ) {
    lines( (j-1)*4+c(1,3) , pl[j,c(1,3)] , lwd=2 , col=rangi2 )
    lines( (j-1)*4+c(2,4) , pl[j,c(2,4)] , lwd=2 , col=rangi2 )
}
points( 1:28 , t(pl) , pch=16 , col="white" , cex=1.7 )
points( 1:28 , t(pl) , pch=c(1,1,16,16) , col=rangi2 , lwd=2 )
yoff <- 0.01
text( 1 , pl[1,1]-yoff , "R/N" , pos=1 , cex=0.8 )
text( 2 , pl[1,2]+yoff , "L/N" , pos=3 , cex=0.8 )
text( 3 , pl[1,3]-yoff , "R/P" , pos=1 , cex=0.8 )
text( 4 , pl[1,4]+yoff , "L/P" , pos=3 , cex=0.8 )
mtext( "observed proportions\n" )
```

![](Chap11_Kazu_files/figure-html/R code 11.15-1.png)<!-- -->

# 

```r
dat <- list( actor=rep(1:7,each=4) , treatment=rep(1:4,times=7) )
p_post <- link_ulam( m11.4 , data=dat )
p_mu <- apply( p_post , 2 , mean )
p_ci <- apply( p_post , 2 , PI )
```

# 

```r
d$side <- d$prosoc_left + 1 # right 1, left 2
d$cond <- d$condition + 1 # no partner 1, partner 
```

#

```r
dat_list2 <- list(
    pulled_left = d$pulled_left,
    actor = d$actor,
    side = d$side,
    cond = d$cond )
m11.5 <- ulam(
    alist(
        pulled_left ~ dbinom( 1 , p ) ,
        logit(p) <- a[actor] + bs[side] + bc[cond] ,
        a[actor] ~ dnorm( 0 , 1.5 ),
        bs[side] ~ dnorm( 0 , 0.5 ),
        bc[cond] ~ dnorm( 0 , 0.5 )
),
data=dat_list2 , chains=4 , , core=2,log_lik=TRUE )
```

#

```r
compare( m11.5 , m11.4 , func=LOO )
```

```
##            LOO     pLOO      dLOO    weight       SE      dSE
## m11.5 531.2791 7.963326 0.0000000 0.6059983 19.11409       NA
## m11.4 532.1401 8.387160 0.8610439 0.3940017 18.87165 1.362118
```

# 

```r
 post <- extract.samples( m11.4 , clean=FALSE )
str(post)
```

```
## List of 4
##  $ log_lik: num [1:2000, 1:504] -0.529 -0.463 -0.576 -0.561 -0.43 ...
##  $ a      : num [1:2000, 1:7] -0.606 -0.736 -0.203 -0.421 -0.622 ...
##  $ b      : num [1:2000, 1:4] 0.246426 0.205903 -0.046847 0.136628 0.000956 ...
##  $ lp__   : num [1:2000(1d)] -265 -270 -265 -267 -265 ...
##  - attr(*, "source")= chr "ulam posterior: 2000 samples from m11.4"
```

# 

```r
m11.4_stan_code <- stancode(m11.4)
```

```
## data{
##     int pulled_left[504];
##     int treatment[504];
##     int actor[504];
## }
## parameters{
##     vector[7] a;
##     vector[4] b;
## }
## model{
##     vector[504] p;
##     b ~ normal( 0 , 0.5 );
##     a ~ normal( 0 , 1.5 );
##     for ( i in 1:504 ) {
##         p[i] = a[actor[i]] + b[treatment[i]];
##         p[i] = inv_logit(p[i]);
##     }
##     pulled_left ~ binomial( 1 , p );
## }
## generated quantities{
##     vector[504] log_lik;
##     vector[504] p;
##     for ( i in 1:504 ) {
##         p[i] = a[actor[i]] + b[treatment[i]];
##         p[i] = inv_logit(p[i]);
##     }
##     for ( i in 1:504 ) log_lik[i] = binomial_lpmf( pulled_left[i] | 1 , p[i] );
## }
```

```r
m11.4_stan <- stan( model_code=m11.4_stan_code , data=dat_list , chains=4 )
```

```
## recompiling to avoid crashing R session
```

```
## 
## SAMPLING FOR MODEL '82480dff1a626a42c2ca9de938d65b9d' NOW (CHAIN 1).
## Chain 1: 
## Chain 1: Gradient evaluation took 8e-05 seconds
## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.8 seconds.
## Chain 1: Adjust your expectations accordingly!
## Chain 1: 
## Chain 1: 
## Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 1: 
## Chain 1:  Elapsed Time: 0.629606 seconds (Warm-up)
## Chain 1:                0.545601 seconds (Sampling)
## Chain 1:                1.17521 seconds (Total)
## Chain 1: 
## 
## SAMPLING FOR MODEL '82480dff1a626a42c2ca9de938d65b9d' NOW (CHAIN 2).
## Chain 2: 
## Chain 2: Gradient evaluation took 6.7e-05 seconds
## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.67 seconds.
## Chain 2: Adjust your expectations accordingly!
## Chain 2: 
## Chain 2: 
## Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 2: 
## Chain 2:  Elapsed Time: 0.640387 seconds (Warm-up)
## Chain 2:                0.497498 seconds (Sampling)
## Chain 2:                1.13789 seconds (Total)
## Chain 2: 
## 
## SAMPLING FOR MODEL '82480dff1a626a42c2ca9de938d65b9d' NOW (CHAIN 3).
## Chain 3: 
## Chain 3: Gradient evaluation took 6.7e-05 seconds
## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.67 seconds.
## Chain 3: Adjust your expectations accordingly!
## Chain 3: 
## Chain 3: 
## Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 3: 
## Chain 3:  Elapsed Time: 0.639039 seconds (Warm-up)
## Chain 3:                0.48202 seconds (Sampling)
## Chain 3:                1.12106 seconds (Total)
## Chain 3: 
## 
## SAMPLING FOR MODEL '82480dff1a626a42c2ca9de938d65b9d' NOW (CHAIN 4).
## Chain 4: 
## Chain 4: Gradient evaluation took 9e-05 seconds
## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.9 seconds.
## Chain 4: Adjust your expectations accordingly!
## Chain 4: 
## Chain 4: 
## Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
## Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
## Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
## Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
## Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
## Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
## Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
## Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
## Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
## Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
## Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
## Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
## Chain 4: 
## Chain 4:  Elapsed Time: 0.590849 seconds (Warm-up)
## Chain 4:                0.496972 seconds (Sampling)
## Chain 4:                1.08782 seconds (Total)
## Chain 4:
```

```r
compare( m11.4_stan , m11.4 )
```

```
## Warning in compare(m11.4_stan, m11.4): Not all model fits of same class.
## This is usually a bad idea, because it implies they were fit by different algorithms.
## Check yourself, before you wreck yourself.
```

```
##                WAIC    pWAIC     dWAIC    weight       SE       dSE
## m11.4      532.0787 8.356455 0.0000000 0.5220629 18.84980        NA
## m11.4_stan 532.2553 8.455036 0.1766177 0.4779371 18.91611 0.1730639
```

# 11.1.2. Relative shark and absolute penguin. 

```r
post <- extract.samples(m11.4)
mean( exp(post$b[,4]-post$b[,2]) )
```

```
## [1] 0.9304046
```

# 11.1.3. Aggregated binomial: Chimpanzees again, condensed.

```r
data(chimpanzees)
d <- chimpanzees
d$treatment <- 1 + d$prosoc_left + 2*d$condition
d$side <- d$prosoc_left + 1 # right 1, left 2
d$cond <- d$condition + 1 # no partner 1, partner 2
d_aggregated <- aggregate(
    d$pulled_left ,
    list( treatment=d$treatment , actor=d$actor ,
          side=d$side , cond=d$cond ) ,
    sum )
colnames(d_aggregated)[5] <- "left_pulls"
```

#

```r
dat <- with( d_aggregated , list(
    left_pulls = left_pulls,
    treatment = treatment,
    actor = actor,
    side = side,
    cond = cond ) )
m11.6 <- ulam(
    alist(
        left_pulls ~ dbinom( 18 , p ) ,
        logit(p) <- a[actor] + b[treatment] ,
        a[actor] ~ dnorm( 0 , 1.5 ) ,
        b[treatment] ~ dnorm( 0 , 0.5 )
),
data=dat , chains=4 , log_lik=TRUE )
```

```
## 
## SAMPLING FOR MODEL '79cee3368c3337e807538171c896ad26' NOW (CHAIN 1).
## Chain 1: 
## Chain 1: Gradient evaluation took 2.8e-05 seconds
## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.28 seconds.
## Chain 1: Adjust your expectations accordingly!
## Chain 1: 
## Chain 1: 
## Chain 1: Iteration:   1 / 1000 [  0%]  (Warmup)
## Chain 1: Iteration: 100 / 1000 [ 10%]  (Warmup)
## Chain 1: Iteration: 200 / 1000 [ 20%]  (Warmup)
## Chain 1: Iteration: 300 / 1000 [ 30%]  (Warmup)
## Chain 1: Iteration: 400 / 1000 [ 40%]  (Warmup)
## Chain 1: Iteration: 500 / 1000 [ 50%]  (Warmup)
## Chain 1: Iteration: 501 / 1000 [ 50%]  (Sampling)
## Chain 1: Iteration: 600 / 1000 [ 60%]  (Sampling)
## Chain 1: Iteration: 700 / 1000 [ 70%]  (Sampling)
## Chain 1: Iteration: 800 / 1000 [ 80%]  (Sampling)
## Chain 1: Iteration: 900 / 1000 [ 90%]  (Sampling)
## Chain 1: Iteration: 1000 / 1000 [100%]  (Sampling)
## Chain 1: 
## Chain 1:  Elapsed Time: 0.068287 seconds (Warm-up)
## Chain 1:                0.054636 seconds (Sampling)
## Chain 1:                0.122923 seconds (Total)
## Chain 1: 
## 
## SAMPLING FOR MODEL '79cee3368c3337e807538171c896ad26' NOW (CHAIN 2).
## Chain 2: 
## Chain 2: Gradient evaluation took 1.1e-05 seconds
## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.11 seconds.
## Chain 2: Adjust your expectations accordingly!
## Chain 2: 
## Chain 2: 
## Chain 2: Iteration:   1 / 1000 [  0%]  (Warmup)
## Chain 2: Iteration: 100 / 1000 [ 10%]  (Warmup)
## Chain 2: Iteration: 200 / 1000 [ 20%]  (Warmup)
## Chain 2: Iteration: 300 / 1000 [ 30%]  (Warmup)
## Chain 2: Iteration: 400 / 1000 [ 40%]  (Warmup)
## Chain 2: Iteration: 500 / 1000 [ 50%]  (Warmup)
## Chain 2: Iteration: 501 / 1000 [ 50%]  (Sampling)
## Chain 2: Iteration: 600 / 1000 [ 60%]  (Sampling)
## Chain 2: Iteration: 700 / 1000 [ 70%]  (Sampling)
## Chain 2: Iteration: 800 / 1000 [ 80%]  (Sampling)
## Chain 2: Iteration: 900 / 1000 [ 90%]  (Sampling)
## Chain 2: Iteration: 1000 / 1000 [100%]  (Sampling)
## Chain 2: 
## Chain 2:  Elapsed Time: 0.065495 seconds (Warm-up)
## Chain 2:                0.057862 seconds (Sampling)
## Chain 2:                0.123357 seconds (Total)
## Chain 2: 
## 
## SAMPLING FOR MODEL '79cee3368c3337e807538171c896ad26' NOW (CHAIN 3).
## Chain 3: 
## Chain 3: Gradient evaluation took 9e-06 seconds
## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.09 seconds.
## Chain 3: Adjust your expectations accordingly!
## Chain 3: 
## Chain 3: 
## Chain 3: Iteration:   1 / 1000 [  0%]  (Warmup)
## Chain 3: Iteration: 100 / 1000 [ 10%]  (Warmup)
## Chain 3: Iteration: 200 / 1000 [ 20%]  (Warmup)
## Chain 3: Iteration: 300 / 1000 [ 30%]  (Warmup)
## Chain 3: Iteration: 400 / 1000 [ 40%]  (Warmup)
## Chain 3: Iteration: 500 / 1000 [ 50%]  (Warmup)
## Chain 3: Iteration: 501 / 1000 [ 50%]  (Sampling)
## Chain 3: Iteration: 600 / 1000 [ 60%]  (Sampling)
## Chain 3: Iteration: 700 / 1000 [ 70%]  (Sampling)
## Chain 3: Iteration: 800 / 1000 [ 80%]  (Sampling)
## Chain 3: Iteration: 900 / 1000 [ 90%]  (Sampling)
## Chain 3: Iteration: 1000 / 1000 [100%]  (Sampling)
## Chain 3: 
## Chain 3:  Elapsed Time: 0.075257 seconds (Warm-up)
## Chain 3:                0.074184 seconds (Sampling)
## Chain 3:                0.149441 seconds (Total)
## Chain 3: 
## 
## SAMPLING FOR MODEL '79cee3368c3337e807538171c896ad26' NOW (CHAIN 4).
## Chain 4: 
## Chain 4: Gradient evaluation took 1.1e-05 seconds
## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.11 seconds.
## Chain 4: Adjust your expectations accordingly!
## Chain 4: 
## Chain 4: 
## Chain 4: Iteration:   1 / 1000 [  0%]  (Warmup)
## Chain 4: Iteration: 100 / 1000 [ 10%]  (Warmup)
## Chain 4: Iteration: 200 / 1000 [ 20%]  (Warmup)
## Chain 4: Iteration: 300 / 1000 [ 30%]  (Warmup)
## Chain 4: Iteration: 400 / 1000 [ 40%]  (Warmup)
## Chain 4: Iteration: 500 / 1000 [ 50%]  (Warmup)
## Chain 4: Iteration: 501 / 1000 [ 50%]  (Sampling)
## Chain 4: Iteration: 600 / 1000 [ 60%]  (Sampling)
## Chain 4: Iteration: 700 / 1000 [ 70%]  (Sampling)
## Chain 4: Iteration: 800 / 1000 [ 80%]  (Sampling)
## Chain 4: Iteration: 900 / 1000 [ 90%]  (Sampling)
## Chain 4: Iteration: 1000 / 1000 [100%]  (Sampling)
## Chain 4: 
## Chain 4:  Elapsed Time: 0.070732 seconds (Warm-up)
## Chain 4:                0.068692 seconds (Sampling)
## Chain 4:                0.139424 seconds (Total)
## Chain 4:
```


```r
compare( m11.6 , m11.4 , func=LOO )
```

```
## Warning in compare(m11.6, m11.4, func = LOO): Different numbers of observations found for at least two models.
## Information criteria only valid for comparing models fit to exactly same observations.
## Number of observations for each model:
## m11.6 28 
## m11.4 504
```

```
## Warning: Some Pareto k diagnostic values are too high. See help('pareto-k-diagnostic') for details.
```

```
##            LOO     pLOO     dLOO       weight       SE      dSE
## m11.6 114.1596 8.343023   0.0000 1.000000e+00  8.38159       NA
## m11.4 532.1401 8.387160 417.9806 1.724549e-91 18.87165 9.410309
```


```r
# deviance of aggregated 6-in-9
-2*dbinom(6,9,0.2,log=TRUE)
```

```
## [1] 11.79048
```

```r
# deviance of dis-aggregated
-2*sum(dbern(c(1,1,1,1,1,1,0,0,0),0.2,log=TRUE))
```

```
## [1] 20.65212
```


```r
( k <- LOOPk(m11.6) )
```

```
## Warning: Some Pareto k diagnostic values are too high. See help('pareto-k-diagnostic') for details.
```

```
##  [1] 0.3595387 0.2990879 0.4707500 0.3550722 0.2605353 0.5795076 0.5001489
##  [8] 0.4523195 0.2824575 0.7431238 0.6233017 0.3408837 0.3958567 0.4616791
## [15] 0.3784047 0.3930594 0.2928150 0.7480926 0.3691650 0.2818920 0.4370494
## [22] 0.2569786 0.2594460 0.5460941 0.1931589 0.3163645 0.4599945 0.6171741
```

# 11.1.4. Aggregatedbinomial:Graduateschooladmissions.

```r
data(UCBadmit)
d <- UCBadmit
```


```r
d$gid <- ifelse( d$applicant.gender=="male" , 1 , 2 )
m11.7 <- quap(
    alist(
        admit ~ dbinom( applications , p ) ,
        logit(p) <- a[gid] ,
        a[gid] ~ dnorm( 0 , 1.5 )
    ) , data=d )
precis( m11.7 , depth=2 )
```

```
##            mean         sd       5.5%     94.5%
## a[1] -0.2199857 0.03877483 -0.2819553 -0.158016
## a[2] -0.8295645 0.05073385 -0.9106470 -0.748482
```


```r
post <- extract.samples(m11.7)
diff_a <- post$a[,1] - post$a[,2]
diff_p <- inv_logit(post$a[,1]) - inv_logit(post$a[,2])
precis( list( diff_a=diff_a , diff_p=diff_p ) )
```

```
##             mean         sd      5.5%     94.5%    histogram
## diff_a 0.6106176 0.06423937 0.5068465 0.7123143  ▁▁▁▃▇▇▅▂▁▁▁
## diff_p 0.1416542 0.01446852 0.1181384 0.1643462 ▁▁▁▂▃▇▇▅▂▁▁▁
```


```r
postcheck( m11.7 , n=1e4 )
# draw lines connecting points from same dept
d$dept_id <- rep( 1:7 , each=2 )
```

```
## Error in `$<-.data.frame`(`*tmp*`, dept_id, value = c(1L, 1L, 2L, 2L, : replacement has 14 rows, data has 12
```

```r
for ( i in 1:6 ) {
    x <- 1 + 2*(i-1)
    y1 <- d$admit[x]/d$applications[x]
    y2 <- d$admit[x+1]/d$applications[x+1]
    lines( c(x,x+1) , c(y1,y2) , col=rangi2 , lwd=2 )
    text( x+0.5 , (y1+y2)/2 + 0.05 , d$dept[x] , cex=0.8 , col=rangi2 )
}
```

![](Chap11_Kazu_files/figure-html/R code 11.31-1.png)<!-- -->


```r
d$dept_id <- rep(1:6,each=2)
m11.8 <- quap(
    alist(
        admit ~ dbinom( applications , p ) ,
        logit(p) <- a[gid] + delta[dept_id] ,
        a[gid] ~ dnorm( 0 , 1.5 ) ,
        delta[dept_id] ~ dnorm( 0 , 1.5 )
    ) , data=d )
precis( m11.8 , depth=2 )
```

```
##                mean        sd       5.5%      94.5%
## a[1]     -0.5278865 0.5322783 -1.3785700  0.3227971
## a[2]     -0.4311956 0.5330873 -1.2831720  0.4207809
## delta[1]  1.1079956 0.5350322  0.2529108  1.9630805
## delta[2]  1.0631978 0.5371968  0.2046535  1.9217420
## delta[3] -0.1502661 0.5347763 -1.0049419  0.7044097
## delta[4] -0.1826828 0.5350901 -1.0378602  0.6724946
## delta[5] -0.6246709 0.5378490 -1.4842575  0.2349156
## delta[6] -2.1727307 0.5468627 -3.0467230 -1.2987385
```


```r
post <- extract.samples(m11.8)
diff_a <- post$a[,1] - post$a[,2]
diff_p <- inv_logit(post$a[,1]) - inv_logit(post$a[,2])
precis( list( diff_a=diff_a , diff_p=diff_p ) )
```

```
##               mean         sd        5.5%       94.5%     histogram
## diff_a -0.09783986 0.08156007 -0.22660460 0.034406908 ▁▁▁▂▅▇▇▅▂▁▁▁▁
## diff_p -0.02188642 0.01861675 -0.05223422 0.007521669      ▁▁▂▇▇▂▁▁
```


```r
pg <- sapply( 1:6 , function(k)
    d$applications[d$dept_id==k]/sum(d$applications[d$dept_id==k]) )
rownames(pg) <- c("male","female")
colnames(pg) <- unique(d$dept)
round( pg , 2 )
```

```
##           A    B    C    D    E    F
## male   0.88 0.96 0.35 0.53 0.33 0.52
## female 0.12 0.04 0.65 0.47 0.67 0.48
```

# 11.1.5. Multinomialandcategoricalmodels.  (optional)

# 11.5 Practice
* 10E1. If an event has probability 0.35, what are the log-odds of this event?

```r
log(0.35/(1-0.35))
```

```
## [1] -0.6190392
```

```r
logit(0.35)
```

```
## [1] -0.6190392
```

* 10E2. If an event has log-odds 3.2, what is the probability of this event?

```r
inv_logit(3.2) # 0.9608343
```

```
## [1] 0.9608343
```

```r
logit(inv_logit(3.2)) # 3.2
```

```
## [1] 3.2
```


* 10E3. Suppose that a coefficient in a logistic regression has value 1.7. What does this imply about the proportional change in odds of the outcome?

* 10M1. As explained in the chapter,binomial data can be organized in aggregated and disaggregated forms, without any impact on inference. But the likelihood of the data does change when the data are converted between the two formats. Can you explain why?


