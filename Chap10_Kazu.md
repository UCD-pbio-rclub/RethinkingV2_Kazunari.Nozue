---
title: "Chap10"
author: "Kazu"
date: "10/18/2019"
output: 
  html_document: 
    keep_md: yes
editor_options: 
  chunk_output_type: console
---




```r
p <- list()
p$A <- c(0,0,10,0,0)
p$B <- c(0,1,8,1,0)
p$C <- c(0,2,6,2,0)
p$D <- c(1,2,4,2,1)
p$E <- c(2,2,2,2,2)
```
* normalize each such that it is a probability distribution

```r
 p_norm <- lapply( p , function(q) q/sum(q))
```

* Since these are now probability distributions, we can compute the information entropy of each. The only trick here is to remember L’Hôpital’s rule (see page 207):

```r
 ( H <- sapply( p_norm , function(q) -sum(ifelse(q==0,0,q*log(q))) ) )
```

```
##         A         B         C         D         E 
## 0.0000000 0.6390319 0.9502705 1.4708085 1.6094379
```

* The bottom-right plot in Figure 10.1 displays these logwayspp values against the infor- mation entropies H. These two sets of values contain the same information, as information entropy is an approximation of the log ways per pebble (see the Overthinking box at the end for details).

```r
ways <- c(1,90,1260,37800,113400)
logwayspp <- log(ways)/10
```
* This is useful, because the distribution that can happen the greatest number of ways is the most plausible distribution. Call this distribution the maximum entropy distribution.

# 10.1.1 Gaussian

# 10.1.2 Binomial

```r
# build list of the candidate distributions
p <- list()
p[[1]] <- c(1/4,1/4,1/4,1/4)
p[[2]] <- c(2/6,1/6,1/6,2/6)
p[[3]] <- c(1/6,2/6,2/6,1/6)
p[[4]] <- c(1/8,4/8,2/8,1/8)
# compute expected value of each
sapply( p , function(p) sum(p*c(0,1,1,2)) )
```

```
## [1] 1 1 1 1
```


```r
# compute entropy of each distribution
sapply( p , function(p) -sum( p*log(p) ) )
```

```
## [1] 1.386294 1.329661 1.329661 1.213008
```


```r
p <- 0.7
( A <- c( (1-p)^2 , p*(1-p) , (1-p)*p , p^2 ) )
```

```
## [1] 0.09 0.21 0.21 0.49
```


```r
 -sum( A*log(A) )
```

```
## [1] 1.221729
```

* simulate random probability distributions that have any specified expected value. 

```r
sim.p <- function(G=1.4) {
    x123 <- runif(3)
    x4 <- ( (G)*sum(x123)-x123[2]-x123[3] )/(2-G)
    z <- sum( c(x123,x4) )
    p <- c( x123 , x4 )/z
    list( H=-sum( p*log(p) ) , p=p )
}
```

* This function generates a random distribution with expected value G and then returns its entropy along with the distribution. We want to invoke this function a large number of times. Here is how to call it 100000 times and then plot the distribution of resulting entropies:

```r
H <- replicate( 1e5 , sim.p(1.4) )
dens( as.numeric(H[1,]) , adj=0.1 )
```

![](Chap10_Kazu_files/figure-html/R code 10.10-1.png)<!-- -->


```r
entropies <- as.numeric(H[1,])
distributions <- H[2,]
```


```r
 max(entropies)
```

```
## [1] 1.221728
```


```r
 distributions[ which.max(entropies) ]
```

```
## [[1]]
## [1] 0.08973709 0.21007035 0.21045547 0.48973709
```

# practice 
* https://github.com/rmcelreath/statrethinking_winter2019/blob/master/homework/week05.pdf
* 1. Consider the data (Wines 2012) data table.These data are expert ratings of 20 different French and American wines by 9 different French and American judges. Your goal is to model score, the subjective rating assigned by each judge to each wine. I recommend standardizing it.

## read data

```r
data(Wines2012)
Wines2012 %>% View()
summary(Wines2012)
```

```
##              judge      flight        wine         score     
##  Daniele Meulder:20   red  :90   A1     :  9   Min.   : 7.0  
##  Francis Schott :20   white:90   A2     :  9   1st Qu.:12.0  
##  Jamal Rayyis   :20              B1     :  9   Median :14.5  
##  Jean-M Cardebat:20              B2     :  9   Mean   :14.2  
##  John Foy       :20              C1     :  9   3rd Qu.:16.0  
##  Linda Murphy   :20              C2     :  9   Max.   :19.5  
##  (Other)        :60              (Other):126                 
##    wine.amer     judge.amer    
##  Min.   :0.0   Min.   :0.0000  
##  1st Qu.:0.0   1st Qu.:0.0000  
##  Median :1.0   Median :1.0000  
##  Mean   :0.6   Mean   :0.5556  
##  3rd Qu.:1.0   3rd Qu.:1.0000  
##  Max.   :1.0   Max.   :1.0000  
## 
```

## standardize data by judge

```r
Wines2012 %>% group_by(judge) %>% mutate(score_norm=score/sum(score)) -> Wines2012 
Wines2012 %>% summarize(all=sum(score_norm)) # this should be one. OK.
```

```
## # A tibble: 9 x 2
##   judge             all
##   <fct>           <dbl>
## 1 Daniele Meulder     1
## 2 Francis Schott      1
## 3 Jamal Rayyis        1
## 4 Jean-M Cardebat     1
## 5 John Foy            1
## 6 Linda Murphy        1
## 7 Olivier Gergaud     1
## 8 Robert Hodgson      1
## 9 Tyler Colman        1
```

```r
# maybe this way?
Wines2012 %>% mutate(score_norm2=scale(score)) -> Wines2012
```

* In this first problem, consider only variation among judges and wines. 
Construct index variables of judge and wine and then use these index variables to construct a linear regression model. 

```r
# index wine
Wines2012 <- Wines2012 %>% group_by(judge) %>% mutate(wine_index=1:n()) #%>% View()
# index judge
Wines2012 <- Wines2012 %>% ungroup() %>% group_by(wine) %>% mutate(judge_index=1:n())  #%>% View()
# score_norm ~ judge*wines
## score_norm ~ a[judge.index] + b[judge.index]*[wine.index] true???
```

Justify your priors. You should end up with 9 judge parameters and 20 wine parameters.

```r
# check
Wines2012 %>% View() # select each judge to see wine index and vise versa for wine
# make it to list object
Wines2012.list<-list(
  score_norm=Wines2012$score_norm,
  score_norm2=Wines2012$score_norm2,
  judge_index=Wines2012$judge_index,
  wine_index=Wines2012$wine_index
)
```

Use ulam instead of quap to build this model, and be sure to check the chains for convergence. 
# score_norm got error

```r
practice.m1_score_norm<-ulam(
    alist(
         score_norm ~ dnorm( mu , sigma ) , # error
        #score_norm2 ~ dnorm( mu , sigma ) , # no error
        # mu <- a[judge.inex] +  b[judge.index]*wine.index,
        mu <- a[judge_index] + b[wine_index], 
        a[judge_index] ~ dnorm(0 , 1 ) ,
        b[wine_index] ~ dnorm(0 , 1 ) ,
        sigma ~ dexp(1)
),
data=Wines2012.list, chains=1,cores=2,iter=2000) # errors....
```

```
## 
## SAMPLING FOR MODEL 'dd18611db922f6c84e626fbcb725fab9' NOW (CHAIN 1).
## Chain 1: 
## Chain 1: Gradient evaluation took 3.8e-05 seconds
## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.38 seconds.
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
## Chain 1:  Elapsed Time: 1.01444 seconds (Warm-up)
## Chain 1:                1.33052 seconds (Sampling)
## Chain 1:                2.34496 seconds (Total)
## Chain 1:
```

```
## Warning: There were 30 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
## http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
```

```
## Warning: Examine the pairs() plot to diagnose sampling problems
```

```
## Warning: The largest R-hat is 1.27, indicating chains have not mixed.
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
# 
```


```r
practice.m1_score_norm2<-ulam(
    alist(
        # score_norm ~ dnorm( mu , sigma ) , # error
        score_norm2 ~ dnorm( mu , sigma ) , # no error
        # mu <- a[judge.inex] +  b[judge.index]*wine.index,
        mu <- a[judge_index] + b[wine_index], 
        a[judge_index] ~ dnorm(0 , 1 ) ,
        b[wine_index] ~ dnorm(0 , 1 ) ,
        sigma ~ dexp(1)
),
data=Wines2012.list, chains=1,cores=2,iter=1000)
```

```
## 
## SAMPLING FOR MODEL '639b1b4b70cb3d837e9b9bf05b263cdc' NOW (CHAIN 1).
## Chain 1: 
## Chain 1: Gradient evaluation took 2.9e-05 seconds
## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.29 seconds.
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
## Chain 1:  Elapsed Time: 0.125945 seconds (Warm-up)
## Chain 1:                0.071377 seconds (Sampling)
## Chain 1:                0.197322 seconds (Total)
## Chain 1:
```

```r
# 
```



If you’d rather build the model directly in Stan or PyMC3, go ahead. I just want you to use Hamiltonian Monte Carlo instead of quadratic approximation.

How do you interpret the variation among individual judges and indi- vidual wines? Do you notice any patterns, just by plotting the differences? Which judges gave the highest/lowest ratings? Which wines were rated worst/ best on average?
* score_norm with errors



```r
traceplot(practice.m1_score_norm )
```

```
## Waiting to draw page 2 of 2
```

![](Chap10_Kazu_files/figure-html/unnamed-chunk-2-1.png)<!-- -->![](Chap10_Kazu_files/figure-html/unnamed-chunk-2-2.png)<!-- -->


```r
plot(precis(practice.m1_score_norm2,2))
```

![](Chap10_Kazu_files/figure-html/unnamed-chunk-3-1.png)<!-- -->



```r
pairs(practice.m1_score_norm2)
```

```
## Error in plot.new(): figure margins too large
```

```r
png(file="practice.m1_score_norm2.png",width=20,height=20,units="in",res=100)
pairs(practice.m1_score_norm2)
dev.off()
```

```
## quartz_off_screen 
##                 2
```


```r
traceplot(practice.m1_score_norm2 )
```

```
## Waiting to draw page 2 of 2
```

![](Chap10_Kazu_files/figure-html/unnamed-chunk-5-1.png)<!-- -->![](Chap10_Kazu_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

## 2. Now consider three features of the wines and judges:
* (1) flight: Whether the wine is red or white.
* (2) wine.amer: Indicator variable for American wines. 
* (3) judge.amer: Indicator variable for American judges.
* Use indicator or index variables to model the influence of these features on the scores. Omit the individual judge and wine index variables from Problem 1. Do not include interaction effects yet. Again use ulam, justify your priors, and be sure to check the chains. What do you conclude about the differences among the wines and judges? Try to relate the results to the inferences in Problem 1.
# indexing

```r
Wines2012.list$flight_index<-ifelse(Wines2012$flight=="white",1,2)
Wines2012.list$wine_amer_index<-ifelse(Wines2012$wine.amer==0,1,2)
Wines2012.list$judge_amer_index<-ifelse(Wines2012$judge.amer==0,1,2)
```


```r
practice.m2_score_norm2<-ulam(
    alist(
        score_norm2 ~ dnorm( mu , sigma ) , # no error
        mu <- a[flight_index] + b[wine_amer_index] + c[judge_amer_index], 
        a[flight_index] ~ dnorm(0 , 1),
        b[wine_amer_index] ~ dnorm(0 , 1),
        c[judge_amer_index] ~ dnorm(0,1),
        sigma ~ dexp(1)
),
data=Wines2012.list, chains=1,cores=2,iter=2000) # errors....
```

```
## 
## SAMPLING FOR MODEL 'd56d06282eb11bcd91de91707e83528b' NOW (CHAIN 1).
## Chain 1: 
## Chain 1: Gradient evaluation took 4.3e-05 seconds
## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.43 seconds.
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
## Chain 1:  Elapsed Time: 0.54396 seconds (Warm-up)
## Chain 1:                0.55393 seconds (Sampling)
## Chain 1:                1.09789 seconds (Total)
## Chain 1:
```

```r
# 
```


```r
plot(precis(practice.m2_score_norm2,2))
```

![](Chap10_Kazu_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
pairs(practice.m2_score_norm2)
```

![](Chap10_Kazu_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
png(file="practice.m2_score_norm2.png",width=20,height=20,units="in",res=100)
pairs(practice.m2_score_norm2)
dev.off()
```

```
## quartz_off_screen 
##                 2
```


```r
traceplot(practice.m2_score_norm2 )
```

![](Chap10_Kazu_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


## 3. Now consider two-way interactions among the three features.You should end up with three different interaction terms in your model. These will be easier to build, if you use indicator variables. Again use ulam, justify your priors, and be sure to check the chains. Explain what each interaction means. Be sure to interpret the model’s predictions on the outcome scale (mu, the expected score), not on the scale of individual parameters. You can use link to help with this, or just use your knowledge of the linear model instead.
* What do you conclude about the features and the scores? Can you relate the results of your model(s) to the individual judge and wine inferences from Problem 1?
### needs to learn how to do with interaction .... help me! which page in the textbook?

```r
practice.m3_score_norm2_interaction<-ulam(
    alist(
        score_norm2 ~ dnorm( mu , sigma ) , # no error
        mu <- a[flight_index] + b[wine_amer_index] + c[judge_amer_index] + d[flight_index]*[wine_amer_index]+e[wine_amer_index]*[judge_amer_index] + f[judge_amer_index]*[flight_index], 
        a[flight_index] ~ dnorm(0 , 1),
        b[wine_amer_index] ~ dnorm(0 , 1),
        c[judge_amer_index] ~ dnorm(0,1),
        d[flight_index]*[wine_amer_index] ~ dnorm(0,1),
        e[wine_amer_index]*[judge_amer_index] ~ dnorm(0,1),
        f[judge_amer_index]*[flight_index] ~ dnorm(0,1),
        sigma ~ dexp(1)
),
data=Wines2012.list, chains=1,cores=2,iter=2000) # errors....
# 
```


```r
link() #...
```

```
## Error in print(class(fit)): argument "fit" is missing, with no default
```


# 10.2. Generalized inear models
##### under construction ####


