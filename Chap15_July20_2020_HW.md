---
title: "Chap15_July13_July20_2020"
author: "Kazu"
date: "7/5/2020"
output: 
  html_document: 
    keep_md: yes
editor_options: 
  chunk_output_type: console
---



# Practice (July 20)
PDF #1 and 2 https://github.com/rmcelreath/statrethinking_winter2019/blob/master/homework/week10.pdf
* 1. Consider the relationship between brain volume (brain) and bodymass(body) in the data(Primates301). These values are presented as single values for each species. However, there is always a range of sizes in a species, and some of these measurements are taken from very small samples. So these values are measured with some unknown error.
We don’t have the raw measurements to work with—that would be best. But we can imagine what might happen if we had them. Suppose error is proportional to the measurement. This makes sense, because larger animals have larger variation. As a consequence, the uncertainty is not uniform across the values and this could mean trouble.
Let’s make up some standard errors for these measurements, to see what might happen. Load the data and scale the the measurements so the maximum is 1 in both cases:

```r
data(Primates301)
d <- Primates301
cc <- complete.cases( d$brain , d$body )
B <- d$brain[cc]
M <- d$body[cc]
B <- B / max(B)
M <- M / max(M)
```
* Now I’ll make up some standard errors for B and M, assuming error is 10% of the measurement.

```r
Bse <- B*0.1
Mse <- M*0.1
```
* Let’s model these variables with this relationship:
* Bi ∼ Log-Normal(μi, σ)
* μi =α+βlogMi 1

* This says that brain volume is a log-normal variable, and the mean on the log scale is given by μ. What this model implies is that the expected value of B is:
* E(Bi|Mi) = exp(α)Mβi
* So this is a standard allometric scaling relationship—incredibly common in biology.
* Ignoring measurement error, the corresponding ulam model is:

```r
dat_list <- list(
    B = B,
M=M)
m1.1 <- ulam(
    alist(
        B ~ dlnorm( mu , sigma ),
        mu <- a + b*log(M),
        a ~ normal(0,1),
        b ~ normal(0,1),
        sigma ~ exponential(1)
    ) , data=dat_list,chains=4,cores=2,iter=2000)
```
* Your job is to add the measurement errors to this model. Use the divorce/marriage example in the chapter as a guide. It might help to initialize the unobserved true values of B and M using the observed values, by adding a list like this to ulam:

```r
 start=list( M_true=dat_list$M , B_true=dat_list$B,log_lik = T)
```

# Answer (Kazu)

```r
# dlist <- list(
#     D_obs = standardize( d$Divorce ),
#     D_sd = d$Divorce.SE / sd( d$Divorce ),
#     M_obs = standardize( d$Marriage ),
#     M_sd = d$Marriage.SE / sd( d$Marriage ),
#     A = standardize( d$MedianAgeMarriage ),
#     N = nrow(d)
# )
# 
# m15.2 <- ulam(
#     alist(
#         D_obs ~ dnorm( D_true , D_sd ),
#         vector[N]:D_true ~ dnorm( mu , sigma ),
#         mu <- a + bA*A + bM*M_true[i],
#         M_obs ~ dnorm( M_true , M_sd ),
#         vector[N]:M_true ~ dnorm( 0 , 1 ),
#         a ~ dnorm(0,0.2),
#         bA ~ dnorm(0,0.5),
#         bM ~ dnorm(0,0.5),
#         sigma ~ dexp( 1 )
#     ) , data=dlist , chains=4 , cores=4 )
dat_list$M_sd <- Mse/sd(dat_list$M)
dat_list$B_sd <- Bse/sd(dat_list$B)
dat_list$N <- sum(cc)

m1.2 <- ulam(
    alist(
        B_obs ~ dnorm(B_true,B_sd),
        vector[N]:B_true ~ dlnorm( mu , sigma ),
        mu <- a + b*log(M_true[i]),
        M_obs ~dnorm(M_true,M_sd),
        vector[N]:M_true ~ dnorm(0,1),
        a ~ normal(0,1),
        b ~ normal(0,1),
        sigma ~ exponential(1)
    ) , data=dat_list,
    start=list( M_true=dat_list$M , B_true=dat_list$B ),
    chains=4,cores=2,iter=5000)
```

```
## Warning: There were 679 divergent transitions after warmup. Increasing adapt_delta above 0.95 may help. See
## http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
```

```
## Warning: There were 9321 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
## http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
```

```
## Warning: There were 4 chains where the estimated Bayesian Fraction of Missing Information was low. See
## http://mc-stan.org/misc/warnings.html#bfmi-low
```

```
## Warning: Examine the pairs() plot to diagnose sampling problems
```

```
## Warning: The largest R-hat is 2.29, indicating chains have not mixed.
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
# "log_lik = T" gave me " no parameter log_lik; sampling not done"
```

* Why following is not used?

```r
dat_list2 <- list(
  M_true=dat_list$M, 
  B_true=dat_list$B,
  M_sd = Mse/sd(dat_list$M),
  B_sd = Bse/sd(dat_list$B),
  N = sum(cc)
  )
m1.3 <- ulam(
    alist(
        B_obs ~ dnorm(B_true,B_sd),
        vector[N]:B_true ~ dlnorm( mu , sigma ),
        mu <- a + b*log(M_true[i]),
        M_obs ~dnorm(M_true,M_sd),
        vector[N]:M_true ~ dnorm(0,1),
        a ~ normal(0,1),
        b ~ normal(0,1),
        sigma ~ exponential(1)
    ) , data=dat_list2,
    chains=4,cores=2,iter=5000)
# "log_lik = T" gave me " no parameter log_lik; sampling not done"
```


* Compare the inference of the measurement error model to those of m1.1 above. Has anything changed? Why or why not?

```r
precis( m1.1 )
```

```
##            mean         sd      5.5%     94.5%    n_eff    Rhat4
## a     0.4248397 0.05830083 0.3358272 0.5176797 1407.049 1.001773
## b     0.7827608 0.01394136 0.7613044 0.8049917 1394.492 1.001632
## sigma 0.2931249 0.01588241 0.2684888 0.3197946 1848.221 1.001202
```

```r
precis( m1.2 )
```

```
## 364 vector or matrix parameters hidden. Use depth=2 to show them.
```

```
##              mean          sd        5.5%       94.5%     n_eff    Rhat4
## B_obs  0.02492230 0.008264683  0.01541565  0.03859558  2.076294 5.775400
## M_obs  0.10971547 0.012749509  0.08827807  0.12855483  8.812306 1.775405
## a     -1.74184304 0.795104344 -3.13155390 -0.69504763 11.507869 1.235596
## b      0.90449085 0.352450588  0.28134708  1.40125727 18.793433 1.149344
## sigma  0.03205633 0.026222208  0.00719762  0.08354364 11.022875 1.399632
```

```r
precis( m1.3 )
```

```
##              mean           sd         5.5%       94.5%     n_eff     Rhat4
## B_obs 0.009880079 0.0008122569 0.0085977954 0.011181162  8358.096 1.0003377
## M_obs 0.001064171 0.0001599976 0.0008064922 0.001323233 11467.943 0.9997315
## a     0.427704623 0.0582232458 0.3350206536 0.520088216  3001.863 1.0000664
## b     0.783333064 0.0141211854 0.7610687243 0.805979506  2989.625 1.0003048
## sigma 0.293336687 0.0154994744 0.2696330248 0.318692457  3918.028 1.0001735
```


```r
compare(m1.1,m1.2) # "log_lik = T" did not work in m1.2 , os I am not able to use compare()
```

```
## Warning in compare(m1.1, m1.2): Different numbers of observations found for at least two models.
## Model comparison is valid only for models fit to exactly the same observations.
## Number of observations for each model:
## m1.1 182 
## m1.2 N
```

```
## Error in check_pars(allpars, pars): no parameter log_lik
```


# 2. Now consider missing values—this data set is lousy with them. You can ignore measurement error in this problem. Let’s get a quick idea of the missing values by counting them in each variable:

```r
library(rethinking)
data(Primates301)
d <- Primates301
colSums( is.na(d) )
```

```
##                name               genus             species          subspecies 
##                   0                   0                   0                 267 
##              spp_id            genus_id     social_learning     research_effort 
##                   0                   0                  98                 115 
##               brain                body          group_size           gestation 
##                 117                  63                 114                 161 
##             weaning           longevity        sex_maturity maternal_investment 
##                 185                 181                 194                 197
```

* We’ll continue to focus on just brain and body, to stave off insanity. Consider only those species with measured body masses:

```r
cc <- complete.cases( d$body )
M <- d$body[cc]
M <- M / max(M)
B <- d$brain[cc]
B <- B / max( B , na.rm=TRUE )
```

* You should end up with 238 species and 56 missing brain values among them. First, consider whether there is a pattern to the missing values. Does it look like missing values are associated with particular values of body mass? Draw a DAG that represents how missingness works in this case. Which type (MCAR, MAR, MNAR)
is this?
Second, impute missing values for brain size. It might help to initialize the 56
imputed variables to a valid value:

```r
 start=list( B_impute=rep(0.5,56) )
```

* This just helps the chain get started.
* Compare the inferences to an analysis that drops all the missing values. Has any-
thing changed? Why or why not? Hint: Consider the density of data in the ranges where there are missing values. You might want to plot the imputed brain sizes to- gether with the observed values.

# optional 15H4

