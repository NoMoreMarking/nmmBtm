
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nmmBtm

<!-- badges: start -->
<!-- badges: end -->

The goal of nmmBtm is to help researchers analyse the data from
www.nomoremarking.com

## Installation

You can install the the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("NoMoreMarking/nmmBtm")
```

## Examples

Using the decisions data downloaded from www.nomoremarking.com you can
fit your own btm model using the following:

``` r
require(nmmBtm)
#> Loading required package: nmmBtm
data("decisions")
mdl <- nmmBtm::btmModel(decisions)
#> Warning in btm(df, fix.eta = 0, ignore.ties = TRUE, eps = 0.3, fix.theta =
#> anchors): NAs introduced by coercion
#> **** Iteration 1 | Maximum parameter change=0.5182712
#> **** Iteration 2 | Maximum parameter change=0.1518327
#> **** Iteration 3 | Maximum parameter change=0.0185697
#> **** Iteration 4 | Maximum parameter change=0.0052206
#> **** Iteration 5 | Maximum parameter change=0.0012743
#> **** Iteration 6 | Maximum parameter change=0.0004125
#> **** Iteration 7 | Maximum parameter change=0.0001675
#> **** Iteration 8 | Maximum parameter change=6.31e-05
print(summary(mdl))
#> ------------------------------------------------------------
#> sirt 3.6-21 (2019-08-01 18:42:53) 
#> R version 3.6.1 (2019-07-05) x86_64, darwin15.6.0 | nodename=Christophers-iMac.local | login=root 
#> Date of Analysis: 2020-12-07 15:21:03 
#> Time difference of 0.01627707 secs
#> Computation Time: 0.01627707 
#> 
#> 
#> Call:
#> btm(data = df, ignore.ties = TRUE, fix.eta = 0, fix.theta = anchors, 
#>     eps = 0.3)
#> 
#> Bradley-Terry Model with Ties and Home Advantage Parameters
#> ------------------------------------------------------------
#> Number of iterations = 8 
#> Number of individuals = 8 
#> Number of pairwise comparisons = 81 
#> ------------------------------------------------------------
#> Ties and Home advantage parameters
#>   parlabel   par est se
#> 1     Ties delta -99 NA
#> 2     Home   eta   0 NA
#> ------------------------------------------------------------
#> Summary of individual effects parameters
#>   M  median     SD     min    max
#> 1 0 -0.0411 1.4699 -2.5251 1.7583
#> ------------------------------------------------------------
#> MLE reliability (separation reliability)
#> MLE Rel=0.8013
#> Separation index=2.2434
#> ------------------------------------------------------------
#> Individual effects parameters
#>   individual id Ntot N1 ND N0   score propscore   theta se.theta outfit  infit
#> 1     5SDTAF  1   20 17  0  3 16.7900    0.8395  1.6182   0.6756 0.2833 0.5234
#> 2     BS696G  2   21  7  0 14  7.1000    0.3381 -0.8097   0.5501 1.0250 0.9997
#> 3     EEJPZC  3   20  8  0 12  8.0600    0.4030 -0.3380   0.5572 0.6388 0.8529
#> 4     JPETNT  4   21 12  0  9 11.9571    0.5694  0.2557   0.5167 0.7273 0.7835
#> 5     JWUKN5  5   20  1  0 19  1.2700    0.0635 -2.5251   0.9427 3.6951 0.9934
#> 6     NNUAHM  6   20 15  0  5 14.8500    0.7425  1.0694   0.6122 1.0219 1.1320
#> 7     UP2FUQ  7   20  4  0 16  4.1800    0.2090 -1.0288   0.6296 0.3789 0.6238
#> 8     VDT6WV  8   20 17  0  3 16.7900    0.8395  1.7583   0.6617 4.1465 1.2546
#> NULL
```

You can estimate judge infit using the following:

``` r
data("decisions")
mdl <- btmModel(decisions)
#> Warning in btm(df, fix.eta = 0, ignore.ties = TRUE, eps = 0.3, fix.theta =
#> anchors): NAs introduced by coercion
#> **** Iteration 1 | Maximum parameter change=0.5182712
#> **** Iteration 2 | Maximum parameter change=0.1518327
#> **** Iteration 3 | Maximum parameter change=0.0185697
#> **** Iteration 4 | Maximum parameter change=0.0052206
#> **** Iteration 5 | Maximum parameter change=0.0012743
#> **** Iteration 6 | Maximum parameter change=0.0004125
#> **** Iteration 7 | Maximum parameter change=0.0001675
#> **** Iteration 8 | Maximum parameter change=6.31e-05
probs <- mdl$probs
judge.infit <- btm_fit_2(probs,decisions)
print(judge.infit)
#>    judgeName       out1 n1    wvar1     win1     infit
#> 1 j1@nmm.com   9.674626 27 3.586837 1.963222 0.5473409
#> 2 j2@nmm.com   9.201085 27 3.448044 1.844541 0.5349529
#> 3 j3@nmm.com 101.168143 27 3.496097 5.546974 1.5866191
```

You can convert theta values to scaled scores:

``` r
data("decisions")
mdl <- btmModel(decisions)
#> Warning in btm(df, fix.eta = 0, ignore.ties = TRUE, eps = 0.3, fix.theta =
#> anchors): NAs introduced by coercion
#> **** Iteration 1 | Maximum parameter change=0.5182712
#> **** Iteration 2 | Maximum parameter change=0.1518327
#> **** Iteration 3 | Maximum parameter change=0.0185697
#> **** Iteration 4 | Maximum parameter change=0.0052206
#> **** Iteration 5 | Maximum parameter change=0.0012743
#> **** Iteration 6 | Maximum parameter change=0.0004125
#> **** Iteration 7 | Maximum parameter change=0.0001675
#> **** Iteration 8 | Maximum parameter change=6.31e-05
scaledScores <- scaleThetas(mdl$effects, 20, 0)
print(scaledScores)
#>   scaledScore scaledScoreSE
#> 1   19.345821      3.154566
#> 2    8.009676      2.568673
#> 3   10.212015      2.601785
#> 4   12.984408      2.412689
#> 5    0.000000      4.401762
#> 6   16.783686      2.858315
#> 7    6.986465      2.939606
#> 8   20.000000      3.089486
```

You can simulate a new set of model parameters from the initial model
estimates:

``` r
data("decisions")
sims <- btmSimulate(decisions, NULL, 2)
#> Warning in btm(df, fix.eta = 0, ignore.ties = TRUE, eps = 0.3, fix.theta =
#> anchors): NAs introduced by coercion
#> **** Iteration 1 | Maximum parameter change=0.5182712
#> **** Iteration 2 | Maximum parameter change=0.1518327
#> **** Iteration 3 | Maximum parameter change=0.0185697
#> **** Iteration 4 | Maximum parameter change=0.0052206
#> **** Iteration 5 | Maximum parameter change=0.0012743
#> **** Iteration 6 | Maximum parameter change=0.0004125
#> **** Iteration 7 | Maximum parameter change=0.0001675
#> **** Iteration 8 | Maximum parameter change=6.31e-05
#> Warning in btm(df, fix.eta = 0, ignore.ties = TRUE, eps = 0.3, fix.theta =
#> anchors): NAs introduced by coercion
#> **** Iteration 1 | Maximum parameter change=0.9212077
#> **** Iteration 2 | Maximum parameter change=0.6339387
#> **** Iteration 3 | Maximum parameter change=0.3206001
#> **** Iteration 4 | Maximum parameter change=0.1108236
#> **** Iteration 5 | Maximum parameter change=0.044231
#> **** Iteration 6 | Maximum parameter change=0.0208232
#> **** Iteration 7 | Maximum parameter change=0.0097585
#> **** Iteration 8 | Maximum parameter change=0.0045156
#> **** Iteration 9 | Maximum parameter change=0.0021358
#> **** Iteration 10 | Maximum parameter change=0.000973
#> **** Iteration 11 | Maximum parameter change=0.000468
#> **** Iteration 12 | Maximum parameter change=0.0002089
#> **** Iteration 13 | Maximum parameter change=0.0001029
#> **** Iteration 14 | Maximum parameter change=4.46e-05
#> Warning in btm(df, fix.eta = 0, ignore.ties = TRUE, eps = 0.3, fix.theta =
#> anchors): NAs introduced by coercion
#> **** Iteration 1 | Maximum parameter change=0.3418373
#> **** Iteration 2 | Maximum parameter change=0.0979899
#> **** Iteration 3 | Maximum parameter change=0.028451
#> **** Iteration 4 | Maximum parameter change=0.008851
#> **** Iteration 5 | Maximum parameter change=0.0025428
#> **** Iteration 6 | Maximum parameter change=0.000896
#> **** Iteration 7 | Maximum parameter change=0.0002191
#> **** Iteration 8 | Maximum parameter change=9.66e-05
print(head(sims))
#>   individual      theta iteration
#> 1     5SDTAF  1.2409106         1
#> 2     BS696G -0.9964422         1
#> 3     EEJPZC -0.1313070         1
#> 4     JPETNT  0.3791734         1
#> 5     JWUKN5 -4.2610559         1
#> 6     NNUAHM  3.3751090         1
```

You can bootstrap sample the decisions file to produce new estimates:

``` r
data("decisions")
bootstrapped <- btmBootstrap(decisions, 3, size = 0.8, replace = FALSE)
#> Warning in btm(df, fix.eta = 0, ignore.ties = TRUE, eps = 0.3, fix.theta =
#> anchors): NAs introduced by coercion
#> **** Iteration 1 | Maximum parameter change=0.5182712
#> **** Iteration 2 | Maximum parameter change=0.1518327
#> **** Iteration 3 | Maximum parameter change=0.0185697
#> **** Iteration 4 | Maximum parameter change=0.0052206
#> **** Iteration 5 | Maximum parameter change=0.0012743
#> **** Iteration 6 | Maximum parameter change=0.0004125
#> **** Iteration 7 | Maximum parameter change=0.0001675
#> **** Iteration 8 | Maximum parameter change=6.31e-05
#> Warning in btm(df, fix.eta = 0, ignore.ties = TRUE, eps = 0.3, fix.theta =
#> anchors): NAs introduced by coercion
#> **** Iteration 1 | Maximum parameter change=0.4634309
#> **** Iteration 2 | Maximum parameter change=0.1180917
#> **** Iteration 3 | Maximum parameter change=0.0159033
#> **** Iteration 4 | Maximum parameter change=0.0043748
#> **** Iteration 5 | Maximum parameter change=0.0016916
#> **** Iteration 6 | Maximum parameter change=0.0006703
#> **** Iteration 7 | Maximum parameter change=0.0003558
#> **** Iteration 8 | Maximum parameter change=0.0001729
#> **** Iteration 9 | Maximum parameter change=9.11e-05
#> Warning in btm(df, fix.eta = 0, ignore.ties = TRUE, eps = 0.3, fix.theta =
#> anchors): NAs introduced by coercion
#> **** Iteration 1 | Maximum parameter change=0.3454189
#> **** Iteration 2 | Maximum parameter change=0.0547988
#> **** Iteration 3 | Maximum parameter change=0.008791
#> **** Iteration 4 | Maximum parameter change=0.002815
#> **** Iteration 5 | Maximum parameter change=0.0011099
#> **** Iteration 6 | Maximum parameter change=0.0004286
#> **** Iteration 7 | Maximum parameter change=0.0001742
#> **** Iteration 8 | Maximum parameter change=7.4e-05
#> Warning in btm(df, fix.eta = 0, ignore.ties = TRUE, eps = 0.3, fix.theta =
#> anchors): NAs introduced by coercion
#> **** Iteration 1 | Maximum parameter change=0.4665048
#> **** Iteration 2 | Maximum parameter change=0.1342726
#> **** Iteration 3 | Maximum parameter change=0.0359379
#> **** Iteration 4 | Maximum parameter change=0.0103298
#> **** Iteration 5 | Maximum parameter change=0.0035475
#> **** Iteration 6 | Maximum parameter change=0.0010422
#> **** Iteration 7 | Maximum parameter change=0.0003716
#> **** Iteration 8 | Maximum parameter change=0.0001101
#> **** Iteration 9 | Maximum parameter change=4.12e-05
print(head(bootstrapped))
#>   individual      theta iteration
#> 1     5SDTAF  1.4837524         1
#> 2     BS696G -0.6983814         1
#> 3     EEJPZC -0.1296823         1
#> 4     JPETNT  0.1667334         1
#> 5     JWUKN5 -2.3566765         1
#> 6     NNUAHM  0.9851683         1
```
