
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

## Example

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
#> Date of Analysis: 2020-12-07 13:29:34 
#> Time difference of 0.01632404 secs
#> Computation Time: 0.01632404 
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
