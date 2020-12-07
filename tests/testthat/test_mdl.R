context("btm mdl")
library(nmmBtm)

test_that("mdl runs", {
  data(decisions)
  mdl <- btmModel(decisions)
  expect_equal(round(mdl$mle.rel,4), round(0.8013069,4))
})
