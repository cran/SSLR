library(tidyverse)
library(tidymodels)
library(SSLR)

context("Testing EMLeastSquaresClassifierSSLR")

source("breast.R")
source("helpers.R")


m <- EMLeastSquaresClassifierSSLR()

test_that(
  desc = "EMLeastSquaresClassifierSSLR model",
  code = {

    expect_is(m,"model_sslr")

  }
)


test_that(
  desc = "EMLeastSquaresClassifierSSLR args",
  code = {

    expect_equal(m$args$x_center,FALSE)
    expect_equal(m$args$scale,FALSE)
    expect_equal(m$args$verbose,FALSE)
    expect_equal(m$args$intercept,TRUE)
    expect_equal(m$args$lambda,0)
    expect_equal(m$args$eps,1e-09)
    expect_equal(m$args$y_scale,FALSE)
    expect_equal(m$args$alpha,1)
    expect_equal(m$args$beta,1)
    expect_equal(m$args$init,"supervised")
    expect_equal(m$args$method,"block")
    expect_equal(m$args$objective,"label")
    expect_equal(m$args$save_all,FALSE)
    expect_equal(m$args$max_iter,1000)

  }
)

model <- m %>% fit(Class ~ ., data = breast$train)

test_that(
  desc = "EMLeastSquaresClassifierSSLR fit",
  code = {

    expect_is(model,"model_sslr_fitted")
    expect_equal(model$model$mode,"classification")

  }
)


test_that(
  desc = "EMLeastSquaresClassifierSSLR predictions data frame",
  code = {

    predictions_frame <- predict(model,breast$test)
    expect_is(predictions_frame,"data.frame")

  }
)

test_that(
  desc = "EMLeastSquaresClassifierSSLR predictions factor",
  code = {

    predictions_factor <- predict(model,breast$test, type = "raw")
    expect_is(predictions_factor,"factor")

    expect_equal(length(predictions_factor),nrow(breast$test))

  }
)



