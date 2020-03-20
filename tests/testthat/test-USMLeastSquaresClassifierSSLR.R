library(tidyverse)
library(tidymodels)
library(SSLR)

context("Testing USMLeastSquaresClassifierSSLR")

source("breast.R")
source("helpers.R")


m <- USMLeastSquaresClassifierSSLR()

test_that(
  desc = "USMLeastSquaresClassifierSSLR model",
  code = {

    expect_is(m,"model_sslr")

  }
)


test_that(
  desc = "USMLeastSquaresClassifierSSLR args",
  code = {

    expect_equal(m$args$lambda,0)
    expect_equal(m$args$intercept,TRUE)
    expect_equal(m$args$x_center,FALSE)
    expect_equal(m$args$scale,FALSE)
    expect_equal(m$args$y_scale,FALSE)
    expect_equal(m$args$use_Xu_for_scaling,TRUE)
  }
)

model <- m %>% fit(Class ~ ., data = breast$train)

test_that(
  desc = "USMLeastSquaresClassifierSSLR fit",
  code = {

    expect_is(model,"model_sslr_fitted")
    expect_equal(model$model$mode,"classification")

  }
)


test_that(
  desc = "USMLeastSquaresClassifierSSLR predictions data frame",
  code = {

    predictions_frame <- predict(model,breast$test)
    expect_is(predictions_frame,"data.frame")

  }
)

test_that(
  desc = "USMLeastSquaresClassifierSSLR predictions factor",
  code = {

    predictions_factor <- predict(model,breast$test, type = "raw")
    expect_is(predictions_factor,"factor")

    expect_equal(length(predictions_factor),nrow(breast$test))

  }
)



