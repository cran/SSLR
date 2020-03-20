## ----eval=FALSE---------------------------------------------------------------
#  
#  library(SSLR)
#  library(tidymodels)

## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  digits = 3,
  collapse = TRUE,
  comment = "#>"
)
options(digits = 3)

library(SSLR)
library(tidymodels)

## ----airquality, results="hide"-----------------------------------------------
set.seed(1)

data <- airquality

#Train and test data
train.index  <- sample(nrow(data), round(0.7 * nrow(data)))
train <- data[ train.index,]
test  <- data[-train.index,]

cls <- which(colnames(wine) == "Ozone")

#% LABELED
labeled.index <- sample(nrow(train), round(0.1 * nrow(train)))
train[-labeled.index,cls] <- NA

## ----fit, results="hide"------------------------------------------------------
m <- SSLRDecisionTree(min_samples_split = round(length(labeled.index) * 0.25),
                      w = 0.3) %>% fit(Ozone ~ ., data = train)

## ----metrics------------------------------------------------------------------
predict(m,test)%>%
  bind_cols(test) %>%
  metrics(truth = "Ozone", estimate = .pred)

## ----fitcobc, results="hide", eval = FALSE------------------------------------
#  m_r <- rand_forest( mode = "regression") %>%
#    set_engine("ranger")
#  
#  m <- coBC(learner = m_r, max.iter = 2) %>% fit(Ozone ~ ., data = train)

