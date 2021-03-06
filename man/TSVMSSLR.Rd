% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TSVM.R
\name{TSVMSSLR}
\alias{TSVMSSLR}
\title{General Interface for TSVM (Transductive SVM classifier using the convex concave procedure) model}
\usage{
TSVMSSLR(
  C = 1,
  Cstar = 0.1,
  kernel = kernlab::vanilladot(),
  balancing_constraint = TRUE,
  s = 0,
  x_center = TRUE,
  scale = FALSE,
  eps = 1e-09,
  max_iter = 20,
  verbose = FALSE
)
}
\arguments{
\item{C}{numeric; Cost parameter of the SVM}

\item{Cstar}{numeric; Cost parameter of the unlabeled objects}

\item{kernel}{kernlab::kernel to use}

\item{balancing_constraint}{logical; Whether a balancing constraint should be enfored that causes the fraction of objects assigned to each label in the unlabeled data to be similar to the label fraction in the labeled data.}

\item{s}{numeric; parameter controlling the loss function of the unlabeled objects (generally values between -1 and 0)}

\item{x_center}{logical;  Should the features be centered?}

\item{scale}{If TRUE, apply a z-transform to all observations in X and X_u before running the regression}

\item{eps}{numeric; Stopping criterion for the maximinimization}

\item{max_iter}{integer; Maximum number of iterations}

\item{verbose}{logical; print debugging messages, only works for vanilladot() kernel (default: FALSE)}
}
\description{
model from RSSL package
Transductive SVM using the CCCP algorithm as proposed by Collobert et al. (2006)
implemented in R using the quadprog package. The implementation does not handle large
datasets very well, but can be useful for smaller datasets and visualization purposes.
C is the cost associated with labeled objects, while Cstar is the cost for the
unlabeled objects. s control the loss function used for the unlabeled objects: it
controls the size of the plateau for the symmetric ramp loss function. The balancing
constraint makes sure the label assignments of the unlabeled objects are similar to the
prior on the classes that was observed on the labeled data.
}
\examples{
library(tidyverse)
library(caret)
library(tidymodels)
library(SSLR)

data(breast)

set.seed(1)
train.index <- createDataPartition(breast$Class, p = .7, list = FALSE)
train <- breast[ train.index,]
test  <- breast[-train.index,]

cls <- which(colnames(breast) == "Class")

#\% LABELED
labeled.index <- createDataPartition(breast$Class, p = .2, list = FALSE)
train[-labeled.index,cls] <- NA

library(kernlab)
m <- TSVMSSLR(kernel = kernlab::vanilladot()) \%>\% fit(Class ~ ., data = train)


#Accesing model from RSSL
model <- m$model

}
\references{
Collobert, R. et al., 2006. Large scale transductive SVMs.
Journal of Machine Learning Research, 7, pp.1687-1712.
}
