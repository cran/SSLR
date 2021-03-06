
#' @title Democratic generic method
#' @description Democratic is a semi-supervised learning algorithm with a co-training
#' style. This algorithm trains N classifiers with different learning schemes defined in
#' list \code{gen.learners}. During the iterative process, the multiple classifiers with
#' different inductive biases label data for each other.
#' @param y A vector with the labels of training instances. In this vector the
#' unlabeled instances are specified with the value \code{NA}.
#' @param gen.learners A list of functions for training N different supervised base classifiers.
#' Each function needs two parameters, indexes and cls, where indexes indicates
#' the instances to use and cls specifies the classes of those instances.
#' @param gen.preds A list of functions for predicting the probabilities per classes.
#' Each function must be two parameters, model and indexes, where the model
#' is a classifier trained with \code{gen.learner} function and
#' indexes indicates the instances to predict.
#' @details
#' democraticG can be helpful in those cases where the method selected as
#' base classifier needs a \code{learner} and \code{pred} functions with other
#' specifications. For more information about the general democratic method,
#' please see \code{\link{democratic}} function. Essentially, \code{democratic}
#' function is a wrapper of \code{democraticG} function.
#' @return A list object of class "democraticG" containing:
#' \describe{
#'   \item{W}{A vector with the confidence-weighted vote assigned to each classifier.}
#'   \item{model}{A list with the final N base classifiers trained using the
#'   enlarged labeled set.}
#'   \item{model.index}{List of N vectors of indexes related to the training instances
#'   used per each classifier. These indexes are relative to the \code{y} argument.}
#'   \item{instances.index}{The indexes of all training instances used to
#'   train the N \code{models}. These indexes include the initial labeled instances
#'   and the newly labeled instances. These indexes are relative to the \code{y} argument.}
#'   \item{model.index.map}{List of three vectors with the same information in \code{model.index}
#'   but the indexes are relative to \code{instances.index} vector.}
#'   \item{classes}{The levels of \code{y} factor.}
#' }
#' @references
#' Yan Zhou and Sally Goldman.\cr
#' \emph{Democratic co-learning.}\cr
#' In IEEE 16th International Conference on Tools with Artificial Intelligence (ICTAI),
#' pages 594-602. IEEE, Nov 2004. doi: 10.1109/ICTAI.2004.48.
democraticG <- function(
  y,
  gen.learners,
  gen.preds
) {
  ### Check parameters ###
  # Check y
  if (!is.factor(y)) {
    if (!is.vector(y)) {
      stop("Parameter y is neither a vector nor a factor.")
    } else {
      y = as.factor(y)
    }
  }
  # Check lengths
  if (length(gen.learners) != length(gen.preds)) {
    stop("The length of gen.learners is not equal to the length of gen.preds.")
  }
  nclassifiers <- length(gen.learners)
  if (nclassifiers <= 1) {
    stop("gen.learners must contain at least two base classifiers.")
  }

  ### Init variables ###
  # Identify the classes
  classes <- levels(y)
  nclasses <- length(classes)

  # Init variable to store the labels
  ynew <- y

  # Obtain the indexes of labeled and unlabeled instances
  labeled <- which(!is.na(y))
  unlabeled <- which(is.na(y))
  nunlabeled <- length(unlabeled)
  ## Check the labeled and unlabeled sets
  if (length(labeled) == 0) {
    # labeled is empty
    stop("The labeled set is empty. All the values in y parameter are NA.")
  }
  if (length(unlabeled) == 0) {
    # unlabeled is empty
    stop("The unlabeled set is empty. None value in y parameter is NA.")
  }

  ### Democratic algorithm ###

  y.map <- unclass(y)

  H <- vector(mode = "list", length = nclassifiers)
  Lind <- vector(mode = "list", length = nclassifiers)
  Lcls <- vector(mode = "list", length = nclassifiers)
  e <- vector(mode = "numeric", length = nclassifiers)
  for (i in 1:nclassifiers) {
    H[[i]] <- gen.learners[[i]](labeled, y[labeled])
    Lind[[i]] <- labeled
    Lcls[[i]] <- y.map[labeled]
    e[i] <- 0
  }

  iter <- 1
  changes <- TRUE
  while (changes) {
    #while some classifier changes

    changes <- FALSE

    LindPrima <- vector(mode = "list", length = nclassifiers)
    LclsPrima <- vector(mode = "list", length = nclassifiers)

    # Internal classify
    predU <- mapply(
      FUN = function(model, pred) {


        prob <- pred(model, unlabeled)
        colnames(prob) <- classes

        getClassIdx(
          checkProb(
            prob = prob,
            ninstances = length(unlabeled),
            classes
          )
        )
      },
      H, gen.preds
    )

    cls <- vote(predU) # voted labels

    # End Internal classify

    # compute the confidence interval over the original set L
    W <- mapply(
      FUN = function(model, pred) {

        prob <- pred(model, labeled)
        colnames(prob) <- classes

        confidenceInterval(
          getClassIdx(
            checkProb(
              prob = prob,
              ninstances = length(labeled),
              classes
            )
          ),
          y.map[labeled]
        )$W
      },
      H, gen.preds
    )

    for (i in 1:nunlabeled) {
      #for each unlabeled example x in U
      # is the sum of the mean confidence values of the learners in the majority
      # group greater than the sum of the mean confidence values in the minority group??
      sumW <- rep(0, nclasses)
      for (j in 1:nclassifiers)
        #for each classifier
        sumW[predU[i, j]] <- sumW[predU[i, j]] + W[j]

      # Calculate the maximum confidence with different label to predicted.
      lab <- cls[[i]][which.max(sumW[cls[[i]]])] #returns the most probable label
      tmp <- sumW[lab] # the total confidence associated with this label
      sumW[lab] <- -Inf # don't select the same label
      Max <- which.max(sumW) # second class with major confidence
      sumW[lab] <- tmp

      if (sumW[lab] > sumW[Max]) {
        # if the classifier i does not label this X unlabeled as predicted, add it to Li.
        for (j in 1:nclassifiers)
          if (predU[i, j] != lab) {
            # wrong label
            LindPrima[[j]] <- c(LindPrima[[j]], unlabeled[i])
            LclsPrima[[j]] <- c(LclsPrima[[j]], lab)
          }
      }
    }
    # end for each unlabeled example x in U


    # Estimate if adding Li' to Li improves the accuracy

    LindUnion <- vector(mode = "list", length = nclassifiers)
    LclsUnion <- vector(mode = "list", length = nclassifiers)

    for (i in 1:nclassifiers) {
      repeated <- intersect(Lind[[i]], LindPrima[[i]])
      if (length(repeated) != 0) {
        indexesToRemove <- sapply(
          X = repeated,
          FUN = function(r) {
            which(LindPrima[[i]] == r)
          }
        )
        LindPrima[[i]] <- LindPrima[[i]][-indexesToRemove]
        LclsPrima[[i]] <- LclsPrima[[i]][-indexesToRemove]
      }
      if (!is.null(LindPrima[[i]])) {
        LindUnion[[i]] <- c(Lind[[i]], LindPrima[[i]])
        LclsUnion[[i]] <- c(Lcls[[i]], LclsPrima[[i]])
      } else {
        LindUnion[[i]] <- Lind[[i]]
        LclsUnion[[i]] <- Lcls[[i]]
      }
    }

    L <- mapply(
      FUN = function(model, pred) {

        prob <- pred(model, Lind[[i]])
        colnames(prob) <- classes

        confidenceInterval(
          getClassIdx(
            checkProb(
              prob = prob,
              ninstances = length(Lind[[i]]),
              classes
            )
          ),
          Lcls[[i]]
        )$L
      },
      H, gen.preds
    )

    q <- ep <- qp <- NULL
    for (i in 1:nclassifiers) {
      # for each classifier
      sizeLi <- length(Lind[[i]])
      sizeLLP <- length(LindUnion[[i]])
      if (sizeLLP > sizeLi) {
        # there are new instances in LindUnion
        q[i] <- sizeLi * (1 - 2 * (e[i] / sizeLi)) ^ 2 # est. of error rate
        ep[i] <- (1 - mean(L[-i])) * length(LindPrima[[i]]) # est. of new error rate
        qp[i] <- sizeLLP * (1 - 2 * (e[i] + ep[i]) / sizeLLP) ^ 2 # if Li' added

        if (qp[i] > q[i]) {
          Lind[[i]] <- LindUnion[[i]]
          Lcls[[i]] <- LclsUnion[[i]]
          e[i] <- e[i] + ep[i]
          changes <- TRUE
          # train i classifier
          yi <- classes[Lcls[[i]]]
          H[[i]] <- gen.learners[[i]](Lind[[i]], factor(yi, classes))
        }
      }
    }
    # end for each classifier
    iter <- iter + 1
  }
  # End while

  ### Result ###

  # determine labeled instances
  instances.index <- unique(unlist(Lind))
  # map indexes respect to instances.index
  model.index.map <- lapply(
    X = Lind,
    FUN = function(indexes) {
      r <- unclass(factor(indexes, levels = instances.index))
      attr(r, "levels") <- NULL
      return(r)
    }
  )

  # compute W
  W <- mapply(
    FUN = function(model, pred) {

      prob <- pred(model, labeled)
      colnames(prob) <- classes

      confidenceInterval(
        getClassIdx(
          checkProb(
            prob = prob,
            ninstances = length(labeled),
            classes
          )
        ),
        y.map[labeled]
      )$W
    },
    H, gen.preds
  )

  # Save result
  result <- list(
    W = W,
    model = H,
    model.index = Lind,
    instances.index = instances.index,
    model.index.map = model.index.map,
    classes = classes
  )
  class(result) <- "democraticG"

  return(result)
}

#' @title General Interface for Democratic model
#' @description Democratic Co-Learning is a semi-supervised learning algorithm with a
#' co-training style. This algorithm trains N classifiers with different learning schemes
#' defined in list \code{gen.learners}. During the iterative process, the multiple classifiers
#' with different inductive biases label data for each other.
#' @param learners List of models from parsnip package for training a supervised base classifier
#' using a set of instances. This model need to have probability predictions
#' @param schemes List of schemes (col x names in each learner).
#' Default is null, it means that learner uses all x columns
#' @details
#' This method trains an ensemble of diverse classifiers. To promote the initial diversity
#' the classifiers must represent different learning schemes.
#' When x.inst is \code{FALSE} all \code{learners} defined must be able to learn a classifier
#' from the precomputed matrix in \code{x}.
#' The iteration process of the algorithm ends when no changes occurs in
#' any model during a complete iteration.
#' The generation of the final hypothesis is
#' produced via a weigthed majority voting.
#' @return (When model fit) A list object of class "democratic" containing:
#' \describe{
#'   \item{W}{A vector with the confidence-weighted vote assigned to each classifier.}
#'   \item{model}{A list with the final N base classifiers trained using the
#'   enlarged labeled set.}
#'   \item{model.index}{List of N vectors of indexes related to the training instances
#'   used per each classifier. These indexes are relative to the \code{y} argument.}
#'   \item{instances.index}{The indexes of all training instances used to
#'   train the N \code{models}. These indexes include the initial labeled instances
#'   and the newly labeled instances. These indexes are relative to the \code{y} argument.}
#'   \item{model.index.map}{List of three vectors with the same information in \code{model.index}
#'   but the indexes are relative to \code{instances.index} vector.}
#'   \item{classes}{The levels of \code{y} factor.}
#'   \item{preds}{The functions provided in the \code{preds} argument.}
#'   \item{preds.pars}{The set of lists provided in the \code{preds.pars} argument.}
#'   \item{x.inst}{The value provided in the \code{x.inst} argument.}
#' }
#' @example demo/Democratic.R
#' @importFrom magrittr %>%
#' @export
democratic <- function(
  learners, schemes = NULL) {

  ### Check parameters ###

  train_function <- function(x, y) {
    x <- as.data.frame(x)
    y <- as.factor(y)

    # Check learners
    if (length(learners) <= 1 & is.list(learners)) {
      stop("Parameter learners must contain at least two base classifiers.")
    }

    if(!is.null(schemes) & is.list(schemes)){
      if (length(learners) != length(schemes)) {
        stop("Schemes and Learners must have the same amount")
      }
    }
    #Schemes is null, Convert schemes in list of nulls
    else{
      schemes <- vector(mode = "list", length = length(learners))
      for(i in 1:length(schemes)){
        schemes[[i]] <- colnames(x)
      }
    }

    m_learners_base <- mapply(
        FUN = function(learner,scheme) {
          learner_base <- function(training.ints, cls) {

            m <- learner %>% parsnip::fit_xy(x = x[training.ints,scheme,drop = FALSE], y = cls)
            return(m)
          }
          return(learner_base)
        },
        learners,
        schemes,
        SIMPLIFY = FALSE
      )
    # Build pred base functions
    m_preds_base <- mapply(
        FUN = function(m,scheme) {
          pred_base <- function(m, testing.ints) {

            prob <- predict(m, x[testing.ints,scheme,drop = FALSE], type = "prob")
            return(prob)
          }
          return(pred_base)
        },
        learners,
        schemes,
        SIMPLIFY = FALSE
      )
    # Call base method
    result <- democraticG(y, m_learners_base, m_preds_base)

    ### Result ###
    result$schemes = schemes
    result$classes = levels(y)
    result$pred.params = c("class","raw")
    result$mode = "classification"
    class(result) <- "democratic"

    return(result)
  }

  args <- list(
    learners = learners
  )

  new_model_sslr(train_function, "democratic", args)


}

#' @title Predictions of the Democratic method
#' @description Predicts the label of instances according to the \code{democratic} model.
#' @details For additional help see \code{\link{democratic}} examples.
#' @param object Democratic model built with the \code{\link{democratic}} function.
#' @param x A object that can be coerced as matrix.
#' Depending on how was the model built, \code{x} is interpreted as a matrix
#' with the distances between the unseen instances and the selected training instances,
#' or a matrix of instances.
#' @param ... This parameter is included for compatibility reasons.
#' @return Vector with the labels assigned.
#' @method predict democratic
#' @importFrom stats predict
#' @importFrom magrittr %>%
predict.democratic <- function(object, x, ...) {

  # Select classifiers for prediction
  lower.limit <- 0.5
  selected <- object$W > lower.limit # TODO: create a parameter for 0.5 lower limit
  W.selected <- object$W[selected]
  if (length(W.selected) == 0) {
    stop(
      sprintf(
        "%s %s %f",
        "Any classifier selected according model's W values.",
        "The classifiers are selected when it's W value is greater than",
        lower.limit
      )
    )
  }

  # Classify the instances using each classifier
  # The result is a matrix of indexes that indicates the classes
  # The matrix have one column per selected classifier
  # and one row per instance
  ninstances = nrow(x)

  pred <- mapply(
    FUN = function(model,scheme) {

      prob <- model %>% predict(x[,scheme,drop = FALSE], type = "prob")
      colnames(prob) <- object$classes

      getClassIdx(
        checkProb(
          prob,
          ninstances,
          object$classes
        )
      )
    },
    object$model[selected],
    object$schemes[selected]
  )

  # Combining predictions
  map <- vector(mode = "numeric", length = ninstances)

  for (i in 1:nrow(pred)) {
    #for each example x in U
    pertenece <- wz <- rep(0, length(object$classes))

    for (j in 1:ncol(pred)) {
      #for each classifier
      z = pred[i, j]
      # Allocate this classifier to group Gz
      pertenece[z] <- pertenece[z] + 1
      wz[z] <- wz[z] + W.selected[j]
    }

    # Compute group average mean confidence
    countGj <- (pertenece + 0.5) / (pertenece + 1) * (wz / pertenece)
    map[i] <- which.max(countGj)
  }
  # end for

  cls <- factor(object$classes[map], object$classes)

  cls
}

#' @title Combining the hypothesis of the classifiers
#' @description This function combines the probabilities predicted by the set of
#' classifiers.
#' @param pred A list with the prediction for each classifier.
#' @param W A vector with the confidence-weighted vote assigned to each classifier
#' during the training process.
#' @param classes the classes.
#' @return The classification proposed.
#' @export
democraticCombine <- function(pred, W, classes) {
  # Check relation between pred and W
  if (length(pred) != length(W)) {
    stop("The lengths of 'pred' and 'W' parameters are not equals.")
  }
  # Check the number of instances
  ninstances <- unique(vapply(X = pred, FUN = length, FUN.VALUE = numeric(1)))
  if (length(ninstances) != 1) {
    stop("The length of objects in the 'pred' parameter are not all equals.")
  }

  nclassifiers <- length(pred)
  nclasses <- length(classes)
  map <- vector(mode = "numeric", length = ninstances)
  for (i in 1:ninstances) {
    #for each example x in U
    pertenece <- wz <- rep(0, nclasses)

    for (j in 1:nclassifiers) {
      #for each classifier
      z <- which(pred[[j]][i] == classes)
      if (W[j] > 0.5) {
        # Allocate this classifier to group Gz
        pertenece[z] <- pertenece[z] + 1
        wz[z] <- wz[z] + W[j]
      }
    }

    # Compute group average mean confidence
    countGj <- (pertenece + 0.5) / (pertenece + 1) * (wz / pertenece)
    map[i] <- which.max(countGj)
  }
  # end for

  factor(classes[map], classes)
}

#' @title Compute the 95\% confidence interval of the classifier
#' @noRd
confidenceInterval <- function(pred, conf.cls) {
  # accuracy
  W <- length(which(pred == conf.cls)) / length(conf.cls)

  # lowest point of the confidence interval
  L <- W - 1.96 * sqrt(W * (1 - W) / length(conf.cls))

  list(L = L, W = W)
}


#' @title Compute the most common label for each instance
#' @param pred a matrix with the prediction of each instance
#' using each classifier
#' @return The voted class for each instance
#' @noRd
vote <- function(pred) {
  FUN = function(p) {
    as.numeric(names(which.max(summary(factor(p)))))
  }
  lab <- apply(X = pred, MARGIN = 1, FUN)
  return(lab)
}
