rgengamma <- function(n, mu = 0, sigma = 1, lambda, zero = 1e-4) {
  if (abs(lambda) > zero) {
    alpha <- 1 / lambda^2
    gamma_samples <- rgamma(n, shape = alpha, rate = 1)
    values <- (log(gamma_samples) - log(alpha)) / lambda
  } else {
    values <- rnorm(n, mean = 0, sd = 1)
  }
  result <- values * sigma + mu
  result
}

#' Simulate Data from a Mixed Regression Model with Random Effects
#'
#' This function simulates clustered binary response data from a mixed regression model with random effects.
#' The model allows for different link functions and random effect distributions.
#'
#' @param n Integer. Number of clusters or subjects.
#' @param m Integer vector of length \code{n}. Each element indicates the number of observations per cluster.
#' @param theta Numeric vector. The first element is the scale or dispersion parameter for the random effects,
#'   and the remaining values are the fixed effects coefficients, including the intercept.
#' @param X A data frame or matrix of covariates with \code{n} rows (one per cluster). Should not include the intercept.
#' @param family Character. The response distribution. Currently only \code{"bernoulli"} is supported (default).
#' @param link Character. The link function to use. One of: \code{"cloglog"}, \code{"logit"}, or \code{"probit"}.
#'   Default is \code{"cloglog"}.
#' @param random Character. Distribution for the random effects. One of: \code{"gengamma"} or \code{"normal"}.
#'   Default is \code{"gengamma"}.
#'
#' @return A \code{tibble} containing the simulated dataset with the following columns:
#' \describe{
#'   \item{Ind}{Cluster or subject ID (integer from 1 to \code{n}).}
#'   \item{y}{Binary response variable (0 or 1).}
#'   \item{x1, x2, ...}{Covariates as defined in \code{X}, repeated according to cluster size.}
#' }
#' The output also has an attribute \code{"proportions"} indicating the proportion of 0's and 1's in \code{y}.
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' n <- 500
#' m <- rep(3,n)
#' theta <- c(0.5,1,-2,1)
#' set.seed(123)
#' X <- cbind(runif(n),rnorm(n))
#' set.seed(456)
#' data1 <- rMRM(n,m,theta,X,family="bernoulli",
#' link="cloglog",random = "gengamma")
#' head(data1
#' }
#'
#' @import stats
#' @importFrom tibble tibble
#' @export
rMRM <- function(n, m, theta, X,
                 family = "bernoulli",
                 link = "cloglog",
                 random = "gengamma") {

  lambda <- theta[1]
  beta <- theta[-1]

  N <- sum(m)

  bi <- switch(random,
               gengamma = rgengamma(n, mu = 0, sigma = 1, lambda = lambda),
               normal = rnorm(n, mean = 0, sd = lambda),
               stop("Unsupported random effect: ", random))


  u <- as.vector(cbind(1, X) %*% beta)

  eta <- u + bi
  p <- switch(link,
              cloglog = 1 - exp(-exp(eta)),
              logit   = exp(eta) / (1 + exp(eta)),
              probit  = pnorm(eta),
              stop("Unsupported link function: ", link))

  stopifnot(length(p) == length(m))

  mY <- unlist(Map(function(prob, reps) rbinom(reps, size = 1, prob = prob), p, m))

  id <- rep(seq_len(n), times = m)
  mX <- as.data.frame(lapply(as.data.frame(X), function(col) rep(col, m)))
  colnames(mX) <- paste0("x", seq_len(ncol(X)))

  out <- tibble::tibble(
    Ind = id,
    y = mY,
    !!!mX
  )

  attr(out, "proportions") <- round(prop.table(table(mY)), 2)
  return(out)
}
