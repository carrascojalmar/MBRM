# robustloggamma package
rgengamma <- function(n, mu = 0, sigma = 1, lambda, zero = 1e-4) {
  if (sigma <= 0) {
    stop("Parameter 'sigma' must be strictly positive.")
  }

  if (abs(lambda) > zero) {
    alpha <- 1 / lambda^2
    gamma_samples <- rgamma(n, shape = alpha, rate = 1)
    values <- (log(gamma_samples) - log(alpha)) / lambda
  } else {
    values <- rnorm(n, mean = 0, sd = 1)
  }

  result <- values * sigma + mu
  return(result)
}

#' Simulate Data from a Mixed Regression Model with GLG Random Effects
#'
#' This function simulates clustered binary response data from a mixed regression model with random effects.
#' The model allows for different link functions and random effect distributions.
#'
#' @param n Integer. Number of clusters or subjects.
#' @param m Integer vector of length \code{n}. Each element indicates the number of observations per cluster.
#' @param theta Numeric vector. The first element is the scale or dispersion parameter for the random effects,
#'   and the remaining values are the fixed effects coefficients, including the intercept.
#' @param X A data frame or matrix of covariates with \code{n} rows (one per cluster). Should not include the intercept.
#'
#' @return A \code{tibble} containing the simulated dataset with the following columns:
#' \describe{
#'   \item{Ind}{Cluster or subject ID (integer from 1 to \code{n}).}
#'   \item{y}{Binary response variable (0 or 1).}
#'   \item{x1, x2, ...}{Covariates as defined in \code{X}, repeated according to cluster size.}
#' }
#' The output also has an attribute \code{"proportions"} indicating the proportions of 0's and 1's in \code{y}.
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
#' data1 <- rMRM(n,m,theta,X)
#' head(data1)
#' }
#'
#' @import stats
#' @importFrom tibble tibble
#' @export
rMRM <- function(n, m, theta, X) {

  lambda <- theta[1]
  beta <- theta[-1]

  if (lambda <= 0) {
    stop("Parameter 'lambda' must be strictly positive.")
  }

  N <- sum(m)

  bi <- rgengamma(n, mu = 0, sigma = lambda, lambda = lambda)

  vbi <- rep(bi, m)

  u <- as.vector(cbind(1, X) %*% beta)

  eta <- u + vbi

  p <- 1 - exp(-exp(eta))

  stopifnot(length(p) == N)

  mY <- rbinom(N, size = 1, prob = p)

  id <- rep(seq_len(n), times = m)
  mX <- as.data.frame(X)

  out <- tibble::tibble(
    Ind = id,
    y = mY,
    !!!mX
  )

  attr(out, "proportions") <- round(prop.table(table(mY)), 2)
  return(out)
}
