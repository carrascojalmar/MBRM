#' Fit Mixed Regression Model with Log-Gamma Random Effects
#'
#' This function fits a mixed regression model for binary outcomes with random effects
#' following a generalized log-gamma distribution. The estimation is performed by maximizing
#' a custom log-likelihood using numerical optimization via \code{\link[stats]{optim}}.
#'
#' @param formula A symbolic description of the model to be fitted, e.g., \code{y ~ x1 + x2}.
#'   The response variable must be binary (0/1).
#' @param data A data frame containing the variables in the model. The data must include an
#'   \code{Ind} column indicating cluster or subject IDs for the random effects.
#' @param family A character string specifying the response distribution. Currently only
#'   \code{"bernoulli"} is supported (default).
#' @param method Optimization method to be used in \code{\link[stats]{optim}}. One of
#'   \code{"Nelder-Mead"}, \code{"BFGS"}, \code{"CG"}, \code{"L-BFGS-B"}, \code{"SANN"}, or \code{"Brent"}.
#'   Default is \code{"BFGS"}.
#' @param ... Additional arguments passed to \code{\link[stats]{optim}}, such as \code{control},
#'   \code{lower}, or \code{upper} (when supported by the chosen method).
#'
#' @return An object of class \code{"MRM"} containing:
#' \item{call}{The matched function call.}
#' \item{formula}{The model formula.}
#' \item{coefficients}{Estimated fixed effects coefficients.}
#' \item{scale}{Estimated scale parameter for the random effects distribution.}
#' \item{loglik}{Maximized log-likelihood value.}
#' \item{n}{Number of clusters or subjects.}
#' \item{m}{Vector with the number of observations per cluster.}
#' \item{ep}{Estimated standard errors for parameters.}
#' \item{iter}{Number of iterations used by the optimizer.}
#' \item{family}{Family string used.}
#' \item{method}{Optimization method used.}
#' \item{data}{The original data frame used.}
#'
#' @examples
#' \dontrun{
#' # Simulated data
#' data1 <- rMRM(n = 50, m = rep(3, 50),
#'               theta = c(0.8, 1, -1),
#'               X = data.frame(x1 = rnorm(150), x2 = rnorm(150)))
#'
#' # Fit using BFGS (default)
#' fit1 <- MRMfit(y ~ x1 + x2, data = data1)
#' summary(fit1)
#'
#' # Fit using L-BFGS-B with bounds
#' fit2 <- MRMfit(y ~ x1 + x2, data = data1,
#'                method = "L-BFGS-B",
#'                lower = c(1e-5, rep(-Inf, 3)),
#'                upper = rep(Inf, 4),
#'                control = list(factr = 1e7))
#' summary(fit2)
#' }
#'
#' @importFrom stats glm model.frame model.matrix model.response optim pnorm
#' @importFrom Formula Formula
#' @import stats
#' @export
#'
MRMfit <- function(formula, data, family = "bernoulli", method = "BFGS", ...) {

  aux <- glm(formula, data = data, family = "binomial")
  initial <- c(1, as.numeric(aux$coefficients))  # escala + betas

  data_list <- dplyr::group_split(dplyr::group_by(data, Ind))
  X_list <- lapply(data_list, function(df) model.matrix(formula, df))
  y_list <- lapply(data_list, function(df) model.response(model.frame(formula, df)))

  op <- optim(
    par = initial,
    fn = lvero,
    y_list = y_list,
    X_list = X_list,
    method = method,
    hessian = TRUE,
    ...
  )

  fit.MRM <- list(
    call = match.call(),
    formula = formula,
    coefficients = op$par[-1],   # betas
    scale = op$par[1],           # parâmetro de escala
    loglik = op$value,
    n = max(data$Ind),
    m = as.numeric(table(data$Ind)),
    ep = sqrt(diag(solve(op$hessian))),
    iter = op$counts[1],
    family = family,
    method = method,
    optim = op,
    data = data
  )

  class(fit.MRM) <- "MRM"
  return(fit.MRM)
}

#' @export
print.MRM <- function(x, ...) {
  cat("Call:\n")
  print(x$call)

  cat("\nCoefficients:\n")
  print(round(x$coefficients, 4))

  cat("\nScale:\n")
  print(round(x$scale, 4))

  cat("\nLog-likelihood:", round(x$loglik, 4), "\n")
}

#' @export
summary.MRM <- function(object, ...) {
  y <- model.response(model.frame(Formula(object$formula),
                                  data = object$data))

  x <- model.matrix(Formula(object$formula),
                    data = eval(object$call$data))

  coef <- object$coefficients
  scale <- object$scale
  ep <- object$ep
  iter <- object$iter
  value <- object$loglik
  family <- object$family
  n <- object$n

  std_alpha <- ep[1]
  std_beta <- ep[2:(1 + length(coef))]

  z_beta <- coef / std_beta
  p_beta <- 2 * (1 - pnorm(abs(z_beta)))

  coef_beta <- data.frame(
    Value = coef,
    `Std. Error` = std_beta,
    z = z_beta,
    p = format.pval(p_beta, digits = 2, eps = 2e-16),
    row.names = colnames(x),
    check.names = FALSE
  )

  coef_scale <- data.frame(
      Estimate = object$scale,
      Std.Error = std_alpha,
      row.names = "scale"
    )

  out <- list(
    call = object$call,
    family = family,
    loglik = value,
    AIC = -2 * value + 2 * length(ep),
    BIC = -2 * value + log(n) * length(ep),
    iter = iter,
    coef = coef_beta,
    scale = coef_scale
  )

  class(out) <- "summary.MRM"
  return(out)
}
#' @export
print.summary.MRM <- function(x, digits = 5, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nDistribution:", x$family, "\n")
  cat("Log-Likelihood:", formatC(x$loglik, digits = digits, format = "f"), "\n")
  cat("AIC:", formatC(x$AIC, digits = digits, format = "f"), "\n")
  cat("BIC:", formatC(x$BIC, digits = digits, format = "f"), "\n")
  cat("Number of Iterations:", x$iter, "\n\n")

  cat("Coefficients :\n")
  print(format(x$coef, digits = digits, nsmall = digits), quote = FALSE)

  cat("\nScale:\n")
  print(format(x$scale, digits = digits, nsmall = digits), quote = FALSE)

}
