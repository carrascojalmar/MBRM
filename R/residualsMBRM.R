#' Compute randomized quantile residuals for MRM model
#'
#' This function computes randomized quantile residuals for objects of class \code{MRM}.
#'
#' If \code{envelope = TRUE}, the function also performs a Monte Carlo simulation to generate a normal QQ-plot with an envelope.
#'
#' @importFrom stats glm model.frame model.matrix model.response optim pnorm
#' @importFrom Formula Formula
#' @importFrom graphics lines
#'
#' @param object An object of class \code{MRM}, typically returned from \code{\link{MRMfit}}.
#' @param envelope Logical. If \code{TRUE}, generates an envelope QQ-plot using Monte Carlo simulations.
#' @param R Integer. Number of replications to simulate the envelope (default is 100).
#' @param random Character. The distribution for the random effects. Possible values are \code{"normal"} and \code{"gengamma"}.
#' @param link Character. The link function used in the model. Examples include \code{"cloglog"} and \code{"logit"}.
#' @param ... Additional arguments passed to \code{\link{MRMfit}} when \code{envelope = TRUE}.
#'
#' @return A numeric vector of randomized quantile residuals, or a QQ-plot with envelope if \code{envelope = TRUE}.
#' @export
#' @method residuals MRM
#'
#' @examples
#' data(Arthritis1)
#' fit <- MRMfit(y ~ Sex + Age + Treatment + Time, data = Arthritis1)
#' summary(residuals(fit))
#'
residuals.MRM <- function(object, envelope=FALSE, R=100,
                          random = "gengamma",link="cloglog",...) {

  formula <- object$formula
  data <-object$data
  n <- object$n
  m <- object$m

  mf <- model.frame(Formula(formula), data = data)
  y <- model.response(mf)

  x <- model.matrix(Formula(formula), data = data)

  beta <- object$coefficients
  phi <- object$scale**(-2)

  family <- object$family
  method <- object$method
  optim <- object$optim

  u <- exp(x%*%beta)

  res <- vector()
  N <- dim(x)[1]

  for(i in 1:N){
    if(y[i]==1){
      p <- 1-(phi/(phi+u[i]))^phi
      ui <- runif(1,1-p,1)
      res[i] <- qnorm(ui)
    }else{
      p <- (phi/(phi+u[i]))^phi
      uii <- runif(1,0,1-p)
      res[i] <- qnorm(uii)
    }
  }

  if(envelope==FALSE){
    return(res)}else{

      x_sorted <- sort(res)
      x_theo <- qnorm(ppoints(N))

      mR <- matrix(NA, ncol = R, nrow = N)
      r <- 1
      extra_args <- list(...)
      while (r <= R) {
        data.new <- rMRM(n = n, m = m, theta = c(phi, beta), X = x[,-1],
                         family = family, link = link, random = random)

        args_enve <- c(list(formula = formula, data = data.new), extra_args)
        op.enve <- try(do.call(MRMfit, args_enve), silent = TRUE)

        if (!inherits(op.enve, "try-error")) {
          mR[, r] <- sort(residuals(op.enve))
          r <- r + 1
        }
      }
      lower_env <- apply(mR,1,min)
      mean_env <- apply(mR,1,mean)
      upper_env <- apply(mR,1,max)


      y_range <- range(x_sorted, lower_env, upper_env)

      # Gráfico
      plot(x_theo, x_sorted,
           xlab = "Theoretical Quantiles (N(0,1))",
           ylab = "Randomized Quantile Residuals",
           main = "Normal QQ-Plot of Randomized Quantile Residuals with Envelope",
           pch = 19, col = "steelblue",
           ylim = y_range)
      lines(x_theo, lower_env, col = "gray", lty = 2, lwd = 1.2)
      lines(x_theo, mean_env, col = "black", lwd = 2)
      lines(x_theo, upper_env, col = "gray", lty = 2, lwd = 1.2)

  }
}
