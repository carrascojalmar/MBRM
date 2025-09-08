#' Compute the randomized quantile residuals for MRM model
#'
#' This function computes the randomized quantile residuals for objects of class \code{MRM}.
#'
#' @importFrom stats glm model.frame model.matrix model.response optim pnorm
#' @importFrom Formula Formula
#' @importFrom graphics lines
#'
#' @param object An object of class \code{MRM}, typically returned from \code{\link{MRMfit}}.
#' @param ... Additional arguments passed to \code{\link{MRMfit}}.
#'
#' @return A numeric vector of randomized quantile residuals.
#' @export
#' @method residuals MRM
#'
#' @examples
#' data(Arthritis1)
#' fit <- MRMfit(y ~ Sex + Age + Treatment + Time, data = Arthritis1)
#' summary(residuals(fit))
#'
residuals.MRM <- function(object,...) {

  formula <- object$formula
  data <-object$data
  n <- object$n
  m <- object$m

  mf <- model.frame(Formula(formula), data = data)
  y <- model.response(mf)

  x <- model.matrix(Formula(formula), data = data)

  lambda <- object$scale
  beta <- object$coefficients
  phi <- lambda**(-2)


  u <- exp(x%*%beta)

  res <- vector()
  N <- dim(x)[1]

  for(i in 1:N){
    if(y[i]==1){
      p <- 1-(phi/(phi+u[i]))^phi
      ui <- runif(1,0,p) #runif(1,p,1) 1-p
      res[i] <- qnorm(ui)
    }else{
      p <- (phi/(phi+u[i]))^phi
      uii <- runif(1,0,p) #1-p
      res[i] <- qnorm(uii)
    }
  }
    return(res)
}
