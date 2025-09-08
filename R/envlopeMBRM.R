#' Compute simulation envelopes for MRM model
#'
#' This function computes the envelopes simulation of the randomized quantile residuals for objects of class \code{MRM}.
#'
#' @importFrom stats glm model.frame model.matrix model.response optim pnorm
#' @importFrom Formula Formula
#' @importFrom graphics lines
#' @import ggplot2
#'
#' @param object An object of class \code{MRM}, typically returned from \code{\link{MRMfit}}.
#' @param R Integer. Number of replications to simulate the envelopes (default is 100).
#' @param ... Additional arguments passed to \code{\link{MRMfit}} when \code{envelope = TRUE}.
#'
#' @return A QQ-plot with envelope.
#' @export
#'
#' @examples
#' data(Arthritis1)
#' fit <- MRMfit(y ~ Sex + Age + Treatment + Time, data = Arthritis1)
#' envelope.MRM(fit)
#'
envelope.MRM <-function(object, R=100, ...){
  formula <- object$formula
  data <-object$data
  n <- object$n
  m <- object$m
  x <- model.matrix(Formula(formula), data = data)
  lambda <- object$scale
  beta <- object$coefficients
  N <- dim(x)[1]

  x_sorted <- sort(residuals(object))
  x_theo <- qnorm(ppoints(N))

  mR <- matrix(NA, ncol = R, nrow = N)
  r <- 1
  extra_args <- list(...)
  while (r <= R) {
    data.new <- rMRM(n = n, m = m, theta = c(lambda, beta), X = x[,-1])

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


  df.aux <- data.frame(r=x_sorted,aux1=lower_env,aux2=mean_env,aux3=upper_env)

  # Gráfico

  ggplot(df.aux) +
    labs(x = "Theorical quantiles",y="Randomized quantile residuals")+
    stat_qq(aes(sample = r), colour = "black") +
    stat_qq(aes(sample = aux1), colour = "blue",geom="line", size=0.5) +
    stat_qq(aes(sample = aux2), colour = "blue",geom="line", size=0.5) +
    stat_qq(aes(sample = aux3), colour = "blue",geom="line", size=0.5) +
    geom_hline(yintercept = c(-3,0,3),linetype="dashed",color = "blue")+
    theme_bw()+
    theme(legend.position="none")+
    theme(axis.text=element_text(size=25),
          axis.title=element_text(size=25))
}
