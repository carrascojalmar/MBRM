#' toenail Dataset
#'
#' This dataset contains binary response data from a longitudinal study on rheumatoid arthritis.
#' The data includes repeated measurements on 38 individuals, each with 5 time points.
#'
#' @format A data frame with 190 rows and 6 variables:
#' \describe{
#'   \item{Ind}{Subject identifier (integer from 1 to 38)}
#'   \item{y}{Binary response variable (0 or 1)}
#'   \item{Treatment}{Treatment group (0 = Placebo, 1 = Treated)}
#'   \item{Time}{Time indicator (0 = baseline, 1 = follow-up)}
#' }
#'
#' @details
#' The original data was collected to evaluate the effect of a treatment over time on arthritis severity.
#' Covariates include demographic and clinical variables, and the response is binary (presence/absence of symptom).
#'
#' @source
#' Derived from the original dataset included in: \cr
#' \emph{toenail.txt} — internal clinical dataset used in Bernoulli-GLG modeling study.
#'
#' @examples
#' \dontrun{
#' data(toenail)
#' head(toenail)
#'
#' # Fit the Bernoulli-LGG model
#' fit <- MRMfit(y ~ Treatment + Time + Treatment:Time, data = toenail)
#' summary(fit)
#' }
"toenail"
