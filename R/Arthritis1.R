#' Arthritis1 Dataset
#'
#' This dataset contains binary response data from a longitudinal study on rheumatoid arthritis.
#' The data include repeated measurements on 38 individuals, each with 5 time points.
#'
#' @format A data frame with 190 rows and 6 variables:
#' \describe{
#'   \item{Ind}{Subject identifier (integer from 1 to 38)}
#'   \item{y}{Binary response variable (0 or 1)}
#'   \item{Sex}{Sex indicator (0 = Female, 1 = Male)}
#'   \item{Age}{Age indicator (0 = <=55 years 1 = > 55 uears)}
#'   \item{Treatment}{Treatment group (0 = Placebo, 1 = Auranofin)}
#'   \item{Time}{Time indicator (0 = baseline, 1 = follow-up)}
#' }
#'
#' @details
#' The original data was collected to evaluate the effect of a treatment over time on arthritis severity.
#' Covariates include demographic and clinical variables, and the response is binary (presence/absence of symptoms).
#'
#' @source
#' Derived from the original dataset included in: \cr
#' \emph{Arthritis.txt} an internal clinical dataset used in Bernoulli-GLG modeling study.
#'
#' @references
#' Fitzmaurice, G. M. and Lipsitz, S. (1995).
#' A model for binary time series data with serial odds ratio patterns.
#' \emph{Journal of the Royal Statistical Society: Series B}, 44, 51–61.
#'
#' @examples
#' \dontrun{
#' data(Arthritis1)
#' head(Arthritis1)
#'
#' # Fit the Bernoulli-LGG model
#' fit <- MRMfit(y ~ Sex + Age + Treatment + Time, data = Arthritis1)
#' summary(fit)
#' }
"Arthritis1"
