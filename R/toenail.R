#' toenail Dataset
#'
#' A data frame with 1908 observations on the following 4 variables.
#'
#' @format A data frame with 1908 rows and 4 variables:
#' \describe{
#'   \item{Ind}{Subject identifier (integer from 1 to 38)}
#'   \item{y}{Binary response is the severity of infection, 0 (not severe) and 1(severe).}
#'   \item{treatment}{Treatment group (0 = Treatment A, 1 = Treatment B)}
#'   \item{month}{a numeric vector giving the time of the visit (not exactly monthly intervals
#'          hence not round numbers)}
#'   \item{visit}{a numeric vector giving the number of the visit}
#' }
#'
#' @details
#' The Toenail data discussed in Molenberghs and Verbeke (2010) were obtained from a
#' randomized, double-blind, parallel-group, multicenter study for the comparison of two
#' oral treatments (coded as A and B) for Toenail Dermatophyte Onychomycosis (TDO).
#' The present study aimed to compare the efficacy of 12 weeks of continuous therapy
#' with Treatment A or B. In total, 294 patients, distributed over 36 centers, were
#' randomized. Subjects were followed during 12 weeks (3 months) of treatment and
#' followed further up to 48 weeks (12 months). Measurements were taken at baseline
#' every month during treatment and every 3 months afterward, resulting in a
#' maximum of 7 measurements per subject.
#'
#' @references
#' Molenberghs G, Verbeke G (2010).
#' Models for Discrete Longitudinal data.
#' Springer, New York.
#'
#' @examples
#' \dontrun{
#' data(toenail)
#' head(toenail)
#'
#' # Fit the Bernoulli-LGG model
#' fit <- MRMfit(y ~ treatment + month + treatment:month, data = toenail)
#' summary(fit)
#' }
"toenail"
