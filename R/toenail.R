#' toenail Dataset
#'
#' A data frame with 1908 observations on the following 5 variables.
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
#' The Toenail data discussed in Molenberghs and Verbeke (2010) and Lesaffre and
#' Spiessens (2001 come from a multicenter study comparing two oral
#' treatments (coded as A and B) for toenail (Dermatophyte Onychomycosis - TDO)
#' infection, involved patients evaluated at seven visits, i.e. on weeks 0, 4,
#' 8, 12, 24, 36 and 48. This study was evaluated on 294 patients comprising
#' 1908 measurements. The binary outcome was infection severity, coded
#' as 0 (no and mild) and 1 (moderate and severe). The patients have not been
#' treated prior to the first visit so this should be regarded as the baseline.
#'
#' @references
#' Molenberghs G, Verbeke G (2010).
#' Models for Discrete Longitudinal data.
#' Springer, New York.
#'
#' Lesaffre, E. and Spiessens, B. (2001).
#' On the effect of the number of quadrature points in a logistic random-effects model: An example.
#' Journal of the Royal Statistical Society, Series C, 50, 325-335.
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
