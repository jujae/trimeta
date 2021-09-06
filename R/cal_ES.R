#' Title
#'
#' @param data name of the data frame
#' @param outcome column name of the outcome
#' @param exposure column name of the exposure
#' @param ref reference level of the exposure (default: 1st level)
#' @param measure six methods are available:
#'     \itemize{
#'       \item "MD": mean difference;
#'       \item "Cohen": Cohan's D;
#'       \item "Glass": Glass's G;
#'       \item "CL": common language;
#'       \item "Hedge": Hedge's G;
#'       \item "Cliff": Cliff's \eqn{\delta}
#'     }
#' @param hetsk indicator of heteroskedasticity
#'
#' @return a data frame of effect measures
#' @export
#'
#' @examples
#' @author Marta Regis \email{m.regis@@tue.nl}

cal_es <- function(
  data,
  outcome,
  study,
  exposure,
  ref,
  measure = c("MD", "Cohen", "Glass", "CL", "Hedge", "Cliff", "RR", "RD", "OR"),
  hetsk = FALSE, design){

  return(list(MeasuresOfEffect=measures, Yk=Yk, Sk=Sk, dfk=dfk))  # data frame
}
