#' Calculate effect measures
#'
#' @param data name of the data frame
#' @param outcome column name of the outcome
#' @param exposure column name of the exposure
#' @param ref reference level of the exposure (default: 1st level)
#' @param measure six methods are available:
#'     \itemize{
#'       \item "MD": Mean Difference;
#'       \item "Cohen": Cohan's D;
#'       \item "Glass": Glass's G;
#'       \item "CL": common language;
#'       \item "Hedge": Hedge's G;
#'       \item "Cliff": Cliff's \eqn{\delta}
#'       \item "RR" Risk Ratio;
#'       \item "RD" Risk Difference;
#'       \item "OR" Odds Ratio
#'     }
#' @param hetsk indicator of heteroskedasticity
#' @param design design of the 2x2 table
#'
#' @return a data frame of effect measures
#' @export
#'
#' @examples
#' \dontrun{
#' test
#' }
#' @author Marta Regis \email{m.regis@@tue.nl}

cal_es <- function(
  data, outcome, study, exposure, ref,
  measure = c("MD", "Cohen", "Glass", "CL", "Hedge", "Cliff", "RR", "RD", "OR"),
  hetsk = FALSE, design){

  return(list(MeasuresOfEffect=measures, Yk=Yk, Sk=Sk, dfk=dfk))  # data frame
}
