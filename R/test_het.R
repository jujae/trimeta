#' Testing for heterogeneity
#'
#' \code{test_het} returns the test statistic according to the methods chosen
#'
#' @param Yk a vector of effect sizes
#' @param Sk a vector of standard errors of the effect sizes
#' @param dfk a vector of degrees of freedom
#' @param method three methods are available:
#'     \itemize{
#'       \item "CQ": Cochran's Q-test;
#'       \item "Bliss": Bliss' \eqn{\chi^2}-test;
#'       \item "CF": Cochran's F-tests (w/o heteroskedasticity)
#'     }
#' @param hetsk indicator of heteroskedasticity assumptions for Cochran's F-test method
#'
#' @return a list of test statistic, degrees of freedom, and p-value
#' @export
#'
#' @examples
#' test_het(yk, sk, dfk, method="CQ")
test_het <- function(Yk, Sk, dfk,
                     method=c("CQ", "Bliss", "CF"),
                     hetsk=TRUE) {

  Q <- cal_Q(Yk, Sk, dfk)
  m <- length(Yk)

  if(method == "CQ") {
    test_stat <- Q
    test_df   <- m-1
    test_pval <- pchisq(q=test_stat, df=test_stat, lower.tail=FALSE)
  }

  if(method == "Bliss") {
    avg_df    <- mean(dfk)
    test_stat <- m-1 + sqrt((avg_df-4)/(avg_df-2)) * (((avg_df-2)/avg_df)*Q-m+1)
    test_df   <- m-1
    test_pval <- pchisq(q=test_stat, df=test_stat, lower.tail=FALSE)
  }

  if(method == "CF") {
    SS2 <- sum((Yk-mean(Yk))^2)
    SP2 <- sum(dfk*Sk^2) / sum(dfk)
    SA2 <- mean(Sk^2)
    if(hetsk == TRUE) {
      FC  <- SS2 / SA2
      df1 <- (m-1)^2 * SA2^2 / ((m-1)*mean(Sk^4) + SA2^2)
      df2 <- m^2*SA2^2 / mean(Sk^4)
    } else {
      FC  <- SS2 / SP2
      df1 <- m-1
      df2 <- sum(dfk)
    }
    test_stat <- FC
    test_df   <- c(df1, df2)
    test_pval <- pf(q=test_stat, df1=df1, df2=df2, lower.tail=FALSE)
  }

  list(test_stat, test_df, test_pval)
}
