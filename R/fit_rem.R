fit_rem <- function(Yk, Sk, dfk, method=c("DL", "REML", "ML")) {
  if (method == "DL") {
    fit <- metafor::rma.uni(Yk, sei=Sk, method="DL")
  }
  if (method == "REML") {
    fit <- metafor::rma.uni(Yk, sei=Sk, method="REML")
  }
  if (method == "ML") {
    fit <- metafor::rma.uni(Yk, sei=Sk, method="ML")
  }
  return(list(fit=fit))
}
