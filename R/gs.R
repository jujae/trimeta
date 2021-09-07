gs <- function(Yk, Sk, dfk, x, alpha=0.05) {
  Sk2 <- Sk**2
  fit <- metaLik::metaLik(Yk~x, sigma2=Sk2)
  fit_pl <- metaLik:::.profLik(fit)
  ci <- predict(fit_pl$smooth.rskovs.inv, x=c(qnorm(alpha/2), qnorm(1-alpha/2)))
  return(list(theta=fit$beta.mle, tau2=fit$tau2.mle, ci=sort(ci$y)))
}
