pl <- function(Yk, Sk, dfk, alpha) {
  tau2h <- pimeta::tau2h(Yk, Sk, method="ML")$tau2h
  w <- (Sk^2 + tau2h)^-1
  theta_hat <- sum(y*w)/sum(w)
  fit <- pimeta::cima(Yk, Sk, alpha=alpha, method="PL")
  return(list(theta=theta_hat, tau2=tau2hat, ci=c(fit$lci, fit$uci)))
}
