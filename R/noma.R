noma <- function(Yk, Sk, dfk, alpha) {
  # TODO: Implement tau2h and cima
  tau2h <- tau2h(Yk, Sk, method="ML")$tau2h
  w <- (Sk^2 + tau2h)^-1
  theta_hat <- sum(y*w)/sum(w)
  fit <- cima(Yk, Sk, alpha=alpha, method="BC")
  return(list(theta=theta_hat, tau2=tau2hat, ci=c(fit$lci, fit$uci)))
}
