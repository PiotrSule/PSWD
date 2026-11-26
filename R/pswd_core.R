
#' Normalizing Constant C(delta, b)
calc_C <- function(a, b, tau) {
  if (a <= 0 || b <= 0 || tau < 0) {
    stop("Parameters must satisfy: a > 0, b > 0, and tau >= 0.")
  }
  if (tau == 0) return(1)
  
  delta <- tau / a
  
  # Exact closed-form sum when b is a positive integer
  if (b >= 1 && abs(b - round(b)) < 1e-12) {
    b_int <- as.integer(round(b))
    k <- 0:(b_int - 1)
    return(delta^b_int + sum(choose(b_int - 1, k) *
                               delta^(b_int - 1 - k) * gamma((k + 1) / b_int)))
  }
  
  # Adaptive quadrature for arbitrary real b > 0
  val <- stats::integrate(function(u) (u + delta)^(b - 1) * exp(-u^b),
                          lower = 0, upper = Inf, rel.tol = 1e-12)$value
  return(delta^b + b * val)
}

#' Probability Density Function
dPSWD <- function(t, a, b, tau) {
  C <- calc_C(a, b, tau)
  res <- numeric(length(t))
  idx <- which(t >= 0)
  if (length(idx) > 0) {
    ti <- t[idx]
    exp_part <- ifelse(ti > tau, ((ti - tau) / a)^b, 0)
    res[idx] <- (1 / C) * (b / a) * ((ti / a)^(b - 1)) * exp(-exp_part)
  }
  return(res)
}

#' Cumulative Distribution Function
pPSWD <- function(t, a, b, tau) {
  if (tau <= 0) return(stats::pweibull(t, shape = b, scale = a))
  C <- calc_C(a, b, tau)
  delta <- tau / a
  res <- numeric(length(t))
  
  idx1 <- which(t >= 0 & t <= tau)
  if (length(idx1) > 0) {
    res[idx1] <- (1 / C) * ((t[idx1] / a)^b)
  }
  
  idx2 <- which(t > tau)
  if (length(idx2) > 0) {
    I_vals <- vapply(t[idx2], function(ti) {
      b * stats::integrate(function(u) (u + delta)^(b - 1) * exp(-u^b),
                           lower = 0, upper = (ti - tau) / a, rel.tol = 1e-12)$value
    }, numeric(1))
    res[idx2] <- (1 / C) * (delta^b + I_vals)
  }
  return(res)
}

#' Reliability Function
relPSWD <- function(t, a, b, tau) {
  1 - pPSWD(t, a, b, tau)
}

#' Hazard Rate Function
hPSWD <- function(t, a, b, tau) {
  R <- 1 - pPSWD(t, a, b, tau)
  ifelse(R > 1e-15, dPSWD(t, a, b, tau) / R, Inf)
}

#' Hazard Rate Average Function
haPSWD <- function(t, a, b, tau) {
  R <- 1 - pPSWD(t, a, b, tau)
  ifelse(t > 0 & R > 1e-15, -log(R) / t, NA)
}

#' Reversed Hazard Rate Function
rhPSWD <- function(t, a, b, tau) {
  F_val <- pPSWD(t, a, b, tau)
  ifelse(t > 0 & F_val > 1e-15, dPSWD(t, a, b, tau) / F_val, Inf)
}

#' Quantile Function
qPSWD <- function(p, a, b, tau) {
  if (tau <= 0) return(stats::qweibull(p, shape = b, scale = a))
  C <- calc_C(a, b, tau)
  delta <- tau / a
  p_tau <- (delta^b) / C
  
  vapply(p, function(pi) {
    if (pi <= 0) return(0)
    if (pi >= 1) return(Inf)
    if (pi <= p_tau) return(a * (C * pi)^(1 / b))
    
    upper <- max(tau + a, a * (-log(1 - pi))^(1 / b) + tau)
    iter <- 0
    while (pPSWD(upper, a, b, tau) < pi && iter < 60 && is.finite(upper * 2)) {
      upper <- upper * 2
      iter <- iter + 1
    }
    stats::uniroot(function(t) pPSWD(t, a, b, tau) - pi,
                   interval = c(tau, upper), tol = 1e-10)$root
  }, numeric(1))
}

#' Pseudo-random Number Generator
rPSWD <- function(n, a, b, tau) {
  qPSWD(stats::runif(n), a, b, tau)
}

#' r-th Moment
mPSWD <- function(r, a, b, tau) {
  if (tau <= 0) return(a^r * gamma(1 + r / b))
  C <- calc_C(a, b, tau)
  delta <- tau / a
  val <- stats::integrate(function(u) (u + delta)^(b + r - 1) * exp(-u^b),
                          lower = 0, upper = Inf, rel.tol = 1e-12)$value
  return((a^r / C) * ((b / (b + r)) * delta^(b + r) + b * val))
}

#' Shannon Entropy
shannon_PSWD <- function(a, b, tau) {
  C <- calc_C(a, b, tau)
  delta <- tau / a
  J1_p1 <- if (delta > 0) (delta^b) * (log(delta) - 1 / b) else 0
  J1_p2 <- b * stats::integrate(function(u) (u + delta)^(b - 1) * log(u + delta) * exp(-u^b),
                                lower = 0, upper = Inf, rel.tol = 1e-12)$value
  J2 <- b * stats::integrate(function(u) (u^b) * (u + delta)^(b - 1) * exp(-u^b),
                             lower = 0, upper = Inf, rel.tol = 1e-12)$value
  return(log(a * C / b) - ((b - 1) / C) * (J1_p1 + J1_p2) + (1 / C) * J2)
}
