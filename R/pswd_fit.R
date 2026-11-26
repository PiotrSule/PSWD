
#' Fit PSWD(a, b, tau) to a Numeric Sample
#'
#' @param data Numeric vector of non-negative observations.
#' @param method Estimation method: "ML", "MPS", "OLS", "WLS", or "LAW".
#' @return Named numeric vector of parameter estimates c(a, b, tau).

fit_PSWD <- function(data, method = c("ML", "MPS", "OLS", "WLS", "LAW")) {
  method <- match.arg(method)
  x <- sort(as.numeric(data))
  if (any(!is.finite(x)) || any(x < 0)) {
    stop("Input 'data' must contain finite non-negative values.")
  }
  
  n <- length(x)
  i_seq <- 1:n
  emp_F <- i_seq / (n + 1)
  w_wls <- ((n + 1)^2 * (n + 2)) / (i_seq * (n - i_seq + 1))
  
  idx_a <- 1; idx_b <- 2; idx_tau <- 3
  
  # Smooth 2D objective for fixed threshold tau >= 0
  obj_ab <- function(par_ab, tau_fixed) {
    a <- exp(par_ab[idx_a])
    b <- exp(par_ab[idx_b])
    if (!is.finite(a) || !is.finite(b) || a <= 1e-8 || b <= 1e-8) return(1e12)
    
    if (method == "ML") {
      dens <- dPSWD(x, a, b, tau_fixed)
      if (any(dens <= 0 | !is.finite(dens))) return(1e12)
      return(-sum(log(dens)))
    }
    
    F_vals <- pPSWD(x, a, b, tau_fixed)
    if (any(!is.finite(F_vals))) return(1e12)
    
    if (method == "MPS") {
      D <- diff(c(0, F_vals, 1))
      return(-mean(log(pmax(D, 1e-12))))
    }
    if (method == "OLS") return(sum((F_vals - emp_F)^2))
    if (method == "WLS") return(sum(w_wls * (F_vals - emp_F)^2))
    if (method == "LAW") return(sum(abs(F_vals - emp_F)))
  }
  
  # Inner 2D optimization for a given candidate tau
  eval_profile <- function(tau_val, maxit_inner = 300) {
    a_init <- max(mean(x) - 0.5 * tau_val, 0.3 * stats::sd(x), 0.1)
    opt <- stats::optim(log(c(a_init, 1.0)), obj_ab, tau_fixed = tau_val,
                        method = "Nelder-Mead",
                        control = list(maxit = maxit_inner, reltol = 1e-6))
    list(val = opt$value, ab = exp(opt$par))
  }
  
  # Step 1: 1D profile scan over tau in [0, Q_0.80(x)]
  tau_candidates <- seq(0, as.numeric(stats::quantile(x, 0.80)), length.out = 10)
  prof_vals <- vapply(tau_candidates, function(tg) eval_profile(tg, 300)$val, numeric(1))
  
  best_k <- which.min(prof_vals)
  low_tau  <- tau_candidates[max(idx_a, best_k - 1)]
  high_tau <- tau_candidates[min(length(tau_candidates), best_k + 1)]
  
  # Step 2: 1D Brent refinement on bracket [low_tau, high_tau]
  opt_1d <- stats::optimize(function(tg) eval_profile(tg, 350)$val,
                            interval = c(low_tau, high_tau), tol = 1e-4)
  
  best_tau <- if (opt_1d$objective < prof_vals[best_k]) opt_1d$minimum else tau_candidates[best_k]
  best_ab  <- eval_profile(best_tau, 450)$ab
  
  # Step 3: Joint 3D Nelder-Mead polish
  if (best_tau > 1e-5) {
    obj_3d <- function(p) obj_ab(c(p[idx_a], p[idx_b]), exp(p[idx_tau]))
    polish <- stats::optim(log(c(best_ab[idx_a], best_ab[idx_b], best_tau)), obj_3d,
                           method = "Nelder-Mead",
                           control = list(maxit = 800, reltol = 1e-8))
    if (polish$value <= min(opt_1d$objective, prof_vals[best_k])) {
      est <- exp(polish$par)
    } else {
      est <- c(best_ab, best_tau)
    }
  } else {
    est <- c(best_ab, 0)
  }
  
  if (est[idx_tau] < 1e-5) est[idx_tau] <- 0
  names(est) <- c("a", "b", "tau")
  return(est)
}
