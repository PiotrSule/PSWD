# Output Directory: C:/PSWD (created automatically if it does not exist)
# Generates all Figures (Figure1.pdf to Figure8.pdf) and Tables (Table1 to Table7)

library(pswd)

out_dir <- "C:/PSWD"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
cat("Saving all replicated figures and tables to:", out_dir, "\n\n")

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}
library(openxlsx)

# Figure 1: Representative PDF shapes of the PSWD(a, b, τ ) for selected parameter values

cairo_pdf(file.path(out_dir, "Figure1.pdf"), width = 7.5, height = 4.8, family = "Arial")
par(mar = c(4.2, 4.2, 1.5, 1.0))

t_grid <- seq(0.001, 5.5, length.out = 500)

df_fig1 <- data.frame(
  a   = c(2.50, 2.50, 2.50, 1.50),
  b   = c(0.70, 1.00, 1.80, 5.00),
  tau = c(1.50, 1.80, 0.80, 3.20),
  col = c("#222222", "#C0392B", "#27AE60", "#2980B9"),
  lty = c(1, 2, 4, 3),
  stringsAsFactors = FALSE
)

leg_fig1 <- expression(
  theta == "(2.50, 0.70, 1.50) [L-shaped]",
  theta == "(2.50, 1.00, 1.80) [Plateau-exponential]",
  theta == "(2.50, 1.80, 0.80) [Broad unimodal]",
  theta == "(1.50, 5.00, 3.20) [Sharp unimodal]"
)

plot(NA, xlim = c(0, 5.5), ylim = c(0, 1.05),
     xlab = "t", ylab = "Density f(t)")
grid(col = "gray90", lty = 1)

for (i in 1:nrow(df_fig1)) {
  lines(t_grid, dPSWD(t_grid, df_fig1$a[i], df_fig1$b[i], df_fig1$tau[i]),
        col = df_fig1$col[i], lty = df_fig1$lty[i], lwd = 2.2)
}

legend("topright", legend = leg_fig1, col = df_fig1$col, lty = df_fig1$lty,
       lwd = 2.2, cex = 0.85, bg = "white")
dev.off()
cat("Saved Figure1.pdf\n")

# Figure 2: Graphical representation of the deviation of PSWD(2.5, 2.0, τ) 
# from the Weibull(2.5, 2.0) distribution for similarity levels M ∈ [0.50, 1.00].

calc_M_base <- function(a, b, tau) {
  if (tau <= 0) return(1.0)
  u <- seq(1e-5, 1 - 1e-5, length.out = 3000)
  du <- u[2] - u[1]
  t_val <- (u^2) / (1 - u^2)
  jac   <- (2 * u) / ((1 - u^2)^2)
  diff_v <- abs(dPSWD(t_val, a, b, tau) - dweibull(t_val, shape = b, scale = a)) * jac
  max(0, min(1, 1 - 0.5 * sum(diff_v) * du))
}

M_targets <- seq(1.00, 0.50, by = -0.05)
tau_sol <- vapply(M_targets, function(m_tar) {
  if (m_tar >= 1.0) return(0.0)
  uniroot(function(tg) calc_M_base(2.5, 2.0, tg) - m_tar,
          interval = c(0, 6.0), tol = 1e-5)$root
}, numeric(1))

cairo_pdf(file.path(out_dir, "Figure2.pdf"), width = 7.5, height = 4.8, family = "Arial")
par(mar = c(4.2, 4.2, 2.2, 1.0))

t_grid2 <- seq(0, 7.5, length.out = 500)
cols2 <- c("#222222", "#555555", "#7F8C8D", "#95A5A6", "#7F8C8D",
           "#E67E22", "#2980B9", "#C0392B", "#27AE60", "#8E44AD", "#16A085")
ltys2 <- c(1, 3, 2, 4, 2, 1, 1, 1, 1, 1, 1)
lwds2 <- c(2.2, 1.8, 1.8, 1.8, 1.8, 1.8, 1.8, 1.8, 1.8, 1.8, 1.8)

plot(NA, xlim = c(0, 7.5), ylim = c(0, 0.43),
     xlab = "t", ylab = "f(t)",
     main = expression(paste("PSWD(2.5, 2.0, ", tau, ") deviation from Weibull(2.5, 2.0)")))
grid(col = "gray90", lty = 1)

for (i in seq_along(tau_sol)) {
  lines(t_grid2, dPSWD(t_grid2, a = 2.5, b = 2.0, tau = tau_sol[i]),
        col = cols2[i], lty = ltys2[i], lwd = lwds2[i])
}

leg2 <- c(
  expression(paste("M = 1, ", tau, " = 0 (Weibull)")),
  as.expression(lapply(2:length(M_targets), function(i) {
    bquote(M == .(sprintf("%.2f", M_targets[i])) * "," ~ tau == .(sprintf("%.3f", tau_sol[i])))
  }))
)

legend("topright", legend = leg2, col = cols2, lty = ltys2, lwd = lwds2,
       cex = 0.78, bg = "white")
dev.off()
cat("Saved Figure2.pdf\n")

# Figure 3: Reliability functions R(t; a, b, τ ) of the PSWD(a, b, τ ) 
# for the parameter values used in Figure 1.

cairo_pdf(file.path(out_dir, "Figure3.pdf"), width = 7.5, height = 4.8, family = "Arial")
par(mar = c(4.2, 4.2, 1.5, 1.0))

t_grid3 <- seq(0, 5.5, length.out = 400)

leg_fig3 <- expression(
  theta == "(2.50, 0.70, 1.50) [L-shaped PDF]",
  theta == "(2.50, 1.00, 1.80) [Plateau-exponential PDF]",
  theta == "(2.50, 1.80, 0.80) [Broad unimodal PDF]",
  theta == "(1.50, 5.00, 3.20) [Sharp unimodal PDF]"
)

plot(NA, xlim = c(0, 5.5), ylim = c(0, 1.02),
     xlab = "t", ylab = "Reliability R(t)")
grid(col = "gray90", lty = 1)

for (i in 1:nrow(df_fig1)) {
  lines(t_grid3, relPSWD(t_grid3, df_fig1$a[i], df_fig1$b[i], df_fig1$tau[i]),
        col = df_fig1$col[i], lty = df_fig1$lty[i], lwd = 2.2)
}

legend("topright", legend = leg_fig3, col = df_fig1$col, lty = df_fig1$lty,
       lwd = 2.2, cex = 0.82, bg = "white")
dev.off()
cat("Saved Figure3.pdf\n")

# Figure 4: Hazard rate functions (left) and hazard rate average functions (right) 
# of the PSWD(a, b, τ ) for selected parameter values.

df_hrf <- data.frame(
  a   = c(2.50, 0.70, 2.50, 2.50, 1.50),
  b   = c(0.70, 0.65, 1.00, 1.80, 3.50),
  tau = c(1.50, 2.20, 1.80, 0.80, 3.20),
  col = c("#222222", "#8E44AD", "#C0392B", "#27AE60", "#2980B9"),
  lty = c(1, 5, 2, 4, 3),
  stringsAsFactors = FALSE
)

leg_hrf <- expression(
  theta == "(2.50, 0.70, 1.50) [Decreasing]",
  theta == "(0.70, 0.65, 2.20) [Bathtub / non-monotonic]",
  theta == "(2.50, 1.00, 1.80) [Increasing-constant]",
  theta == "(2.50, 1.80, 0.80) [Smoothly increasing]",
  theta == "(1.50, 3.50, 3.20) [Threshold-surge]"
)

cairo_pdf(file.path(out_dir, "Figure4.pdf"), width = 11, height = 5.0, family = "Arial")
layout(matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE), heights = c(1, 0.25))

t_hrf <- seq(0.02, 5.1, length.out = 350)

# Panel (A): HRF
par(mar = c(4.0, 4.2, 2.0, 1.0))
plot(NA, xlim = c(0, 5.1), ylim = c(0, 2.65),
     xlab = "t", ylab = "HRF h(t)",
     main = "(A) Hazard Rate Function", adj = 0)
grid(col = "gray90", lty = 1)
for (i in 1:nrow(df_hrf)) {
  lines(t_hrf, hPSWD(t_hrf, df_hrf$a[i], df_hrf$b[i], df_hrf$tau[i]),
        col = df_hrf$col[i], lty = df_hrf$lty[i], lwd = 2.1)
}

# Panel (B): HRAF
par(mar = c(4.0, 4.2, 2.0, 1.0))
plot(NA, xlim = c(0, 5.1), ylim = c(0, 1.30),
     xlab = "t", ylab = "HRAF H(t)",
     main = "(B) Hazard Rate Average Function", adj = 0)
grid(col = "gray90", lty = 1)
for (i in 1:nrow(df_hrf)) {
  lines(t_hrf, haPSWD(t_hrf, df_hrf$a[i], df_hrf$b[i], df_hrf$tau[i]),
        col = df_hrf$col[i], lty = df_hrf$lty[i], lwd = 2.1)
}

# Shared Bottom Legend
par(mar = c(0.1, 0.2, 0.1, 0.2))
plot.new()
legend("center", legend = leg_hrf, col = df_hrf$col, lty = df_hrf$lty,
       lwd = 2.1, bty = "n", cex = 0.85, ncol = 3)
dev.off()
cat("Saved Figure4.pdf\n")

# Figure 5: Reversed hazard rate functions of the PSWD(a, b, τ) 
# for the parameter values used in Figure 4.

cairo_pdf(file.path(out_dir, "Figure5.pdf"), width = 7.5, height = 4.8, family = "Arial")
par(mar = c(4.2, 4.4, 1.5, 1.0))

t_rhrf <- seq(0.08, 5.1, length.out = 350)

leg_fig5 <- expression(
  theta == "(2.50, 0.70, 1.50)",
  theta == "(0.70, 0.65, 2.20)",
  theta == "(2.50, 1.00, 1.80)",
  theta == "(2.50, 1.80, 0.80)",
  theta == "(1.50, 3.50, 3.20)"
)

plot(NA, xlim = c(0, 5.1), ylim = c(0, 5.2),
     xlab = "t", ylab = expression(paste("RHRF ", tilde(h)(t))))
grid(col = "gray90", lty = 1)

for (i in 1:nrow(df_hrf)) {
  lines(t_rhrf, rhPSWD(t_rhrf, df_hrf$a[i], df_hrf$b[i], df_hrf$tau[i]),
        col = df_hrf$col[i], lty = df_hrf$lty[i], lwd = 2.1)
}

legend("topright", legend = leg_fig5, col = df_hrf$col, lty = df_hrf$lty,
       lwd = 2.1, cex = 0.85, bg = "white")
dev.off()
cat("Saved Figure5.pdf\n")

# Figure 6: Comparison between the theoretical CDF and empirical CDFs generated via
# the PSWD PRNG for θ = (2.50, 1.80, 1.50) with sample sizes n = 30 (left) and n = 1000 (right).

set.seed(101)
x_small <- rPSWD(30,   a = 2.50, b = 1.80, tau = 1.50)
x_large <- rPSWD(1000, a = 2.50, b = 1.80, tau = 1.50)

cairo_pdf(file.path(out_dir, "Figure6.pdf"), width = 11, height = 4.6, family = "Arial")
par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.2, 1.0))

t_grid6 <- seq(0, 5.8, length.out = 400)
cdf_theo <- pPSWD(t_grid6, a = 2.50, b = 1.80, tau = 1.50)

# Panel (A): Small sample (n = 30)
plot(t_grid6, cdf_theo, type = "l", col = "#222222", lwd = 2.1,
     xlim = c(0, 5.7), ylim = c(0, 1.02),
     xlab = "t", ylab = "F(t)",
     main = "(A) Small sample (n = 30)", adj = 0)
grid(col = "gray90", lty = 1)
lines(ecdf(x_small), verticals = TRUE, do.points = FALSE,
      col = "#C0392B", lty = 2, lwd = 1.8)
legend("bottomright",
       legend = c("Theoretical CDF", "Empirical CDF (n = 30)"),
       col = c("#222222", "#C0392B"), lty = c(1, 2), lwd = c(2.1, 1.8),
       cex = 0.85, bg = "white")

# Panel (B): Large sample (n = 1000)
plot(t_grid6, cdf_theo, type = "l", col = "#222222", lwd = 2.1,
     xlim = c(0, 5.7), ylim = c(0, 1.02),
     xlab = "t", ylab = "F(t)",
     main = "(B) Large sample (n = 1000)", adj = 0)
grid(col = "gray90", lty = 1)
lines(ecdf(x_large), verticals = TRUE, do.points = FALSE,
      col = "#2980B9", lty = 2, lwd = 1.8)
legend("bottomright",
       legend = c("Theoretical CDF", "Empirical CDF (n = 1000)"),
       col = c("#222222", "#2980B9"), lty = c(1, 2), lwd = c(2.1, 1.8),
       cex = 0.85, bg = "white")

dev.off()
cat("Saved Figure6.pdf\n")

# Figure 7: Quantile functions for the parameter vales from Figure 4 (left) 
# and Shannon entropy as a function of the threshold parameter τ for the corresponding (a, b) pairs (right).

cairo_pdf(file.path(out_dir, "Figure7.pdf"), width = 11, height = 4.6, family = "Arial")
par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.2, 1.0))

p_grid <- seq(0.005, 0.96, length.out = 150)

leg_fig7A <- expression(
  theta == "(2.50, 0.70, 1.50)",
  theta == "(0.70, 0.65, 2.20)",
  theta == "(2.50, 1.00, 1.80)",
  theta == "(2.50, 1.80, 0.80)",
  theta == "(1.50, 3.50, 3.20)"
)

plot(NA, xlim = c(0, 1), ylim = c(0, 7.2),
     xlab = "Probability p", ylab = "Quantile Q(p)",
     main = "(A) Quantile Function Q(p)", adj = 0)
grid(col = "gray90", lty = 1)
for (i in 1:nrow(df_hrf)) {
  lines(p_grid, qPSWD(p_grid, df_hrf$a[i], df_hrf$b[i], df_hrf$tau[i]),
        col = df_hrf$col[i], lty = df_hrf$lty[i], lwd = 2.1)
}
legend("topleft", legend = leg_fig7A, col = df_hrf$col, lty = df_hrf$lty,
       lwd = 2.1, cex = 0.82, bg = "white")

tau_ent <- seq(0, 5.0, length.out = 40)
plot(NA, xlim = c(0, 5.0), ylim = c(0.4, 2.4),
     xlab = expression(paste("Threshold ", tau)), ylab = "Shannon Entropy H(T)",
     main = expression(paste("(B) Shannon Entropy vs. Threshold ", tau)), adj = 0)
grid(col = "gray90", lty = 1)
for (i in 1:nrow(df_hrf)) {
  ent_v <- sapply(tau_ent, function(ti) shannon_PSWD(df_hrf$a[i], df_hrf$b[i], ti))
  lines(tau_ent, ent_v, col = df_hrf$col[i], lty = df_hrf$lty[i], lwd = 2.1)
}
leg_fig7B <- sprintf("(a, b) = (%.2f, %.2f)", df_hrf$a, df_hrf$b)
legend("bottomright", legend = leg_fig7B, col = df_hrf$col, lty = df_hrf$lty,
       lwd = 2.1, cex = 0.82, bg = "white")

dev.off()
cat("Saved Figure7.pdf\n")


#  Tables 1 and 2

fit_PSWD_mc_exact <- function(data, method = c("ML", "MPS", "OLS", "WLS", "LAW")) {
  method <- match.arg(method)
  x <- sort(data)
  n <- length(x)
  i_seq <- 1:n
  emp_F <- i_seq / (n + 1)
  w_wls <- ((n + 1)^2 * (n + 2)) / (i_seq * (n - i_seq + 1))
  idx_a <- 1; idx_b <- 2; idx_tau <- 3
  
  obj_ab <- function(par_ab, tau_fixed) {
    a <- exp(par_ab[idx_a]); b <- exp(par_ab[idx_b])
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
      return(-mean(log(pmax(D, 1e-16))))
    }
    
    if (method == "OLS") return(sum((F_vals - emp_F)^2))
    if (method == "WLS") return(sum(w_wls * (F_vals - emp_F)^2))
    if (method == "LAW") return(sum(abs(F_vals - emp_F)))
  }
  
  eval_profile <- function(tau_val, maxit_inner = 250) {
    a_init <- max(mean(x) - 0.5 * tau_val, 0.3 * sd(x), 0.1)
    opt <- optim(log(c(a_init, 1.5)), obj_ab, tau_fixed = tau_val,
                 method = "Nelder-Mead", control = list(maxit = maxit_inner, reltol = 1e-5))
    list(val = opt$value, ab = exp(opt$par))
  }
  
  tau_candidates <- seq(0, as.numeric(quantile(x, 0.80)), length.out = 8)
  prof_vals <- vapply(tau_candidates, function(tg) eval_profile(tg, 250)$val, numeric(1))
  best_k <- which.min(prof_vals)
  low_tau  <- tau_candidates[max(idx_a, best_k - 1)]
  high_tau <- tau_candidates[min(length(tau_candidates), best_k + 1)]
  
  opt_1d <- optimize(function(tg) eval_profile(tg, 300)$val, interval = c(low_tau, high_tau), tol = 1e-3)
  best_tau <- if (opt_1d$objective < prof_vals[best_k]) opt_1d$minimum else tau_candidates[best_k]
  best_ab  <- eval_profile(best_tau, 400)$ab
  
  if (best_tau > 1e-5) {
    obj_3d <- function(p) obj_ab(c(p[idx_a], p[idx_b]), exp(p[idx_tau]))
    polish <- optim(log(c(best_ab[idx_a], best_ab[idx_b], best_tau)), obj_3d,
                    method = "Nelder-Mead", control = list(maxit = 600, reltol = 1e-8))
    est <- if (polish$value <= min(opt_1d$objective, prof_vals[best_k])) exp(polish$par) else c(best_ab, best_tau)
  } else {
    est <- c(best_ab, 0)
  }
  if (est[idx_tau] < 1e-5) est[idx_tau] <- 0
  names(est) <- c("a", "b", "tau")
  return(est)
}


run_mc_table <- function(theta_true, n_vec = c(30, 50, 100, 250, 500), M_reps = 1000) {
  methods <- c("ML", "MPS", "OLS", "WLS", "LAW")
  i1 <- 1; i2 <- 2; i3 <- 3
  
  n_cores <- max(1, parallel::detectCores() - 1)
  cl <- parallel::makeCluster(n_cores)
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
  
  if (requireNamespace("pswd", quietly = TRUE)) {
    parallel::clusterEvalQ(cl, library(pswd))
  } else {
    parallel::clusterExport(cl, varlist = c("calc_C", "dPSWD", "pPSWD", "relPSWD",
                                            "hPSWD", "haPSWD", "rhPSWD", "qPSWD",
                                            "rPSWD", "mPSWD", "shannon_PSWD"),
                            envir = .GlobalEnv)
  }
  
  parallel::clusterExport(cl, varlist = c("fit_PSWD_mc_exact", "theta_true", "methods", "i1", "i2", "i3"),
                          envir = environment())
  
  res_list <- list()
  for (n in n_vec) {
    cat(sprintf("Replicating n = %d (%d reps on %d threads)...\n", n, M_reps, n_cores))
    sim_out <- parallel::parLapply(cl, 1:M_reps, function(r) {
      set.seed(100000 * n + r)
      x_sim <- rPSWD(n, theta_true[i1], theta_true[i2], theta_true[i3])
      est_mat <- matrix(NA, nrow = length(methods), ncol = 3)
      for (m in seq_along(methods)) {
        est_mat[m, ] <- fit_PSWD_mc_exact(x_sim, method = methods[m])
      }
      est_mat
    })
    
    est_arr <- simplify2array(sim_out)
    for (m in seq_along(methods)) {
      ests <- t(est_arr[m, , ])
      bias <- colMeans(ests) - theta_true
      rmse <- sqrt(colMeans((t(t(ests) - theta_true))^2))
      res_list[[length(res_list) + 1]] <- data.frame(
        n = n, Method = methods[m],
        BIAS_a = round(bias[i1], 4), RMSE_a = round(rmse[i1], 4),
        BIAS_b = round(bias[i2], 4), RMSE_b = round(rmse[i2], 4),
        BIAS_tau = round(bias[i3], 4), RMSE_tau = round(rmse[i3], 4)
      )
    }
  }
  do.call(rbind, res_list)
}

cat("Running Monte Carlo replication")
table1_mc <- run_mc_table(c(2.50, 1.80, 1.50), M_reps = 50)
openxlsx::write.xlsx(table1_mc, file.path(out_dir, "Table1.xlsx"), overwrite = TRUE)

table2_mc <- run_mc_table(c(2.50, 0.90, 1.50), M_reps = 50)
openxlsx::write.xlsx(table2_mc, file.path(out_dir, "Table2.xlsx"), overwrite = TRUE)


#  Tables 3 to 7 and Figure8.pdf


# Competing 3-parameter models (Appendix A)
dEW <- function(t, a, b, alpha) {
  z <- (t / a)^b
  (alpha * b / a) * ((t / a)^(b - 1)) * ((1 - exp(-z))^(alpha - 1)) * exp(-z)
}
pEW <- function(t, a, b, alpha) {
  (1 - exp(-(t / a)^b))^alpha
}

dMOW <- function(t, a, b, alpha) {
  z <- (t / a)^b
  ez <- exp(-z)
  (alpha * (b / a) * ((t / a)^(b - 1)) * ez) / ((1 - (1 - alpha) * ez)^2)
}
pMOW <- function(t, a, b, alpha) {
  ez <- exp(-(t / a)^b)
  (1 - ez) / (1 - (1 - alpha) * ez)
}

dAPW <- function(t, a, b, alpha) {
  if (abs(alpha - 1) < 1e-8) return(dweibull(t, shape = b, scale = a))
  z <- (t / a)^b
  ez <- exp(-z)
  (log(alpha) / (alpha - 1)) * (b / a) * ((t / a)^(b - 1)) * ez * (alpha^(1 - ez))
}
pAPW <- function(t, a, b, alpha) {
  if (abs(alpha - 1) < 1e-8) return(pweibull(t, shape = b, scale = a))
  ez <- exp(-(t / a)^b)
  (alpha^(1 - ez) - 1) / (alpha - 1)
}

d3PW <- function(t, a, b, tau) {
  ifelse(t <= tau, 0, dweibull(t - tau, shape = b, scale = a))
}
p3PW <- function(t, a, b, tau) {
  ifelse(t <= tau, 0, pweibull(t - tau, shape = b, scale = a))
}

calc_all_gof <- function(x, pfun, p1, p2, p3) {
  x_s <- sort(x); n <- length(x_s)
  F_v <- pmin(pmax(pfun(x_s, p1, p2, p3), 1e-12), 1 - 1e-12)
  i <- 1:n
  ks  <- max(max(i / n - F_v), max(F_v - (i - 1) / n))
  cvm <- sum((F_v - (2 * i - 1) / (2 * n))^2) + 1 / (12 * n)
  u2  <- cvm - n * (mean(F_v) - 0.5)^2
  ad  <- -n - mean((2 * i - 1) * (log(F_v) + log(1 - rev(F_v))))
  c(KS = ks, AD = ad, CvM = cvm, U2 = u2)
}
fit_all_models_ic <- function(x) {
  n <- length(x); k <- 3; i1 <- 1; i2 <- 2; i3 <- 3
  p_pswd <- as.numeric(fit_PSWD(x, method = "ML"))
  logL_pswd <- sum(log(dPSWD(x, p_pswd[i1], p_pswd[i2], p_pswd[i3])))
  fit_pos3 <- function(dfun) {
    best_v <- Inf; best_p <- NULL
    for (init3 in c(0.5, 1.2, 2.5)) {
      opt <- optim(log(c(median(x), 1.5, init3)), function(par) {
        p <- exp(par); d <- dfun(x, p[i1], p[i2], p[i3])
        if (any(!is.finite(d) | d <= 0)) return(1e12); -sum(log(d))
      }, method = "Nelder-Mead", control = list(maxit = 3000))
      if (opt$value < best_v) { best_v <- opt$value; best_p <- exp(opt$par) }
    }
    list(est = best_p, logL = -best_v)
  }
  r_ew <- fit_pos3(dEW); r_mow <- fit_pos3(dMOW); r_apw <- fit_pos3(dAPW)
  mx <- min(x)
  opt_3pw <- optim(c(log(median(x)), log(1.5), 0), function(par) {
    a <- exp(par[i1]); b <- exp(par[i2]); tau <- (mx - 1e-5) / (1 + exp(-par[i3]))
    d <- d3PW(x, a, b, tau); if (any(!is.finite(d) | d <= 0)) return(1e12); -sum(log(d))
  }, method = "Nelder-Mead", control = list(maxit = 3000))
  p_3pw <- c(exp(opt_3pw$par[i1]), exp(opt_3pw$par[i2]), (mx - 1e-5) / (1 + exp(-opt_3pw$par[i3])))
  logLs <- c(logL_pswd, r_ew$logL, r_mow$logL, r_apw$logL, -opt_3pw$value)
  ests  <- rbind(p_pswd, r_ew$est, r_mow$est, r_apw$est, p_3pw)
  tab <- data.frame(
    Model = c("PSWD", "EW", "MOW", "APW", "3PW"),
    Param1 = round(ests[, i1], 4), Param2 = round(ests[, i2], 4), Param3 = round(ests[, i3], 4),
    logLik = round(logLs, 3),
    AIC = round(-2 * logLs + 2 * k, 3),
    BIC = round(-2 * logLs + k * log(n), 3),
    HQIC = round(-2 * logLs + 2 * k * log(log(n)), 3)
  )
  list(table = tab, ests = ests)
}
run_gof_boot <- function(x, ests, B = 500, seed = 2026) {
  set.seed(seed)
  n <- length(x)
  i1 <- 1; i2 <- 2; i3 <- 3
  
  rEW  <- function(n, a, b, alpha) a * (-log(1 - runif(n)^(1 / alpha)))^(1 / b)
  rMOW <- function(n, a, b, alpha) { u <- runif(n); a * (-log((1 - u) / (1 - (1 - alpha) * u)))^(1 / b) }
  rAPW <- function(n, a, b, alpha) { u <- runif(n); a * (-log(1 - log(1 + (alpha - 1) * u) / log(alpha)))^(1 / b) }
  r3PW <- function(n, a, b, tau)   tau + rweibull(n, shape = b, scale = a)
  
  p_pswd <- ests[1, ]; p_ew <- ests[2, ]; p_mow <- ests[3, ]; p_apw <- ests[4, ]; p_3pw <- ests[5, ]
  
  specs <- list(
    "PSWD" = list(
      p = p_pswd, pfun = pPSWD, rfun = rPSWD,
      ffun = function(xb) as.numeric(fit_PSWD(xb, "ML"))
    ),
    "EW" = list(
      p = p_ew, pfun = pEW, rfun = rEW,
      ffun = function(xb) exp(optim(log(p_ew), function(p) -sum(log(dEW(xb, exp(p[i1]), exp(p[i2]), exp(p[i3])))))$par)
    ),
    "MOW" = list(
      p = p_mow, pfun = pMOW, rfun = rMOW,
      ffun = function(xb) exp(optim(log(p_mow), function(p) -sum(log(dMOW(xb, exp(p[i1]), exp(p[i2]), exp(p[i3])))))$par)
    ),
    "APW" = list(
      p = p_apw, pfun = pAPW, rfun = rAPW,
      ffun = function(xb) exp(optim(log(p_apw), function(p) -sum(log(dAPW(xb, exp(p[i1]), exp(p[i2]), exp(p[i3])))))$par)
    ),
    "3PW" = list(
      p = p_3pw, pfun = p3PW, rfun = r3PW,
      ffun = function(xb) {
        mx <- min(xb)
        o <- optim(c(log(p_3pw[i1]), log(p_3pw[i2]), 0), function(p) {
          -sum(log(d3PW(xb, exp(p[i1]), exp(p[i2]), (mx - 1e-5) / (1 + exp(-p[i3])))))
        })
        c(exp(o$par[i1]), exp(o$par[i2]), (mx - 1e-5) / (1 + exp(-o$par[i3])))
      }
    )
  )
  
  out_list <- lapply(names(specs), function(mn) {
    sp <- specs[[mn]]
    p_hat <- sp$p
    obs <- calc_all_gof(x, sp$pfun, p_hat[i1], p_hat[i2], p_hat[i3])
    
    b_mat <- matrix(NA, nrow = B, ncol = 4)
    for (b in 1:B) {
      xb <- sort(sp$rfun(n, p_hat[i1], p_hat[i2], p_hat[i3]))
      pb <- try(sp$ffun(xb), silent = TRUE)
      if (!inherits(pb, "try-error") && length(pb) == 3 && all(is.finite(pb))) {
        b_mat[b, ] <- calc_all_gof(xb, sp$pfun, pb[i1], pb[i2], pb[i3])
      }
    }
    b_mat <- b_mat[complete.cases(b_mat), , drop = FALSE]
    pv <- colMeans(sweep(b_mat, 2, obs, ">="))
    
    data.frame(
      Model = mn,
      KS  = round(obs[1], 4), p_KS  = round(pv[1], 4),
      AD  = round(obs[2], 4), p_AD  = round(pv[2], 4),
      CvM = round(obs[3], 4), p_CvM = round(pv[3], 4),
      U2  = round(obs[4], 4), p_U2  = round(pv[4], 4)
    )
  })
  
  do.call(rbind, out_list)
}


# Dataset I: FTSE Daily Volatility (n = 146)

ftse_ret <- sort(abs(diff(log(as.numeric(datasets::EuStockMarkets[1:151, "FTSE"])))) * 100)
ftse_ret <- ftse_ret[ftse_ret > 0]
fit1 <- fit_all_models_ic(ftse_ret)
table3_ic  <- fit1$table
table4_gof <- run_gof_boot(ftse_ret, fit1$ests, B = 500)
write.xlsx(table3_ic,  file.path(out_dir, "Table3.xlsx"),  overwrite = TRUE)
write.xlsx(table4_gof, file.path(out_dir, "Table4.xlsx"), overwrite = TRUE)
print(table3_ic, row.names = FALSE)
print(table4_gof, row.names = FALSE)

# Dataset II: Monthly Aviation Demand Growth Rates (n = 139)

air_growth <- sort(abs(diff(log(as.numeric(datasets::AirPassengers)))) * 100)
air_growth <- air_growth[air_growth > 0]
fit2 <- fit_all_models_ic(air_growth)
table5_ic  <- fit2$table
table6_gof <- run_gof_boot(air_growth, fit2$ests, B = 500)
write.xlsx(table5_ic,  file.path(out_dir, "Table5.xlsx"),  overwrite = TRUE)
write.xlsx(table6_gof, file.path(out_dir, "Table6.xlsx"), overwrite = TRUE)
print(table5_ic, row.names = FALSE)
print(table6_gof, row.names = FALSE)


#  Figure 8: Fitted PDFs overlaid on the empirical histograms for Dataset I (left)
# and Dataset II (right).

i1 <- 1; i2 <- 2; i3 <- 3
c1 <- "#C0392B"; c2 <- "#2980B9"; c3 <- "#27AE60"; c4 <- "#8E44AD"; c5 <- "#E67E22"
l1 <- 1; l2 <- 2; l3 <- 4; l4 <- 3; l5 <- 5
w1 <- 2.6; w2 <- 1.9; w3 <- 1.9; w4 <- 1.9; w5 <- 1.9
pdf(file.path(out_dir, "Figure8.pdf"), width = 11, height = 4.8)
layout(matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE), heights = c(1, 0.22))
par(mar = c(4.2, 4.2, 2.0, 1.2))
b1 <- seq(0, max(ftse_ret) * 1.05, length.out = 13)
t1 <- seq(0.005, max(ftse_ret) * 1.08, length.out = 500)
hist(ftse_ret, breaks = b1, freq = FALSE, col = "gray93", border = "gray70",
     xlim = c(0, max(t1)), ylim = c(0, 1.35),
     xlab = "Daily Absolute Log-Return (%)", ylab = "Density",
     main = "(A) Dataset I: FTSE Daily Volatility", adj = 0)
grid(col = "gray90", lty = 1)
lines(t1, dEW(t1,   fit1$ests[2,i1], fit1$ests[2,i2], fit1$ests[2,i3]), col = c2, lty = l2, lwd = w2)
lines(t1, dMOW(t1,  fit1$ests[3,i1], fit1$ests[3,i2], fit1$ests[3,i3]), col = c3, lty = l3, lwd = w3)
lines(t1, dAPW(t1,  fit1$ests[4,i1], fit1$ests[4,i2], fit1$ests[4,i3]), col = c4, lty = l4, lwd = w4)
lines(t1, d3PW(t1,  fit1$ests[5,i1], fit1$ests[5,i2], fit1$ests[5,i3]), col = c5, lty = l5, lwd = w5)
lines(t1, dPSWD(t1, fit1$ests[1,i1], fit1$ests[1,i2], fit1$ests[1,i3]), col = c1, lty = l1, lwd = w1)
par(mar = c(4.2, 4.2, 2.0, 1.2))
b2 <- seq(0, max(air_growth) * 1.05, length.out = 12)
t2 <- seq(0.05, max(air_growth) * 1.08, length.out = 500)
hist(air_growth, breaks = b2, freq = FALSE, col = "gray93", border = "gray70",
     xlim = c(0, max(t2)), ylim = c(0, 0.085),
     xlab = "Monthly Passenger Growth Rate (%)", ylab = "Density",
     main = "(B) Dataset II: Aviation Demand Growth Rates", adj = 0)
grid(col = "gray90", lty = 1)
lines(t2, dEW(t2,   fit2$ests[2,i1], fit2$ests[2,i2], fit2$ests[2,i3]), col = c2, lty = l2, lwd = w2)
lines(t2, dMOW(t2,  fit2$ests[3,i1], fit2$ests[3,i2], fit2$ests[3,i3]), col = c3, lty = l3, lwd = w3)
lines(t2, dAPW(t2,  fit2$ests[4,i1], fit2$ests[4,i2], fit2$ests[4,i3]), col = c4, lty = l4, lwd = w4)
lines(t2, d3PW(t2,  fit2$ests[5,i1], fit2$ests[5,i2], fit2$ests[5,i3]), col = c5, lty = l5, lwd = w5)
lines(t2, dPSWD(t2, fit2$ests[1,i1], fit2$ests[1,i2], fit2$ests[1,i3]), col = c1, lty = l1, lwd = w1)
par(mar = c(0.1, 0.5, 0.1, 0.5))
plot.new()
legend("center", legend = c("PSWD", "EW", "MOW", "APW", "3PW"),
       col = c(c1, c2, c3, c4, c5), lty = c(l1, l2, l3, l4, l5),
       lwd = c(w1, w2, w3, w4, w5), bty = "n", cex = 0.98, ncol = 5)
dev.off()
cat("Saved Figure8.pdf")


#  Table 7

  
  eval_all_methods <- function(x, label) {
    methods <- c("ML", "MPS", "OLS", "WLS", "LAW")
    i1 <- 1; i2 <- 2; i3 <- 3
    do.call(rbind, lapply(methods, function(m) {
      p <- as.numeric(fit_PSWD(x, method = m))
      g <- calc_all_gof(x, pPSWD, p[i1], p[i2], p[i3])
      data.frame(Dataset = label, Method = m,
                 a = round(p[i1],4), b = round(p[i2],4), tau = round(p[i3],4),
                 KS = round(g["KS"],4), AD = round(g["AD"],4),
                 CvM = round(g["CvM"],4), U2 = round(g["U2"],4))
    }))
  }
table7_estimators <- rbind(
  eval_all_methods(ftse_ret,   "Dataset I"),
  eval_all_methods(air_growth, "Dataset II")
)
write.xlsx(table7_estimators, file.path(out_dir, "Table7.xlsx"), overwrite = TRUE)
print(table7_estimators, row.names = FALSE)
