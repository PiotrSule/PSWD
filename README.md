# `pswd`: Partly Shifted Weibull Distribution in R

[![License: MIT](https://shields.io)](https://opensource.org)
[![R](https://shields.io)](https://r-project.org)

An R package providing probability functions, reliability measures, moments, Shannon entropy, hybrid pseudorandom number generation, and parameter estimation routines for the three-parameter **Partly Shifted Weibull Distribution ($\text{PSWD}(a, b, \tau)$)**.

---
  
  ## Overview
  
  Unlike the classical three-parameter shifted Weibull distribution—which truncates the left tail at $\tau$ and restricts the support to $(\tau, \infty)$—the **PSWD** applies the threshold shift $\tau \ge 0$ selectively to the exponential damping term while preserving the full non-negative support $[0, \infty)$:
  
  $$f(t; a, b, \tau) = \frac{1}{C(\delta, b)} \frac{b}{a} \left(\frac{t}{a}\right)^{b - 1} \exp\left[ - \mathbb{I}(t > \tau) \left(\frac{t - \tau}{a}\right)^b \right], \quad t \ge 0,$$
    
    where:
    - $a > 0$ is the scale parameter,
  - $b > 0$ is the shape parameter,
  - $\tau \ge 0$ is the internal threshold parameter ($\delta = \tau / a$),
  - $C(\delta, b)$ is the exact normalizing constant expressed via incomplete gamma functions.
  
  ---
    
    ## Installation
    
    You can install the development version of `pswd` directly from GitHub using `remotes` (or `devtools`):
    
    ```r
  # install.packages("remotes")
  remotes::install_github("PiortSule/PSWD")
  ```
  
  ---
    
    ## Implemented Functions
    
    | Function | Description |
    | :--- | :--- |
    | `calc_C(a, b, tau)` | Normalizing constant $C(\delta, b)$ (exact closed-form for integer $b$, quadrature for real $b$) |
    | `dPSWD(t, a, b, tau)` | Probability density function (PDF) |
    | `pPSWD(t, a, b, tau)` | Cumulative distribution function (CDF) |
    | `relPSWD(t, a, b, tau)` | Reliability function (RF) |
    | `hPSWD(t, a, b, tau)` | Hazard rate function (HRF) |
    | `haPSWD(t, a, b, tau)` | Hazard rate average function (HRAF) |
    | `rhPSWD(t, a, b, tau)` | Reversed hazard rate function (RHRF) |
    | `qPSWD(p, a, b, tau)` | Hybrid quantile function |
    | `rPSWD(n, a, b, tau)` | Hybrid pseudorandom number generator (PRNG) |
    | `mPSWD(r, a, b, tau)` | $r$-th raw moment $\mathbb{E}[T^r]$ |
    | `shannon_PSWD(a, b, tau)` | Shannon entropy $\mathcal{H}(T)$ |
    | `fit_PSWD(data, method)` | Parameter estimation via `ML`, `MPS`, `OLS`, `WLS`, or `LAW` |
    
    ---
    
    ## Quick Example: Financial Volatility Modeling (FTSE 100)
    
    The example below reproduces the estimation on daily realized volatility (absolute percentage log-returns) of the UK FTSE 100 index from the base R `EuStockMarkets` dataset:
    
    ```r
  library(pswd)
  
  # Prepare daily absolute percentage log-returns of FTSE 100 (n = 146)
  ftse_ret <- abs(diff(log(as.numeric(datasets::EuStockMarkets[1:151, "FTSE"])))) * 100
  ftse_ret <- ftse_ret[ftse_ret > 0]
  
  # Fit PSWD via Maximum Likelihood (ML)
  est_ml <- fit_PSWD(ftse_ret, method = "ML")
  print(round(est_ml, 4))
  #      a      b    tau 
  # 0.3549 0.9133 0.8039
  
  # Compare all five estimation methods (ML, MPS, OLS, WLS, LAW)
  methods <- c("ML", "MPS", "OLS", "WLS", "LAW")
  all_ests <- t(sapply(methods, function(m) fit_PSWD(ftse_ret, method = m)))
  print(round(all_ests, 4))
  
  # Compute theoretical mean and Shannon entropy at ML estimates
  cat("Theoretical Mean E[T]:", round(mPSWD(1, est_ml["a"], est_ml["b"], est_ml["tau"]), 4), "\n")
  cat("Shannon Entropy H(T):", round(shannon_PSWD(est_ml["a"], est_ml["b"], est_ml["tau"]), 4), "\n")
  ```
  
  ---
    
    ## Citation
    
    If you use this package in your research, please cite:
    
    > Sulewski, P. (2026). *Partly Shifted Weibull Distribution*. (Manuscript / Zenodo DOI).
  