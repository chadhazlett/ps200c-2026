# sim_experiments.R
# Running simulation for PS200C exp_part2 lecture.
#
# GOTV experiment: Y = voted (0/1), individuals nested in households nested
# in precincts. Past turnout dominates Y(0); age modest; partisan strength
# near-null. Constant ATE on the probability scale (~ +5 pp).
#
# Produces:
#   figures/sim_simple.pdf      simple randomization, diff-in-means
#   figures/sim_adjusted.pdf    + lm(Y ~ T + past_turnout + age + partisan)
#   figures/sim_blocked.pdf     blocked on past_turnout quartiles
#   figures/sim_clustered.pdf   precinct-level (cluster-randomized) assignment
#   figures/sim_combined.pdf    all four overlaid
#   figures/balance_table.tex   one realized sample's Table 1
#   figures/balance_adjusted.tex same sample, residualized on T

set.seed(42)
suppressPackageStartupMessages(library(sandwich))

# ---- Setup ----------------------------------------------------------------
out_dir <- path.expand("~/teaching/ps200c-2026/slides/03-experiments/figures")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- DGP ------------------------------------------------------------------
N_precincts   <- 100
hh_per_prec   <- 10
ppl_per_hh    <- 4
N             <- N_precincts * hh_per_prec * ppl_per_hh   # 4000

sigma_prec    <- 1.20    # precinct random effect on logit scale (boosted)
sigma_hh      <- 0.70    # household random effect (bigger -> within-hh dep.)

beta_pt       <- 3.5     # past turnout (dominant; tuned for visible R^2)
beta_age      <- 0.4     # modest
beta_part     <- 0.1     # near-null
intercept     <- -0.3

tau_prob      <- 0.05    # true ATE on probability scale (mean)
sigma_tau     <- 0.40    # SD of precinct-level heterogeneity in tau
                         # for the het-tx scenario only

R             <- 2000    # replications

# Build a fixed population frame; we re-randomize T and re-draw Y each rep.
prec_id <- rep(1:N_precincts, each = hh_per_prec * ppl_per_hh)
hh_id   <- rep(1:(N_precincts * hh_per_prec), each = ppl_per_hh)

past_turnout <- rnorm(N)
age          <- rnorm(N)
partisan     <- rnorm(N)
u_prec       <- rnorm(N_precincts, sd = sigma_prec)[prec_id]
u_hh         <- rnorm(max(hh_id),  sd = sigma_hh)[hh_id]

linpred0 <- intercept + beta_pt*past_turnout + beta_age*age +
            beta_part*partisan + u_prec + u_hh
p0 <- plogis(linpred0)
p1 <- pmin(pmax(p0 + tau_prob, 0), 1)

draw_Y <- function(T_vec) {
  p <- ifelse(T_vec == 1, p1, p0)
  rbinom(N, 1, p)
}

# Het-tx version: each precinct has its own tau, drawn once and fixed
tau_by_prec <- rnorm(N_precincts, mean = tau_prob, sd = sigma_tau)
p1_het <- pmin(pmax(p0 + tau_by_prec[prec_id], 0), 1)
draw_Y_het <- function(T_vec) {
  p <- ifelse(T_vec == 1, p1_het, p0)
  rbinom(N, 1, p)
}

# Block id: quartiles of past_turnout
block_id <- as.integer(cut(past_turnout,
                           breaks = quantile(past_turnout, 0:4/4),
                           include.lowest = TRUE))

# ---- Estimators -----------------------------------------------------------
hc2_se <- function(fit, coef_name) {
  V <- tryCatch(vcovHC(fit, type = "HC2"), error = function(e) NULL)
  if (is.null(V)) return(NA_real_)
  sqrt(V[coef_name, coef_name])
}

cr_se <- function(fit, coef_name, cluster) {
  V <- tryCatch(vcovCL(fit, cluster = cluster, type = "HC1"),
                error = function(e) NULL)
  if (is.null(V)) return(NA_real_)
  sqrt(V[coef_name, coef_name])
}

# Simple DIM via lm(Y ~ D), with HC2 SE and CR-at-precinct SE
est_dim <- function(Y, T) {
  fit <- lm(Y ~ T)
  c(tau    = unname(coef(fit)["T"]),
    se     = hc2_se(fit, "T"),
    se_cr  = cr_se(fit, "T", prec_id))
}

# Lin (2013): demeaned covariates interacted with T, HC2 SE
est_lin <- function(Y, T) {
  Xc <- cbind(past_turnout, age, partisan)
  Xc <- sweep(Xc, 2, colMeans(Xc), "-")
  colnames(Xc) <- c("pt_c", "age_c", "part_c")
  df <- data.frame(Y = Y, T = T, Xc)
  fit <- lm(Y ~ T + pt_c + age_c + part_c +
                T:pt_c + T:age_c + T:part_c, data = df)
  c(tau = unname(coef(fit)["T"]),
    se  = hc2_se(fit, "T"))
}

est_blocked <- function(Y, T) {
  # block fixed effects via lm; HC2 SE on T
  fit <- lm(Y ~ T + factor(block_id))
  c(tau = unname(coef(fit)["T"]),
    se  = hc2_se(fit, "T"))
}

# Assignment mechanisms
assign_simple <- function() rbinom(N, 1, 0.5)

assign_blocked <- function() {
  T <- integer(N)
  for (b in unique(block_id)) {
    idx <- which(block_id == b)
    T[idx] <- sample(rep(c(0,1), length.out = length(idx)))
  }
  T
}

assign_cluster <- function() {
  # randomize precincts to T/C, all members get the precinct's assignment
  prec_T <- rbinom(N_precincts, 1, 0.5)
  prec_T[prec_id]
}

# ---- Run simulation -------------------------------------------------------
tau_simple    <- numeric(R); se_simple    <- numeric(R); se_simple_cr <- numeric(R)
tau_adjusted  <- numeric(R); se_adjusted  <- numeric(R)
tau_blocked   <- numeric(R); se_blocked   <- numeric(R)
tau_clustered <- numeric(R); se_clustered <- numeric(R); se_clustered_cr <- numeric(R)
tau_het       <- numeric(R); se_het_hc2   <- numeric(R); se_het_cr <- numeric(R)

cat("Running", R, "replications...\n")
for (r in seq_len(R)) {
  T1 <- assign_simple()
  Y1 <- draw_Y(T1)
  e1 <- est_dim(Y1, T1)
  tau_simple[r]    <- e1["tau"]
  se_simple[r]     <- e1["se"]
  se_simple_cr[r]  <- e1["se_cr"]
  e2 <- est_lin(Y1, T1); tau_adjusted[r] <- e2["tau"]; se_adjusted[r] <- e2["se"]

  T2 <- assign_blocked()
  Y2 <- draw_Y(T2)
  e3 <- est_blocked(Y2, T2); tau_blocked[r]  <- e3["tau"]; se_blocked[r]  <- e3["se"]

  T3 <- assign_cluster()
  Y3 <- draw_Y(T3)
  e4 <- est_dim(Y3, T3)
  tau_clustered[r]    <- e4["tau"]
  se_clustered[r]     <- e4["se"]
  se_clustered_cr[r]  <- e4["se_cr"]

  # Het-tx scenario: individual assignment, but tau varies by precinct
  T4 <- assign_simple()
  Y4 <- draw_Y_het(T4)
  fit4 <- lm(Y4 ~ T4)
  tau_het[r]     <- unname(coef(fit4)["T4"])
  se_het_hc2[r]  <- hc2_se(fit4, "T4")
  se_het_cr[r]   <- cr_se(fit4, "T4", prec_id)
}
cat("Done.\n")

summarize <- function(x, se, name) {
  cat(sprintf("%-12s mean=%+.4f  sd(actual)=%.4f  mean(SE_HC2)=%.4f  rmse=%.4f\n",
              name, mean(x), sd(x), mean(se, na.rm=TRUE),
              sqrt(mean((x - tau_prob)^2))))
}
summarize(tau_simple,    se_simple,    "simple")
summarize(tau_adjusted,  se_adjusted,  "adjusted")
summarize(tau_blocked,   se_blocked,   "blocked")
summarize(tau_clustered, se_clustered, "clustered")
cat(sprintf("  + clustered, CR-SE  mean(SE_CR)=%.4f  (clustered at precinct)\n",
            mean(se_clustered_cr)))

# Diagnostic: R^2 of Y(0) on past_turnout (population, no noise from D)
Y0_one <- rbinom(N, 1, p0)
r2_pt <- summary(lm(Y0_one ~ past_turnout))$r.squared
r2_all <- summary(lm(Y0_one ~ past_turnout + age + partisan))$r.squared
cat(sprintf("\nR^2 of Y(0) on past_turnout alone: %.3f\n", r2_pt))
cat(sprintf("R^2 of Y(0) on all 3 covariates : %.3f\n", r2_all))

# Diagnostic: HC2 vs CR-at-precinct under individual assignment
cat(sprintf("\nIndividual assignment, baselines correlated within precincts:\n"))
cat(sprintf("  SD(actual)            = %.4f\n", sd(tau_simple)))
cat(sprintf("  Mean HC2 SE           = %.4f\n", mean(se_simple)))
cat(sprintf("  Mean cluster-robust SE = %.4f  (clustered at precinct)\n",
            mean(se_simple_cr)))

cat(sprintf("\nIndividual assignment, HETEROGENEOUS tau by precinct:\n"))
cat(sprintf("  SD(actual)            = %.4f\n", sd(tau_het)))
cat(sprintf("  Mean HC2 SE           = %.4f\n", mean(se_het_hc2)))
cat(sprintf("  Mean cluster-robust SE = %.4f  (clustered at precinct)\n",
            mean(se_het_cr)))

# Save summary table for use in slides if desired
sink(file.path(out_dir, "sim_summary.txt"))
cat("Estimator       Mean       SD(actual)  Mean(HC2 SE)  RMSE\n")
for (nm in c("simple","adjusted","blocked","clustered")) {
  x  <- get(paste0("tau_", nm))
  se <- get(paste0("se_",  nm))
  cat(sprintf("%-14s %+.4f    %.4f      %.4f        %.4f\n",
              nm, mean(x), sd(x), mean(se, na.rm=TRUE),
              sqrt(mean((x - tau_prob)^2))))
}
sink()

# ---- Plot helpers ---------------------------------------------------------
xlim_all <- range(c(tau_simple, tau_adjusted, tau_blocked, tau_clustered))
brks <- seq(xlim_all[1], xlim_all[2], length.out = 60)

# Common y-axis: tallest density across all four estimators (so successive
# panels are visually comparable).
ylim_max <- max(sapply(list(tau_simple, tau_adjusted, tau_blocked, tau_clustered),
                       function(x) max(density(x)$y))) * 1.05

col_simple_bar <- adjustcolor("grey70",     alpha.f = 0.7)
col_simple     <- "grey40"
col_adjusted   <- "darkorange"
col_blocked    <- "seagreen"
col_clustered  <- "firebrick"

# Each panel: bars for the simple-DIM baseline, then density curves added on
# top for each subsequent estimator. So each new panel only adds one curve.
draw_panel <- function(layers, fname, main) {
  pdf(file.path(out_dir, fname), width = 5.5, height = 3.4)
  par(mar = c(4, 4, 3, 1))
  hist(tau_simple, breaks = brks, col = col_simple_bar, border = "white",
       xlim = xlim_all, ylim = c(0, ylim_max), freq = FALSE,
       main = main, xlab = expression(hat(tau)))
  abline(v = tau_prob, col = "black", lwd = 2, lty = 2)
  # Always draw the simple-DIM density as the baseline reference line
  d0 <- density(tau_simple)
  lines(d0, col = col_simple, lwd = 2)
  legend_lab <- "Simple DIM"
  legend_col <- col_simple
  for (lyr in layers) {
    d <- density(lyr$x)
    lines(d, col = lyr$col, lwd = 2.5)
    legend_lab <- c(legend_lab, lyr$label)
    legend_col <- c(legend_col, lyr$col)
  }
  legend("topright", legend = legend_lab, col = legend_col,
         lwd = 2.5, bty = "n", cex = 0.85)
  dev.off()
}

# Panel 1: simple DIM only (bars + line)
draw_panel(list(),
           "sim_simple.pdf",
           "Simple randomization, diff-in-means")

# Panel 2: + Lin-adjusted
draw_panel(list(list(x = tau_adjusted, col = col_adjusted, label = "Lin-adjusted")),
           "sim_adjusted.pdf",
           "Add regression adjustment (Lin)")

# Panel 3: + blocked
draw_panel(list(list(x = tau_adjusted, col = col_adjusted, label = "Lin-adjusted"),
                list(x = tau_blocked,  col = col_blocked,  label = "Blocked")),
           "sim_blocked.pdf",
           "Add blocking on past-turnout quartiles")

# Panel 4: + cluster-assigned
draw_panel(list(list(x = tau_adjusted,  col = col_adjusted,  label = "Lin-adjusted"),
                list(x = tau_blocked,   col = col_blocked,   label = "Blocked"),
                list(x = tau_clustered, col = col_clustered, label = "Cluster-randomized")),
           "sim_clustered.pdf",
           "Cluster-randomize at precinct level")

# sim_combined is now identical to sim_clustered (the final cumulative plot)
file.copy(file.path(out_dir, "sim_clustered.pdf"),
          file.path(out_dir, "sim_combined.pdf"),
          overwrite = TRUE)

# ---- Balance tables (one realized sample) ---------------------------------
T_one <- assign_simple()
Y_one <- draw_Y(T_one)

bal_raw <- data.frame(
  variable = c("past\\_turnout", "age", "partisan"),
  treated  = c(mean(past_turnout[T_one==1]),
               mean(age[T_one==1]),
               mean(partisan[T_one==1])),
  control  = c(mean(past_turnout[T_one==0]),
               mean(age[T_one==0]),
               mean(partisan[T_one==0]))
)
bal_raw$diff <- bal_raw$treated - bal_raw$control

write_bal_tex <- function(df, fname, caption) {
  fp <- file.path(out_dir, fname)
  con <- file(fp, "w")
  writeLines(c(
    "\\begin{tabular}{lrrr}",
    "\\toprule",
    "Variable & Treated & Control & Difference \\\\",
    "\\midrule",
    sprintf("%s & %+.3f & %+.3f & %+.3f \\\\",
            df$variable, df$treated, df$control, df$diff),
    "\\bottomrule",
    "\\end{tabular}"
  ), con)
  close(con)
}
write_bal_tex(bal_raw, "balance_table.tex", "Balance, raw")

# After residualizing T on covariates, the "effective" T has zero correlation
# with the covariates -- this is what regression adjustment does.
res_T <- residuals(lm(T_one ~ past_turnout + age + partisan))
bal_adj <- data.frame(
  variable = c("past\\_turnout", "age", "partisan"),
  treated  = c(weighted.mean(past_turnout, pmax(res_T, 0)),
               weighted.mean(age,          pmax(res_T, 0)),
               weighted.mean(partisan,     pmax(res_T, 0))),
  control  = c(weighted.mean(past_turnout, pmax(-res_T, 0)),
               weighted.mean(age,          pmax(-res_T, 0)),
               weighted.mean(partisan,     pmax(-res_T, 0)))
)
bal_adj$diff <- bal_adj$treated - bal_adj$control
write_bal_tex(bal_adj, "balance_adjusted.tex", "Balance after adjustment")

cat("\nWrote outputs to:", out_dir, "\n")
