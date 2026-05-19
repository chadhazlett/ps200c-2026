## lfm_demo.R
## Stylized linear-factor-model illustration for the synth lecture.
## Generates four PDFs in figures/:
##   factors.pdf         -- 3 smooth time-varying factors lambda_t
##   loadings.pdf        -- factor loadings u_i by state (bars)
##   trajectories.pdf    -- assembled Y_it(0) trajectories (CA highlighted)
##   sleeping_giant.pdf  -- same plus 4th factor silent pre-1989, active post

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
})

set.seed(1989)

## Resolve figures/ relative to this script. Run from 10-synth/ or 10-synth/sim/.
script_path <- tryCatch({
  args <- commandArgs(trailingOnly = FALSE)
  fa <- args[grep("^--file=", args)]
  if (length(fa)) sub("^--file=", "", fa[1]) else sys.frame(1)$ofile
}, error = function(e) NULL)
if (is.null(script_path) || !nzchar(script_path)) script_path <- "sim/lfm_demo.R"
here <- normalizePath(dirname(script_path), mustWork = FALSE)
figdir <- normalizePath(file.path(here, "..", "figures"), mustWork = FALSE)
dir.create(figdir, showWarnings = FALSE, recursive = TRUE)

## ---- Time grid (annual, 1970-2000; treatment year 1989) ----
years <- 1970:2000
T_pre <- sum(years <= 1988)
treat_year <- 1989

## ---- Three latent factors lambda_t (smooth, scaled to similar amplitudes) ----
##  1: health consciousness  -- gentle upward trend
##  2: anti-tobacco sentiment -- low-then-rising
##  3: willingness to use tax structure for social aims -- ramp from mid-80s
t_idx <- seq_along(years)
lambda1 <- scale(0.05 * (years - 1970) + 0.4 * sin((years - 1970) / 6))[, 1]
lambda2 <- scale(plogis((years - 1985) / 4))[, 1]
lambda3 <- scale(pmax(0, years - 1983) ^ 0.7)[, 1]
Lambda <- cbind(health = lambda1, antitob = lambda2, taxpolicy = lambda3)

factor_df <- as.data.frame(Lambda) |>
  mutate(year = years) |>
  pivot_longer(-year, names_to = "factor", values_to = "lambda") |>
  mutate(factor = recode(factor,
                         health    = "Health consciousness",
                         antitob   = "Anti-tobacco sentiment",
                         taxpolicy = "Tax-as-policy-tool"))

p_factors <- ggplot(factor_df, aes(year, lambda, colour = factor)) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = treat_year - 0.5, linetype = "dashed", alpha = 0.4) +
  scale_colour_manual(values = c("Health consciousness" = "#1f78b4",
                                 "Anti-tobacco sentiment" = "#33a02c",
                                 "Tax-as-policy-tool" = "#e31a1c")) +
  labs(x = NULL, y = expression(lambda[t]^(k)),
       colour = NULL,
       title = "Three latent time-varying factors") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

ggsave(file.path(figdir, "factors.pdf"), p_factors,
       width = 7.0, height = 4.0)

## ---- Hand-set loadings (substantively right signs) with real-data intercepts ----
## Why hand-set: real cigsale data is mostly common-trend + noise, so per-state
## OLS on our hand-designed factors gives loadings that are numerically unstable
## and substantively perverse (e.g., NC appearing more "anti-tobacco" than CA).
## Instead: hand-set u_i so signs match the story (anti-tobacco states get
## negative loadings on suppressive forces; tobacco states get positive); pull
## state intercepts alpha_i from real pre-period means so the trajectory
## *levels* are in the right ballpark.
suppressPackageStartupMessages(library(tidysynth))
data(smoking, package = "tidysynth")

## 9-state set: CA + 8 donors. The three "good" donors (Utah, Colorado, Nevada)
## are picked to match Abadie--Diamond--Hainmueller (2010): the actual top
## three in their synthetic California. Tobacco/extreme states pull the
## uniform-weight baseline away from CA.
states <- c("California", "Utah", "Colorado", "Nevada",
            "Kentucky", "North Carolina",
            "Mississippi", "Alabama", "Wyoming")
focal_states <- states

## Real pre-1989 mean cigsale per state (anchors trajectory levels)
real_pre <- smoking |>
  filter(state %in% states, year %in% 1970:1988) |>
  group_by(state) |> summarise(mean_cigsale = mean(cigsale), .groups = "drop")
alphas <- setNames(real_pre$mean_cigsale, real_pre$state)[states]

## Hand-set loadings: CA inside the donor hull on visible factors;
## the "extreme" donors have strongly positive loadings.
U <- rbind(
  California       = c(-1.2, -1.4, -1.3),  # high responder
  Utah             = c(-1.5, -1.3, -1.1),  # exceeds CA on health
  Colorado         = c(-1.0, -1.6, -1.5),  # exceeds CA on antitob, tax
  Nevada           = c(-0.4, -0.3, -0.2),  # near zero
  Kentucky         = c( 0.6,  0.5,  0.4),  # tobacco
  `North Carolina` = c( 0.5,  0.6,  0.4),  # tobacco
  Mississippi      = c( 0.9,  0.8,  0.6),  # more tobacco / Deep South
  Alabama          = c( 0.8,  0.7,  0.5),  # more tobacco / Deep South
  Wyoming          = c( 1.0,  0.6,  0.3)   # extreme low-responder
)
colnames(U) <- c("health", "antitob", "taxpolicy")

## Common downward trend (national decline) so tobacco states also decline.
trend_t <- (years - 1985) / 10            # centered, scaled
beta_trend <- -10                         # ~-1 pack/year national decline
s_signal  <- 3                            # state-specific factor scale (tuned for ballpark CA decline)

cat("\n--- State intercepts (real pre-period means) ---\n")
print(round(alphas, 1))
cat("\n--- Hand-set factor loadings u_i ---\n")
print(round(U, 2))

load_df <- as.data.frame(U) |>
  mutate(state = rownames(U)) |>
  pivot_longer(-state, names_to = "factor", values_to = "u") |>
  mutate(factor = recode(factor,
                         health    = "Health consc.",
                         antitob   = "Anti-tobacco",
                         taxpolicy = "Tax policy"),
         state  = factor(state, levels = states))

p_loadings <- ggplot(load_df, aes(state, u, fill = factor)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 0, colour = "grey50") +
  scale_fill_manual(values = c("Health consc." = "#1f78b4",
                               "Anti-tobacco" = "#33a02c",
                               "Tax policy" = "#e31a1c")) +
  labs(x = NULL, y = expression(u[i]^(k)), fill = NULL,
       title = "State-level factor loadings") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 25, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

ggsave(file.path(figdir, "loadings.pdf"), p_loadings,
       width = 7.0, height = 4.0)

## ---- Assemble Y_it(0) = alpha_i + beta * t + s * lambda_t' u_i + eps_it ----
## The common downward trend (beta * t) gives every state a national decline;
## state-specific factor loadings make anti-tobacco states decline faster and
## tobacco states decline slower.
##
## Pick sigma_eps so that the pre-period R^2 of the *factor signal*
## (s * Lambda u_i) against (factor signal + noise) hits a target value.
ypure_pre  <- s_signal * Lambda[years <= 1988, ] %*% t(U)   # T_pre x N
var_signal <- var(as.vector(ypure_pre))
target_R2  <- 0.50
sigma_eps  <- sqrt(var_signal * (1 - target_R2) / target_R2)
cat(sprintf("\nVar(factor signal) = %.2f; target R^2 = %.2f -> sigma_eps = %.2f\n",
            var_signal, target_R2, sigma_eps))

Y0 <- matrix(NA, nrow = length(years), ncol = length(states),
             dimnames = list(years, states))
for (i in seq_along(states)) {
  Y0[, i] <- alphas[i] + beta_trend * trend_t +
             s_signal * as.numeric(Lambda %*% U[i, ]) +
             rnorm(length(years), sd = sigma_eps)
}

traj_df <- as.data.frame(Y0) |>
  mutate(year = years) |>
  pivot_longer(-year, names_to = "state", values_to = "Y0") |>
  mutate(state = factor(state, levels = states),
         is_CA = state == "California")

p_traj <- ggplot(traj_df, aes(year, Y0, group = state, colour = is_CA)) +
  geom_line(aes(linewidth = is_CA)) +
  geom_vline(xintercept = treat_year - 0.5, linetype = "dashed", alpha = 0.4) +
  scale_colour_manual(values = c("FALSE" = "grey55", "TRUE" = "#e31a1c"),
                      guide = "none") +
  scale_linewidth_manual(values = c("FALSE" = 0.7, "TRUE" = 1.3),
                         guide = "none") +
  annotate("text", x = 1972, y = max(traj_df$Y0), hjust = 0,
           label = "CA (red); donors (grey)", size = 4.2) +
  labs(x = NULL, y = "per-capita cigarette sales (packs)",
       title = expression("Assembled "*Y[it](0)==alpha[i]+lambda[t]^T*u[i]+epsilon[it])) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(figdir, "trajectories.pdf"), p_traj,
       width = 7.0, height = 4.0)

## ---- Sleeping giant: 4th factor silent pre-1989, active post ----
##   u_CA^(4) = 1; donors drawn iid Uniform(-0.5, 0.5)
##   lambda_t^(4) = 0 for t <= 1988; constant magnitude post (in packs/capita units).
set.seed(4)
u4 <- c(California = 1)
donor_names <- setdiff(states, "California")
u4[donor_names] <- runif(length(donor_names), -0.5, 0.5)
u4 <- u4[states]  # reorder

## lambda4 ~ 10 so the implied bias is in interpretable packs/capita units.
lambda4 <- ifelse(years < treat_year, 0,
                  10 + 1 * (years - treat_year))

Y0_sg <- Y0 + outer(lambda4, u4)

traj_sg <- as.data.frame(Y0_sg) |>
  mutate(year = years) |>
  pivot_longer(-year, names_to = "state", values_to = "Y0") |>
  mutate(state = factor(state, levels = states),
         is_CA = state == "California")

p_sg <- ggplot(traj_sg, aes(year, Y0, group = state, colour = is_CA)) +
  geom_line(aes(linewidth = is_CA)) +
  geom_vline(xintercept = treat_year - 0.5, linetype = "dashed", alpha = 0.4) +
  scale_colour_manual(values = c("FALSE" = "grey55", "TRUE" = "#e31a1c"),
                      guide = "none") +
  scale_linewidth_manual(values = c("FALSE" = 0.7, "TRUE" = 1.3),
                         guide = "none") +
  annotate("text", x = 1990.5, y = max(traj_sg$Y0), hjust = 0,
           label = "Sleeping giant: silent pre-1989,\nimbalanced and active post",
           size = 3.6) +
  labs(x = NULL, y = "per-capita cigarette sales (packs)",
       title = "Same DGP plus a sleeping-giant factor (post-1989)") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(figdir, "sleeping_giant.pdf"), p_sg,
       width = 7.0, height = 4.0)

## ---- Combined factors-plus-sleeping-giant for the recoverability slide ----
## Normalize lambda4 for visual comparability with z-scored visible factors.
lambda4_display <- ifelse(years < treat_year, 0, 1)
factor_df_sg <- as.data.frame(cbind(Lambda, sleeping = lambda4_display)) |>
  mutate(year = years) |>
  pivot_longer(-year, names_to = "factor", values_to = "lambda") |>
  mutate(factor = recode(factor,
                         health    = "Health consciousness",
                         antitob   = "Anti-tobacco sentiment",
                         taxpolicy = "Tax-as-policy-tool",
                         sleeping  = "Sleeping giant"),
         factor = factor(factor,
                         levels = c("Health consciousness",
                                    "Anti-tobacco sentiment",
                                    "Tax-as-policy-tool",
                                    "Sleeping giant")))

p_factors_sg <- ggplot(factor_df_sg, aes(year, lambda, colour = factor)) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = treat_year - 0.5, linetype = "dashed", alpha = 0.4) +
  scale_colour_manual(values = c("Health consciousness" = "#1f78b4",
                                 "Anti-tobacco sentiment" = "#33a02c",
                                 "Tax-as-policy-tool" = "#e31a1c",
                                 "Sleeping giant" = "black")) +
  labs(x = NULL, y = expression(lambda[t]^(k)), colour = NULL,
       title = "Factors with a sleeping giant") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

ggsave(file.path(figdir, "factors_with_sg.pdf"), p_factors_sg,
       width = 7.0, height = 4.0)

## ============================================================
## Numerical check: does mean-balancing on Y_pre balance u_i?
## ============================================================
suppressPackageStartupMessages(library(quadprog))

## Abadie-style simplex QP: minimize ||X_co w - x_tr||^2 s.t. w >= 0, sum(w)=1
mean_balance_simplex <- function(X_co, x_tr) {
  n <- ncol(X_co)
  Dmat <- crossprod(X_co) + 1e-6 * diag(n)
  dvec <- as.vector(crossprod(X_co, x_tr))
  Amat <- cbind(rep(1, n), diag(n))
  bvec <- c(1, rep(0, n))
  solve.QP(Dmat, dvec, Amat, bvec, meq = 1)$solution
}

## Pre-period outcome matrix (T_pre x 5 donors), CA vs the 5 focal donors.
pre_idx <- years <= 1988
Y_pre   <- Y0[pre_idx, ]
x_CA    <- Y_pre[, "California"]
X_donor <- Y_pre[, setdiff(colnames(Y_pre), "California")]

w <- mean_balance_simplex(X_donor, x_CA)
names(w) <- colnames(X_donor)

cat("\nDonor weights from mean-balancing on Y_pre:\n")
print(round(w, 3))

## Uniform-weight baseline: every donor gets 1/N
n_donors <- ncol(X_donor)
w_uniform <- setNames(rep(1/n_donors, n_donors), colnames(X_donor))

## --- 3-factor scenario: imbalance on each u_i^(k) ---
u_CA      <- U["California", ]
U_donors  <- U[setdiff(rownames(U), "California"), , drop = FALSE]
u_donor_w        <- as.vector(t(U_donors) %*% w)
u_donor_uniform  <- as.vector(t(U_donors) %*% w_uniform)
imbalance_3        <- u_CA - u_donor_w
imbalance_uniform  <- u_CA - u_donor_uniform
names(imbalance_3)       <- c("Health consc.", "Anti-tobacco", "Tax policy")
names(imbalance_uniform) <- c("Health consc.", "Anti-tobacco", "Tax policy")

cat("\n--- Mean-balancing on Y_pre (3-factor LFM, 8 donors) ---\n")
cat("Imbalance on each u_i^(k):  (u_CA - sum_i w_i u_donor_i)\n")
print(round(imbalance_3, 3))
cat("\nImbalance under UNIFORM (1/N) weights for comparison:\n")
print(round(imbalance_uniform, 3))

## Implied bias on Y(0)_{T+1} under each weighting scheme:
##   bias = alpha_imbalance + s_signal * sum_k lambda_T+1^(k) * imbalance^(k)
alpha_imb_balanced <- sum(w         * alphas[setdiff(states, "California")]) - alphas["California"]
alpha_imb_uniform  <- sum(w_uniform * alphas[setdiff(states, "California")]) - alphas["California"]
bias_balanced_T1 <- alpha_imb_balanced + s_signal * sum(Lambda[years == treat_year, ] * imbalance_3)
bias_uniform_T1  <- alpha_imb_uniform  + s_signal * sum(Lambda[years == treat_year, ] * imbalance_uniform)
cat(sprintf("\nImplied bias on Y(0)_{T+1} (packs/capita):\n  uniform weights:    %.2f\n  mean-balanced:      %.2f\n",
            bias_uniform_T1, bias_balanced_T1))

## --- Sleeping-giant scenario: add 4th factor, same Y_pre, recompute imbalance ---
U_sg <- cbind(U, sleeping = u4[rownames(U)])
u_CA_sg      <- U_sg["California", ]
U_donors_sg  <- U_sg[setdiff(rownames(U_sg), "California"), , drop = FALSE]
u_donor_w_sg <- as.vector(t(U_donors_sg) %*% w)
imbalance_4 <- u_CA_sg - u_donor_w_sg
names(imbalance_4) <- c(names(imbalance_3), "Sleeping giant")

## --- Bias contributions at T+1 (the directly interpretable quantity) ---
##   Per-factor bias contribution = s_signal * lambda_T+1^(k) * imbalance_u^(k).
##   The common trend cancels (same coefficient for all states); alpha imbalance
##   reflects state-intercept imbalance after mean-balancing.
lambda_T1     <- Lambda[years == treat_year, ]
lambda4_T1    <- lambda4[years == treat_year]
lambda4_avg   <- mean(lambda4[years >= treat_year])

alpha_imbalance <- sum(w * alphas[setdiff(states, "California")]) - alphas["California"]
bias_visible_T1 <- s_signal * as.numeric(lambda_T1 * imbalance_3)
bias_sg_T1      <- lambda4_T1 * imbalance_4["Sleeping giant"]
bias_total_T1   <- alpha_imbalance + sum(bias_visible_T1) + bias_sg_T1
bias_total_avg  <- alpha_imbalance +
                   s_signal * sum(as.numeric(colMeans(Lambda[years >= treat_year, ]) * imbalance_3)) +
                   lambda4_avg * imbalance_4["Sleeping giant"]

cat("\n--- With sleeping giant (same weights; Y_pre unchanged) ---\n")
cat("Imbalance on each u_i^(k):\n"); print(round(imbalance_4, 3))
cat(sprintf("\nAlpha imbalance (state intercept): %.2f packs\n", alpha_imbalance))
cat(sprintf("\nBias contribution at T+1 (packs/capita), by factor:\n"))
print(round(c(bias_visible_T1, `Sleeping giant` = as.numeric(bias_sg_T1)), 2))
cat(sprintf("\nTotal bias at T+1 = %.2f packs/capita\n", bias_total_T1))
cat(sprintf("Avg bias over post = %.2f packs/capita\n", bias_total_avg))

## ---- Plot: per-factor bias contribution at T+1 (packs/capita), side-by-side ----
bias_visible_named <- setNames(bias_visible_T1, names(imbalance_3))
imb_df <- rbind(
  data.frame(scenario = "Only the 3 visible factors",
             factor = names(bias_visible_named),
             bias = as.numeric(bias_visible_named)),
  data.frame(scenario = "Same weights, sleeping giant added",
             factor = c(names(bias_visible_named), "Sleeping giant"),
             bias = c(as.numeric(bias_visible_named), as.numeric(bias_sg_T1)))
)
imb_df$scenario <- factor(imb_df$scenario,
                          levels = c("Only the 3 visible factors",
                                     "Same weights, sleeping giant added"))
imb_df$factor <- factor(imb_df$factor,
                        levels = c("Health consc.", "Anti-tobacco",
                                   "Tax policy", "Sleeping giant"))

p_balance <- ggplot(subset(imb_df, scenario == "Same weights, sleeping giant added"),
                    aes(factor, bias, fill = factor == "Sleeping giant")) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0, colour = "grey50") +
  scale_fill_manual(values = c("FALSE" = "#1f78b4", "TRUE" = "black"),
                    guide = "none") +
  labs(x = NULL,
       y = expression(lambda[T+1]^(k) %.% (u[CA]^(k) - sum(w[i] * u[i]^(k)))~"  (packs/cap.)"),
       title = expression("Contribution to imbalance on " * Y(0)[t+1] * " after weighting")) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 25, hjust = 1))

ggsave(file.path(figdir, "balance_check.pdf"), p_balance,
       width = 6.5, height = 4.0)

## Single-panel version for the early "sanity check" frame
p_balance_3 <- ggplot(subset(imb_df, scenario == "Only the 3 visible factors"),
                      aes(factor, bias)) +
  geom_col(width = 0.5, fill = "#1f78b4") +
  geom_hline(yintercept = 0, colour = "grey50") +
  labs(x = NULL,
       y = "bias contribution at T+1 (packs/capita)",
       title = "Mean-balancing on Y_pre nearly zeros the visible-factor bias") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

ggsave(file.path(figdir, "balance_check_3.pdf"), p_balance_3,
       width = 6.5, height = 3.6)

## ---- Weights bar chart (which donors got picked) ----
w_df <- data.frame(state = names(w), weight = as.numeric(w))
w_df$state <- factor(w_df$state, levels = w_df$state[order(-w_df$weight)])
w_df$gotweight <- w_df$weight > 1e-3

p_weights <- ggplot(w_df, aes(state, weight, fill = gotweight)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = ifelse(gotweight, sprintf("%.2f", weight), "")),
            vjust = -0.3, size = 3.6) +
  scale_fill_manual(values = c(`FALSE` = "grey70", `TRUE` = "#1f78b4"),
                    guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.18))) +
  labs(x = NULL, y = "donor weight",
       title = "Simplex weights from mean-balancing on Y_pre") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(figdir, "weights.pdf"), p_weights,
       width = 7.0, height = 3.6)

## ---- Imbalance comparison: mean-balance vs uniform weights ----
cmp_df <- rbind(
  data.frame(method = "Mean-balance on Y_pre",
             factor = names(imbalance_3),
             imbalance = as.numeric(imbalance_3)),
  data.frame(method = "Uniform donor weights (1/N)",
             factor = names(imbalance_uniform),
             imbalance = as.numeric(imbalance_uniform))
)
cmp_df$method <- factor(cmp_df$method,
                        levels = c("Uniform donor weights (1/N)",
                                   "Mean-balance on Y_pre"))
cmp_df$factor <- factor(cmp_df$factor,
                        levels = c("Health consc.", "Anti-tobacco", "Tax policy"))

p_compare <- ggplot(cmp_df, aes(factor, imbalance, fill = method)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_hline(yintercept = 0, colour = "grey50") +
  scale_fill_manual(values = c("Uniform donor weights (1/N)" = "grey60",
                               "Mean-balance on Y_pre" = "#1f78b4")) +
  labs(x = NULL,
       y = expression(u[CA]^(k) - sum(w[i] * u[i]^(k))),
       fill = NULL,
       title = "u-imbalance: mean-balanced vs uniform donor weights") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

ggsave(file.path(figdir, "balance_compare.pdf"), p_compare,
       width = 7.5, height = 3.8)

cat("\nWeights labeled:\n"); print(round(w[w > 1e-3], 3))

## ============================================================
## T_pre = 2 scenario: rank condition fails (3 factors, 2 pre-periods)
## ============================================================
## Use only the last two pre-periods, 1985 and 1986, then mean-balance on
## those alone. Row span of Lambda_{1985,1986} has dimension at most 2,
## while K = 3, so one direction of u remains unidentified.
short_years   <- c(1985, 1986)
Y_pre_short   <- Y0[match(short_years, years), , drop = FALSE]
x_CA_short    <- Y_pre_short[, "California"]
X_donor_short <- Y_pre_short[, setdiff(colnames(Y_pre_short), "California")]

w_short <- mean_balance_simplex(X_donor_short, x_CA_short)
names(w_short) <- colnames(X_donor_short)

u_donor_w_short <- as.vector(t(U_donors) %*% w_short)
imbalance_short <- u_CA - u_donor_w_short
names(imbalance_short) <- c("Health consc.", "Anti-tobacco", "Tax policy")

cat("\n--- T_pre = 2 (just 1985, 1986): rank condition fails (3 factors, 2 rows) ---\n")
cat("Imbalance on each u_i^(k):\n"); print(round(imbalance_short, 3))

## Compare imbalance: full pre-period (T_pre = 17), T_pre = 2, and no weighting
cmp_T_df <- rbind(
  data.frame(scenario = "Full pre-period (T_pre = 17)",
             factor = names(imbalance_3), imbalance = as.numeric(imbalance_3)),
  data.frame(scenario = "Short pre-period (T_pre = 2)",
             factor = names(imbalance_short), imbalance = as.numeric(imbalance_short)),
  data.frame(scenario = "No weighting (uniform 1/N)",
             factor = names(imbalance_uniform), imbalance = as.numeric(imbalance_uniform))
)
cmp_T_df$scenario <- factor(cmp_T_df$scenario,
                            levels = c("Full pre-period (T_pre = 17)",
                                       "Short pre-period (T_pre = 2)",
                                       "No weighting (uniform 1/N)"))
cmp_T_df$factor <- factor(cmp_T_df$factor,
                          levels = c("Health consc.", "Anti-tobacco", "Tax policy"))

p_tpre <- ggplot(cmp_T_df, aes(factor, imbalance, fill = scenario)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_hline(yintercept = 0, colour = "grey50") +
  scale_fill_manual(values = c("Full pre-period (T_pre = 17)" = "#1f78b4",
                               "Short pre-period (T_pre = 2)" = "#e31a1c",
                               "No weighting (uniform 1/N)"   = "grey55")) +
  labs(x = NULL,
       y = expression(u[CA]^(k) - sum(w[i] * u[i]^(k))),
       fill = NULL,
       title = "Too few pre-periods: rank condition fails (3 factors, 2 pre-periods)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

ggsave(file.path(figdir, "balance_short_T.pdf"), p_tpre,
       width = 7.5, height = 4.0)

## ============================================================
## EIV demo: N=49 synthetic donor pool for stable ||w||^2
## ============================================================
## Many donors -> ||w||^2 small and slowly-varying with T_pre. Donor u's
## drawn IID Gaussian and intercepts drawn to match real-state spread;
## CA keeps its real loadings/intercept. CA's u sits in the donor hull
## with high probability, so the noiseless floor is tiny and what we see
## is essentially the EIV / noise-averaging contribution.
N_eiv <- 49
sd_u <- 1.5    # wider than the real-state spread so the donor hull
               # comfortably contains u_CA = (-1.2, -1.4, -1.3)

## Seed search: find a draw where CA is well inside the donor hull in
## every dimension, i.e., the noiseless QP achieves a small u-imbalance
## in all three directions.
best_seed <- NA_integer_; best_score <- Inf
for (cand_seed in 1:300) {
  set.seed(cand_seed)
  U_try <- matrix(rnorm(N_eiv * 3, mean = 0, sd = sd_u), nrow = N_eiv)
  a_try <- rnorm(N_eiv, mean = mean(alphas), sd = sd(alphas))
  donor_sig_try <- matrix(NA_real_, nrow = T_pre, ncol = N_eiv)
  for (i in seq_len(N_eiv)) {
    donor_sig_try[, i] <- a_try[i] + beta_trend * trend_t[years <= 1988] +
                          s_signal * as.numeric(Lambda[years <= 1988, ] %*% U_try[i, ])
  }
  w_try <- tryCatch(mean_balance_simplex(donor_sig_try, CA_signal_pre_placeholder <- (alphas["California"] + beta_trend * trend_t + s_signal * as.numeric(Lambda %*% u_CA))[years <= 1988]),
                    error = function(e) rep(1/N_eiv, N_eiv))
  imb_try <- u_CA - as.vector(t(U_try) %*% w_try)
  score <- max(abs(imb_try))   # worst-dim floor
  if (score < best_score) { best_score <- score; best_seed <- cand_seed }
}
cat(sprintf("\n[EIV pool seed-search] best seed = %d, worst-dim floor = %.3f\n",
            best_seed, best_score))

set.seed(best_seed)
U_eiv <- matrix(rnorm(N_eiv * 3, mean = 0, sd = sd_u),
                nrow = N_eiv, ncol = 3,
                dimnames = list(paste0("d", sprintf("%02d", 1:N_eiv)),
                                c("health", "antitob", "taxpolicy")))
alpha_eiv <- rnorm(N_eiv, mean = mean(alphas), sd = sd(alphas))
names(alpha_eiv) <- rownames(U_eiv)

cat("\n--- 49-donor synthetic EIV pool (seed ", best_seed, ", sd_u = ", sd_u, ") ---\n", sep="")
cat(sprintf("u_CA = (%.2f, %.2f, %.2f)\n", u_CA[1], u_CA[2], u_CA[3]))
cat(sprintf("Donor u range:\n  health  [%.2f, %.2f]\n  antitob [%.2f, %.2f]\n  tax     [%.2f, %.2f]\n",
            min(U_eiv[,1]), max(U_eiv[,1]),
            min(U_eiv[,2]), max(U_eiv[,2]),
            min(U_eiv[,3]), max(U_eiv[,3])))

## Noiseless signals (deterministic): used both for resampling and the floor.
CA_signal_full <- alphas["California"] + beta_trend * trend_t +
                  s_signal * as.numeric(Lambda %*% u_CA)
CA_signal_pre  <- CA_signal_full[years <= 1988]
donor_signal_pre <- matrix(NA_real_, nrow = T_pre, ncol = N_eiv,
                           dimnames = list(NULL, rownames(U_eiv)))
for (i in seq_len(N_eiv)) {
  donor_signal_pre[, i] <- alpha_eiv[i] + beta_trend * trend_t[years <= 1988] +
                           s_signal * as.numeric(Lambda[years <= 1988, ] %*% U_eiv[i, ])
}

T_pre_grid <- 5:17
set.seed(2026)
n_reps <- 100
records <- vector("list", n_reps * length(T_pre_grid))
rec <- 0L
for (r in seq_len(n_reps)) {
  donor_Y_pre <- donor_signal_pre +
                 matrix(rnorm(T_pre * N_eiv, sd = sigma_eps), nrow = T_pre)
  CA_Y_pre    <- CA_signal_pre + rnorm(T_pre, sd = sigma_eps)
  for (k in T_pre_grid) {
    idx <- (T_pre - k + 1):T_pre
    Y_pre_k <- donor_Y_pre[idx, , drop = FALSE]
    x_CA_k  <- CA_Y_pre[idx]
    w_k <- tryCatch(mean_balance_simplex(Y_pre_k, x_CA_k),
                    error = function(e) rep(1/N_eiv, N_eiv))
    imb_k <- u_CA - as.vector(t(U_eiv) %*% w_k)
    rec <- rec + 1L
    records[[rec]] <- data.frame(rep = r, T_pre = k,
                                 health = imb_k[1],
                                 antitob = imb_k[2],
                                 tax = imb_k[3],
                                 wnorm2 = sum(w_k^2))
  }
}
eiv_results <- do.call(rbind, records)
eiv_summary <- eiv_results |>
  group_by(T_pre) |>
  summarise(
    health  = mean(abs(health)),
    antitob = mean(abs(antitob)),
    tax     = mean(abs(tax)),
    wnorm2  = mean(wnorm2),
    .groups = "drop"
  )

cat("\n--- EIV (N=49 synth donors): mean |u-imbalance| and mean ||w||^2 over ",
    n_reps, " draws ---\n", sep = "")
print(round(eiv_summary, 3))
cat(sprintf("\nFor theoretical fits: sigma_eps = %.3f, s_signal = %.2f, N_eiv = %d\n",
            sigma_eps, s_signal, N_eiv))

## Pool across the 3 factor dimensions: single avg curve
eiv_pooled <- eiv_summary |>
  mutate(mean_abs_avg = (health + antitob + tax) / 3) |>
  select(T_pre, mean_abs_avg)

## ---- Noiseless QP floor (kept for the printed diagnostic; not plotted) ----
w_floor    <- mean_balance_simplex(donor_signal_pre, CA_signal_pre)
imbal_floor <- u_CA - as.vector(t(U_eiv) %*% w_floor)
names(imbal_floor) <- c("Health consc.", "Anti-tobacco", "Tax policy")
cat("\n--- Noiseless QP floor on |u-imbalance| (N=49) ---\n")
print(round(abs(imbal_floor), 3))
cat(sprintf("||w_floor||^2 = %.4f; top 5 weights:\n", sum(w_floor^2)))
print(round(sort(w_floor, decreasing = TRUE)[1:5], 3))

## ---- Best-fitting c / sqrt(T_pre) line ----
fit <- lm(mean_abs_avg ~ 0 + I(1 / sqrt(T_pre)), data = eiv_pooled)
c_hat <- coef(fit)[1]
cat(sprintf("\nBest-fit c in c/sqrt(T_pre): c_hat = %.3f  (R^2 = %.3f)\n",
            c_hat, summary(fit)$r.squared))

fit_df <- data.frame(T_pre = T_pre_grid,
                     mean_abs_avg = c_hat / sqrt(T_pre_grid))

p_eiv_u <- ggplot(eiv_pooled, aes(T_pre, mean_abs_avg)) +
  geom_line(colour = "#1f78b4", linewidth = 1.0) +
  geom_point(colour = "#1f78b4", size = 1.9) +
  geom_line(data = fit_df, linetype = "dashed",
            colour = "black", linewidth = 0.8) +
  annotate("text", x = max(T_pre_grid) - 0.3, y = c_hat / sqrt(max(T_pre_grid)) + 0.08,
           label = sprintf("fit: %.2f / sqrt(T_pre)", c_hat),
           hjust = 1, size = 4, colour = "black") +
  scale_x_continuous(breaks = seq(min(T_pre_grid), max(T_pre_grid), 2)) +
  expand_limits(y = 0) +
  labs(x = expression(T[pre]),
       y = "mean |u-imbalance| (avg of 3 dims)",
       title = paste0("Mean |u-imbalance| (avg across 3 factor dims) vs T_pre")) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(figdir, "eiv_u_imbalance.pdf"), p_eiv_u,
       width = 7.0, height = 3.6)

## Save numbers so the slide can pull them in
writeLines(
  c(sprintf("imbalance_sg = %.3f", imbalance_4["Sleeping giant"]),
    sprintf("lambda4_T1 = %.3f", lambda4_T1),
    sprintf("lambda4_avg = %.3f", lambda4_avg),
    sprintf("bias_sg_T1 = %.3f", bias_sg_T1),
    sprintf("bias_visible_T1_sum = %.3f", sum(bias_visible_T1)),
    sprintf("bias_total_T1 = %.3f", bias_total_T1),
    sprintf("bias_total_avg = %.3f", bias_total_avg)),
  con = file.path(figdir, "bias_numbers.txt")
)

cat("LFM demo figures written to:", figdir, "\n")
