## prop99.R
## Real CA Prop 99 demo using tidysynth::smoking data + tjbal.
## Produces:
##   prop99_panel.pdf       -- raw CA vs donors trajectories
##   prop99_linear_fit.pdf  -- tjbal with linear kernel (mean balancing)
##   prop99_linear_gap.pdf  -- gap plot (CA - synthetic), linear kernel
##   prop99_kernel_fit.pdf  -- tjbal with Gaussian kernel
##   prop99_kernel_gap.pdf  -- gap plot, Gaussian kernel

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(tidysynth)
  library(kbal)
  library(tjbal)
})

## Modern kbal renamed several output fields (underscore -> dot) and dropped
## `earlyfail`. tjbal still expects the older names. Shim kbal in the
## attached package env so tjbal's lookups see the patched version.
.kbal_orig <- kbal::kbal
.kbal_shim <- function(...) {
  res <- .kbal_orig(...)
  if (is.null(res$earlyfail)) res$earlyfail <- FALSE
  if (!is.null(res$biasbound_opt))   res$biasbound.opt   <- res$biasbound_opt
  if (!is.null(res$biasbound_orig))  res$biasbound.orig  <- res$biasbound_orig
  if (!is.null(res$biasbound_ratio)) res$biasbound.ratio <- res$biasbound_ratio
  if (!is.null(res$dist_record))     res$dist.record     <- res$dist_record
  res
}
pkg_env <- as.environment("package:kbal")
unlockBinding("kbal", pkg_env)
assign("kbal", .kbal_shim, envir = pkg_env)
lockBinding("kbal", pkg_env)
assignInNamespace("kbal", .kbal_shim, ns = "kbal")

script_path <- tryCatch({
  args <- commandArgs(trailingOnly = FALSE)
  fa <- args[grep("^--file=", args)]
  if (length(fa)) sub("^--file=", "", fa[1]) else sys.frame(1)$ofile
}, error = function(e) NULL)
if (is.null(script_path) || !nzchar(script_path)) script_path <- "sim/prop99.R"
here <- normalizePath(dirname(script_path), mustWork = FALSE)
figdir <- normalizePath(file.path(here, "..", "figures"), mustWork = FALSE)
dir.create(figdir, showWarnings = FALSE, recursive = TRUE)

data(smoking, package = "tidysynth")
dat <- smoking |>
  ## Prop 99 was being organized through 1987 (Coalition for a Healthy California
  ## formed; initiative drafted), 1.1M signatures gathered late-1987 -- May 1988,
  ## ballot Nov 1988, tax effective Jan 1989. Anticipatory purchasing was likely
  ## already happening in 1987. Treat 1987 as the first post-treatment period.
  mutate(D = as.integer(state == "California" & year >= 1987))

## ---- Raw panel view ----
p_panel <- ggplot(dat, aes(year, cigsale, group = state,
                           colour = state == "California",
                           linewidth = state == "California")) +
  geom_line() +
  geom_vline(xintercept = 1986.5, linetype = "dashed", alpha = 0.4) +
  scale_colour_manual(values = c("FALSE" = "grey55", "TRUE" = "#e31a1c"),
                      guide = "none") +
  scale_linewidth_manual(values = c("FALSE" = 0.5, "TRUE" = 1.4),
                         guide = "none") +
  annotate("text", x = 1971, y = 280, hjust = 0,
           label = "California (red); 38 donors (grey)", size = 4.2) +
  labs(x = NULL, y = "per-capita cigarette sales (packs)",
       title = "California Proposition 99 (passed Nov 1988): raw panel") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(figdir, "prop99_panel.pdf"), p_panel,
       width = 7.0, height = 4.2)

## ---- tjbal helper: build dataframe expected by tjbal() ----
df <- dat |>
  arrange(state, year) |>
  as.data.frame()

## ---- tjbal with linear (mean-balancing) kernel ----
fit_lin <- tjbal(
  data = df, Y = "cigsale", D = "D",
  index = c("state", "year"),
  demean = FALSE, kernel = FALSE,
  vce = "jackknife", print.baltable = FALSE, parallel = FALSE
)

## Extract CA actual + synthetic (counterfactual) trajectories from Y.bar
make_fit_df <- function(fit) {
  Yb <- as.data.frame(fit$Y.bar)
  ## row names like "cigsale1970" -> 1970
  years <- as.integer(gsub("[^0-9]", "", rownames(Yb)))
  data.frame(year = years,
             actual = Yb$Y.tr.bar,
             synthetic = Yb$Y.ct.bar,
             donor_avg = Yb$Y.co.bar)
}

fdf_lin <- make_fit_df(fit_lin)

plot_fit <- function(fdf, title) {
  ggplot(fdf, aes(year)) +
    geom_line(aes(y = actual), colour = "#e31a1c", linewidth = 1.3) +
    geom_line(aes(y = synthetic), colour = "black", linewidth = 1.0,
              linetype = "dashed") +
    geom_vline(xintercept = 1986.5, linetype = "dashed", alpha = 0.4) +
    annotate("text", x = 1971, y = max(fdf$actual, fdf$synthetic),
             hjust = 0, vjust = 1,
             label = "California (red, solid)\nSynthetic CA (black, dashed)",
             size = 3.8) +
    labs(x = NULL, y = "per-capita cigarette sales (packs)",
         title = title) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank())
}

plot_gap <- function(fdf, title) {
  fdf$gap <- fdf$actual - fdf$synthetic
  ggplot(fdf, aes(year, gap)) +
    geom_hline(yintercept = 0, colour = "grey50") +
    geom_vline(xintercept = 1986.5, linetype = "dashed", alpha = 0.4) +
    geom_line(colour = "#1f78b4", linewidth = 1.2) +
    labs(x = NULL, y = "CA - synthetic CA (packs)",
         title = title) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank())
}

ggsave(file.path(figdir, "prop99_linear_fit.pdf"),
       plot_fit(fdf_lin, "tjbal, linear kernel (= mean balancing on Y_pre)"),
       width = 7.0, height = 4.2)

ggsave(file.path(figdir, "prop99_linear_gap.pdf"),
       plot_gap(fdf_lin, "Estimated effect, linear kernel"),
       width = 7.0, height = 4.2)

## ---- Proper LDV regression with treatment indicator -----------------------
##   For each candidate "event year" t*, fit on ALL 39 states:
##      Y_{i,t*} = alpha + beta' Y_{i, pre-window} + tau_{t*} D_i + eps
##   where D_i = 1 for California, 0 otherwise. Pre-window: 1970-1986. For
##   pre-period t* (placebo years), drop t* from the pre-window (leave-one-out)
##   so we never include the target year on both sides.
pre_years_ldv  <- 1970:1986
post_years_ldv <- 1987:2000
all_years_ldv  <- 1970:2000

wide_panel <- df |>
  select(state, year, cigsale) |>
  pivot_wider(names_from = year, values_from = cigsale, names_prefix = "y") |>
  mutate(D = as.integer(state == "California"))
pre_cols   <- paste0("y", pre_years_ldv)

ldv_tau_one <- function(tgt) {
  ycol       <- paste0("y", tgt)
  regressors <- if (tgt %in% pre_years_ldv) setdiff(pre_cols, ycol) else pre_cols
  fmla       <- as.formula(paste0("`", ycol, "` ~ D + ",
                                  paste0("`", regressors, "`", collapse = " + ")))
  fit <- lm(fmla, data = wide_panel)
  as.numeric(coef(fit)["D"])
}

ldv_gap <- sapply(all_years_ldv, ldv_tau_one)   # tau_hat at each year

## Merge LDV gap into fdf_lin for overlay
gap_compare <- merge(
  fdf_lin |> transmute(year, tjbal_gap = actual - synthetic),
  data.frame(year = all_years_ldv, ldv_gap = ldv_gap),
  by = "year"
)

p_compare_gap <- ggplot(gap_compare, aes(year)) +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_vline(xintercept = 1986.5, linetype = "dashed", alpha = 0.4) +
  geom_line(aes(y = tjbal_gap, colour = "tjbal linear (synth)"), linewidth = 1.3) +
  geom_line(aes(y = ldv_gap,   colour = "LDV regression"),         linewidth = 1.3,
            linetype = "dashed") +
  scale_colour_manual(values = c("tjbal linear (synth)" = "#1f78b4",
                                 "LDV regression"        = "#e31a1c")) +
  labs(x = NULL, y = "CA actual - estimate (packs)",
       colour = NULL,
       title = "LDV regression vs mean-balancing synth: gap series") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top")

ggsave(file.path(figdir, "prop99_ldv_vs_tjbal.pdf"), p_compare_gap,
       width = 7.0, height = 4.2)

cat(sprintf("\nLDV avg post-treatment estimated effect: %.2f packs\n",
            mean(ldv_gap[all_years_ldv %in% post_years_ldv])))
cat(sprintf("tjbal-linear avg post: %.2f packs (for comparison)\n",
            mean((fdf_lin$actual - fdf_lin$synthetic)[fdf_lin$year %in% post_years_ldv])))

## ---- Linear-kernel weights bar chart (Prop 99) ----
weights_df <- data.frame(
  unit   = fit_lin$data.wide$unit[match(fit_lin$id.co, fit_lin$data.wide$id)],
  weight = as.numeric(fit_lin$weights.co)
)
weights_df$unit <- factor(weights_df$unit, levels = weights_df$unit[order(-weights_df$weight)])
weights_df$gotweight <- weights_df$weight > 0.005

p_w_prop99 <- ggplot(weights_df, aes(unit, weight, fill = gotweight)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = ifelse(gotweight, sprintf("%.2f", weight), "")),
            vjust = -0.3, size = 3.2) +
  scale_fill_manual(values = c(`FALSE` = "grey75", `TRUE` = "#1f78b4"),
                    guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.18))) +
  labs(x = NULL, y = "donor weight",
       title = "Linear-kernel simplex weights on real Prop 99 data") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 8))

ggsave(file.path(figdir, "prop99_weights.pdf"), p_w_prop99,
       width = 8.5, height = 4.0)

cat("\nLinear-kernel weights (Prop 99), top 6:\n")
print(round(setNames(weights_df$weight, weights_df$unit) |> sort(decreasing = TRUE) |> head(6), 3))

## ---- tjbal with Gaussian kernel ----
fit_kern <- tjbal(
  data = df, Y = "cigsale", D = "D",
  index = c("state", "year"),
  demean = FALSE, kernel = TRUE,
  vce = "jackknife", print.baltable = FALSE, parallel = FALSE
)
fdf_kern <- make_fit_df(fit_kern)

ggsave(file.path(figdir, "prop99_kernel_fit.pdf"),
       plot_fit(fdf_kern, "tjbal, Gaussian kernel (balance richer features of Y_pre)"),
       width = 7.0, height = 4.2)

ggsave(file.path(figdir, "prop99_kernel_gap.pdf"),
       plot_gap(fdf_kern, "Estimated effect, Gaussian kernel"),
       width = 7.0, height = 4.2)

## ---- Comparison panel: tjbal linear, tjbal kernel, Abadie SCM, augsynth ----
## Build a single "synthetic CA" trajectory per estimator and overlay all.
suppressPackageStartupMessages({library(tidysynth); library(augsynth)})

## (1) Classic Abadie SCM via tidysynth: just match on pre-period cigsale
##     (no extra covariates; mirrors what tjbal-linear does).
abadie_pipe <- smoking |>
  mutate(D = as.integer(state == "California" & year >= 1987)) |>
  synthetic_control(outcome = cigsale,
                    unit    = state,
                    time    = year,
                    i_unit  = "California",
                    i_time  = 1987,
                    generate_placebos = FALSE) |>
  generate_predictor(time_window = 1970:1986, mean_cigsale = mean(cigsale)) |>
  generate_predictor(time_window = 1970,    cigsale_1970 = cigsale) |>
  generate_predictor(time_window = 1975,    cigsale_1975 = cigsale) |>
  generate_predictor(time_window = 1980,    cigsale_1980 = cigsale) |>
  generate_predictor(time_window = 1986,    cigsale_1986 = cigsale) |>
  generate_weights(optimization_window = 1970:1986) |>
  generate_control()
abadie_synth <- abadie_pipe |> grab_synthetic_control(placebo = FALSE)
abadie_df <- data.frame(year = abadie_synth$time_unit,
                        actual = abadie_synth$real_y,
                        synthetic = abadie_synth$synth_y,
                        method = "Abadie SCM (tidysynth)")

## (2) Augmented SCM via augsynth (with default ridge-augmentation)
aug_fit <- augsynth(cigsale ~ D, unit = state, time = year, data = df,
                    progfunc = "Ridge", scm = TRUE)
aug_pred <- predict(aug_fit, att = FALSE)   # synthetic CA outcome
aug_actual <- df |> filter(state == "California") |> arrange(year) |> pull(cigsale)
aug_df <- data.frame(year = sort(unique(df$year)),
                     actual = aug_actual,
                     synthetic = as.numeric(aug_pred),
                     method = "Augmented SCM (augsynth)")

## (3) tjbal linear (already computed)
tjbal_lin_df <- transmute(fdf_lin, year, actual, synthetic,
                          method = "tjbal linear")

## (4) tjbal Gaussian (already computed)
tjbal_kern_df <- transmute(fdf_kern, year, actual, synthetic,
                           method = "tjbal Gaussian kernel")

compare_all <- rbind(tjbal_lin_df, tjbal_kern_df, abadie_df, aug_df)
compare_all$method <- factor(compare_all$method,
  levels = c("Abadie SCM (tidysynth)", "Augmented SCM (augsynth)",
             "tjbal linear", "tjbal Gaussian kernel"))

p_compare_methods <- ggplot(compare_all, aes(year)) +
  geom_vline(xintercept = 1986.5, linetype = "dashed", alpha = 0.4) +
  geom_line(data = subset(compare_all, method == "Abadie SCM (tidysynth)"),
            aes(y = actual), colour = "#e31a1c", linewidth = 1.4) +  # actual CA, once
  geom_line(aes(y = synthetic, colour = method, linetype = method),
            linewidth = 1.0) +
  scale_colour_manual(values = c("Abadie SCM (tidysynth)"   = "#1f78b4",
                                 "Augmented SCM (augsynth)" = "#33a02c",
                                 "tjbal linear"             = "#ff7f00",
                                 "tjbal Gaussian kernel"    = "black")) +
  scale_linetype_manual(values = c("Abadie SCM (tidysynth)"   = "dashed",
                                   "Augmented SCM (augsynth)" = "dotted",
                                   "tjbal linear"             = "longdash",
                                   "tjbal Gaussian kernel"    = "solid")) +
  labs(x = NULL, y = "per-capita cigarette sales (packs)",
       colour = NULL, linetype = NULL,
       title = "Prop 99: synthetic California across four estimators") +
  annotate("text", x = 1971, y = max(compare_all$actual), hjust = 0, vjust = 1,
           label = "Actual CA (red)", size = 3.6, colour = "#e31a1c") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank())

ggsave(file.path(figdir, "prop99_synth_comparison.pdf"), p_compare_methods,
       width = 8.0, height = 4.5)

## Post-period average effects for each
post_yrs <- 1987:2000
cat("\n--- Post-period average ATTs across estimators ---\n")
for (m in levels(compare_all$method)) {
  sub <- subset(compare_all, method == m & year %in% post_yrs)
  cat(sprintf("  %-30s %.2f packs/cap\n", m, mean(sub$actual - sub$synthetic)))
}

## ---- tjbal: kernel + explicit mean balance via X = unit pre-period mean ----
## Compute each state's pre-period (1970-1986) mean of cigsale as a state-level
## covariate, then balance on it alongside the kernel features of demeaned Y_pre.
df$cigsale_premean <- ave(
  ifelse(df$year <= 1986, df$cigsale, NA),
  df$state,
  FUN = function(x) mean(x, na.rm = TRUE)
)

fit_kern_xmean <- tjbal(
  data = df, Y = "cigsale", D = "D",
  index = c("state", "year"),
  X = "cigsale_premean",
  demean = TRUE, kernel = TRUE,
  vce = "jackknife", print.baltable = FALSE, parallel = FALSE
)
fdf_kern_xmean <- make_fit_df(fit_kern_xmean)

ggsave(file.path(figdir, "prop99_kernel_xmean_fit.pdf"),
       plot_fit(fdf_kern_xmean,
                "tjbal, Gaussian kernel + explicit mean-balance (X = pre-period mean)"),
       width = 7.0, height = 4.2)

ggsave(file.path(figdir, "prop99_kernel_xmean_gap.pdf"),
       plot_gap(fdf_kern_xmean,
                "Estimated effect: Gaussian kernel + mean-balance via X"),
       width = 7.0, height = 4.2)

## ---- Summary numbers for the deck ----
summary_lin       <- tryCatch(fit_lin$att.avg,       error = function(e) NA)
summary_kern      <- tryCatch(fit_kern$att.avg,      error = function(e) NA)
summary_kern_xmean<- tryCatch(fit_kern_xmean$att.avg, error = function(e) NA)
cat("\n--- tjbal Prop 99 estimates ---\n")
cat("linear-kernel ATT (avg post):                          ", round(summary_lin, 2), "\n")
cat("Gaussian-kernel ATT (avg post):                        ", round(summary_kern, 2), "\n")
cat("Gaussian-kernel + explicit X=premean ATT (avg post):   ", round(summary_kern_xmean, 2), "\n")

cat("\nFigures written to:", figdir, "\n")
