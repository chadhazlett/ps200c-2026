## Event-study illustration for PS200C DID slides
## Two panels:
##   (left)  Single treatment time + never-treated control -> clean event-study
##   (right) Staggered adoption, heterogeneous effects, NO never-treated
##          -> already-treated used as controls for later cohorts
##          -> contaminated leads and lags
##
## Outside-window event-times absorbed into explicit endpoint bins so they
## don't leak into the k=-1 reference.

library(fixest)
library(ggplot2)
library(patchwork)

set.seed(1)
window <- -5:5

make_dummies <- function(df, col = "event_time", bin_endpoints = TRUE) {
  for (k in window) {
    df[[paste0("k", gsub("-", "m", k))]] <- as.numeric(
      !is.na(df[[col]]) & df[[col]] == k)
  }
  if (bin_endpoints) {
    df[["k_earlybin"]] <- as.numeric(
      !is.na(df[[col]]) & df[[col]] < min(window))
    df[["k_latebin"]]  <- as.numeric(
      !is.na(df[[col]]) & df[[col]] > max(window))
  }
  df
}

rhs_windowed <- paste0("k", gsub("-", "m", setdiff(window, -1)),
                        collapse = " + ")
rhs_binned <- paste0(
  c(paste0("k", gsub("-", "m", setdiff(window, -1))),
    "k_earlybin", "k_latebin"),
  collapse = " + ")

fit_and_coefs <- function(df, rhs) {
  fit <- feols(as.formula(paste("Y ~", rhs, "| i + t")), data = df)
  cn <- names(coef(fit))
  window_names <- paste0("k", gsub("-", "m", setdiff(window, -1)))
  keep <- window_names %in% cn
  out <- data.frame(event_time = setdiff(window, -1)[keep],
                    est = coef(fit)[window_names[keep]],
                    se = se(fit)[window_names[keep]])
  out <- rbind(out, data.frame(event_time = -1, est = 0, se = 0))
  out[order(out$event_time), ]
}

## -------- LEFT: clean event-study --------
N1 <- 400
T1 <- 20
treat_time <- 11

df1 <- expand.grid(i = 1:N1, t = 1:T1)
df1$D_unit <- ifelse(df1$i <= N1/2, 1, 0)             # half treated, half never
df1$event_time <- ifelse(df1$D_unit == 1, df1$t - treat_time, NA)

tau_post <- 0.5
df1$effect <- ifelse(!is.na(df1$event_time) & df1$event_time >= 0,
                     tau_post + 0.08 * df1$event_time, 0)

alpha_i1 <- rnorm(N1, 0, 0.5)
gamma_t1 <- seq(0, 0.3, length.out = T1)
df1$Y <- alpha_i1[df1$i] + gamma_t1[df1$t] + df1$effect +
         rnorm(nrow(df1), 0, 0.5)

## clean panel: use endpoint-binned spec so outside-window treated obs don't
## leak into the reference group
df1e <- make_dummies(df1, bin_endpoints = TRUE)
coef1 <- fit_and_coefs(df1e, rhs_binned)

## -------- RIGHT: staggered, heterogeneous, no never-treated --------
per_cohort <- 120
cohort_time <- c(8, 14, 20)
T2 <- 27
N2 <- per_cohort * length(cohort_time)

df2 <- expand.grid(i = 1:N2, t = 1:T2)
df2$cohort <- (df2$i - 1) %/% per_cohort + 1
df2$treat_time <- cohort_time[df2$cohort]
df2$event_time <- df2$t - df2$treat_time

tau_het_base <- c(1.6, 1.0, 0.4)
df2$effect <- ifelse(df2$event_time >= 0,
                     tau_het_base[df2$cohort] + 0.08 * df2$event_time, 0)

alpha_i2 <- rnorm(N2, 0, 0.5)
gamma_t2 <- seq(0, 0.3, length.out = T2)
df2$Y <- alpha_i2[df2$i] + gamma_t2[df2$t] + df2$effect +
         rnorm(nrow(df2), 0, 0.5)

## contaminated panel: DO NOT bin endpoints. Outside-window post-treatment
## observations for earlier cohorts (with large effects) get pooled with the
## k=-1 reference, which inflates the implicit baseline and pulls the
## displayed leads/lags away from the truth. This is the classic TWFE
## event-study pathology under staggered adoption + effect heterogeneity.
df2e <- make_dummies(df2, bin_endpoints = FALSE)
coef2 <- fit_and_coefs(df2e, rhs_windowed)

## -------- Plot --------
plot_es <- function(df, title, sub) {
  ggplot(df, aes(event_time, est)) +
    geom_hline(yintercept = 0, color = "gray60") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60") +
    geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se),
                  width = 0.25) +
    geom_point(size = 2.2) +
    scale_x_continuous(breaks = window) +
    labs(title = title, subtitle = sub,
         x = "Event time k (periods since treatment)",
         y = "Coefficient") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))
}

p1 <- plot_es(coef1,
              "DGP A: single treatment time, homogeneous effect",
              "Parallel trends holds; effects positive post-treatment")
p2 <- plot_es(coef2,
              "DGP B: staggered adoption, heterogeneous effects",
              "Parallel trends still holds; effects positive post-treatment")

ggsave("figures/event_study_clean.pdf", p1, width = 6.5, height = 4)
ggsave("figures/event_study_contaminated.pdf", p2, width = 6.5, height = 4)
ggsave("figures/event_study.pdf", p1 + p2, width = 10, height = 4)
cat("Wrote figures/event_study_clean.pdf, _contaminated.pdf, and combined.\n")

cat("\n---- LEFT (clean) ----\n"); print(coef1)
cat("\n---- RIGHT (contaminated) ----\n"); print(coef2)
