## blattman_estimates.R
## Compares ATT estimates across methods on Blattman & Annan (2009).
##   (1) Subclassification on age-quartile x location
##   (2) Pscore matching (1-NN, ATT) on full covariate set
##   (3) IPW        (ATT weights: w=1 for treated, pi/(1-pi) for controls)
##   (4) Entropy balancing (ebal, mean balance)
##   (5) Kernel balancing (kbal)
##   (6) G-computation / Lin interacted OLS
##
## SEs: weighted regression Y ~ D + X with HC2 for IPW/ebal/kbal;
##      subclassification via stratum-weighted variance;
##      matching via Abadie-Imbens;
##      g-comp via HC2 on Lin interaction regression.

library(Matching)
library(ebal)
library(kbal)
library(sandwich)

setwd("/Users/chadhazlett/teaching/ps200c-2026/slides/04-soo/figures")
dat <- read.csv("BlattmanAnnan.csv")

## Keep complete cases on the analysis vars
covars_full <- c("age","fthr_ed","mthr_ed","hh_size96","orphan96",
                 "C_ach","C_akw","C_ata","C_kma","C_oro","C_pad","C_paj","C_pal")
## Drop one location dummy to avoid perfect collinearity when balancing
covars_bal  <- c("age","fthr_ed","mthr_ed","hh_size96","orphan96",
                 "C_ach","C_akw","C_ata","C_kma","C_oro","C_pad","C_paj")  # C_pal = reference
dat <- dat[complete.cases(dat[, c("abd", "educ", covars_full)]), ]

D <- dat$abd
Y <- dat$educ
X <- as.matrix(dat[, covars_bal])

## ----- HC2 helper -----
hc2_se <- function(fit) sqrt(diag(vcovHC(fit, type = "HC2")))

## ==========================================================
## (1) Subclassification: age quartile x location
## ==========================================================
dat$age_q <- cut(dat$age, breaks = quantile(dat$age, probs = seq(0, 1, 0.25)),
                 include.lowest = TRUE, labels = FALSE)
loc_vars <- c("C_ach","C_akw","C_ata","C_kma","C_oro","C_pad","C_paj","C_pal")
## Define location as index of which C_* is 1 (some are 0 everywhere -> residual)
loc_idx <- apply(dat[, loc_vars], 1, function(r) { w <- which(r == 1); if (length(w) == 0) 0 else w[1] })
dat$loc <- loc_idx
dat$stratum <- paste(dat$age_q, dat$loc, sep = "_")

## Compute within-stratum DIM and ATT-weighted sum
strata <- split(dat, dat$stratum)
strat_info <- lapply(strata, function(s) {
  n1 <- sum(s$abd == 1); n0 <- sum(s$abd == 0)
  if (n1 == 0 || n0 == 0) return(NULL)
  y1 <- s$educ[s$abd == 1]; y0 <- s$educ[s$abd == 0]
  list(n1 = n1, n0 = n0, dim = mean(y1) - mean(y0),
       v = (if (n1 >= 2) var(y1) else 0) / n1 +
           (if (n0 >= 2) var(y0) else 0) / n0)
})
strat_info <- strat_info[!sapply(strat_info, is.null)]
total_n1 <- sum(sapply(strat_info, function(x) x$n1))
att_sub <- sum(sapply(strat_info, function(x) x$n1 * x$dim)) / total_n1
se_sub  <- sqrt(sum(sapply(strat_info, function(x) (x$n1 / total_n1)^2 * x$v)))

## ==========================================================
## (2a) Mahalanobis matching on full X (1-NN, ATT)
## ==========================================================
Xmah <- as.matrix(dat[, covars_full])
m_mah <- Match(Y = Y, Tr = D, X = Xmah, M = 1, estimand = "ATT", Weight = 2)
att_mah <- m_mah$est[1, 1]
se_mah  <- m_mah$se.standard

## ==========================================================
## (2b) Propensity score matching (1-NN, ATT)
## ==========================================================
pi_out <- glm(abd ~ age + fthr_ed + mthr_ed + hh_size96 + orphan96 +
                C_ach + C_akw + C_ata + C_kma + C_oro + C_pad + C_paj + C_pal,
              data = dat, family = binomial("logit"))
pscore <- pi_out$fitted.values
m_out <- Match(Y = Y, Tr = D, X = pscore, M = 1, estimand = "ATT")
att_ps  <- m_out$est[1, 1]
se_ps   <- m_out$se.standard   # Abadie-Imbens

## ==========================================================
## (3) IPW (ATT weights) + weighted regression Y ~ D + X, HC2
## ==========================================================
w_ipw <- ifelse(D == 1, 1, pscore / (1 - pscore))
fit_ipw <- lm(educ ~ abd + age + fthr_ed + mthr_ed + hh_size96 + orphan96 +
                C_ach + C_akw + C_ata + C_kma + C_oro + C_pad + C_paj + C_pal,
              data = dat, weights = w_ipw)
att_ipw <- coef(fit_ipw)["abd"]
se_ipw  <- hc2_se(fit_ipw)["abd"]

## ==========================================================
## (4) Entropy balancing (mean balance), ATT
## ==========================================================
eb <- ebalance(Treatment = D, X = X)
w_ebal <- rep(1, length(D))
w_ebal[D == 0] <- eb$w
fit_ebal <- lm(educ ~ abd + age + fthr_ed + mthr_ed + hh_size96 + orphan96 +
                 C_ach + C_akw + C_ata + C_kma + C_oro + C_pad + C_paj + C_pal,
               data = dat, weights = w_ebal)
att_ebal <- coef(fit_ebal)["abd"]
se_ebal  <- hc2_se(fit_ebal)["abd"]

## ==========================================================
## (5) Kernel balancing (kbal), ATT
## ==========================================================
set.seed(42)
kb <- kbal(allx = X, treatment = D, printprogress = FALSE)
w_kbal <- kb$w   # length n; treated should have w=1, controls reweighted
cat("\nkbal diagnostics:\n"); print(names(kb))
cat("biasbound before/after/ratio:",
    round(kb$biasbound_orig, 4), "/",
    round(kb$biasbound_opt, 4), "/",
    round(kb$biasbound_ratio, 4), "\n\n")
fit_kbal <- lm(educ ~ abd + age + fthr_ed + mthr_ed + hh_size96 + orphan96 +
                 C_ach + C_akw + C_ata + C_kma + C_oro + C_pad + C_paj + C_pal,
               data = dat, weights = w_kbal)
att_kbal <- coef(fit_kbal)["abd"]
se_kbal  <- hc2_se(fit_kbal)["abd"]

## ==========================================================
## (6) G-comp / Lin interacted OLS (ATT: evaluate at treated mean of X)
## ==========================================================
Xc_treated <- X - matrix(colMeans(X[D == 1, ]), nrow = nrow(X), ncol = ncol(X), byrow = TRUE)
dat_lin <- data.frame(educ = Y, abd = D, Xc_treated)
fit_lin <- lm(educ ~ abd * ., data = dat_lin)
att_gcomp <- coef(fit_lin)["abd"]
se_gcomp  <- hc2_se(fit_lin)["abd"]

## ==========================================================
## Assemble + plot
## ==========================================================
methods <- c("Subclassification\n(age x location)",
             "Mahalanobis matching",
             "Pscore matching",
             "IPW",
             "Entropy balancing",
             "Kernel balancing",
             "G-comp / Lin")
ests <- c(att_sub, att_mah, att_ps, att_ipw, att_ebal, att_kbal, att_gcomp)
ses  <- c(se_sub,  se_mah,  se_ps,  se_ipw,  se_ebal,  se_kbal,  se_gcomp)
lo   <- ests - 1.96 * ses
hi   <- ests + 1.96 * ses

print(data.frame(method = methods, est = round(ests, 3),
                 se = round(ses, 3), lo = round(lo, 3), hi = round(hi, 3)))

pdf("blattman_estimates.pdf", height = 4.2, width = 6.6)
par(mar = c(4, 10.5, 2, 1))
yy <- seq_along(methods)
xlim <- c(-2.5, 1)
plot(NA, xlim = xlim, ylim = c(0.5, length(methods) + 0.5),
     yaxt = "n", xlab = "ATT estimate (years of education)", ylab = "",
     cex.lab = 1.2, cex.axis = 1.1,
     main = "Blattman & Annan (2009): ATT across methods")
abline(v = 0, lty = 2, col = "gray60")
segments(lo, yy, hi, yy, lwd = 2)
points(ests, yy, pch = 19, cex = 1.3)
axis(2, at = yy, labels = methods, las = 1, cex.axis = 0.95)
dev.off()

cat("Wrote blattman_estimates.pdf\n")
