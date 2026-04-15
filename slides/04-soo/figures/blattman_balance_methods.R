## blattman_balance_methods.R
## Standardized mean differences on Blattman & Annan, across 4 adjustment methods:
## (orig, pscore matching, IPW, entropy balancing). Writes all_blattman.pdf.
## Legend: boxed, upper-right.

library(Matching)
library(ebal)

setwd("/Users/chadhazlett/teaching/ps200c-2026/slides/04-soo/figures")
dat <- read.csv("BlattmanAnnan.csv")

covars_full <- c("age","fthr_ed","mthr_ed","hh_size96","orphan96",
                 "C_ach","C_akw","C_ata","C_kma","C_oro","C_pad","C_paj","C_pal")
covars_bal  <- setdiff(covars_full, "C_pal")
dat <- dat[complete.cases(dat[, c("abd","educ", covars_full)]), ]
D <- dat$abd

## SMD (standardized mean difference): (mean_t - mean_c) / sd_pooled.
## For weighted samples: weighted means and weighted sds (pooled).
smd <- function(x, d, w = rep(1, length(x))) {
  w1 <- w[d == 1]; w0 <- w[d == 0]
  m1 <- sum(w1 * x[d == 1]) / sum(w1)
  m0 <- sum(w0 * x[d == 0]) / sum(w0)
  v1 <- sum(w1 * (x[d == 1] - m1)^2) / sum(w1)
  v0 <- sum(w0 * (x[d == 0] - m0)^2) / sum(w0)
  (m1 - m0) / sqrt((v1 + v0) / 2)
}

## Pscore
pi_out <- glm(abd ~ age + fthr_ed + mthr_ed + hh_size96 + orphan96 +
                C_ach + C_akw + C_ata + C_kma + C_oro + C_pad + C_paj + C_pal,
              data = dat, family = binomial("logit"))
pscore <- pi_out$fitted.values

## Pscore matching weights (ATT): treated=1; controls get number of times used
m_ps <- Match(Y = dat$educ, Tr = D, X = pscore, M = 1, estimand = "ATT")
w_psm <- rep(0, nrow(dat))
w_psm[D == 1] <- 1
tab <- table(m_ps$index.control)
w_psm[as.integer(names(tab))] <- as.integer(tab)

## IPW (ATT): treated=1; controls get pi/(1-pi)
w_ipw <- ifelse(D == 1, 1, pscore / (1 - pscore))

## Entropy balancing
X_bal <- as.matrix(dat[, covars_bal])
eb <- ebalance(Treatment = D, X = X_bal)
w_ebal <- rep(1, length(D))
w_ebal[D == 0] <- eb$w

## Compute SMDs across methods
methods <- c("orig", "psmatch", "ipw", "ebal")
weight_list <- list(orig    = rep(1, nrow(dat)),
                    psmatch = w_psm,
                    ipw     = w_ipw,
                    ebal    = w_ebal)

smd_mat <- sapply(weight_list, function(w) sapply(covars_full, function(v) smd(dat[[v]], D, w)))
## rows = covariates, cols = methods

## Plot: dotplot, one row per covariate, points per method
pdf("all_blattman.pdf", height = 6, width = 8)
par(mar = c(4.5, 7, 1.2, 1))
p <- nrow(smd_mat)
yy <- seq(p, 1)
plot(NA, xlim = c(-1.1, 1.1), ylim = c(0.5, p + 0.5), yaxt = "n",
     xlab = "standardized difference in means", ylab = "",
     cex.lab = 1.25, cex.axis = 1.1)
abline(v = 0, lwd = 1)
abline(v = c(-1, -0.5, 0.5, 1), lwd = 1, lty = "dotted")
for (i in seq_len(p)) abline(h = i, lwd = 0.6, lty = "dashed", col = "gray90")

cols <- c(orig = "#66C2A5", psmatch = "#FC8D62", ipw = "#8DA0CB", ebal = "#E78AC3")
pchs <- c(orig = 16,        psmatch = 17,        ipw = 15,        ebal = 1)
for (m in methods) {
  points(smd_mat[, m], yy, col = cols[m], pch = pchs[m], cex = 1.6, lwd = 2)
}
axis(2, at = yy, labels = rownames(smd_mat), las = 1, cex.axis = 1.05)

legend("topright", legend = methods, col = cols[methods], pch = pchs[methods],
       cex = 1.2, pt.cex = 1.6, pt.lwd = 2, bty = "o", box.lwd = 1,
       bg = "white", inset = 0.01)
dev.off()

cat("Wrote all_blattman.pdf\n")
