## Generates blattman_pscore_pre.pdf and blattman_pscore_post.pdf
## Distribution of pscores (i) before matching, (ii) after 1-NN pscore matching.
## Legends enlarged.

library(Matching)

setwd("/Users/chadhazlett/teaching/ps200c-2026/slides/04-soo/figures")
dat <- read.csv("BlattmanAnnan.csv")

pi_out <- glm(abd ~ age + fthr_ed + mthr_ed + hh_size96 + orphan96 +
                C_ach + C_akw + C_ata + C_kma + C_oro + C_pad + C_paj + C_pal,
              data = dat, family = binomial(link = "logit"))
pscore <- pi_out$fitted.values

m_out <- Match(Y = dat$educ, Tr = dat$abd, X = pscore, M = 1, estimand = "ATT")
idx_treated <- m_out$index.treated
idx_control <- m_out$index.control

pre_t <- pscore[dat$abd == 1]
pre_c <- pscore[dat$abd == 0]
post_t <- pscore[idx_treated]
post_c <- pscore[idx_control]

xlim_common <- range(pscore)
ylim_common <- c(0, max(
  max(density(pre_t)$y), max(density(pre_c)$y),
  max(density(post_t)$y), max(density(post_c)$y)
)) * 1.05

draw_panel <- function(dens_t, dens_c, main_txt) {
  plot(dens_t, lwd = 3, col = "black", lty = 1,
       main = main_txt, xlab = "Estimated propensity score",
       xlim = xlim_common, ylim = ylim_common,
       cex.main = 1.6, cex.lab = 1.4, cex.axis = 1.3)
  lines(dens_c, lwd = 3, col = "black", lty = 2)
  legend("topleft", legend = c("treated (abducted)", "control"),
         lty = c(1, 2), lwd = 3, cex = 1.4, bty = "n")
}

pdf("blattman_pscore_pre.pdf", height = 4.2, width = 6)
par(mar = c(4.2, 4.2, 2.4, 1))
draw_panel(density(pre_t), density(pre_c), "Before matching")
dev.off()

pdf("blattman_pscore_post.pdf", height = 4.2, width = 6)
par(mar = c(4.2, 4.2, 2.4, 1))
draw_panel(density(post_t), density(post_c), "After 1-NN pscore matching")
dev.off()

cat("Wrote blattman_pscore_pre.pdf and blattman_pscore_post.pdf\n")
