# Generate randomization inference null distribution figure
# For PS200C Experiments slides

set.seed(42)

# --- Simulate a small experiment ---
N <- 50
N1 <- 25  # treated
N0 <- N - N1

# Generate potential outcomes with a true effect of ~3
Y0 <- rnorm(N, mean = 10, sd = 4)
tau_true <- 3
Y1 <- Y0 + tau_true + rnorm(N, sd = 1)  # heterogeneous effects

# Actual treatment assignment
D <- sample(c(rep(1, N1), rep(0, N0)))
Y_obs <- D * Y1 + (1 - D) * Y0

# Observed ATE
ate_obs <- mean(Y_obs[D == 1]) - mean(Y_obs[D == 0])

# --- Randomization inference under sharp null ---
n_sims <- 10000
ate_null <- numeric(n_sims)
for (s in 1:n_sims) {
  D_sim <- sample(D)  # re-randomize (permute treatment)
  ate_null[s] <- mean(Y_obs[D_sim == 1]) - mean(Y_obs[D_sim == 0])
}

# Two-sided p-value
p_val <- mean(abs(ate_null) >= abs(ate_obs))

# --- Plot ---
pdf("ri_null_distribution.pdf", width = 7, height = 4.5)
par(mar = c(4.5, 4, 2, 1))

hist(ate_null, breaks = 50, col = "gray85", border = "gray60",
     main = "", xlab = "Difference in Means Under Sharp Null",
     ylab = "Frequency", xlim = range(c(ate_null, ate_obs, -ate_obs)) * 1.1)

# Shade tails
h <- hist(ate_null, breaks = 50, plot = FALSE)
for (i in seq_along(h$mids)) {
  if (h$mids[i] >= abs(ate_obs) | h$mids[i] <= -abs(ate_obs)) {
    rect(h$breaks[i], 0, h$breaks[i + 1], h$counts[i],
         col = "firebrick3", border = "firebrick4")
  }
}

# Mark observed ATE
abline(v = ate_obs, col = "firebrick3", lwd = 2.5, lty = 1)
abline(v = -ate_obs, col = "firebrick3", lwd = 2.5, lty = 2)

# Labels
text(ate_obs, max(h$counts) * 0.92,
     bquote(hat(tau)[obs] == .(sprintf("%.2f", ate_obs))),
     pos = 4, col = "firebrick4", cex = 0.95, font = 2)

text(-ate_obs, max(h$counts) * 0.92,
     bquote("(" * .(sprintf("%.2f", -ate_obs)) * ")"),
     pos = 2, col = "firebrick4", cex = 0.85, font = 2)


# Subtitle
mtext(bquote(N == .(N) ~ ", " ~ N[1] == .(N1) ~ ", " ~
             .(format(n_sims, big.mark = ",")) ~ " random assignments"),
      side = 3, line = 0.3, cex = 0.85)

# t-test for comparison
t_out <- t.test(Y_obs[D == 1], Y_obs[D == 0], var.equal = FALSE)
p_ttest <- t_out$p.value

# Add t-test comparison annotation (far right)
text(par("usr")[2] * 0.95, max(h$counts) * 0.55,
     bquote(atop(
       "RI:   " * italic(p) == .(sprintf("%.4f", p_val)),
       italic(t)*"-test: " * italic(p) == .(sprintf("%.4f", p_ttest))
     )),
     pos = 2, col = "gray30", cex = 0.85, font = 1)

dev.off()
cat("Observed ATE:", round(ate_obs, 3), "\n")
cat("RI two-sided p-value:", round(p_val, 4), "\n")
cat("t-test p-value:", round(p_ttest, 4), "\n")
