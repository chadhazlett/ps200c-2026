# make_hidalgo_pos.R
# Stylized Hidalgo-shaped figure illustrating the continuity assumption.
# Generates 4 build-up PDFs for a beamer overlay sequence, plus a combined PDF.
# - data + cutoff
# - + observed Y(0) (left) and Y(1) (right), solid
# - + counterfactual extensions, dashed
# - + treatment effect arrow at cutoff
#
# Output: Hidalgo_pos_1.pdf ... Hidalgo_pos_4.pdf, Hidalgo_pos_all.pdf

set.seed(42)

xmin   <- 20000
xmax   <- 60000
cutoff <- 40500
n      <- 500

voters <- runif(n, xmin, xmax)

y0 <- function(x) 0.55 + 0.30 * pnorm((x - 38000) / 5000)
y1 <- function(x) 0.93 + 0.03 * pnorm((x - 45000) / 8000)

above <- voters > cutoff
y_obs <- ifelse(above, y1(voters), y0(voters)) + rnorm(n, 0, 0.045)
y_obs <- pmin(pmax(y_obs, 0.50), 1.00)

xgrid <- seq(xmin, xmax, length.out = 400)
y0_g  <- y0(xgrid)
y1_g  <- y1(xgrid)
left  <- xgrid <= cutoff
right <- !left

# colors
col_y0_obs    <- "black"
col_y1_obs    <- "#2E5EAA"
col_cf        <- "gray45"
col_cutoff    <- "gray60"
col_effect    <- "#B22222"

draw_base <- function() {
  par(mar = c(4.2, 4.4, 1, 1), bty = "n")
  plot(voters, y_obs,
       pch = 16, col = adjustcolor("gray30", 0.35), cex = 0.65,
       xlim = c(xmin, xmax), ylim = c(0.55, 1.02),
       xlab = "Number of registered voters",
       ylab = "Share of valid votes",
       xaxt = "n", las = 1)
  axis(1, at = c(20000, 30000, 40500, 50000, 60000),
       labels = c("20k", "30k", "40.5k", "50k", "60k"))
  abline(v = cutoff, col = col_cutoff, lty = 3, lwd = 2)
}

draw_observed <- function() {
  lines(xgrid[left],  y0_g[left],  col = col_y0_obs, lwd = 3)
  lines(xgrid[right], y1_g[right], col = col_y1_obs, lwd = 3)
  text(xmin + 2000, y0(xmin) - 0.04, expression(Y(0)),
       col = col_y0_obs, cex = 1.3, font = 2)
  text(xmax - 2000, y1(xmax) + 0.03, expression(Y(1)),
       col = col_y1_obs, cex = 1.3, font = 2)
}

draw_counterfactual <- function() {
  lines(xgrid[right], y0_g[right], col = col_cf, lwd = 2, lty = 2)
  lines(xgrid[left],  y1_g[left],  col = col_cf, lwd = 2, lty = 2)
}

draw_effect_arrow <- function() {
  xa <- cutoff + 600
  arrows(xa, y0(cutoff), xa, y1(cutoff),
         code = 3, length = 0.07, lwd = 2.5, col = col_effect)
  text(cutoff + 5500, (y0(cutoff) + y1(cutoff)) / 2,
       expression(alpha[SRD]), col = col_effect, cex = 1.5)
}

w <- 7.5; h <- 4.6

write_stage <- function(name, fn) {
  pdf(name, width = w, height = h)
  draw_base()
  fn()
  dev.off()
}

write_stage("Hidalgo_pos_2.pdf", function() draw_observed())
write_stage("Hidalgo_pos_4.pdf", function() { draw_observed(); draw_counterfactual(); draw_effect_arrow() })

message("Wrote Hidalgo_pos_2.pdf and Hidalgo_pos_4.pdf to ", getwd())
