#---------------------------------------
# Love plot for Blattman & Annan:
# SMDs pre- vs post- subclassification on age x location
#---------------------------------------
setwd("~/teaching/ps200c-2026/slides/04-soo/figures")
dat <- read.csv("BlattmanAnnan.csv")

# Location = single factor from the 8 dummies
loc_cols <- c("C_ach","C_akw","C_ata","C_kma","C_oro","C_pad","C_paj","C_pal")
dat$location <- apply(dat[, loc_cols], 1, function(r) {
  m <- which(r == 1)
  if (length(m) == 1) loc_cols[m] else NA
})
# Units with no location dummy = reference location
dat$location[is.na(dat$location)] <- "C_ref"

# Age in 4 bins (roughly quartiles)
dat$age_bin <- cut(dat$age, breaks = quantile(dat$age, probs = 0:4/4, na.rm = TRUE),
                   include.lowest = TRUE, labels = c("Q1","Q2","Q3","Q4"))

# Stratum
dat$stratum <- interaction(dat$age_bin, dat$location, drop = TRUE)

# Covariates to check
covs <- c("age", "fthr_ed", "mthr_ed", "orphan96", "hh_fthr_frm", "hh_size96",
          loc_cols)
cov_labels <- c("Age", "Father's educ", "Mother's educ", "Orphan 1996",
                "Father in HH", "HH size 1996",
                "Loc: Acholibur", "Loc: Awach", "Loc: Atanga", "Loc: Kitgum-Matidi",
                "Loc: Oroom", "Loc: Padibe", "Loc: Pajule", "Loc: Palabek")
targeted <- c("age", loc_cols)   # variables used to define strata

# Standardized mean difference (pooled SD)
smd <- function(x, d) {
  x1 <- x[d == 1]; x0 <- x[d == 0]
  s <- sqrt((var(x1, na.rm = TRUE) + var(x0, na.rm = TRUE))/2)
  if (s == 0 || is.na(s)) return(NA)
  (mean(x1, na.rm = TRUE) - mean(x0, na.rm = TRUE)) / s
}

# Post-adjustment SMD: weighted average of within-stratum SMDs
# (weights = stratum share; drop strata with no variation in D or only 1 unit)
smd_post <- function(x, d, stratum) {
  # pool SD from full sample so units/scale are comparable to pre
  s_pool <- sqrt((var(x[d==1], na.rm = TRUE) + var(x[d==0], na.rm = TRUE))/2)
  if (is.na(s_pool) || s_pool == 0) return(NA)
  strata <- split(seq_along(x), stratum)
  diffs <- numeric(0); wts <- numeric(0)
  for (idx in strata) {
    if (length(idx) < 2) next
    dj <- d[idx]; xj <- x[idx]
    if (length(unique(dj)) < 2) next
    if (sum(dj == 1) == 0 || sum(dj == 0) == 0) next
    diffs <- c(diffs, mean(xj[dj==1], na.rm=TRUE) - mean(xj[dj==0], na.rm=TRUE))
    wts   <- c(wts, length(idx))
  }
  if (length(diffs) == 0) return(NA)
  sum(wts*diffs)/sum(wts) / s_pool
}

d <- dat$abd
pre  <- sapply(covs, function(v) smd(dat[[v]], d))
post <- sapply(covs, function(v) smd_post(dat[[v]], d, dat$stratum))

balance <- data.frame(var = cov_labels, pre = pre, post = post,
                      targeted = covs %in% targeted,
                      stringsAsFactors = FALSE)

# Order: non-targeted first (more interesting), then targeted
balance <- balance[order(balance$targeted, balance$var), ]
balance$y <- seq_len(nrow(balance))

# Plot
pdf("blattman_balance.pdf", width = 8.2, height = 5.2)
par(mar = c(4, 11, 2.5, 1))
xr <- c(-0.7, 0.7)

plot(NA, xlim = xr, ylim = c(0.5, nrow(balance) + 0.5),
     yaxt = "n", xlab = "Standardized mean difference (treated - control)",
     ylab = "", main = "Balance: before vs after subclassification on age x location",
     cex.main = 1.0)
axis(2, at = balance$y, labels = balance$var, las = 1, cex.axis = 0.85)
abline(v = 0, lty = 1, col = "grey70")
abline(v = c(-0.25, 0.25), lty = 3, col = "grey70")
# lines from pre to post
segments(balance$pre, balance$y, balance$post, balance$y, col = "grey60")
# targeted vars in blue, others in orange
col_post <- ifelse(balance$targeted, "#1f77b4", "#d95f02")
points(balance$pre,  balance$y, pch = 1,  col = "grey40",  cex = 1.1)
points(balance$post, balance$y, pch = 19, col = col_post,  cex = 1.1)
legend("topright",
       legend = c("Pre-adjustment",
                  "Post: targeted (age, location)",
                  "Post: not targeted"),
       pch = c(1, 19, 19),
       col = c("grey40", "#1f77b4", "#d95f02"),
       bty = "n", cex = 0.8, inset = c(0.01, 0.01))
dev.off()

cat("Wrote blattman_balance.pdf\n")
print(balance)
