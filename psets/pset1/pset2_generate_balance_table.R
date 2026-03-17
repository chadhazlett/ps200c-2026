library(fabricatr)
library(ebal)
library(Matching)

set.seed(19861108)

# Set up study data
junk_study = fabricate(
  N = 120,
  gender = draw_binary(N = N, prob = 0.65),
  GPA = runif(n = N, min = 2.5, max = 4.0),
  optimism = rnorm(n = N, mean = 3 + 0.5 * gender, sd = 1),
  treatment = sample(c(rep(1, 60), rep(0, 60))),
  gender_treatment = ifelse(gender == 1,
                            draw_binary(N = N, prob = 0.75),
                            draw_binary(N = N, prob = 0.2))
)

# Balanced treatment
balance_obj = MatchBalance(treatment ~ gender + GPA + optimism,
                           data = junk_study)
table_bal = baltest.collect(balance_obj, 
                            var.names = c("gender", "GPA", "optimism"),
                            after = FALSE)[, c(1:4, 6:7)]

# Unbalanced treatment
unbalance_obj = MatchBalance(gender_treatment ~ gender + GPA + optimism,
                             data = junk_study)
table_unbal = baltest.collect(unbalance_obj,
                              var.names = c("gender", "GPA", "optimism"),
                              after = FALSE)[, c(1:4, 6:7)]


dput(table_bal)
dput(table_unbal)
