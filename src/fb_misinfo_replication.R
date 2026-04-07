# Reducing misinformation sharing at scale using digital accuracy prompt ads
# Facebook Experiment Replication code
#

# =========================================
# Setup packages, directory, etc.
# =========================================

rm(list = ls())
library(fixest)


# =========================================
# Generate simulated data
# =========================================

# Simulate results from 2 million users; we use the original treatment
# coefficients in generating the data simply so the final reg output is
# similar to that which appears in the main text
set.seed(12345)
n <- 2e6

treatment <- rbinom(n, 1, 0.5)
p <- 0.021899 + treatment * (-0.000570)
misinfo_yes <- rbinom(n, 1, p)

# This data.frame mimics whether each user (row) posted misinfo within 60
# minutes of exposure (0/1), as well as whether they were in treatment or
# control (0/1)
dt <- data.frame(
  misinfo_yes = misinfo_yes,
  treatment = treatment
)

# =========================================
# Run analysis
# =========================================

# Run on the original data, this produces the main result from Figure 2A in
# the main text (b = -5.70, p = .004). Running different variants of this
# specification (e.g., on different subpopulations) yields the other main
# results
reg <- feglm(misinfo_yes ~ treatment, data = dt, vcov = "iid")
summary(reg)
