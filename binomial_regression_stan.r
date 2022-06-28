# binomial_regression_stan.r
# Based on McCullagh and Nelder Generalized Linear Models problem 4.26; comparing Bayesian
# approach in Stan to non-Bayesian approach described in the problem. 
#
# Andrew Roberts
# Working Directory: statistical-modeling-practice

library(data.table)
library(rstan)
library(boot) # For inv.logit()

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Notes:
# Exercise 4.26: Galton's experiment on inheritance of eye-colour.
# Table 4.10: Number of light-eyed children in each of 78 families.
# `plight`, `phazel`, and `pdark` are counts of parents' eye colours
# `gplight`, `gphazel`, and `gpdark` are counts of grandparents' eye colours
# `lighteyed` and `total` are counts of children that are light-eyed and total counts

#
# Read Data and define variables
#

# Read data
dt <- fread(file.path("data", "eyecolour.csv"))

# Create factor P, encoding every possible combination of parent eye color. 
setDT(dt)[, P := .GRP, by = .(plight, phazel, pdark)]
dt[, P := as.factor(P)]

# Create factor G, encoding every possible combination of grandparent eye color. 
setDT(dt)[, G := .GRP, by = .(gplight, gphazel, gpdark)]
dt[, G := as.factor(G)]


#
# Non-Bayesian approach, as in McCullagh and Nelder exercise 4.26.
#

# Regress `lighteyed` on P.
bm1 <- glm(cbind(lighteyed, total - lighteyed) ~ P, data = dt, family = binomial)
summary(bm1)          

# Look at deviance residuals and remove "discrepant" points. 
dt[, residual := bm1$residuals]
hist(dt[, residual], breaks = 20)
dt[residual == min(residual)]       # Easily identified discrepant point
dt <- dt[residual != min(residual)] # Remove observation with residual -10.33021 
dt[, residual := NULL]

# Re-fit model on remaining data. Compute fitted probabilities. 
bm2 <- glm(cbind(lighteyed, total - lighteyed) ~ P, data = dt, family = binomial)
summary(bm2)
dt[, fitted := bm2$fitted.values]
parent_probs <- unique(dt[, .(plight, phazel, pdark, fitted)], by = 'fitted')

#
# Bayesian model in Stan
#

# Create model matrix (expanding out factor into dummy variables)
X <- model.matrix(lighteyed ~ P, data = dt)
X <- X[, 2:ncol(X)] # Removing intercept column; I add this manually in stan file

# Settings to pass to Stan
stan_list <- list(N = nrow(dt), 
                  K = ncol(X), 
                  X = X, 
                  y = dt$lighteyed, 
                  M = dt$total, 
                  scale_alpha = 10, 
                  scale_beta = rep(10, ncol(X)))

# Compile and fit Stan Model
model <- stan_model('binomial_regression_stan.stan')
fit <- sampling(model, stan_list, iter = 2000, chains = 4)

# Fit summary
print(fit)
model_coefs <- as.data.table(head(summary(fit)$summary), keep.rownames = TRUE) # Drops eta estimates; keeps alpha and 5 beta coefs

# Compare to non-Bayesian estimates; results are very similar 
bm2_coefs <- as.data.table(list(rn = names(bm2$coefficients), 
                                mean = bm2$coefficients, 
                                sd = sqrt(diag(vcov(bm2)))))
print(model_coefs[, c('rn', 'mean', 'sd')])
print(bm2_coefs)

# Plot parameter samples
params <- extract(fit)
hist(params$alpha)
hist(inv.logit(params$alpha)) # Distribution of lighteyed probability for base group (both parents lighteyed)
mean(inv.logit(params$alpha))
parent_probs[plight == 2, fitted] # Very similar to prob found by non-Bayesian methods

hist(inv.logit(params$alpha + params$beta[, 2])) # Distribution of lighteyed probability when both parents are darkeyed
mean(inv.logit(params$alpha + params$beta[, 2]))
parent_probs[pdark == 2, fitted] # Very similar to prob found by non-Bayesian methods
