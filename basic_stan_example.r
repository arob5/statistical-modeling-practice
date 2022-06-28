# basic_stan_example.r
# Basic binomial sampling model to make sure Stan is running correctly. 
#
# Andrew Roberts
# Working Directory: statistical-modeling-practice

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

set.seed(12)

# Generate Binomial data
N <- 100
y <- rbinom(N, 1, .6)

# Compile Model
model <- stan_model('basic_stan_example.stan')
fit <- sampling(model, list(N = N, Y=y), iter = 200, chains = 4)

# Fit summary
print(fit)

# Plot histogram of samples
params <- extract(fit)
hist(params$theta)



