data {
  int<lower=0> N; // Number observations
  int<lower=0> K; // Number predictors
  matrix[N, K] X; // Design matrix (doesn't include intercept)
  int y[N]; // Binomial response 
  int M[N]; // Number of Bernoulli trials associated with each observation
  real scale_alpha; // Std dev for normal prior on alpha
  vector[K] scale_beta; // Vector of std devs for multivariate normal prior on beta
}

parameters {
  real alpha; 
  vector[K] beta; 
}

transformed parameters {
  vector[N] eta; 
  eta = alpha + X * beta; 
}

model {
  alpha ~ normal(0., scale_alpha);
  beta ~ normal(0., scale_beta); 
  for(i in 1:N)
    y[i] ~ binomial_logit(M[i], eta[i]); 
}


