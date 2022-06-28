data {
  int<lower=0> N; 
  int<lower=0, upper=1> Y[N];  
}

parameters {
  real<lower=0, upper=1> theta; 
}

model {
  theta ~ normal(.5, .1);
  for(i in 1:N)
    Y[i] ~ bernoulli(theta); 
}


