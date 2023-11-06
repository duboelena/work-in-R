data {
  int<lower=0> N;         // Sample size
  vector[N] x;            // X variables
  //int<lower=0, upper=1> y;// Y outcome, between 1 and 0 
  array[N] int<lower=0, upper=1> y;            // Vector of outcomes
}

parameters {
  real alpha;
  real beta;           // Coefficient vector
}

model {
  beta ~ normal(0, 1);     // do i need these for logistic distribution?
  alpha ~ normal(0, 1);     // With sigma bounded at 0, this is half-cauchy
  for (n in 1:N) {
    y[n] ~ bernoulli_logit(alpha + beta*x[n]);
  }
}
