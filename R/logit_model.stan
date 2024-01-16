data {
  int<lower=0> N;          // Number of observations
  int<lower=0> K;          // Number of predictors
  matrix[N, K] X;          // Predictor matrix
  int<lower=0, upper=1> y[N]; // Binary outcome
}

parameters {
  vector[K] beta;          // Coefficients for predictors
}

model {
  // Priors
  beta ~ normal(0, 5);

  // Likelihood
  y ~ bernoulli_logit(X * beta);
}
generated quantities {
  int y_rep[N];
  for (n in 1:N) y_rep[n] = bernoulli_logit_rng(X[n, ] * beta);
}
