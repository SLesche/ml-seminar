data {
  int<lower=1> N; // Sample size
  int<lower=0> ncat; // Number of categories
  vector[N] y; // Pitch type data
  int<lower=0> npred; // Number of predictor variables
  matrix[N, npred] x; // Predictor variable matrix
}

parameters {
  matrix[npred, ncat] beta; // Weights in multinomial regression
}

model {
  matrix[N, ncat] x_beta = x * beta; //predicted matrix

  to_vector(beta) ~ normal(0, 5); // priors

  for (n in 1:N)
    y[n] ~ categorical_logit(x_beta[n]');
}

