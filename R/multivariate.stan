data {
  int<lower=1> K;
  int<lower=1> J;
  int<lower=0> N;
  vector[J] x[N];
  vector[K] y[N];
}
parameters {
  matrix[K, J] beta;
  cholesky_factor_corr[K] L_Omega;
  vector<lower=0>[K] L_sigma;
}

transformed parameters {
  array[N] vector[K] mu;
  matrix[K, K] L_Sigma;

  for (n in 1:N) {
    mu[n] = beta * x[n];

  }

  L_Sigma = diag_pre_multiply(L_sigma, L_Omega);
}
model {
  to_vector(beta) ~ normal(0, 5);
  L_Omega ~ lkj_corr_cholesky(4);
  L_sigma ~ cauchy(0, 2.5);

  y ~ multi_normal_cholesky(mu, L_Sigma);
}

generated quantities {
  vector[K] ypred[N];
  for (n in 1:N)
    ypred[n] = multi_normal_cholesky_rng(mu[n, ], L_Sigma);
}