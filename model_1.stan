data {
  int<lower=1> N;                         // Total number of observations
  int<lower=1> J;                         // Number of countries
  int<lower=1, upper=J> country[N];       // Country index for each observation
  matrix[N, 13] X;                        // Covariate matrix
  int<lower=0,upper=1> y[N];              // Binary outcome
  vector[J] gdp_pc;                       // Log-GDP per capita for each country
  vector[J] atm_density;                  // Log-ATM density for each country
  vector[J] bank_branch_density;          // Log-bank branch density for each country
  vector[J] unemployment_rate;            // Log-unemployment rate for each country
}

parameters {
  vector[13] beta;                        // Fixed effects coefficients for predictors
  vector[J] alpha;                        // Random intercepts for each country
  real gamma0;                            // Intercept for fixed effects
  real <lower=0> gamma_gdp;               // Effect of GDP per capita on outcome
  real <lower=0> gamma_atm;               // Effect of ATM density on outcome
  real <lower=0> gamma_bank;              // Effect of bank branch density on outcome
  real <upper=0> gamma_unemployment;      // Effect of unemployment rate on outcome
  real<lower=0> sigma_hat;                // Variance between countries
}

transformed parameters {
  vector[J] alpha_hat;
  for (j in 1:J) {
    alpha_hat[j] = gamma0 +
                   gamma_gdp * gdp_pc[j] +
                   gamma_atm * atm_density[j] +
                   gamma_bank * bank_branch_density[j] +
                   gamma_unemployment * unemployment_rate[j];
  }
}

model {
  alpha ~ normal(alpha_hat, sigma_hat);
  beta ~ normal(0, 1);
  gamma0 ~ normal(0, 1);
  gamma_gdp ~ normal(0, 1);
  gamma_atm ~ normal(0, 1);
  gamma_bank ~ normal(0, 1);
  gamma_unemployment ~ normal(0, 1);
  sigma_hat ~ normal(0, 1);

  // Likelihood
  y ~ bernoulli_logit(alpha[country] + X * beta);
}

generated quantities {
  vector[N] log_lik;  // Pointwise log-likelihood for LOO
  vector[N] y_rep;     // Replications from posterior predictive dist

  for (n in 1:N) {
    real logit_p = alpha[country[n]] + dot_product(X[n], beta);

    // Generate replication from the posterior predictive distribution for binary outcome
    y_rep[n] = bernoulli_logit_rng(logit_p);

    // Calculate pointwise log-likelihood for LOO
    log_lik[n] = bernoulli_logit_lpmf(y[n] | logit_p);
  }
}
