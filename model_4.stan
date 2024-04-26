data {
  int<lower=1> N;                          // Total number of observations
  int<lower=1> J;                          // Number of countries
  int<lower=1, upper=J> country[N];        // Country index for each observation
  matrix[N, 13] X;                         // Covariate matrix
  int<lower=0, upper=1> y[N];              // Binary outcome
  vector[J] gdp_pc;                        // Log-GDP per capita for each country
  vector[J] atm_density;                   // Log-ATM density for each country
  vector[J] bank_branch_density;           // Log-bank branch density for each country
  vector[J] unemployment_rate;             // Log-unemployment rate for each country
}

parameters {
  matrix[13, J] beta_raw;                  // Random effects coefficients for predictors
  vector[J] alpha;                         // Random intercepts for each country
  real gamma0;                             // Intercept for fixed effects
  real<lower=0> sigma_alpha;               // Variance of random intercepts
  real<lower=0> sigma_beta;                // Variance of random slopes for beta
  vector<lower=0>[J] gamma_gdp;            // Random slopes for GDP per capita
  vector<lower=0>[J] gamma_atm;            // Random slopes for ATM density
  vector<lower=0>[J] gamma_bank;           // Random slopes for bank branch density
  vector<upper=0>[J] gamma_unemployment;   // Random slopes for unemployment rate
}

transformed parameters {
  matrix[13, J] beta;
  for (j in 1:J) {
    beta[, j] = beta_raw[, j];
  }
}

model {
  alpha ~ normal(0, sigma_alpha);          // Prior for random intercepts
  gamma_gdp ~ normal(0, sigma_beta);        // Prior for random slopes for GDP per capita
  gamma_atm ~  normal(0, sigma_beta);       // Prior for random slopes for ATM density
  gamma_bank ~ normal(0, sigma_beta);       // Prior for random slopes for bank branch density
  gamma_unemployment ~ normal(0, sigma_beta); // Prior for random slopes for unemployment rate
  
  for (i in 1:13) {
    for (j in 1:J) {
      beta[i, j] ~ normal(0, sigma_beta);   // Prior for random slopes for beta
    }
  }

  // Priors for fixed effects
  gamma0 ~ normal(0, 1);
  sigma_alpha ~ normal(0, 1);
  sigma_beta ~ normal(0, 1);

  // Likelihood
  for (n in 1:N) {
    real logit_p = alpha[country[n]] + dot_product(X[n], beta[, country[n]]) +
                   gamma_gdp[country[n]] * gdp_pc[country[n]] +
                   gamma_atm[country[n]] * atm_density[country[n]] +
                   gamma_bank[country[n]] * bank_branch_density[country[n]] +
                   gamma_unemployment[country[n]] * unemployment_rate[country[n]];
    
    y[n] ~ bernoulli_logit(logit_p);
  }
}

generated quantities {
  vector[N] log_lik;  // Pointwise log-likelihood for LOO
  vector[N] y_rep;     // Replications from posterior predictive dist

  for (n in 1:N) {
    real logit_p = alpha[country[n]] + dot_product(X[n], beta[, country[n]]) +
                   gamma_gdp[country[n]] * gdp_pc[country[n]] +
                   gamma_atm[country[n]] * atm_density[country[n]] +
                   gamma_bank[country[n]] * bank_branch_density[country[n]] +
                   gamma_unemployment[country[n]] * unemployment_rate[country[n]];

    y_rep[n] = bernoulli_logit_rng(logit_p);
    log_lik[n] = bernoulli_logit_lpmf(y[n] | logit_p);
  }
}
