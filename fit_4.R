source("data_cleaned.R")
X<- df_final_eda[, c("sex", "educ", "internetaccess", "age","income_quintile")]
X<- model.matrix(~ ., data = X)

stan_data_4<- list(
  N = nrow(df_final_eda),
  J=length(unique(df_final_eda$country)),
  country = as.integer(factor(df_final_eda$country)),
  y = df_final_eda$account, 
  X =X,
  atm_density=as.vector(country_level$atm_density),          
  gdp_pc= as.vector(country_level$r_gdp_per_capita),                 
  unemployment_rate=as.vector(country_level$unemployment_rate), 
  bank_branch_density= as.vector(country_level$bank_branch_density )
)

stan_model <- stan_model(file = "model_4.stan")
fit_4 <- sampling(stan_model, data = stan_data_4, chains = 4,iter=400,cores=4,seed=243)

# Save the fitted model
saveRDS(fit_4, "fit_4.rds")
