
source("data_cleaned.R",print.eval = T)

###The design Matrix

X<- df_final_eda[, c("sex", "educ", "internetaccess", "age","income_quintile")] 
X <- model.matrix(~ sex + educ + internetaccess + age + income_quintile+ sex:educ + sex:internetaccess + sex:age+sex:income_quintile, data = X)

###Creating the data list
stan_data_2 <- list(
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


###Running the model
stan_model <- stan_model(file = "model_2.stan")
fit_2 <- sampling(stan_model, data = stan_data_2, chains = 4,iter=500,cores=4,seed=243)

# Save the fitted model
saveRDS(fit_2, "fit_2.rds")
