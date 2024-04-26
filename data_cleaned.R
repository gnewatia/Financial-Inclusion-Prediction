
data_individual<-read_csv("data/data_individual.csv")
data_country<-read_csv("data/data_country.csv")
data_fas<-read_csv("data/FAS.csv")



data_individual<-data_individual|>
  mutate(economy=str_replace(economy,"T�rkiye","Turkey"),economy=str_replace(economy,"Korea, Rep.","Korea"),economy=str_replace(economy,"United Arab Emirates","UAE") )

data_country<-data_country|>
  mutate(`Country Name`=str_replace(`Country Name`,"Turkiye","Turkey"),`Country Name`=str_replace(`Country Name`,"Korea, Rep.","Korea"),`Country Name`=str_replace(`Country Name`,"United Arab Emirates","UAE"))

data_fas<-data_fas|>
  mutate(Country=str_replace(Country,"T�rkiye","Turkey"),Country=str_replace(Country,"China,P.R.: Mainland","China"),Country=str_replace(Country,"United Arab Emirates","UAE"))


emerging<-c("Brazil", "Chile", "China", "Colombia", "Egypt", "Greece", "Hungary", "India", "Indonesia", "Korea", "Malaysia", "Mexico", "Peru", "Philippines", "Poland",
            "Saudi Arabia", "South Africa", "Thailand", "Turkey","UAE")

data_individual<-data_individual|>
  filter(economy %in% emerging)



data_country<-data_country|>
  pivot_wider(id_cols = `Country Name`,names_from = Indicators,values_from =Values)



data_fas<-data_fas|>
  pivot_wider(id_cols = Country,names_from = Indicators,values_from ="2021")



data_individual_final<-data_individual|>
  select(economy,female,age,educ,inc_q,account,internetaccess)





df_final<-left_join(data_individual_final,data_country,by=c("economy"="Country Name"))|>
  left_join(data_fas,by=c("economy"="Country"))


df_final<-df_final|>
  filter(!is.na(age),educ<=3,internetaccess<=2)



df_final<-df_final|>
  mutate(age=cut(age,
                 breaks = c(0, 18, 30, 40, 50, 60, 70,80,Inf),
                 labels = c("Under 18", "19-30", "31-40", "41-50", "51-60", "61-70","71-80","Over 80"),
                 include.lowest = TRUE))





df_final_eda<-df_final|>
  mutate(female=factor(female),internetaccess=factor(internetaccess),account_group=factor(account),educ=as.factor(educ),inc_q=as.factor(inc_q))
colnames(df_final_eda)<-c("country", "sex", "age", "educ" ,"income_quintile", "account","internetaccess","r_gdp_per_capita",    "unemployment_rate","bank_branch_density", "atm_density","account_group")        
df_final_eda$income_quintile<-ifelse((as.integer(df_final_eda$income_quintile))>3,"High Income","Low Income")
levels(df_final_eda$account_group)=c("No Account","Has Account")
levels(df_final_eda$sex)=c("Female","Male")
levels(df_final_eda$educ)=c("Primary School or Less","Secondary School","Tertiary Education or More")
levels(df_final_eda$internetaccess)=c("Internet","No Internet")




df_country_eda<-df_final|>
  group_by(economy)|>
  summarise(Proportion=mean(account))|>
  left_join(data_fas,by=c("economy"="Country"))|>
  arrange(-Proportion)|>
  left_join(data_country,by=c("economy"="Country Name"))|>
  mutate(`Number of commercial bank branches per 1,000 km2`=log(`Number of commercial bank branches per 1,000 km2`),`Number of ATMs per 1,000 km2`=log(`Number of ATMs per 1,000 km2`),`GDP per capita growth (annual %)`=log(`GDP per capita growth (annual %)`),`Unemployment, total (% of total labor force) (modeled ILO estimate)`=log(`Unemployment, total (% of total labor force) (modeled ILO estimate)`))



country_level=left_join(data_country,data_fas,by=c("Country Name"="Country"))



country_level<-country_level|>
  filter(`Country Name`!="Egypt")|>
  select(-`Country Name`)|>
  mutate(across(everything(),~scale(.x,center=T,scale=T)))

colnames(country_level)<-c("r_gdp_per_capita","unemployment_rate","bank_branch_density", "atm_density")





