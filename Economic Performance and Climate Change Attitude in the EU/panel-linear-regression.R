library(dplyr)
library(readr)
library(tidyverse)

#filtering necessary variables of the dataset from World Bank
unemployment = read_csv("filepath/Unemployment.csv")
unemployment_clean = select(unemployment, LOCATION, TIME, Value) %>% 
  rename(country = LOCATION,
         unemployment = Value,
         year = TIME)

public_debt = read_csv("filepath/Public Debt per GDP.csv")
public_debt_clean = select(public_debt, LOCATION, TIME, Value) %>%
  rename(country = LOCATION,
         public_debt = Value,
         year = TIME)

inflation = read_csv("filepath/inflation.csv")
inflation_clean = select(inflation, LOCATION, TIME, Value) %>%
  rename(country = LOCATION,
         cpi = Value,
         year = TIME)

gdp_per_cap = read_csv("filepath/GDP per cap, PPP (current  USD)/API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_887206.csv", 
                        skip = 4)
gdp_per_cap_clean = select(gdp_per_cap, `Country Code`, "2009", "2011", 
                           "2013", "2015", "2017") %>% 
  filter(`Country Code` %in% c("AUT", "BEL", "CZE", "DNK", "FIN", "FRA", 
                               "DEU", "GRC", "HUN", "IRL", "ITA", "LUX", 
                               "NLD", "POL", "PRT", "SVK", "ESP", "SWE", 
                               "GBR", "BGR", "HRV", "CYP", "EST", "LVA", 
                               "LTU", "MLT", "ROU", "SVN")) %>%
  rename(country = `Country Code`)
gdp_per_cap_long = pivot_longer(gdp_per_cap_clean,
                                cols = matches("^20"),
                                names_to = "year") %>% rename(gdp_pc = value)
gdp_per_cap_long$year = as.numeric(gdp_per_cap_long$year)

#filtering the data from the Eurobarometer Survey and OECD Database
climate_poll = readxl::read_excel("filepath/EU Wide Data.xlsx") %>%
  rename(country = Country)

F_to_M = readxl::read_excel("filepath/F to M.xlsx", 
                    sheet = "Sheet1")
sex_clean = pivot_longer(F_to_M, 
                         cols = matches("^20"),
                         names_to = "year") %>% rename(f_to_m = value)
sex_clean$year = as.numeric(sex_clean$year)

#joining datasets
join_2 = left_join(gdp_per_cap_long, unemployment_clean)
join_3 = left_join(join_2, gini_clean)
join_4 = left_join(join_3, public_debt_clean)
join_5 = left_join(join_4, sex_clean)
join_6 = left_join(join_5, inflation_clean)
climate_att = left_join(climate_poll, join_6, by = c("country","year"))
climate_att$gdp_pc_k = climate_att$gdp_pc/1000

#cleaning for the final dataset to run our models
climate_att = climate_att %>% filter(year != 2009) %>%
  mutate(year_2017 = ifelse(year == "2017", 1, 0),
         year_2015 = ifelse(year == "2015", 1, 0),
         year_2013 = ifelse(year == "2013", 1, 0)) %>% 
  select(country, year, Q5, Q4, gdp_pc_k, public_debt, unemployment, f_to_m,
        cpi, year_2017, year_2015, year_2013) 

#understanding our data
summary(climate_att, digits = 6)
sd(climate_att$Q5, na.rm = TRUE)
sd(climate_att$Q4, na.rm = TRUE)
sd(climate_att$gdp_pc_k, na.rm = TRUE)
sd(climate_att$public_debt, na.rm = TRUE)
sd(climate_att$unemployment, na.rm = TRUE)
sd(climate_att$f_to_m, na.rm = TRUE)
sd(climate_att$cpi, na.rm = TRUE)

#running the panel linear model
library(jtools)
library(plm)
Q4_1 = plm(Q4 ~ f_to_m + unemployment, 
           data = climate_att,
           index = c("year"), 
           model = "within")
Q4_2 = plm(Q4 ~ f_to_m + unemployment + gdp_pc_k + cpi + public_debt, 
           data = climate_att,
           index = c("year"), 
           model = "within")
Q5_1 = plm(Q5 ~ f_to_m + unemployment, 
           data = climate_att,
           index = c("year"), 
           model = "within")
Q5_2 = plm(Q5 ~ f_to_m + unemployment + gdp_pc_k + cpi + public_debt, 
           data = climate_att,
           index = c("year"), 
           model = "within")

#formatting the display of the panel linear models 
library(stargazer)
rob_se = list(sqrt(diag(vcovHC(Q4_1, type = "HC1"))),
              sqrt(diag(vcovHC(Q4_2, type = "HC1"))),
              sqrt(diag(vcovHC(Q5_1, type = "HC1"))),
              sqrt(diag(vcovHC(Q5_2, type = "HC1"))))

stargazer(Q4_1, Q4_2, Q5_1, Q5_2,
          digits = 3,
          header = FALSE,
          type = "text", 
          se = rob_se,
          title = "Linear Panel Regression Models of Climate Attitude & Action due to Macroeconomic Conditions",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)", "(1)", "(2)"))
