##Data Cleaning
#Load the data 
#Note: all data were downloaded from World Bank's database. 
library(readr)
gov_rev = read_csv("file_directory.csv", skip = 3)
gov_exp = read_csv("file_directory.csv", skip = 3)
gdp = read_csv("file_directory.csv", skip = 3)
gdp_pc = read_csv("file_directory.csv", skip = 3)
savings = read_csv("file_directory.csv", skip = 3)
tot = read_csv("file_directory.csv", skip = 3)
unemployment = read_csv("file_directory.csv", skip = 3)

#Clean & organize the data
library(dplyr)
library(tidyverse)
gov_rev_clean = select(gov_rev, `Country Name`, "2012", "2017", "2018") %>%
  rename(country_name = `Country Name`)
gov_rev_long = pivot_longer(gov_rev_clean,
                            cols = matches("^20"),
                            names_to = "year") %>% rename(gov_rev = value)

gov_exp_clean = select(gov_exp, `Country Name`, "2012", "2017") %>%
  rename(country_name = `Country Name`)
gov_exp_long = pivot_longer(gov_exp_clean,
                            cols = matches("^20"),
                            names_to = "year") %>% rename(gov_exp = value)

gdp_clean = select(gdp, `Country Name`, "2012", "2017") %>%
  rename(country_name = `Country Name`)
gdp_long = pivot_longer(gdp_clean,
                        cols = matches("^20"),
                        names_to = "year") %>% rename(gdp_lcu = value)

gdp_pc_clean = select(gdp_pc, `Country Name`, "2012", "2017") %>%
  rename(country_name = `Country Name`)
gdp_pc_long = pivot_longer(gdp_pc_clean,
                        cols = matches("^20"),
                        names_to = "year") %>% rename(gdp_pc = value)

savings_clean = select(savings, `Country Name`, "2012", "2017") %>%
  rename(country_name = `Country Name`)
savings_long = pivot_longer(savings_clean,
                            cols = matches("^20"),
                            names_to = "year") %>% 
  rename(gross_savings = value)

tot_clean = select(tot, `Country Name`, "2012", "2017") %>%
  rename(country_name = `Country Name`)
tot_long = pivot_longer(tot_clean,
                        cols = matches("^20"),
                        names_to = "year") %>% rename(tot = value)

unemployment_clean = select(unemployment, `Country Name`, 
                            "2012", "2017") %>%
  rename(country_name = `Country Name`)
unemployment_long = pivot_longer(unemployment_clean,
                                cols = matches("^20"),
                                names_to = "year") %>% 
  rename(unemployment = value)

#Merge all variables
library(plyr)
df_v1 = join_all(list(gov_exp_long,
              gov_rev_long,
              gdp_long,
              gdp_pc_long,
              savings_long,
              tot_long,
              unemployment_long), 
              by=c("country_name", "year"), type='left')

#Clean & organize the data
df_v2 = filter(df_v1, 
               !(country_name %in% 
                   c("Arab World", "Central Europe and the Baltics",
                    "Caribbean small states", 
                    "East Asia & Pacific (excluding high income)",
                    "Early-demographic dividend", 
                    "East Asia & Pacific", 
                    "Europe & Central Asia (excluding high income)", 
                    "Europe & Central Asia",
                    "Euro area", "European Union", 
                    "Fragile and conflict affected situations",
                    "High income",
                    "Heavily indebted poor countries (HIPC)",
                    "IBRD only", "IDA & IBRD total", 
                    "IDA total", "IDA blend", "IDA only", 
                    "Not classified", 
                    "Latin America & Caribbean (excluding high income)",
                    "Latin America & Caribbean", 
                    "Least developed countries: UN classification",
                    "Low income", "Lower middle income", 
                    "Low & middle income", "Late-demographic dividend", 
                    "Middle East & North Africa", "Middle income", 
                    "Middle East & North Africa (excluding high income)",
                    "OECD members", "Other small states", 
                    "Pre-demographic dividend", 
                    "Pacific island small states", 
                    "Post-demographic dividend", "South Asia",
                    "Sub-Saharan Africa (excluding high income)", 
                    "Sub-Saharan Africa", "Small states", 
                    "East Asia & Pacific (IDA & IBRD countries)",
                    "Europe & Central Asia (IDA & IBRD countries)", 
                    "Latin America & the Caribbean (IDA & IBRD countries)",
                    "Middle East & North Africa (IDA & IBRD countries)", 
                    "South Asia (IDA & IBRD)",
                    "Sub-Saharan Africa (IDA & IBRD countries)", 
                    "Upper middle income", "World")))
df_v2$budget_balance = df_v2$gov_rev - df_v2$gov_exp
df_v2$balance_per_gdp = (df_v2$budget_balance/df_v2$gdp_lcu * 100)

#Using log models on variables that have many skewed outliers
df_v2$gdp_pc_log = log(df_v2$gdp_pc)
df_v2$unemployment_log = log(df_v2$unemployment)

df_final = df_v2 %>% 
  mutate(year_2017 = ifelse(year == '2017', 1, 0)) %>% 
  select(country_name, year, balance_per_gdp, gdp_pc_log, gross_savings, 
         tot, unemployment_log, year_2017)

##Descriptive Statistics
#Understanding the data for year 2012 and 2017
df_2017 = df_final %>% filter(year_2017 == 1)
df_2012 = df_final %>% filter(year_2017 == 0)

summary(df_2017)
summary(df_2012)

#Plot boxplot
library(ggplot2)
df_plot = pivot_longer(df_final,
                       cols = matches("^b|^g|^t|^u|^e"),
                       names_to = "variable") %>% 
  select(country_name, variable, value, year)

ggplot(df_plot, aes(x=variable, y=value)) + 
  geom_boxplot(aes(group = year, fill = year), 
               varwidth = TRUE, outlier.shape = 1) + 
  facet_wrap( ~ variable, scales="free") +
  xlab("Explanatory Variable") + ylab("Values") + 
  ggtitle("Diagram 1 Boxplot for All Variables") +
  guides(fill=guide_legend(title="Year")) + 
  theme_bw(base_size = 12)

#Plot distribution for 2017
ggplot(df_plot[df_plot$year == "2017",], aes(x=value)) + 
  geom_histogram(binwidth = 2, fill = "#d90502") +
  xlab("Values") + ylab("Frequency") + 
  ggtitle("Diagram 2 Distribution All Variables in 2017")  + 
  theme_bw(base_size = 12) +
  facet_grid( ~ variable, scales="free")

#Plot distribution for 2012
ggplot(df_plot[df_plot$year == "2012",], aes(x=value)) + 
  geom_histogram(binwidth = 2, fill = "#d90502") +
  xlab("Values") + ylab("Frequency") + 
  ggtitle("Diagram 2 Distribution All Variables in 2012")  + 
  theme_bw(base_size = 12) +
  facet_grid( ~ variable, scales="free")

#Build the regression model
reg1 = lm(balance_per_gdp ~ gdp_pc_log + tot + unemployment_log + 
            year_2017, df_final)
reg2 = lm(balance_per_gdp ~ gdp_pc_log + gross_savings + tot + 
            unemployment_log + year_2017, df_final)
           
# VIF test
library(car)
vif(reg1)
vif(reg2)

#Run the correlation between explanatory variables
library(corrplot)
cor(df_final[,c(3,4,5,6,7)],use="pairwise.complete.obs")
corrplot(cor(df_final[,c(3,4,5,6,7)],use="pairwise.complete.obs"),
         method = "circle", tl.col = "black",
         title = "Diagram 3 Correlation between Explanatory Variables",
         mar=c(0,0,1,0))
         

#Assess the adjusted R squared since the model has quite a few variables
library(jtools)
summary(reg1)$adj.r.squared 
summary(reg2)$adj.r.squared 

#Display the regression result
export_summs(reg1, reg2)
