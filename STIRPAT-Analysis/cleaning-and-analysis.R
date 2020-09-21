library(dplyr)
library(readr)
library(tidyverse)
library(readxl)
rm(list = ls())
setwd("workingdirectory")

# data cleaning
#dependent: per capita co2
co2 = read_csv("co2.csv", skip = 3)

co2_long = select(co2, `Country Name`, "2007", "2008", "2009", "2010", "2011",
                  "2012", "2013", "2014", "2015", "2016") %>% 
  pivot_longer(cols = matches("^20"), names_to = "year") %>% 
  rename(country = `Country Name`, co2 = value)

pop = read_csv("pop_total.csv", skip = 3)

pop_long = select(pop, `Country Name`, "2007", "2008", "2009", "2010", "2011",
                  "2012", "2013", "2014", "2015", "2016") %>% 
  pivot_longer(cols = matches("^20"), names_to = "year") %>% 
  rename(country = `Country Name`, pop = value)

#independent 1: gdp per cap
gdppc = read_csv("gdp_pc.csv", skip = 3)

gdppc_long = select(gdppc, `Country Name`, "2007", "2008", "2009", "2010", "2011",
                  "2012", "2013", "2014", "2015", "2016") %>% 
  pivot_longer(cols = matches("^20"), names_to = "year") %>% 
  rename(country = `Country Name`, gdppc = value)

#independent 2: urban pop
urbanpop = read_csv("urban_pop.csv", skip = 3)

urbanpop_long = select(urbanpop, `Country Name`, "2007", "2008", "2009", "2010", "2011",
                    "2012", "2013", "2014", "2015", "2016") %>% 
  pivot_longer(cols = matches("^20"), names_to = "year") %>% 
  rename(country = `Country Name`, urbanpop = value)

#independent 3: industrialization level
industry = read_csv("industrialization.csv", skip = 3)

industry_long = select(industry, `Country Name`, "2007", "2008", "2009", "2010", "2011",
                       "2012", "2013", "2014", "2015", "2016") %>% 
  pivot_longer(cols = matches("^20"), names_to = "year") %>% 
  rename(country = `Country Name`, industry = value)
industry_long$year = as.numeric(industry_long$year)

#independent 4: economic freedom
econfree_2007 = read_csv("economic free_2007.csv") %>% 
  select(Name, `Index Year`, `Overall Score`) %>% 
  rename(country = Name, year = `Index Year`, econfree = `Overall Score`)

econfree_2008 = read_csv("economic free_2008.csv") %>% 
  select(Name, `Index Year`, `Overall Score`) %>% 
  rename(country = Name, year = `Index Year`, econfree = `Overall Score`)

econfree_2009 = read_csv("economic free_2009.csv") %>% 
  select(Name, `Index Year`, `Overall Score`) %>% 
  rename(country = Name, year = `Index Year`, econfree = `Overall Score`)

econfree_2010 = read_csv("economic free_2010.csv") %>% 
  select(Name, `Index Year`, `Overall Score`) %>% 
  rename(country = Name, year = `Index Year`, econfree = `Overall Score`)

econfree_2011 = read_csv("economic free_2011.csv") %>% 
  select(Name, `Index Year`, `Overall Score`) %>% 
  rename(country = Name, year = `Index Year`, econfree = `Overall Score`)

econfree_2012 = read_csv("economic free_2012.csv") %>% 
  select(Name, `Index Year`, `Overall Score`) %>% 
  rename(country = Name, year = `Index Year`, econfree = `Overall Score`)

econfree_2013 = read_csv("economic free_2013.csv") %>% 
  select(Name, `Index Year`, `Overall Score`) %>% 
  rename(country = Name, year = `Index Year`, econfree = `Overall Score`)

econfree_2014 = read_csv("economic free_2014.csv") %>% 
  select(Name, `Index Year`, `Overall Score`) %>% 
  rename(country = Name, year = `Index Year`, econfree = `Overall Score`)

econfree_2015 = read_csv("economic free_2015.csv") %>% 
  select(Name, `Index Year`, `Overall Score`) %>% 
  rename(country = Name, year = `Index Year`, econfree = `Overall Score`)

econfree_2016 = read_csv("economic free_2016.csv") %>% 
  select(Name, `Index Year`, `Overall Score`) %>% 
  rename(country = Name, year = `Index Year`, econfree = `Overall Score`)

econfree = rbind(econfree_2007, econfree_2008, econfree_2009, econfree_2010, econfree_2011,
                 econfree_2012, econfree_2013, econfree_2014, econfree_2015, econfree_2016)
econfree$econfree[econfree$econfree == "N/A"] = NA

#independent 5: political freedom
polifree = read_excel("polifree.xlsx", sheet = "FIW06-20") %>%
  select(`Country/Territory`, Edition, Total) %>% 
  rename(country = `Country/Territory`, year = Edition, polifree = Total)

# combining dataframe
join2 = left_join(co2_long, pop_long, by = c("country", "year"))
join3 = left_join(join2, gdppc_long, by = c("country", "year"))
join4 = left_join(join3, urbanpop_long, by = c("country", "year"))
join4$year = as.numeric(join4$year)
join5 = left_join(join4, econfree, by = c("country", "year"))
join6 = left_join(join5, polifree, by = c("country", "year"))
join6$econfree = as.numeric(join6$econfree)
join7 = left_join(join6, industry_long, by = c("country", "year"))
join7$popk = join7$pop / 1000
join7$co2pc = join7$co2 * 1000 / join7$pop

# organizing data
join7[,3:11] = log(join7[,3:11]+1)

df_annexIcomply = filter(join7, country %in% c("Australia", "Austria", "Belgium", "Bulgaria",
                                               "Croatia", "Czech Republic", "Denmark", "Estonia",
                                               "Finland", "France", "Germany", "Greece", "Hungary",
                                               "Iceland", "Ireland", "Italy", "Japan", "Latvia", 
                                               "Liechtenstein", "Lithuania", "Luxembourg", "Monaco",
                                               "Netherlands", "New Zealand", "Norway", "Poland",
                                               "Portugal", "Romania", "Russian Federation",
                                               "Slovak Republic", "Slovenia", "Spain", "Sweden",
                                               "Switzerland", "Ukraine", "United Kingdom"))
df_non_annexIcomply = filter(join7, country %in% c("Belarus", "Canada", "Cyprus", "Malta", 
                                                   "Turkey", "United States", "Afghanistan",
                                                   "Albania", "Algeria", "Andorra", "Angola",
                                                   "Antigua and Barbuda", "Argentina", "Armenia",
                                                   "Azerbaijan", "Bahamas, The", "Bahrain", 
                                                   "Bangladesh", "Barbados", "Belize", "Benin",
                                                   "Bhutan", "Bolivia", "Bosnia and Herzegovina",
                                                   "Botswana", "Brazil", "Brunei Darussalam", 
                                                   "Burkina Faso", "Burundi", "Cabo Verde",
                                                   "Cambodia", "Cameroon", "Central African Republic",
                                                   "Chad", "Chile", "China", "Colombia", "Comoros",
                                                   "Congo, Rep.", "Costa Rica", "Cote d'Ivoire",
                                                   "Cuba", "Congo, Dem. Rep.", "Korea, Dem. People's Rep.",
                                                   "Djibouti", "Dominica", "Dominican Republic",
                                                   "Ecuador", "Egypt, Arab Rep.", "El Salvador",
                                                   "Equatorial Guinea", "Eritrea", "Eswatini",
                                                   "Ethiopia", "Fiji", "Gabon", "Gambia, The", "Georgia",
                                                   "Ghana", "Grenada", "Guatemala", "Guinea",
                                                   "Guinea-Bissau", "Guyana", "Haiti", "Honduras",
                                                   "India", "Indonesia", "Iran, Islamic Rep.",
                                                   "Iraq", "Israel", "Jamaica", "Jordan", 
                                                   "Kazakhstan", "Kenya", "Kiribati", "Kuwait",
                                                   "Kyrgyz Republic", "Lao PDR", "Lebanon",
                                                   "Lesotho", "Liberia", "Madagascar", "Malawi",
                                                   "Malaysia", "Maldives", "Mali", "Marshall Islands",
                                                   "Mauritania", "Mauritius", "Mexico", 
                                                   "Micronesia, Fed. Sts.", "Mongolia", "Montenegro",
                                                   "Morocco", "Mozambique", "Myanmar", "Namibia",
                                                   "Nauru", "Nepal", "Nicaragua", "Niger", "Nigeria",
                                                   "Oman", "Pakistan", "Palau", "Panama", "Papua New Guinea", 
                                                   "Paraguay", "Peru", "Philippines", "Qatar", "Korea, Rep.", 
                                                   "Moldova", "Rwanda", "Samoa", "San Marino",
                                                   "Sao Tome and Principe", "Saudi Arabia", "Senegal",
                                                   "Serbia", "Seychelles", "Sierra Leone", "Singapore",
                                                   "Solomon Islands", "Somalia", "South Africa", "South Sudan",
                                                   "Sri Lanka", "Sudan", "Suriname", "Syrian Arab Republic",
                                                   "Tajikistan", "Thailand", "North Macedonia", "Timor-Leste",
                                                   "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", 
                                                   "Turkmenistan", "Tuvalu", "Uganda", "United Arab Emirates",
                                                   "Tanzania", "Uruguay", "Uzbekistan", "Vanuatu",
                                                   "Venezuela, RB", "Vietnam", "Yemen, Rep.", "Zambia",
                                                   "Zimbabwe"))
df_commit = na.omit(df_annexIcomply)
df_commit$commit = "KPAIC"
df_non_commit = na.omit(df_non_annexIcomply)
df_non_commit$commit = "non-KPAIC"
df_total = rbind(df_commit, df_non_commit)

# summary statistics
summary(df_commit)
sd(df_commit$gdppc, na.rm = TRUE)
sd(df_commit$co2pc, na.rm = TRUE)
sd(df_commit$popk, na.rm = TRUE)
sd(df_commit$urbanpop, na.rm = TRUE)
sd(df_commit$industry, na.rm = TRUE)
sd(df_commit$econfree, na.rm = TRUE)
sd(df_commit$polifree, na.rm = TRUE)

summary(df_non_commit)
sd(df_non_commit$gdppc, na.rm = TRUE)
sd(df_non_commit$co2pc, na.rm = TRUE)
sd(df_non_commit$popk, na.rm = TRUE)
sd(df_non_commit$urbanpop, na.rm = TRUE)
sd(df_non_commit$industry, na.rm = TRUE)
sd(df_non_commit$econfree, na.rm = TRUE)
sd(df_non_commit$polifree, na.rm = TRUE)

# diagnostics
library(jtools)
library(plm)
library(stargazer)
library(lmtest)
library(car)

#model 1
#test for entity fixed effect
ols1 = lm(co2pc ~ gdppc + industry + popk + urbanpop, data = df_commit)

mod1_entity = plm(co2pc ~ gdppc + industry + popk + urbanpop,   
                  data = df_commit,
                  index = c("country"),
                  model = "within")

pFtest(mod1_entity, ols1) 

#test for time fixed effect
mod1_both = plm(co2pc ~ gdppc + industry + popk + urbanpop + factor(year), 
                data = df_commit,
                index = c("country"),
                model = "within")

pFtest(mod1_both, mod1_entity) 

#test for random vs fixed effects
mod1 = plm(co2pc ~ gdppc + industry + popk + urbanpop, 
           data = df_commit,
           index = c("country", "year"),
           model = "within")

mod1_random = plm(co2pc ~ gdppc + industry + popk + urbanpop, 
           data = df_commit,
           index = c("country", "year"),
           model = "random")

phtest(mod1, mod1_random)

#vif test
mod1_pool = plm(co2pc ~ gdppc + industry + popk + urbanpop, 
                  data = df_commit,
                  index = c("country", "year"),
                  model = "pooling")
vif(mod1_pool)

#model 2
#test for entity fixed effect
ols2 = lm(co2pc ~ gdppc  + industry + popk + urbanpop + econfree + polifree, data = df_commit)

mod2_entity = plm(co2pc ~ gdppc + industry + popk + urbanpop + econfree + polifree,   
                  data = df_commit,
                  index = c("country"),
                  model = "within")

pFtest(mod2_entity, ols2)

#test for time fixed effect
mod2_both = plm(co2pc ~ gdppc + industry + popk + urbanpop + econfree + polifree + factor(year), 
                data = df_commit,
                index = c("country"),
                model = "within")

pFtest(mod2_both, mod2_entity)

#test for random vs fixed effects
mod2 = plm(co2pc ~ gdppc + industry + popk + urbanpop + econfree + polifree, 
           data = df_commit,
           index = c("country", "year"),
           model = "within")

mod2_random = plm(co2pc ~ gdppc + industry + popk + urbanpop + econfree + polifree, 
                 data = df_commit,
                 index = c("country", "year"),
                 model = "random")

phtest(mod2, mod2_random)

#vif test
mod2_pool = plm(co2pc ~ gdppc + industry + popk + urbanpop + econfree + polifree, 
                  data = df_commit,
                  index = c("country", "year"),
                  model = "pooling")
vif(mod2_pool)

#model 3
#test for entity fixed effects
ols3 = lm(co2pc ~ gdppc + industry + popk + urbanpop, data = df_non_commit)

mod3_entity = plm(co2pc ~ gdppc + industry + popk + urbanpop,   
                  data = df_non_commit,
                  index = c("country"),
                  model = "within")

pFtest(mod3_entity, ols3)

#test for time fixed effects
mod3_both = plm(co2pc ~ gdppc + industry + popk + urbanpop + factor(year), 
                data = df_non_commit,
                index = c("country"),
                model = "within")

pFtest(mod3_both, mod3_entity)

#test for random vs fixed effects
mod3 = plm(co2pc ~ gdppc + industry + popk + urbanpop,   
           data = df_non_commit,
           index = c("country"),
           model = "within")

mod3_random = plm(co2pc ~ gdppc + industry + popk + urbanpop, 
                data = df_non_commit,
                index = c("country"),
                model = "random")

phtest(mod3, mod3_random)

#vif test
mod3_pool = plm(co2pc ~ gdppc + industry + popk + urbanpop, 
                  data = df_non_commit,
                  index = c("country", "year"),
                  model = "pooling")
vif(mod3_pool)

#model 4
#test for entity fixed effects
ols4 = lm(co2pc ~ gdppc + industry + popk + urbanpop + econfree + polifree, data = df_non_commit)

mod4_entity = plm(co2pc ~ gdppc + industry + popk + urbanpop + econfree + polifree,   
                  data = df_non_commit,
                  index = c("country"),
                  model = "within")

pFtest(mod4_entity, ols4)

#test for time fixed effects
mod4_both = plm(co2pc ~ gdppc + industry + popk + urbanpop + econfree + polifree + factor(year), 
                data = df_non_commit,
                index = c("country"),
                model = "within")

pFtest(mod4_both, mod4_entity)

#test for random vs fixed effects
mod4 = plm(co2pc ~ gdppc + industry + popk + urbanpop + econfree + polifree,   
           data = df_non_commit,
           index = c("country"),
           model = "within")

mod4_random = plm(co2pc ~ gdppc + industry + popk + urbanpop + econfree + polifree,   
                  data = df_non_commit,
                  index = c("country"),
                  model = "random")

phtest(mod4, mod4_random)

#vif test
mod4_pool = plm(co2pc ~ gdppc + industry + popk + urbanpop + econfree + polifree,   
                  data = df_non_commit,
                  index = c("country", "year"),
                  model = "pooling")
vif(mod4_pool)

# model output
rob_se1 = list(sqrt(diag(vcovHC(mod1, type = "HC1"))),
               sqrt(diag(vcovHC(mod2, type = "HC1"))))

stargazer(mod1, mod2, 
          digits = 3,
          header = FALSE,
          type = "text", 
          se = rob_se1,
          title = "Title",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)"))

rob_se2 = list(sqrt(diag(vcovHC(mod3, type = "HC1"))),
               sqrt(diag(vcovHC(mod4, type = "HC1"))))

stargazer(mod3, mod4, 
          digits = 3,
          header = FALSE,
          type = "text", 
          se = rob_se2,
          title = "Title",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)"))
          
#visualization
library(ggplot2)

co2pc = ggplot(df_total, aes(x = co2pc, color = commit, fill = commit)) +                      
  geom_histogram(position = "identity", alpha = 0.3, binwidth = 0.1) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") + 
  labs(fill = "Country Group", color = "Country Group") +
  xlab("ln(co2pc)") + ylab("Count") +
  ggtitle("Per-capita CO2 Emission (in log-scale)") +
  theme_gray(base_family="Times New Roman")

co2pc_cdf = ggplot(df_total, aes(x = co2pc, color = commit)) +
  stat_ecdf() +
  scale_color_brewer(palette = "Dark2") + 
  labs(fill = "Country Group", color = "Country Group") +
  xlab("ln(co2pc)") +
  ylab("") +
  ggtitle("Per-capita CO2 Emission (in log-scale)")

pop = ggplot(df_total, aes(x = popk, color = commit, fill = commit)) +                      
  geom_histogram(position = "identity", alpha = 0.3, binwidth = 0.2) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") + 
  labs(fill = "Country Group", color = "Country Group") +
  xlab("ln(popk)") + ylab("Count") +
  ggtitle("Population (in log-scale)")

pop_cdf = ggplot(df_total, aes(x = popk, color = commit)) +
  stat_ecdf() +
  scale_color_brewer(palette = "Dark2") +
  xlab("ln(popk)") +
  ylab("") +
  ggtitle("Population (in log-scale)")

gdppc = ggplot(df_total, aes(x = gdppc, color = commit, fill = commit)) +                      
  geom_histogram(position = "identity", alpha = 0.3, binwidth = 0.15) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") + 
  labs(fill = "Country Group", color = "Country Group") +
  xlab("ln(gdppc)") + ylab("Count") +
  ggtitle("Per-capita GDP (in log-scale)")

gdppc_cdf = ggplot(df_total, aes(x = gdppc, color = commit)) +
  stat_ecdf() +
  scale_color_brewer(palette = "Dark2") +
  labs(fill = "Country Group", color = "Country Group") +
  xlab("ln(gdppc)") +
  ylab("") +
  ggtitle("Per-capita GDP (in log-scale)")

urban = ggplot(df_total, aes(x = urbanpop, color = commit, fill = commit)) +                      
  geom_histogram(position = "identity", alpha = 0.3, binwidth = 0.05) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") + 
  labs(fill = "Country Group", color = "Country Group") +
  xlab("ln(urbanpop)") + ylab("Count") +
  ggtitle("Urbanization Rate (in log-scale)")

urban_cdf = ggplot(df_total, aes(x = urbanpop, color = commit)) +
  stat_ecdf() +
  scale_color_brewer(palette = "Dark2") +
  xlab("ln(urbanpop)") +
  ylab("") +
  ggtitle("Urbanization Rate (in log-scale)")

industry = ggplot(df_total, aes(x = industry, color = commit, fill = commit)) +                      
  geom_histogram(position = "identity", alpha = 0.3, binwidth = 0.05) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") + 
  labs(fill = "Country Group", color = "Country Group") +
  xlab("ln(industry)") + ylab("Count") +
  ggtitle("Industrialization Rate (in log-scale)")

industry_cdf = ggplot(df_total, aes(x = industry, color = commit)) +
  stat_ecdf() +
  scale_color_brewer(palette = "Dark2") +
  labs(fill = "Country Group", color = "Country Group") +
  xlab("ln(industry)") +
  ylab("") +
  ggtitle("Industrialization Rate (in log-scale)")

econfree = ggplot(df_total, aes(x = econfree, color = commit, fill = commit)) +                      
  geom_histogram(position = "identity", alpha = 0.3, binwidth = 0.03) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") + 
  labs(fill = "Country Group", color = "Country Group") +
  xlab("ln(econfree)") + ylab("Count") +
  ggtitle("Economic Freedom (in log-scale)")

econfree_cdf = ggplot(df_total, aes(x = econfree, color = commit)) +
  stat_ecdf() +
  scale_color_brewer(palette = "Dark2") +
  xlab("ln(econfree)") +
  ylab("") +
  ggtitle("Economic Freedom (in log-scale)")

polifree = ggplot(df_total, aes(x = polifree, color = commit, fill = commit)) +                      
  geom_histogram(position = "identity", alpha = 0.3, binwidth = 0.08) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") + 
  labs(fill = "Country Group", color = "Country Group") +
  xlab("ln(polifree)") + ylab("Count") +
  ggtitle("Political Freedom (in log-scale)")

polifree_cdf = ggplot(df_total, aes(x = polifree, color = commit)) +
  stat_ecdf() +
  scale_color_brewer(palette = "Dark2") +
  xlab("ln(polifree)") +
  ylab("") +
  ggtitle("Political Freedom (in log-scale)")
