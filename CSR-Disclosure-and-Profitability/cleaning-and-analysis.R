rm(list = ls())
setwd("filepath")
library(readr)
library(dplyr)

# the metrics are downloaded from the CSMAR database, where each indicator can be searched and located
actual_ind = read_csv("Actual Indicators173012379/AF_Actual.csv")
cor_info = read_csv("OFDI_ListStkRight.csv")
eq_nature = read_csv("EN_EquityNatureAll.csv")
csr = read_csv("CSRR_BasicInfo.csv")
finance_ind = read_csv("Solvency172626208/FI_T1.csv")
asset = read_csv("Balance Sheet103649398/FS_Combas.csv")
totalequity = read_csv("Balance Sheet_equity/FS_Combas.csv")
npm = read_csv("Earning Capacity Analysis133308789/FI_T5.csv")

#cleaning csr
csr_long = select(csr, Symbol, DeclareDate, IndustryCode, DonationAmount, ShareholdersProtection, 
                  CreditorProtection, StaffProtection, DeliveryProtection, CustomerProtection, 
                  EnvironmentProtection, PublicRelations, SystemConstruction, WorkSafety,
                  Deficiency) 
csr_long$ShareholdersProtection = ifelse(csr_long$ShareholdersProtection == 2, 0, 1)
csr_long$CreditorProtection = ifelse(csr_long$CreditorProtection == 2, 0, 1)
csr_long$StaffProtection = ifelse(csr_long$StaffProtection == 2, 0, 1)
csr_long$DeliveryProtection = ifelse(csr_long$DeliveryProtection == 2, 0, 1)
csr_long$CustomerProtection = ifelse(csr_long$CustomerProtection == 2, 0, 1)
csr_long$EnvironmentProtection = ifelse(csr_long$EnvironmentProtection == 2, 0, 1)
csr_long$PublicRelations = ifelse(csr_long$PublicRelations == 2, 0, 1)
csr_long$SystemConstruction = ifelse(csr_long$SystemConstruction == 2, 0, 1)
csr_long$WorkSafety = ifelse(csr_long$WorkSafety == 2, 0, 1)
csr_long$Deficiency = ifelse(csr_long$Deficiency == 2, 0, 1)

csr_long$csr_score = csr_long$ShareholdersProtection + csr_long$CreditorProtection + csr_long$StaffProtection +
  csr_long$DeliveryProtection + csr_long$CustomerProtection + csr_long$EnvironmentProtection + csr_long$PublicRelations +
  csr_long$SystemConstruction + csr_long$WorkSafety + csr_long$Deficiency
csr_lag = csr_long %>% select(Symbol, DeclareDate, DonationAmount, csr_score, IndustryCode) %>%
  rename(stock = Symbol, date = DeclareDate, donation_lag = DonationAmount, csr_lag = csr_score, industry = IndustryCode)
csr_lag$date = format(as.Date(csr_lag$date), "%Y")
csr_lag$date = as.numeric(csr_lag$date)
csr_lag$date = csr_lag$date + 1

csr_clean = csr_long %>% select(Symbol, DeclareDate, DonationAmount, csr_score) %>%
  rename(stock = Symbol, date = DeclareDate, donation = DonationAmount, csr = csr_score)
csr_clean$date = format(as.Date(csr_clean$date), "%Y")
csr_clean$date = as.numeric(csr_clean$date)

#cleaning actual_ind
ind_clean = actual_ind %>% select(Stkcd, Ddate, Meps, Mpe, Mnetpro, BM, ROA, ROE, PB) %>% 
  rename(stock = Stkcd, date = Ddate, eps = Meps, pe = Mpe, netprofit = Mnetpro)
ind_clean$date = format(as.Date(ind_clean$date), "%Y")
ind_clean$date = as.numeric(ind_clean$date)
ind_clean$netprof_bil = ind_clean$netprofit / 1000000000

#cleaning cor_info
cor_info_clean = cor_info %>% select(Symbol, EndDate, EnterpriseAge) %>% 
  rename(stock = Symbol, date = EndDate, age = EnterpriseAge)
cor_info_clean$date = format(as.Date(cor_info_clean$date), "%Y")
cor_info_clean$date = as.numeric(cor_info_clean$date)

#cleaning eq_nature
equity = eq_nature %>% select(Symbol, EndDate, EquityNatureID) %>% rename(stock = Symbol, date = EndDate)
equity$date = format(as.Date(equity$date), "%Y")
equity$soe = ifelse(equity$EquityNatureID == 1, 1, 0)
equity$foreign = ifelse(equity$EquityNatureID == 3, 1, 0)
equity_clean = equity %>% select(stock, date, soe, foreign)
equity_clean$date = as.numeric(equity_clean$date)

#cleaning finance_ind
finance = finance_ind %>% select(Stkcd, Accper, Typrep, F011201A, F011701A) %>% 
  rename(stock = Stkcd, date = Accper, type = Typrep, DA = F011201A, DE = F011701A)
finance_clean = finance %>% filter(grepl("12-31$", date)) %>% filter(type == "A")
finance_clean$date = format(as.Date(finance_clean$date), "%Y")
finance_clean$date = as.numeric(finance_clean$date)

#cleaning asset
asset_clean = asset %>% select(Stkcd, Accper, Typrep, A001000000) %>%
  rename(stock = Stkcd, date = Accper, type = Typrep, asset = A001000000) %>%
  filter(grepl("12-31$", date)) %>% filter(type == "A")
asset_clean$date = format(as.Date(asset_clean$date), "%Y")
asset_clean$date = as.numeric(asset_clean$date)
asset_clean$asset_bil = asset_clean$asset / 1000000000

# cleaning equity
totalequity_clean = totalequity %>% select(Stkcd, Accper, Typrep, A003000000) %>%
  rename(stock = Stkcd, date = Accper, type = Typrep, equity = A003000000) %>%
  filter(grepl("12-31$", date)) %>% filter(type == "A")
totalequity_clean$date = format(as.Date(totalequity_clean$date), "%Y")
totalequity_clean$date = as.numeric(totalequity_clean$date)
totalequity_clean$equity_bil = totalequity_clean$equity / 1000000000

# clean npm
npm_clean = npm %>% select(Stkcd, Accper, Typrep, F051501B) %>%
  rename(stock = Stkcd, date = Accper, type = Typrep, npm = F051501B) %>%
  filter(grepl("12-31$", date)) %>% filter(type == "A")
npm_clean$date = format(as.Date(npm_clean$date), "%Y")
npm_clean$date = as.numeric(npm_clean$date)

# combine data frame
join1 = full_join(csr_clean, csr_lag, by = c("stock", "date"))
join2 = left_join(join1, ind_clean, by = c("stock", "date"))
join3 = left_join(join2, cor_info_clean, by = c("stock", "date"))
join4 = left_join(join3, equity_clean, by = c("stock", "date"))
join5 = left_join(join4, finance_clean, by = c("stock", "date"))
join5$date = ifelse(join5$date == 2109, 2019, join5$date)
join6 = left_join(join5, asset_clean, by = c("stock", "date"))
join7 = left_join(join6, totalequity_clean, by = c("stock", "date"))
join8 = left_join(join7, npm_clean, by = c("stock", "date"))

# filter 2015 - 2019 data
final = join8 %>% select(stock, date, csr, csr_lag, industry, eps, ROA, ROE, 
                         age, soe, DE, asset_bil, npm)
final = final %>% filter(date > 2014 & date < 2020)
final$ROA = final$ROA * 100
final$asset_log = log(final$asset_bil)
df.final = na.omit(final)

# descriptive statistics
summary(df.final)
sd(df.final$ROA, na.rm = TRUE)
sd(df.final$ROE, na.rm = TRUE)
sd(df.final$eps, na.rm = TRUE)
sd(df.final$npm, na.rm = TRUE)
sd(df.final$csr, na.rm = TRUE)
sd(df.final$age, na.rm = TRUE)
sd(df.final$soe, na.rm = TRUE)
sd(df.final$asset_log, na.rm = TRUE)
sd(df.final$DE, na.rm = TRUE)

# correlation matrix
cor.test(df.final$ROA, df.final$ROE, method = "pearson")

cor.test(df.final$ROA, df.final$eps, method = "pearson")
cor.test(df.final$ROE, df.final$eps, method = "pearson")

cor.test(df.final$ROA, df.final$netprof_bil, method = "pearson")
cor.test(df.final$ROE, df.final$netprof_bil, method = "pearson")
cor.test(df.final$eps, df.final$netprof_bil, method = "pearson")

cor.test(df.final$ROA, df.final$csr, method = "pearson")
cor.test(df.final$ROE, df.final$csr, method = "pearson")
cor.test(df.final$eps, df.final$csr, method = "pearson")
cor.test(df.final$netprof_bil, df.final$csr, method = "pearson")

cor.test(df.final$ROA, df.final$age, method = "pearson")
cor.test(df.final$ROE, df.final$age, method = "pearson")
cor.test(df.final$eps, df.final$age, method = "pearson")
cor.test(df.final$netprof_bil, df.final$age, method = "pearson")
cor.test(df.final$csr, df.final$age, method = "pearson")

cor.test(df.final$ROA, df.final$soe, method = "pearson")
cor.test(df.final$ROE, df.final$soe, method = "pearson")
cor.test(df.final$eps, df.final$soe, method = "pearson")
cor.test(df.final$netprof_bil, df.final$soe, method = "pearson")
cor.test(df.final$csr, df.final$soe, method = "pearson")
cor.test(df.final$age, df.final$soe, method = "pearson")

cor.test(df.final$ROA, df.final$asset_log, method = "pearson")
cor.test(df.final$ROE, df.final$asset_log, method = "pearson")
cor.test(df.final$eps, df.final$asset_log, method = "pearson")
cor.test(df.final$netprof_bil, df.final$asset_log, method = "pearson")
cor.test(df.final$csr, df.final$asset_log, method = "pearson")
cor.test(df.final$age, df.final$asset_log, method = "pearson")
cor.test(df.final$soe, df.final$asset_log, method = "pearson")

cor.test(df.final$ROA, df.final$DE, method = "pearson")
cor.test(df.final$ROE, df.final$DE, method = "pearson")
cor.test(df.final$eps, df.final$DE, method = "pearson")
cor.test(df.final$netprof_bil, df.final$DE, method = "pearson")
cor.test(df.final$csr, df.final$DE, method = "pearson")
cor.test(df.final$age, df.final$DE, method = "pearson")
cor.test(df.final$soe, df.final$DE, method = "pearson")
cor.test(df.final$asset_log, df.final$DE, method = "pearson")

cor.test(df.final$ROA, df.final$npm, method = "pearson")
cor.test(df.final$ROE, df.final$npm, method = "pearson")
cor.test(df.final$eps, df.final$npm, method = "pearson")
cor.test(df.final$csr, df.final$npm, method = "pearson")
cor.test(df.final$age, df.final$npm, method = "pearson")
cor.test(df.final$soe, df.final$npm, method = "pearson")
cor.test(df.final$asset_log, df.final$npm, method = "pearson")
cor.test(df.final$DE, df.final$npm, method = "pearson")

# apply model
library(plm)
library(stargazer)
library(lmtest)
mod1 = lm(ROA ~ csr + age + soe + asset_log + DE, df.final)

mod1_time = plm(ROA ~ csr + age + soe + asset_log + DE,   
               data = df.final,
               index = c("date"),
               model = "within")
mod1_both = plm(ROA ~ csr + age + soe + asset_log + DE + factor(industry),   
               data = df.final,
               index = c("date"),
               model = "within")

rob_se1 = list(sqrt(diag(vcovHC(mod1_time, type = "HC1"))),
               sqrt(diag(vcovHC(mod1_both, type = "HC1"))))
stargazer(mod1_time, mod1_both, 
          digits = 3,
          header = FALSE,
          type = "text", 
          se = rob_se1,
          title = "Title",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)"))

mod2 = lm(ROE ~ csr + age + soe + asset_log + DE, df.final)

mod2_time = plm(ROE ~ csr + age + soe + asset_log + DE,   
                data = df.final,
                index = c("date"),
                model = "within")

mod2_both = plm(ROE ~ csr + age + soe + asset_log + DE + factor(industry),   
                data = df.final,
                index = c("date"),
                model = "within")

rob_se2 = list(sqrt(diag(vcovHC(mod2_time, type = "HC1"))),
               sqrt(diag(vcovHC(mod2_both, type = "HC1"))))
stargazer(mod2_time, mod2_both, 
          digits = 3,
          header = FALSE,
          type = "text", 
          se = rob_se2,
          title = "Title",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)"))

mod3 = lm(eps ~ csr + age + soe + asset_log + DE, df.final)

mod3_time = plm(eps ~ csr + age + soe + asset_log + DE,   
                data = df.final,
                index = c("date"),
                model = "within")

mod3_both = plm(eps ~ csr + age + soe + asset_log + DE + factor(industry),   
                data = df.final,
                index = c("date"),
                model = "within")

rob_se3 = list(sqrt(diag(vcovHC(mod3_time, type = "HC1"))),
               sqrt(diag(vcovHC(mod3_both, type = "HC1"))))
stargazer(mod3_time, mod3_both, 
          digits = 3,
          header = FALSE,
          type = "text", 
          se = rob_se3,
          title = "Title",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)"))

mod4 = lm(npm ~ csr + age + soe + asset_log + DE, df.final)

mod4_time = plm(npm ~ csr + age + soe + asset_log + DE,   
                data = df.final,
                index = c("date"),
                model = "within")

mod4_both = plm(npm ~ csr + age + soe + asset_log + DE + factor(industry),   
                data = df.final,
                index = c("date"),
                model = "within")

rob_se4 = list(sqrt(diag(vcovHC(mod4_time, type = "HC1"))),
               sqrt(diag(vcovHC(mod4_both, type = "HC1"))))
stargazer(mod4_time, mod4_both, 
          digits = 3,
          header = FALSE,
          type = "text", 
          se = rob_se4,
          title = "Title",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)"))
