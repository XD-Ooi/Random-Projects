library(dplyr)
library(tidyverse)

#This is for cleaning the data downloaded from OECD's database. 
#I highly recommend getting as much dataset as possible from OECD's database since the downloaded data are already very neat and clean.
#I only used these columns for my model. Feel free to keep other columns that you need.

variable_i = read_csv("file_directory.csv")
variable_i_clean = select(unemployment, LOCATION, TIME, Value) %>% 
  rename(country = LOCATION,
         unemployment = Value,
         year = TIME)
 
#This is for cleaning the data downloaded from World Bank database.
variable_j = read_csv("file_directory.csv", 
                        skip = 4) #Make sure to double check the number of rows you need to skip depending on your downloads
variable_j_clean = select(variable_j, `Country Code`, "2009", "2010", "2011", 
                           "2012", "2013", "2014", "2016", "2017", "2018") %>% 
                    filter(`Country Code` %in% c("AUT", "BEL", "CZE", "DNK", "FIN", "FRA", 
                                                 "DEU", "GRC", "HUN", "IRL", "ITA", "LUX", 
                                                 "NLD", "POL", "PRT", "SVK", "ESP", "SWE", 
                                                 "GBR", "BGR", "HRV", "CYP", "EST", "LVA", 
                                                 "LTU", "MLT", "ROU", "SVN")) %>%
                    rename(country = `Country Code`)
variable_j_long = pivot_longer(variable_j_clean,
                                cols = matches("^20"),
                                names_to = "year") %>% 
                  rename(variable_j_name = value) #Change the name accordingly 
variable_j_long$year = as.numeric(variable_j_long$year)

#The data from European Commission’s biannual Special Eurobarometer reports are extracted using Excel (because it was more convenient to do so).
variable_k = readxl::read_excel("file_directory.xlsx")

#Joining the datasets together
final_data = left_join(variable_i_clean, variable_j_clean, by = c("country","year")) %>%
             left_join(., variable_k_clean, by='Flag') #Continue on the nested left_join process until you finish joining all your variables into one dataframe.

#Once you are done, you can continue on to run the regression using R. 
#Just in case if you want to switch over to STATA for some random reason, which was what I did.

library(foreign)
write.dta(final_data, "file_directory.dta")

#Visualizing your variables
final_data %>% 
  ggplot(aes(variable_i, variable_k)) + 
  geom_point(size = 5, alpha = 0.3, colour = "#d90502") + 
  geom_smooth(method = "lm", se = FALSE, size = 0.5, colour = "black") #if you want to observe a regression line
  
#Running the multiple linear regression model
library(jtools) 
variable_k_1 = lm(varible_k ~ variable_i + variable_j, final_data)
variable_k_2 = lm(varible_k ~ variable_i + variable_j + variable_m, final_data)
variable_k_3 = lm(varible_k ~ variable_i + variable_j + variable_m + variable_n, final_data)

export_summs(variable_k_, variable_k_2, variable_k_3) #highly recommend the export_summs function. It produces a very neat outcome
