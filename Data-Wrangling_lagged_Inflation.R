setwd("~/Desktop/WS21_22/Data Science_R&P/1915_project-app")

#rm(list = ls())

#install.packages("eurostat")
#install.packages("ggthemes")
library(eurostat)
library(tidyverse)
library(ranger)
library(zoo)
library(caret)
library(glmnet)
library(forecast)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(gridExtra)
library(ggthemes)
library(ggrepel)

############################
### 1. Data Preparation
############################

#scrape data

tmp <- get_eurostat_toc()

############################
### Group 1
############################

disposable_income <- get_eurostat(id = "teina090")
disposable_income <- disposable_income %>% select(geo, time, values)
disposable_income$description <- "Gross disposable income"
disposable_income$code <- "teina090"
disposable_income$group_name <- "Output and income"
disposable_income <- disposable_income %>% select(group_name, code, description, geo, time, values)
disposable_income$frequency <- "Quarterly"
disposable_income <- disposable_income %>% 
  select(geo, time, Disposable_Income = values)

industrial_production <- get_eurostat(id = "sts_inpr_q")
industrial_production_ <- industrial_production %>% 
  filter(nace_r2 %in% c("MIG_COG", "MIG_DCOG", "MIG_NDCOG", "B", "C", "D")) %>% 
  filter(s_adj == "NSA" & unit == "I15") %>% 
  select(nace_r2, geo:values) %>% 
  mutate(nace_r2 = paste0("sts_inpr_q_", nace_r2)) %>% 
  pivot_wider(names_from = nace_r2, values_from = values)
colnames(industrial_production_) <- c("geo", "time", "Prod_Mining_Quarring", "Prod_Manufacturing",
                                      "Prod_Electricity_gas_steam_and_air_conditioning_supply",
                                      "Prod_Consumer_Goods", "Prod_Durable_Consumer_Goods",
                                      "Prod_Nondurable_Consumer_Goods")

group1 <- left_join(disposable_income, industrial_production_)

###################################
### Group 2
###################################

part1 <- get_eurostat(id = "lfsi_neet_q")
part1_ <- part1 %>% 
  filter(s_adj == "NSA" & unit == "PC_POP" & sex == "T" & age == "Y15-29") %>% 
  select(-s_adj, -unit, -sex, -age)

colnames(part1_)[3] <- "lfsi_neet_q"

part2 <- get_eurostat(id = "lfsi_emp_q")
part2_ <- part2 %>% 
  filter(indic_em == "ACT", s_adj == "NSA", unit == "PC_POP", sex == "T", age == "Y20-64") %>% 
  select(-indic_em, -s_adj, -unit, -sex, -age) 

colnames(part2_)[3] <- "lifsi_emp_q"

part3 <- get_eurostat(id = "lfsi_educ_q")
part3_ <- part3 %>% 
  filter(s_adj == "NSA", unit == "PC_EMP", sex == "T", age == "Y20-64") %>% 
  select(-s_adj, -unit, -sex, -age) %>% 
  pivot_wider(names_from = isced11, values_from = values)
colnames(part3_)[3:5] <- paste0("lfsi_eduq_q", colnames(part3_)[3:5])

part4 <- get_eurostat(id = "lfsi_pt_q")
part4_ <- part4 %>% 
  filter(wstatus == "EMP_PT", s_adj == "NSA", unit == "PC_EMP", sex == "T", age == "Y20-64") %>% 
  select(-wstatus, -s_adj, -unit, -sex, -age)
colnames(part4_)[3] <- "lfsi_pt_q"

part5 <- get_eurostat(id = "une_rt_q")
part5_ <- part5 %>% 
  filter(s_adj == "NSA", age == "Y20-64", unit == "PC_ACT", sex == "T") %>% 
  select(-s_adj, -age, -unit, -sex)
colnames(part5_)[3] <- "une_rt_q"

part6 <- get_eurostat(id = "une_ltu_q")
part6_ <- part6 %>% 
  filter(indic_em == "LTU", age == "Y20-64", s_adj == "NSA", sex == "T", unit == "PC_ACT") %>% 
  select(-indic_em, -age, -s_adj, -sex, -unit)
colnames(part6_)[3] <- "une_ltu_q"

group2 <- plyr::join_all(list(part1_, part2_, part3_, part4_, part5_, part6_),
                         type = "left", by = c("geo", "time"))

################################
### Group 3
##############################

part1 <- get_eurostat(id = "ei_hppi_q")
part1_ <- part1 %>% 
  filter(unit == "I15_NSA") %>% 
  select(geo, time, House_Prices = values)

part2 <- get_eurostat(id = "prc_hpi_ooq")
part2_ <- part2 %>% 
  filter(expend == "TOTAL" & unit == "I15_Q") %>% 
  select(geo, time, Owner_Occupies_Housing_Prices = values)

group3 <- left_join(part1_, part2_)

################################
### Group 4
################################

group4 <- get_eurostat(id = "sts_trtu_q")
group4 <- group4 %>% 
  filter(indic_bt == "TOVT", s_adj == "NSA", unit == "I15") %>% 
  select(-indic_bt, -s_adj, -unit) %>% 
  mutate(nace_r2 = paste0("sts_trtu_q_", nace_r2)) %>% 
  pivot_wider(names_from = nace_r2, values_from = values)

#####################################
### Group 5
#####################################

m1 <- read.csv("./01_input_data/data.csv", skip = 4)
m1_ <- m1
m1_$Period.Unit. <- gsub("Oct", "Okt", m1_$Period.Unit.)
m1_$Period.Unit. <- gsub("May", "Mai", m1_$Period.Unit.)
m1_$Period.Unit. <- gsub("Dec", "Dez", m1_$Period.Unit.)
m1_$Period.Unit. <- gsub("Mar", "Mär", m1_$Period.Unit.)
m1_$Period.Unit. <- paste0(substr(m1_$Period.Unit., 1, 4),"-", substr(m1_$Period.Unit., 5, 7), "-01")
m1_$Period.Unit. <- as.Date(m1_$Period.Unit., format("%Y-%b-%d"))
colnames(m1_) <- c("time", "M1 vis a vis Euro area non_MFI")

m2 <- read.csv("./01_input_data/data-2.csv", skip = 4)
m2_ <- m2
m2_$Period.Unit. <- gsub("Oct", "Okt", m2_$Period.Unit.)
m2_$Period.Unit. <- gsub("May", "Mai", m2_$Period.Unit.)
m2_$Period.Unit. <- gsub("Dec", "Dez", m2_$Period.Unit.)
m2_$Period.Unit. <- gsub("Mar", "Mär", m2_$Period.Unit.)
m2_$Period.Unit. <- paste0(substr(m2_$Period.Unit., 1, 4),"-", substr(m2_$Period.Unit., 5, 7), "-01")
m2_$Period.Unit. <- as.Date(m2_$Period.Unit., format("%Y-%b-%d"))
colnames(m2_) <- c("time", "M2 vis a vis Euro area non_MFI")

m3 <- read.csv("./01_input_data/data-3.csv", skip = 4)
m3_ <- m3
m3_$Period.Unit. <- gsub("Oct", "Okt", m3_$Period.Unit.)
m3_$Period.Unit. <- gsub("May", "Mai", m3_$Period.Unit.)
m3_$Period.Unit. <- gsub("Dec", "Dez", m3_$Period.Unit.)
m3_$Period.Unit. <- gsub("Mar", "Mär", m3_$Period.Unit.)
m3_$Period.Unit. <- paste0(substr(m3_$Period.Unit., 1, 4),"-", substr(m3_$Period.Unit., 5, 7), "-01")
m3_$Period.Unit. <- as.Date(m3_$Period.Unit., format("%Y-%b-%d"))
colnames(m3_) <- c("time", "M3 vis a vis Euro area non_MFI")

group5 <- plyr::join_all(list(m1_, m2_, m3_), type = "left")

######################
### Group 6
######################

mro <- read.csv("./01_input_data/data-5.csv", skip = 4)
mro$Period.Unit. <- as.character(mro$Period.Unit.)
mro$Period.Unit. <- as.Date(mro$Period.Unit., format("%Y-%m-%d"))
colnames(mro) <- c("time", "ECB deposit facility", "ECB marginal lending facility", "ECB MRO - fixed rate tenders", "ECB MRO - variable rate tenders")
mro$time <- format(mro$time, "%Y-%m")
mro$time <- paste0(mro$time, "-01")
mro$time <- as.Date(mro$time, format("%Y-%m-%d"))

dates <- data.frame(time = group5$time)
dates$time <- format(dates$time, "%Y-%m")
dates$time <- paste0(dates$time, "-01")
dates$time <- as.Date(dates$time, format("%Y-%m-%d"))
dates_ <- left_join(dates, mro)

dates_[1,2] <- -0.50 
dates_[1,3] <- 0.25 
dates_[1,4] <- 0.00 
dates_[1,5] <- 4.75
dates_$`ECB MRO - variable rate tenders` <- NULL

dates_[nrow(dates_), 2] <- 2
dates_[nrow(dates_), 3] <- 4.50
dates_[nrow(dates_), 4] <- 3

dates_$`ECB deposit facility` <- na.locf(dates_$`ECB deposit facility`, fromLast = T)
dates_$`ECB marginal lending facility` <- na.locf(dates_$`ECB marginal lending facility`, fromLast = T)
dates_$`ECB MRO - fixed rate tenders` <- na.locf(dates_$`ECB MRO - fixed rate tenders`, fromLast = T)

dates_ <- dates_ %>% 
  unique()

interest <- get_eurostat(id = "ei_mfir_m")
interest <- interest %>% 
  filter(s_adj == "NSA") %>% 
  select(-s_adj, -p_adj) %>% 
  pivot_wider(names_from = indic, values_from = values) %>% 
  filter(geo == "EA") %>% 
  select(-geo)

group6 <- left_join(interest, dates_)

###############
### Inflation
###############

inflation <- get_eurostat(id = "ei_cphi_m")
inflation <- inflation %>% 
  filter(unit == "HICP2015" & s_adj == "NSA" & indic == "CP-HI00")
inflation <- inflation %>% select(geo, time, values)
colnames(inflation)[3] <- "Inflation"

inflation_ <- inflation %>% 
  arrange(time)

inflation_$Inflation_Rate <- NA

inflation.tmp <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(inflation.tmp) <- colnames(inflation_)


for (country in unique(inflation_$geo)){
  
  start = 1
  end = 13
  
  #country = "CZ"
  
  infl.tmp <- inflation_ %>% 
    filter(geo == country)
  
  for (i in 13:nrow(infl.tmp)){
    
    infl.tmp[i,4] <- (infl.tmp[end,3] - infl.tmp[start,3])/infl.tmp[start,3] * 100
    
    start = start + 1
    end = end + 1
    
  }
  
  inflation.tmp <- rbind(inflation.tmp, infl.tmp)
  
}

inflation.tmp <- inflation.tmp %>% 
  group_by(geo) %>% 
  mutate(Inflation_lag = lag(Inflation_Rate, k = 1, default = NA)) %>% 
  ungroup()

inflation <- inflation.tmp %>% 
  select(time, geo, Inflation_lag, Inflation = Inflation_Rate)

######################
### Group 7
######################

part1 <- get_eurostat(id = "sts_inppd_m")
part1_ <- part1 %>% 
  filter(unit == "I15") %>% 
  filter(nace_r2 %in% c("B", "C", "D", "MIG_CAG", "MIG_COG", "MIG_DCOG", "MIG_NDCOG")) %>% 
  mutate(nace_r2 = paste0("sts_inppd_m_", nace_r2)) %>% 
  select(nace_r2, geo, time, values) %>% 
  pivot_wider(names_from = nace_r2, values_from = values)

part2 <- get_eurostat(id = "prc_hicp_midx")
part2_ <- part2 %>%
  filter(unit == "I15") %>%
  filter(coicop %in% c("CP07", "CP03", "CP06", "SERV")) %>%
  mutate(coicop = paste0("prc_hicp_midx_", coicop)) %>%
  select(coicop, geo, time, values) %>%
  pivot_wider(names_from = coicop, values_from = values)

group7 <- left_join(part1_, part2_)

# merge data

dates_geo <- data.frame(time = part1_$time, geo = part1_$geo)

total <- plyr::join_all(list(dates_geo,group6, group7, group4, group3, group2, group5, group1, inflation), 
                        type = "left")

#total$geo <- gsub("PT", "PRT", total$geo)

total <- total %>% # no data for Greece
  filter(geo %in% c("AT", "BE", "CY", "EE", "FI", "FR", "DE", "GR", "IE",
                    "IT", "LV", "LT", "LU", "MT", "NL", "PT", "SK", "SI", "ES"))

total <- total %>% 
  filter(grepl("-03-01|-06-01|-09-01|-12-01", time))
  #select(-Disposable_Income, -Prod_Mining_Quarring, -Prod_Manufacturing, -Prod_Electricity_gas_steam_and_air_conditioning_supply, -Prod_Consumer_Goods, -Prod_Durable_Consumer_Goods, -Prod_Nondurable_Consumer_Goods)

total <- total %>% 
  #filter(time != "2021-10-01") %>% 
  filter(time > "2010-01-01")

colnames(total) <- gsub(" ", "_", colnames(total))

save(total, file = "Data_Wrangling_V1_lagged.rda")
