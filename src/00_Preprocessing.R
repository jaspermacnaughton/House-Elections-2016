# Preprocess data for project to produce .rda file. Used to separate cleaning from main
setwd("C:/Users/jaspe/OneDrive/Documents/Classes/Mathematics/318 - Mathematical Analysis of Statistical Software/Final Project/src")


library(tidyverse)
rawdata <- read.csv("../dataset/CandidateSummaryAction1.csv")
campaignFundingFull <- as.tibble(rawdata)

#subset(campaignFundingFull, can_inc_cha_ope_sea == "")
campaignFundingFull <- campaignFundingFull[which(campaignFundingFull$can_inc_cha_ope_sea!=''),]

campaignFundingFull$can_inc_cha_ope_sea <- as.factor(as.character(campaignFundingFull$can_inc_cha_ope_sea))


# Convert numeric cols to numeric type
currencyFields <- c("ind_ite_con", "ind_uni_con", "ind_con", "par_com_con", "oth_com_con", "can_con", "tot_con", "tra_fro_oth_aut_com", "can_loa", "oth_loa", "tot_loa", "off_to_ope_exp", "off_to_fun", "off_to_leg_acc", "oth_rec", "tot_rec", "ope_exp", "exe_leg_acc_dis", "fun_dis", "tra_to_oth_aut_com", "can_loa_rep", "oth_loa_rep", "tot_loa_rep", "ind_ref", "par_com_ref", "oth_com_ref", "tot_con_ref", "oth_dis", "tot_dis", "cas_on_han_beg_of_per", "cas_on_han_clo_of_per", "net_con", "net_ope_exp", "deb_owe_by_com", "deb_owe_to_com")
clean <- function(col) {
  as.numeric( gsub('[$,]', '', col ))
}
campaignFundingFull[currencyFields] <- sapply(campaignFundingFull[currencyFields], clean)

# Date Cols
campaignFundingFull$cov_sta_dat <- as.Date(campaignFundingFull$cov_sta_dat,format='%m/%d/%Y')
campaignFundingFull$cov_end_dat <- as.Date(campaignFundingFull$cov_end_dat,format='%m/%d/%Y')


# Trim dataset for analysis
campaignFunding <- campaignFundingFull[,c("can_off", "can_off_sta", "can_par_aff", "can_inc_cha_ope_sea", "ind_ite_con", "ind_uni_con", "ind_con", "par_com_con", "oth_com_con", "can_con", "tot_con", "ope_exp", "net_con", "tot_loa", "winner")]
colnames(campaignFunding) <- c("Office", "State", "Party", "Seat_Status", "Ind_Itemized_Con", "Ind_Unitemized_Con", "Ind_Total_Con", "Party_Con", "Com_Con", "Self_Con", "Total_Con", "Operating_Exp", "Net_Con", "Total_Loans", "Winner")




# Convert Winner to Numeric 0/1
campaignFunding$Numeric_Winner <- as.numeric(campaignFunding$Winner)-1

# Save Winner as a factor variable
campaignFunding$Winner <- as.factor(campaignFunding$Numeric_Winner)

# Coerce NA's to 0
campaignFunding[is.na(campaignFunding)] <- 0


# Sanity Checks
summary(campaignFunding$Ind_Itemized_Con == 0) # == 244 in Original Dataset
summary(campaignFunding$Total_Con == 0)        # == 119 in Original Dataset
summary(campaignFunding$Numeric_Winner == 0)   # == 1343 Losses before cleansing


congressional <- subset(campaignFunding, Office!="P")

save(congressional, file = "campaignFunding.Rda")



