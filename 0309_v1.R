list.files()
library(readxl)
install.packages("readxl")
#library(xlsx)
library(openxlsx)
names(MASTER2Report)


#===========
# Notes: 
# FinalQuestFacilityRoster-AFMC021722.xlsx -- the data from rental and contracting data, Tony data (doctors has contract) NPI has blanks, need to have the missing NPIs; submit to CMS
# the organizationNPI is facility NPI;  
# draw from the QuestFull data
#
# NPI_AFMCGoldKidney-20220224.xlsx -- the data by goldkidey that with NPI missing; only rental network data
# QuestFullProviderMarketReport_20220225.xlsx -- public data collected from Internet; 
# have a discrepanacy NPI data from Dave)
# 
# remove space from the string
#=======
finalquest_squ <- read.xlsx("FinalQuestFacilityRoster-AFMC021722.xlsx", "Squirrel SQL Export", cols = 1:18, rows = 1:2908)
finalquest_squ$OrganizationNPI
View(finalquest_squ)

NPI_AFMCGoldKidney_squ <- read.xlsx("NPI_AFMCGoldKidney-20220224.xlsx", "Squirrel SQL Export", cols = 1:18, rows = 1:2904)
NPI_AFMCGoldKidney_squ$OrganizationNPI

QuestFullProviderMarket <- read.xlsx("QuestFullProviderMarketReport_20220225.xlsx", "Market Servicing Providers", cols = 1:48, rows = 10:114036)
QuestFullProviderMarket

library(dplyr)
names(NPI_AFMCGoldKidney_squ)
# separate the providers with and without NA
#finalquest_squ_NPI_NA <- finalquest_squ[is.na(finalquest_squ$OrganizationNPI) == TRUE, ]
#finalquest_squ_NPI_NOT_NA <- finalquest_squ[is.na(finalquest_squ$OrganizationNPI) == FALSE, ]


# ===join with AFM data
finalquest_squ_newids <- finalquest_squ %>% mutate(NEW_ID1 = paste0(
  gsub(" ", "", tolower(finalquest_squ$LocationName)),
  gsub(" ", "", tolower(finalquest_squ$Address1)),
  gsub(" ", "", finalquest_squ$ZIPCode)),
  NEW_ID2 = paste0(
    gsub(" ", "", tolower(finalquest_squ$LocationName)),
    gsub(" ", "", tolower(finalquest_squ$Address1)))
  )

NPI_AFMCGoldKidney_squ_newids <- NPI_AFMCGoldKidney_squ %>% mutate(NEW_ID1 = paste0(
  gsub(" ", "", tolower(NPI_AFMCGoldKidney_squ$LocationName)),
  gsub(" ", "", tolower(NPI_AFMCGoldKidney_squ$Address1)),
  gsub(" ", "", NPI_AFMCGoldKidney_squ$ZIPCode)))

QuestFullProviderMarket_newids <- QuestFullProviderMarket %>% mutate(NEW_ID2 = paste0(
    gsub(" ", "", tolower(QuestFullProviderMarket$Name)),
    gsub(" ", "", tolower(QuestFullProviderMarket$Address))))

names(QuestFullProviderMarket_newids )
# join with AFC data and final quest data
t1 <- (finalquest_squ_newids %>%
         left_join(select(NPI_AFMCGoldKidney_squ_newids, c(OrganizationNPI, NEW_ID1)), by= "NEW_ID1")) %>%
  left_join(select(QuestFullProviderMarket_newids, c(NPI, NEW_ID2)), by= "NEW_ID2") 


# t1 %>% mutate(NPI_AFMorQuestfull = ifelse(is.na(t1$OrganizationNPI.y)==TRUE & is.na(t1$NPI)==FALSE, t1$NPI, ifelse(is.na(t1$OrganizationNPI.y==FALSE & is.na(t1$NPI==TRUE)), 
#                                                                                                                    t1$OrganizationNPI.y,ifelse()
#                                                                                                                    )))



names(t1)[3] <- c("OrganizationNPI")
names(t1)[21] <- c("NPI_from_AMF")
names(t1)[22] <- c("NPI_from_questfull")


names(t1)

t1 %>%
  mutate_(NPI_final = case_when((is.na(OrganizationNPI)==FALSE) ~ t1$OrganizationNPI,
                               (is.na(OrganizationNPI) == TRUE & is.na(NPI_from_AMF)== FALSE & is.na(NPI_from_questfull) == TRUE) ~ t1$NPI_from_AMF,
                               (is.na(OrganizationNPI) == TRUE & is.na(NPI_from_AMF)== TRUE & is.na(NPI_from_questfull) == FALSE) ~ t1$NPI_from_questfull,
                               (is.na(OrganizationNPI) == TRUE & is.na(NPI_from_AMF)== FALSE & is.na(NPI_from_questfull) == FALSE) ~ t1$NPI_from_questfull,
                               (is.na(OrganizationNPI) == TRUE & is.na(NPI_from_AMF)== TRUE & is.na(NPI_from_questfull) == TRUE) ~ NULL
                               ))


t2 <- t1 %>% filter(is.na(OrganizationNPI)==FALSE) %>% mutate(NPI_final = OrganizationNPI)
t3 <- t1 %>% filter(is.na(OrganizationNPI) == TRUE & is.na(NPI_from_AMF)== FALSE & is.na(NPI_from_questfull) == TRUE) %>% mutate(NPI_final = NPI_from_AMF)
t4 <- t1 %>% filter(is.na(OrganizationNPI) == TRUE & is.na(NPI_from_AMF)== TRUE & is.na(NPI_from_questfull) == FALSE) %>% mutate(NPI_final = NPI_from_questfull)
t5 <- t1 %>% filter(is.na(OrganizationNPI) == TRUE & is.na(NPI_from_AMF)== FALSE & is.na(NPI_from_questfull) == FALSE) %>% mutate(NPI_final = NPI_from_questfull)
t6 <- t1 %>% filter(is.na(OrganizationNPI) == TRUE & is.na(NPI_from_AMF)== TRUE & is.na(NPI_from_questfull) == TRUE) %>% mutate(NPI_final = NULL)


t1_final<- rbind(t2, t3, t4, t5) %>% distinct()



# fun_NPI_Final <- function(OrgNPI, AMFNPI, QuestfullNPI) {
#   finalNPI = NULL
#   if (is.na(OrgNPI)==FALSE){
#     finalNPI = rbind(finalNPI, OrgNPI)
#   }
#   else if (is.na(OrgNPI) == TRUE & is.na(AMFNPI)== FALSE & is.na(QuestfullNPI) == TRUE){
#     finalNPI = rbind(finalNPI, AMFNPI)
#   }
#   else if (is.na(OrgNPI) == TRUE & is.na(AMFNPI)== TRUE & is.na(QuestfullNPI) == FALSE){
#     finalNPI = rbind(finalNPI, QuestfullNPI)
#   }
#   else if (is.na(OrgNPI) == TRUE & is.na(AMFNPI)== FALSE & is.na(QuestfullNPI) == FALSE){
#     finalNPI = rbind(finalNPI, QuestfullNPI)
#   }
#   else {
#     finalNPI = rbind(finalNPI, NULL)
#   }
# }
# 
# names(t1)
# 
# 
# apply(t1[c(3,21,22)], 1, fun_NPI_Final(OrgNPI= t1$OrganizationNPI, AMFNPI=t1$NPI_from_AMF, QuestfullNPI = t1$NPI_from_questfull))
# 

require(openxlsx)
# list_of_datasets <- list("finalquest_NPI_update" = t1, "finalquest_NPI_AFM" = finalquest_squ_match,
#                          "finalquest_NPINotfound_AFM" = finalquest_squ_nomatch)

write.xlsx(t1_final, file = "finalquest_squ_NPI_update_0311.xlsx")



save.image()


# load(".RData")








