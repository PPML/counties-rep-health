
##This script processes the raw NSFG,ACS and YRBS data, downloadded from public use files
##It also recodes ACS variables to match NSFG

rm(list=ls())
library(readstata13)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(survey)
library(geojsonio)
library(rgdal)
library(broom)
library(rgeos)
library(stringr)
library(matrixStats)
library(haven)
library(censusGeography)
library(urbnmapr)
library(covidcast)


#Filepaths
in.path = "~/data_counties/Data/Raw/"
out.path = "~/data_counties/Data/Raw/"

############
##  ACS ###
############
acs = fread(paste0(in.path,"usa_00012.csv"))

#Check 2019
#acs1 = fread(paste0(in.path,"ACS_2019.csv"))

#acs = fread(paste0(in.path,"acs_income_only.csv"))

acs = acs[FTOTINC == 9999999, FTOTINC := NA]

##Note: this is not necessarily the biolocal mother
acs[,mom_birth_age := AGE_MOM - AGE]
acs[,mom_age_cat := "missing"]
acs[mom_birth_age <= 24, mom_age_cat := "24 or younger"]
acs[mom_birth_age >= 25, mom_age_cat := "25 or older"]
acs[mom_birth_age <= 12, mom_age_cat := "Non-trad mother"]
acs[mom_age_cat == "missing", mom_age_cat := NA]

#Align household income variable
acs[, totincr := cut(FTOTINC, 
                     c(-13000,5000, 7499, 9999, 12499,
                       14999,19999,24999,29999,34999,39999,
                       49999,59999,74999,99999,
                       5000000), include.lowest = FALSE)]
acs[!is.na(FTOTINC),household_cat := "Under $25000"]
acs[FTOTINC >=25000 & FTOTINC < 600000, household_cat := "$25000-$59,999"]
acs[FTOTINC >= 600000, household_cat := "$60,000 or more"]
acs[is.na(FTOTINC), household_cat := NA]

##Align sex variable - no missing values
acs[,SEX := ifelse(SEX == 1, "male","female")]

##Align Race variables - no missing values
acs[,race := ifelse(RACE  == 1, "white","other")]
acs[RACE == 2, race := "black"]
acs[,race := factor(race, levels = c("black","white","other"))]

#Combined race and ethnicity
#HISPAN = 1:4 = Mexican, Puerto Rican, Cuban, Other; 0 = Not hispanic
#https://usa.ipums.org/usa-action/variables/HISPAN#codes_section
acs[,race_eth := race]
acs[HISPAN %in% c(1:4), race_eth := "hisp"]
acs[race_eth == "white", race_eth := "non-hisp white"]
acs[race_eth == "black", race_eth := "non-hisp black"]
acs[,race_eth := factor(race_eth, levels = c("non-hisp black","hisp", "non-hisp white","other"))]

##Align Metro variable - no missing values
#https://usa.ipums.org/usa-action/variables/METRO#codes_section 
acs[,metro := "missing"]
acs[METRO == 1, metro := "not msa"]
acs[METRO == 2, metro := "principal city of msa"]
acs[METRO %in% c(3:4), metro := "other msa"]
acs[METRO  == 0, metro := "not msa"]
acs[,metro := factor(metro,c("not msa","other msa", "principal city of msa") )]
 
#Alternative classification because status 0 is 'mixed' metropolitan status
acs[,metro_alt := metro]
acs[METRO  == 0, metro_alt := "other msa"]

##Household Composition
acs[,household_comp := "single_parent"]
acs[MOMLOC == 0 & MOMLOC2 == 0 & POPLOC == 0 & POPLOC2 == 0, household_comp := "other" ]
acs[(MOMLOC != 0 & POPLOC != 0) | (MOMLOC2 != 0 & MOMLOC != 0) | (POPLOC != 0 & POPLOC2 != 0), household_comp := "two_parents" ]
table(acs$MOMLOC, acs$household_comp)
acs[household_comp == "single_parent", .(MOMLOC, MOMLOC2, POPLOC, POPLOC2)]

##Number of family members in the household
acs[,nhhmembers := ifelse(FAMSIZE >= 8, 8, FAMSIZE)]
acs[,nhhmembers_cat := "1-4"]
acs[nhhmembers <= 4,nhhmembers_cat := "1-4" ]
acs[nhhmembers >= 5,nhhmembers_cat := "5 or more" ]
acs[,nhhmembers_cat := factor(nhhmembers_cat, 
                              levels = c("1-4","5 or more"), 
                              labels = c("1-4","5 or more"))]


##Grade
acs[GRADEATTD %in% c(51:54), grade := GRADEATTD - 42]
acs[GRADEATTD %in% c(41:44), grade := GRADEATTD - 36]
acs[GRADEATTD  >= 60, grade := 13] #College
acs[GRADEATTD  == 0 , grade := NA]

##Align school variable
acs[,school := factor(SCHOOL, levels = c(1,2), labels = c(0,1))]

acs = acs[MULTYEAR == 2019]
weight_names = c(grep("REPWT",colnames(acs),value=T),"CBSERIAL")
acs_weights = acs[,..weight_names]
acs1 = acs[,.(year = 2019, year_cat = "2017_2019", age = AGE, race, 
             sex=SEX,grade,  household_cat,race_eth, household_comp,
             nhhmembers_cat, metro, mom_age_cat, strata = STRATA, 
             cluster = CLUSTER, state_code = STATEFIP, goschol = school,
             countyICP = COUNTYICP, COUNTYFIP, PUMA, weight = PERWT,
             CBSERIAL)]
acs = data.table(cbind(acs1,acs_weights))
acs[,survey := "acs"]



#saveRDS(xx, paste0(out.path,"ACS_bootstraps.RDS"))
saveRDS(acs, paste0(out.path,"ACS_Raw_final.RDS"))

