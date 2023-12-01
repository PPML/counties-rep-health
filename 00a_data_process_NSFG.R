
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


#Filepaths
in.path = "~/data_counties/Data/Raw/"
out.path = "~/data_counties/Data/Raw/"
  
############
##  NSFG ###
############

#In addition to using the sampling weight variable WGT2017_2019, researchers must use
#the design variables for the sampling stratum (SEST) and cluster (SECU) to obtain correct
#standard errors for their estimates.
female19 = data.table(read.dta13(paste0(in.path,"/NSFG_all_years/2017_2019_FemResp_raw.dta"), 
                      select.cols = c("AGE_R","hadsex", "educat", "VRY1STAG", 
                                      "SEX1AGE" ,"higrade" ,"PRIMLANG1","hispanic" ,"race",
                                      "hhpartyp","AGEMOMB1" ,"lsexrage" ,"PARTS1YR" ,
                                      "metro" ,"poverty" ,"totincr" , "roscnt",
                                       "WGT2017_2019", "sest", "secu","goschol")))
female19[,year := 2019]
female19[,weight := WGT2017_2019]
female19[,WGT2017_2019 := NULL]
colnames(female19) = tolower(colnames(female19))

female17 = data.table(read.dta13(paste0(in.path,"/NSFG_all_years/2015_2017_FemResp_raw.dta"),
                      select.cols = c("AGE_R","HADSEX", "EDUCAT", "VRY1STAG", 
                                      "SEX1AGE" ,"HIGRADE" ,"PRIMLANG1","HISPANIC" ,"RACE",
                                      "HHPARTYP","AGEMOMB1" ,"PARTS1YR" , "METRO",
                                      "POVERTY","TOTINCR", "ROSCNT", "WGT2015_2017",
                                       "SEST", "SECU","GOSCHOL")))
female17[,year := 2017]
female17[,weight := WGT2015_2017]
female17[,WGT2015_2017 := NULL]
colnames(female17) = tolower(colnames(female17))

female15 = data.table(read.dta13(paste0(in.path,"/NSFG_all_years/2013_2015_FemResp_raw.dta"),
                      select.cols = c("AGE_R","HADSEX", "EDUCAT", "VRY1STAG", 
                                      "SEX1AGE" ,"HIGRADE" ,"PRIMLANG1","HISPANIC" ,"RACE",
                                      "HHPARTYP","AGEMOMB1" ,"PARTS1YR" , "METRO",
                                      "POVERTY","TOTINCR",  "WGT2013_2015", "ROSCNT",
                                      "SEST", "SECU","GOSCHOL")))
female15[,year := 2015]
female15[,weight := WGT2013_2015]
female15[,WGT2013_2015 := NULL]
colnames(female15) = tolower(colnames(female15))

female13 = data.table(read.dta13(paste0(in.path,"/NSFG_all_years/2011_2013_FemResp_raw.dta"),
                      select.cols = c("AGE_R","HADSEX", "EDUCAT", "VRY1STAG", 
                                      "SEX1AGE" ,"HIGRADE" ,"PRIMLANG1","HISPANIC" ,"RACE",
                                      "HHPARTYP","AGEMOMB1" ,"PARTS1YR" , "METRO",
                                      "POVERTY","TOTINCR",  "WGT2011_2013", "ROSCNT",
                                      "SEST", "SECU","GOSCHOL")))
female13[,year := 2013]
female13[,weight := WGT2011_2013]
female13[,WGT2011_2013 := NULL]
colnames(female13) = tolower(colnames(female13))

female = rbind(female19,female17, female15, female13, use.names = TRUE, fill = TRUE)
female[,sex := "female"]

male19 = data.table(read.dta13(paste0(in.path,"/NSFG_all_years/2017_2019_Male_raw.dta"), 
                                 select.cols = c("AGE_R","hadsex", "educat", "VRY1STAG", 
                                                 "higrade" ,"PRIMLANG1","hispanic" ,"race",
                                                 "hhpartyp","AGEMOMB1" ,"lsexrage" ,"PARTS1YR" ,
                                                 "metro" ,"poverty" ,"totincr" , "roscnt",
                                                 "WGT2017_2019", "sest", "secu","goschol")))
male19[,year := 2019]
male19[,weight := WGT2017_2019]
male19[,WGT2017_2019 := NULL]
colnames(male19) = tolower(colnames(male19))

male17 = data.table(read.dta13(paste0(in.path,"/NSFG_all_years/2015_2017_Male_raw.dta"),
                                 select.cols = c("AGE_R","HADSEX", "EDUCAT", "VRY1STAG", 
                                                 "HIGRADE" ,"PRIMLANG1","HISPANIC" ,"RACE",
                                                 "HHPARTYP","AGEMOMB1" ,"PARTS1YR" , "METRO",
                                                 "POVERTY","TOTINCR", "ROSCNT", "WGT2015_2017",
                                                 "SEST", "SECU","GOSCHOL")))
male17[,year := 2017]
male17[,weight := WGT2015_2017]
male17[,WGT2015_2017 := NULL]
colnames(male17) = tolower(colnames(male17))

male15 = data.table(read.dta13(paste0(in.path,"/NSFG_all_years/2013_2015_Male_raw.dta"),
                                 select.cols = c("AGE_R","HADSEX", "EDUCAT", "VRY1STAG", 
                                                 "SEX1AGE" ,"HIGRADE" ,"PRIMLANG1","HISPANIC" ,"RACE",
                                                 "HHPARTYP","AGEMOMB1" ,"PARTS1YR" , "METRO",
                                                 "POVERTY","TOTINCR",  "WGT2013_2015", "ROSCNT",
                                                 "SEST", "SECU","GOSCHOL")))
male15[,year := 2015]
male15[,weight := WGT2013_2015]
male15[,WGT2013_2015 := NULL]
colnames(male15) = tolower(colnames(male15))

male13 = data.table(read.dta13(paste0(in.path,"/NSFG_all_years/2011_2013_Male_raw.dta"),
                                 select.cols = c("AGE_R","HADSEX", "EDUCAT", "VRY1STAG", 
                                                 "SEX1AGE" ,"HIGRADE" ,"PRIMLANG1","HISPANIC" ,"RACE",
                                                 "HHPARTYP","AGEMOMB1" ,"PARTS1YR" , "METRO",
                                                 "POVERTY","TOTINCR",  "WGT2011_2013", "ROSCNT",
                                                 "SEST", "SECU","GOSCHOL")))
male13[,year := 2013]
male13[,weight := WGT2011_2013]
male13[,WGT2011_2013 := NULL]
colnames(male13) = tolower(colnames(male13))

male = rbind(male19,male17, male15, male13, use.names = TRUE, fill = TRUE)
male[,sex := "male"]


nsfg = rbind(male,female, fill=TRUE, use.names = TRUE)


nsfg[,year := factor(year, levels = c(2013,2015,2017,2019))]
nsfg[,year_cat := "missing"]
nsfg[year %in% c(2013,2015), year_cat := "2013_2015"]
nsfg[year %in% c(2017,2019), year_cat := "2015_2019"]

#Documentation says to use educat variable 
#https://www.cdc.gov/nchs/data/nsfg/2017-2019_NSFG_FemResp_SectionA_Codebook-508.pdf 
#Description available: https://www.cdc.gov/nchs/data/nsfg/NSFG-2017-2019-UG-App3a-FemRespRecodeSpecs-508.pdf
nsfg[,grade := "missing"]
nsfg[grepl("COLLEGE",educat), grade := "college"]
nsfg[grepl("GRADE",educat), grade := substring(educat,1,2)]
nsfg[educat %in% c("Not ascertained","Refused","Don't know"), grade := NA]

nsfg[,household_cat := "Under $25000"]
#Note that Totincr is actually total family income - https://www.cdc.gov/nchs/data/nsfg/NSFG-2017-2019-UG-App3a-FemRespRecodeSpecs-508.pdf
nsfg[totincr %in% c("$25,000-$29,999",
                    "$30,000-$34,999",
                    "$35,000-$39,999",
                    "$40,000-$49,999",
                    "$50,000-$59,999"),household_cat := "$25000-$59,999"]
nsfg[totincr %in% c("$60,000-$74,999",
                    "$75,000-$99,999",
                    "$100,000 or more",
                    "$100,000 OR MORE"),household_cat := "$60,000 or more"]
nsfg[,household_cat := factor(household_cat,levels = c("Under $25000","$25000-$59,999","$60,000 or more"))]

nsfg[,nhhmembers_cat := "1-4"]
nsfg[roscnt %in% c("1 HOUSEHOLD MEMBER",
                   "2 HOUSEHOLD MEMBERS",
                   "3 HOUSEHOLD MEMBERS",
                   "4 HOUSEHOLD MEMBERS"),nhhmembers_cat := "1-4" ]
nsfg[roscnt %in% c("5 HOUSEHOLD MEMBERS",
                   "6 HOUSEHOLD MEMBERS",
                   "7 HOUSEHOLD MEMBERS",
                   "8 OR MORE HOUSEHOLD MEMBERS"),nhhmembers_cat := "5 or more" ]
nsfg[,nhhmembers_cat := factor(nhhmembers_cat, 
                              levels = c("1-4","5 or more"), 
                              labels = c("1-4","5 or more"))]

#Age of mother or mother-figure - https://www.cdc.gov/nchs/data/nsfg/NSFG-2017-2019-UG-App3a-FemRespRecodeSpecs-508.pdf
nsfg[grepl("LESS THAN", agemomb1), mom_age := "Less than 18"]
nsfg[grepl("18-19", agemomb1), mom_age := "18-19 years"]
nsfg[grepl("20-24", agemomb1), mom_age := "20-24 years"]
nsfg[grepl("25-29", agemomb1), mom_age := "25-29 years"]
nsfg[grepl("30", agemomb1), mom_age := "30 or older"]
nsfg[,mom_age := factor(mom_age, levels = c("Less than 18",
                                            "18-19 years",
                                            "20-24 years",
                                            "25-29 years",
                                            "30 or older"))]
nsfg[,mom_age_cat := "missing"]
nsfg[mom_age %in% c("Less than 18","18-19 years", "20-24 years"),mom_age_cat := "24 or younger"]
nsfg[mom_age %in% c("25-29 years", "30 or older"),mom_age_cat := "25 or older"]
nsfg[is.na(mom_age), mom_age_cat := "Non-trad mother"] #Note setting non-bio mother and no mother to non-trad
nsfg[,mom_age_cat := factor(mom_age_cat, levels = c("24 or younger","25 or older","Non-trad mother"))]

#Using hispanic recode as recommended: https://www.cdc.gov/nchs/data/nsfg/NSFG-2017-2019-UG-App3a-FemRespRecodeSpecs-508.pdf
nsfg[,ethnicity := ifelse(hispanic %in% c("HISPANIC","hispanic"), 1, 0)]
nsfg[,ethnicity := factor(ethnicity, levels = c(0,1), labels = c("non_hisp","hisp"))]

nsfg[,household_comp := "single_parent"]
nsfg[hhpartyp %in% c("Both biological or both adoptive parents",
                     "Biological and step- or adoptive parent",
                     "Biological and step-or adoptive parent"),
     household_comp := "two_parents"]
nsfg[hhpartyp == "Other", household_comp := "other"]

nsfg[,household_comp := factor(household_comp,levels = c("single_parent","other","two_parents"))]

#https://www.cdc.gov/nchs/data/nsfg/NSFG-2017-2019-UG-App3a-FemRespRecodeSpecs-508.pdf
nsfg[,metro := tolower(metro)]
nsfg[,metro := factor(metro,levels = c("not msa","other msa", "principal city of msa"))]

#https://www.cdc.gov/nchs/data/nsfg/NSFG-2017-2019-UG-App3a-FemRespRecodeSpecs-508.pdf
nsfg[,hadsex1 := 0]
nsfg = nsfg[grepl("YES",hadsex), hadsex1 := 1]
nsfg = nsfg[grepl("NO",hadsex), hadsex1 := 0]
nsfg[,hadsex := factor(hadsex1, levels = c(0,1))]
nsfg[,hadsex1 := NULL]

nsfg[,nhhmembers := substring(roscnt,1,1)]
nsfg[,age := factor(age_r, levels = c(15,16,17,18,19))]
nsfg[,goschol1 := factor(goschol, levels = c("No","Yes"), labels = c(0,1))]

nsfg[,race := tolower(race)]
nsfg[,race_eth := race]
nsfg[ethnicity == "hisp", race_eth := "hisp"]
nsfg[ethnicity == "non_hisp" & race == "white", race_eth := "non-hisp white"]
nsfg[ethnicity == "non_hisp" & race == "black", race_eth := "non-hisp black"]
nsfg[,race_eth := factor(race_eth,  levels = c("non-hisp black","hisp","non-hisp white","other"))]


nsfg = nsfg[,.(year, year_cat, age,hadsex, race,sex = sex,
               nhhmembers_cat, metro,grade,
               household_cat,mom_age_cat,
               nhhmembers_orig = roscnt, weight, household_comp,
               strata = sest, cluster = secu, goschol = goschol1,
               race_eth)] 

nsfg[,survey := "nsfg"]


saveRDS(nsfg, paste0(out.path,"NSFG_Raw_final.RDS"))
















