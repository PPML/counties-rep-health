
#YRBS Districts data
#yrbs_districts.csv

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


#Filepaths
in.path = "~/Documents/data_counties/Data/Processed/"
out.path = "~/Documents/data_counties/Data/Processed/"

districts = readRDS(paste0(in.path, "YRBS_districts.RDS"))
districts = districts[year == 2019]

#Align strings
first_split = str_split_fixed(unique(districts$sitename), ",", 2)[,1]
second_split = gsub(" County","",first_split)
third_split = gsub("Borough of ","",second_split)

new_list = as.list(third_split)
names(new_list) <- third_split

for(search_district in third_split){

  matches = unique(acs[grepl(search_district, cntyname), cntyname])

  if(length(matches) > 0) {
    new_list[search_district] <- toString(matches)
  } else {
    new_list[search_district] <- NA
  }

}
  
#Use state when there are  multiple matches and double check correct state
sites = unique(districts$sitename)

#1) Orange
test = "Orange"
state = sites[grepl(test,sites)]
new_list[test] <- unique(acs[grepl(test, cntyname) & state == "FL", cntyname])

#2) Hillsborough
test = "Hillsborough"
state = sites[grepl(test,sites)]
new_list[test] <- unique(acs[grepl(test, cntyname) & state == "FL", cntyname])

#3) Duval
test = "Duval"
state = sites[grepl(test,sites)]
new_list[test] <- unique(acs[grepl(test, cntyname) & state == "FL", cntyname])

#4) Cleveland - no direct name match in ACS
test = "Cleveland"
state = sites[grepl(test,sites)]
new_list[test] <- NA

#5) Shelby
test = "Shelby"
state = sites[grepl(test,sites)]
new_list[test] <- unique(acs[grepl(test, cntyname) & state == "TN", cntyname])

#6) Oakland is incorrect - only Oakland, MA available in ACS
test = "Oakland"
new_list[test] <- NA

exact_match = unlist(new_list)
exact_match = exact_match[!is.na(exact_match)]

#Try to fill in NAs
test = "Albuquerque"
county = "Bernalillo"
new_list[test] <- unique(acs[grepl(county, cntyname) & state == "NM", cntyname])

test = "Cleveland"
county = "Cuyahoga"
new_list[test] <- unique(acs[grepl(county, cntyname) & state == "OH", cntyname])

test = "Chicago"
county = "Cook"
new_list[test] <- unique(acs[grepl(county, cntyname) & state == "IL", cntyname])

test = "Eaton Consortium"
county = "Eaton"
new_list[test] <- unique(acs[grepl(county, cntyname) & state == "MI", cntyname])

test = "Fort Worth"
county = "Tarrant"
new_list[test] <- unique(acs[grepl(county, cntyname) & state == "TX", cntyname])

test = "Genesee Consortium" 
#Appears to include 2 counties = Genesee and Lapeer county
county = "Genesee"
new_list[test] <- unique(acs[grepl(county, cntyname) & state == "MI", cntyname])

test = "Newark"
county = "Essex"
new_list[test] <- unique(acs[grepl(county, cntyname) & state == "NJ", cntyname])

test = "New York City"
county = "New York"
new_list[test] <- unique(acs[grepl(county, cntyname) & state == "NY", cntyname])

test = "Brooklyn"
county = "Kings"
new_list[test] <- unique(acs[grepl(county, cntyname) & state == "NY", cntyname])

test = "Manhattan"
county = "New York"
new_list[test] <- unique(acs[grepl(county, cntyname) & state == "NY", cntyname])

test = "Staten Island"
county = "Richmond"
new_list[test] <- unique(acs[grepl(county, cntyname) & state == "NY", cntyname])

test = "Oakland"
county = "Alameda"
new_list[test] <- unique(acs[grepl(county, cntyname) & state == "CA", cntyname])

test = "Portland"
county = "Multnomah"
new_list[test] <- unique(acs[grepl(county, cntyname) & state == "OR", cntyname])

test = "Seattle"
county = "King"
new_list[test] <- unique(acs[grepl(county, cntyname) & state == "WA", cntyname])

#Map back to ACS
match_key = data.table(yrbs_site = unique(districts$sitename),
                       cntyname = unlist(new_list))
match_key[cntyname %in% exact_match, exact := 1]
match_key[is.na(exact), exact := 0]

saveRDS(match_key,paste0(out.path,"ACS_YRBS_match.RDS"))










