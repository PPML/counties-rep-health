
##This script processes the raw YRBS data, downloadded from public use files
##It also checks for data availability in different states

rm(list=ls())
library(readstata13)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(survey)
library(rgdal)
library(broom)
library(rgeos)
library(stringr)
library(matrixStats)
library(haven)
library(urbnmapr)
library(openxlsx)
library(Hmisc)
library(geojsonio)


#Filepaths
in.path = "~/data_counties/Data/Raw/"
out.path = "~/data_counties/Data/Processed/"

#######################
##  YRBS  National ###
######################
#Codebook: https://www.cdc.gov/healthyyouth/data/yrbs/pdf/2019/2019_National_YRBS_Data_Users_Guide.pdf

dt_nat = fread(paste0(in.path,"yrbs_national.csv"))

#q58 = ever had sex
nat = dt_nat[,.(year = 2019, weight, stratum, PSU=psu, sitename = "National", age = q1, sex = q2, grade = q3,   race = raceeth, hadsex = q58)]
#Note n = 72 observations with missing age
nat = nat[age >= 4 & age <= 7]
nat[age == 4, age := 15]
nat[age == 5, age := 16]
nat[age == 6, age := 17]
nat[age == 7, age := 18]

nat[hadsex == 1, hadsex := 1]
nat[hadsex == 2, hadsex := 0]

dt_all = nat
dt_all = dt_all[!is.na(age) & !is.na(sex) & !is.na(grade) & !is.na(race) & !is.na(hadsex)]
dt_all[,sex := ifelse(sex==1, "female", "male")]
dt_all[,race_eth  := "NEW"]
dt_all[race %in% c(1,2,4,8), race_eth := "other"]
dt_all[race %in% c(5), race_eth  := "non-hisp white"]
dt_all[race %in% c(3), race_eth  := "non-hisp black"]
dt_all[race %in% c(6,7), race_eth  := "hisp"]
dt_all[race == 0, race_eth  := NA]

saveRDS(dt_all,paste0(out.path,"YRBS_national.RDS" ))

#######################
##  YRBS  Districts ###
######################
dt_all = fread(paste0(in.path, "yrbs_districts.csv"))
dt_all = dt_all[,.(year,weight, stratum, PSU, sitename,age, sex, grade, race = race4,hadsex =q58,agesex =q59,numbersex =q60)]

dt_all[,sex := ifelse(sex==1, "female", "male")]
dt_all[,race_eth := "NEW"]
dt_all[race %in% c(4), race_eth := "Other"]
dt_all[race %in% c(1), race_eth := "non-hisp white"]
dt_all[race %in% c(2), race_eth := "non-hisp black"]
dt_all[race %in% c(3), race_eth := "hisp"]
dt_all[is.na(race), race_eth := NA]

districts = dt_all
districts = districts[age >= 4]
districts[age == 4, age := 15]
districts[age == 5, age := 16]
districts[age == 6, age := 17]
districts[age == 7, age := 18]

districts[hadsex == 1, hadsex := 1]
districts[hadsex == 2, hadsex := 0]

saveRDS(districts,paste0(out.path,"YRBS_districts.RDS"))


###################
##  YRBS  State ###
###################

## Load public datasets - only needed to be done once
# in.path = "~/data_counties/Data/Raw/"
# states_a_m = fread(paste0(in.path,"table_a_m.csv"))
# states_n_z = fread(paste0(in.path,"table_n_z.csv"))
# states_combined = rbind(states_a_m, states_n_z)
# fwrite(states_combined, paste0(in.path,"SADCQ_2019_State.csv"))

states_combined_dt = fread(paste0(in.path,"SADCQ_2019_State.csv"))
states_combined_dt[sitecode == "AZB", sitecode := "AZ"]

state_svy_qn <- svydesign(id = ~PSU, weight = ~weight, strata = ~stratum,
                          data = states_combined_dt, nest = TRUE)

#old = fread("~/Documents/lgbtq_project/From Johannes/SADCQ_2017_State.csv")

#Necessary ID variables:
id_vars = c(
  'sitecode', 'year', 'weight', 'stratum', 'psu'
)

varimp_inds = as.data.table(read.xlsx(paste0('~/data_counties/colname_mapping_2019.xlsx')))
modeling_vars = varimp_inds[, .(var, yrbs_2017)] #varimp_inds[incl_v2 == "y", var]
missing_2019 <- modeling_vars[is.na(var)] 
modeling_vars = modeling_vars[ ,var]
modeling_vars = modeling_vars[!is.na(modeling_vars)]##Note NA col due to q40

#Public dataset subset
ss_vars = c(id_vars,modeling_vars)
public_ss_vars = union(ss_vars, c('race4'))
public_ss_vars = tolower(public_ss_vars)
public_ss_vars = public_ss_vars[!is.na(public_ss_vars)]
colnames(states_combined_dt) = tolower(colnames(states_combined_dt))
public_state_subset = states_combined_dt[, ..public_ss_vars]
public_subset = states_combined_dt[, ..public_ss_vars]

# Load individual state data
stateFiles = paste0(in.path,grep("", list.files(in.path), value = T))
stateFiles = c(stateFiles[grepl("MA",stateFiles)], 
               stateFiles[grepl("OH",stateFiles)],
               stateFiles[grepl("IN",stateFiles)])
state_DTs = sapply(stateFiles[grepl(".csv",stateFiles)], fread, USE.NAMES = T, simplify = F)
for (dt in state_DTs) {
  setnames(dt, tolower(colnames(dt)))
}
names(state_DTs) <- gsub(in.path,"", names(state_DTs))


#Map variables to public column names & recode the correct race variable
map_variables_to_public_colnames = function(in_dt, sitecode, year, mapping_dt, in_col_names_in_mapping_dt) {
  
  out_dt = copy(in_dt)
  for(i in seq(1, nrow(mapping_dt))) {
    out_colname = mapping_dt$var[i]
    in_colname = mapping_dt[, get(in_col_names_in_mapping_dt)][i]
    if(is.na(in_colname)) {
      out_dt[, (out_colname) := as.numeric(NA)]  
    } else {
      out_dt[, (out_colname) := in_dt[, as.numeric(get(in_colname))]] 
    }
  }
  
  out_dt[, year := year]
  out_dt[, sitecode := sitecode]
  
  if("raceeth" %in% colnames(out_dt)){
    out_dt[,race7 := 1]
    out_dt[raceeth == 1, race7 := 1] #Alaska Native
    out_dt[raceeth == 2, race7 := 2] #Asian
    out_dt[raceeth == 3, race7 := 3] #Black
    out_dt[raceeth == 4, race7 := 5] #4 Native Hawaiian/other vs 5
    out_dt[raceeth == 5, race7 := 6] #White
    out_dt[raceeth == 6, race7 := 4] #Hispanic/latino 
    out_dt[raceeth == 7, race7 := 4] #multiple - hispanic/latino
    out_dt[raceeth == 8, race7 := 7] #mutiple non-hispanic
    out_dt[is.na(raceeth), race7 := NA]
    
  }
  
  out_dt = out_dt[, ..ss_vars]
  out_dt[, public := "N"]
  return(out_dt)
}


# MA 2019
ma_2019_out = map_variables_to_public_colnames(
  in_dt = state_DTs$MAH2019_YRBS_Data.csv,
  sitecode = "MA",
  year = 2019,
  mapping_dt = varimp_inds[, .(var, MA_2019)],
  in_col_names_in_mapping_dt = "MA_2019"
)

#MA 2017
ma_2017_out = map_variables_to_public_colnames(
  in_dt = state_DTs$MAH2017_YRBS_Data.csv,
  sitecode = "MA",
  year = 2017,
  mapping_dt = varimp_inds[, .(var, MA_2017)],
  in_col_names_in_mapping_dt = "MA_2017"
)

#MA 2015
ma_2015_out = map_variables_to_public_colnames(
  in_dt = state_DTs$MAH2015_YRBS_Data.csv,
  sitecode = "MA",
  year = 2015,
  mapping_dt = varimp_inds[, .(var, MA_2015)],
  in_col_names_in_mapping_dt = "MA_2015"
)

# OH 2019
oh_2019_out = map_variables_to_public_colnames(
  in_dt = state_DTs$OHH2019_YRBS_Data.csv,
  sitecode = "OH",
  year = 2019,
  mapping_dt = varimp_inds[, .(var, OH_2019)],
  in_col_names_in_mapping_dt = "OH_2019"
)


# OH 2013
oh_2013_out = map_variables_to_public_colnames(
  in_dt = state_DTs$OHH2013_YRBS_Data.csv,
  sitecode = "OH",
  year = 2013,
  mapping_dt = varimp_inds[, .(var, OH_2013)],
  in_col_names_in_mapping_dt = "OH_2013"
)

#IN 2015
in_2015_out = map_variables_to_public_colnames(
  in_dt = state_DTs$INH2015_YRBS_Data.csv,
  sitecode = "IN",
  year = 2015,
  mapping_dt = varimp_inds[, .(var, IN_2015)],
  in_col_names_in_mapping_dt = "IN_2015"
)

# Combined data
public_w_states = rbindlist(list(
  public_subset[, public := "Y"], 
  oh_2019_out,
  ma_2019_out,
  ma_2017_out,
  ma_2015_out,
  oh_2013_out,
  in_2015_out
  
), fill=T)


#Identify missing columns - double checked these are truly missing
all_cols = colnames(public_w_states)

for(c in all_cols) {
  subset_dt = public_w_states[public == "N"]
  subset_dt = subset_dt[, .(non_missing = sum(!is.na(get(c)))), by = c('sitecode', 'year')]
  if(nrow(subset_dt[non_missing==0])>0){
    print(paste("Column:", c, ", missing in:"))
    print(subset_dt[non_missing==0, paste(sitecode,year)])
  }
}


#Look at differences in max/min modeling_vars from public dataset when not missing
# Get rid of weird values identified below
public_w_states[grade == 5, grade := NA]

for(c in modeling_vars) {
  
  # Check that have non-missing values in non-public; otherwise next
  if(public_w_states[public == "N" & !is.na(get(c)), .N] == 0) {
    next
  }
  
  public_min = public_w_states[public == "Y" & !is.na(get(c)), min(get(c))]
  public_max = public_w_states[public == "Y" & !is.na(get(c)), max(get(c))]
  
  # non-public min and maxes by state-year
  nonpublicsaggs = public_w_states[public == "N" & !is.na(get(c)), .(
    min = min(get(c)),
    max = max(get(c))
  ), by = c('sitecode', 'year')]
  
  # subset to values outside public data
  nonpublicsaggs = nonpublicsaggs[min < public_min | max > public_max]
  
  if(nrow(nonpublicsaggs)>0) {
    print(paste("Column:", c, ", weird in:"))
    print(nonpublicsaggs[, paste(sitecode,year)])
    nonpublicsaggs[, `:=`(public_min = public_min, public_max = public_max, col = c)]
    print(nonpublicsaggs)
  }
}

#Write out combined data
site_regions_divisions = fread("~/data_counties/state_regions_divisions.csv")
public_w_states = merge(public_w_states, site_regions_divisions, by='sitecode')

# filter to state - years with answers to q66 OR q67 (2019 version is 66 or 65)
public_w_states[, include := sum(!is.na(q66) | !is.na(q65)), by=c('sitecode','year')]
public_w_states[, include := sign(include)]
public_w_states[, table(include)]

# for(mm in unique(t1$sitecode)){
#   print(mm)
#  # if(mm %in% c("CT","GA","MD","NM","TX","VT")) next
#   al1 = t1[sitecode == mm]
#   al2 = public_w_states[sitecode == mm]
#   print(all.equal(table(al1$year),table(al2$year)))
# }

#CT, GA, MD, NM, TX, VT have extra historical years creating mismatch from 2017 data.
fwrite(public_w_states[include==1], paste0(out.path,'combined_pred_data.csv'))
fwrite(public_w_states, paste0(out.path,'combined_pred_data_all.csv'))

#Simplify 2019 data for use in raking
dt_small = public_w_states[,.(year,weight, stratum, psu, sitecode,age, sex, grade, race = race7,hadsex =q58)]

dt_small[,race_eth := "some"]
dt_small[race %in% c(1,2,5,7), race_eth := "other"]
dt_small[race == 3, race_eth := "non-hisp black"]
dt_small[race == 6, race_eth := "non-hisp white"]
dt_small[race == 4, race_eth := "hisp"]
dt_small[is.na(race), race_eth := NA]

yrbs = dt_small
yrbs[,survey := "yrbs"]

yrbs = yrbs[,age := age + 11]
yrbs = yrbs[,grade := grade + 8]

yrbs[hadsex == 1, hadsex := 1]
yrbs[hadsex == 2, hadsex := 0]

yrbs[,sex2 := "ff"]
yrbs[sex == 1, sex2 := "female"]
yrbs[sex == 2, sex2 := "male"]
yrbs[is.na(sex), sex2 := NA]
yrbs[,sex := sex2]
yrbs[,sex2 := NULL]


saveRDS(yrbs,paste0("~/data_counties/Data/Raw/YRBS_Raw_final.RDS"))

#############################
##  YRBS State Predictions ##
#############################
#all_pred = fread(paste0(in.path,"combined_pred_data_all.csv")) #From Jiayi - only to 2017
all_pred_new = fread(paste0(out.path,"combined_pred_data_all.csv"))
all_states = c(unique(all_pred_new$sitecode), "WA","OR","DC","MN")
pred_mat = data.table(expand.grid(sitecode = all_states, method = "none"))

#Note that there is a much higher degree of missingness in the new combined dataset.
#this is because it includes more historic years, e.g. 6680 missing age values for Vermont in 1993.
#Otherwise the missingness is the same.

all_pred_new[, include1 := sum(!is.na(q58)), by=c('sitecode','year')]
all_pred_new[, include1 := sign(include1)]

for(ps in unique(pred_mat$sitecode)){
  test = all_pred_new[sitecode == ps, .(year, q58)]
  mm <- "impute"
  
  if(nrow(test) > 0) { 
    mm <- "predict"
  } 
  
  if(sum(test[year > 2009 & year < 2019,.(!all(is.na(q58))), by = "year"]$V1) >= 1) {
    mm <- "extrapolate"
  }  
  if(nrow(test[year == 2019]) > 0){
      if(test[year  == 2019,.(!all(is.na(q58))), by="year"]$V1) {
        mm <- "direct"
      }
    } 

  pred_mat[sitecode == ps, method := mm]
}
  
# Choose predictor variables and setup training data 
state_years_w_responses = unique(all_pred_new[!is.na(q58), .(sitecode, year)])
subset_dt = merge(all_pred_new, state_years_w_responses, by = c("sitecode","year"))

varimp_inds = as.data.table(read.xlsx(paste0('~/data_counties/colname_mapping_2019.xlsx')))
modeling_vars = varimp_inds[, .(var, yrbs_2017)] #varimp_inds[incl_v2 == "y", var]
missing_2019 <- modeling_vars[is.na(var)] 
modeling_vars = modeling_vars[ ,var]
modeling_vars = modeling_vars[!is.na(modeling_vars)]##Note NA col due to q40

# Limit the data to modelling variables
id_vars = c('sitecode', 'census_region', 'census_division', 'year', 'weight')
ss_vars = intersect(c(id_vars, modeling_vars),colnames(subset_dt))
subset_dt = subset_dt[, ..ss_vars]

#Create prediction dataset in the same format
predict_dt = all_pred_new[,..ss_vars]
predict_dt = predict_dt[sitecode %in% pred_mat[method == "predict"]$sitecode]

save(list=c("subset_dt","pred_mat","varimp_inds","modeling_vars","predict_dt",  "all_pred_new" ), file="~/data_counties/Data/Processed/state_prediction.RData")

##YRBS data availability
dt = copy(all_pred_new)
dt[,year := as.numeric(as.character(year))]
since_2010 = dt[year >= 2010]

since_2010_q58 = copy(since_2010)[!is.na(q58), unique(sitecode),by="year"]
q58 = dcast(since_2010_q58, V1 ~ year)
any_yrbs = since_2010[, unique(sitecode),by="year"]
any = dcast(any_yrbs, V1 ~ year)

#No YRBS or available data
no_yrbs = c("WA","OR","DC","MN")

#Asks q58 in 2019
yrbs_2019 = q58[!is.na(q58[,`2019`]),`2019`]

#Asks q58 between 2010 and 2017
yrbs_pre_2019 = unique(q58[,c(`2011`,`2013`,`2015`,`2017`)])
yrbs_pre_2019 = unique(yrbs_pre_2019)[!unique(yrbs_pre_2019) %in% unique(yrbs_2019)]

#Never asks q58
no_q58 = unique(any_yrbs[,V1])[!unique(any_yrbs[,V1]) %in% unique(since_2010_q58[,V1])]


#Generate data availability map
spdf =  geojson_read("C:/Users/DeepaJahagirdar/Documents/counties-project/us_states_hexgrid.geojson", what = "sp") ##In git repo
#spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))

#spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "iso3166_2")

centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))
spdf_fortified <- data.table(spdf_fortified)
spdf_fortified[,yrbs := "no YRBS since 2009"]
spdf_fortified[id %in% yrbs_2019,yrbs := "Sex q available for 2019"]
spdf_fortified[id %in% yrbs_pre_2019,yrbs := "Sex q at least once since 2010"]
spdf_fortified[id %in% no_q58, yrbs := "Has YRBS but no sex q"]
spdf_fortified[id %in% no_yrbs, yrbs := "No YRBS or no available YRBS data"]


ggplot() +
  geom_polygon(data = spdf_fortified,
               aes( x = long, y = lat, group = group, fill = yrbs),  color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id)) +
  theme_void() +
  coord_map()



