

rm(list=ls())



library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rgdal)
library(broom)
library(rgeos)
library(weights)
library(spdep)
library(proj4)
library(maptools)
library(urbnmapr)
library(covidcast)
library(viridis)
library(boot)
library(lme4)


#Filepaths
in.path = "~/data_counties/Data/Processed/"
out.path = "~/counties-project/Results/"


acs = readRDS(paste0(out.path,"080523_ACS_predictions.RDS"))
acs_sample_sizes = readRDS(paste0(in.path,"ACS_n_size.RDS"))
yrbs = readRDS(paste0(in.path,"YRBS_final.RDS"))
yrbs$age = factor(yrbs$age, levels = c(15,16,17,18,19)) ##Dunno why this isn't saving in data processing
acs_sample_sizes$age = factor(acs_sample_sizes$age, levels = c(15,16,17,18,19)) 

acs_state = acs[,.(goschol,sitecode=state, state_code, county_prediction, pop2019, county_fips,  age, sex,weight)]
acs_state = merge(acs_state,acs_sample_sizes)


######################
### Main Estimates ###
######################

#Construct raking factor on in school population only
acs_state_school = acs_state[goschol == 1]
yrbs = yrbs[,.(sitecode, weight, hadsex, age, sex)]

##Strata, state average
county_wts = acs_state_school[,.(weight_c = sum(weight)), by = c("county_fips","age","sex")]
acs_state_school = merge(acs_state_school,county_wts, by = c("county_fips","age","sex"))
yrbs_strata = yrbs[,.(yrbs_state = weighted.mean(hadsex, weight), n_yrbs = .N), by = c("age","sex","sitecode")]
acs_strata = acs_state_school[,.(acs_state = weighted.mean(county_prediction,weight_c)), by = c("age","sex","sitecode","state_code")]
acs_strata = merge(acs_strata, acs_sample_sizes)

##Add all other states
predicted = readRDS(paste0(out.path,"/yrbs_pred_model/090623_prediction_results.RDS"))[,type := "predict"]
extrap = readRDS(paste0(out.path,"/extrapolation_model/090623_prediction_results.RDS"))[,type := "extrap"]
impute = readRDS(paste0(out.path,"/yrbs_impute_model/090623_prediction_results.RDS"))[,type := "impute"]
setnames(predicted,"state","sitecode")
setnames(extrap,"state","sitecode")
setnames(impute,"state","sitecode")

yrbs_strata = rbind(yrbs_strata[,type := "direct"], predicted, extrap, impute, fill=T)

#Create 'raking factor', i.e. ratio of state level estimates, only on 'in school' population
combined = merge(acs_strata, yrbs_strata, by = c("age","sex","sitecode"),  all = TRUE)
combined[,scale_factor := acs_state/yrbs_state]
combined[,logit_scale_factor := logit(acs_state)/logit(yrbs_state)]

saveRDS(combined,paste0(out.path,"073123_ACS_YRBS_combined.RDS"))

#Adjust the county values according to scaling factor
setnames(acs,"state","sitecode")
acs = merge(acs, combined[,.(age,sex,sitecode,scale_factor,yrbs_state)], by = c("age","sex","sitecode"))
acs[,raked := county_prediction/scale_factor]

#County predictions
county_wts_all = acs_state[,.(weight_c = sum(weight)), by = c("county_fips","age","sex")]
county_dat = copy(acs)
county_dat = merge(county_dat,county_wts_all, by = c("county_fips","age","sex"),all.x=TRUE)
county_dat = county_dat[,.(county_raked = weighted.mean(raked,weight_c), ##Note that weighting here does not make any difference
                           county_unraked = weighted.mean(county_prediction,weight_c)), 
                           by=c("sitecode", "county_fips","cntyname", "age","sex")]

saveRDS(county_dat,paste0(out.path,"073123_ACS_raked.RDS"))
#saveRDS(county_dat,paste0(out.path,"090822_ACS_raked.RDS")) #No weights for means and updated n_acs

county_ranges = county_dat %>% group_by(age,sex) %>% summarise(county_min = min(county_raked,na.rm=T), 
                                               county_max = max(county_raked,na.rm=T))
state_ranges = yrbs_strata %>% group_by(age,sex) %>% summarise(state_min = min(yrbs_state,na.rm=T), 
                                                               state_max = max(yrbs_state,na.rm=T))
county_dat[,source := "Raked county estimate"]
yrbs_strata[,source := "State estimate"]
two = rbind(county_dat[,.(age,sex,value = county_raked,source)], 
      yrbs_strata[,.(age,sex,value = yrbs_state, source)])

ggplot(county_dat, aes(age,county_raked)) + geom_violin() + facet_wrap(~sex)
gg = ggplot(two, aes(age,value, fill=source)) + geom_violin() + facet_wrap(~sex) + theme_bw() +
  labs(y = "Proportion who ever had sex", x = NULL) + scale_fill_viridis_d() +
  theme(axis.text.x = element_text(size = 10, face="bold",  colour = "black"),
        axis.text.y = element_text(size = 12,  colour = "black"),
        axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 16),
        strip.background = element_rect(fill="white"),
        strip.text = element_text(size = 12, face="bold"),
        plot.caption = element_text(size = 10),
        legend.text = element_text(size = 10)) 


#####################
### Uncertainty #####
#####################
#library(rsample)
#load(paste0(in.path,"state_prediction.RData"))
#n=100
set.seed(12345)
#xx <- rsample::bootstraps(all_pred_new, times = n, strata="sitecode")

#100 resamples
# yrbs_resample <- lapply(xx$splits, function(x){
#   x <- data.table(analysis(x))
#   state_years_w_responses = unique(x[!is.na(q58), .(sitecode, year)])
#   state_years_w_responses = state_years_w_responses[year == 2019]
#   subset_dt = merge(x, state_years_w_responses, by = c("sitecode","year"))
#   subset_dt = subset_dt[age %in% 4:7]
#   subset_dt = subset_dt[!is.na(sex) & !is.na(age) & !is.na(q58)]
#   subset_dt[,id := paste0(sitecode,age,sex)]
#   subset_dt[, hadsex := factor(q58, levels = c(1,2), labels = c(1,0))]   # Change Y to factor
#   subset_dt[, hadsex := as.numeric(as.character(hadsex))]  # Ensure 0 is the reference category
#   subset_dt = subset_dt[,.(yrbs_state = weighted.mean(hadsex, weight)), by = c("age","sex","sitecode")]
#   return(subset_dt)
# })


#saveRDS(yrbs_resample,paste0(in.path,"yrbs_resample.RData"))
yrbs_resample = readRDS(paste0(in.path,"yrbs_resample.RData"))
yrbs_cast = rbindlist(yrbs_resample)
yrbs_cast[,id := rep(1:100, each = 288)]
yrbs_cast = dcast(yrbs_cast, age + sex + sitecode ~ id, value.var = "yrbs_state")
colnames(yrbs_cast) <- c("age","sex","sitecode",paste0("pred_",1:100))

uncertainty = readRDS(paste0(out.path,"080523_ACS_uncertainty.RDS"))
uncertainty_state = readRDS(paste0(out.path,"080523_ACS_uncertainty_state.RDS"))
uncertainty_school = readRDS(paste0(out.path,"080523_ACS_uncertainty_in_school.RDS"))

#Add estimated states
predicted = readRDS(paste0(out.path,"/yrbs_pred_model/090623_uncertainty.RDS"))
extrap = readRDS(paste0(out.path,"/extrapolation_model/090623_uncertainty.RDS")) ###NEED TO LIMIT THIS TO EXTRAPOLATED STATES
impute = readRDS(paste0(out.path,"/yrbs_impute_model/090623_uncertainty.RDS"))

setnames(acs,"state","sitecode", skip_absent = T)
uncertainty <- merge(unique(acs[,.(sitecode,county_fips)]),uncertainty, by = c("county_fips"))
uncertainty_school <- merge(unique(acs[,.(sitecode,county_fips)]),uncertainty_school, by = c("county_fips"))

yrbs_cast[,age := as.numeric(as.character(factor(age, levels = c(4,5,6,7), labels = c(15,16,17,18))))]
yrbs_cast[,sex1 := ifelse(sex == 1,"female","male")]
yrbs_cast[,sex := sex1]
yrbs_cast[,sex1 := NULL]

split_states1 = split(yrbs_cast,yrbs_cast$sitecode)
predicted[, run := paste0("pred_",run)]
predicted1 = dcast(predicted[,.(sitecode = state, age , sex, yrbs_state, run )], 
                   sitecode + age + sex ~ run, value.var = "yrbs_state"  )
setnames(extrap,"state","sitecode")
setnames(impute,"state","sitecode")
split_states2 = split(predicted1,predicted1$sitecode)
split_states3 = split(impute,impute$sitecode)
split_states4 = split(extrap,extrap$sitecode[extrap$sitecode %in% c("DE","HI","IN","NJ","NM","TN","WY")])

split_states <- c(split_states1,split_states2,split_states3,split_states4)


bind_all = data.table()

for(a in 15:18){
  out1 = lapply(lapply(split_states,get_rf_uncertainty,a,"female"),"[[",1)
  out1 = rbindlist(out1)
  bind_all = rbind(bind_all,out1)
}

for(a in 15:18){
  out1 = lapply(lapply(split_states,get_rf_uncertainty,a,"male"),"[[",1)
  out1 = rbindlist(out1)
  bind_all = rbind(bind_all,out1)
}

all_dat = readRDS(paste0(out.path,"061223_ACS_raked.RDS"))
county_out[,age := factor(age)]
all_dat = merge(county_out, all_dat, by = c("county_fips","age","sex"))
ggplot(all_dat, aes(mean, county_raked)) + geom_point() + geom_abline(slope = 1, intercept = 0)



###################
#  Compare Means  #
###################
combined = readRDS(paste0(out.path,"061223_ACS_YRBS_combined.RDS"))
check = uncertainty_state[,5:ncol(uncertainty_state)]
melt_uncertainty = uncertainty_state %>% select(-replicate) %>% melt(id.vars = c("age","sex","state")) 
est = melt_uncertainty %>% group_by(age,sex,state) %>% summarise(mean = mean(value)) %>% data.table()
est_original = combined[,.(age,sex,state = sitecode, acs_state)]
est[,age := factor(age)]
all = merge(est, est_original, by = c("age","sex","state"))

ggplot(all, aes(acs_state,mean)) + geom_point() + 
  geom_abline(slope = 1,intercept = 0) + facet_wrap(~age + sex, scales = "free")

est[age == 15 & sex == "female" & state == "AK", mean]
est_original[age == 15 & sex == "female" & state == "AK", acs_state]
wight_cols = c(paste0("REPWT",1:80))
tt = acs[age == 15 & sex == "female" & sitecode == "AK"]
tt = tt[,wight_cols,with=FALSE]
apply(tt,1,mean)



#Raking factor
yrbs_cast <- yrbs_cast[rep(seq_len(nrow(yrbs_cast)), 81),] #Expanding to facilitate uncertainty calc
yrbs_cast <- yrbs_cast[order(sitecode, sex, age)]
uncertainty_state1 <- copy(uncertainty_state)[state %in% unique(yrbs_cast$sitecode)]
uncertainty_state1 <- uncertainty_state1[order(state,sex,age)]

create_seq1 = seq(1,nrow(uncertainty_state)+81,by=81*4*2)

#Raked county predictions

uncertainty1 <- merge(unique(acs[,.(sitecode,county_fips)]),uncertainty, by = c("county_fips"))
uncertainty1 <- uncertainty1[sitecode %in% unique(yrbs_cast$sitecode)]
uncertainty1 <- uncertainty1[order(sitecode,sex,age)]

state = length(unique(yrbs_cast$sitecode))
raking_factors <- vector("list",state)
county_raked <- vector("list",state)



for(i in 1:1){
  
  yrbs1 = yrbs_cast[create_seq1[i]:(create_seq1[i+1]-1),4:ncol(yrbs_cast)]
  acs_state = uncertainty_state1[create_seq1[i]:(create_seq1[i+1]-1),5:ncol(uncertainty_state)]
  acs_county = uncertainty1[create_seq1[i]:(create_seq1[i+1]-1),6:ncol(uncertainty1)]
  out = as.matrix(acs_state,nrow = 81,ncol = 100)/yrbs1
  raking_factors[[i]] <- cbind(uncertainty_state[create_seq1[i]:(create_seq1[i+1]-1),.(age,sex,state)],out)
  out_final = as.matrix(acs_county,nrow = 81,ncol = 100)/out
  county_raked[[i]] <- cbind(uncertainty1[create_seq1[i]:(create_seq1[i+1]-1),.(county_fips,age,sex,state)],out_final)
  
  
}

