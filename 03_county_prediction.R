
rm(list=ls())



library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(parallel)
library(urbnmapr)
library(rsample)


#Filepaths
in.path = "~/data_counties/Data/Processed/"
out.path = "~/counties-project/Results/"

#Arugments
vars = c("sex", "age", "year","race_eth","household_cat","metro",
         "mom_age_cat","nhhmembers_cat","goschol","household_comp")
outcome = "hadsex1"
weights = "weight"

source("~/counties-project/model_functions.R")


#Generate predictions for ACS (no need to redo every time)
m1 = readRDS(paste0(out.path, "042623_best_model.RDS"))
bs_model = readRDS(paste0(out.path, "042623_bootstrap_model.RDS"))
acs = readRDS(paste0(in.path,"ACS_final.RDS"))

acs$age = factor(acs$age, levels = c(15,16,17,18)) ##Dunno why this isn't saving in data processing
acs$year = factor(acs$year, levels = c(2019))


acs$prediction = all_preds(m1=m1, data_test=acs, vars=vars,outcome=outcome,weight=TRUE, type="logistic")
acs$prediction <- as.numeric(acs$prediction)
acs[,county_prediction := weighted.mean(prediction, weight), by = c("county_fips","age","sex")]
acs_school <- copy(acs)
acs_school[,county_prediction := weighted.mean(prediction, weight), by = c("county_fips","age","sex","goschol")]
#acs[,county_prediction_logit := weighted.mean(prediction, weight), by = c("county_fips","age","sex")]
saveRDS(acs, paste0(out.path,"080523_ACS_predictions.RDS"))


###############
# Uncertainty #
###############

#Create ACS bootstraps
set.seed(12345)
n = 50
xx <- rsample::bootstraps(acs[,c(vars,"weight","state_code","strata",
                                 "county_fips","stat1"),with=FALSE], 
                                  times = n, strata="stat1")


acs[,stat1 := paste0(PUMA,state_code)]

##Potential process
#1. Bootstrap
#2. Remove age sex
#3. Set unique values
#5. Predict values
#6. Weight by county

# sampling with replacement from "cluster" 
acs_tt <- unique(acs[,vars,with=FALSE])

# reconstructing the overall simulated sample
acs_15_f <- acs[age == 15 & sex == "female"]
cls.resample <- merge(cls.col, acs_15_f, by="strat")


#10,000 predictions from bootstrap model x bootstrap data
all_pred_bs = lapply(xx$splits, function(x){
  x = analysis(x)
  acs_tmp = copy(x)
  print("x")
  acs_tmp = acs_tmp[order(county_fips,sex,age)]
  for(l in 1:length(bs_model)){
    bs_acs = all_preds(m1=bs_model[l][[1]], 
                       data_test=acs_tmp,
                       vars=vars,outcome=outcome,weight=TRUE, type="logistic")
    bs_acs <- as.numeric(bs_acs[,1])
    acs_tmp[,paste0("pred_",l) := bs_acs ]

    } 
  return(acs_tmp)
})


set.seed(12345)
n = 50
acs[,strat := paste0(PUMA,"_",state_code)]
xx2 <- rsample::bootstraps(data.table(strat = unique(acs$strat)), times = n)

new_list = vector("list",length = 4L)
names(new_list) <- c(15:18)
for(age1 in c(15:18)){
  print(age1)
  all_pred_bs1 = lapply(xx2$splits, function(cls){
    cls = analysis(cls)
    cls.col <- data.table(strat=cls$strat)
    acs_f <- acs[age == age1 & sex == "female"]
    cls.resample <- merge(cls.col, 
                          acs_f[,c(vars,"weight","state_code","strat",
                                  "county_fips","state"),with=FALSE], by="strat") %>% data.table()
    for(l in 1:length(bs_model)){
      if(l %in% c(1,20,40,60,80,100)) print(l)
      bs_acs = all_preds(m1=bs_model[l][[1]], 
                         data_test=cls.resample,
                         vars=vars,outcome=outcome,weight=TRUE, type="logistic")
      bs_acs <- as.numeric(bs_acs[,1])
      cls.resample[,paste0("pred_",l) := bs_acs ]
      
      
    } 
    return(cls.resample)
  })
  new_list[age1 - 14] <- all_pred_bs1
}

#Create ACS bootstraps using reps
#Organize predictions as reps x pred
all_pred_save <- all_pred_bs
for(i in 1:n){
  all_pred_save[[i]] <- all_pred_save[[i]][,rep := i]
}

saveRDS(all_pred_save, paste0(out.path,"county_prediction_bootstraps.RDS"))

all_pred_save_test <- rbindlist(all_pred_save[1:2])

county_means = lapply(all_pred_save,function(x){
  print("xx")
  tt1 = x %>% 
    group_by(age,sex,county_fips,rep, goschol) %>%
    summarise_at(vars(starts_with('pred')), funs(weighted.mean(., weight))) %>% 
    data.table() 
  return(tt1)
})

county_means_list = rbindlist(county_means)
county_means_list = county_means_list[order(county_fips,age,sex)]

county_means_list_n_strata = rbindlist(county_means)
county_means_list_n_strata = county_means_list_n_strata[order(county_fips,age,sex)]

#Check means
pred_cols = paste0("pred_",1:100)
get_preds = county_means_list[,c(pred_cols),with=FALSE]
get_preds = apply(get_preds, 1, mean)

compare1 = acs[,.(state_code, age, sex, prediction)]
compare1[,bs_pred := get_preds]
compare1[,diff := bs_pred - prediction]
compare1[abs(diff) > 0.03]
ggplot(compare1[state_code == "01"], aes(prediction,bs_pred)) + geom_point() + 
  geom_abline(slope = 1, intercept = 0) + facet_wrap(~age + sex, scales = "free_y")

#Check weighted means by state
pred_cols = paste0("pred_",1:100)
acs_tmp_m = melt(acs_tmp[,c("state_code","age","sex","weight",pred_cols),with=FALSE], 
                 id.vars = c("state_code","age","sex","weight"))
means1 = acs_tmp_m[,.(wtdmean = weighted.mean(value, weight)), by = c("age","sex","state_code")]
means2 = acs_tmp[,.(wtdmean = weighted.mean(prediction, weight)), by = c("age","sex","state_code")]
all_means = merge(means1,means2,by=c("age","sex","state_code"))
ggplot(all_means, aes(wtdmean.x,wtdmean.y)) + geom_point() + geom_abline(slope = 1, intercept = 0)



#Check weighted means by county
pred_cols = paste0("pred_",1:100)
acs_tmp_m = melt(acs_tmp[,c("county_fips","age","sex","weight",pred_cols),with=FALSE], 
                 id.vars = c("county_fips","age","sex","weight"))
means1 = acs_tmp_m[,.(wtdmean = weighted.mean(value, weight)), by = c("age","sex","county_fips")]
means2 = acs_tmp[,.(wtdmean = weighted.mean(prediction, weight)), by = c("age","sex","county_fips")]
all_means = merge(means1,means2,by=c("age","sex","county_fips"))
ggplot(all_means, aes(wtdmean.x,wtdmean.y)) + 
  geom_point() + geom_abline(slope = 1, intercept = 0) + facet_wrap(~age + sex)


#Use bootstraps, each combined with 80 replicate weights for full uncertainty, overall and in-school at state-level
age = 1:4 ; ages = 15:18
sex = 1:2 ; sexes = c("male","female")
counties = 1:length(unique(acs$county_fips)); counties_x = unique(acs$county_fips)
resamples = 80 + 1
bootstrap = length(bs_model)

rep_weights = acs_tmp[,grep("REP",colnames(acs),value = TRUE)]
uncertainty = data.table(expand.grid(county_fips = counties_x,
                                     age = ages, sex = sexes,
                                     replicate = rep_weights))

bs_preds = acs_tmp[,grep("pred_",colnames(acs),value = TRUE)]


all_uncertainty = lapply(1:length(bs_model), function(x){
                            print(x)
                            nn = x
                            est = acs_tmp %>% group_by(age,sex,county_fips) %>%
                                  summarise_at(vars(starts_with('REP')), 
                                  funs(weighted.mean(get(paste0("pred_",nn)),weight*.))) %>% data.table()
                            est = melt(est,id.vars = c("age","sex","county_fips"), value.name = paste0("pred_",nn)) 
                            setnames(est, "variable","replicate")
                            est[,age := as.numeric(as.character(age))]
                            return(est)
                            })


all_dat = Reduce(function(d1, d2) merge(d1, d2, by = c("age","sex","county_fips","replicate")),all_uncertainty)

all_uncertainty_school = lapply(1:length(bs_model), function(x){
  print(x)
  nn = x
  est = acs_tmp %>% filter(goschol == 1) %>% group_by(age,sex,county_fips) %>%
    summarise_at(vars(starts_with('REP')), 
                 funs(weighted.mean(get(paste0("pred_",nn)),weight*.))) %>% data.table()
  est = melt(est,id.vars = c("age","sex","county_fips"), value.name = paste0("pred_",nn)) 
  setnames(est, "variable","replicate")
  est[,age := as.numeric(as.character(age))]
  return(est)
})


all_dat_school = Reduce(function(d1, d2) merge(d1, d2, by = c("age","sex","county_fips","replicate")),all_uncertainty_school)

all_uncertainty_state = lapply(1:length(bs_model), function(x){
  print(x)
  nn = x
  est = acs_tmp %>% filter(goschol == 1) %>% group_by(age,sex,state) %>%
    summarise_at(vars(starts_with('REP')), 
                 funs(weighted.mean(get(paste0("pred_",nn)),weight*.))) %>% data.table()
  est = melt(est,id.vars = c("age","sex","state"), value.name = paste0("pred_",nn)) 
  setnames(est, "variable","replicate")
  est[,age := as.numeric(as.character(age))]
  return(est)
})

all_dat_state = Reduce(function(d1, d2) merge(d1, d2, by = c("age","sex","state","replicate")),all_uncertainty_state)


saveRDS(all_dat, paste0(out.path,"080523_ACS_uncertainty.RDS"))
saveRDS(all_dat_school, paste0(out.path,"080523_ACS_uncertainty_in_school.RDS"))
saveRDS(all_dat_state, paste0(out.path,"080523_ACS_uncertainty_state.RDS"))

#saveRDS(acs, paste0(out.path,"081122_ACS_predictions.RDS"))
#saveRDS(all_dat, paste0(out.path,"081122_ACS_uncertainty.RDS"))

#NSFG uncertainty only
unc = readRDS(paste0(out.path,"080523_ACS_uncertainty.RDS"))
ex1 = unc[county_fips == "04013" & replicate == "REPWT"]
ex2 = unc[county_fips == "01033" & replicate == "REPWT"]


ex1 = melt(ex1, id.vars = c("age","sex","county_fips","replicate"))
variances = ex1[,.(variance = var(value)), by = c("age","sex")]
ggplot(ex1, aes(value)) + geom_histogram() + 
  facet_grid(age ~ sex) + theme_bw() +
  geom_text(data = variances,aes(x = 0.5,y = 40, 
                                 label = round(variances$variance,5)))


ex2 = melt(ex2, id.vars = c("age","sex","county_fips","replicate"))
variances = ex2[,.(variance = var(value)), by = c("age","sex")]
ggplot(ex2, aes(value)) + geom_histogram() + 
  facet_grid(age ~ sex) + theme_bw() +
  geom_text(data = variances,aes(x = 0.5,y = 30, 
                                 label = round(variances$variance,5)))


