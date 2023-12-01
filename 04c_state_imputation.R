
rm(list=ls())

library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
library(irr)
library(ranger)
library(glmnet)
library(weights)
library(MASS)
library(rsample)

#Functions
source("model_functions.R")

#Filepaths
in.path = "~/data_counties/Data/Processed/"
out.path = "~/counties-project/Results/yrbs_impute_model/"

yrbs = readRDS(paste0("~/data_counties/Data/Processed/YRBS_final.RDS"))
load(paste0(in.path,"state_prediction.RData"))

year_sub = 2019 #Optional subset year

##Bootstrap resamples prep
n=100
set.seed(12345)
xx <- rsample::bootstraps(all_pred_new, times = n, strata="sitecode")

#Table 47a - 2019 Minnesota Student Survey Statewide Tables 
mn = data.table(sitecode = "MN",
                grade = c(9,11,9,11),
                sex = c("female", "female","male","male"),
                hadsex = c(0.1,0.34,0.14,0.35),
                year  = 2019)


#Source: https://www.oregon.gov/oha/PH/BIRTHDEATHCERTIFICATES/SURVEYS/OREGONHEALTHYTEENS/Documents/2019/Gender/11th/Sex11.pdf
or = data.table(sitecode = "OR",
                grade = 11,
                sex = c("female","male"),
                hadsex = c(0.422,0.395),
                n = c(5685,5381),
                year = 2019)

mn_or_dat_prep = function(vs){
  
  if(!is.data.table(vs))  vs = data.table(analysis(vs))
  
  ##Organize the data
  outcome = "hadsex"
  year_sub = c(2019) #Optional subset year
  
  vs = vs[year %in% year_sub]
  state_years_w_responses = unique(vs[!is.na(q58), .(sitecode, year)])
  subset_dt = merge(vs, state_years_w_responses, by = c("sitecode","year"))
  
  get_2018 = subset_dt[year %in% c(2019),.(year,sitecode, age,sex, grade,hadsex = as.numeric(as.character(q58)), weight)]
  check_stats = data.table(table(get_2018$sitecode, get_2018$year))
  get_2018[!is.na(sex),sex1 := ifelse(sex == 1, "female","male")]
  get_2018[,sex := sex1]
  get_2018[,sex1 := NULL]
  get_2018[,hadsex := as.numeric(as.character(factor(hadsex, levels = c(1,2), labels = c(1,0))))]
  get_2018 = get_2018[complete.cases(get_2018)]
  get_2018 = get_2018[age %in% 4:7]
  
  all_age = copy(get_2018)[,.(hadsex = weighted.mean(hadsex, weight,na.rm=T)), by = c("sitecode","age",  "sex", "year")]
  all_grade = copy(get_2018)[,.(hadsex = weighted.mean(hadsex, weight,na.rm=T)), by = c("sitecode", "sex", "year","grade")]
  all_grade_c =  dcast(all_grade,  sitecode + year  + sex ~ grade  , value.var = "hadsex")
  check1 = merge(all_age, all_grade_c, by = c("sitecode","year","sex"), allow.cartesian = TRUE)
  check1[,age := factor(age)]
  
  age_c = list()
  for(i in 4:7){
    age_i = copy(get_2018[age == i])[,.(hadsex = weighted.mean(hadsex, weight,na.rm=T)), by = c("sitecode", "sex", "year","grade")]
    age_i_c =  dcast(age_4,  sitecode + year  + sex ~ grade  , value.var = "hadsex")
    age_i_c[,age := i]
    age_c = rbind(age_c, age_i_c)
  }
  
  return(check1)
  
}

all_yrbs = mn_or_dat_prep(all_pred_new)
all_yrbs[,sex := factor(sex)]
all_yrbs_bs  = lapply(xx$splits,mn_or_dat_prep)

################
## OREGON ###
################
n = 100

##Main estimates
f1 = paste0(" ~  age + ", paste0("`3`"), "+ sex + (1 | sitecode)")
m1 = lmer(as.formula(paste0(outcome,f1)), data = all_yrbs )
or_c = dcast(or, sitecode + year + sex ~ grade, value.var = "hadsex")
colnames(or_c) <- c("sitecode","year","sex","3")

or_c = rbind(or_c, or_c, or_c, or_c)
or_c[,age := factor(rep(c(4,5,6,7), each = 2))]
or_c[,prediction := predict(m1, newdata = or_c, allow.new.levels = T )]
or_c = or_c[,.(sitecode, year, sex, age, hadsex = prediction)]

#Uncertainty
for(rs in 1:length(all_yrbs_bs)){
  f1 = paste0(" ~  age + ", paste0("`3`"), "+ sex + (1 | sitecode)")
  m = lmer(as.formula(paste0(outcome,f1)), data = all_yrbs_bs[[rs]])
  sd = attr(VarCorr(m)[[1]],"stddev")
  set.seed(12345)
  par <- mvrnorm(n=n, mu = apply(coef(m1)[[1]],2,mean), Sigma = vcov(m,complete=TRUE)) #FE variance 
  set.seed(12345)
  re_sd <- rnorm(n=n,mean=0,sd=sd) #RE variance

  preds_bs_var1 = c()

  for(num in 1:100){
    ###NOTE: THIS IS NOT WORKING, AS MODEL.MATRIX FUNCTION NOT WORKING FOR RANDOM INTERCEPT. 
    #NEED TO FIND AN ALTERNATE
    preds_var1 = model.matrix(as.formula(paste0(f1)), or_c) %*% par[num,]
    preds_var2 = preds_var1 + re_sd[num]
    preds_bs_var1 = cbind(preds_bs_var1, preds_var2)

  }
}

################
## Minnesota ###
################

##Main estimates
f1 = paste0(" ~  age + ",paste0("`1`"), "+", paste0("`3`"), "+ sex + (1 | sitecode)")
m1 = lmer(as.formula(paste0(outcome,f1)), data = all_yrbs )
mn_c = dcast(mn, sitecode + year + sex ~ grade, value.var = "hadsex")
colnames(mn_c) <- c("sitecode","year","sex","1","3")

mn_c = rbind(mn_c, mn_c, mn_c, mn_c)
mn_c[,age := factor(rep(c(4,5,6,7), each = 2))]
mn_c[,prediction := predict(m1, newdata = mn_c, allow.new.levels = T )]
mn_c = mn_c[,.(sitecode, year, sex, age, hadsex = prediction)]
mn_c[,age := as.numeric(as.character(age))]

#Uncertainty
for(rs in 1:length(all_yrbs_bs)){
  f1 = paste0(" ~  age + ",paste0("`1`"), "+", paste0("`3`"), "+ sex + (1 | sitecode)")
  m = lmer(as.formula(paste0(outcome,f1)), data = all_yrbs_bs[[rs]])
  sd = attr(VarCorr(m)[[1]],"stddev")
  set.seed(12345)
  par <- mvrnorm(n=n, mu = apply(coef(m1)[[1]],2,mean), Sigma = vcov(m,complete=TRUE)) #FE variance 
  set.seed(12345)
  re_sd <- rnorm(n=n,mean=0,sd=sd) #RE variance
  
  preds_bs_var1 = c()
  
  for(num in 1:100){
    ###NOTE: THIS IS NOT WORKING, AS MODEL.MATRIX FUNCTION NOT WORKING FOR RANDOM INTERCEPT. 
    #NEED TO FIND AN ALTERNATE
    preds_var1 = model.matrix(as.formula(paste0(f1)), mn_c) %*% par[num,]
    preds_var2 = preds_var1 + re_sd[num]
    preds_bs_var1 = cbind(preds_bs_var1, preds_var2)
    
  }
}

###############
## Washington #
###############
#Source: Washington Healthy Youth Survey
wa = data.table(sitecode = "WA",
                grade = c(10,12,10,12),
                sex = c("male","male","female","female"),
                hadsex = c(0.281,0.491,0.238,0.458),
                n = c(1450,1073,1603,1084),
                error_margin = c(0.031,0.046,0.034,0.049))
store_wa = data.table(expand.grid(sitecode = "WA", age = c(4:7), sex = c("male","female"), year = 2019, hadsex = 0))
out_pred = data.table(expand.grid(sitecode = "WA", age = c(4:7), sex = c("male","female"), year = 2019))

wa_dat_prep = function(vs=all_pred_new){
  
  if(!is.data.table(vs))  vs = data.table(analysis(vs))
  
  year_sub = c(2017,2019) #Optional subset year
  
  vs = vs[year %in% year_sub]
  state_years_w_responses = unique(vs[!is.na(q58), .(sitecode, year)])
  subset_dt = merge(vs, state_years_w_responses, by = c("sitecode","year"))
  
  get_2018 = subset_dt[year %in% c(2017, 2019),.(year,sitecode, age,sex, grade,hadsex = as.numeric(as.character(q58)), weight)]
  check_stats = data.table(table(get_2018$sitecode, get_2018$year))
  no_2017 = check_stats[N == 0, V1]
  get_2018 = get_2018[!sitecode %in% no_2017]
  get_2018[!is.na(sex),sex1 := ifelse(sex == 1, "female","male")]
  get_2018[,sex := sex1]
  get_2018[,sex1 := NULL]
  get_2018[,hadsex := as.numeric(as.character(factor(hadsex, levels = c(1,2), labels = c(1,0))))]
  get_2018 = get_2018[complete.cases(get_2018)]
  get_2018 = get_2018[age %in% 4:7]
  
  #Proportions by age
  all_age = copy(get_2018)[,.(hadsex = weighted.mean(hadsex, weight,na.rm=T)), by = c("sitecode","age", "year","sex")]
 
  #Interpolate for 2018
  change = dcast(all_age, age  + sitecode + sex ~ year,  value.var = "hadsex")
  change[,v2018 := 0]
  for(i in 1:nrow(change)){
    change[i,'v2018'] <- approx(x=c(as.numeric(change[i,'2017']),as.numeric(change[i,'2019'])), n = 3)$y[2]
  }
  setnames(change, "v2018","2018")
  change1 = melt(change, id.vars = c("age","sex","sitecode"))
 
  #Proportions by grade
  all_grade = copy(get_2018)[,.(hadsex = weighted.mean(hadsex, weight,na.rm=T)), by = c("sitecode","grade", "year","sex")]
  
  change = dcast(all_grade, grade + sex + sitecode ~ year, value.var = "hadsex")
  change[,v2018 := 0]
  for(i in 1:nrow(change)){
    change[i,'v2018'] <- approx(x=c(as.numeric(change[i,'2017']),as.numeric(change[i,'2019'])), n = 3)$y[2]
  }
  setnames(change, "v2018","2018")
  
  change2 = melt(change, id.vars = c("grade","sex","sitecode"))

  #Join age and grade
  all_grade_c =  dcast(change2[variable %in% c(2018)],  sitecode   + sex ~ grade + variable , value.var = "value")
  check1 = merge(all_grade_c, change1[variable == 2019])
   check1[,age := factor(age)]
  setnames(check1, c("variable","value"), c("year","hadsex"))
  
  return(check1)

}


all_yrbs = wa_dat_prep(all_pred_new)
all_yrbs_bs  = lapply(xx$splits,wa_dat_prep)

outcome = "hadsex"

#Main prediction
f1 = paste0(" ~  age + ",paste0("`2_2018`"), "+", paste0("`4_2018`"), "+ sex + (1 | sitecode)")
m1 = lmer(as.formula(paste0(outcome,f1)), data = all_yrbs )
wa_c = dcast(wa, sitecode + sex ~ grade, value.var = "hadsex")
colnames(wa_c) <- c("sitecode","sex","2_2018","4_2018")

wa_c  = rbind(wa_c , wa_c , wa_c , wa_c )
wa_c[,age := factor(rep(c(4,5,6,7), each = 2))]
wa_c[,prediction := predict(m1, newdata = wa_c , allow.new.levels = T )]
wa_c = wa_c [,.(sitecode, sex, age, hadsex = prediction)]
wa_c[,age := as.numeric(as.character(age))]
    
#Uncertainty
for(rs in 1:length(all_yrbs_bs)){
  f1 = paste0(" ~  age + ",paste0("`2_2018`"), "+", paste0("`4_2018`"), "+ sex + (1 | sitecode)")
  m = lmer(as.formula(paste0(outcome,f1)), data = all_yrbs_bs[[rs]])
  sd = attr(VarCorr(m)[[1]],"stddev")
  set.seed(12345)
  par <- mvrnorm(n=n, mu = apply(coef(m1)[[1]],2,mean), Sigma = vcov(m,complete=TRUE)) #FE variance 
  set.seed(12345)
  re_sd <- rnorm(n=n,mean=0,sd=sd) #RE variance
  
  preds_bs_var1 = c()
  
  for(num in 1:100){
    ###NOTE: THIS IS NOT WORKING, AS MODEL.MATRIX FUNCTION NOT WORKING FOR RANDOM INTERCEPT. 
    #NEED TO FIND AN ALTERNATE
    preds_var1 = model.matrix(as.formula(paste0(f1)), wa_c) %*% par[num,]
    preds_var2 = preds_var1 + re_sd[num]
    preds_bs_var1 = cbind(preds_bs_var1, preds_var2)
    
  }
}



props_out = rbind(mn_c, or_c, wa_c[,year := 2019])
props_out = props_out[,.(state = sitecode, age = as.numeric(as.character(age)) + 11, sex,yrbs_state = hadsex)]

saveRDS(props_out,paste0(out.path,"/090623_prediction_results.RDS"))

##NOTE: Replace this filepath once uncertainty is finished
saveRDS(all_unc,paste0(out.path,"/090623_uncertainty.RDS"))

#########








