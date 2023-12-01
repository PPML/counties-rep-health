
rm(list=ls())

library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(irr)
library(MASS)
library(lme4)
library(rsample)


#Functions
source("model_functions.R")

#Filepaths
in.path = "~/data_counties/Data/Processed/"
out.path = "~/counties-project/Results/extrapolation_model/"


yrbs = readRDS(paste0(in.path,"YRBS_final.RDS"))
load(paste0(in.path,"state_prediction.RData"))

##Check trends in states that ask the question
all_pred_new = all_pred_new[age %in% c(4,5,6,7) & year > 2009]
all_pred_new[q58 == 1,hadsex := 1]
all_pred_new[q58 == 2,hadsex := 0]
all_pred_new[,hadsex := as.numeric(as.character(hadsex))]

##Create bootstrap version for uncertainty & check overall means and distributions
set.seed(12345)
n=100
all_pred_bs <- rsample::bootstraps(all_pred_new, times = n, strata = "sitecode")


limit = all_pred_new[sitecode %in% pred_mat[method == "direct",sitecode]]

limit_mean = limit[,.(count = .N, mean = weighted.mean(hadsex, weight, na.rm=T)), 
              by=c("sitecode","year","age","sex")]
bs_limit_mean = lapply(all_pred_bs$splits, function(f1) {
                            f2 <- data.table(analysis(f1))
                            f2 = f2[sitecode %in% pred_mat[method == "direct",sitecode]]
                            f2 = f2[,.(count = .N, mean = weighted.mean(hadsex, weight, na.rm=T)), 
                                  by=c("sitecode","year","age","sex")]
                            return(f2)
                          })

bs_limit_mean_c  = rbindlist(bs_limit_mean)
bs_limit_mean_c = bs_limit_mean_c[,.(count = .N, mean = mean(mean)), by=c("sitecode","year","age","sex")]

#Bootstrap samples have almost identical distribution to main data overall
ggplot() + 
  geom_density(data = bs_limit_mean_c,
           aes(x = mean), fill="red",alpha=0.5) + 
  geom_density(data = limit_mean,
           aes(x = mean), fill="blue",alpha=0.5) + facet_wrap(~age + sex) + theme_bw()


overall_mean =  all_pred_new[sitecode %in% pred_mat[method == "direct",sitecode]]
overall_mean = overall_mean[,.(count = .N, mean = weighted.mean(hadsex, weight, na.rm=T)), 
                            by=c("year","age","sex")]


ggplot() + 
       geom_point(data = limit_mean[!is.na(sex)], aes(year, mean)) + theme_bw() +
       geom_line(data = limit_mean[!is.na(sex)], aes(year, mean, group=sitecode), size = 0.3, se=F) + 
       ylim(c(0.1,0.8)) +
       geom_smooth(method='lm', data = overall_mean[!is.na(sex)], aes(year, mean), col="red", size=1, se = FALSE) +
       facet_grid(age ~ sex) 

ggplot() + 
  geom_point(data = limit_mean[!is.na(sex)], aes(age, mean)) + theme_bw() +
  geom_smooth(method='lm', data = limit_mean[!is.na(sex)], aes(age, mean, group=sitecode), size = 0.3, se=F) + 
  ylim(c(0.1,0.8)) +
  geom_smooth(method='lm', data = overall_mean[!is.na(sex)], aes(age, mean), col="red", size=1, se = FALSE) +
  facet_grid(year ~ sex) 

#Aggregate data and bootstrap data
agg_bs_dat = function(dat_bs){

  if(!is.data.table(dat_bs)) dat_bs = data.table(analysis(dat_bs))
  dat_bs[, Y := factor(q58, levels = c(1,2), labels = c(1,0))]   # Change Y to factor
  dat_bs[, Y := factor(Y, levels = c(0,1))]  # Ensure 0 is the reference category
  dat_bs[, hadsex := as.numeric(as.character(Y))]
  agg_dat = dat_bs[sitecode %in% pred_mat[method == "direct",sitecode] & year > 2009 & age %in% c(4:7) & include1 == 1]
  agg_dat = agg_dat[,.(mean_prop = weighted.mean(hadsex, weight, na.rm=TRUE)),
                    by = c("age","sex","sitecode","year")]
  agg_dat = agg_dat[!is.na(sex) & !is.na(age)]
  agg_dat = data.table::dcast(agg_dat,  age + sex + sitecode ~ year, 
                              value.var = "mean_prop")
  setnames(agg_dat, c("2011","2013","2015","2017","2019"),c("v2011","v2013","v2015","v2017","v2019"))
  agg_dat = agg_dat[!sitecode == "VA"]
  
  pred_dat = dat_bs[sitecode %in% pred_mat[method == "extrapolate",sitecode] & year > 2009 & age %in% c(4:7)]
  pred_dat = pred_dat[,.(mean_prop = weighted.mean(hadsex, weight, na.rm=TRUE)),
                      by = c("age","sex","sitecode","year")]
  pred_dat = pred_dat[!is.na(sex) & !is.na(age)]
  pred_dat = data.table::dcast(pred_dat,  age + sex + sitecode ~ year, value.var = "mean_prop")
  setnames(pred_dat, c("2011","2013","2015","2017","2019"),c("v2011","v2013","v2015","v2017","v2019"))
  agg_dat[,age := factor(age, levels = c(4,5,6,7))]
  pred_dat[,age := factor(age, levels = c(4,5,6,7))]
  
  return(list(agg_dat = agg_dat, pred_dat = pred_dat))
  
}

agg_dat = agg_bs_dat(all_pred_new)$agg_dat
pred_dat = agg_bs_dat(all_pred_new)$pred_dat

agg_dat_bs = lapply(all_pred_bs$splits,agg_bs_dat)
pred_dat_bs = lapply(agg_dat_bs, "[[", 2)
agg_dat_bs = lapply(agg_dat_bs, "[[", 1)

saveRDS(list(agg_dat_bs=agg_dat_bs, agg_dat=agg_dat, pred_dat = pred_dat, pred_dat_bs = pred_dat_bs), 
        file=paste0("~/data_counties/extrapolation_model_dat.RDS"))

#ID missing patterns 
dat_2011 = all_pred_new[year == 2011 & include1 == 1 & sitecode %in% pred_mat[method == "direct",sitecode], unique(sitecode)]
dat_2013 = all_pred_new[year == 2013 & include1 == 1 & sitecode %in% pred_mat[method == "direct",sitecode], unique(sitecode)]
dat_2015 = all_pred_new[year == 2015 & include1 == 1 & sitecode %in% pred_mat[method == "direct",sitecode], unique(sitecode)]
dat_2017 = all_pred_new[year == 2017 & include1 == 1 & sitecode %in% pred_mat[method == "direct",sitecode], unique(sitecode)]
availability = data.table(expand.grid(v2011 = 0, v2013 = 0, v2015 = 0, v2017 = 0, sitecode = pred_mat[method == "direct",sitecode]))
availability[sitecode %in% dat_2011, v2011 := 1]                                                         
availability[sitecode %in% dat_2013, v2013 := 1]           
availability[sitecode %in% dat_2015, v2015 := 1]           
availability[sitecode %in% dat_2017, v2017 := 1]

extrap_patterns = all_pred_new[sitecode %in% pred_mat[method == "extrapolate",sitecode] & year > 2009 & age %in% c(4:7) & include1 == 1,.(sitecode, year)]
extrap_patterns = unique(extrap_patterns[,available := 1] )
extrap_patterns = data.table::dcast(extrap_patterns, sitecode ~ year, value.var = "available")
colnames(extrap_patterns) = c("sitecode","v2011","v2013", "v2015", "v2017")
extrap_patterns[sitecode %in% c("DE"), pattern := 1]
extrap_patterns[sitecode %in% c("HI","NJ","TN"), pattern := 2]
extrap_patterns[sitecode %in% c("NM"), pattern := 3]
extrap_patterns[sitecode %in% c("WY"), pattern := 4]
extrap_patterns[sitecode %in% c("IN"), pattern := 5]

group1 = availability[v2011 == 1 & v2013 == 1 & v2015 == 1 & v2017 == 1, unique(sitecode)]
group2 = availability[v2011 == 1 & v2013 == 1, unique(sitecode)]
group3 = availability[v2015 == 1 & v2017 == 1, unique(sitecode)]
group4 = availability[v2011 == 1 & v2013 == 1 & v2015 == 1, unique(sitecode)]
group5 = availability[v2015 == 1, unique(sitecode)]

all_groups = list(group1 = group1, group2 = group2,
                  group3 = group3, group4 = group4, group5 = group5)


predictors = list(pattern1 = c("v2011 + v2013 + v2015 + v2017"),
                  pattern2 = c("v2011 + v2013"),
                  pattern3 = c("v2015 + v2017"),
                  pattern4 = c("v2011 + v2013 + v2015"),
                  pattern5 = c("v2015"))

mods = function(state, dt=agg_dat,pattern="pattern1",
                sub_dt = c("age","sex","none","age_sex"),
                random_effect = TRUE){
  
  complete = agg_dat[complete.cases(agg_dat) & sitecode != state]
  complete_pred = agg_dat[sitecode == state]
  
  if(sub_dt == "age"){
    
    pt = unlist(strsplit(pattern,split=" + ", fixed = TRUE))
    form = paste0("v2019~",pattern,"+ age + sex + ",paste(paste0("age*",pt),collapse = "+"))
    m=glm(as.formula(form), family="quasibinomial",data = complete)
    preds = predict(m, complete_pred, type = "response")
    
    if(random_effect){
      form = paste0("v2019~",pattern,"+ age + sex + ",paste(paste0("age*",pt),collapse = "+")," + (1|sitecode)")
      m=lmer(as.formula(form),data = complete)
      preds = predict(m, complete_pred, allow.new.levels = TRUE)
    }
 
    complete_pred[,prediction := preds]
    
  } 
  
  if(sub_dt == "sex"){
    
    pt = unlist(strsplit(pattern,split=" + ", fixed = TRUE))
    form = paste0("v2019~",pattern,"+ age + sex +",paste(paste0("sex*",pt),collapse = "+"))
    m=glm(as.formula(form), family="quasibinomial",data = complete)
    preds = predict(m, complete_pred, type="response")
    
    if(random_effect){
      form = paste0("v2019~",pattern,"+ age + sex + ",paste(paste0("sex*",pt),collapse = "+")," + (1|sitecode)")
      m=lmer(as.formula(form),data = complete)
      preds = predict(m, complete_pred, allow.new.levels = TRUE)
    }
    
    complete_pred[,prediction := preds] 
    
  }
  
  if(sub_dt == "none"){
      
      form = paste0("v2019~",pattern," + sex + age")
      m=glm(as.formula(form), family="quasibinomial",data = complete)
      preds = predict(m, complete_pred, type="response")
      
     if(random_effect){
        form = paste0("v2019~",pattern,"+ age + sex  + (1|sitecode)")
        m=lmer(as.formula(form),data = complete)
        preds = predict(m, complete_pred, allow.new.levels = TRUE)
      }
      
      complete_pred[,prediction := preds] 
  }
  
  if(sub_dt == "age_sex"){
    
      pt = unlist(strsplit(pattern,split=" + ", fixed = TRUE))
      form = paste0("v2019~",pattern," + age + sex +",
                  paste(paste0("sex*",pt), collapse= "+"),"+", 
                  paste(paste0("age*",pt),collapse = "+"))
      m=glm(as.formula(form), family="quasibinomial",data = complete)
      preds = predict(m, complete_pred, type="response")
      
      if(random_effect){
        form = paste0("v2019~",pattern," + age + sex +",
                      paste(paste0("sex*",pt), collapse= "+"),"+", 
                      paste(paste0("age*",pt),collapse = "+")," + (1|sitecode)")
        m=lmer(as.formula(form),data = complete)
        preds = predict(m, complete_pred, allow.new.levels = TRUE)
      }
      
      complete_pred[,prediction := preds]
  }
  
  return(list(preds = complete_pred, model = m))
}

#LOO state approach - 
#testing all interactions but ultimately used no interaction + random effect

all_age = list()
all_sex = list()
no_inter = list()
all_inter = list()

for(group in c(1:5)){
  print(group)
  pattern = predictors[grep(group,names(predictors), value=T)][[1]]
  group_t = c(all_groups[grep(group,names(all_groups), value=T)])[[1]]
  xx1 = lapply(group_t, mods, sub_dt = "age", pattern = pattern)
  xx2 = lapply(group_t, mods, sub_dt = "sex", pattern = pattern)
  xx3 = lapply(group_t, mods, sub_dt = "none", pattern = pattern)
  xx4 = lapply(group_t, mods, sub_dt = "age_sex", pattern = pattern)
  
  xx1 = lapply(xx1, "[[","preds")
  xx1 = lapply(xx1, function(x) x[,group_cont := group])
  xx1 = rbindlist(xx1)
  all_age = rbind(all_age, xx1)
  
  xx2 = lapply(xx2, "[[","preds")
  xx2 = lapply(xx2, function(x) x[,group_cont := group])
  xx2 = rbindlist(xx2)
  all_sex = rbind(all_sex, xx2)
  
  xx3 = lapply(xx3, "[[","preds")
  xx3 = lapply(xx3, function(x) x[,group_cont := group])
  xx3 = rbindlist(xx3)
  no_inter = rbind(no_inter, xx3)
  
  xx4 = lapply(xx4, "[[","preds")
  xx4 = lapply(xx4, function(x) x[,group_cont := group])
  xx4 = rbindlist(xx4)
  all_inter = rbind(all_inter, xx4)
  
}

all_age[,model := "Age Interaction"]
all_sex[,model := "Sex Interaction"]
no_inter[,model := "No interactions"]
all_inter[,model := "All interactions"]

all_preds = rbind(all_age, all_sex, no_inter, all_inter)
all_preds_state = all_preds[,.(v2019 = mean(v2019),
                               v2011 = mean(v2011, na.rm=TRUE),
                               v2013 = mean(v2013, na.rm=TRUE),
                               v2015 = mean(v2015, na.rm=TRUE),
                               v2017 = mean(v2017, na.rm=TRUE),
                               prediction = mean(prediction)),
                            by=c("age","sex","model","group_cont")]
all_preds_state = melt(all_preds_state[,.(age, sex,  model, prediction,group_cont,
                                    v2011, v2013, v2015, v2017, v2019)], 
                       id.vars = c("age","sex","model","group_cont"))
setnames(all_preds_state, "variable","year")
all_preds_state[,year := gsub("v","",year)]
all_preds_state[,predicted := ifelse(year == "prediction",1,0)]
all_preds_state[,year := ifelse(year == "prediction", 2019, year)]
all_preds_state[,year := as.numeric(as.character(year))]

pdf(file = paste0(out.path,"RE_overall_model_interactions.pdf") , height = 8, width = 10)
for(group in c(1:4)){
    state_preds_sub = all_preds_state[group_cont == group]
    if(nrow(state_preds_sub) > 0){
      gg <- ggplot() + geom_point(data = state_preds_sub, aes(year, value, col=age, shape=factor(predicted))) +
        geom_line(data = state_preds_sub[predicted == 0], aes(year, value, col=age, group=age)) +
        facet_grid(model~sex) + theme_bw() + ggtitle(paste0("group=",group)) 
      print(gg)
    }
  }
dev.off()

all_preds_year = melt(all_preds[,.(age, sex, sitecode, model, prediction,group_cont,v2011, v2013, v2015, v2017, v2019)], id.vars = c("age","sex","sitecode","model","group_cont"))
setnames(all_preds_year, "variable","year")
all_preds_year[,year := gsub("v","",year)]
all_preds_year[,predicted := ifelse(year == "prediction",1,0)]
all_preds_year[,year := ifelse(year == "prediction", 2019, year)]
all_preds_year[,year := as.numeric(as.character(year))]

pdf(file = paste0(out.path,"RE_model_Extrapolations.pdf") , height = 8, width = 10)
for(state in unique(c(group1, group2, group3, group4))){
  for(group in c(1:4)){
    state_preds_sub = all_preds_year[sitecode == state & group_cont == group]
    if(nrow(state_preds_sub) > 0){
      gg <- ggplot() + geom_point(data = state_preds_sub, aes(year, value, col=age, shape=factor(predicted))) +
        geom_line(data = state_preds_sub[predicted == 0], aes(year, value, col=age, group=age)) +
        facet_grid(model~sex) + theme_bw() + ggtitle(paste0(state," group=",group)) 
      print(gg)
    }
  }
}
dev.off()

yr_2019 = all_preds_year[year == 2019]
yr_2019 = dcast(yr_2019, age + sex + year + sitecode + model + group_cont ~ predicted, value.var = "value")
setnames(yr_2019,c("0","1"), c("hadsex","prediction"))
yr_2019[, age := as.numeric(as.character(age)) + 11]
yr_2019[, sex := factor(sex, levels = c(1,2), labels = c("male","female"))]

set.seed(12345)
get_iccs = data.table(expand.grid(age = unique(yr_2019$age), 
                                  sex = unique(yr_2019$sex),
                                  model = unique(yr_2019$model),
                                  group = unique(yr_2019$group_cont),
                                  icc = 0))
for(age1 in unique(yr_2019$age)){
  for(sex1 in unique(yr_2019$sex)){
    for(model1 in unique(yr_2019$model)){
      for(group1 in unique(yr_2019$group)){
      icc1 = icc(yr_2019[sex == sex1 & age == age1 & model == model1 & group_cont == group1, 
                           c("hadsex","prediction")],
                 model = "twoway",type = "agreement", unit = "single")$value
      get_iccs[sex == sex1 & age == age1 & model == model1 & group == group1, icc := icc1]
    }
  }
  }
}

fread(paste0(out.path,"group1_all_ICCs.csv" ))
fwrite(get_iccs, paste0(out.path,"group1_all_ICCs.csv" ))
get_iccs[, x :=  rep(0.5,nrow(get_iccs)) ]
setnames(get_iccs, "group","group_cont")

pdf(file = paste0(out.path,"group1_extrap_ICC.pdf") , height = 8, width = 10)

for(m in unique(yr_2019$model)){
  
  gg = ggplot(yr_2019[ model == m ], aes(hadsex, prediction)) + geom_point()  +
    #geom_text(aes(label = sitecode), hjust=-0.5, size = 3) +
    geom_abline(slope = 1, intercept = 0) + ylim(c(0.1,0.90)) + xlim(c(0.1,0.90)) +
    labs(x = "Real Proportion", y = "Predicted Proportion") + 
    geom_text(data = get_iccs[model == m], 
              aes(x = x, label = paste0("icc=",round(icc, 2)), y = 0.80), size = 3) +
    theme_bw()  +  
    facet_grid(age ~ sex + group_cont) + ggtitle(paste0(m," model"))
  print(gg)
}

dev.off()

extrapolation_dat = readRDS(paste0("~/data_counties/extrapolation_model_dat.RDS"))
n=100
agg_dat = extrapolation_dat$agg_dat
pred_dat = extrapolation_dat$pred_dat
agg_dat_bs = extrapolation_dat$agg_dat_bs
pred_dat_bs = extrapolation_dat$pred_dat_bs

group_list = vector("list",5)
names(group_list) = names(all_groups)

#Apply predictions
for(group in c(1:5)){
  
  print(group)
  pattern = predictors[grep(group,names(predictors), value=T)][[1]]
  group_t = c(all_groups[grep(group,names(all_groups), value=T)])[[1]]
  
  
  bs_uncertainty = function(agg=agg_dat_bs[[1]], pred1=pred_dat_bs[[1]], counter1){
    
    print(counter1)
    agg_dat_state = agg[sitecode %in% group_t]
    pred_states = c(extrap_patterns[pattern == group, unique(sitecode)])
    pred_dat_state = pred1[sitecode %in% pred_states]
    pred_dat_state[,age := factor(age, levels = c(4,5,6,7))]
    agg_dat_state[,age := factor(age, levels = c(4,5,6,7))]
    
    form = paste0("v2019~",pattern,"+ age + sex  + (1|sitecode)")
    m=lmer(as.formula(form),data = agg_dat_state)
    preds_bs = predict(m, pred_dat_state, allow.new.levels = TRUE)
    

    #Uncertainty
    sd = attr(VarCorr(m)[[1]],"stddev")
    vars = gsub(" ","",stringr::str_split(pattern,pattern="\\+",simplify = T))[1,]
    set.seed(12345)
    par <- mvrnorm(n=n, mu = apply(coef(m)[[1]],2,mean), Sigma = vcov(m,complete=TRUE)) #FE variance 
    set.seed(12345)
    re_sd <- rnorm(n=n,mean=0,sd=sd) #RE variance
      
    
    preds_bs_var1 = c()
    preds_bs_var2 = c()

    
    for(num in 1:n){
     
      preds_var1 = model.matrix(as.formula(paste0("~",pattern,"+ age + sex")), 
                                pred_dat_state[,c("age","sex",vars),with=FALSE]) %*% par[num,]
           
      preds_var2 = preds_var1 + re_sd[num]
      preds_bs_var1 = cbind(preds_bs_var1, preds_var1)
      preds_bs_var2 = cbind(preds_bs_var2, preds_var2)

      
    }
    
    preds_bs_var1 = data.table(preds_bs_var1)
    preds_bs_var2 = data.table(preds_bs_var2)
    #preds_bs_var3 = data.table(preds_bs_var3)
    preds_bs_var1[,type := "FE_only"]
    preds_bs_var2[,type := "RE"]
    #preds_bs_var3[,type := "RE_Alt"]
    unc1 = data.table(cbind(pred_dat_state, preds_bs, preds_bs_var1))
    unc2 = data.table(cbind(pred_dat_state, preds_bs, preds_bs_var2))
   #unc3 = data.table(cbind(pred_dat_state, preds_bs, preds_bs_var3))
    
    #Note that Final chosen uncertainty was RE uncertainty (unc2)

    
    return(list(unc1 = unc1, unc2 = unc2,  beta = par, var = re_sd, final_uncertainty = final_uncertainty))

  }
  
  main_estimates = bs_uncertainty(agg_dat, pred_dat)[[1]][,.(age, sex, sitecode, v2011, v2013, v2015, v2019 = preds_bs)]
  uncertainty =  mapply(bs_uncertainty, agg_dat_bs, pred_dat_bs, SIMPLIFY = FALSE)
  
  group_list[[grep(group,names(all_groups))]] <- uncertainty
  pred_dat[sitecode %in% main_estimates$sitecode,"v2019"] <- main_estimates$v2019
}

saveRDS(group_list,paste0(out.path,"090623_all_bs_results.RDS"))


for(g in c(1,2,3,4,5)){
  tiff(file =  paste0(out.path,"group_",g,"extrapolation_results.tiff") , width = 750, height = 600)

    unc_vars = paste0("V",1:n)
    
    #Variances
    var = lapply(group_list[[g]], function(x) {
      variance = x$var
      return(variance)
    })
    var = unlist(var)
    var = as.data.table(var)
    fwrite(var, paste0(out.path, paste0("group",g,"_RE_variances.csv")))
    
    #Coeffiecients
    beta = lapply(group_list[[g]], function(x) {
      coeff = as.data.table(x$beta)
      return(coeff)
    })
    beta = rbindlist(beta)
    fwrite(beta, paste0(out.path,  paste0("group",g,"_MVN_betas.csv")))
    
    #uncertainty only due to bootstrap
    bs = lapply(group_list[[g]], function(x) {
      x1 = x$unc1
      x2 = x$unc2
      x=rbind(x1[,.(age, sex, sitecode, preds_bs)],
              x2[,.(age, sex, sitecode, preds_bs)])
      return(x)
      })
    
    bs = rbindlist(bs)
    bs = melt(bs, id.vars = c("age","sex","sitecode"))
    sumstats = copy(bs)
    sumstats = sumstats[,mean_val := mean(value), by = c("age","sex","sitecode")]
    sumstats = sumstats[,se := sqrt(var(value)), by = c("age","sex","sitecode")]
    sumstats = sumstats[,upper := mean_val + qnorm(0.975)*se, by = c("age","sex","sitecode")]
    sumstats = sumstats[,lower :=  mean_val + qnorm(0.025)*se, by = c("age","sex","sitecode")]
    sumstats1 = unique(sumstats[,.(age, sex, sitecode, mean_val, lower, upper, type="BS")])
    
  
    #uncertainty due to FE
    fe = lapply(group_list[[g]], function(x) {
      x1 = x$unc1
      x=rbind(x1[,c("age", "sex", "sitecode","preds_bs",unc_vars), with=FALSE])
      return(x)
    })
    
    fe = rbindlist(fe)
    fe = melt(fe, id.vars = c("age","sex","sitecode"))[variable != "preds_bs"]
    sumstats = copy(fe)
    sumstats = sumstats[,mean_val := mean(value), by = c("age","sex","sitecode")]
    sumstats = sumstats[,se := sqrt(var(value)), by = c("age","sex","sitecode")]
    sumstats = sumstats[,upper := mean_val + qnorm(0.975)*se, by = c("age","sex","sitecode")]
    sumstats = sumstats[,lower :=  mean_val + qnorm(0.025)*se, by = c("age","sex","sitecode")]
    sumstats2 = unique(sumstats[,.(age, sex, sitecode, mean_val, lower, upper, type="FE")])
    
    #uncertainty with added RE
    re1 = lapply(group_list[[g]], function(x) {
      x1 = x$unc2
      x=rbind(x1[,c("age", "sex", "sitecode","preds_bs",unc_vars), with=FALSE])
      return(x)
    })
    re1 = rbindlist(re1)
    re1 = melt(re1, id.vars = c("age","sex","sitecode"))[variable != "preds_bs"]
    sumstats = copy(re1)
    sumstats = sumstats[,mean_val := mean(value), by = c("age","sex","sitecode")]
    sumstats = sumstats[,se := sqrt(var(value)), by = c("age","sex","sitecode")]
    sumstats = sumstats[,upper := mean_val + qnorm(0.975)*se, by = c("age","sex","sitecode")]
    sumstats = sumstats[,lower :=  mean_val + qnorm(0.025)*se, by = c("age","sex","sitecode")]
    sumstats3 = unique(sumstats[,.(age, sex, sitecode, mean_val, lower, upper, type="added RE")])
    
    all_unc = rbind(sumstats1, sumstats2, sumstats3)
    
    #HI example
    # group_t = c(all_groups[grep(2,names(all_groups), value=T)])[[1]]
    # variable1 = paste0("v",1:100)
    # agg_dat_state_bs = lapply(agg_dat_bs,function(x) x[,.(age,sex,sitecode,v2011,v2013,v2019)])
    # agg_dat_state_bs = mapply(function(x,y) x[,variable := y], agg_dat_state_bs,variable1,SIMPLIFY  = FALSE)
    # agg_dat_state_bs = rbindlist(agg_dat_state_bs)
    # fwrite(agg_dat_state_bs, paste0(out.path, "group1_bs_agg_dat.csv"))
    # 
    # hi_bs = lapply(pred_dat_bs, function(x) x[sitecode == "HI"])
    # hi_bs = mapply(function(x,y) x[,variable := y], hi_bs,variable1,SIMPLIFY  = FALSE)
    # hi_bs = rbindlist(hi_bs)[,.(age,sex,sitecode,v2011,v2013,v2019,variable)]
    # fwrite(hi_bs, paste0(out.path, "hi_bs_agg_dat.csv"))
    # 
    # result_hi = re2[sitecode == "HI"] 
    # fwrite(result_hi, paste0(out.path, "hi_bs_result.csv"))
    # 
    # agg_dat_state = agg_dat[sitecode %in% group_t]
    # agg_dat_state = agg_dat_state[,.(age,sex,sitecode,v2011,v2013,v2019,prediction = FALSE)]
    # agg_dat_state = rbind(agg_dat_state,pred_dat[sitecode == "HI",.(age,sex,sitecode,v2011,v2013,v2019,prediction = TRUE)])
    # fwrite(agg_dat_state, paste0(out.path, "group1_agg_dat.csv"))
    
  
    #add main estimates
    mainstats =  pred_dat[sitecode %in% sumstats3$sitecode]
    mainstats = merge(mainstats, all_unc)
    
    #add empirical data
    group_t = c(all_groups[grep(g,names(all_groups), value=T)])[[1]]
    agg_dat_m = melt(agg_dat[sitecode %in% group_t], id.vars = c("age","sex","sitecode"))
    agg_dat_m[,mean_val := mean(value,na.rm=TRUE), by = c("age","sex","variable")]
    agg_dat_m[,lower := quantile(value,0.025,na.rm=TRUE), by = c("age","sex","variable")]
    agg_dat_m[,upper := quantile(value,0.975,na.rm=TRUE), by = c("age","sex","variable")]
    agg_dat_sum = unique(agg_dat_m[,.(age, sex, variable, mean_val,lower, upper)])[variable == "v2019"]
    agg_dat_sum[,sitecode := "empirical"]
    
    mainstats[,sex := factor(sex,levels = c(1,2), labels = c("Females","Males"))]
    mainstats[,age := factor(age, levels = c(4,5,6,7), labels = c(15,16,17,18))]
    mainstats[,age := as.factor(age)]
    
    
    agg_dat_sum[,sex := factor(sex,levels = c(1,2), labels = c("Females","Males"))]
    agg_dat_sum[,age := factor(age, levels = c(4,5,6,7), labels = c(15,16,17,18))]
    agg_dat_sum[,age := as.factor(age)]
    
    plot_a = ggplot(mainstats[type == "added RE"], aes(sitecode, mean_val)) + 
                    geom_point(size = 2) + 
                    geom_point(data = agg_dat_sum, col = "red") +
                    geom_errorbar(aes(ymin = lower, ymax = upper), linewidth = 1, width=0.5) +
                    geom_errorbar(data = agg_dat_sum , aes(ymin = lower, ymax = upper), col = "red", linewidth = 1, width=0.5) +
                    facet_grid(as.factor(sex) ~ as.factor(age)) + 
                    theme_bw() + coord_flip() + 
                    ggtitle("Availability pattern: ",paste0(predictors[[g]])) +
                    labs(y = "Predicted proportion of adolescents who have had sex",x=NULL)
    print(plot_a)
    
  dev.off()
}


#Format for raking
pred_dat_m = melt(pred_dat, id.vars = c("age","sex","sitecode"))
pred_dat_m[,variable := as.factor(gsub("v","",variable))]
final_pred = pred_dat_m[variable == 2019]

#No sample size for Indiana/Delaware because no YRBS in 2019
sample_sizes = all_pred_new[sitecode %in% pred_mat[method == "extrapolate",unique(sitecode)] & 
                              year == 2019 & !is.na(sex) , 
                            .(n_yrbs= .N), by = c("age","sex","sitecode"),]
sample_sizes[,age := factor(age, levels = c(4,5,6,7))]
final_pred = merge(final_pred, sample_sizes, by = c("age","sex","sitecode"),all.x=TRUE)
final_pred[, age := factor(as.numeric(as.character(age)) + 11)]
final_pred[, sex := as.character(sex)]
final_pred[, sex := ifelse(sex == 1, "female","male")]
setnames(final_pred, c("value","sitecode"),c("yrbs_state","state"))


group_list <- readRDS(paste0(out.path,"090623_all_bs_results.RDS"))
uncertainty_sum1 <- rbindlist(lapply(group_list$group1,function(x) return(x$unc2)))
uncertainty_sum2 <- rbindlist(lapply(group_list$group2,function(x) return(x$unc2)))
uncertainty_sum3 <- rbindlist(lapply(group_list$group3,function(x) return(x$unc2)))
uncertainty_sum4 <- rbindlist(lapply(group_list$group4,function(x) return(x$unc2)))
uncertainty_sum5 <- rbindlist(lapply(group_list$group5,function(x) return(x$unc2)))

#need to reorganize output to ensure draws are aligned and match other output
combine_uncertainty = rbind(uncertainty_sum1,uncertainty_sum2,uncertainty_sum3,uncertainty_sum4,uncertainty_sum5)
combine_uncertainty[,combo_id := paste0(age,sex,sitecode)]
key = unique(combine_uncertainty[,.(age,sex,combo_id,sitecode)])

all_results = vector("list",length(unique(combine_uncertainty$combo_id)))
names(all_results) <- unique(combine_uncertainty$combo_id)
new_names <- paste0(rep(1:100,each = 100),"_",1:100)

for(combo_id1 in unique(combine_uncertainty$combo_id)){
  
  print(combo_id1)
  tt = combine_uncertainty[combo_id == combo_id1]
  tt = tt[,grepl("V",colnames(tt)), with=FALSE]
  tt = unlist(tt)
  
  tt <- t(data.table(tt))
  colnames(tt) <- new_names
  tt <- round(tt,2)
  tt <- noquote(cbind(combo_id = combo_id1,tt))
  
  
  all_results[[combo_id1]] <- tt
  
}


newdat <- do.call("rbind",all_results)
newdat <- merge(key, newdat, by = c("combo_id"))
newdat <- as.data.table(newdat)
newdat[, age := factor(as.numeric(as.character(age)) + 11)]
newdat[, sex := as.character(sex)]
newdat[, sex := ifelse(sex == 1, "female","male")]
newdat[,combo_id := NULL]
colnames(newdat) <- paste0("pred_",colnames(newdat))
setnames(newdat,c("pred_age","pred_sex","pred_sitecode"), c("age","sex","state"))

newdat <- newdat[, lapply(.SD, as.numeric),by = c("age","sex","state"), .SDcols = colnames(newdat)[grepl("pred_",colnames(newdat))]]

  
saveRDS(final_pred[,.(age, sex, state, yrbs_state, n_yrbs)],paste0(out.path,"/090623_prediction_results.RDS"))
saveRDS(newdat,paste0(out.path,"/090623_uncertainty.RDS"))


#Double check main predictions vs bootstrap means
newdat <- data.table(newdat)
check_sum = apply(newdat[,4:10000], 1, mean)
check_sum = cbind(newdat[,1:3],check_sum)
final_pred = readRDS(paste0(out.path,"/090623_prediction_results.RDS"))
compare = merge(check_sum, final_pred, by = c("age","sex","state"))
ggplot(compare, aes(check_sum, yrbs_state)) + geom_point() + geom_abline(slope = 1, intercept = 0) + facet_wrap(~age + sex)
