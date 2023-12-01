

rm(list=ls())



library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(glmnet)
library(caret)
library(survey)
library(ranger)
library(BMA)
library(rms)
library(Hmisc)
library(predtools)
library(modelr)
library(purrr)
library(ROCR)


#Functions
source("~/counties-project/model_functions.R")

#Filepaths
in.path = "~/data_counties/Data/Processed/"
out.path = "~/counties-project/Results/"

#Set arguments
vars = c("sex", "age", "year","race_eth","household_cat","metro",
         "mom_age_cat","nhhmembers_cat","goschol", "household_comp")
outcome = "hadsex1"
weights = "weight"
strata1 = c("sex")


##Data
nsfg = readRDS(paste0(in.path,"NSFG_final.RDS"))
nsfg_bs = readRDS(paste0(in.path,"NSFG_bootstrap.RDS"))
nsfg[,hadsex1 := as.numeric(as.character(hadsex))]
nsfg[,year_cat := factor(year_cat, levels = c("2013_2015","2015_2019"))]
nsfg[, year := factor(year, levels = c(2013, 2015, 2017, 2019))]
nsfg[, age := factor(age, levels = c(15,16,17,18))]
dt = copy(nsfg)
dt = data.frame(dt)

#Run all predictions
run_models = function(df = dt, vars1 = vars,strata_x="run", outcome1=outcome, weights1=weights, weighted=TRUE){
  
  set.seed(1234)
  cv_data = createFolds(y=df$hadsex1)
  
  model_results = lapply(cv_data,function(x){
                  print("x")
                  train = df[-x,]
                  test = df[x,]
                  
                  #Lasso
                  lasso_m = lasso_mod(data_train = train, vars = vars1, outcome=outcome1, weight=weights1)
                  lasso_preds = all_preds(lasso_m,test,vars = vars1, outcome=outcome1, weight=weights1, type = "lasso")
                  lasso_preds = lasso_preds %>% dplyr::rename("p_lasso"="p")
                  
                  #Logistic
                  log_m = logistic_mod(data_train = train,vars = vars1, outcome=outcome1, weight=weights1)
                  log_preds = all_preds(log_m ,test,vars = vars1, outcome=outcome1, weight=weights1, type = "logistic")
                  log_preds = log_preds %>% dplyr::rename("p_logis"="p")
                  
                  #RF
                  rf_m = rf_mod(data_train = train,vars = vars1, outcome=outcome1, weight=weights1)
                  rf_preds = all_preds(rf_m ,test,vars = vars1, outcome=outcome1, weight=weights1, type = "rf")
                  rf_preds = rf_preds %>% dplyr::rename("p_rf"="p")
                  
                  #Bayes
                  # bayes_m = bayes_mod(data_train = train,vars = vars1, outcome=outcome1, weight=weights1)
                  # bayes_preds = all_preds(bayes_m ,test,vars = vars1, outcome=outcome1, weight=weights1, type = "bayes")
                  # bayes_preds = bayes_preds %>% dplyr::rename("p_bayes"="p")

                  observed = test
                  
                  full_dat = data.table(observed,
                                        # p_bayes=bayes_preds[,"p_bayes"],
                                        p_rf=rf_preds[,"p_rf"], 
                                        p_logis=log_preds[,"p_logis"],
                                        p_lasso=lasso_preds[,"p_lasso"])
                  
                  return(list(full_dat=full_dat,lasso_m = lasso_m, 
                              # bayes_m=bayes_m,
                              log_m = log_m, rf_m=rf_m))
        
              })
  
    return(list(model_results = model_results, cv_data = cv_data))
  }

##Baseline/Default results
get_mods = run_models(df = dt, vars1 = vars,strata_x="run",outcome1=outcome, weights1=weights, weighted=TRUE)
get_dat = rbindlist(lapply(get_mods$model_results, function(x) return(x$full_dat)))
get_dat[,run := 1] #Run argument provides an option to produce run by draws, but just 1 if not

#saveRDS(get_mods,paste0(out.path,"260423_all_models.RDS"))
#saveRDS(get_mods,paste0(out.path,"311022_all_models.RDS")) 

##Strata results
overall =  get_accuracy(data_test = get_dat, weighted=TRUE)
age = get_accuracy(data_test = get_dat, strata1 = "age", weighted=TRUE)
sex = get_accuracy(data_test = get_dat, strata1 = "sex", weighted=TRUE)
race = get_accuracy(data_test = get_dat, strata1 = "race_eth", weighted=TRUE)
age_sex = get_accuracy(data_test = get_dat, strata1 = c("sex", "age"), weighted=TRUE)
age_sex_race = get_accuracy(data_test = get_dat, strata1 = c("race_eth", "sex", "age"), weighted=TRUE)
sex_race = get_accuracy(data_test = get_dat, strata1 = c("race_eth", "sex"), weighted=TRUE)

accuracy = list(overall = overall,
                age = age, 
                sex = sex,
                race = race,
                age_sex = age_sex,
                age_sex_race = age_sex_race,
                sex_race)
saveRDS(accuracy, paste0(out.path, "accuracy_results.RDS"))


##ROC curve
together = list()
for(measure in c("acc","sens","ppv")){
  pred = prediction(get_dat$p_lasso.p_lasso, get_dat$hadsex1)
  perf = performance(pred, measure)
  d1 = data.table(cutoff = unlist(perf@x.values),value=unlist(perf@y.values),model="lasso")
  
  pred = prediction(get_dat$p_rf, get_dat$hadsex1)
  perf = performance(pred, measure)
  d2= data.table(cutoff =unlist(perf@x.values),value=unlist(perf@y.values),model="rf")
  
  pred = prediction(get_dat$p_bayes, get_dat$hadsex1)
  perf = performance(pred, measure)
  d3 = data.table(cutoff =unlist(perf@x.values),value=unlist(perf@y.values),model="bayes")
  
  pred = prediction(get_dat$p_logis, get_dat$hadsex1)
  perf = performance(pred, measure)
  d4= data.table(cutoff =unlist(perf@x.values),value=unlist(perf@y.values),model="logistic")
  
  all_measure = rbind(d1,d2,d3,d4)
  all_measure[,type := measure]
  
  together = rbind(together, all_measure)
}


ggplot(together, aes(cutoff, value, col=model)) + geom_line() + 
  facet_wrap(~type, scales="free") + theme_bw()

#Best model - get final predictions from full dataset
get_mods = readRDS(paste0(out.path,"260423_all_models.RDS"))
train_set = unique(rbindlist(lapply(get_mods$cv_data, function(x){
                            train = dt[-x,]
                            return(train)
                          })
                      ))

best_model = logistic_mod(data_train = train_set,vars = vars, outcome=outcome, weight=weights)
preds = predict(best_model, train_set, type="response")[1:nrow(train_set)]


#Bootstrap results - ensure same result using boostraps created in processing script
bs_analysis = lapply(nsfg_bs$splits, 
                 function(x) {
                   dt = rsample::analysis(x)
                   dt[,hadsex1 := as.numeric(as.character(hadsex))]
                   dt[,year_cat := factor(year_cat, levels = c("2013_2015","2015_2019"))]
                   dt[, year := factor(year, levels = c(2013, 2015, 2017, 2019))]
                   dt[, age := factor(age, levels = c(15,16,17,18))]
                   return(as.data.table(dt))
                 })
bs_model = map(bs_analysis,logistic_mod,  vars, outcome, weights)

#Check mean coefficients
coef_dat = data.frame(value = rep(0, n),
                      coefficient = rep(names(coefficients(bs_model[[1]])),n),
                      id = rep(seq(1,100),each=length(coefficients(bs_model[[1]]))))

all = unlist(lapply(bs_model,coefficients))
coef_dat[,'value']<- all

mean_values = data.frame(value = coefficients(best_model),
                         coefficient = names(coefficients(best_model)))

ggplot(coef_dat, aes(value)) + 
  geom_histogram() + facet_wrap(~coefficient, scales="free_x") + theme_bw() +
  geom_vline(data = mean_values, aes(xintercept = value), col="red")

saveRDS(best_model,paste0(out.path, "042623_best_model.RDS"))
saveRDS(bs_model,paste0(out.path, "042623_bootstrap_model.RDS"))
#saveRDS(best_model,paste0(out.path, "060722_best_model.RDS")) #Logistic regression
#saveRDS(get_dat,paste0(out.path,"100722_all_models.RDS")) 


