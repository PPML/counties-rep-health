

rm(list=ls())

library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
library(irr)
library(parallel)
library(ranger)
library(glmnet)
library(rsample)
library(purrr)
library(readxl)


#Functions
source("model_functions.R")

#Filepaths

in.path = "~/data_counties/Data/Processed/"
out.path = "~/counties-project/Results/yrbs_pred_model/"

yrbs = readRDS(paste0("~/data_counties/Data/Raw/YRBS_Raw_final.RDS"))
load(paste0(in.path,"state_prediction.RData"))


all_results = function(x, training=TRUE,  nnx = nn2, ssx = sx_dt){
  
  library(data.table)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(lme4)
  library(irr)
  library(parallel)
  library(ranger)
  library(glmnet)
  library(rsample)
  library(purrr)
  


  source("model_functions.R")

 
  test_all = function(state1="WV",split = TRUE, 
                      split_vars = c("age","sex"), 
                      added_var = FALSE, 
                      model1 = "lasso", nn = nn1,
                      ss_dt = sx_dt, 
                      pred_dtx = pred_dt,
                      test1 = TRUE){
    
    
    xx1 = get_preds(state = state1,
                    model = model1,
                    family1 = nn$family,
                    split = split,
                    split_vars = split_vars,
                    ss_dt = ss_dt,
                    pred_dt = pred_dtx,
                    X = nn$X,
                    Y = nn$Y,
                    w = nn$w,
                    test = test1)
    

    correlations = data.table(state = state1, xx1$state1)
    if(added_var) correlations = data.table(added_var = preds_t[length(preds_t)], xx1$state1)
    xx1$ind[,site := state1]
    
   
    return(list(correlations=correlations, data = xx1$ind, model = xx1$model))
    
}
  
  if(training){
    
    nn1 = readRDS("~/counties-project/Results/yrbs_pred_model/RF_model.RDS")
    nn2 = readRDS("~/counties-project/Results/yrbs_pred_model/lasso_model.RDS")
    sx_dt = fread("~/counties-project/Results/yrbs_pred_model/setup_dat.csv")
    pred_dt = fread("~/counties-project/Results/yrbs_pred_model/setup_dat.csv") ##Just dummy data here
  
    xx1 = test_all(state1=x, added_var = FALSE, model1 = "rf", nn = nn1, split = FALSE, ss_dt = ssx)
    xx2 = test_all(state1=x, added_var = FALSE, model1 = "rf", nn = nn1, split = TRUE, 
                   split_vars = c("age5","age6","age7","sex2"), ss_dt = ssx)
    #xx3 = test_all(state1=x, added_var = FALSE, model1 = "rf", nn = nn1, split = TRUE, split_vars = c("sex2"))
    xx4 = test_all(state1=x, added_var = FALSE, model1 = "lasso", nn = nn2, ss_dt = ssx)
    #xx5 = test_all(state1=x, added_var = FALSE, model1 = "rf", nn = nn1, split = TRUE, split_vars = c("age5","age6","age7"))
    return(list(all1 = xx1, all2 = xx2, all4 =  xx4))
    
  } else {
    
    #nn2 = readRDS("~/counties-project/Results/yrbs_pred_model/bs_lasso_model.RDS")
    #sx_dt = fread("~/counties-project/Results/yrbs_pred_model/bs_setup_dat.csv")
    pred_dt = fread("~/counties-project/Results/yrbs_pred_model/setup_dat.csv")
    
    xx4 = test_all(state1=x, added_var = FALSE, model1 = "lasso", nn = nnx, test1 = FALSE, ss_dt = ssx)
    return(list(all4 =  xx4))
  }
  
  
  
}

#######################
### Model testing  ###
#######################

# set up data, including X matrix
out_dat = dat_setup(vs = all_pred_new, pred_mat)
nn1 = pred_data(preds = out_dat$preds, outcome = "hadsex", ss_dt = out_dat$sx_dt, model = "rf", all_vars = out_dat$all_vars)
nn2 = pred_data(preds = out_dat$preds, outcome = "hadsex", ss_dt = out_dat$sx_dt, model = "lasso", all_vars = out_dat$all_vars)

# saveRDS(nn1,paste0(out.path,"RF_model.RDS"))
# saveRDS(nn2,paste0(out.path,"lasso_model.RDS"))

sx_dt = fread(paste0(out.path,"setup_dat.csv"))
states = as.list(unique(sx_dt$sitecode))

cl <- makeCluster(getOption("cl.cores", 5))
out = parLapply(cl, states[c(1,10,12,15,20)], all_results)
stopCluster(cl)

#saveRDS(out, paste0(out.path,"pred_mod_state26-35.RDS"))
#saveRDS(out, paste0(out.path,"pred_mod_state1-25.RDS"))

#Split up to faciltate run time
out = list(readRDS(paste0(out.path,"pred_mod_state1-25.RDS")),
           readRDS(paste0(out.path,"pred_mod_state26-35.RDS")))


for(i in 1 : length(unique(sx_dt$sitecode))){
  
  out1a = lapply(out[[1]],function(x) x$all1$data)
  out1b = lapply(out[[2]],function(x) x$all1$data)
  out1 = rbind(rbindlist(out1a), rbindlist(out1b))
  out1[,model := "RF"]
  
  out2a = lapply(out[[1]],function(x) x$all2$data)
  out2b = lapply(out[[2]],function(x) x$all2$data)
  out2 = rbind(rbindlist(out2a), rbindlist(out2b))
  out2[,model := "RF: force split age sex"]
  
  out4a = lapply(out[[1]],function(x) x$all4$data)
  out4b = lapply(out[[2]],function(x) x$all4$data)
  out4 = rbind(rbindlist(out4a), rbindlist(out4b))
  out4[,model := "LASSO"]
}

all = rbind(out1,out2,out4)
all = data.table(all)
all[,age := factor(age,levels = c(4,5,6,7), labels = c(15,16,17,18))]
all[,sex := factor(sex,levels = c(1,2), labels = c("Female","Male"))]

all_state = all[,.(mean_pred = weighted.mean(prediction, weight),
       mean_obs = weighted.mean(real_outcome, weight)),
    by = c("model","age","sex")]
all_state[,diff := abs(mean_pred - mean_obs)]
all_state_m = melt(all_state, id.vars = c("model","age","sex"))
all_state_m[,age := as.numeric(as.character(age)) + 11]



ggplot(all_state_m[ variable != "diff" ], aes(age, value, col=variable, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") + facet_wrap(~ sex + model) +
  theme_bw()  + ylab("Proportion who have had sex")

labs = all_state[,.(mean_diff = mean(diff)), by = c("model","age","sex")]
labs[, y := rep(c(0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4),3) ]
all_overall = all[,.(mean_pred = weighted.mean(prediction, weight),
             mean_obs = weighted.mean(real_outcome, weight)),
          by = c("model","age","sex")]
all_m = melt(all_overall, id.vars = c("age","sex","model"))

tiff(file =  paste0(out.path,"pred_model_MAE_results.tiff") , width = 750, height = 600)
  gg = ggplot() + 
              geom_bar(data = all_m, aes(age, value, col=variable, fill = variable),stat="identity", position="dodge") +
              facet_grid(model ~ sex) + theme_bw() +
              geom_text(data = labs, aes(age, y, label = round(mean_diff,3))) + 
              labs(x = "Age",y="Proportion who have had sex",caption = "Numbers on the plot are the mean absolute difference betwen the prediction and observation across states") +
              scale_fill_discrete(name=NULL,labels = c("Prediction","Observation")) +
              scale_color_discrete(guide = "none")
  print(gg)
    
dev.off()
  
#Look at ICCs
#Agreement vs consistency = agreement requires absolute equality, 
#consistency requires 
get_iccs = data.table(expand.grid(age = unique(all$age), 
                                  sex = unique(all$sex),
                                  model = unique(all$model),
                                  icc = 0))

all_state_mean = all[,.(mean_pred = weighted.mean(prediction, weight),
                   mean_obs = weighted.mean(real_outcome, weight)),
                by = c("model","age","sex","site")]
for(age1 in unique(all$age)){
  for(sex1 in unique(all_state$sex)){
    for(model1 in unique(all_state$model)){
    icc1 = icc(all_state_mean[sex == sex1 & age == age1 & model == model1, 
                         c("mean_obs","mean_pred")],
               model = "oneway",type = "agreement", unit = "single")$value
    get_iccs[sex == sex1 & age == age1 & model == model1, icc := icc1]
    }
  }
}
get_iccs[, x :=  rep(0.5,8*3) ]

tiff(file =  paste0(out.path,"pred_model_ICC_results.tiff") , width = 750, height = 600)
  
  gg = ggplot(all_state_mean, aes(mean_obs, mean_pred)) + 
        geom_point()  +
        geom_abline(slope = 1, intercept = 0) + ylim(c(0.1,0.90)) + xlim(c(0.1,0.90)) +
        labs(x = "Real Proportion", y = "Predicted Proportion") + 
        geom_text(data = get_iccs, aes(x = x, label = paste0("icc=",round(icc, 2)), y = 0.80), size = 3) +
        theme_bw()  +  
        facet_grid(age ~ sex + model)
  print(gg)
  
dev.off()


#########################
### Main Predictions  ###
#########################
out_dat = dat_setup(vs = all_pred_new, pred_mat)

#Final run of model on all states
ss_dt = rbind(out_dat$dt_pred, out_dat$sx_dt)
ss_dt[,age_is_missing  := NULL]
nn2 = pred_data(preds = out_dat$preds, outcome = "hadsex",
                ss_dt = ss_dt, model = "lasso", all_vars = out_dat$all_vars)

pred_states = pred_mat[method == "predict",sitecode]
dt_filtered = ss_dt[!sitecode %in% pred_states]

sitecode_ids = as.numeric(as.factor(dt_filtered$sitecode))
STATES_PER_FOLD = 6
sitecode_ids = ceiling(sitecode_ids/STATES_PER_FOLD)

X = nn2$X
Y = nn2$Y
w = nn2$w

X_filtered = X[!ss_dt[, sitecode] %in% pred_states,]
Y_filtered = Y[!ss_dt[, sitecode] %in% pred_states]
w_filtered = w[!ss_dt[, sitecode] %in% pred_states]

X_pred = X[ss_dt[, sitecode %in% pred_states],]
w_pred = w[ss_dt[, sitecode %in% pred_states ]]
Y_pred = Y[ss_dt[, sitecode %in% pred_states]]

s1 = Sys.time()

set.seed(12345)

m = cv.glmnet(
  X_filtered,
  Y_filtered,
  foldid = sitecode_ids,
  nfolds = max(sitecode_ids),
  type.measure = "mse",
  nlambda = 10,
  family="binomial"
)

s2 = Sys.time()
preds = predict(m, newx=X_pred, s = "lambda.min", type = "response")[,1]

# predicting on all data due to missing factor levels in predicting states
wm_dt = data.table(
  ss_dt[sitecode %in% pred_states, "sitecode"],
  ss_dt[sitecode %in% pred_states, .(age,sex)],
  prediction = preds,
  real_outcome = Y_pred,
  weight = w_pred)

# Aggregate individual responses to the state level to make a state-level
# prediction for the held-out state. 

#Format for raking factor
dt_out = wm_dt[,.(yrbs_state = weighted.mean(prediction, weight), n_yrbs = .N),by=c("sitecode","age","sex")]
setnames(dt_out, "sitecode","state")
dt_out[,age := as.numeric(as.character(age)) + 11]
dt_out[,sex := ifelse(sex == 1,"female","male")]


saveRDS(dt_out,paste0(out.path,"/090623_prediction_results.RDS")) #Changes from original resulted in 1-2% higher proportions
#saveRDS(dt_out,paste0(out.path,"/111022_prediction_results.RDS")) ##Original Run, before redoing bootstrapping


####################
### Uncertainty  ###
###################

#To ensure the draws align, bootstraps are created on the same data as the extrapolation model. We cannot directly use the same bootstraps as extrapolation model because here data are limited to those 
#with predictors we are interested in. We use more data for extrapolation model.
n=50
set.seed(12345)
xx <- rsample::bootstraps(all_pred_new, times = n, strata = sitecode)

get_dat = function(x){
  
  #x = xx$splits[[1]] #For testing only
  out_dat1 = dat_setup(vs = data.table(analysis(x)), pred_mat)
  
  #These do not actually vary across bootstraps
  all_vars = out_dat1$all_vars
  preds = out_dat1$preds
  
  ss_dt = rbind(out_dat1$dt_pred,out_dat1$sx_dt)
  nn2 = pred_data(preds = preds, outcome = "hadsex",
                  ss_dt = ss_dt,
                  model = "lasso", all_vars = all_vars)
  
  pred_states = pred_mat[method == "predict",sitecode]
  dt_filtered = ss_dt[!sitecode %in% pred_states]
  
  sitecode_ids = as.numeric(as.factor(dt_filtered$sitecode))
  STATES_PER_FOLD = 6
  sitecode_ids = ceiling(sitecode_ids/STATES_PER_FOLD)
  
  X = nn2$X
  Y = nn2$Y
  w = nn2$w
  
  X_filtered = X[!ss_dt[, sitecode] %in% pred_states,]
  Y_filtered = Y[!ss_dt[, sitecode] %in% pred_states]
  w_filtered = w[!ss_dt[, sitecode] %in% pred_states]
  
  X_pred = X[ss_dt[, sitecode %in% pred_states],]
  w_pred = w[ss_dt[, sitecode %in% pred_states ]]
  Y_pred = Y[ss_dt[, sitecode %in% pred_states]]
  
  s1 = Sys.time()
  print(s1)
  set.seed(12345)
  
  m = cv.glmnet(
    X_filtered,
    Y_filtered,
    foldid = sitecode_ids,
    type.measure = "mse",
    nlambda = 10,
    family="binomial"
  )
  
  s2 = Sys.time()
  print(s2)
  
  predictions = predict(m, newx=X_pred, s = "lambda.min", type = "response")[,1]
  
  wm_dt = data.table(
    ss_dt[sitecode %in% pred_states, "sitecode"],
    ss_dt[sitecode %in% pred_states, .(age,sex)],
    prediction = predictions,
    real_outcome = Y_pred,
    weight = w_pred)
  
  # Aggregate individual responses to the state level to make a state-level
  # prediction for the held-out state. 
  
  #Format for raking factor
  dt_out = wm_dt[,.(yrbs_state = weighted.mean(prediction, weight), n_yrbs = .N),by=c("sitecode","age","sex")]
  setnames(dt_out, "sitecode","state")
  dt_out[,age := as.numeric(as.character(age)) + 11]
  dt_out[,sex := ifelse(sex == 1,"female","male")]
  
  return(dt_out)
 
}


all_bs = lapply(xx$splits,get_dat)
#saveRDS(all_bs,paste0(out.path,"/090623_bs_31_50.RDS"))

bs1 = rbindlist(readRDS(paste0(out.path,"/090623_bs_1_10.RDS")))
bs2 = rbindlist(readRDS(paste0(out.path,"/090623_bs_11_30.RDS")))
bs3 = rbindlist(readRDS(paste0(out.path,"/090623_bs_31_50.RDS")))

all2 = rbind(bs1,bs2,bs3)
all2[,run := rep(1:50, each = 32)]
saveRDS(all2, paste0(out.path,"/090623_uncertainty.RDS") )

#Double check means
summary1 = all2[,.(mean = mean(yrbs_state), 
                   q1 = quantile(yrbs_state, c(0.025)), 
                   q2 = quantile(yrbs_state, c(0.975))) , by = c("state","age","sex")]



#saveRDS(dt_out,paste0(out.path,"/101822_prediction_results.RDS"))


