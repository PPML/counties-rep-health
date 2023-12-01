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
library(covidcast)
library(viridis)
library(boot)
library(wesanderson)
library(urbnmapr)

#Filepaths
in.path = "~/data_counties/Data/Processed/"
out.path = "~/counties-project/Results/"

final = readRDS(paste0(out.path,"073123_ACS_raked.RDS"))
models = readRDS(paste0(out.path,"260423_all_models.RDS"))
combined = readRDS(paste0(out.path,"073123_ACS_YRBS_combined.RDS"))
yrbs = readRDS(paste0(in.path,"YRBS_national.RDS"))
yrbs[,hadsex1 := as.numeric(as.character(hadsex))]
nsfg = readRDS(paste0(in.path,"NSFG_final.RDS"))[year == 2019]

#County mapping data
counties_sf <- get_urbn_map(map = "counties", sf = TRUE)
county_groups <- countydata %>% mutate(cat_var = paste0("Group ",sample(1:4, nrow(countydata), replace = TRUE)))

#Functions
source("~/counties-project/model_functions.R")

#####################################
#	Predictive model comparisons	#
####################################

##Overall
get_mods <- readRDS(paste0("~/counties-project/Results/260423_all_models.RDS"))
get_dat = rbindlist(lapply(get_mods$model_results, function(x) return(x$full_dat)))
get_dat[,run := 1] #Run argument provides an option to produce run by draws, but just 1 if not

d_strata = get_accuracy(data_test = get_dat, strata1 = c("age","sex"), weighted=TRUE)
d = d_strata$data %>% data.table()
d[,variable := factor(variable,
                      levels = c("p_bayes",
                                 "p_rf",
                                 "hadsex1",
                                 "p_logis",
                                 "p_lasso.p_lasso"),
                      labels = c("Bayesian model averaging",
                                 "Random Forest",
                                 "Observed",
                                 "Logistic regression",
                                 "Lasso"))]


d[,obs  := ifelse(variable == "Observed", 1, 0)]
gg = ggplot(d, aes(variable,value, fill=factor(obs), ymin=lower_ci, ymax=upper_ci)) + 
  labs(x = NULL, title = paste0("Proportion reporting having had sex")) + facet_grid(age ~ sex)
gg = pretty_strata(gg) + theme(axis.text.x = element_text(size = 8))
pdf(file = paste0(out.path,"predictive_model_overall_age_sex.pdf") , height = 8, width = 10)
  print(gg)
dev.off()

##Combos
all_combos = list("run",
                 c("age"),
                 c("sex"),
                 c("race_eth"),
                 c("household_cat"),
                 c("metro"),
                 c("mom_age_cat"),
                 c("age","sex"),
                 c("age","race_eth"),
                 c("sex","race_eth"),
                 c("age","sex","race_eth"),
                 c("age","household_cat"),
                 c("sex","household_cat"),
                 c("age", "sex","household_cat"),
                 c("age","metro"),
                 c("sex","metro"),
                 c("age", "sex","metro"),
                 c("age","mom_age_cat"),
                 c("sex","mom_age_cat"),
                 c("age", "sex","mom_age_cat"))
models=models[age != 19]              

pdf(file = paste0(out.path,"predictive_model_plots.pdf") , height = 8, width = 10)

	for(i in 2:length(all_combos)){
	  print(i)
	  var = all_combos[[i]]
		d_strata = get_accuracy(data_test = models, strata1 = var, weighted=TRUE)
		d = d_strata$data %>% data.table()
		d[,variable := factor(variable,
		                      levels = c("p_bayes",
		                      			      "p_rf",
		                      	         "hadsex1",
		                      			      "p_logis",
		                                 "p_lasso.p_lasso"),
		                      labels = c("Bayesian model averaging",
		                                 "Random Forest",
		                                 "Observed",
		                                 "Logistic regression",
		                                 "Lasso"))]
		

		d[,obs  := ifelse(variable == "Observed", 1, 0)]
		gg = ggplot(d, aes(variable,value, fill=factor(obs), ymin=lower_ci, ymax=upper_ci)) + 
	  					 labs(x = NULL, title = paste0("Proportion reporting having had sex by \n ",toString(all_combos[[i]])))
		gg = pretty_strata(gg) 
		
		if(length(var)  == 1) {
		  form = as.formula(paste0("~",var[1]))
		  gg = gg + facet_wrap(form,scales="free_y")
		  gg = gg + scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 12))
		}
		
		if(length(var)  == 2) {
		  form = as.formula(paste0(var[1],"~",var[2]))
		  gg = gg + facet_grid(form)
		  gg = gg + scale_x_discrete(breaks=NULL)
		}
		
		if(length(var)  == 3) {
		  form = as.formula(paste0(var[1],"~",var[2],"+",var[3]))
		  gg = gg + facet_grid(form)
		  gg = gg + scale_x_discrete(breaks=NULL)
		}


	  	print(gg)
	 }
	
dev.off()

###################################
#	YRBS & NSFG/ACS Comparisons	  #
###################################

#--- National ---#
nsfg[,hadsex1 := as.numeric(as.character(hadsex))]
nsfg[,age := as.numeric(as.character(age))]

#note: using sample size, not effective sample size, for sensible results that are comparable to survey package
d1 = yrbs %>% 
    group_by(age, sex) %>% 
    summarise(mean = weighted.mean(hadsex1, weight),  n= n())  %>% 
    mutate(variable = "yrbs") %>%
    data.table()
d2 = nsfg %>% 
    filter(age != 19) %>% 
    group_by(age, sex) %>% 
    summarise(mean = weighted.mean(hadsex1, weight), n= n()) %>% 
    mutate(variable = "nsfg") %>%
    data.table()

dd = rbind(d1,d2)
dd[,lower_ci := binconf(floor(mean * n), n)[,'Lower']]
dd[,upper_ci := binconf(floor(mean * n), n)[,'Upper']]

dd[,Source := factor(variable, levels = c("nsfg","yrbs"), labels = c("NSFG","YRBS"))]

png(file = "/Users/deepajahagirdar/Documents/NEEMA presentation/yrbs_vs_nsfg.png", width = 1000, height = 1500)
  
gg = ggplot(dd, aes(age, mean, ymin=lower_ci, ymax = upper_ci, col=variable)) + facet_wrap(~sex) +
            geom_point(size=3)  + 
            geom_errorbar(width=0.5,size=1) + 
            facet_wrap(~sex) + theme_bw() +
            labs(y = "Proportion Yes", x = "Age") + 
            theme(axis.text.x = element_text(size = 12, face="bold", colour = "black"),
                  axis.text.y = element_text(size = 12,  colour = "black"),
                  axis.title = element_text(size = 14),
                  plot.title = element_text(size = 14),
                  strip.background = element_rect(fill="white"),
                  strip.text = element_text(size = 12, face="bold"),
                  legend.text = element_text(size = 12, face="bold")) +
            scale_color_manual(name=NULL, breaks=c("nsfg","yrbs"), labels = c("NSFG","YRBS"),values = c("darkred","lightblue"))
  print(gg)       
  
dev.off() 
      

#--- State ---#

combined = combined[,c("scale_factor", "logit_scale_factor") := NULL]

mm = melt(combined, id.vars = c("age","sex","state","n_yrbs"))
combined_n = mm[variable == "acs_state",.(variable, age, sex, state)]
combined_n = rbind(combined_n, mm[variable == "yrbs_state",.(n=n_yrbs,variable, age, sex, state)] )
mm = merge(mm, combined_n, by=c("age","sex", "variable","state"))

mm[,Source := factor(variable, levels = c("acs_state","yrbs_state"), labels = c("ACS","YRBS"))]
mm[!is.na(value),lower_ci := binconf(floor(value*n),n)[,'Lower']]
mm[!is.na(value),upper_ci := binconf(floor(value*n),n)[,'Upper']]
setnames(mm, "state","state_abbv")
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

pdf(file = "/Users/deepajahagirdar/Documents/NEEMA presentation/acs_vs_yrbs_female.pdf", width = 10, height = 8)
	gg = ggplot(mm[sex == "female"], aes(age, value, col=Source,group=Source, ymin = lower_ci, ymax = upper_ci)) + 
		    geom_point(size = 0.5) + 
	      geom_errorbar(width=0.5) + facet_geo(~state_abbv,grid = "us_state_grid1") + 
			  labs(title = "ACS predictions and YRBS state-data for proportion having sex by age (Females)",y = "Proportion Yes", x="Age")
	gg = theme_geo(gg) + scale_color_manual(name=NULL, values = c("#CC79A7", "#56B4E9"))
	print(gg)
dev.off()

pdf(file = "/Users/deepajahagirdar/Documents/NEEMA presentation/acs_vs_yrbs_male.pdf", width = 10, height = 8)
  gg = ggplot(mm[sex == "male"], aes(age, value, col=Source,group=Source, ymin = lower_ci, ymax = upper_ci)) + 
    geom_point() + 
    geom_errorbar(width=0.5) + facet_geo(~state_abbv,grid = "us_state_grid1") + 
    labs(title = "ACS predictions and YRBS state-data for proportion having sex by age (Males)",y = "Proportion Yes", x="Age")
  gg = theme_geo(gg) + scale_color_manual(name=NULL, values = c("#CC79A7", "#56B4E9"))
  print(gg)
dev.off()

###################
#	Final Maps	  #
###################

#--- Overall ---#
acs_agg = final[,.(raked = mean(county_raked),
                   unraked =mean(county_unraked)), by = c("county_fips","age","sex")]
household_data <- left_join(counties_sf, acs_agg, by="county_fips")
household_data$group_raked <- cut(household_data$raked, c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,1.0), include.lowest = TRUE)
household_data$group_raked <- factor(household_data$group_raked,
									levels = c("(0,0.1]","(0.1,0.2]","(0.2,0.3]","(0.3,0.4]","(0.4,0.5]",
												"(0.5,0.6]","(0.6,0.7]","(0.7,0.8]","(0.8,1]"),
									labels = c("<0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5",
												"0.5-0.6","0.6-0.7",">0.7",">0.7"))
household_data$group_unraked <- cut(household_data$unraked, 
                                    c(-0.1,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,1.0))
household_data$group_unraked  <- factor(household_data$group_unraked ,
									levels = c("(-0.1,0.1]", "(0.1,0.2]","(0.2,0.3]",
									           "(0.3,0.4]","(0.4,0.5]",
												"(0.5,0.6]","(0.6,0.7]","(0.7,0.8]","(0.8,1]"),
									labels = c( "<0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5",
												"0.5-0.6","0.6-0.7",">0.7",">0.7"))
household_data_males = household_data[household_data$sex == "male",]
household_data_females = household_data[household_data$sex == "female",]

##ID missing counties
acs_agg %>% filter(age == 15) %>% group_by(sex) %>% summarise(n_distinct(county_fips))
acs_agg[sex == "male" & age == 15,unique(county_fips)][!acs_agg[sex == "male" & age == 15,unique(county_fips)] %in% 
                                             acs_agg[sex == "female" & age == 15,unique(county_fips)]]


all_counties = counties_sf %>% data.table()
select_counties = acs[age == 15 & sex == "female",.(age,sex,county_name=cntyname)]
missing = all_counties[!all_counties[,county_name] %in% select_counties[age == 15 & sex == "female",unique(county_name)],county_name]
county_fips_miss = counties_sf[counties_sf$county_name == "Glades County",]


png(paste0(out.path,"/full_map_raked_males.png"), width = 1000, height = 800)
  gg = ggplot(household_data_males) + 
       geom_sf(aes(fill = factor(group_raked)), color=NA) + 
       scale_fill_viridis_d(name="Proportion Yes",limits = levels(household_data$group_raked)) +
       facet_wrap(~age) +  
         theme(panel.background = element_rect(fill="white"),
               axis.text = element_blank(),
               axis.ticks = element_blank(),
               panel.grid = element_blank(),
               legend.text = element_text(size = 16),
               legend.title = element_text( size = 16))
               
  print(gg)
dev.off()

png(paste0(out.path,"/full_map_raked_females.png"), width = 1000, height = 800)
  gg = ggplot(household_data_females) + 
    geom_sf(aes(fill = factor(group_raked)), color=NA) + 
    scale_fill_viridis_d(name="Proportion Yes",limits = levels(household_data$group_raked)) +
    facet_wrap(~age) +  
    theme(panel.background = element_rect(fill="white"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          legend.text = element_text(size = 16),
          legend.title = element_text( size = 16))
  
  print(gg)
dev.off()

png(paste0(out.path,"/full_map_unraked_males.png"), width = 1000, height = 800)
  gg = ggplot(household_data_males) + 
    geom_sf(aes(fill = factor(group_unraked)), color=NA) + 
    scale_fill_viridis_d(name="Proportion Yes",limits = levels(household_data$group_raked)) +
    facet_wrap(~age) +  
    theme(panel.background = element_rect(fill="white"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          legend.text = element_text(size = 16),
          legend.title = element_text( size = 16))
  
  print(gg)
dev.off()

png(paste0(out.path,"/full_map_unraked_females.png"), width = 1000, height = 800)
  gg = ggplot(household_data_females) + 
    geom_sf(aes(fill = factor(group_unraked)), color=NA) + 
    scale_fill_viridis_d(name="Proportion Yes",limits = levels(household_data$group_raked)) +
    facet_wrap(~age) +  
    theme(panel.background = element_rect(fill="white"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          legend.text = element_text(size = 16),
          legend.title = element_text( size = 16))
  
  print(gg)
dev.off()


#--- Overall Logit ---#
acs_agg = final[,.(raked_logit = weighted.mean(inv.logit(logit_raked),weight), 
                   raked_normal = weighted.mean(raked,weight)), by = c("county_fips")]
household_data <- left_join(counties_sf, acs_agg, by="county_fips")

household_data$group_raked_logit <- cut(household_data$raked_logit, c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,1.0), include.lowest = TRUE)
household_data$group_raked_logit <- factor(household_data$group_raked_logit,
                                     levels = c("(0,0.1]","(0.1,0.2]","(0.2,0.3]","(0.3,0.4]","(0.4,0.5]",
                                                "(0.5,0.6]","(0.6,0.7]","(0.7,0.8]","(0.8,1]"),
                                     labels = c("<0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5",
                                                "0.5-0.6","0.6-0.7",">0.7",">0.7"))
household_data$group_raked_normal <- cut(household_data$raked_normal, c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,1.0), include.lowest = TRUE)
household_data$group_raked_normal  <- factor(household_data$group_raked_normal ,
                                        levels = c("(0,0.1]","(0.1,0.2]","(0.2,0.3]","(0.3,0.4]","(0.4,0.5]",
                                                   "(0.5,0.6]","(0.6,0.7]","(0.7,0.8]","(0.8,1]"),
                                        labels = c("<0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5",
                                                   "0.5-0.6","0.6-0.7",">0.7",">0.7"))

png("/Users/deepajahagirdar/Documents/NEEMA presentation/full_map_logit.png", width = 1000, height = 800)
gg1 = pretty_map(ggplot(household_data) + 
                  geom_sf(aes(fill = factor(group_raked_logit)),  lwd = 0.03, col="black") + 
                  scale_fill_viridis_d(name="Proportion Yes",limits = levels(household_data$group_raked_logit)))

gg2 = pretty_map(ggplot(household_data) + 
                  geom_sf(aes(fill = factor(group_raked_normal)),  lwd = 0.03, col="black") + 
                  scale_fill_viridis_d(name="Proportion Yes",limits = levels(household_data$group_raked_logit)))

print(gridExtra::grid.arrange(gg1,gg2))
dev.off()

acs_agg = final[,.(raked_logit = weighted.mean(inv.logit(logit_raked),weight), 
                   raked_normal = weighted.mean(raked,weight)), by = c("county_fips","age","sex")]
household_data <- left_join(counties_sf, acs_agg, by="county_fips")

household_data$group_raked_logit <- cut(household_data$raked_logit, c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,1.0), include.lowest = TRUE)
household_data$group_raked_logit <- factor(household_data$group_raked_logit,
                                           levels = c("(0,0.1]","(0.1,0.2]","(0.2,0.3]","(0.3,0.4]","(0.4,0.5]",
                                                      "(0.5,0.6]","(0.6,0.7]","(0.7,0.8]","(0.8,1]"),
                                           labels = c("<0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5",
                                                      "0.5-0.6","0.6-0.7",">0.7",">0.7"))
household_data$group_raked_normal <- cut(household_data$raked_normal, c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,1.0), include.lowest = TRUE)
household_data$group_raked_normal  <- factor(household_data$group_raked_normal ,
                                             levels = c("(0,0.1]","(0.1,0.2]","(0.2,0.3]","(0.3,0.4]","(0.4,0.5]",
                                                        "(0.5,0.6]","(0.6,0.7]","(0.7,0.8]","(0.8,1]"),
                                             labels = c("<0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5",
                                                        "0.5-0.6","0.6-0.7",">0.7",">0.7"))


png("/Users/deepajahagirdar/Documents/NEEMA presentation/full_map_logit_age_sex.png", width = 1000, height = 800)
gg1 = pretty_map(ggplot(household_data) + 
                   geom_sf(aes(fill = factor(group_raked_logit)),  lwd = 0.03, col="black") + 
                   scale_fill_viridis_d(name="Proportion Yes",limits = levels(household_data$group_raked_logit)) +
                   facet_wrap(~age + sex))

gg2 = pretty_map(ggplot(household_data) + 
                   geom_sf(aes(fill = factor(group_raked_normal)),  lwd = 0.03, col="black") + 
                   scale_fill_viridis_d(name="Proportion Yes",limits = levels(household_data$group_raked_logit))+
                   facet_wrap(~age + sex))

print(gridExtra::grid.arrange(gg1,gg2))
dev.off()

acs_agg_m = melt(acs_agg, id.vars = c("county_fips","age","sex"))
ggplot(acs_agg_m, aes(value, fill=variable)) +
  geom_histogram() + facet_wrap(~age + sex) + theme_bw()

#--- Age & Sex ---#
acs_agg = final[,.(raked = mean(county_raked), unraked = mean(county_unraked)), by = c("county_fips","age","sex")]
acs_agg[raked > 1, raked := 1]

household_data <- left_join(counties_sf, acs_agg, by="county_fips")
household_data$group_raked <- cut(household_data$raked, c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,1.0), include.lowest = TRUE)
household_data$group_raked <- factor(household_data$group_raked,
									levels = c("[0,0.1]", "(0.1,0.2]","(0.2,0.3]","(0.3,0.4]","(0.4,0.5]",
												"(0.5,0.6]","(0.6,0.7]","(0.7,0.8]","(0.8,1]"),
									labels = c("<0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5",
												"0.5-0.6","0.6-0.7",">0.7",">0.7"))
household_data$group_unraked <- cut(household_data$unraked, c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,1.0), include.lowest = TRUE)
household_data$group_unraked  <- factor(household_data$group_unraked ,
									levels = c("[0,0.1]", "(0.1,0.2]","(0.2,0.3]","(0.3,0.4]","(0.4,0.5]",
												"(0.5,0.6]","(0.6,0.7]","(0.7,0.8]","(0.8,1]"),
									labels = c("<0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5",
												"0.5-0.6","0.6-0.7",">0.7",">0.7"))

household_data <- household_data[!is.na(household_data$age),]
household_data$sex <- factor(household_data$sex, levels = c("male","female"), labels = c("Male","Female"))

age_dat = household_data
gg1 = ggplot(counties_sf[counties_sf$state_name=="South Dakota",]) + 
  geom_sf(aes(fill=county_fips)) + scale_fill_discrete(guide="none")
ggplotly(gg1)
full_county = counties_sf[counties_sf$state_name=="South Dakota",]
full_county = full_county$county_fips
age_county = age_dat[age_dat$state_name=="South Dakota" & 
                       age_dat$age == 15 & age_dat$sex == "Female",] %>% data.table()
age_county = unique(age_county$county_fips)
full_county[!full_county %in% age_county]

#ex 46031
age_dat[age_dat$sex == "Female" & age_dat$state_name == "South Dakota" &
          age_dat$county_fips == 46031,]

##Using separate folders to create graphic animations for powerpoint
png(filename = "~/counties-project/tables-figures/female_all_ages_unraked.png", width = 1000, height = 800)
	gg = pretty_map(
					  ggplot(age_dat[age_dat$sex == "Female",]) + 
					  geom_sf(aes(fill = factor(group_unraked)),  lwd = 0.03, col="black") + 
					  scale_fill_viridis_d(name="Proportion Yes",limits = levels(household_data$group_raked)) +
					  facet_wrap(~age)
					)
	print(gg)

dev.off() 


png(filename = "~/counties-project/tables-figures/female_all_ages_raked.png", width = 1000, height = 800)
	gg = pretty_map(
            	    ggplot(age_dat[age_dat$sex == "Female",]) + 
            	    geom_sf(aes(fill = factor(group_raked)),  lwd = 0.03, col="black") + 
            	    scale_fill_viridis_d(name="Proportion Yes",limits = levels(household_data$group_raked)) +
            	    facet_wrap(~age)
	                )
	print(gg)
dev.off() 

png(filename = "~/counties-project/tables-figures/male_all_ages_unraked.png", width = 1000, height = 800)
    gg = pretty_map(
      ggplot(age_dat[age_dat$sex == "Male",]) + 
        geom_sf(aes(fill = factor(group_unraked)),  lwd = 0.03, col="black") + 
        scale_fill_viridis_d(name="Proportion Yes",limits = levels(household_data$group_raked)) +
        facet_wrap(~age)
    )
    print(gg)
dev.off() 

png(filename = "~/counties-project/tables-figures/male_all_ages_raked.png", width = 1000, height = 800)
    gg = pretty_map(
      ggplot(age_dat[age_dat$sex == "Male",]) + 
        geom_sf(aes(fill = factor(group_raked)),  lwd = 0.03, col="black") + 
        scale_fill_viridis_d(name="Proportion Yes",limits = levels(household_data$group_raked)) +
        facet_wrap(~age)
    )
    print(gg)
dev.off() 


#--- Age & Sex Logit ---#
acs_agg = final[,.(raked_logit = weighted.mean(inv.logit(logit_raked), weight), 
                   raked_normal = weighted.mean(raked, weight)), by = c("county_fips","age","sex")]
acs_agg = melt(acs_agg, id.vars = c("county_fips","age","sex"))
household_data <- left_join(counties_sf, acs_agg, by="county_fips")
household_data <- household_data[!is.na(household_data$age),]
household_data$sex <- factor(household_data$sex, levels = c("male","female"), labels = c("Male","Female"))

age_dat = household_data

pdf(file = "/Users/deepajahagirdar/Documents/NEEMA presentation/compare_logit_raking.pdf", width = 10, height = 8)
gg1 = ggplot(age_dat) + 
    geom_sf(aes(fill = value),  lwd = 0) + 
    scale_fill_viridis_c(guide="none") +
    facet_grid(age ~ variable + sex) +
    theme(panel.background = element_rect(fill="white"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank())

print(gg1)

gg2 = ggplot(age_dat) + 
  geom_histogram(aes(value, fill=value)) +
  xlab("Proportion Yes") + theme_bw() +
  facet_grid(age ~ variable + sex)

print(gg)

dev.off() 

gridExtra::grid.arrange(gg1, gg2, ncol=2)


#######################
## Age-Sex scalars	##
######################
state_final = final[,.(mean_scalar = mean(scale_factor), 
                       mean_est = mean(county_prediction),
                       mean_raked = mean(raked)), by = c("state","age","sex")]
ggplot(state_final, aes(age, mean_scalar, col=sex)) + 
  geom_point() + facet_wrap(~state) + theme_bw()
ggplot(state_final, aes(age, mean_raked, col=sex)) + 
         geom_point() + facet_wrap(~state) + theme_bw() + ylim(c(0,0.8))
ggplot(state_final, aes(age, mean_est, col=sex)) + 
         geom_point() + facet_wrap(~state) + theme_bw() + ylim(c(0,0.8))

no_Est = c("HI","IN","GA","UT","NJ", "AL","DC","DE","KY","LA","MN","MO","NC","NM","OR","TN","VT","WA","WV","WY")

state_melt = melt(state_final, id.vars = c("age","sex","state"))
outliers = rbind(state_melt[age == 15 & value > 0.6],
                 state_melt[age == 16 & value > 0.9],
                 state_melt[age == 17 & value > 0.85],
                 state_melt[age == 18 & value > 1.2])
ggplot(state_melt[ !state %in% no_Est & variable == "mean_scalar" ], 
       aes(age, value, col=sex)) + geom_boxplot() + theme_bw() +
       #geom_text(data = outliers, aes(age, value, label = state)) +
      ggrepel::geom_text_repel(data = outliers, aes(age, value, label = state),
                      min.segment.length = unit(0, 'lines'))



ggplot(state_melt[age == 15 & !state %in% no_Est ], 
       aes(variable, value, col=sex)) +
  geom_point() + facet_wrap(~state) + theme_bw() + ylim(c(0.05,0.80)) +
  geom_line(aes(group = sex))
ggplot(state_melt[age == 18 & !state %in% no_Est & !variable == "mean_scalar"], 
       aes(variable, value, col=sex)) +
  geom_point() + facet_wrap(~state) + theme_bw() + 
  geom_line(aes(group = sex)) + ylim(c(0.05,0.80)) 
ggplot(state_melt[age %in% c(15,18) & 
                    !state %in% no_Est &
                    variable == "mean_scalar"], 
       aes(age, value, col=sex)) +
  geom_point() + facet_wrap(~state) + theme_bw() 



