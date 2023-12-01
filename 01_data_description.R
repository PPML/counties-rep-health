
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
library(weights)
library(rsample)


#Filepaths
in.path = "~/data_counties/Data/Raw/"
out.path = "~/data_counties/Data/Processed/"

nsfg = readRDS(paste0(in.path,"NSFG_Raw_final.RDS"))
acs = readRDS(paste0(in.path,"ACS_Raw_final.RDS")) #Note that this was already limited to age 14-19 to limit the size of the data download
yrbs = readRDS(paste0(in.path,"YRBS_Raw_final.RDS")) 


#Sample restrictions - age only, and complete cases
nsfg = nsfg[age %in% c(15:18)]
acs = acs[age %in% c(15:18)]
yrbs = yrbs[age %in% c(15:18) & year == 2019]

##NSFG sample sizes
full_sample_nsfg = nrow(nsfg)
nsfg = nsfg[complete.cases(nsfg)]
nsfg_sample = nrow(nsfg)
age_nsfg = nsfg[,.(count=.N), by = c("age","sex")]


##ACS total sample sizes
full_sample_acs = nrow(acs)
acs[,grade := NULL]
acs_sample = nrow(acs[complete.cases(acs)])
acs = acs[complete.cases(acs)]
age_acs = acs[,.(count=.N), by = c("age","sex")]

##YRBS total sample sizes
full_sample_yrbs = nrow(yrbs)
limit = yrbs[,.(year, sitecode, age, sex, race_eth, hadsex, weight)]
yrbs_sample = nrow(limit[complete.cases(yrbs)])
yrbs =limit[complete.cases(limit),]
age_yrbs = yrbs[,.(count=.N), by = c("age","sex")]

##ACS state Sample sizes before merging PUMA (because this ends up multiplying rows), but after removing missing data
sampe_sizes = acs[,.(count=.N), by= c("age","sex","state_code")] 
sampe_sizes[,state_code := as.character(state_code)]
sampe_sizes[nchar(state_code) < 2, state_code := paste0("0",state_code),state_code]

acs_sample = list(full_sample_acs = full_sample_acs, 
                  final_sample_acs = acs_sample,
                  ages = age_acs)
nsfg_sample = list(full_sample_nsfg = full_sample_nsfg,
                   final_sample_nsfg = nsfg_sample,
                   ages = age_nsfg)
yrbs_sample = list(full_sample_yrbs = full_sample_yrbs,
                   final_sample_yrbs = yrbs_sample,
                   ages = age_yrbs)

saveRDS(sampe_sizes, paste0(out.path,"ACS_n_size.RDS")) #For future use
saveRDS(list(acs_sample, nsfg_sample, yrbs_sample), paste0(out.path,"all_sample_sizes.RDS"))


##Merge counties for ACS
acs[,CBSERIAL := NULL]
puma_map = fread(paste0(in.path,"geocorr2018.csv")) ##File given by Marissa Reitsema
county_census = data.table(county_census)
setnames(county_census, c("FIPS","POPESTIMATE2019"),c("county_fips","pop2019"))
county_census[,county_fips := as.numeric(county_fips)]
puma_map = merge(puma_map, county_census[,.(county_fips, pop2019)])

#Some county fips are available in acs
acs[,county_fips_acs := as.character(COUNTYFIP)]
acs[nchar(county_fips_acs)  == 2 & county_fips_acs != 0, county_fips_acs := paste0("0",county_fips_acs)]
acs[nchar(county_fips_acs)  == 1 & county_fips_acs != 0, county_fips_acs := paste0("00",county_fips_acs)]
acs[county_fips_acs != 0, county_fips_acs := paste0(state_code,county_fips_acs)]

acs <- merge(acs, puma_map[,.(cntyname, county_fips, PUMA, state_code, state, pop10,pop2019)], by = c("PUMA", "state_code"), allow.cartesian = T)
acs[,county_fips := as.character(county_fips)]
acs[nchar(county_fips) < 5, county_fips := paste0("0",county_fips)]
acs[,state_code := as.character(state_code)]
acs[nchar(state_code) < 2, state_code := paste0("0",state_code),state_code]

saveRDS(nsfg, paste0(out.path,"NSFG_final.RDS"))
saveRDS(acs, paste0(out.path,"ACS_final.RDS")) 
saveRDS(yrbs, paste0(out.path,"YRBS_final.RDS")) 

#Create bootstrap resamples - cannot create ACS due to size
set.seed(12345)
n=100

xx <- rsample::bootstraps(yrbs, times = n)
saveRDS(xx, paste0(out.path,"YRBS_bootstrap.RDS"))

xx <- rsample::bootstraps(nsfg, times = n)
saveRDS(xx, paste0(out.path,"NSFG_bootstrap.RDS")) 

##Compare YRBS national to NSFG
yrbs = readRDS(paste0(in.path,"YRBS_national.RDS"))
yrbs[,hadsex1 := as.numeric(as.character(hadsex))]
nsfg = readRDS(paste0(in.path,"NSFG_final.RDS"))[year == 2019]
nsfg[,hadsex1 := as.numeric(as.character(hadsex))]
nsfg[,age := as.numeric(as.character(age))]

#note: using sample size, not effective sample size, for sensible results that are comparable to survey package
d1 = yrbs %>% group_by(age, sex) %>% summarise(mean = weighted.mean(hadsex1, weight),  n= n())  %>% data.table()
d2 = nsfg %>% filter(age != 19) %>% group_by(age, sex) %>% summarise(mean = weighted.mean(hadsex1, weight), n= n()) %>% data.table()
d1[,lower_ci := mean + (qnorm(0.025)*(sqrt(mean*(1-mean)/n)))]
d1[,upper_ci := mean + (qnorm(0.975)*(sqrt(mean*(1-mean)/n)))]
d1[,variable := "yrbs"]

d2[,lower_ci := mean + (qnorm(0.025)*(sqrt(mean*(1-mean)/n)))]
d2[,upper_ci := mean + (qnorm(0.975)*(sqrt(mean*(1-mean)/n)))]
d2[,variable := "nsfg"]

dd = rbind(d1,d2)

png(filename = "/Users/deepajahagirdar/Documents/NEEMA presentation/nsfg_vs_yrbs.png", 
    width = 800, height = 600)
gg=ggplot(dd, aes(age, mean, col = variable, group=variable, ymin = lower_ci, ymax = upper_ci)) + 
  geom_point(size=3)  + 
  geom_errorbar(width=0.5,size=1) + 
  facet_wrap(~sex) + theme_bw() +
  labs(y = "Proportion Yes", x = "Age") + 
  theme(axis.text.x = element_text(size = 18, face="bold", colour = "black"),
        axis.text.y = element_text(size = 18,  colour = "black"),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        strip.background = element_rect(fill="white"),
        strip.text = element_text(size = 22, face="bold"),
        legend.text = element_text(size = 22, face="bold")) +
  scale_color_manual(name=NULL, breaks=c("nsfg","yrbs"), labels = c("NSFG","YRBS"),values = c("darkred","lightblue"))
print(gg)
dev.off()


























nsfg[,hadsex1 := as.numeric(as.character(hadsex))]

#Compare characteristics of those who go and don't go to school
acs = acs[!is.na(ethnicity)]
acs[,ethnicity1 := factor(ethnicity, levels = c("non_hisp","hisp"), labels = c(0,1))]
acs[,ethnicity1 := as.numeric(as.character(ethnicity1))]
acs[,goschol1 := as.numeric(as.character(goschol))]

nsfg = nsfg[!is.na(ethnicity)]
nsfg = nsfg[!is.na(mom_age)]
nsfg[,ethnicity1 := factor(ethnicity, levels = c("non_hisp","hisp"), labels = c(0,1))]
nsfg[,ethnicity1 := as.numeric(as.character(ethnicity1))]
nsfg[,goschol1 := as.numeric(as.character(goschol))]

vars = c("ethnicity1","age","household_cat","race","mom_age_cat","metro","sex","hadsex","household_comp")

var = "age"
dx = list()
for(var in vars){
  #if(!var %in% colnames(acs)) next
  dx = rbind(dx,
             nsfg[,list(weighted.mean(goschol1,weight)), by = c(var)] %>% 
               melt(id.vars = "V1") %>%
               data.table())
}

dx[,data := "NSFG"]
dx1 = dx

dx[,data := "ACS"]
dx2 = dx

dx = rbind(dx1,dx2)
ggplot(dx, aes(value, V1, fill = data)) + 
  geom_bar(stat="identity", position = "dodge") +  
  facet_wrap(~variable, scales = "free") + 
  ylab("Proportion still in school")

nsfg %>% group_by(hadsex) %>% 
  summarise(prop_goschol = weighted.mean(goschol1, weights = weight))
nsfg %>% group_by(age, goschol) %>% 
  summarise(prop_had_sex = weighted.mean(hadsex1, weights = weight),
            sample_size = n())

options("survey.lonely.psu")
s = svydesign(ids = ~0, weights = ~PERWT, data = dt)
t <- svymean(~mom_birth_age,s)
mean(dt$mom_birth_age)
weighted.mean(acs$hhincome_orig,  acs$weight)
weighted.mean(acs$hhincome_orig,  acs$hhweight)
mean(acs$hhincome_orig)
median(acs$hhincome_orig)

svyby(hadsex+goschol, ~age, s, svymean)
svyby(~mom_birth_age, ~RACE, s, svymean)
svyby(~mom_birth_age, ~RACE, s, svymean)
svyby(~mom_birth_age, ~RACE + AGE, s, unwtd.count)

pct = data.table(pct = c(wpct(acs$hhincome, weight=acs$weight),wpct(acs$hhincome)),
                 hhincome = names(wpct(acs$hhincome)),
                 type = c(rep("weighted",length(wpct(acs$hhincome, weight=acs$weight))),
                          rep("unweighted",length(wpct(acs$hhincome)))))
pct[,hhincome := factor(hhincome, levels = names(wpct(acs$hhincome)))]
ggplot(pct, aes(hhincome, pct, fill=type)) + 
  geom_bar(stat = "identity", position = "dodge") + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
colnames(acs)
var = "age"
acs1 = wpct(acs[,get(var)], weight=acs$weight)
nsfg1 =  wpct(nsfg[,get(var)], weight=nsfg$weight)
pct = data.table(pct = c(acs1,nsfg1),
                 var = c(names(acs1),names(nsfg1)),
                 source = c(rep("ACS",length(acs1)),
                          rep("NSFG",length(nsfg1))))
ggplot(pct, aes(var, pct, fill=source)) + 
  geom_bar(stat = "identity", position = "dodge") + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


prop.table(table(acs$agemomb)) ##NEED TO CHECK THIS VARIABLE
prop.table(table(nsfg$race))

prop.table(tapply(acs$weight, list(acs$race, acs$hhincome), sum), 2)
prop.table(tapply(nsfg$weight, list(nsfg$race, nsfg$hhincome), sum), 2)


#YRBS
##Imputation
yrbs_impute1 = fread(paste0(in.path,"yrbs_since_2010.csv" ))
yrbs_sum = data.table(expand.grid(state = unique(yrbs_impute1$sitename),
                      no = 1,
                      yes = 1,
                      year = unique(yrbs_impute1$year),
                      sex  = unique(yrbs_impute1$sex),
                      grade = unique(yrbs_impute1$grade)))

for(s in unique(yrbs_impute1$sitename)){
  for(y in unique(yrbs_impute1$year)){
    for(f in unique(yrbs_impute1$sex)){
      for(g in unique(yrbs_impute1$grade)){
    
  ss = yrbs_impute1[sitename == s & year == y & sex == f & grade == g]
  props = wpct(ss[,get(var)], weight=ss$weight)
  yrbs_sum[state == s & year == y & sex == f & grade == g, yes := as.numeric(props[1])]
  yrbs_sum[state == s & year == y & sex == f & grade == g, no := as.numeric(props[2])]
      }
    }
  }
}

national = data.table(state = "national", 
                      year = c(2011,2013,2015,2017),
                      value = c(0.47,0.47,0.4,0.39), 
                      variable = "yes")
yrbs_sum = melt(yrbs_sum[,.(state,yes,year,grade,sex)], 
                id.var = c("state","year","grade","sex"))
yrbs_sum = rbind(yrbs_sum,national)
yrbs_sum[,grade := factor(grade, levels = c(1,2,3,4), labels = c(9,10,11,12))]
yrbs_sum[,sex := factor(sex, levels = c(1,2), labels = c("male","female"))]

ggplot(yrbs_sum[state == "Delaware"], 
       aes(year, value, color = state, group = state)) + 
  geom_point() + geom_line() +
  ylab("Proportion reporting ever having sex") + 
  theme_bw() + facet_grid(sex~grade)

ggplot(yrbs_sum[state == "Hawaii"], 
       aes(year, value, color = state, group = state)) + 
  geom_point() + geom_line() +
  ylab("Proportion reporting ever having sex") + 
  theme_bw() + facet_grid(sex~grade)

ggplot(yrbs_sum[state == "New Jersey"], 
       aes(year, value, color = state, group = state)) + 
  geom_point() + geom_line() +
  ylab("Proportion reporting ever having sex") + 
  theme_bw() + facet_grid(sex~grade)

ggplot(yrbs_sum[state == "Tennessee"], 
       aes(year, value, color = state, group = state)) + 
  geom_point() + geom_line() +
  ylab("Proportion reporting ever having sex") + 
  theme_bw() + facet_grid(sex~grade)

ggplot(yrbs_sum[state == "Wyoming"], 
       aes(year, value, color = state, group = state)) + 
  geom_point() + geom_line() +
  ylab("Proportion reporting ever having sex") + 
  theme_bw() + facet_grid(sex~grade)

ggplot(yrbs_sum[state == "New Mexico"], 
       aes(year, value, color = state, group = state)) + 
  geom_point() + geom_line() +
  ylab("Proportion reporting ever having sex") + 
  theme_bw() + facet_grid(sex~grade)


yrbs_sum = data.table(state = unique(yrbs$sitename),
                      no = 1,
                      yes = 1)
for(s in unique(yrbs$sitename)){
  print(s)
  ss = yrbs[sitename == s]
  props = wpct(ss[,get(var)], weight=ss$weight)
  yrbs_sum[state == s, yes := as.numeric(props[1])]
  yrbs_sum[state == s, no := as.numeric(props[2])]
}
yrbs_sum = melt(yrbs_sum[,.(state,yes)], id.var = "state")

median(yrbs_sum$value,na.rm=TRUE)
range(yrbs_sum$value,na.rm=TRUE)

spdf =  geojson_read("~/Documents/county_project/us_states_hexgrid.geojson", what = "sp")
spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))
spdf_fortified <- data.table(spdf_fortified)
spdf_fortified[,yrbs := NA]
spdf_fortified = merge(spdf_fortified, yrbs_sum, by.x = "id", by.y = "state",all.x = TRUE)
spdf_fortified[,value := as.numeric(value)]

ggplot() +
  geom_polygon(data = spdf_fortified,
               aes( x = long, y = lat, group = group, fill = value),  color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id)) +
  theme_void() +
  coord_map() +
  scale_fill_continuous(type = "viridis") +
  theme(legend.position = "top")

##By grade
var = "hadsex"
yrbs_sum = data.table(expand.grid(state = unique(yrbs$sitename),
                      no = 1,
                      yes = 1,
                      grade = unique(yrbs$grade)))
for(s in unique(yrbs$sitename)){
  for(j in unique(yrbs$grade)){
  ss = yrbs[sitename == s & grade == j]
  props = wpct(ss[,get(var)], weight=ss$weight)
  yrbs_sum[state == s & grade == j, yes := as.numeric(props[1])]
  yrbs_sum[state == s & grade == j, no := as.numeric(props[2])]
  }
}

yrbs_sum = melt(yrbs_sum[,.(state,yes,grade)], id.var = c("state","grade"))
yrbs_sum[,grade := factor(grade, levels = c(1,2,3,4,5), labels = c(9,10,11,12,0))]
yrbs_sum = yrbs_sum[grade != 0]

ggplot(yrbs_sum, aes(state, grade, fill = value)) + geom_tile() + coord_flip() +
  scale_fill_continuous(type = "viridis") +
  theme(legend.position = "top")

##By race
var = "hadsex"
yrbs_sum = data.table(expand.grid(state = unique(yrbs$sitename),
                                  no = 1,
                                  yes = 1,
                                  race2 = unique(yrbs$race2)))
for(s in unique(yrbs$sitename)){
  for(j in unique(yrbs$race2)){
    ss = yrbs[sitename == s & race2 == j]
    props = wpct(ss[,get(var)], weight=ss$weight)
    yrbs_sum[state == s & race2 == j, yes := as.numeric(props[1])]
    yrbs_sum[state == s & race2 == j, no := as.numeric(props[2])]
  }
}

yrbs_sum = melt(yrbs_sum[,.(state,yes,race2)], id.var = c("state","race2"))
yrbs_sum[,race := factor(race, levels = c(1,2,3,4,5,6,7), 
                          labels = c("American Indian/Alaska Native",
                                      "Asian","Black or African American",
                                     "Hispanic/Latino","Native Hawaiian/Other PI",
                                     "White","Multiple (Non Hispanic)"))]
ggplot(yrbs_sum[!is.na(race2)], aes(state, race2, fill = value)) + geom_tile() + coord_flip() +
  scale_fill_continuous(type = "viridis") +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45,hjust = 1))


yrbs[race == 2 & sitename == "Idaho"]
yrbs[race == 2 & sitename == "Mississippi"]

acs[state == "WV" & PUMA == 100 & age == 18 & sex == "male"]
acs[state == "WV" & PUMA == 100 & age == 18 & sex == "male" & county_fips == 54009, ]
acs[state == "WV" & PUMA == 100 & age == 18 & sex == "male" & county_fips == 54029, ]

##Check if 5 year ACS goes down to county
