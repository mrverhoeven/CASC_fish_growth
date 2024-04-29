#'---
#' title: "CASC Midwestern Fish Survey Data Tutorial"
#' author: "Holly Kundel, Mike Verhoeven, Denver Link, Gretchen Hansen"
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---

#' 
#' This script will show how to use the apache arrow package to access a "parquet" file holding the big fish data. We'll show how once those fish data are connected, common queries can be done using dplyr() syntax to generate summaries and slices of the data that are small enough to handle in R memory.
#' 


#' # Document Preamble
#+ warning = FALSE

# load libraries ------------------------------------------------------------------
# ## Load Libraries


library(arrow)
library(tidyverse)
# library(mnsentinellakes)
library(ggplot2)
library(zoo)
library(pals)
library(lme4)
library(data.table)


# connect to data -------------------------------------------------
#' ## Connect to Data

#open the connection to MN data 
mn_ALK <- open_dataset(sources = "scripts&data/data/input/age_assigned_data/mn_halk_aged_data/most_common_structures") #note that you point this fn at the parent folder of the parquet
wi_ALK <- open_dataset(sources = "scripts&data/data/input/age_assigned_data/wi_halk_aged_data/most_common_structures") #note that you point this fn at the parent folder of the parquet
mi_ALK <- open_dataset(sources = "scripts&data/data/input/age_assigned_data/mi_halk_aged_data/most_common_structures") #note that you point this fn at the parent folder of the parquet
ia_ALK <- open_dataset(sources = "scripts&data/data/input/age_assigned_data/ia_halk_aged_data/most_common_structures") #note that you point this fn at the parent folder of the parquet
sd_ALK <- open_dataset(sources = "scripts&data/data/input/age_assigned_data/sd_halk_aged_data/most_common_structures") #note that you point this fn at the parent folder of the parquet
il_ALK <- open_dataset(sources = "scripts&data/data/input/age_assigned_data/il_halk_aged_data/most_common_structures") #note that you point this fn at the parent folder of the parquet
in_ALK <- open_dataset(sources = "scripts&data/data/input/age_assigned_data/in_halk_aged_data/most_common_structures") #note that you point this fn at the parent folder of the parquet




mn_ALK %>% glimpse()

mn_ALK %>% 
   group_by(alk, species) %>%
  summarise(n = n(), n_surveys = n_distinct(total.effort.ident) ) %>% 
  collect() %>% print(n = 100)

mn_ALK %>% group_by(length.unit) %>% count() %>% collect()

#restrict to year level - all mm
mn_ALK %>% 
  filter(alk=="year") %>% 
  group_by(species, est.age) %>%
  summarise(n = n(), mean = mean(length), median=median(length)) %>% 
  arrange(species, est.age) %>% 
  collect() %>% 
  {.->> mean_LAA}

mn_ALK %>% 
  filter(alk == "year") %>% 
  arrange(species, est.age) %>%
  collect() %>% 
  {.->>all_ALKed}




ggplot(mean_LAA, aes(est.age, mean, group = species))+
  geom_line(aes(color= species))
# 
# ggplot(all_ALKed, aes(est.age, length, group = species))+
#   geom_point() +
#   facet_wrap(~species, scales = "free")+
#   geom_smooth()



# MN lake level time to 15" --------------------------------------------------


# lake-level time to 15"

#mean time to 15" walleye
mn_ALK %>%
  filter(species=="walleye") %>%
  group_by(alk) %>%
  summarise(n = n()) %>% 
  collect()


mn_ALK %>%
  filter(species=="walleye") %>%
  filter(!alk %in% c("species")) %>%
  filter(est.age <50) %>% 
  filter(year>1993) %>% 
  group_by(lake.id, lake.name, nhdhr.id, year) %>% 
  summarise(n = n(),
            mean_age = mean(est.age, na.rm = T),
            med_age = median(est.age, na.rm = T),
            mean_y15 = mean(ifelse(length > 368.3 & length < 393.7, est.age, NA), na.rm = T),
            median_y15 = median(ifelse(length > 368.3 & length < 393.7, est.age, NA), na.rm = T),
            mean_size = mean(length, na.rm=T),
            sd_length = sd(length, na.rm = T)
            
  ) %>% 
  collect() %>% 
  {. ->> wae_time_to15}

ggplot(wae_time_to15, aes(mean_y15, group = year))+
  # geom_density(aes(color = year), bw = 2, size = .75, alpha =  )
  geom_line(aes(color=year), stat="density", bw = .5, linewidth=1, alpha=0.5)+ 
  scale_x_continuous(breaks=c(1:10))

ggplot(wae_time_to15, aes(year, mean_y15,group = lake.id))+
  geom_line(aes(group = lake.id))+
  geom_smooth(aes(group = lake.id), method = "lm", se = F, alpha = .2)
  

library(rstanarm)
library(shinystan)

wae_time_to15 %>% 
  group_by(lake.id) %>% 
  summarise(count = n_distinct(year)) %>% 
  filter(count)


# Fit linear mixed-effects model
model <- stan_lmer(mean_y15 ~ year + (1 | lake.id), data = wae_time_to15)

# Print model summary
print(model)

launch_shinystan(model)


# MN time to 15" -------------------------------------------------------------
#whole dataset mean t15
mn_ALK %>%
  filter(species=="walleye") %>%
  filter(alk %in% c("year")) %>%
  filter(est.age <50) %>% 
  filter(year>1993) %>% 
  group_by(year) %>% 
  summarise(mean_age = mean(est.age, na.rm = T),
            med_age = median(est.age, na.rm = T),
            mean_y15 = mean(ifelse(length > 368.3 & length < 393.7, est.age, NA), na.rm = T),
            sd_y15 = sd(ifelse(length > 368.3 & length < 393.7, est.age, NA), na.rm = T),
            median_y15 = median(ifelse(length > 368.3 & length < 393.7, est.age, NA), na.rm = T),
            mean_size = mean(length, na.rm=T),
            sd_length = sd(length, na.rm = T)
  ) %>% 
  collect() %>% 
  {. ->> yearly_wae_time_to15}

# wae_time_to15 %>%
#   group_by(year) %>% 
#   summarise(mean = mean(mean_y15, na.rm = T)) %>% 
#   {. ->> wae_mean_y15_peryear}


ggplot(yearly_wae_time_to15, aes(year, mean_y15))+
  geom_ribbon(aes(ymin = mean_y15-sd_y15,
              ymax = mean_y15+sd_y15), alpha = 0.1)+
  geom_line()+
  geom_smooth(method = "lm", se = FALSE)


# MN lengths -----------------------------------------------------------------

mn_ALK %>%
  filter(species=="walleye") %>%
  # filter(!alk %in% c("species")) %>%
  filter(est.age <50) %>% 
  filter(year>1993) %>% 
  group_by(year) %>% 
  summarise(#mean_age = mean(est.age, na.rm = T),
            #med_age = median(est.age, na.rm = T),
            #mean_y15 = mean(ifelse(length > 368.3 & length < 393.7, est.age, NA), na.rm = T),
            #sd_y15 = sd(ifelse(length > 368.3 & length < 393.7, est.age, NA), na.rm = T),
            #median_y15 = median(ifelse(length > 368.3 & length < 393.7, est.age, NA), na.rm = T),
            mean_size = mean(length, na.rm=T),
            sd_length = sd(length, na.rm = T)
  ) %>% 
  collect() %>% 
  {. ->> yearly_wae_lengths}

ggplot(yearly_wae_lengths, aes(year, mean_size))+
  geom_ribbon(aes(ymin = mean_size-sd_length,
                  ymax = mean_size+sd_length), alpha = 0.1)+
  geom_line()+
  geom_smooth(method = "lm")


# geom_histogram()# now do all ages for all species by year and lake. lets use median to avoid outlier effects. filter by minimum sample size

mn_ALK %>% 
  filter(est.age < 15&est.age>0&alk=="year") %>% 
  group_by(species, est.age, nhdhr.id, year) %>%
  summarise(n = n(), median.length = median(length)) %>% filter(n>4) %>% 
  collect() %>% 
  arrange(species, est.age, year) %>% {.->> mean_LAA_by_lake_mn}

write.csv(mean_LAA_by_lake_mn,"mn_mean_length.csv")







# wisconsin old code ------------------------------------------------------

#open the connection to WI data 
wi_ALK <- open_dataset(sources = "Parquet files/Age-assigned Data/wi_halk_aged_data") #note that you point this fn at the parent folder of the parquet



wi_ALK %>% glimpse()
wi_ALK %>% group_by(length.unit) %>% count() %>% collect()


wi_ALK %>% 
  group_by(length.unit) %>%
  summarise(n = n()) %>% 
  collect()
#restrict to lakeid level
wi_ALK %>% 
  filter(alk=="year") %>% 
  group_by(species, est.age) %>%
  summarise(n = n(), mean = mean(length), median=median(length)) %>% 
  arrange(species, est.age) %>% collect() 





# now do all ages for all species by year and lake. lets use median to avoid outliers. filter by minimum sample size

wi_ALK %>% 
  filter(est.age < 15&est.age>0 &alk=="year"&length.unit=="in") %>% mutate(length=length*25.4) %>%   group_by(species, est.age, nhdhr.id, year) %>%
  summarise(n = n(), median.length = median(length)) %>% filter(n>4) %>% 
  collect() %>% 
  arrange(species, est.age, year) %>% {.->> mean_LAA_by_lake_wi}




# MI old code -------------------------------------------------------------

#open the connection to MI data 
mi_ALK <- open_dataset(sources = "Parquet files/Age-assigned Data/mi_halk_aged_data") #note that you point this fn at the parent folder of the parquet



mi_ALK %>% glimpse()
mi_ALK %>% group_by(length.unit) %>% count() %>% summarise(mean=max(length))%>% collect()


mi_ALK %>% 
  group_by(alk) %>%
  summarise(n = n()) %>% 
  collect()
#restrict to lakeid level
mi_ALK %>% 
  filter(alk=="year") %>% 
  group_by(species, est.age) %>%
  summarise(n = n(), mean = mean(length), median=median(length)) %>% 
  arrange(species, est.age) %>% glimpse() 





# now do all ages for all species by year and lake. lets use median to avoid outliers. filter by minimum sample size
#inch group is not great here

mi_ALK %>% 
  filter(est.age < 15&est.age>0 &alk=="year"&length.unit=="inch_group") %>% mutate(length=length*25.4) %>%   group_by(species, est.age, nhdhr.id, year) %>%
  summarise(n = n(), median.length = median(length)) %>% filter(n>4) %>% 
  collect() %>% 
  arrange(species, est.age, year) %>% {.->> mean_LAA_by_lake_mi}


# merge across states  ----------------------------------------------------



mean_LAA_by_lake=rbind(mean_LAA_by_lake_mn, mean_LAA_by_lake_wi, mean_LAA_by_lake_mi)


#read in temperature data 
current.temps=read_feather("D:/My Drive/Lake temperature modeling/temperature data/lake_temperature_metrics_GLM_NLDAS.feather")
#head(current.temps)

mn.temps=current.temps%>% select(site_id, year, gdd_wtr_5c, mean_surf_JulAugSep )
#calculate sum of GDD at 2 year, 3 year, 4 year, 5 year, 6 year
#rolling mean function that returns the measured value when we have less than n years of data. 
#function to calculate rolled sum, returns a column vector
roll<-function(x,lags){
  if (length(x)<lags) {
    tmp=c(rep(NA,length(x)))  
  }
  else {
    tmp=rollsum(x, lags, align = "right", fill = NA)
  }
  tmp=as.numeric(tmp)
  return(tmp)
}

gdd_sums <- mn.temps %>% 
  group_by(site_id) %>%
  mutate(gdd2 = ave(gdd_wtr_5c, site_id, FUN = function(x) roll(x, 2)), gdd3 = ave(gdd_wtr_5c, site_id, FUN = function(x) roll(x, 3)), gdd4 = ave(gdd_wtr_5c, site_id, FUN = function(x) roll(x, 4)),  gdd5 = ave(gdd_wtr_5c, site_id, FUN = function(x) roll(x, 5)), gdd6 = ave(gdd_wtr_5c, site_id, FUN = function(x) roll(x, 6)))%>%   ungroup 



growth.lakes=merge(mean_LAA_by_lake, gdd_sums, by.y=c("site_id", "year"), by.x=c("nhdhr.id", "year"))


bad.length=growth.lakes %>% filter(median.length<5) 


windows()
ggplot()+stat_smooth(data=subset(growth.lakes, est.age==2), aes(gdd2, median.length), colour="black")+stat_smooth(data=subset(growth.lakes, est.age==3), aes(gdd3, median.length), colour="blue")+stat_smooth(data=subset(growth.lakes, est.age==4), aes(gdd4, median.length), colour="red")+stat_smooth(data=subset(growth.lakes, est.age==5), aes(gdd5, median.length), colour="turquoise")+stat_smooth(data=subset(growth.lakes, est.age==6), aes(gdd6, median.length), colour="purple")+facet_wrap(~species, scales="free_y")+facet_wrap(~species, scales="free_y")

ggsave("mean_size_age_dd.png", units="in", height=8, width=10)


#actual model - include lake id and structure?



#median length over time


growth.summary=  growth.lakes %>% group_by(year, est.age, species) %>% summarise (count=n())

windows()
ggplot(growth.lakes, aes(year, median.length, group=factor(est.age), colour=factor(est.age)))+geom_point(size=.5, alpha=.2)+facet_wrap(~species, scale="free_y")+scale_colour_manual(values=unname(glasbey()))+stat_smooth(method="lm", se=F)+theme_classic()

ggsave("mean_size_age_overtime.png", units="in", height=8, width=10)






# all states t15in --------------------------------------------------------

mn_ALK %>%
  filter(species=="walleye") %>%
  filter(!alk %in% c("species")) %>%
  filter(est.age <50) %>% 
  filter(year>1993& year<2025) %>% 
  group_by(state, lake.id, lake.name, nhdhr.id, year) %>% 
  summarise(n = n(),
            mean_age = mean(est.age, na.rm = T),
            med_age = median(est.age, na.rm = T),
            mean_y15 = mean(ifelse(length > 368.3 & length < 393.7, est.age, NA), na.rm = T),
            median_y15 = median(ifelse(length > 368.3 & length < 393.7, est.age, NA), na.rm = T),
            mean_size = mean(length, na.rm=T),
            sd_length = sd(length, na.rm = T)
            
  ) %>% 
  collect() %>% 
  {. ->> mn_wae_time_to15}

sd_ALK %>%
  filter(species=="walleye") %>%
  filter(!alk %in% c("species")) %>%
  filter(est.age <50) %>% 
  filter(year>1993& year<2025) %>% 
  group_by(state, lake.id, lake.name , year) %>% 
  summarise(n = n(),
            mean_age = mean(est.age, na.rm = T),
            med_age = median(est.age, na.rm = T),
            mean_y15 = mean(ifelse(length > 368.3 & length < 393.7, est.age, NA), na.rm = T),
            median_y15 = median(ifelse(length > 368.3 & length < 393.7, est.age, NA), na.rm = T),
            mean_size = mean(length, na.rm=T),
            sd_length = sd(length, na.rm = T)
            
  ) %>% 
  collect() %>% 
  {. ->> sd_wae_time_to15}

ia_ALK %>%
  filter(species=="walleye") %>%
  filter(!alk %in% c("species")) %>%
  filter(est.age <50) %>% 
  filter(year>1993& year<2025) %>% 
  group_by(state, lake.id, lake.name , year) %>% 
  mutate(length = length * 25.4) %>% 
  summarise(n = n(),
            mean_age = mean(est.age, na.rm = T),
            med_age = median(est.age, na.rm = T),
            mean_y15 = mean(ifelse(length > 368.3 & length < 393.7, est.age, NA), na.rm = T),
            median_y15 = median(ifelse(length > 368.3 & length < 393.7, est.age, NA), na.rm = T),
            mean_size = mean(length, na.rm=T),
            sd_length = sd(length, na.rm = T)
            
  ) %>% 
  collect() %>% 
  {. ->> ia_wae_time_to15}


wi_ALK %>%
  filter(species=="walleye") %>%
  filter(!alk %in% c("species")) %>%
  filter(est.age <50) %>% 
  filter(year>1993& year<2025) %>% 
  group_by(state, lake.id, lake.name, nhdhr.id, year) %>% 
  mutate(length = length * 25.4) %>% 
  summarise(n = n(),
            mean_age = mean(est.age, na.rm = T),
            med_age = median(est.age, na.rm = T),
            mean_y15 = mean(ifelse(length > 368.3 & length < 393.7, est.age, NA), na.rm = T),
            median_y15 = median(ifelse(length > 368.3 & length < 393.7, est.age, NA), na.rm = T),
            mean_size = mean(length, na.rm=T),
            sd_length = sd(length, na.rm = T)
            
  ) %>% 
  collect() %>% 
  {. ->> wi_wae_time_to15}

il_ALK %>%
  filter(species=="walleye") %>%
  filter(!alk %in% c("species")) %>%
  filter(est.age <50) %>% 
  filter(year>1993& year<2025) %>% 
  group_by(state, lake.id, lake.name, year) %>% 
  # mutate(length = length * 25.4) %>% 
  summarise(n = n(),
            mean_age = mean(est.age, na.rm = T),
            med_age = median(est.age, na.rm = T),
            mean_y15 = mean(ifelse(length > 368.3 & length < 393.7, est.age, NA), na.rm = T),
            median_y15 = median(ifelse(length > 368.3 & length < 393.7, est.age, NA), na.rm = T),
            mean_size = mean(length, na.rm=T),
            sd_length = sd(length, na.rm = T)
            
  ) %>% 
  collect() %>% 
  {. ->> il_wae_time_to15}

mi_ALK %>%
  filter(species=="walleye") %>%
  # filter(!alk %in% c("species")) %>%
  filter(est.age <50) %>% 
  filter(year>1993& year<2025) %>% 
  group_by(lake.id, lake.name, year) %>% 
  mutate(length = length * 25.4) %>% 
  summarise(n = n(),
            mean_age = mean(est.age, na.rm = T),
            med_age = median(est.age, na.rm = T),
            mean_y15 = mean(ifelse(length > 368.3 & length < 393.7, est.age, NA), na.rm = T),
            median_y15 = median(ifelse(length > 368.3 & length < 393.7, est.age, NA), na.rm = T),
            mean_size = mean(length, na.rm=T),
            sd_length = sd(length, na.rm = T)
            
  ) %>% 
  collect() %>% 
  {. ->> mi_wae_time_to15}

in_ALK %>%
  filter(species=="walleye") %>%
  # filter(!alk %in% c("species")) %>%
  filter(est.age <50) %>% 
  filter(year>1993 & year<2025) %>% 
  group_by(lake.id, lake.name, year) %>% 
  # mutate(length = length * 25.4) %>% 
  summarise(n = n(),
            mean_age = mean(est.age, na.rm = T),
            med_age = median(est.age, na.rm = T),
            mean_y15 = mean(ifelse(length > 368.3 & length < 393.7, est.age, NA), na.rm = T),
            median_y15 = median(ifelse(length > 368.3 & length < 393.7, est.age, NA), na.rm = T),
            mean_size = mean(length, na.rm=T),
            sd_length = sd(length, na.rm = T)
            
  ) %>% 
  collect() %>% 
  {. ->> in_wae_time_to15}


wae_t15_all <- rbindlist(list(ia_wae_time_to15,
               il_wae_time_to15,
               in_wae_time_to15,
               mi_wae_time_to15,
               mn_wae_time_to15,
               sd_wae_time_to15,
               wi_wae_time_to15), fill = TRUE, use.names = TRUE)





ggplot(wae_t15_all, aes(mean_y15, group = year))+
  # geom_density(aes(color = year), bw = 2, size = .75, alpha =  )
  geom_line(aes(color=year), stat="density", bw = .5, linewidth=1, alpha=0.5)+ 
  scale_x_continuous(breaks=c(1:10))

ggplot(wae_time_to15, aes(year, mean_y15,group = lake.id))+
  geom_line(aes(group = lake.id))+
  geom_smooth(aes(group = lake.id), method = "lm", se = F, alpha = .2)


a <- wae_t15_all[ , .("mean"=mean(mean_y15, na.rm = T), "sd_y15"= sd(mean_y15, na.rm = T))  , year]
b <- wae_t15_all[ , .("mean"=mean(mean_y15, na.rm = T), "sd_y15"= sd(mean_y15, na.rm = T))  , year,state]


ggplot(a, aes(year, mean))+
  geom_ribbon(aes(ymin = mean-sd_y15,
                  ymax = mean+sd_y15), alpha = 0.1)+
  geom_line()+
  geom_smooth(method = "lm", se = FALSE)+
  ylab("mean_t15in")


wae_t15_all %>% 
  group_by(year) %>% 
  summarise(n = n(), n_lakes = n_distinct(lake.id) ) %>% 
  collect() %>% print(n = 100)

wae_t15_all %>% 
  group_by(lake.id,state) %>% 
  summarise(n = n(), n_lakes = n_distinct(year) ) %>% 
  collect() %>% print(n = 100)

mn_ALK %>% 
  group_by(alk, species) %>%
  summarise(n = n(), n_surveys = n_distinct(total.effort.ident) ) %>% 
  collect() %>% print(n = 100)






# get LW the mean size at age 3 for 4 spp ---------------------------------

ia_ALK %>% 
  filter(species %in% c("walleye", "largemouth_bass", "bluegill", "black_crappie" ) & !is.na(est.age))  %>%
  select(species, state, est.age, age, length, length.unit, length.bin, length.bin.unit) %>% 
  collect() %>% 
  {. ->> ia_LA}
mn_ALK %>% 
  filter(species %in% c("walleye", "largemouth_bass", "bluegill", "black_crappie" ) & !is.na(est.age))  %>%
  select(species, state, est.age, age, length, length.unit, length.bin, length.bin.unit) %>% 
  collect() %>% 
  {. ->> mn_LA}
wi_ALK %>% 
  filter(species %in% c("walleye", "largemouth_bass", "bluegill", "black_crappie" ) & !is.na(est.age))  %>%
  select(species, state, est.age, age, length, length.unit) %>% 
  collect() %>% 
  {. ->> wi_LA}
in_ALK %>% 
  filter(species %in% c("walleye", "largemouth_bass", "bluegill", "black_crappie" ) & !is.na(est.age))  %>%
  select(species, state, est.age, age, length, length.unit) %>% 
  collect() %>% 
  {. ->> in_LA}
il_ALK %>% 
  filter(species %in% c("walleye", "largemouth_bass", "bluegill", "black_crappie" ) & !is.na(est.age))  %>%
  select(species, state, est.age, age, length, length.unit, length.bin, length.bin.unit) %>% 
  collect() %>% 
  {. ->> il_LA}
sd_ALK %>% 
  filter(species %in% c("walleye", "largemouth_bass", "bluegill", "black_crappie" ) & !is.na(est.age))  %>%
  select(species, state, est.age, age, length, length.unit, length.bin, length.bin.unit) %>% 
  collect() %>% 
  {. ->> sd_LA}
mi_ALK %>% 
  filter(species %in% c("walleye", "largemouth_bass", "bluegill", "black_crappie" ) & !is.na(est.age))  %>%
  select(species, state, est.age, length, length.unit) %>% 
  collect() %>% 
  {. ->> mi_LA}
  


mw_ages <- rbindlist(list(ia_LA,
                              il_LA,
                              in_LA,
                              mi_LA,
                              mn_LA,
                              sd_LA,
                              wi_LA), fill = TRUE, use.names = TRUE)

mw_ages[ , .N, length.unit]

mw_ages[ length.unit == "in", ':=' (length = (length*25.4), length.unit = "mm") , ]

mw_ages[ , .N, length.unit]

mw_ages[ , .(stat = names(summary(length)), value = summary(length), units = first(length.unit)),  .(species, age)][age == 3]








