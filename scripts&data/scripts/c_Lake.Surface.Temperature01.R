# Packages ####
rm(list=ls(all=TRUE))
`%ni%` <- Negate(`%in%`)
library(tidyverse)
library(ncdf4)
library(lubridate)
library(data.table)

# data ####
## lakes ####
lakes1 <- readRDS("data/StudyAreaLake.rds") %>% dplyr::select(Lake.ID, state, usgs.id, x_UTM.state, y_UTM.state, group_id) %>%
  filter(group_id == 1)

dat1 <- readRDS("data/Length.Age.all.species.rds") %>% filter(Length.mm %ni% NA) %>% 
  mutate(Lake.catch = paste(Lake.ID, year.b, sep = "_")) %>% 
  arrange(Lake.catch)

sample.select <- dat1 %>% group_by(Lake.catch, age, species) %>% summarise(count = n()) %>% group_by(Lake.catch)%>% 
  summarise(age.n = n(), sample.n = sum(count)) %>% data.frame()# %>% 
  # filter(age.n > 5, sample.n >= 35)

dat <- dat1 %>% filter(Lake.catch %in% sample.select$Lake.catch) %>% 
  dplyr::select(Lake.ID, year) %>% distinct()

lakes <- lakes1 %>% filter(., Lake.ID %in% dat$Lake.ID)

## Temperature ####
dat <- nc_open("/Volumes/MNPostDoc/LakeTemperatureData/LakeTemperature/Data/01_predicted_temp_N24-53_W98-126.nc")

# Lakes rows
l.id <- ncvar_get(dat,"site_id")
i <- 1
row.id <- which(l.id %in% lakes[,"usgs.id"])
dim(l.id)

# date 
time <- ncvar_get(dat, "time")
time.units <- ncatt_get(dat, "time", "units")
## t is used in the loop
t <- as.Date(time, origin = as.Date(substr(time.units$value, 11,21)))

# temperature
ST <- ncvar_get(dat, "surftemp")
dim(ST)


# Temp loop ####

data.final <- matrix(nrow = 0, ncol = 6) %>% data.table()
names(data.final) <- c("year","site.id", "mean.3yr.temp", "mean.3yr.temp.ggd", "GGD.mean", "GGD.sum")
i <- 1
j <- 1 #length(row.id)
for(i in 1:length(row.id)) {
dat.t <- data.frame(Site.id = l.id[row.id[i]],
                  date = t,
                  temp = ST[,row.id[i]]) %>% 
  mutate(year = year(date),
         julian.d =  yday(date),
         ggd.5c = case_when(temp < 5 ~ 0,
                            temp >= 5 ~ 1))
dat.append <- data.frame(year = 1980:2020,
                         site.id = first(dat.t$Site.id),
                         mean.3yr.temp = NA,
                         mean.3yr.temp.ggd = NA,
                         GGD.mean = NA,
                         GGD.sum = NA)

for(j in 1:(length(dat.append[,1])-3)){
  
  dat.append[j, "mean.3yr.temp"] <- mean(data.frame(dat.t %>% 
                                                      filter(., julian.d <= 274 & julian.d >= 152) %>% 
                                                      # filter(., ggd.5c == 1) %>% 
                                                      filter(., year >= dat.append[j,"year"] & year <= dat.append[j,"year"]+2))[,"temp"])
  
  dat.append[j, "mean.3yr.temp.ggd"] <- mean(data.frame(dat.t %>% 
                                                          #filter(., julian.d <= 274 & julian.d >= 152) %>% 
                                                          filter(., ggd.5c == 1) %>% 
                                                          filter(., year >= dat.append[j,"year"] & year <= dat.append[j,"year"]+2))[,"temp"])
  
  
  dat.append[j, "GGD.mean"] <- mean(data.frame(dat.t %>% 
                                                 filter(., year >= dat.append[j,"year"] & year <= dat.append[j,"year"]+2) %>%
                                                 group_by(year) %>% summarise(ggd.y = sum(ggd.5c)))[, "ggd.y"])
  dat.append[j, "GGD.sum"] <- sum(data.frame(dat.t %>% 
                                               filter(., year >= dat.append[j,"year"] & year <= dat.append[j,"year"]+2) %>%
                                               group_by(year) %>% summarise(ggd.y = sum(ggd.5c)))[, "ggd.y"])
  
  
}

data.final <- rbindlist(list(data.final, data.table(dat.append)))

}

saveRDS(data.final, "data/Lake.Surface.Temperature01.rds")


