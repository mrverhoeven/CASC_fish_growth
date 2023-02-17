
#'
#'## MRV Notes: 
#'###Missing data:
#' - SD NOP, Sauger/saugeye
#' - MN files: "Aged Fish" and "Aged Fish 1" (not clear to me what these are)
#' - WI Lk Winnebago all species
#' - IN, IL, IA, KS, NE, ON, AR
#' - MI appears complete. 
#'







# packages ####
rm(list=ls(all=TRUE))
`%ni%` <- Negate(`%in%`)
library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)


# data ####
species <- c("BLG", "WAE", "BLC", "LMB", "SMB", "YEP", "NOP")
## MN ####

#import MN aged fish dat
mn.new <- read.csv("E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/Growth_Analysis_Files/ALL_AGED_FISH 2.csv", header = T)
names(mn.new)

# this is *maybe* stripping out erroneous entries?
mn.cor <- mn.new %>% filter(., YOY %ni% "Measured calcified structure", OFFICIAL_AGE %ni% "Measured calcified structure") 

#this set seems to be getting corrected or updated by Shad
unique(mn.new$YOY)
mn.msh <- mn.new %>% 
  filter(., YOY %in% "Measured calcified structure") %>% # where yoy is this <-
  dplyr::select(., -ASSIGNED_AGE_SOURCE_NAME, -CALCIFIED_STRUC_TYPE_NAME) %>%  #drop these 2 cols
  dplyr::rename(OFFICIAL_AGE = MESH, ASSIGNED_AGE_SOURCE_NAME = YOY, CALCIFIED_STRUC_TYPE_NAME = OFFICIAL_AGE) %>% # reassign these three column values from other cols
  mutate(MESH = NA, YOY = NA)

#these are also getting corrected
mn.yoy <- mn.new %>% 
  filter(., OFFICIAL_AGE %in% "Measured calcified structure") %>%
  dplyr::select(., -CALCIFIED_STRUC_TYPE_NAME) %>% 
  dplyr::rename(OFFICIAL_AGE = YOY, CALCIFIED_STRUC_TYPE_NAME = ASSIGNED_AGE_SOURCE_NAME, ASSIGNED_AGE_SOURCE_NAME = OFFICIAL_AGE) %>%
  mutate(YOY = NA)

# heres a single line of code to do many things
mn <- inner_join(bind_rows(mn.cor, mn.msh, mn.yoy) %>% #rebind the three corrected sets
                   dplyr::select(ID_NBR, SRVY_DT, SP, COMMON_NAME, LEN_MM, OFFICIAL_AGE, SURVEY_ID, GEARTP, WT_G),
                 read.csv("E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/Growth_Analysis_Files/mn_lake_list.csv", header = T) %>%
                   mutate(DOW = str_pad(DOW, width = 8, pad = 0)) %>%
                   dplyr::select(DOW, LAKE_CENTER_LAT_DD5, LAKE_CENTER_LONG_DD5),
                 by = c("ID_NBR"="DOW")) %>% 
  filter(OFFICIAL_AGE %in% readr::parse_number(OFFICIAL_AGE)) %>%
  mutate(year = year(as.Date(SRVY_DT, "%m/%d/%Y")),
         age = as.numeric(OFFICIAL_AGE),
         year.b = year - age,
         Date = as.Date(SRVY_DT, "%m/%d/%Y")) %>% 
  filter(age < 100) %>% 
  dplyr::rename(Survey_ID = SURVEY_ID, Lake.ID = ID_NBR, Gear = GEARTP, species = SP, Length.mm = LEN_MM, weight.g = WT_G, lat = LAKE_CENTER_LAT_DD5, long = LAKE_CENTER_LONG_DD5) %>% 
  dplyr::select(Survey_ID, Lake.ID, Gear, Date, species, Length.mm, weight.g, age, year, year.b, lat, long) %>% 
  mutate(Survey_ID = as.character(Survey_ID),
         weight.g = as.numeric(weight.g),
         state = "mn")

#summary of the corrected data
summary(as.factor(mn$age))

#summary of species in data before joined to lake list
mn.new %>%
  dplyr::select(SP, COMMON_NAME) %>%
  distinct() %>%
  arrange()


## MI ####

#does a bunch of MI data munging in one line
mi <- inner_join(read.csv("E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/Growth_Analysis_Files/mi_snt_age_2002_2020_cleaned.csv", header = T), 
                 read.csv("E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/Growth_Analysis_Files/mi_snt_catch_data_mar2021.csv", header = T) %>% 
  dplyr::select(Survey_Number, LONG_DD, LAT_DD, Water_Body_Name, NEW_KEY) %>% distinct(), by = "Survey_Number") %>% 
  mutate(Date = dmy(Collection_Date),
         year = year(Date),
         year.b = year - AGE,
         Length.mm = round(LENGTH_IN*25.4, digit = 1), 
         Gear = NA,
         weight.g = NA) %>% 
  dplyr::rename(Lake.ID = Water_Body_Key, Survey_ID = Survey_Number, species = SP, lat = LAT_DD, long = LONG_DD, age = AGE) %>% 
  dplyr::select(Survey_ID, Lake.ID, Gear, Date, species, Length.mm, weight.g, age, year, year.b, lat, long) %>% 
  mutate(Survey_ID = as.character(Survey_ID),
         state = "mi")

#check alignment for future merges
head(mi)
head(mn)
summary(as.factor(mi$species))


## WI ####
#wi.catch <- read.csv("...//wdnr_inland_fish_data-002.csv", header = T)

wi.len <- read.csv("E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/Growth_Analysis_Files/wi_inland_lenage_19Mar2021.csv", header = T) %>% 
  mutate(Date = as.Date(str_sub(sample.date, 1, - 11)),
         year = year(Date),
         year.b = year - age, 
         Length.mm = round(length*25.4, digits = 1),
         weight.g = round(weight*453.592, digits = 1)) %>%
  dplyr::rename(Lake.ID = wbic, Survey_ID = survey.seq.no, Gear = gear) %>%
  dplyr::select(Survey_ID, Lake.ID, Gear, Date, species, Length.mm, weight.g, age, year, year.b)
wi.loc <- read.csv("E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/Growth_Analysis_Files/wi_lake_wbic_lat_long.csv", header = T) %>% mutate(Lake.ID = as.integer(WBIC)) %>% 
  dplyr::select(-WBIC) %>% 
  dplyr::rename(lat = Latitude, long = Longitude)

wi <- inner_join(wi.len, wi.loc, by = "Lake.ID") %>% 
  mutate(Survey_ID = as.character(Survey_ID),
         Lake.ID = as.character(Lake.ID),
         state = "wi")
summary(as.factor(wi$species))

## SD ####
library(sf)

sd.age <- read.csv("E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/Growth_Analysis_Files/sd_length_age_4Oct2021.csv") %>% 
  dplyr::select(SurveyID, SpeciesName, Length, Age) %>% 
  dplyr::rename(Survey_ID = SurveyID, species = SpeciesName, Length.mm = Length, age = Age) %>% 
  mutate(weight.g = NA)

sd.srvy <- read.csv("E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/Growth_Analysis_Files/sd_effort_4Oct2021.csv") %>% 
  dplyr::select(SurveyDate, StateID, SurveyID, Method) %>% 
  dplyr::rename(Date = SurveyDate, Lake.ID = StateID, Survey_ID = SurveyID, Gear = Method) %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

# something is awry with these shafefiles, they've got no data, just geoms
# sd.site <- read_sf("E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/Growth_Analysis_Files/sd_lake_shapefiles/ManagedFisheries.shp")

sd.site.s <- read.csv("E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data/Growth_Analysis_Files/ManagedFisheries.csv")

colnames(sd.site.s)  <- word(colnames(sd.site.s), start = 1L , sep = fixed("."))


sd.cnt <- sd.site.s %>% 
  dplyr::select(StateID, Latitude, Longitude) %>% 
  dplyr::rename(Lake.ID = StateID, lat = Latitude, long = Longitude) %>% 
  filter(Lake.ID %ni% NA)

sd <- inner_join(inner_join(sd.age, sd.srvy, by = "Survey_ID"),
                 sd.cnt, by = "Lake.ID") %>% 
  mutate(year = year(Date),
         year.b = year - age,
         state = "sd", 
         Length.mm = as.numeric(Length.mm))

summary(as.factor(sd$species))
## data combine ####              
 str(mn) 
 str(mi) 
 str(sd) 
 str(wi) 
 species <- c("BLG", "WAE", "BLC", "LMB", "SMB", "YEP")     
dat <- bind_rows(sd, mn, mi, wi) %>% 
  mutate(species = as.factor(species)) %>% 
  filter(species %in% c("BLG", "bluegill", "Bluegill",# bluegill
                        "WAE", "walleye", "Walleye", ## walleye
                        "BLC", "Black Crappie", "black_crappie", # black Croppies
                        "largemouth bass", "Largemouth Bass", "largemouth_bass", "LMB", # Large MOuth bass
                        "SMB", "Smallmouth Bass", "smallmouth_bass", # small mouth bass
                        "YEP", "Yellow Perch", "yellow_perch", ## yellow perch
                        "NOP", "northern_pike", # NOrthern Pike
                        "CCF", "Channel Catfish"
                        )) %>% 
  mutate(species1 = case_when(species %in% c("BLG", "bluegill", "Bluegill") ~ "bluegill",
                              species %in% c("largemouth bass", "Largemouth Bass", "largemouth_bass", "LMB") ~ "largemouth_bass",
                              species %in% c("WAE", "walleye", "Walleye") ~ "walleye", 
                              species %in% c("YEP", "Yellow Perch", "yellow_perch") ~ "yellow_perch",
                              species %in% c("SMB", "Smallmouth Bass", "smallmouth_bass") ~ "smallmouth_bass",
                              species %in% c("BLC", "Black Crappie", "black_crappie") ~ "black_crappie",
                              species %in% c("NOP", "northern_pike") ~ "northern_pike",
                            species %in% c("CCF", "Channel Catfish") ~ "channel_catfish"))

dat.sites <- dat %>% dplyr::select(Lake.ID, lat, long, species1) %>% distinct()
# saveRDS(object = dat, "data/Length.Age.all.species.rds")

## Figure map ####
MainStates <- map_data("state") #%>% filter(., region %in% c("montana", "minnesota", "michigan", "north dakota", "iowa", "wisconsin", "missouri"))
FocalStates <- map_data("state") %>% filter(., region %in% c( "minnesota", "michigan", "wisconsin", "south dakota"))
ggplot() +
  geom_point(data = dat.sites, aes(x = long, y = lat)) +
  facet_wrap(~species1) +
  # geom_polygon(data=MainStates, aes(x=long, y=lat, group=group),
  #              color="grey40", fill="NA" ) +
  geom_polygon(data=FocalStates, aes(x=long, y=lat, group=group),
               color="blue", fill="NA", size = 1.25) +
  
  theme_classic()
                 
                 


