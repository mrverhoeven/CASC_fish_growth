#'---
#' title: "Dataset prep and export for MW CASC Repo"
#' author: "Mike Verhoeven"
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---

#' Goal: 
#'  
#' This script will:
#' 1. read in estimated ages data from Big Midwestern Fish Dataset,
#' 2. link it to lake thermal data,
#' 3. conduct some basic QC to manage quality of data submitted into the repo
#' 
#' In order to do this, we'll 
#' 1. connect to the BMFD
#' 2. pull in a subset of it that we need (8 species, ALKed only, lake NHD ID, date, est.age, age)
#' 3. grab up the parallel thermal data
#' 4. connect the two such that fish A of age i has gdd for each of it's life years from i to i-i
#' 5. model the length as a fn of gdd of each year of life (l~ gdd1+gdd2+gdd3+gdd4+gdd5)
#' 6. display outputs as length age 1 , length age 2, length age 3, length age 4 on the same plot. 
#' 
#' 
#' To Do List and Modifications: 
#' - We need to know the scope of these aged fish data relative to full MW fish data 
#' - We need to update locs with the MSU crosswalk update
#'   
#' 


#' # Document Preamble
#+ warning = FALSE

# load libraries ------------------------------------------------------------------
# ## Load Libraries


library(arrow)
library(tidyverse)
# library(mnsentinellakes)
library(ggplot2)
library(data.table)
library(mwlaketemps)
library(GGally)
library(VCA)
library(mgcv)
library(gratia)


# load in functions -------------------------------------------------------
#' ### Functions

f_dowle3natozeros = function(DT, x) {
  # or by number (slightly faster than by name) :
  for (j in x)
    set(DT,which(is.na(DT[[j]])),j,"0")
}




# connect to data -------------------------------------------------
#' ## Connect to Data

#here note that we have chose to use the 

#open the connection to MN data 
mn_ALK <- open_dataset(sources = "scripts&data/data/input/age_assigned_data/mn_halk_aged_data/most_common_structures") #note that you point this fn at the parent folder of the parquet
wi_ALK <- open_dataset(sources = "scripts&data/data/input/age_assigned_data/wi_halk_aged_data/most_common_structures") #note that you point this fn at the parent folder of the parquet
mi_ALK <- open_dataset(sources = "scripts&data/data/input/age_assigned_data/mi_halk_aged_data/most_common_structures") #note that you point this fn at the parent folder of the parquet
ia_ALK <- open_dataset(sources = "scripts&data/data/input/age_assigned_data/ia_halk_aged_data/most_common_structures") #note that you point this fn at the parent folder of the parquet
sd_ALK <- open_dataset(sources = "scripts&data/data/input/age_assigned_data/sd_halk_aged_data/most_common_structures") #note that you point this fn at the parent folder of the parquet
il_ALK <- open_dataset(sources = "scripts&data/data/input/age_assigned_data/il_halk_aged_data/most_common_structures") #note that you point this fn at the parent folder of the parquet
in_ALK <- open_dataset(sources = "scripts&data/data/input/age_assigned_data/in_halk_aged_data/most_common_structures") #note that you point this fn at the parent folder of the parquet


# subset data ---------------------------------
#' ## Bring subset of data into R env
#' Here you'll see we use Arrow to grab a subset relevant to our questions from state datasets. Then we'll bind them together to make a single dataset. We do not have to subset species because Paul Frater only ran the HALK on the eight species of interest to us. 
#' 
#' 

# only spp of interest in here:
ia_ALK %>% 
  group_by( species) %>% 
  count() %>% 
  collect()

#' Here we can see that the ages were assigned using the most common structure in every ALK (lake-year) resulting in mixed methods. Here we assume that the biologists have chosen ages based on best practices. 

# in the MN aged fish, what structures were used for each species?
mn_ALK %>% 
  filter(alk == "year") %>% 
  group_by( species, alk.age.str) %>% 
  count() %>% 
  collect()

# you can see that all lengthed fish have an assigned age
mn_ALK %>% 
  group_by(is.na(est.age), is.na(length)) %>% 
  count() %>% 
  collect()

#which columns do we need?
glimpse(ia_ALK)

# 


#one state at a time:
ia_ALK %>% 
  filter(!is.na(est.age))  %>% # skip any fish with no estimated age
  select(state, county, lake.name, lake.id, nhdhr.id,
         date, year,
         species, 
         length, length.unit, length.bin, length.bin.unit, age, flag, 
         obs.id, est.age, alk.age.str, alk, alk.n
         ) %>% # only relevant cols
  collect() %>% #bring in to env
  {. ->> ia_LA} #save as object

mn_ALK %>% 
  filter(!is.na(est.age))  %>% # skip any fish with no estimated age
  select(state, county, lake.name, lake.id, nhdhr.id,
         date, year,
         species, 
         length, length.unit, length.bin, length.bin.unit, age, flag, 
         obs.id, est.age, alk.age.str, alk, alk.n
  ) %>% # only relevant cols
  collect() %>% #bring in to env
  {. ->> mn_LA} #save as object
wi_ALK %>% 
  filter(!is.na(est.age))  %>% # skip any fish with no estimated age
  select(state, county, lake.name, lake.id, nhdhr.id,
         date, year,
         species, 
         length, length.unit, age,  
         est.age, alk.age.str, alk, alk.n
  ) %>% # only relevant cols
  collect() %>% #bring in to env
  {. ->> wi_LA}
in_ALK %>% 
  filter(!is.na(est.age))  %>% # skip any fish with no estimated age
  select(state, county, lake.name, lake.id, nhdhr.id,
         date, year,
         species, 
         length, length.unit, age, flag, 
         obs.id, est.age, alk.age.str, alk, alk.n
  ) %>% # only relevant cols
  collect() %>% #bring in to env
  {. ->> in_LA}
il_ALK %>% 
  filter(!is.na(est.age))  %>% # skip any fish with no estimated age
  select(state, county, lake.name, lake.id,
         date, year,
         species, 
         length, length.unit, length.bin, length.bin.unit, age, flag, 
         obs.id, est.age, alk.age.str, alk, alk.n
  ) %>% # only relevant cols
  collect() %>% #bring in to env
  {. ->> il_LA}
sd_ALK %>% 
  filter(!is.na(est.age))  %>% # skip any fish with no estimated age
  select(state, county, lake.name, lake.id, nhdhr.id,
         date, year,
         species, 
         length, length.unit, length.bin, length.bin.unit, age, flag, 
         obs.id, est.age, alk.age.str, alk, alk.n
  ) %>% # only relevant cols
  collect() %>% #bring in to env
  {. ->> sd_LA}
mi_ALK %>% 
  filter(!is.na(est.age))  %>% # skip any fish with no estimated age
  select(state, county, lake.name, lake.id, nhdhr.id,
         date, year,
         species, 
         length, length.unit,
         est.age, alk.age.str, alk, alk.n
  ) %>% # only relevant cols
  collect() %>% #bring in to env
  {. ->> mi_LA}

rm(ia_ALK,
   il_ALK,
   in_ALK,
   mi_ALK,
   mn_ALK,
   sd_ALK,
   wi_ALK)

# combine data ------------------------------------------------------------



#mi limited ages --only for years in 2003-2010 
mi_LA %>% 
  group_by(year, is.na(length)) %>% 
  count() %>% 
  collect() %>% 
  arrange(year) %>%
  print(n=27)

#fix date format
setDT(mi_LA)
mi_LA[ , date := as.Date(date, format = "%Y-%m-%d")] 
summary(year(mi_LA$date))
summary(mi_LA$year)
mi_LA[ , hist(year) , ]


# bind into a single data.table object
mw_ages <- rbindlist(list(ia_LA,
                          il_LA,
                          in_LA,
                          mi_LA,
                          mn_LA,
                          sd_LA,
                          wi_LA), fill = TRUE, use.names = TRUE)
rm(ia_LA,
   il_LA,
   in_LA,
   mi_LA,
   mn_LA,
   sd_LA,
   wi_LA)





# check and summarise data ------------------------------------------------


#' ## Data checks
#' You will notice that we use the est.age column. This is because the metric
#' avoids the bias created in length~age relationship from sub-sampling for age
#' based on length 
#' 

#bad years:
mw_ages[  , sort(unique(year)) , ]
mw_ages <- mw_ages[year %in% c(1944:2023)]


mw_ages[ , .N, length.unit] #check units all same scale

mw_ages[ length.unit == "in", ':=' (length = (length*25.4), length.unit = "mm") , ] #convert inches to mm

mw_ages[ , .N, length.unit] #check work

# consider trimming to exclude length outliers in each speciesXage class
mw_ages[ , .(stat = names(summary(length)), #list the names of the summary call object
             value = summary(length), #list the values of the summary call object
             units = first(length.unit)), #add a column that shows the length units of the first obs
         .(species,est.age)] # and execute the three of these by a species X est.age grouping

mw_ages[ , .(stat = names(summary(length)),
             value = summary(length),
             units = first(length.unit)),
         .(species, est.age)][est.age == 3] #same as above, but trim the product to only age 3


# some clearly erroneous ages
mw_ages[ ,.N , est.age ][order(est.age)]
mw_ages <- mw_ages[est.age<30]





# bring in temps grab out gdd ------------------------------

#read in temperature data (uses mwlaketemps)
read_lake_temp_metrics() %>%
  select(nhdhr.id, year, gdd.wtr.5c) %>% 
  setDT() %>% 
  {. ->> gdds}


# reshape & join: fish to temps ---------------------------------------------


# reshape fish for a year specific connection
#how are zero years handled?
mw_ages[ , .N , est.age ]

mw_ages[ , obs.id.2 := .I , ]

#get age restructured and add cols fo the reshape
mw_ages <- uncount(mw_ages, weights = est.age, .remove = FALSE, .id = "years_ago") 
  mw_ages[ , years_ago := years_ago-1 , ]
  mw_ages[ , year_gdd := year-years_ago , ]
  mw_ages[ , age_year_gdd := est.age-years_ago ,]

head(mw_ages)

#add gdds to the fish  
mw_ages[gdds , on = .(nhdhr.id = nhdhr.id, year = year) , gdd := gdd.wtr.5c ]


#summarise result
mw_ages[ , .N ,  is.na(gdd)]
mw_ages[ , .N ,  .(state, is.na(gdd))]


rm(gdds)



## Constructing the formula for dcast
keycols <- names(mw_ages)[!(names(mw_ages) %in% c("years_ago", "year_gdd", "age_year_gdd", "gdd"))]
f <- as.formula(paste(paste(keycols, collapse = " + "), "~ age_year_gdd"))

mw_ages <- dcast(mw_ages, formula = f, value.var = "gdd")

names(mw_ages)[names(mw_ages)%in% as.character(c(1:29))]

setnames(mw_ages ,
         old = names(mw_ages)[names(mw_ages)%in% as.character(c(1:29))],
         new = paste("y", names(mw_ages)[names(mw_ages)%in% as.character(c(1:29))], sep = "_") )

names(mw_ages)

mw_ages[ , .N , is.na(y_1) ]

#clear out records with no gdds
mw_ages <- mw_ages[!is.na(y_1)]

#summarize
mw_ages[ , .N , species ]
mw_ages[ , .N , .(state, species) ][order(species)]


#convert NAs to zeros
f_dowle3natozeros(mw_ages, paste("y", c(1:29), sep = "_")  )

head(mw_ages)

#add lifetime gdd:
mw_ages[ , life_gdd := rowSums(.SD) , .SDcols = paste("y", c(1:29), sep = "_")  ]

# summarize range of lifetime gdd vals
mw_ages[ , .(stat = names(summary(life_gdd)),
             value = summary(life_gdd),
             units = "gdd"),
         .(species, est.age)][est.age == 1] #review only for age 1 fish

# summarize range of year 1 gdd vals
mw_ages[ , .(stat = names(summary(y_1)),
             value = summary(y_1),
             units = "gdd"), species]


# outliers: speciesXage&gdd length -------------------------------------------------
#summarize length by sppXage

mw_ages[, .(stat = c(names(summary(length)), "sd"),
            value = c(summary(length), sd(length)),
            units = "gdd"), .(species,est.age) ]

mw_ages[ , sd(length) , .(species,est.age)][order(species, est.age)]


#Age based outliers
#build outlier parameters
#3sds away from median throw out
med_sd_tbl <- mw_ages[, .(stat = c(names(summary(length)), "sd"),
            value = c(summary(length), sd(length)),
            units = "mm"), .(species,est.age) ][stat %in% c("Median","sd")]

med_sd_tbl <- dcast(med_sd_tbl, species + est.age ~ stat, value.var = "value")

med_sd_tbl[ , `:=` ("upper_med" = Median + 3*sd, "lower_med" = Median - 3*sd) , ]
mw_ages[med_sd_tbl, on = .(species, est.age), `:=` ("upper_3sd_len" = upper_med, "lower_3sd_len" = lower_med)  ]

mw_ages[length > upper_3sd_len | length < lower_3sd_len , .N , ]
mw_ages <- mw_ages[!(length > upper_3sd_len | length < lower_3sd_len),]

 
#build outlier parameters
#3sds away from mean throw out
mean_sd_tbl <- mw_ages[, .(stat = c(names(summary(length)), "sd"),
                          value = c(summary(length), sd(length)),
                          units = "mm"), .(species,est.age) ][stat %in% c("Mean", "sd")]

mean_sd_tbl <- dcast(mean_sd_tbl, species + est.age ~ stat, value.var = "value")

mean_sd_tbl[ , `:=` ("upper_mean" = Mean + 3*sd, "lower_mean" = Mean - 3*sd) , ]
mw_ages[mean_sd_tbl, on = .(species, est.age), `:=` ("upper_3sd_len" = upper_mean, "lower_3sd_len" = lower_mean)  ]

mw_ages[length > upper_3sd_len | length < lower_3sd_len , .N , ]
mw_ages <- mw_ages[!(length > upper_3sd_len | length < lower_3sd_len),]





#GDD based
#build outlier parameters
#3sds away from median throw out
med_sd_tbl <- mw_ages[, .(stat = c(names(summary(length)), "sd"),
                          value = c(summary(length), sd(length)),
                          units = "mm"), .(species,gdd_bin = round(life_gdd/1000,0)) ][stat %in% c("Median","sd")]

med_sd_tbl <- dcast(med_sd_tbl, species + gdd_bin ~ stat, value.var = "value")

med_sd_tbl[ , `:=` ("upper_med" = Median + 3*sd, "lower_med" = Median - 3*sd) , ]

mw_ages[ , gdd_bin := round(life_gdd/1000,0)]
mw_ages[med_sd_tbl, on = .(species, gdd_bin), `:=` ("upper_3sd_len" = upper_med, "lower_3sd_len" = lower_med)  ]

mw_ages[length > upper_3sd_len | length < lower_3sd_len , .N , ]
mw_ages <- mw_ages[!(length > upper_3sd_len | length < lower_3sd_len),]

#build outlier parameters
#3sds away from mean throw out
mean_sd_tbl <- mw_ages[, .(stat = c(names(summary(length)), "sd"),
                           value = c(summary(length), sd(length)),
                           units = "mm"), .(species,gdd_bin = round(life_gdd/1000,0)) ][stat %in% c("Mean", "sd")]

mean_sd_tbl <- dcast(mean_sd_tbl, species + gdd_bin ~ stat, value.var = "value")

mean_sd_tbl[ , `:=` ("upper_mean" = Mean + 3*sd, "lower_mean" = Mean - 3*sd) , ]
mw_ages[mean_sd_tbl, on = .(species, gdd_bin), `:=` ("upper_3sd_len" = upper_mean, "lower_3sd_len" = lower_mean)  ]

mw_ages[length > upper_3sd_len | length < lower_3sd_len , .N , ]
mw_ages <- mw_ages[!(length > upper_3sd_len | length < lower_3sd_len),]



# save(mw_ages, file = "fish_gdds_ageALKed.rds")


# exclude all est.age over 15y & drop those cols
mw_ages <- mw_ages[est.age < 16]
set(mw_ages, i = NULL, j = paste("y", c(16:29), sep = "_"), NULL)


# gdd_classes -------------------------------------------------------------

mw_ages[ , mean_ann_gdd := life_gdd/est.age , ]

mw_ages[mean_ann_gdd > 3110, gdd_grp := "m.gdd.3110+"  , ]
mw_ages[mean_ann_gdd <= 3110 & mean_ann_gdd >= 2250, gdd_grp := "m.gdd.2250-3110"  , ]
mw_ages[mean_ann_gdd < 2250, gdd_grp := "m.gdd.2250-"  , ]


# save(mw_ages, file = "fish_gdds_ageALKed.rds")
# load(file = "fish_gdds_ageALKed.rds")
# setDT(mw_ages)

mw_ages[ , c("year", "nhdhr.id") := lapply(.SD, as.factor)  , .SDcols = c("year", "nhdhr.id") ]


# data prep checkpoint ----------------------------------------------------

mw_ages[1:10, , ]



# make species models -----------------------------------------------------

#all use same formula
f <- as.formula(paste("length ~",paste(paste("y", c(1:15), sep = "_"), collapse = " + ")))

cisco_mod <- lm(f, data = mw_ages[species == "cisco"])
perch_mod <- lm(f, data = mw_ages[species == "yellow_perch"])
bluegill_mod <- lm(f, data = mw_ages[species == "bluegill"])
crappie_mod <- lm(f, data = mw_ages[species == "black_crappie"])
walleye_mod <- lm(f, data = mw_ages[species == "walleye"])
northern_mod <- lm(f, data = mw_ages[species == "northern_pike"])
largemouth_mod <- lm(f, data = mw_ages[species == "largemouth_bass"])
smallmouth_mod <- lm(f, data = mw_ages[species == "smallmouth_bass"])


summary(cisco_mod)
summary(perch_mod)
summary(bluegill_mod)
summary(crappie_mod)
summary(walleye_mod)
summary(northern_mod)
summary(largemouth_mod)
summary(smallmouth_mod)

#compile into table?
lm_results <- rbindlist(list(
  as.data.table(summary(crappie_mod)$coefficients, keep.rownames = "coefficient"),
  as.data.table(summary(bluegill_mod)$coefficients, keep.rownames = "coefficient"),
  as.data.table(summary(cisco_mod)$coefficients, keep.rownames = "coefficient"),
  as.data.table(summary(largemouth_mod)$coefficients, keep.rownames = "coefficient"),
  as.data.table(summary(smallmouth_mod)$coefficients, keep.rownames = "coefficient"),
  as.data.table(summary(northern_mod)$coefficients, keep.rownames = "coefficient"),
  as.data.table(summary(perch_mod)$coefficients, keep.rownames = "coefficient"),
  as.data.table(summary(walleye_mod)$coefficients, keep.rownames = "coefficient")))

lm_results[ , species := rep(c("black_crappie", "bluegill", "cisco", "largemouth_bass", "smallmouth_bass", "northern_pike", "yellow_perch", "walleye"), each = 16)]

lm_table <- 
dcast( lm_results, species ~ coefficient, value.var = "Estimate")
 setcolorder(lm_table, neworder = c("(Intercept)", paste("y", c(1:15), sep = "_")))



# now to predict a species length at age, just use those splines?
# age 6 walleye is generated by intercept + meangdd*y_1 + meangdd*y_2 + meangdd*y_3 + meangdd*y_4 + meangdd*y_5 + meangdd*y_6 ?

#wae
mw_ages[ species == "walleye" , summary(life_gdd/est.age) , ]
#average fish
coldT_annualgains <- lm_table[ , lapply(.SD , function(x){x*1500})  , .SDcols = paste("y", c(1:15), sep = "_")  ]
setnames(coldT_annualgains, old = paste("y", c(1:15), sep = "_"), new = paste("ldelta_cold", c(1:15), sep = "_"))

warmT_annualgains <- lm_table[ , lapply(.SD , function(x){x*4000})  , .SDcols = paste("y", c(1:15), sep = "_")  ]
setnames(warmT_annualgains, old = paste("y", c(1:15), sep = "_"), new = paste("ldelta_warm", c(1:15), sep = "_"))


lm_ests <- cbind(cbind(lm_table, warmT_annualgains), coldT_annualgains)        

lm_ests <- melt(lm_ests, id = "species", measure=patterns( "^ldelta_warm_", "^ldelta_cold_"), variable.name = "age", value.name = c("gdd_4000", "gdd_1500"))
 
intercepts <- lm_results[coefficient == "(Intercept)", .(species, "gdd_4000" = Estimate, "gdd_1500" = Estimate,  age = 0) , ]

lm_ests <- rbindlist(list(lm_ests, intercepts), use.names = T)

lm_ests

ggplot(lm_ests, aes(age, gdd_4000) )+
  geom_path()

new_data <- data.table(species = "walleye", 
                       "y_1" = c(rep(4000, 15), rep(1500, 15)),
                       "y_2" = c(rep(0,1), rep(4000, 14), rep(0,1), rep(1500, 14)),
                       "y_3" = c(rep(0,2), rep(4000, 13), rep(0,2), rep(1500, 13)),
                       "y_4" = c(rep(0,3), rep(4000, 12), rep(0,3), rep(1500, 12)),
                       "y_5" = c(rep(0,4), rep(4000, 11), rep(0,4), rep(1500, 11)),
                       "y_6" = c(rep(0,5), rep(4000, 10), rep(0,5), rep(1500, 10)),
                       "y_7" = c(rep(0,6), rep(4000, 9),  rep(0,6), rep(1500, 9)),
                       "y_8" = c(rep(0,7), rep(4000, 8),  rep(0,7), rep(1500, 8)),
                       "y_9" = c(rep(0,8), rep(4000, 7),  rep(0,8), rep(1500, 7)),
                       "y_10" = c(rep(0,9), rep(4000, 6),  rep(0,9), rep(1500, 6)),
                       "y_11" = c(rep(0,10), rep(4000, 5), rep(0,10), rep(1500, 5)),
                       "y_12" = c(rep(0,11), rep(4000, 4), rep(0,11), rep(1500, 4)),
                       "y_13" = c(rep(0,12), rep(4000, 3), rep(0,12), rep(1500, 3)),
                       "y_14" = c(rep(0,13), rep(4000, 2), rep(0,13), rep(1500, 2)),
                       "y_15" = c(rep(0,14), rep(4000, 1),rep(0,14), rep(1500, 1)))
#now write this to have gdd from 1500-4000
# Define the GDD range
gdd_values <- seq(1500,4000, by = 10)

# for each of the gdd vals, I'll need 15 fish

result <- data.table(species = rep("walleye", 15*251),
                     "y_1" = NA,
                     "y_2" = NA,
                     "y_3" = NA,
                     "y_4" = NA,
                     "y_5" = NA,
                     "y_6" = NA,
                     "y_7" = NA,
                     "y_8" = NA,
                     "y_9" = NA,
                     "y_10" = NA,
                     "y_11" = NA,
                     "y_12" = NA,
                     "y_13" = NA,
                     "y_14" = NA,
                     "y_15" = NA)

for(cols in colnames(result)){
  # Create a vector of values for each row
  for(j in 1:nrow(result)){
  #col 1
  rep(gdd_values, each = 15)
  
  #col2
  rep(rep(0,) , rep(gdd_values, each), )
    
    row_values <- c(rep(4000, 4000 - gdd + 1), rep(gdd, gdd - 1499))
  }
  
  
}








lm_preds <- bind_cols(new_data,
                      as.data.frame(predict(walleye_mod, newdata = new_data,
                      se.fit = TRUE)))

lm_preds[ , age := rep(c(1:15),2 ),]

ggplot(lm_preds, aes(age,fit, group = y_1))+
  geom_line(aes(color = y_1))



# new_data <- tidyr::expand(rats, nesting(subject, treatment),
#                           transf_time = unique(transf_time))
# which we then use to predict from the model
# 
# m1_pred <- bind_cols(new_data,
#                      as.data.frame(predict(m1_gam, newdata = new_data,
#                                            se.fit = TRUE)))






# visualizations: length ~ gdd  -----------------------------------------------------------
#here's a classic: thermal age plot
len_gdd_viz <- 
  ggplot(mw_ages[], aes(life_gdd , length))+
  geom_point(aes(color = est.age), alpha = .1)+
  geom_smooth()+
  facet_wrap(~species, scales = "free")

png(file = "thermal_age.png", width = 8.5, height = 11, units = "in", res = 1200)
len_gdd_viz
dev.off()




len_age_viz <- 
  ggplot(mw_ages[], aes(est.age , length))+
  geom_point(aes(color = life_gdd), alpha = .1)+
  geom_smooth()+
  facet_wrap(~species, scales = "free")
png(file = "year_age_length.png", width = 8.5, height = 11, units = "in", res = 1200)
len_age_viz
dev.off()

# here's the venturelli plot match
len_gdd_viz <- 
  ggplot(mw_ages[], aes(life_gdd , length))+
  geom_point( alpha = .001)+
  geom_smooth(aes(color = gdd_grp))+
  facet_wrap(~species, scales = "free")

png(file = "thermal_age_gddgroup.png", width = 8.5, height = 8.5, units = "in", res = 1200)
len_gdd_viz
dev.off()

len_age_viz <- 
  ggplot(mw_ages[], aes(est.age , length))+
  geom_point( alpha = .1)+
  geom_smooth(aes(color = gdd_grp))+
  facet_wrap(~species, scales = "free")
png(file = "year_age_gddgroup.png", width = 8.5, height = 8.5, units = "in", res = 1200)
len_age_viz
dev.off()


gh_plot <- 
ggplot()+
  stat_smooth(data=mw_ages[est.age==1,], aes(life_gdd, length), colour="red")+
  stat_smooth(data=mw_ages[est.age==2,], aes(life_gdd, length), colour="orange")+
  stat_smooth(data=mw_ages[est.age==3,], aes(life_gdd, length), colour="green")+
  stat_smooth(data=mw_ages[est.age==4,], aes(life_gdd, length), colour="blue")+
  stat_smooth(data=mw_ages[est.age==5,], aes(life_gdd, length), colour="purple")+
  stat_smooth(data=mw_ages[est.age==6,], aes(life_gdd, length), colour="red")+
  stat_smooth(data=mw_ages[est.age==7,], aes(life_gdd, length), colour="orange")+
  stat_smooth(data=mw_ages[est.age==8,], aes(life_gdd, length), colour="green")+
  stat_smooth(data=mw_ages[est.age==9,], aes(life_gdd, length), colour="blue")+
  stat_smooth(data=mw_ages[est.age==10,], aes(life_gdd, length), colour="purple")+
  stat_smooth(data=mw_ages[est.age==11,], aes(life_gdd, length), colour="red")+
  stat_smooth(data=mw_ages[est.age==12,], aes(life_gdd, length), colour="orange")+
  stat_smooth(data=mw_ages[est.age==13,], aes(life_gdd, length), colour="green")+
  stat_smooth(data=mw_ages[est.age==14,], aes(life_gdd, length), colour="blue")+
  stat_smooth(data=mw_ages[est.age==15,], aes(life_gdd, length), colour="purple")+
  # stat_smooth(data=mw_ages[est.age==16,], aes(life_gdd, length), colour="red")+
  # stat_smooth(data=mw_ages[est.age==17,], aes(life_gdd, length), colour="orange")+
  # stat_smooth(data=mw_ages[est.age==18,], aes(life_gdd, length), colour="green")+
  # stat_smooth(data=mw_ages[est.age==19,], aes(life_gdd, length), colour="blue")+
  # stat_smooth(data=mw_ages[est.age==20,], aes(life_gdd, length), colour="purple")+
  facet_wrap(~species, scales="free_y")

png(file = "annual_gdd_stack.png", width = 5.5, height = 5, units = "in", res = 1200)
gh_plot
dev.off()

# where is the variance coming from? --------------------------------------



# interpret time to harvestable sizes from figs ---------------------------


# for walleye we see that the 33% and 66% quantiles of gdds show a 2 year difference in their time to harvestable size (4 yr to 6 year). 

# for 



















# plot a picture of the simple model --------------------------------------



# visualization: length~y_1 by age ----------------------------------------


mod_viz <- 
ggplot(mw_ages[], aes(y_1, length))+
  geom_smooth(aes(color = as.factor(est.age)), method = "lm")+
  facet_wrap(~species, scales="free_y")

png(file = "model_viz_y1eff_allspp.png", width = 5.5, height = 5, units = "in", res = 1200)
mod_viz
dev.off()

mod_viz <- 
  ggplot(mw_ages[], aes(y_1, length))+
  geom_smooth( method = "lm")+
  facet_wrap(~species, scales="free")

png(file = "model_viz_y1eff_allspp_agecombi.png", width = 5.5, height = 5, units = "in", res = 1200)
mod_viz
dev.off()




# corellation in gdds -----------------------------------------------------
 # ggpairs(mw_ages[ est.age <11 & species == "walleye",.SD , .SDcols = paste("y", c(1:29), sep = "_") ])

# distribution of observation dates within a year -------------------------

mw_ages[ , hist(yday(date)) ,  ]


# gam fit to lifetime gdd -------------------------------------------------

# follows Gavin Simpson vignette https://fromthebottomoftheheap.net/2021/02/02/random-effects-in-gams/


# Specifyoing interactions:
# https://stats.stackexchange.com/questions/519433/gam-and-multiple-continuous-continuous-interactions-tensor-smooths

# https://r.qcbs.ca/workshop08/book-en/gam-with-interaction-terms.html


# walleye_gam <- gam(length ~ est.age + life_gdd +
#                 s(nhdhr.id, bs = 're') +
#                 s(year, bs = 're'),
#               data = mw_ages[species == "walleye"], method = 'REML')
# 
# walleye_gam <- gam(length ~ est.age + life_gdd +
#                      s(nhdhr.id, bs = 're'),# +
#                      # s(year, bs = 're'),
#                    data = mw_ages[species == "walleye"], method = 'REML')

walleye_gam <- bam(length ~ est.age + life_gdd +
                     s(nhdhr.id, bs = 're'), # +
                   # s(year, bs = 're'),
                   data = mw_ages[species == "walleye"], method = 'REML')
save(walleye_gam, file = "wae_gam.rds")

# walleye_gam <- bam(length ~ est.age:life_gdd + #here consider interaction term(s) for year&gdd
#                      s(nhdhr.id, bs = 're'), # +
#                    # s(year, bs = 're'),
#                    data = mw_ages[species == "walleye"], method = 'REML')

walleye_gam2 <- bam(length ~ est.age:life_gdd +
                     s(nhdhr.id, bs = 're')+
                     s(year, bs = 're'),
                   data = mw_ages[species == "walleye"], method = 'REML', select = T)
save(walleye_gam2, file = "wae_gam2.rds")
load(wae_gam2)
summary(wae_gam2)
variance_comp(walleye_gam2)


walleye_gam3 <- bam(length ~ est.age:life_gdd +
                      s(nhdhr.id, bs = 're')+
                      s(nhdhr.id, year, bs = 're'),
                    data = mw_ages[species == "walleye"], method = 'REML', select = T)
save(walleye_gam3, file = "wae_gam3.rds")
load(walleye_)
summary(walleye_gam2)
variance_comp(walleye_gam2)
