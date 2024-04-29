#'---
#' title: "First pass re-display of fish data"
#' author: "Mike Verhoeven"
#' output: 
#'    html_document:
#'       toc: true
#'       theme: default
#'       toc_depth: 3
#'       toc_float:
#'           collapsed: false
#'---

#' 
#' This script will read in the data from Big Midwestern Fish Dataset
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


# get LW the mean size at age 3 for 4 spp ---------------------------------
#' ## Bring subset of data into R env
#' Here you'll see we use Arrow to grab a subset relevant to our questions from
#' state datasets. Then we'll bind them together to make a single dataset.


#one state at a time:
ia_ALK %>% 
  filter(species %in% c("walleye", "largemouth_bass", "bluegill", "black_crappie" ) & # only bring in the species of interest
           !is.na(est.age))  %>% # skip any fish with no estimated age
  select(species, state, est.age, age, length, length.unit, length.bin, length.bin.unit) %>% # only relevant cols
  collect() %>% #bring in to env
  {. ->> ia_LA} #save as object
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

# bind into a single data.table object
mw_ages <- rbindlist(list(ia_LA,
                          il_LA,
                          in_LA,
                          mi_LA,
                          mn_LA,
                          sd_LA,
                          wi_LA), fill = TRUE, use.names = TRUE)


#' ## Summary stats
#' You will notice that we use the est.age column. This is because the metric
#' avoids the bias created in length~age relationship from sub-sampling for age
#' based on length 

mw_ages <- mw_ages[est.age<30 , ,]

mw_ages[ , .N, length.unit] #check units all same scale

mw_ages[ length.unit == "in", ':=' (length = (length*25.4), length.unit = "mm") , ] #convert inches to mm

mw_ages[ , .N, length.unit] #check work

mw_ages[ , .(stat = names(summary(length)), #list the names of the summary call object
             value = summary(length), #list the values of the summary call object
             units = first(length.unit)), #add a column that shows the length units of the first obs
         .(species,est.age)] # and execute the three of these by a species X est.age grouping

mw_ages[ , .(stat = names(summary(length)),
             value = summary(length),
             units = first(length.unit)),
         .(species, est.age)][est.age == 4] #same as above, but trim the product to only age 3


# fwrite(
#   mw_ages[ , .(stat = names(summary(length)), #list the names of the summary call object
#                value = summary(length), #list the values of the summary call object
#                units = first(length.unit)), #add a column that shows the length units of the first obs
#            .(species,est.age)],
#   file = "scripts&data/data/output/species_size_at_age_summaries.csv"
#   
# )
