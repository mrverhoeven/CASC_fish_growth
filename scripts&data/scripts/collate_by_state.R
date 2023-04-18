#' Cleaning and organizing & formatting state datsets to meet growth proj needs:
#' 1. 

library(sp)
library(maps)
library(maptools)


# custom fns --------------------------------------------------------------



# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}

# Test the function using points in Wisconsin and Oregon.
testPoints <- data.frame(x = c(-90, -120), y = c(44, 44))



#now start merging these together
#all data have consistent column names because we forced this through the data explainer 
# we might be able to make a quick key col set by searching all cols for these keywords
# colnames(ar)[str_detect(colnames(ar), pattern =
#              "state|lake_name|location_notes|year|date|species|age|length|weight|sex|aging")]
           
           
cols <- c("state","lake_name", "location_notes.1", #loc dat
          "year","date.1", "date.2", #date dat
          "species.1", "age", "length.1", "length_unit.1", #core fish dat
          "weight.1", "weight_unit.1", "aging_structure", "sex" #extra useful bits
          )



# ar ----------------------------------------------------------------------


# Arkansas
names(ar_reservoir_age_8Oct2021)
names(ar_reservoir_age2_8Oct2021)

ar <- merge.data.table(ar_reservoir_age_8Oct2021, ar_reservoir_age2_8Oct2021,  all = T)



ar[ , summary(.SD) , .SDcols = cols]


# ar_location -------------------------------------------------------------


#loc dat
ar[ , .N , .(state,lake_name, location_notes.1)]
ar[lake_name == "BSL", lake_name := "Bull Shoals Lake"]
ar[lake_name == "NFL", lake_name := "Norfork Lake"]


# ar_date -----------------------------------------------------------------


#date dat
ar[ , .N , .(year, date.1, date.2) ]
ar[ , date_clean :=  as.IDate(paste(year,date.2, date.1, sep = "/"), format = "%Y/%m/%d")  , ]
ar[ , summary(date_clean) , ]

# year
ar[ , .N , year ]
ar[is.na(year) & is.na(date_clean), .N , .(lake_name, species.1) ] #there are a few records here for which we have no dates at all. 


# ar_species ------------------------------------------------------------------


#core fish dat:
#species:
ar[ , .N , species.1]

#renaming table (from metadata in GDrive)
ar_rename <- transpose(keep.names = "oldname", data.table(BLC = "black_crappie",
                                                          LMB = "largemouth_bass",
                                                          SMB = "smallmouth_bass",
                                                          SPB = "spotted_bass",
                                                          WAL = "walleye",
                                                          WHC = "white_crappie",
                                                          BCP = "black_crappie",
                                                          WCP = "white_crappie"))
#execute
ar[ , species.1 := 
      ar_rename[match(ar[ ,species.1],ar_rename[,oldname]) ,
                V1 , ]
    , ]

# ar_age ------------------------------------------------------------------


#age
ar[ , .N , age ]


# ar_length ---------------------------------------------------------------


#length
ar[ , .N , length_unit.1 ]
ar[ , .N , length.1 ]

#rewrite inch lengths to mm, and amends units to reflect this change
ar[ length_unit.1 == "col_name_Length_in" ,  ':=' (length.1 = length.1 * 25.4, length_unit.1 = "col_name_Length..mm." )   ,  ]

ar[length_unit.1 == "col_name_Length..mm." , length_unit.1 := "millimeters" ]
ar[ , mean(length.1) , species.1]


#weight
ar[ , summary(weight.1) ,]
ar[ , .N , weight_unit.1]

ar[ weight_unit.1 == "col_name_Weight..g." , weight_unit.1 := "grams" ,]
ar[ is.na(weight.1), weight_unit.1 := NA ,  ]

ar[ !is.na(weight.1), mean(weight.1) , species.1]

#aging structure
ar[ , .N , aging_structure]
ar[ is.na(aging_structure) , aging_structure := "unspecified" , ]

ar[ , aging_method := aging_structure]

ar[ , aging_structure := NA , ]


#sex
ar[ , .N ,  sex]
ar[sex == "?" , sex := "unk", ]

#bring critical cols to left
setcolorder(ar, c(cols,"date_clean", "aging_method"))

rm(ar_reservoir_age_8Oct2021, ar_reservoir_age2_8Oct2021, ar_rename)



# ar_geoloc ---------------------------------------------------------------


#get lat/long for locs:

ar[ , .N , .(lake_name, location_notes.1) ]

ar[ lake_name == "Norfork Lake" , ':=' (latitude = "36.351351" , longitude = "-92.232243")   , ]
ar[ lake_name == "Bull Shoals Lake" , ':=' (latitude = "36.486572" , longitude = "-92.894508")   , ]
ar[ location_notes.1 == "Location:James River Arm" , ':=' (latitude = "36.718906601766705" , longitude = "-93.59351150562455")   , ]
ar[ location_notes.1 == "Location:Kings River Arm" , ':=' (latitude = "36.55397644090967" , longitude = "-93.50610006173052")   , ]
ar[ location_notes.1 == "Location:Mid White River Arm" , ':=' (latitude = "36.60279620933327" , longitude = "-93.50610006173052")   , ]
ar[ location_notes.1 == "Location:Long Creek Arm" , ':=' (latitude = "36.48231274609963" , longitude = "-93.3020160955791")   , ]
ar[ location_notes.1 == "Location:Upper White River Arm" , ':=' (latitude = "36.52369836589746" , longitude = "-93.72775995312399")   , ]

ar[ , .N , .(lake_name, location_notes.1, latitude, longitude) ]


#IOWA

# ia ----------------------------------------------------------------------


ia <- rbindlist(list(rbindlist(list(ia_CCF_age_length_21Aug2021,
                                    ia_BLG_age_length_21Aug2021),
                               fill = TRUE,
                               use.names = TRUE),
                     ia_age_length_21Aug2021),
                fill = TRUE,
                use.names = TRUE)

names(ia)
cols <- c("state", "county","lake_name", "lake_id", #loc dat
          "year","date.1", #date dat
          "species.1", "age", "length.1", "length_unit.1", #core fish dat
          "weight.1", "weight_unit.1", "aging_structure", "sex", "aging_data_notes.1", "aging_data_notes.2" #extra useful bits
)
setcolorder(ia,c(cols))
 colnames(ia)



#locdat
# is there any data cap in state or county the we need? 
ia[ , .N , .(state,county, lake_name, lake_id)]
all(ia[ , .N , .(state,county, lake_name, lake_id)][,N] == ia[ , .N , .(lake_name, lake_id)][, N]) # all the fish by the larger keyset is captured in lake_name.  

# ia_location -------------------------------------------------------------

str(ia_locs)

#working to resolve ambiguous matches/locs
ia_locs <- ia_locs[!`First SiteName`== ""]

ia_locs[ is.na(Latitude)|is.na(Longitude)]
ia_locs <- ia_locs[ !(is.na(Latitude)|is.na(Longitude))]


ia_locs[ , .N , `First SiteName`]
ia_locs[ , .N , `First SiteName`][N>1]

ia_locs[`First SiteName` %in%  ia_locs[ , .N , `First SiteName`][N>1, `First SiteName`]][order(`First SiteName`)]

#add county to these:
ia_locs$county <- latlong2county(ia_locs[!is.na(Longitude) , .(Longitude,Latitude), ])

any(duplicated(ia_locs)) #nice. now all are unique

ia_locs[ , state := word(county, start = 1, sep = ",")]
ia_locs[ , county := word(county, start = -1, sep = ",")]

ia[ ,.N , county]
ia_locs[ , .N, county]

ia[ , county := tolower(county) , ]
colnames(ia_locs)[colnames(ia_locs)== "First SiteName"] <- "lake_name"


# get the county names out of the lake names:
ia[str_detect(lake_name, "\\("), .N, county]
ia[str_detect(lake_name, "\\("), .N, .(lake_name, county)]
ia[str_detect(lake_name, "\\(") , lake_name := gsub("\\s*\\([^\\)]+\\)", "", lake_name), ]

#misspelled co name:
ia_locs[county == "pottawattamie", county := "pottawatamie"]



#tidy up the lake names
ia_locs[ , lake_name := gsub(" park", "", gsub("lake ", "", gsub(" lake","", gsub(" pond","", tolower(lake_name))))), ]
ia[ , lake_name := gsub(" park", "", gsub("lake ", "", gsub(" lake","", gsub(" pond","", tolower(lake_name))))), ]
ia[lake_name == "ave. of the saints", lake_name := "avenue of the saints" ]
ia_locs[str_detect(lake_name, "osceola"), lake_name := c("east osceola", "west osceola", "iowa")]
ia_locs[str_detect(lake_name, "storm"), lake_name:= "storm"]
ia_locs[str_detect(lake_name, "silver"), lake_name := gsub("\\s*\\([^\\)]+\\)", "", lake_name) ]
ia_locs[str_detect(lake_name, "lake"), lake_name := gsub("\\s*\\([^\\)]+\\)", "", lake_name) ]
ia[ , county := gsub("'", "", county)]
ia[str_detect(lake_name, "beed") ,lake_name := "beeds" ]
ia_locs[str_detect(lake_name, "west swan"), lake_name := "west swan"]
ia_locs[str_detect(lake_name, "twelve mile"), lake_name := "twelve mile" ]
ia_locs[str_detect(lake_name, "white oak"), lake_name := "white oak" ]
ia_locs[str_detect(lake_name, "red rock"), lake_name := "red rock" ]
ia_locs[str_detect(lake_name, "crawford creek"), lake_name := "crawford creek" ]



ia_locs[str_detect(lake_name, "crawford creek"), ]

names(ia_locs)[names(ia_locs)=="state"] <- "state.geoloc"


#try merge
ia <- merge(ia, ia_locs, by = c("lake_name", "county"), all.x = T, suffixes = c(".x", ".y"), no.dups = TRUE)


#percent with sucessful georeferences:
ia[!is.na(Longitude), .N]/ia[,.N]

# the un referenced/ambiguous LAA samples (n by lake)
ia[is.na(Longitude), .(n_LAA_obs=.N) , .(lake_name, county, lake_id)]

names(ia) <- tolower(names(ia))

names(ia)

# ia_date -----------------------------------------------------------------

#date data
ia[, unique(year)]

#backfill year?
ia[is.na(year) & !is.na(date.1)]
#date align with year?
ia[!is.na(date.1) & !year==year(date.1)]

#lost in garbage can? yes there are some month and season data there, 
ia[is.na(date.1), date.2 := word(garbage_bin_notes.1, -1, sep = ":"), ]
ia[is.na(date.1), .N , date.2]
ia[date.2 == "7", date.2 := "July"]
ia[date.2 == "6", date.2 := "June"]
ia[date.2 == "8", date.2 := "August"]
ia[date.2 == "5", date.2 := "May"]
ia[date.2 == "NA", date.2 := NA  ]
ia[is.na(date.1)& is.na(date.2), date.2 := word(garbage_bin_notes.3, -1, sep = ":"), ]
ia[date.2 == "." , date.2 := NA, ]
ia[ ,date.2 := tolower(date.2) , ]

ia[is.na(date.1), .N , date.2]

ia[, date_clean :=  as.IDate(date.1)]

hist(ia[!is.na(date_clean) ,yday(date_clean)])

#now populate some dates where only mo or season provided (set to 15 if mo give, or season date approximated from histogram here^)

ia_datefill <- transpose(keep.names = "oldname", data.table(fall = "2 Oct",
                                                          july = "15 July",
                                                          spring = "20 April",
                                                          summer = "19 July",
                                                          june = "15 June",
                                                          august = "15 August",
                                                          may = "15 May"))
ia[ , date.3 := 
      ia_datefill[match(ia[ ,date.2],ia_datefill[,oldname]) ,
                V1 , ]
    , ]

ia[!is.na(year) , date.4 := paste(date.3, year)]

ia[ , .N, date.4]

ia[str_detect(date.4, "NA"), date.4 := NA]

ia[is.na(date_clean) & !is.na(date.4), date_clean := as.IDate(date.4, format = "%d %B %Y") ]

# check coverage
hist(ia[ ,yday(date_clean)])
ia[!is.na(date_clean), .N , ]/ia[ , .N , ]

rm(ia_datefill)


# ia_species ------------------------------------------------------------------
ia[ , .N, species.1 ]

#catfish file had no species name in it.
ia[str_detect(new_file_name, "CCF"), species.1 := "CCF"]


ia_rename <- transpose(keep.names = "oldname", data.table(BLC = "black_crappie",
                                                          LMB = "largemouth_bass",
                                                          SMB = "smallmouth_bass",
                                                          sMB = "smallmouth_bass",
                                                          WAE = "walleye",
                                                          CCF = "channel_catfish",
                                                          BLC = "black_crappie",
                                                          YEP = "yellow_perch",
                                                          BLG = "bluegill_sunfish",
                                                          Bluegill = "bluegill_sunfish",
                                                          NOP = "northern pike"))
#execute
ia[ , species.1 := 
      ia_rename[match(ia[ ,species.1],ia_rename[,oldname]) ,
                V1 , ]
    , ]

ia[ , .N ,  species.1]
 rm(ia_rename)


# ia_age ------------------------------------------------------------------

ia[ , hist(age) , ] 

ia[ , .N , age]

ia[is.na(age) , , ]
ia[age == -1]

ia <- ia[!is.na(age) & age != -1]

ia[ , mean(age) , species.1]



# ia_length ---------------------------------------------------------------

ia[ , hist(length.1) , ]

ia[ , .N , length_unit.1]

#readme specifies all these remaining length units are inches
ia[is.na(length_unit.1), length_unit.1 := "inches"]

#rewrite inch lengths to mm, and amends units to reflect this change
ia[ length_unit.1 == "inches" ,  ':=' (length.1 = length.1 * 25.4, length_unit.1 = "col_name_Total Length (mm)" )   ,  ]

ia[length_unit.1 == "col_name_Total Length (mm)" , length_unit.1 := "millimeters" ]
ar[ , mean(length.1) , species.1]

ia[species.1 == "walleye" , hist(length.1) , ]

plot(length.1~age, data = ia[species.1=="walleye"])

rm(ia_age_length_21Aug2021, ia_BLG_age_length_21Aug2021, ia_CCF_age_length_21Aug2021, ia_locs)



## ILLINOIS

# il ----------------------------------------------------------------------

il_aged_fish_surveys_28Dec2022[ , .N , age ]
il_catch_age_effort_17Jan22[ , .N , age] # no reason to import these data. 




il <- rbindlist(list(il_aged_fish_surveys_28Dec2022,
                                    il_catch_age_effort_17Jan22),
                               fill = TRUE,
                               use.names = TRUE)


il_aged_fish_surveys_28Dec2022[!is.na(age)&
     !age %in% c("TRUE", "FALSE"), .N, .(state, county, lake_name, lake_id, lat_unspec, lon_unspec) ]

il_catch_age_effort_17Jan22[!is.na(age), .N, .(state, county, lake_name, lake_id, lat_unspec, lon_unspec, age) ]

il[!is.na(age)&!age %in% c("TRUE", "FALSE"), .N, .(state, county, lake_name, lake_id, lat_unspec, lon_unspec) ]

# note bad behavior in the age merge in rbindlist (ages T/F is converted to 1/0 )
il[!is.na(age), .("meanage" = mean(age), "meanlength"= mean(length.1)) , .(state, county, lake_name, lake_id, lat_unspec, lon_unspec) ]


il <- il_aged_fish_surveys_28Dec2022

names(il)

str(il)


cols <- c("state", "county","lake_name", "lake_id", "lat_unspec", "lon_unspec",  #loc dat
          "year","date.1", #date dat
          "species.1", "age", "length.1", #core fish dat
          "weight.1", "weight_unit.1", "aging_structure", "survey_id", "reproductive_condition_notes"  #extra useful bits
)
setcolorder(il,c(cols))


# il_location -------------------------------------------------------------

il[ , .N, .(state, county, lake_name, lake_id, lat_unspec, lon_unspec) ]

il[ ,"latitude" := lat_unspec , ]
il[ ,"longitude" := lon_unspec , ]

il[ , c("lat_unspec", "lon_unspec") := NULL , ]

names(il)

il[is.na(latitude) | is.na(longitude) , ,]


# il_date -----------------------------------------------------------------

il[ , .N , year ]

il[ , str(date.1) , ]

il[ , date.1 := as.IDate(date.1) , ]

il[ , summary(date.1) , ]

il[is.na(date.1), , ]



# il_species --------------------------------------------------------------

il[ , .N , species.1]

il[ , species.1 := gsub( " ", "_"  , tolower(species.1)) ,]

il[species.1 == "bluegill", species.1 := "bluegill_sunfish"]

il[ , .N , species.1]


# il_length ---------------------------------------------------------------

#length units unspecified (not found in Readmes, either), assumed mm based on values
il[ , hist(length.1) ,  ]

il[ , length_unit.1 := "millimeters" , ]

il[ , mean(length.1), species.1 ] 

il <- il[!is.na(length.1)]

rm(il_aged_fish_surveys_28Dec2022, il_catch_age_effort_17Jan22)


# in ----------------------------------------------------------------------

indy <- in_reservoir_age_fish_16Aug2022[!is.na(species.1)]

# indy[in_reservoir_age_effort_16Aug2022 , on = .(survey_id)]

setkey(in_reservoir_age_effort_16Aug2022, survey_id)

in_reservoir_age_effort_16Aug2022[ , .N , survey_id ]


#misnumbered survey ID
indy[survey_id == 373 | survey_id == 273, .N, lake_name]
indy[lake_name == "Cypress Lake", survey_id := 273 ]

in_reservoir_age_effort_16Aug2022[lake_name == "Brookville", survey_id]

# The following survey ID has no match in th join... based on the funky low N in #478 and the fact the 479 is a survey on Ferdinand State forest from anothe IN file... change survey_id to 478 where 479
indy[survey_id %in% c(471, 478, 484, 486, 479) , .N , .(survey_id, state, lake_name, survey_id) ]
indy[survey_id == 479, survey_id := 478 ]



indy <- merge(indy, in_reservoir_age_effort_16Aug2022, by = "survey_id", all.x = T, all.y =F,  suffixes = c(".laa", ".effort"))


any(names(indy) == "year")


cols <- c("state.laa", "state.effort", "lake_name.laa", "lake_name.effort", "survey_id",  "lat_unspec", "lon_unspec", #loc dat
          "date.1", "end_date",  #date dat
          "species.1", "age", "length.1", "length_unit.1", #core fish dat
          "weight.1", "weight_unit.1", "aging_structure" #extra useful bits
)
setcolorder(indy,c(cols))

# in_loc ------------------------------------------------------------------

indy[ , .N , .(survey_id, state.laa, state.effort, lake_name.laa, lake_name.effort, survey_id,  lat_unspec, lon_unspec) ]

indy[ , c("state.effort", "lake_name.effort") := NULL ]

indy[ , .(number_of_aged_fish = .N) , .(survey_id, state.laa, lake_id, lake_name.laa, survey_id,  lat_unspec, lon_unspec, date.1) ]


# in_date -----------------------------------------------------------------

indy[ , date.1 := as.IDate(date.1) , ]


# in_species --------------------------------------------------------------

indy[  ,  unique(tolower(species.1)),  ]

indy[ , species.1 := gsub(" ", "_", tolower(species.1))  ,]

indy[ , .N , species.1 ]

indy[ species.1 == "bluegill", species.1 :=  "bluegill_sunfish" ,  ,  ]


# in_length ---------------------------------------------------------------

indy[ , unique(length_unit.1) , ]

indy[ , summary(as.numeric(length.1)) , ]

indy[ , l_2 := as.numeric(length.1) , ]

indy[ is.na(l_2) ,  , ] #obviously these lengths are botched

indy[ ,':=' (length.1 = as.numeric(length.1) * 25.4, length_unit.1 = "millimeters" ), ]

indy <- indy[!is.na(length.1)]

indy[ , mean(length.1) , species.1 ]

indy[ , .N , species.1 ]

rm(in_reservoir_age_effort_16Aug2022, in_reservoir_age_fish_16Aug2022)



# ks ----------------------------------------------------------------------

ks <- KS_Standard_fish_19Jan2023

names(ks)
cols <- c("state", "lake_id", #loc dat
          "date.1", #date dat
          "species.1", "length.1", "length_unit.1", #core fish dat
          "weight.1", "weight_unit.1" #extra useful bits
)
setcolorder(ks,c(cols))
colnames(ks)

#no ages!

rm(ks, KS_Standard_fish_19Jan2023)


# ne ----------------------------------------------------------------------

ne <- NE_Standard_fish_19Jan2023

names(ne)
cols <- c("state", "lake_id", #loc dat
          "date.1", #date dat
          "species.1", "length.1", "length_unit.1", #core fish dat
          "weight.1", "weight_unit.1" #extra useful bits
)
setcolorder(ne,c(cols))
colnames(ne)


# ne_locs -----------------------------------------------------------------

ne[ , .N , .(lake_id) ]

ne[ , lake_name := word(lake_id, start = 1, end = -3 , sep = fixed('_')) ]

ne[ , .N , .(lake_name, lake_id) ]

#no lat/longs - need better lake keying


# ne_date -----------------------------------------------------------------

ne[ , date.1 := as.IDate(date.1) , ]

ne[ , hist(yday(date.1)) , ]



# ne_species --------------------------------------------------------------

ne[ , .N , species.1 ]
ne[ , species.1 := gsub(" ", "_", tolower(species.1))]


# ne_length ---------------------------------------------------------------

ne[ , hist(length.1) , ]

ne[ , unique(length_unit.1)]

ne[ ,':=' (length.1 = length.1 * 10, length_unit.1 = "millimeters" ), ]

plot(length.1~age, data = ne[species.1=="walleye"])

rm(NE_Standard_fish_19Jan2023)

# on ----------------------------------------------------------------------

on <- ON_Standard_fish_19Jan2023

names(on)
cols <- c("state", "lake_id", #loc dat
          "date.1", #date dat
          "species.1", "length.1", "length_unit.1", #core fish dat
          "weight.1", "weight_unit.1", "sex", "reproductive_condition_notes" #extra useful bits
)
setcolorder(on,c(cols))
colnames(on)


# on_locs -----------------------------------------------------------------

on[ , .N , .(lake_id, state) ]

on[ , lake_name := word(lake_id, start = 1, end = -2 , sep = fixed('_')) ]

on[ , .N , .(lake_name, lake_id, state) ]

#no lat/longs - need better lake keying


# on_date -----------------------------------------------------------------

on[ , date.1 := as.IDate(date.1) , ]

on[ , hist(yday(date.1)) , ]



# on_species --------------------------------------------------------------

on[ , .N , species.1 ]
on[ , species.1 := gsub(" ", "_", tolower(species.1))]


# on_length ---------------------------------------------------------------

on[ , hist(length.1) , ]

on[ , unique(length_unit.1)]

on[ ,':=' (length.1 = length.1 * 10, length_unit.1 = "millimeters" ), ]

plot(length.1~age, data = on[species.1=="walleye"])

rm(ON_Standard_fish_19Jan2023)


# mi ----------------------------------------------------------------------

#joining these looks like a bit of a mess
mi_statustrends_catch_16Mar2021[ ,unique(lake_id) ]
mi_statustrends_lenage_20May2021[ , unique(lake_id)]
mi_statustrends_lenage_20May2021[ is.na(lake_id), .N , secondary_lake_id]


mi_statustrends_lenage_20May2021[, unique(survey_id)]%in%mi_statustrends_catch_16Mar2021[, unique(survey_id)]

#backfill missing dates:
mi_statustrends_catch_16Mar2021[ , .N , .(survey_id)]

mi_statustrends_lenage_20May2021[is.na(date.1), survey_id] %in% mi_statustrends_catch_16Mar2021[ , survey_id , ]

mi_statustrends_catch_16Mar2021[survey_id %in% mi_statustrends_lenage_20May2021[is.na(date.1), survey_id], .N ,  .(survey_id, date.1)]

mi_statustrends_lenage_20May2021[is.na(date.1), .N , .(survey_id)]  

mi_statustrends_lenage_20May2021[is.na(date.1), ]  

match(mi_statustrends_lenage_20May2021[is.na(date.1), survey_id], mi_statustrends_catch_16Mar2021[ , survey_id]) 

mi_statustrends_lenage_20May2021[is.na(date.1), date.1:= mi_statustrends_catch_16Mar2021[
                                   match(mi_statustrends_lenage_20May2021[is.na(date.1), survey_id], mi_statustrends_catch_16Mar2021[ , survey_id]), date.1 , ]]

# now join first on survey_id then on secondary_id or lake_id
mi_statustrends_lenage_20May2021[!survey_id %in% mi_statustrends_catch_16Mar2021[ ,survey_id], .N , .(survey_id, lake_id, secondary_lake_id) ]#welp - -cant use lake_id...or secondary_id

mi_statustrends_lenage_20May2021[!survey_id %in% mi_statustrends_catch_16Mar2021[ ,survey_id], .N , .(survey_id, lake_id, secondary_lake_id) ]

mi_statustrends_catch_16Mar2021[ , .N,.(survey_id, lake_id, lake_name, county, lat_unspec, lon_unspec) ]

mi <- merge(mi_statustrends_lenage_20May2021, 
            mi_statustrends_catch_16Mar2021[ , .N,.(survey_id, lake_id, lake_name, county, lat_unspec, lon_unspec)][ , N := NULL]
            , by = c("survey_id", "lake_id"), all.x = T)

colnames(mi)
cols <- c("state", "county", "lake_name", "lake_id", "secondary_lake_id", "lon_unspec", "lat_unspec",  #loc dat
          "date.1", #date dat
          "species.1", "length.1", "length_unit.1", #core fish dat
          "aging_structure" #extra useful bits
)
setcolorder(mi,c(cols))
colnames(mi)

# mi_locs -----------------------------------------------------------------

mi[ , .N , is.na(lat_unspec) ]

mi[ , summary(lat_unspec) , ]
mi[ , summary(lon_unspec) , ]

names(mi)[names(mi)== "lat_unspec"] <- "latitude"
names(mi)[names(mi)== "lon_unspec"] <- "longitude"

# theres 11k + surveys without any lake data...


# mi_date -----------------------------------------------------------------

mi[ , unique(date.1) , ]

mi[ str_detect(date.1, "SAMPLE"), date.temporary :=  as.IDate(word(date.1, -1, sep = ":"), format = "%m/%d/%Y") , ]
mi[ str_detect(date.1, "SAMPLE", negate = T), date.temporary := as.IDate(date.1, format = "%d-%B-%y")]

mi[ , .N  , is.na(date.temporary) ]

mi[ , date.1 := date.temporary , ]
mi[ , date.temporary := NULL]

mi[ , hist(yday(date.1))]


# mi_species --------------------------------------------------------------

mi[ , .N , species.1 ]

#renaming table (from metadata in GDrive)
mi_rename <- transpose(keep.names = "oldname", data.table(ATS	= "atlantic salmon",
                                                          BCR =	"black crappie",
                                                          BKT = "brook trout",
                                                          BLG =	"Bluegill sunfish",
                                                          BLP	= "see ambiguous code",
                                                          BLR	= "Black Redhorse",
                                                          BNT	= "Brown Trout",
                                                          BRB	= "Brown Bullhead",
                                                          CAR	= "common carp",
                                                          CCF	= "channel catfish",
                                                          CIS	= "Cisco",
                                                          COS	= "Coho Salmon",
                                                          CRA	= "see ambiguous code",
                                                          CWS	= "see ambiguous code",
                                                          FCF	= "see ambiguous code",
                                                          GOS	= "Golden Shiner",
                                                          GRP	= "Grass Pickerel",
                                                          GSF	= "see ambiguous code",
                                                          HSF	= "see ambiguous code",
                                                          LAT	= "Lake Trout",
                                                          LMB	= "Largemouth Bass",
                                                          LNG	= "Longnose Gar",
                                                          LSF	= "longear sunfish",
                                                          LWF	= "Lake Whitefish",
                                                          MUS = "Muskellunge",
                                                          NOP	= "Northern Pike",
                                                          PSF	= "see ambiguous code",
                                                          RBT	= "Rainbow Trout",
                                                          RKB	= "Rock Bass",
                                                          RSF	= "see ambiguous code",
                                                          SMB	= "Smallmouth Bass",
                                                          SMT	= "see ambiguous code",
                                                          SPL	= "see ambiguous code",
                                                          STN	= "Sturgeon",
                                                          TMU	= "see ambiguous code",
                                                          WAE	= "Walleye",
                                                          WCR	= "White Crappie",
                                                          WHB	= "White Bass",
                                                          WHP	= "White Perch",
                                                          YEP	= "Yellow Perch"
                                                          ))
#execute
mi[ , species_code := species.1]
mi[ , species.1 := 
      gsub(" ", "_", tolower(mi_rename[match(mi[ ,species.1],mi_rename[,oldname]) ,
                V1 , ]))
    , ]

mi[ , .N , .(species.1, species_code)][order(-N)]


# mi_length ---------------------------------------------------------------

mi[ , summary(length.1) , ]
mi[ , .N , length_unit.1]

mi[ ,':=' (length.1 = length.1 * 25.4, length_unit.1 = "millimeters" ), ]

plot(length.1~age, data = mi[species.1=="walleye"])

mi[ , mean(length.1) , species.1 ]

rm(mi_rename, mi_statustrends_catch_16Mar2021, mi_statustrends_lenage_20May2021)

# mn ----------------------------------------------------------------------

mn <- rbindlist(list(rbindlist(list(mn_aged_fish_1_29Sep2021,
                                    mn_aged_fish_2_29Sep2021),
                               fill = TRUE,
                               use.names = TRUE),
                     mn_aged_fish_29Sep2021),
                fill = TRUE,
                use.names = TRUE)


colnames(mn)
cols <- c("state", "county", "lake_name", "lake_id", "survey_id", "sample_id.1", "site_id.1" ,  #loc dat
          "date.1", #date dat
          "species.1", "age", "length.1", "length_unit.1", "species.2", "length.2", "length_unit.2" ,  #core fish dat
          "aging_structure", "sex", "weight.1", "weight_unit.1", "weight.2", "weight_unit.2", "aging_data_notes.1", "aging_data_notes.2", "aging_data_notes.3", "aging_data_notes.4", "aging_data_notes.5", "young_of_year"  #extra useful bits
)
setcolorder(mn,c(cols))
colnames(mn)



