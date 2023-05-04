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
# testPoints <- data.frame(x = c(-90, -120), y = c(44, 44))



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

# make other dates character strings
datecols <- colnames(ar)[str_detect(colnames(ar), "date\\.")]
ar[    , (datecols) := lapply(.SD, as.character)    ,   .SDcols = datecols]


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


colnames(ar)







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

ia[ , date.1 :=  as.IDate(date.1) , ]

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

# make other dates character strings
datecols <- colnames(ia)[str_detect(colnames(ia), "date\\.")]
ia[    , (datecols) := lapply(.SD, as.character)    ,   .SDcols = datecols]


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
                                                          NOP = "northern_pike"))
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



colnames(ia)

ia[, c("state.geoloc") := NULL, ]





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

il[, date_clean :=  date.1]

# make other dates character strings
datecols <- colnames(il)[str_detect(colnames(il), "date\\.")]
il[    , (datecols) := lapply(.SD, as.character)    ,   .SDcols = datecols]


# il_species --------------------------------------------------------------

il[ , .N , species.1]

il[ , species.1 := gsub( " ", "_"  , tolower(species.1)) ,]

il[species.1 == "bluegill", species.1 := "bluegill_sunfish"]

il[ , .N , species.1]


# il_age ------------------------------------------------------------------

il[ , hist(age) , ]

colnames(il)




# il_length ---------------------------------------------------------------

#length units unspecified (not found in Readmes, either), assumed mm based on values
il[ , hist(length.1) ,  ]

il[ , length_unit.1 := "millimeters" , ]

il[ , mean(length.1), species.1 ] 

# il <- il[!is.na(length.1)]

rm(il_aged_fish_surveys_28Dec2022, il_catch_age_effort_17Jan22)


colnames(il)











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

indy[ ,"latitude" := lat_unspec , ]
indy[ ,"longitude" := lon_unspec , ]

indy[ , c("lat_unspec", "lon_unspec") := NULL , ]

indy[ ,"state" := state.laa , ]

indy[ , .N , state ]

indy[ , c("state.laa") := NULL , ]

# in_date -----------------------------------------------------------------

indy[ , date.1 := as.IDate(date.1) , ]

indy[, date_clean :=  date.1]

# make other dates character strings
datecols <- colnames(indy)[str_detect(colnames(indy), "date\\.")]
indy[    , (datecols) := lapply(.SD, as.character)    ,   .SDcols = datecols]


# in_species --------------------------------------------------------------

indy[  ,  unique(tolower(species.1)),  ]

indy[ , species.1 := gsub(" ", "_", tolower(species.1))  ,]

indy[ , .N , species.1 ]

indy[ species.1 == "bluegill", species.1 :=  "bluegill_sunfish" ,  ,  ]


# in_age ------------------------------------------------------------------

indy[ , .N , age]


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

colnames(indy)

colnames(indy) <-  gsub("\\.laa", "", colnames(indy))

indy[ , c("l_2") := NULL ,]

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

ne[, date_clean :=  date.1]

# make other dates character strings
datecols <- colnames(ne)[str_detect(colnames(ne), "date\\.")]
ne[    , (datecols) := lapply(.SD, as.character)    ,   .SDcols = datecols]


# ne_species --------------------------------------------------------------

ne[ , .N , species.1 ]
ne[ , species.1 := gsub(" ", "_", tolower(species.1))]


# ne_age ------------------------------------------------------------------

ne[ ,.N, age]


# ne_length ---------------------------------------------------------------

ne[ , hist(length.1) , ]

ne[ , unique(length_unit.1)]

ne[ ,':=' (length.1 = length.1 * 10, length_unit.1 = "millimeters" ), ]

plot(length.1~age, data = ne[species.1=="walleye"])

rm(NE_Standard_fish_19Jan2023)

colnames(ne)



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

on[, date_clean :=  date.1]

# make other dates character strings
datecols <- colnames(on)[str_detect(colnames(on), "date\\.")]
on[    , (datecols) := lapply(.SD, as.character)    ,   .SDcols = datecols]


# on_species --------------------------------------------------------------

on[ , .N , species.1 ]
on[ , species.1 := gsub(" ", "_", tolower(species.1))]


# on_age ------------------------------------------------------------------

on[ , .N , age]


# on_length ---------------------------------------------------------------

on[ , hist(length.1) , ]

on[ , unique(length_unit.1)]

on[ ,':=' (length.1 = length.1 * 10, length_unit.1 = "millimeters" ), ]

plot(length.1~age, data = on[species.1=="walleye"])

rm(ON_Standard_fish_19Jan2023)

colnames(on)


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

mi[, date_clean :=  date.1]

# make other dates character strings
datecols <- colnames(mi)[str_detect(colnames(mi), "date\\.")]
mi[    , (datecols) := lapply(.SD, as.character)    ,   .SDcols = datecols]


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


# mi_age ------------------------------------------------------------------

mi[ , .N , age]


# mi_length ---------------------------------------------------------------

mi[ , summary(length.1) , ]
mi[ , .N , length_unit.1]

mi[ ,':=' (length.1 = length.1 * 25.4, length_unit.1 = "millimeters" ), ]

plot(length.1~age, data = mi[species.1=="walleye"])

mi[ , mean(length.1) , species.1 ]

rm(mi_rename, mi_statustrends_catch_16Mar2021, mi_statustrends_lenage_20May2021)

colnames(mi)



# mn ----------------------------------------------------------------------

mn <- mn_aged_fish_v2_20apr2023


colnames(mn)
cols <- c("state", "county", "lake_name", "lake_id", "survey_id", "site_id.1" ,  #loc dat
          "date.1", "date.2", #date dat
          "species.1", "age", "length.1", "length_unit.1", "species.2", #core fish dat
          "aging_structure", "sex", "weight.1", "weight_unit.1",  "young_of_year"  #extra useful bits
)
setcolorder(mn,c(cols))
colnames(mn)

mn[ ,.N , age]

# mn_location ------------------------------------------------------------------

mn[, .N , lake_id ][order(lake_id)]

mn[ , DOW := as.integer(lake_id) , ]
mn_locs[, unique(DOW)]

mn <- merge(mn, mn_locs[ ,.(FISHERIES_WATERBODY_ID, LAKE_NAME, ALT_LAKE_NAME, DOW, DOW_NBR_PRIMARY, DOW_SUB_BASIN_NBR_PRIMARY, LAKE_CENTER_LAT_DD5, LAKE_CENTER_LONG_DD5) ,], by = "DOW", all.x = T)

mn[ ,"latitude" := LAKE_CENTER_LAT_DD5 , ]
mn[ ,"longitude" := LAKE_CENTER_LONG_DD5 , ]

mn[ , c("LAKE_CENTER_LAT_DD5", "LAKE_CENTER_LONG_DD5") := NULL , ]

mn[ , .N , .(DOW, latitude, longitude) ]
mn[ , .N , .(DOW_SUB_BASIN_NBR_PRIMARY) ]

mn[ , c("FISHERIES_WATERBODY_ID", "LAKE_NAME", "ALT_LAKE_NAME", "DOW", "DOW_NBR_PRIMARY", "DOW_SUB_BASIN_NBR_PRIMARY") := NULL  ,  ]

names(mn)

mn[ , survey_id := as.character(survey_id)]


# mn_date -----------------------------------------------------------------

mn[ , .N , .(date.1, date.2) ]

mn[ , unique(date.1) ,]

mn[ , date.1 := as.IDate(date.1, format = "%m/%d/%Y") ,]
mn[ , date.2 := as.IDate(date.2, format = "%m/%d/%Y") ,]

#date 1 was called sample_date, and date two was called SRVY_DT. 
mn[ !(date.1 == date.2), summary(date.1 - date.2) ]
mn[ !(date.1 == date.2), hist(date.1 - date.2) ]

# we will use "sample date"
mn[, date_clean :=  as.IDate(date.1)]

mn[ , hist(yday(date_clean))]

# make other dates character strings
datecols <- colnames(mn)[str_detect(colnames(mn), "date\\.")]
mn[    , (datecols) := lapply(.SD, as.character)    ,   .SDcols = datecols]

# mn_species --------------------------------------------------------------

mn[ , .N , .(species.1, species.2)][ order(-species.1)]

mn[ , species.1 := gsub(" ", "_", tolower(species.1)) ]

mn[ species.1 == "bluegill", species.1 := "bluegill_sunfish"]


# mn_age ------------------------------------------------------------------

mn[ , .N , age] #some of those are certainly not right!


# mn_length ---------------------------------------------------------------

mn[ , hist(length.1) , ]
mn[ , unique(length_unit.1) , ]

mn[length_unit.1 == "col_name_LEN_MM", length_unit.1 := "millimeters"]


mn[ ,max(length.1), species.1][ order(species.1)]

rm(mn_aged_fish_v2_20apr2023, mn_locs)

colnames(mn)


# wi ----------------------------------------------------------------------


wi <- wi_inland_lenage_19Mar2021


colnames(wi)
cols <- c("state", "county", "lake_name", "lake_id", "survey_id",  #loc dat
          "date.1", #date dat
          "species.1", "age", "length.1", "length_unit.1", #core fish dat
          "aging_structure", "sex", "weight.1", "weight_unit.1" #extra useful bits
)
setcolorder(wi,c(cols))
colnames(wi)

wi[ ,.N , age]


# wi_locs -----------------------------------------------------------------

wi[ , .N , .(state, county, lake_name, lake_id) ]

colnames(wi_locs)[colnames(wi_locs)=="lake.id"] <- "lake_id"

wi <- merge(wi, wi_locs, by = "lake_id", all.x = T)

names(wi)

wi[ ,"latitude" := lat , ]
wi[ ,"longitude" := long , ]

wi[ , c("lat", "long", "state.y") := NULL , ]

colnames(wi)[colnames(wi)=="state.x"] <- "state"

wi[ , .N , .(state, county, lake_name, lake_id, latitude, longitude) ]

wi[ is.na(latitude), .N , .(county, lake_name, lake_id) ]

#manual locs for some of those:
wi[ is.na(latitude) & lake_id == 261100  , `:=`(latitude = 44.34026959720516, longitude = -89.15008499972733) ,]#chain_o'_lakes
wi[ is.na(latitude) & lake_id == 299000  , `:=`(latitude = 44.686273697662806, longitude = -88.6610130587461) ,]#cloverleaf_chain
wi[ is.na(latitude) & lake_id == 339800  , `:=`(latitude = 44.90509784350446, longitude = -88.55579995050842) ,]#legend_lake  
wi[ is.na(latitude) & lake_id == 455500  , `:=`(latitude = 45.07430035317849, longitude = -88.25180404816608) ,]#ucil_lake  
wi[ is.na(latitude) & lake_id == 755500  , `:=`(latitude = 42.77098798220831, longitude = -88.56316236838366) ,]#lauderdale_lakes
wi[ is.na(latitude) & lake_id == 2267600 , `:=`(latitude = 45.902013035820694, longitude = -90.06703957294965) ,]#pike_lake_chain 



# wi_date -----------------------------------------------------------------

#this was called "sample_date" and there are some major issues in here
wi[ , date.1 := as.IDate(date.1) , ]

#these two dates sets were called "survey_date" (start and end)
wi[ , date.2 := word(garbage_bin_notes.2, -1, sep = ":") ,]
wi[ , date.3 := word(garbage_bin_notes.3, -1, sep = ":") ,]

wi[ , date.2 := as.IDate(date.2)]
wi[ , date.3 := as.IDate(date.3)]

wi[ , summary(date.2 - date.3) ,]

wi[ , sort(unique(year(date.1))) ,]
wi[ , sort(unique(year(date.2))) ,]
wi[ , sort(unique(year(date.3))) ,]

wi[ !(year(date.1) == year(date.2)) , sort(unique(year(date.1)))]

wi[ !(year(date.1) == year(date.2)) , .(date.1, date.2)]

#for about 8% of the data the date.1 seems super wonky
wi[ abs(date.1 - date.2) > 30, .(date.1, date.2)]

#we're going to use the survey dates
wi[ , date_clean := date.2 , ]

wi[ , hist(yday(date_clean)) , ]

# make other dates character strings
datecols <- colnames(wi)[str_detect(colnames(wi), "date\\.")]
wi[    , (datecols) := lapply(.SD, as.character)    ,   .SDcols = datecols]


# wi_species --------------------------------------------------------------

wi[ , .N , species.1 ]

wi[species.1 == "bluegill", species.1 := "bluegill_sunfish"  , ]

wi[species.1 == "northern_pike_x_muskellunge", species.1 := "tiger_muskellunge"  , ]



# wi_age ------------------------------------------------------------------

wi[ , .N , age ]


# wi_length ---------------------------------------------------------------

wi[ , summary(length.1)]

wi[ , .N ,  length_unit.1]

wi[ ,':=' (length.1 = as.numeric(length.1) * 25.4, length_unit.1 = "millimeters" ), ]

plot(length.1~age, data = wi[species.1=="walleye"])

#how many missing critical data? 35%
wi[is.na(age) | is.na(length.1), .N]/ wi[ , .N , ]

wi <- wi[!(is.na(age) | is.na(length.1))]

rm(wi_locs, wi_inland_lenage_19Mar2021)


colnames(wi)

# sd ----------------------------------------------------------------------


sd <- rbindlist(list(rbindlist(list(sd_length_age_4Oct2021,
                                    sd_NOP_age_growth_3Nov2022),
                               fill = TRUE,
                               use.names = TRUE),
                     sd_sauger_saugeye_age_growth_03Nov22),
                fill = TRUE,
                use.names = TRUE)

colnames(sd)
cols <- c("state", "lake_name", "survey_id",  #loc dat
          "date.1", #date dat
          "species.1", "age", "length.1", #core fish dat
          "aging_structure", "sex", "weight.1" #extra useful bits
)
setcolorder(sd,c(cols))
colnames(sd)

sd[ ,.N , age]



# sd_location -------------------------------------------------------------

sd[ , .N , .(survey_id, lake_name, date.1) ]

names(sd_locs) <- word(names(sd_locs),1,sep = ",")

sd_srvys <- merge(sd_srvys, sd_locs, all.x = T )

sd[ , survey_id := gsub("\\{|\\}" , "" , survey_id) ,  ]

sum(is.na(match(sd[ ,survey_id ,], sd_srvys[,SurveyID]))) #most  fish obs have a survey ID that worked. 

colnames(sd_srvys)[names(sd_srvys)=="SurveyID"] <- "survey_id"

sd_srvys <- sd_srvys[!duplicated(survey_id)]

colnames(sd)

sd <- merge(sd, sd_srvys, by = "survey_id", all.x = T)

sd[ , length(unique(sample_id.1)) ,]

sd[ is.na(Longitude), .N]

colnames(sd)

#there are some fish obs that went unmatched: here I directly match these to a lake ID (instead of a survey)
sd[is.na(Name), .N , .(lake_name, StateID) ][,StateID]

sd_locs[ StateID %in% sd[is.na(Name), .N , .(lake_name, StateID) ][,StateID]]

sd[is.na(Name),   , ][sd_locs[!duplicated(StateID)], on = "StateID"][is.na(i.Longitude)]

sd.a <- sd[!is.na(Name), ]

sd.b <- sd[is.na(Name)]

sd.b <- merge(sd.b, sd_locs, by = "StateID", all.x = T)

names(sd.b)

sd.b[ , c("Acres.x","Name.x","Latitude.x","Longitude.x", "County.x") := NULL , ]

colnames(sd.b) <- gsub("\\.y", "", colnames(sd.b))

sd <- rbindlist(list(sd.a,sd.b), fill = TRUE , use.names = TRUE)

rm(sd.a, sd.b)

sd[is.na(Longitude), .N , .(survey_id, lake_name, date.1, species.1) ]

sd[ ,"latitude" := Latitude , ]
sd[ ,"longitude" := Longitude , ]

colnames(sd)
sd[is.na(lake_name) , lake_name := Name,]

sd[ , c("Latitude", "Longitude", "FMA", "Waterbody", "ObjectID", "Station", "Method", "MethodCode", "Effort", "Acres", "Name") := NULL , ]

colnames(sd)[colnames(sd) == "StateID"] <- "lake_id" 
colnames(sd)[colnames(sd)=="County"] <- "county"

#14 rows from Wi will go unmatched


# sd_date -----------------------------------------------------------------

names(sd)

sd[ , summary(date.1) , ]#some data came with a date, others drew dates from the survey table
sd[ , date.1 := as.IDate(date.1) ,]
sd[ , date_clean := date.1 , ]

sd[ , any(is.na(as.IDate(SurveyDate, format = "%m/%d/%Y"))) ,]
sd[ , SurveyDate := as.IDate(SurveyDate, format = "%m/%d/%Y") , ]

sd[is.na(date.1) , date_clean := SurveyDate ,]

sd[is.na(date_clean), ]
sd[ , summary(date_clean) , ]

sd[ , hist(year(date_clean)) ,]
sd[ , hist(yday(date_clean)) ,]

colnames(sd)[colnames(sd)=="SurveyDate"] <- "date.2"

# make other dates character strings
datecols <- colnames(sd)[str_detect(colnames(sd), "date\\.")]
sd[    , (datecols) := lapply(.SD, as.character)    ,   .SDcols = datecols]



# sd_species --------------------------------------------------------------

sd[ , .N , species.1 ]

sd[ , species.1 := gsub(" ", "_", tolower(species.1))]

sd[species.1 == "bluegill", species.1 := "bluegill_sunfish"]


# sd_age ------------------------------------------------------------------

sd[ , hist(age) ,]


# sd_length ---------------------------------------------------------------

sd[ , hist(as.numeric(length.1)) , ]

sd[ , unique(length.1) ,]

sd[ , length.1 := as.numeric(length.1) ,] # verified NA introduced for NULL vals
sd[ !is.na(length.1) , mean(length.1) , .(original_file_name.1,species.1) ] #these look like millimeters to me!

sd[ , length_unit.1 := "millimeters"]


plot(length.1~age, data = sd[species.1=="walleye"])

rm(sd_length_age_4Oct2021, sd_locs, sd_srvys, sd_NOP_age_growth_3Nov2022, sd_sauger_saugeye_age_growth_03Nov22)

colnames(sd)


# merge all files ---------------------------------------------------------


cols <- c("state", "latitude", "longitude", #loc dat
          "date_clean", #date dat
          "species.1", "age", "length.1", "length_unit.1" #core fish dat
          )

# ar[ , .SD , .SDcols = cols]
# ia[ , .SD , .SDcols = cols]
# il[ , .SD , .SDcols = cols]
# indy[ , .SD , .SDcols = cols]
# mi[ , .SD , .SDcols = cols]
# mn[ , .SD , .SDcols = cols]
# # on[ , .SD , .SDcols = cols] # no lat/longs
# # ne[ , .SD , .SDcols = cols] #no ages
# sd[ , .SD , .SDcols = cols]
# wi[ , .SD , .SDcols = cols]
# 
# laa <-rbindlist(list(ar[ , .SD , .SDcols = cols],
#                                      ia[ , .SD , .SDcols = cols],
#                                      il[ , .SD , .SDcols = cols],
#                                      indy[ , .SD , .SDcols = cols],
#                                      mi[ , .SD , .SDcols = cols],
#                                      mn[ , .SD , .SDcols = cols],
#                                      sd[ , .SD , .SDcols = cols],
#                                      wi[ , .SD , .SDcols = cols]),
#                                fill = TRUE,
#                                use.names = TRUE)

laa <-rbindlist(list(ar,
                     ia,
                     il,
                     indy,
                     mi,
                     mn,
                     sd,
                     on,
                     wi),
                fill = TRUE,
                use.names = TRUE)

sort(colnames(laa))

cols <- c("state", "county","lake_name", "lake_id", "secondary_lake_id", "site_id.1", "latitude", "longitude",   #loc dat
          "year","date_clean", "survey_id", #date dat
          "species.1", "age", "length.1", "length_unit.1", "sample_id.1" , "sample_id.2",  #core fish dat
          "backcalculated_age",  "weight.1", "weight_unit.1", "aging_structure", "aging_method", "sex", "aging_data_notes.1", "aging_data_notes.2", "young_of_year", "reproductive_condition_notes"   #extra useful bits
)
setcolorder(laa,c(cols))
colnames(laa)

# fwrite(laa[ , 1:42 ,], file = "C:\\Users\\verh0064\\Desktop\\laa_merged.csv")

laa[ !is.na(length.1)& !is.na(age) & !is.na(latitude), .(.N, max(latitude), min(latitude)) , species.1 ][order(-N)]

laa[ !is.na(length.1)& !is.na(age) & !is.na(latitude), .("count" = .N, "prop_total" = .N/nrow(laa[ !is.na(length.1)& !is.na(age) & !is.na(latitude)])), state]



crosswalk_fodder <- laa[ , .N , .(state, county, lake_name, lake_id, secondary_lake_id, latitude, longitude)]

# save(laa, file = "scripts&data/data/output/length_age_merged.RData")

