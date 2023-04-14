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

ar[length_unit.1 == "col_name_Length..mm." , length_unit.1 := "milimeters" ]
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

ia[length_unit.1 == "col_name_Total Length (mm)" , length_unit.1 := "milimeters" ]
ar[ , mean(length.1) , species.1]

ia[species.1 == "walleye" , hist(length.1) , ]

plot(length.1~age, data = ia[species.1=="walleye"])


















