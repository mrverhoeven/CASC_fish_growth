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



# Arkansas
names(ar_reservoir_age_8Oct2021)
names(ar_reservoir_age2_8Oct2021)

ar <- merge.data.table(ar_reservoir_age_8Oct2021, ar_reservoir_age2_8Oct2021,  all = T)



ar[ , summary(.SD) , .SDcols = cols]


#loc dat
ar[ , .N , .(state,lake_name, location_notes.1)]
ar[lake_name == "BSL", lake_name := "Bull Shoals Lake"]
ar[lake_name == "NFL", lake_name := "Norfork Lake"]


#date dat
ar[ , .N , .(year, date.1, date.2) ]
ar[ , date_clean :=  as.IDate(paste(year,date.2, date.1, sep = "/"), format = "%Y/%m/%d")  , ]
ar[ , summary(date_clean) , ]

# year
ar[ , .N , year ]
ar[is.na(year) & is.na(date_clean), .N , .(lake_name, species.1) ] #there are a few records here for which we have no dates at all. 


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

#age
ar[ , .N , age ]

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




#locdat
# is there any data cap in state or county the we need? 
ia[ , .N , .(state,county, lake_name, lake_id)]
all(ia[ , .N , .(state,county, lake_name, lake_id)][,N] == ia[ , .N , .(lake_name, lake_id)][, N]) # all the fish by the larger keyset is captured in lake_name.  

#import Iowa loc data
ia_locs <- fread("E:\\Shared drives\\Hansen Lab\\RESEARCH PROJECTS\\Fish Survey Data\\IA_Data\\ia_raw_disaggregated_data\\samplestationlocationmap_drawrectangletofilterlocations.csv")


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




#try merge
try <- merge(ia, ia_locs, by = c("lake_name", "county"), all.x = T, suffixes = c(".x", ".y"), no.dups = TRUE)

try[!is.na(Longitude), .N]/try[,.N]
try[is.na(Longitude), .N , .(lake_name, county, lake_id)]






ia_locs[str_detect( lake_name, "yellow")]

yellow smoke, crawford, 42.023675942780336, -95.32476570431955
wilson, taylor, 40.83724098351677, -94.54277411857501
winterfield, sioux, 43.2121169757937, -96.29127739988448



ia_locs[str_detect(lake_name,"rrowh"), lake_name]
try[ is.na(Longitude) , .N , .(lake_name,county)]
try[ is.na(Longitude) , unique(lake_name) , ]

ia[is.na(Longitude), Longitude :=  , ]


ia[is.na(county)]

match(ia[ is.na(county),.N, lake_name][,lake_name], ia_locs[ , lake_name, ])

ia_locs[ , lake_name, ]


#do the ia lat/long merge into the LAA data: 

ia_locs[ia, on = .(lake_name, county)][!is.na(Latitude) ,.N ,]

ia_locs[ia, on = .(lake_name, county)]

ia[ , length(unique(lake_id)) , .(lake_name, county)][V1>1]


ia[ , .N , .(lake_name, lake_id)]

ia[ , unique(lake_id) , ]

file.choose()

ia <- ia[!duplicated(ia[ , .(state, loc, species, age, length) , ]  ) , , ]


!duplicated(ia[ , .(state, loc, species, age, length) , ]  )



#' 