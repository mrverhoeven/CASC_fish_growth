#' Cleaning and organizing & formatting state datsets to meet growth proj needs:
#' 1. 


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

ar[ , .N , .(lake_name, location_notes.1, latitude, longitude) ]

ar[ lake_name == "Norfork Lake" , ':=' (latitude = "36.351351" , longitude = "-92.232243")   , ]
ar[ lake_name == "Bull Shoals Lake" , ':=' (latitude = "36.486572" , longitude = "-92.894508")   , ]
ar[ location_notes.1 == "Location:James River Arm" , ':=' (latitude = "36.718906601766705" , longitude = "-93.59351150562455")   , ]
ar[ location_notes.1 == "Location:Kings River Arm" , ':=' (latitude = "36.55397644090967" , longitude = "-93.50610006173052")   , ]
ar[ location_notes.1 == "Location:Mid White River Arm" , ':=' (latitude = "36.60279620933327" , longitude = "-93.50610006173052")   , ]
ar[ location_notes.1 == "Location:Long Creek Arm" , ':=' (latitude = "36.48231274609963" , longitude = "-93.3020160955791")   , ]
ar[ location_notes.1 == "Location:Upper White River Arm" , ':=' (latitude = "36.52369836589746" , longitude = "-93.72775995312399")   , ]


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
# is there any dats cap in state or county the we need? 
ia[ , .N , .(state,county, lake_name, lake_id)]
all(ia[ , .N , .(state,county, lake_name, lake_id)][,N] == ia[ , .N , .(lake_name, lake_id)][, N]) # all the 

ia[ , .N , .(lake_name, lake_id)]

ia[ , unique(lake_id) , ]

file.choose()



#' 