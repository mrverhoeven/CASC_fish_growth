# header ---------------------------------------------------------

library(data.table)
# update_dev_pkg()
library(sharpshootR)
library(stringr)
library(sf)



# load the length at age data and the nhd id crosswalk key
# load(file = "scripts&data/data/output/length_age_merged.RData")

nhds <- fread("scripts&data/data/input/lake_id_crosswalk.csv") #https://www.sciencebase.gov/catalog/item/6206d3c2d34ec05caca53071



# mn ----------------------------------------------------------------------
#scope out the MN merge

#how many of the MN lake IDs exist in the NHDIDs?
sum(laa[ state == "Minnesota", .N , lake_id][ , lake_id , ] %in% word(nhds[ , MNDOW_ID], -1, sep = "_"))


#add a col in the NHDs that strips out the colnames
nhds[ , MNDOW_ID_c := gsub("MNDOW_", "", MNDOW_ID)  ]

#we're going to have some 1:many merge issues:
nhds[!is.na(MNDOW_ID_c) ,  ][duplicated(MNDOW_ID_c)]

#these data include a bunch of stream and river catches (lake_id is a kittle no. https://mn.gov/mnit/government/policies/geo/mn-river-watercourse-identification-codes.jsp)
laa[state == "Minnesota", substr(lake_id, 7,8)]
laa[state == "Minnesota" ,.N , lake_id ]

# we need to ignore these when writing code to grab the subbasin ID's from lakes
laa[state == "Minnesota" & !str_detect(lake_id, "[a-zA-Z]"), .N ]
laa[state == "Minnesota" & !str_detect(lake_id, "[a-zA-Z]"), .N , lake_id ]
laa[state == "Minnesota" & !str_detect(lake_id, "[a-zA-Z]"), subbasin := substr(lake_id, 7,8)]

#this field captures 2 things--first, where NA and state=MN, the fish was captured from a non-lake, otherwise the field shows subbasin ID
laa[state == "Minnesota" , .N , subbasin]
laa[!is.na(subbasin), .N , ]

# # so what we can do here is try to merge only to the dows that have a subbasin:
# #we'll call that a, then do a merge on the other half of MN data call that b, then rbind a,b
# #this is a clean merge ()
# a <- merge(laa[!is.na(subbasin) & subbasin != "00", , ], nhds, by.x = "lake_id", by.y = "MNDOW_ID_c")
# 
# #this half is NOT
# merge(laa[!is.na(subbasin) & subbasin == "00", , ], nhds, by.x = "lake_id", by.y = "MNDOW_ID_c")
# 
# laa[!is.na(subbasin) & subbasin == "00", .N , lake_id ] # these each need an nhdid






#I want to cast this thing wider so that each MN DOW only has one row
molten <- melt(nhds[!is.na(MNDOW_ID), ], id.vars = "MNDOW_ID_c", na.rm = TRUE)

any(duplicated(molten))

molten <- molten[!duplicated(molten)]

molten[ , case := seq_len(.N) , .(MNDOW_ID_c,variable) ]

molten[ , variable.v := paste(variable,case, sep = ".")]

nhds.mnkey <- dcast(molten, MNDOW_ID_c ~ variable.v, value.var = "value")

names(nhds.mnkey)[names(nhds.mnkey)== "site_id.1"] <- "NHD_ID"


# now smooth: 
mn_laa <- merge(laa[state == "Minnesota"], nhds.mnkey, by.x = "lake_id", by.y = "MNDOW_ID_c", all.x = TRUE)

names(mn_laa)

mn_laa[ , .N, NHD_ID][ , hist(log(N)) , ]


#drop all NA cols
mn_laa <- mn_laa[, .SD , .SDcols = \(x) !all(is.na(x))]


#now we simply re-run this process state by state (assuming that all the data within a state match something in that crosswalk cleanly)


# wi ----------------------------------------------------------------------

#I want to cast this thing wider so that each WBIC only has one row
molten <- melt(nhds[!is.na(WBIC_ID), ], id.vars = "WBIC_ID", na.rm = TRUE)

any(duplicated(molten))

molten <- molten[!duplicated(molten)]

molten[ , case := seq_len(.N) , .(WBIC_ID,variable) ]

molten[ , variable.v := paste(variable,case, sep = ".")]

nhds.wikey <- dcast(molten, WBIC_ID ~ variable.v, value.var = "value")

names(nhds.wikey)[names(nhds.wikey)== "site_id.1"] <- "NHD_ID"

#add a col in the NHDs that strips out the colnames
nhds.wikey[ , WBIC_ID_c := gsub("WBIC_", "", WBIC_ID)  ]


# now smooth: 
wi_laa <- merge(laa[state == "Wisconsin"], nhds.wikey, by.x = "lake_id", by.y = "WBIC_ID_c", all.x = TRUE)

wi_laa[ , .N, NHD_ID][ , hist(log(N)) , ]
wi_laa[ , .N , is.na(NHD_ID) ] #95% coverage

names(wi_laa)

#drop all NA cols
wi_laa <- wi_laa[, .SD , .SDcols = \(x) !all(is.na(x))]


# mi ----------------------------------------------------------------------

# May 2023 we recieved a crosswalk/key from Jared Ross at Michigan State University (here's their advice):
# 
#"Arthur and I worked with Kevin Wehrly to hopefully help address this issue.  I’ve resent the geodatabase of lakes using the FileDepot service (might take 10-15 minutes),
# but also attached here is a csv that should provide a linkage to your fish data and the Michigan crosswalk 

# 1. survey_id from your fish table = Survey_Number in the attached file,
# 2. unique_id should help link to the crosswalk table
# 3. Within the geodatabase, the “lake_information” table should provide an NHD ID you can pull over after linking lagoslakeid (I wouldn’t call that an easy match!)."

laa[state == "Michigan", .N, .(survey_id, lake_id, secondary_lake_id, lake_name.1, county, latitude, longitude) ]

mi_locs[ , .N , Survey_Number]

# check match 1.
sum(is.na(match(laa[state == "Michigan", .N, .(survey_id) ][,survey_id], mi_locs[, Survey_Number]
))) # two go unmatched

#check match 2.
sum(is.na(match(mi_locs[,UNIQUE_ID], mi_lagos_crosswalk[,UNIQUE_ID]))) #2 go unmatched

#check step 3. 
#the last bit here (connecting to the .gdb that jared sent) seems irrelevant-- we've got the lagos-NHDID crosswalk built into THE NHDS table. 
#check lagosID matches to lplatt table -- kinda shitty without their "lake information" table from LAGOS
#show lplatt crosswalk, then LAGOS gdb connections

#nomatches
sum(is.na(match(mi_lagos_crosswalk[!lagoslakeid == -999,lagoslakeid], nhds[  , unique(as.integer(gsub("LAGOS_" , "", LAGOS_ID))) ,])))
#goodmatches
sum(!is.na(match(mi_lagos_crosswalk[!lagoslakeid == -999,lagoslakeid], nhds[  , unique(as.integer(gsub("LAGOS_" , "", LAGOS_ID))) ,])))

mi_lakeinformation <- st_read(dsn = "E:\\Shared drives\\Hansen Lab\\RESEARCH PROJECTS\\Fish Survey Data\\MI_Data\\FileDepot-UpYTQqaSS7gtVQW7\\MGLP_LAGOS_Oct_2022.gdb\\MGLP_LAGOS_Oct_2022.gdb\\MGLP_LAGOS.gdb", layer = "lake_information")

sum(mi_lagos_crosswalk[!lagoslakeid == -999,lagoslakeid] %in% mi_lakeinformation[, "lake_information_csv_lagoslakeid"])
sum(is.na(match(mi_lagos_crosswalk[!lagoslakeid == -999,lagoslakeid], mi_lakeinformation[, "lake_information_csv_lagoslakeid"]))) #this lakeinformation table has complete coverage. 

#ultimately, we want a LAGOS id and a NHDid attached to each of these MI datapoints. The rest of the crap that comes with these crosswalks is extraneous. 

names(laa)

#can we clear a place in the mi data for these LAGOS ids to go? LAGOS_ID.1 is the format that we'll end up with anyway (see completed MN merge)
names(mn_laa)
mn_laa[ , .N , LAGOS_ID.1]

#add lakeinformation table's NHDs to the mi_lagos crosswalk:

match(mi_lagos_crosswalk[,lagoslakeid], mi_lakeinformation[, "lake_information_csv_lagoslakeid"])
mi_lakeinformation[ match(mi_lagos_crosswalk[,lagoslakeid], mi_lakeinformation[, "lake_information_csv_lagoslakeid"]) , "lake_information_csv_lake_nhdid"  ]
mi_lagos_crosswalk[ , NHD_ID := mi_lakeinformation[ match(mi_lagos_crosswalk[,lagoslakeid], mi_lakeinformation[, "lake_information_csv_lagoslakeid"]) , "lake_information_csv_lake_nhdid"  ] , ]

#bring the NHD_ID into the mi_locs file 
match(mi_locs[,UNIQUE_ID], mi_lagos_crosswalk[,UNIQUE_ID])
mi_locs[ , NHD_ID := mi_lagos_crosswalk[ match(mi_locs[,UNIQUE_ID], mi_lagos_crosswalk[,UNIQUE_ID]), NHD_ID], ]
mi_locs[ , LAGOS_ID := mi_lagos_crosswalk[ match(mi_locs[,UNIQUE_ID], mi_lagos_crosswalk[,UNIQUE_ID]), lagoslakeid], ]

mi_locs[ , unique(NHD_ID)]
mi_locs[ , unique(LAGOS_ID)]


#bring the lagos and NHD_IDs into the mi_laa dataset 

match(laa[state == "Michigan", .N, .(survey_id) ][,survey_id], mi_locs[, Survey_Number])
mi_laa <- laa[state == "Michigan"]

mi_laa[  , `:=` (LAGOS_ID = mi_locs[match(mi_laa[, survey_id], mi_locs[, Survey_Number]) , c(LAGOS_ID)],
                 NHD_ID = mi_locs[match(mi_laa[, survey_id], mi_locs[, Survey_Number]) , c(NHD_ID)])  ]

mi_laa[ , .N , is.na(NHD_ID)]
mi_laa[ , .N , is.na(LAGOS_ID)]


#now connect to lplatts crosswalk

sum(!(mi_laa[ , unique(LAGOS_ID) , ] %in% nhds[  , unique(as.integer(gsub("LAGOS_" , "", LAGOS_ID))) ,]))
sum((mi_laa[ , unique(NHD_ID) , ] %in% nhds[  , unique(gsub("nhdhr_" , "", site_id)) ,])) #this match is the same no matter which code we match on--we'll use lagos:

#I want to cast this thing wider so that each LAGOS only has one row
molten <- melt(nhds[!is.na(LAGOS_ID), ], id.vars = "LAGOS_ID", na.rm = TRUE)

any(duplicated(molten))

molten <- molten[!duplicated(molten)]

molten[ , case := seq_len(.N) , .(LAGOS_ID,variable) ]

molten[ , variable.v := paste(variable,case, sep = ".")]

nhds.lagoskey <- dcast(molten, LAGOS_ID ~ variable.v, value.var = "value")

names(nhds.lagoskey)[names(nhds.lagoskey)== "site_id.1"] <- "NHD_ID"

nhds.lagoskey[ , LAGOS_ID_c := as.numeric(gsub("LAGOS_" , "", LAGOS_ID)) , ]



# now smooth merge: 
mi_laa <- merge(mi_laa, nhds.lagoskey, by.x = "LAGOS_ID", by.y = "LAGOS_ID_c", all.x = TRUE)

mi_laa[ , .N, NHD_ID.x][ , hist(log(N)) , ]

mi_laa[ , .N , .(NHD_ID.x,NHD_ID.y)]

sum(!(mi_laa[!is.na(NHD_ID.y) , gsub("nhdhr_" , "", NHD_ID.y) , ] == mi_laa[!is.na(NHD_ID.y) , NHD_ID.x ,])) # this means that lyndise's nhd's matched all of the NHDs from the MSU LAGOS db

#drop lyndsie's because they are less complete
mi_laa[, NHD_ID.y := NULL]

names(mi_laa)[names(mi_laa)== "NHD_ID.x"] <- "NHD_ID"

#drop extra LAGOS col (was used for merge)
mi_laa[ , LAGOS_ID := NULL]

names(mi_laa)[names(mi_laa)== "LAGOS_ID.y"] <- "LAGOS_ID"

#drop all NA cols
mi_laa <- mi_laa[, .SD , .SDcols = \(x) !all(is.na(x))]

#fill NA lat/longs
match(mi_laa[is.na(latitude), survey_id , ],mi_locs[ , Survey_Number ,])

mi_laa[is.na(latitude), `:=` (latitude  = mi_locs[match(mi_laa[is.na(latitude), survey_id , ],mi_locs[ , Survey_Number ,]),  LAT_DD],
                              longitude = mi_locs[match(mi_laa[is.na(latitude), survey_id , ],mi_locs[ , Survey_Number ,]), LONG_DD])
]



mi_laa[ ,.N, .(NHD_ID, survey_id, lake_id, secondary_lake_id, lake_name.1, county, latitude, longitude, LAGOS_ID) ][ ,.N, is.na(NHD_ID) ] #17 lakes of 556 with missing NHDs

mi_laa[is.na(NHD_ID) , .N , .(NHD_ID, survey_id, lake_id, secondary_lake_id, lake_name.1, county, latitude, longitude, LAGOS_ID) ]





#use PLSS to get a lat long for a few more
#anywho-- no worky could give PLSS lat longs a try with https://rdrr.io/cran/sharpshootR/man/PLSS2LL.html

mi_plss <- mi_laa[is.na(latitude) & 
                  str_detect(secondary_lake_id, "[A-Za-z]") & 
                  !secondary_lake_id == "ONTON01"
                , .N , .(secondary_lake_id) ]

mi_plss[ , t := paste("T", str_sub(secondary_lake_id, 1, 3), sep = "")  ]
mi_plss[ , r := paste("R", str_sub(secondary_lake_id, 4, 6), sep = "")  ]
mi_plss[ , s := str_sub(secondary_lake_id, 7, 8)  ]
mi_plss[ , type := "SN"  ]
mi_plss[ , m := "MI19"  ]
mi_plss[ , id := .I , ]
mi_plss[ , ':=' (q = NA, qq = NA)]



# generate formatted PLSS codes
mi_plss[ ,plssid :=  formatPLSS(data.frame(mi_plss)) ]

# fetch lat/long coordinates
mi_plss <- cbind(mi_plss, PLSS2LL(mi_plss))

mi_laa[is.na(latitude), `:=` (latitude  = mi_plss[match(mi_laa[is.na(latitude),secondary_lake_id], mi_plss[,secondary_lake_id]), lat],
                             longitude = mi_plss[match(mi_laa[is.na(latitude),secondary_lake_id], mi_plss[,secondary_lake_id]), lon])]
mi_laa[state == "Michigan", .N, .(NHD_ID, survey_id, lake_id, secondary_lake_id, lake_name.1, county, latitude, longitude, LAGOS_ID) ][ ,.N, is.na(NHD_ID) ] 

mi_laa[ , .N, NHD_ID][ , hist(N) , ]


# ia ----------------------------------------------------------------------

a <- laa[state == "Iowa", .N, .(lake_id, secondary_lake_id, lake_name.1, county, latitude, longitude)][order(lake_name.1,county)]

sort(nhds[ , unique(gsub("IADNR_", "" , IADNR_ID)) ,])

#first let's check coverage on the IDs we have:

match( laa[state == "Iowa", unique(lake_id)] , 
       nhds[ , unique(gsub("IADNR_", "" , IADNR_ID)) ,]
       )
#matched
sum(!is.na(match( laa[state == "Iowa", unique(lake_id)] , 
       nhds[ , unique(gsub("IADNR_", "" , IADNR_ID)) ,]
)))
#unmatched
sum(is.na(match( laa[state == "Iowa", unique(lake_id)] , 
                  nhds[ , unique(gsub("IADNR_", "" , IADNR_ID)) ,]
)))
#call those out
laa[state == "Iowa", unique(lake_id)][is.na(match( laa[state == "Iowa", unique(lake_id)] , 
                                                   nhds[ , unique(gsub("IADNR_", "" , IADNR_ID)) ,]
))]

#can we backfill any of these lake IDs?
laa[state == "Iowa", .N, .(lake_id, secondary_lake_id, lake_name.1, county, latitude, longitude)][order(lake_name.1,county)][is.na(lake_id)]

a <- laa[state == "Iowa" & is.na(lake_id)]
a[ , .N , .(lake_name.1,county,lake_id)]

b <- laa[state == "Iowa" & !is.na(lake_id)]
b[ , .N , .(lake_name.1, county)]

#fill codes first with county, then without it 
a[ , `:=` (lake_id = b[match(a[, paste(lake_name.1,county)],b[ , paste(lake_name.1,county)]) , lake_id])]
a[is.na(county) , `:=` (lake_id = b[match(a[is.na(county), lake_name.1],b[ , lake_name.1]) , lake_id])]

#fill county, lat and long
a[ , `:=` (county    = b[match(a[, lake_id],b[ , lake_id]) , county],
           latitude  = b[match(a[, lake_id],b[ , lake_id]) , latitude],
           longitude = b[match(a[, lake_id],b[ , lake_id]) , longitude])]


laa[state == "Iowa" & is.na(lake_id)] <- a
rm(a,b)

#I want to cast this thing wider so that each IA code only has one row
molten <- melt(nhds[!is.na(IADNR_ID), ], id.vars = "IADNR_ID", na.rm = TRUE)

any(duplicated(molten))

molten <- molten[!duplicated(molten)]

molten[ , case := seq_len(.N) , .(IADNR_ID,variable) ]

molten[ , variable.v := paste(variable,case, sep = ".")]

nhds.iakey <- dcast(molten, IADNR_ID ~ variable.v, value.var = "value")

names(nhds.iakey)[names(nhds.iakey)== "site_id.1"] <- "NHD_ID"

#add a col in the NHDs that strips out the colnames
nhds.iakey[ , IADNR_ID_c := gsub("IADNR_", "", IADNR_ID)  ]


# now smooth: 
ia_laa <- merge(laa[state == "Iowa"], nhds.iakey, by.x = "lake_id", by.y = "IADNR_ID_c", all.x = TRUE)

ia_laa[ , .N, NHD_ID][ , hist(log(N)) , ]
ia_laa[ , .N , is.na(NHD_ID) ] #71% coverage

#we're still missing NHDs for 52/151 waterbodies
ia_laa[, .N , .(NHD_ID, lake_id, lake_name.1, county, latitude, longitude) ][order(-is.na(NHD_ID))]

names(ia_laa)

#drop all NA cols
ia_laa <- ia_laa[, .SD , .SDcols = \(x) !all(is.na(x))]

#this table could go out to IADNR for filling of information (especially NHD_ID, Lat/Long okay too)
ia_laa[is.na(NHD_ID), .N , .(NHD_ID, lake_id, lake_name.1, county, latitude, longitude, lake_type,  original_file_name.1, garbage_bin_notes.12 )]



# il ----------------------------------------------------------------------

laa[state == "Illinois" , .N, .(lake_id, secondary_lake_id, lake_name.1, county, latitude, longitude, species.1)][order(lake_name.1,county)]
# I don't see anything in here that could be useful as a key to the NHDs. However, we ony have 2 lakes, so could probably retrieve manually. 

names(nhds)


# in ----------------------------------------------------------------------

laa[state == "Indiana" , .N, .(site_id.1 , lake_id, secondary_lake_id, lake_name.1, county, latitude, longitude)][order(lake_name.1,county)]

match(laa[state == "Indiana" , .N, .(lake_id, secondary_lake_id, lake_name.1, county, latitude, longitude)][ ,lake_id] , nhds[ , gsub("nhdhr_", "", site_id) , ])

in_laa <- laa[state == "Indiana"]

#NHDs has both an IN_DNR_ID and a IN_CLP_ID, none of which appear to be useful matches to any lake IDs we have here.

#drop all NA cols
in_laa <- in_laa[, .SD , .SDcols = \(x) !all(is.na(x))]

in_laa[ , unique(lake_id) , ]# despite not matching the lplatt crosswalk, these sure as heck look like NHDIDs to me. 

in_laa[ , NHD_ID := lake_id , ]


# sd ----------------------------------------------------------------------

laa[state == "South Dakota" , .N, .(lake_id, secondary_lake_id, lake_name.1, county, latitude, longitude)][order(lake_name.1,county)]

laa[state == "South Dakota" ,unique(lake_id)]

nhds[  , unique(gsub("SD_SD-", "", SD_ID)) ] 

nhds[  , unique(gsub("MGLP_", "", MGLP_ID)) ] 

























# merge back together -----------------------------------------------------

laa_nhds <- 
  rbindlist(list(mi_laa,mn_laa,wi_laa, ia_laa, in_laa, laa[!(state %in% c("Michigan", "Minnesota", "Wisconsin", "Iowa", "Indiana"))]),
    fill = TRUE,
    use.names = TRUE)

# saveRDS(laa_nhds, file = "scripts&data\\data\\output\\laa_nhds.rds")





# Graveyard -------------------------------------------------------------------------

laa_nhds[ , .N , .(state,county,lake_name.1, lake_id, NHD_ID) ][ , .N , is.na(NHD_ID)]
365002/1799847





























