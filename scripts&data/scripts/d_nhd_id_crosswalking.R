# nhdid_crosswalk ---------------------------------------------------------

library(data.table)
# update_dev_pkg()
library(sharpshootR)
library(stringr)



# load the length at age data and the nhd id crosswalk key
load(file = "scripts&data/data/output/length_age_merged.RData")

nhds <- fread("scripts&data/data/input/lake_id_crosswalk.csv") #https://www.sciencebase.gov/catalog/item/6206d3c2d34ec05caca53071

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



# mn ----------------------------------------------------------------------


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

mn_laa[ , .N, NHD_ID][ , hist(log(N)) , ]


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
wi_laa[ , summary(as.numeric(gsub("nhdhr_", "", NHD_ID))) , ]


# mi ----------------------------------------------------------------------

#goddam GDrive scrapped a script where i thoroughly investigate this, BUT MI data do not have any nice keys in the NHDhr crosswalk from the USGS folks. 

#here's a snapshot of things to show off the problem:

laa[state == "Michigan", .N, .(lake_id, secondary_lake_id, lake_name, county, latitude, longitude) ]

laa[state =="Michigan", unique(secondary_lake_id)]

# here are the three semi-consistent sets of ID's we could try to key to
laa[state =="Michigan", unique(lake_id)]

laa[state =="Michigan" &
      str_detect(secondary_lake_id, "[A-Za-z]"), unique(secondary_lake_id)] # these are PLSS w/ some mod final 2 characters, none of the NHDs have this. COuld conver to lat/lon

laa[state =="Michigan" &
      !str_detect(secondary_lake_id, "[A-Za-z]"), unique(secondary_lake_id)]

colnames(nhds)# could try LAGOS
nhds[  , unique(gsub("LAGOS_" , "", LAGOS_ID)) ,]

match(laa[state =="Michigan" &
            !str_detect(secondary_lake_id, "[A-Za-z]"), unique(secondary_lake_id)], 
      nhds[  , unique(gsub("LAGOS_" , "", LAGOS_ID)) ,]
      )#NOPE


# could try MGLP
nhds[ str_detect(MGLP_ID, "MI") , unique(gsub("MGLP_" , "", MGLP_ID)) ,]

nhds[ str_detect(MGLP_ID, "MI") , unique(str_sub(MGLP_ID, -5,-1)) ,]

match(laa[state =="Michigan" &
            !str_detect(secondary_lake_id, "[A-Za-z]"), unique(secondary_lake_id)], 
      nhds[ str_detect(MGLP_ID, "MI") , unique(str_sub(MGLP_ID, -5,-1)) ,]
)#NOPE


#anywho-- no worky could give PLSS lat longs a try with https://rdrr.io/cran/sharpshootR/man/PLSS2LL.html

mi_plss <- laa[ state == "Michigan" & 
                  is.na(latitude) & 
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

laa[state == "Michigan" & 
      is.na(latitude), latitude := mi_plss[match(laa[state == "Michigan" & 
            is.na(latitude),secondary_lake_id], mi_plss[,secondary_lake_id]), lat
        
      ] ]

laa[state == "Michigan" & 
      is.na(longitude), longitude := mi_plss[match(laa[state == "Michigan" & 
                                                       is.na(longitude),secondary_lake_id], mi_plss[,secondary_lake_id]), lon
                                           
      ] ]

# that filled a bunch! (but doesn't really fix the connection to Lindsay Platts crosswalk table...)
laa[state == "Michigan", .N, .( lake_id, secondary_lake_id, lake_name, county, latitude, longitude) ]

laa[state == "Michigan" & is.na(latitude), .N, .(lake_id, secondary_lake_id, lake_name, county, latitude, longitude) ]

# May 2023 we recieved a crosswalk/key from Jared Ross at Michigan State University
# Arthur and I worked with Kevin Wehrly to hopefully help address this issue.  I’ve resent the geodatabase of lakes using the FileDepot service (might take 10-15 minutes),
# but also attached here is a csv that should provide a linkage to your fish data and the Michigan crosswalk 

# 1. survey_id from your fish table = Survey_Number in the attached file,
# 2. unique_id should help link to the crosswalk table
# 3. Within the geodatabase, the “lake_information” table should provide an NHD ID you can pull over after linking lagoslakeid (I wouldn’t call that an easy match!).

laa[state == "Michigan", .N, .(survey_id, lake_id, secondary_lake_id, lake_name, county, latitude, longitude) ]

mi_locs[ , .N , Survey_Number]

# check match 1.
match(laa[state == "Michigan", .N, .(survey_id) ][,survey_id], mi_locs[, Survey_Number]
)

#check match 2.
match(mi_locs[,UNIQUE_ID], mi_lagos_crosswalk[,UNIQUE_ID])

#check lagosID matches to lplatt table -- kinda shitty
#nomatches
sum(is.na(match(mi_lagos_crosswalk[!lagoslakeid == -999,lagoslakeid], nhds[  , unique(as.integer(gsub("LAGOS_" , "", LAGOS_ID))) ,])))
#goodmatches
sum(!is.na(match(mi_lagos_crosswalk[!lagoslakeid == -999,lagoslakeid], nhds[  , unique(as.integer(gsub("LAGOS_" , "", LAGOS_ID))) ,])))

#the last bit here (connecting to the .gdb that jared sent) seems irrelevant-- we've got the lagos-NHDID crosswalk built into THE NHDS table. 


# DO
# add "survey_number to mi in laa
mi_locs <- merge(mi_locs, mi_lagos_crosswalk, by = "UNIQUE_ID", all.x = T)

mi_laa <- laa[state == "Michigan"]
mi_laa[ , survey_id := as.integer(survey_id) ,]

mi_laa <- merge(mi_laa, mi_locs, by.x = "survey_id", by.y = "Survey_Number", all.x = T)

#still missing 4 lagos IDs
mi_laa[state == "Michigan", .N, .(survey_id, lake_id, secondary_lake_id, lake_name, county, latitude, longitude, lagoslakeid) ][, summary(lagoslakeid)]
# and still more NAs
mi_laa[lagoslakeid == -999, lagoslakeid := NA]

# check lagos IDs 
sum(mi_laa[ , unique(lagoslakeid) , ] %in% nhds[  , unique(as.integer(gsub("LAGOS_" , "", LAGOS_ID))) ,])

sum(mi_laa[ , unique(lagoslakeid) , ] %in% nhds[  , unique(as.integer(gsub("LAGOS_" , "", LAGOS_ID))) ,])
sum(!(mi_laa[ , unique(lagoslakeid) , ] %in% nhds[  , unique(as.integer(gsub("LAGOS_" , "", LAGOS_ID))) ,]))

#I want to cast this thing wider so that each LAGOS only has one row
molten <- melt(nhds[!is.na(LAGOS_ID), ], id.vars = "LAGOS_ID", na.rm = TRUE)

any(duplicated(molten))

molten <- molten[!duplicated(molten)]

molten[ , case := seq_len(.N) , .(LAGOS_ID,variable) ]

molten[ , variable.v := paste(variable,case, sep = ".")]

nhds.lagoskey <- dcast(molten, LAGOS_ID ~ variable.v, value.var = "value")

names(nhds.lagoskey)[names(nhds.lagoskey)== "site_id.1"] <- "NHD_ID"

nhds.lagoskey[ , LAGOS_ID_c := as.integer(gsub("LAGOS_" , "", LAGOS_ID)) , ] 

# now smooth merge: 
mi_laa <- merge(mi_laa, nhds.lagoskey, by.x = "lagoslakeid", by.y = "LAGOS_ID_c", all.x = TRUE)

mi_laa[ , .N, NHD_ID][ , hist(log(N)) , ]

mi_laa[state == "Michigan", .N, .(NHD_ID, survey_id, lake_id, secondary_lake_id, lake_name, county, latitude, longitude, lagoslakeid) ][ ,.N, is.na(NHD_ID) ] #374 lakes of 556 with NHDs (this number *might* be improved by usging the gdb provided by MSU crew)

mi_laa[state == "Michigan", .N, .(NHD_ID, survey_id, lake_id, secondary_lake_id, lake_name, county, latitude, longitude, lagoslakeid) ][ ,.N, is.na(lagoslakeid) ] 

names(mi_laa) # there are alot of junk cols here. 
mi_laa[  ,.N, .(is.na(LAT_DD), is.na(latitude))  ]

mi_laa[ , .N , .(survey_id, NHD_ID, lagoslakeid) ]

#use this many column'd mess to get a cleaner mi_laa
# mi_laa <- merge(laa[state == "Michigan"], mi_laa, by.x = "lake_id", by.y = "WBIC_ID_c", all.x = TRUE)

mi_laa[ , 131:149 := NULL , ]

library(sf)
mi_lakeinformation <- st_read(dsn = "E:\\Shared drives\\Hansen Lab\\RESEARCH PROJECTS\\Fish Survey Data\\MI_Data\\FileDepot-UpYTQqaSS7gtVQW7\\MGLP_LAGOS_Oct_2022.gdb\\MGLP_LAGOS_Oct_2022.gdb\\MGLP_LAGOS.gdb", layer = "lake_information")

sum(mi_laa[is.na(NHD_ID) , lagoslakeid] %in% mi_lakeinformation[, "lake_information_csv_lagoslakeid"])

mi_laa[is.na(NHD_ID), NHD_ID := mi_lakeinformation[match(mi_laa[is.na(NHD_ID) , lagoslakeid], mi_lakeinformation[, "lake_information_csv_lagoslakeid"]), "lake_information_csv_lake_nhdid" ]]

mi_laa[ , .N , .(is.na(NHD_ID),is.na(lagoslakeid)) ]

fwrite(mi_laa, file = "E:\\My Drive\\Documents\\UMN\\Hansen Lab\\Rprojects\\CASC_fish_growth\\scripts&data\\data\\output\\mi_with_lagos.csv")

# ia ----------------------------------------------------------------------

a <- laa[state == "Iowa", .N, .(lake_id, secondary_lake_id, lake_name, county, latitude, longitude)][order(lake_name,county)]

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

laa[state == "Iowa", .N, .(lake_id, secondary_lake_id, lake_name, county, latitude, longitude)][order(lake_name,county)][is.na(lake_id)]

#anita
laa[state == "Iowa" & is.na(lake_id) &
      lake_name == "anita" ,.N]


# merge back together -----------------------------------------------------

laa_nhds <- rbindlist(list(rbindlist(list(mi_laa,
                                    mn_laa),
                               fill = TRUE,
                               use.names = TRUE),
                     wi_laa),
                fill = TRUE,
                use.names = TRUE)

saveRDS(laa_nhds, file = "scripts&data\\data\\output\\laa_nhds.rds")


















































