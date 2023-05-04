# nhdid_crosswalk ---------------------------------------------------------

# load the length at age data and the nhd id crosswalk key
load(file = "scripts&data/data/output/length_age_merged.RData")

nhds <- fread("C:\\Users\\verh0064\\Desktop\\lake_id_crosswalk.csv") #https://www.sciencebase.gov/catalog/item/6206d3c2d34ec05caca53071



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

# so what we can do here is try to merge only to the dows that have a subbasin:
#we'll call that a, then do a merge on the other half of MN data call that b, then rbind a,b
#this is a clean merge ()
a <- merge(laa[!is.na(subbasin) & subbasin != "00", , ], nhds, by.x = "lake_id", by.y = "MNDOW_ID_c")

#this half is NOT
merge(laa[!is.na(subbasin) & subbasin == "00", , ], nhds, by.x = "lake_id", by.y = "MNDOW_ID_c")

laa[!is.na(subbasin) & subbasin == "00", .N , lake_id ] # these each need an nhdid

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

















nhds[!is.na(MNDOW_ID_c) , .N , MNDOW_ID_c][N>1, MNDOW_ID_c]


# a <- nhds[MNDOW_ID_c %in% nhds[!is.na(MNDOW_ID_c) , .N , MNDOW_ID_c][N>1, MNDOW_ID_c], ]

#but there IS a one-to-one match in the DOW to NHDHD in this group. 
# a[ , .N , .(MNDOW_ID_c, site_id) ]

#I want to cast this thing wider so that each NHDID only has one row
molten <- melt(nhds, id.vars = "site_id", na.rm = TRUE)

any(duplicated(molten))

molten <- molten[!duplicated(molten)]

molten[ , case := seq_len(.N) , .(variable, site_id) ]

molten[ , variable.v := paste(variable,case, sep = ".")]

nhds.w <- dcast(molten[!is.na(value)], site_id~variable.v, value.var = "value")










b <- dcast(a, site_id + MNDOW_ID + MNDOW_ID_c ~ MGLP_ID)
