#' # Aggregating Length at Age Data
#' 

#' 

# library(readr)
# library(readxl)
# library(dplyr)
# library(stringr)
# library(arrow)
# library(data.table)
# # update_dev_pkg()
# library(googledrive)
# library(janitor)
# library(tidyr)
# # install.packages("tidyr")
# 
# 



#' 
#' 
#' Load into workspace
#' MN:mn_aged_fish_29Sep2021, mn_aged_fish_1_29Sep2021, mn_aged_fish_2_29Sep2021
#' 
#' WI: wi_inland_lenage_19Mar2021
#' 
#' MI: mi_statustrends_lenage_20May2023
#' 
#' SD: sd_sauger_saugeye_age_growth_03Nov22, sd_lakesurvey_fishdata_agedfish, sd_NOP_age_growth_3Nov2022
#' 
#' IA: ia_age_length_21Aug2021, ia_BLG_age_length_21Aug2021, ia_CCF_age_length_21Aug2021
#' 
#' IN:in_reservoir_age_fish_16Aug2022
#' 
#' IL: il_aged_fish_surveys_28Dec2022
#' 
#' NE: NE_Standard_fish_19Jan2023
#' 
#' AR:ar_reservoir_age_8Oct2021, ar_reservoir_age2_8Oct2021
#' 
#' KS: KS_Standard_fish_19Jan2023
#' 
#' ON: ON_Standard_fish_19Jan2023
#' 
#' 

#list all files
list.files(path = "E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data", recursive = T , full.names = T, pattern = ".+\\.csv")

#count how many have my age-length name of interest?
sum(str_detect(list.files(path = "E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data", 
                          recursive = T , full.names = T, pattern = ".+\\.csv"),
               pattern =
                 "wi_inland_lenage_19Mar2021|mn_aged_fish_29Sep2021|mn_aged_fish_1_29Sep2021|mn_aged_fish_2_29Sep2021|mi_statustrends_lenage_20May2021|sd_sauger_saugeye_age_growth_03Nov22|sd_length_age_4Oct2021|sd_NOP_age_growth_3Nov2022|ia_age_length_21Aug2021|ia_BLG_age_length_21Aug2021|ia_CCF_age_length_21Aug2021|in_reservoir_age_fish_16Aug2022|il_aged_fish_surveys_28Dec2022|NE_Standard_fish_19Jan2023|KS_Standard_fish_19Jan2023|ON_Standard_fish_19Jan2023|ar_reservoir_age_8Oct2021|ar_reservoir_age2_8Oct2021"  ))

#list growth file paths
growth_files <- list.files(path = "E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data", 
           recursive = T , full.names = T, pattern = ".+\\.csv")[
             str_detect(list.files(path = "E:/Shared drives/Hansen Lab/RESEARCH PROJECTS/Fish Survey Data", 
                          recursive = T , full.names = T, pattern = ".+\\.csv"),
               pattern =
                 "wi_inland_lenage_19Mar2021|mn_aged_fish_29Sep2021|mn_aged_fish_1_29Sep2021|mn_aged_fish_2_29Sep2021|mi_statustrends_lenage_20May2021|sd_sauger_saugeye_age_growth_03Nov22|sd_length_age_4Oct2021|sd_NOP_age_growth_3Nov2022|ia_age_length_21Aug2021|ia_BLG_age_length_21Aug2021|ia_CCF_age_length_21Aug2021|in_reservoir_age_fish_16Aug2022|il_aged_fish_surveys_28Dec2022|NE_Standard_fish_19Jan2023|KS_Standard_fish_19Jan2023|ON_Standard_fish_19Jan2023|ar_reservoir_age_8Oct2021|ar_reservoir_age2_8Oct2021")]


n <- length(growth_files)

nobs <- 0

for(i in 1:n) {
  #i = 3
  filei <- word(gsub(".csv","", growth_files[i]), start = -1, sep = fixed("/"))
  #this does those two steps in one package
  assign(filei ,
         data.table(read_csv_arrow(growth_files[i])))
  
  # note we want to review a sorted list of column names to check misspelling etc.
  # we still need to use the columns with names like col_name_length_in, or known_units
  
  
  cde %>% # call data explainer file
    filter(`new_file_name`== filei)%>% #keep only the row relevant to this file
    select_if(~ !any(is.na(.))) %>% 
    transpose(keep.names = "newname") %>% 
    rename("oldname" = V1) %>% 
    assign("names", ., envir = .GlobalEnv)
  
  #see if any column names will not have a match! 
  # IF any pop FALSE, force stop and revist of data explainer ()
  # - e.g., named something "total catch" when actual column name was "total_catch"
  print(
    cbind(colnames(get(filei)),
          colnames(get(filei)) %in% names[ !str_detect(newname,"unique_row_key"), oldname, ]
    )
  )
  
  # break the loop if the current file has column names not in the data explainer
  if (all(cbind(colnames(get(filei)),  colnames(get(filei)) %in% names[ !str_detect(newname,"unique_row_key"), oldname, ])[,2]) == FALSE ) break
  
  # append old col names into new "notes" columns:
  get(filei)[ , (names[ str_detect(newname, "notes") , oldname   ,  ]) := Map(paste, colnames(.SD), .SD, sep = ':') , .SDcols =  names[ str_detect(newname, "notes") , oldname   ,  ] ]
  
  
  
  
  
  #now rename that file's colnames
  setnames(get(filei), colnames(get(filei)), names[!str_detect(newname,"unique_row_key")] [match(names(get(filei)),names[!str_detect(newname,"unique_row_key"),oldname]), newname] )
  
  #append all other data from data explainer
  unusedbits <- 
    data.table(
      matrix(
        rep(names[ !newname %in% colnames(get(filei)) , oldname , ],
            each = nrow(get(filei))
        ),
        nrow = nrow(get(filei)),
        dimnames = list(rep(NA,nrow(get(filei))),
                        names[ !newname %in% colnames(get(filei)) , newname , ])
        )
      )
  
  #add all not yet used columns from data explainer:
  get(filei)[ , (names[ !newname %in% colnames(get(filei)) , newname , ]) := unusedbits[] ]
  
  #purge any cols full of NAs
  assign(filei,
         get(filei)[ , .SD , .SDcols = colnames(get(filei))[unlist(get(filei)[ , lapply(.SD,function(x) !all(is.na(x))), ])] ,  ]
  )

  #confirm import of files:  
  print(paste(filei ,"added to workspace" ))  
  
  #count rows added:
  nobs <- nobs + nrow(get(filei))
  
  print(nobs)
  
}  

