

#' # Dataset Updater for CASC Fish Growth

#' You'll need to have access to the google drive folder where these data are 
#' stored before running this script. 
#' 
#' Currently the growth proj does not use this approach, GDrive Desktop is used
#' to directly load from the shared drive.
#' 
#' Use this to download local copies of the files if you can't go the GDrive desktop route.
#'  Or go here and download them manually: https://drive.google.com/drive/folders/1Un2GGcO-Fv1ONJ-IRNnInZrGODqgaV2O?usp=share_link
#' 
#' The code is written to populate Mike's data folder structure within an R proj:
#' "scripts&data/data/input/[file_being_saved.csv]"

  # load library
  library(googledrive)
  library(purrr)
  library(data.table)

  #drive authorization request

  # link to google drive & force new token auth (can change to 1 to use existing account)
  drive_auth(email = FALSE)


  # store folder url as a file ID
  # identify this folder on Drive
  # let googledrive know this is a file ID or URL, as opposed to file name
    infolder <- drive_get(as_id("https://drive.google.com/drive/u/1/folders/1Un2GGcO-Fv1ONJ-IRNnInZrGODqgaV2O"))
    # outfolder <- drive_get(as_id("https://drive.google.com/open?id=18s1G78WA9Hu4_6k_fIPMROV9DVo-dI_5"))


  # identify the csv files in each folder
  input_files <- data.table(drive_ls(infolder))
  # output_files <- data.table(drive_ls(outfolder))


  # download them, overwriting your currrent project data folder:
  walk(input_files[str_detect(name, ".csv")|str_detect(name, ".txt")]$id, ~ drive_download(as_id(.x), path = paste("scripts&data/data/input", input_files[id==.x, name], sep = "/"), overwrite = T ))
  # walk(output_files$id, ~ drive_download(as_id(.x), path = paste("data/output", output_files[id==.x, name], sep = "/"), overwrite = T ))



  