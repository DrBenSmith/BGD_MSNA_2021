# Load Audit - Function Script
# Author: Mehedi?
# 2021

# This script reads the audit files from the surveys. These contain the time (in milli seconds) that the questions were completed. This script unzips the audit files, extracts the data and then returns audit data that corresponds to the desired surveys being checked. The main function this is being used for is to calculate the durations of surveys so that they can be checked for potential errors/issues.

# On of the potential issues is that the audit files can come with very large filepaths that are too long to be unzipped into the folders that they are already in. They may therefore need to be unzipped into higher folders and then deleted (deletion is contained in the function).

# ERROR - one of the problems may be that the zip file may have subfolders with different names (e.g. msna_test_account or msna_2021
Load_Audit <- function(data,
                       path.to.zip,
                       path.to.unzip,
                       copy.zip=TRUE,
                       path.to.copy.zip,
                       filter.column = "informed_consent",
                       filter.on = "yes",
                       uuid.column ="X_uuid",
                       delete.unzipped = TRUE){ # days.ago.reported = 0

  # setwd("C:/Users/Ben SMITH/SynDrive/REACH_BGD/REACH/Ongoing/70XXX - J-MSNA 2021/04 Data Analysis/04_scripts")
  # getwd()
  # If desired, copy the zip file to a new location:
  if(copy.zip==TRUE){file.copy(from = path.to.zip, to = path.to.copy.zip, overwrite = TRUE)}

  # Unzip the files to a temporary location:
  unzip(path.to.zip, exdir = path.to.unzip, overwrite = TRUE)

  # Get the Uuids and directory names from the audit file/s:
  all_uuid_df <- data.frame(all_uuids = basename(dirname(list.files(path_unzip, recursive=TRUE))),
                            all_paths = dirname(list.files(path_unzip, recursive=TRUE, full.names = TRUE)))

  # Create a column in the dataset that is a copy of the desired variable (probably informed consent, returning surveys with consent):
  data$filter.col <- data[[filter.column]]

  # Find the uuids that are in the audit:
  filtered_uuid_df <- all_uuid_df[all_uuid_df$all_uuids %in% data[data$filter.col == filter.on, uuid.column],]

  # Get the audit paths of the uuids of interest:
  filtered_audit_dirs <- filtered_uuid_df[,"all_paths"] %>% as.character()

  # Get the paths to the csvs we are interested in:
  filtered_audit_csvs <- paste0(filtered_audit_dirs, "/audit.csv") #list.files(filtered_audit_dirs, recursive = TRUE, full.names=TRUE)

  # Read in the csv data as a list (using map)
  data <- filtered_audit_csvs %>% purrr::map(function(x) readr::read_csv(x))
        #( file = filtered_audit_csvs)) # [BS: read_csv is faster than read.csv]

  # Name the list of audits:
  names(data) <- filtered_uuid_df$all_uuids

  if(delete.unzipped==TRUE){
    delete_dir <- list.files(path_unzip, full.names = TRUE)
    unlink(delete_dir, recursive=TRUE)
  }

  return(data)
}


