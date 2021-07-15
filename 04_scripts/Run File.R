# -------------------------------------------------------------------------
# MSNA 2021 - Daily Quantitative Data Checking Script
# July 2021
# Sajnin Tasnin & Ben Smith
# -------------------------------------------------------------------------

# This script is used to call the Daily Monitoring Report Builder.Rmd
# This will produce the daily data quality checking document.

# EDIT the following:
#   > population [index]
#   > data_file_name
#   > audit_name

# CHECK the following:
#   > data_path
#   > audit_zip_dir

# ERRORS in this script are probably due to incorrect paths below! Check these, as they are sneaky.


# Data Paths (CHANGE) -----------------------------------------------------

population      <- c("host","refugee")[1]
data_file_name  <- "Host_JMSNA_2021_-_latest_version_-_False_-_2021-07-15-13-04-42.xlsx"
audit_name      <- "aqwujmBVgmCsd2wJW98WWW_2021_07_15_13_05_28"
# data_file_name  <- "Camp_JMSNA_2021_-_latest_version_-_False_-_2021-07-15-12-31-51.xlsx"
# audit_name      <- "a5C6hZmgWjP2NcTwt5GAb8_2021_07_15_12_33_07"

data_path <- audit_zip_dir <- paste0("../02 Data Collection and Planning/Quant/4_Data_collection/", population, "/Day4/")
# audit_zip_dir <- paste0("../02 Data Collection and Planning/Quant/4_Data_collection/", population, "/Day3/")


# Setup -------------------------------------------------------------------

# rm(list = ls())
library(rmarkdown)
library(knitr)
day_to_run <- Sys.Date()
write_csv_output <-c("yes","no")[1]


# Sample Data (change if not working on Ben's PC) -------------------------

if(population=="refugee"){
  kobo_tool <- "../01 Design/3_Tools/Quant/Kobo/Camp_JMSNA_2021.xlsx"
  sample_sheet_path = "C:/Users/Ben SMITH/Documents/Bangladesh/REACH Projects/MSNA 2021/JMSNA_sample_for_verification_camps.xlsx"
}
if(population=="host"){
  kobo_tool <- "../01 Design/3_Tools/Quant/Kobo/Host_JMSNA_2021.xlsx"
  sample_sheet_path = "C:/Users/Ben SMITH/Documents/Bangladesh/REACH Projects/MSNA 2021/JMSNA_Host_Sample_for_verification.xlsx"
}


# Output paths (should not need to change) --------------------------------

hh_path <- paste0("02_inputs/", population, "/01_raw_data/hh.csv")
indv_path <- paste0("02_inputs/", population, "/01_raw_data/indv.csv")
indv_dis_repeat_path <- paste0("02_inputs/", population, "/01_raw_data/indv2.csv")

date_log_path <- paste0("05_outputs/01_daily_monitoring/", population, "/date_log/")
cleaning_log_path <- paste0("05_outputs/03_cleaning_log/", population, "/")

copy_zip_to <- paste0("05_outputs/01_daily_monitoring/", population, "/audit/", day_to_run,".zip")
path_unzip <- "06_others/audit_temp"
audit_zipfile <- paste0(audit_zip_dir, audit_name, ".zip")
audit_node <- paste0("/", unlist(strsplit(audit_name, "_"))[1], "/")


# Run the Checking Script -------------------------------------------------

render(input = "04_Scripts/Daily Monitoring Report Builder.Rmd",
       output_dir = paste0("05_outputs/01_daily_monitoring/", population, "/Reports/"),
       output_file = paste0("Daily Monitoring Report - ", population,  " - ", gsub("-","_", day_to_run), ".html"))

