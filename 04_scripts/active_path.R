# MSNA 2021 - Daily Quantitative Data Checking Script
# July 2021
# Sajnin Tasnin | Ben Smith

# This script sets the paths to the different datasets that will be quality checked.

if(population == "host"){
  # Change
  data_path = "../../02 Data Collection and Planning/Quant/2_Data_cleaning/SOPs/dummy5/Host_JMSNA_2021_20210704_-_all_versions_-_False_-_2021-07-03-11-35-36.xlsx"
  audit_name <- "av8MhwimnDqpjbueBVYpKu_2021_07_03_07_36_10"

  # Do not change:
  hh_path <- "../02_inputs/host/01_raw_data/hh.csv"
  indv_path <- "../02_inputs/host/01_raw_data/indv.csv"
  indv_dis_repeat_path <- "../02_inputs/host/01_raw_data/indv2.csv"
  date_log_path <- "../05_outputs/01_daily_monitoring/host/date_log/"
  cleaning_log_path <- "../05_outputs/03_cleaning_log/host/"
  # Audit paths:
  path_unzip <- "../06_others/audit_temp"
  audit_zip_dir <- "../../02 Data Collection and Planning/Quant/2_Data_cleaning/SOPs/dummy5/"
  audit_zipfile <- paste0(audit_zip_dir, audit_name, ".zip") #  "_", str_replace_all(day_to_run, "-", "_"),
  # copy_zip_to <- "../../TEMP.zip" # this is so that the name is shorter - it will be deleted imediately #
  copy_zip_to <- paste0("../05_outputs/01_daily_monitoring/host/audit/", day_to_run,".zip")
  audit_node <- paste0("/", unlist(strsplit(audit_name, "_"))[1], "/")
}

if(population == "refugee"){ # UPDATE
  hh_path <- "../02_inputs/refugee/01_raw_data/hh.csv"
  indv_path <- "../02_inputs/refugee/01_raw_data/indv.csv"
  indv_dis_repeat_path <- "../02_inputs/refugee/01_raw_data/indv2.csv"
  date_log_path <- "../05_outputs/01_daily_monitoring/refugee/date_log/"
  path_unzip <- "../06_others/audit_temp"
  audit_zip_dir <-"../02_inputs/refugee/02_audit/"
  audit_zipfile <- paste0(audit_zip_dir, "aFfG7HjodP7ynJXK7vhbCB_", str_replace_all(day_to_run, "-", "_"), ".zip")
  copy_zip_to <-paste0("../05_outputs/01_daily_monitoring/refugee/audit/", day_to_run,".zip")
  audit_node <-"/aFfG7HjodP7ynJXK7vhbCB/"
  cleaning_log_path <- "../05_outputs/03_cleaning_log/refugee/"
}
