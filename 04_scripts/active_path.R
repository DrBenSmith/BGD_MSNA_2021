# MSNA 2021 - Daily Quantitative Data Checking Script
# July 2021
# Sajnin Tasnin | Ben Smith

# This script sets the paths to the different datasets that will be quality checked.

if(population == "host"){
  # >>> Change:
  data_path = "../../02 Data Collection and Planning/Quant/2_Data_cleaning/SOPs/dummy6/Host_JMSNA_2021_pilot_-_latest_version_-_english_-_2021-07-07-12-45-09.xlsx"
  audit_name <- "aj8skQ5bTMgMgm9n62PFaD_2021_07_07_12_45_45"
  kobo_tool <- "../../01 Design/3_Tools/Quant/Kobo/Host_JMSNA_2021_FINAL_TRAINING.xlsx"

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


if(population == "refugee"){
  # >>> Change:
  data_path <- "../../02 Data Collection and Planning/Quant/2_Data_cleaning/SOPs/dummy6/Camp_JMSNA_2021_pilot_-_latest_version_-_False_-_2021-07-07-07-33-42.xlsx"
  audit_name <- "a4BDGkbBxy94YDMCMuGKx2_2021_07_07_07_35_08"
  kobo_tool <- "../../01 Design/3_Tools/Quant/Kobo/Camp_JMSNA_2021_FINAL_TRAINING.xlsx"

  # Do not change:
    hh_path <- "../02_inputs/refugee/01_raw_data/hh.csv"
    indv_path <- "../02_inputs/refugee/01_raw_data/indv.csv"
    indv_dis_repeat_path <- "../02_inputs/refugee/01_raw_data/indv2.csv"
    date_log_path <- "../05_outputs/01_daily_monitoring/refugee/date_log/"
    cleaning_log_path <- "../05_outputs/03_cleaning_log/refugee/"
    # Audit paths:
    path_unzip <- "../06_others/audit_temp"
    audit_zip_dir <- "../../02 Data Collection and Planning/Quant/2_Data_cleaning/SOPs/dummy6/" # "../02_inputs/refugee/02_audit/"
    audit_zipfile <- paste0(audit_zip_dir, audit_name, ".zip") #  "_", str_replace_all(day_to_run, "-", "_"),
    # copy_zip_to <- "../../TEMP.zip" # this is so that the name is shorter - it will be deleted imediately #
    copy_zip_to <- paste0("../05_outputs/01_daily_monitoring/refugee/audit/", day_to_run,".zip")
    audit_node <- paste0("/", unlist(strsplit(audit_name, "_"))[1], "/")
}
