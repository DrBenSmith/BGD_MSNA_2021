# MSNA 2021 - Daily Quantitative Data Checking Script
# July 2021
# Sajnin Tasnin | Ben Smith

# This script sets the paths to the different datasets that will be quality checked.
# IF THERE IS AN ERROR in the script, CHECK these paths, they are sneaky.

if(population == "host"){
  # >>> Change:
  data_file_name = "Host_JMSNA_2021_pilot_-_latest_version_-_False_-_2021-07-08-16-41-47 - pilot.xlsx"
  audit_name <- "aj8skQ5bTMgMgm9n62PFaD_2021_07_08_16_43_40"
  kobo_tool <- "../../01 Design/3_Tools/Quant/Kobo/Host_JMSNA_2021_FINAL_TRAINING.xlsx"

  # Do not change:
  data_path = paste0("../../02 Data Collection and Planning/Quant/4_Data_collection/Host/", data_file_name)
  hh_path <- "../02_inputs/host/01_raw_data/hh.csv"
  indv_path <- "../02_inputs/host/01_raw_data/indv.csv"
  indv_dis_repeat_path <- "../02_inputs/host/01_raw_data/indv2.csv"
  date_log_path <- "../05_outputs/01_daily_monitoring/host/date_log/"
  cleaning_log_path <- "../05_outputs/03_cleaning_log/host/"
  audit_zip_dir <- "../../02 Data Collection and Planning/Quant/4_Data_collection/Host/"
  copy_zip_to <- paste0("../05_outputs/01_daily_monitoring/host/audit/", day_to_run,".zip")
}


if(population == "refugee"){
  # >>> Change:
  data_file_name <- "Camp_JMSNA_2021_pilot_-_latest_version_-_False_-_2021-07-08-15-42-31-P.xlsx"
  audit_name <- "a4BDGkbBxy94YDMCMuGKx2_2021_07_08_15_43_06"
  kobo_tool <- "../../01 Design/3_Tools/Quant/Kobo/Camp_JMSNA_2021_FINAL_TRAINING.xlsx"

  # Do not change:
  data_path = paste0("../../02 Data Collection and Planning/Quant/4_Data_collection/Camps/0_Pilot/", data_file_name)
  hh_path <- "../02_inputs/refugee/01_raw_data/hh.csv"
  indv_path <- "../02_inputs/refugee/01_raw_data/indv.csv"
  indv_dis_repeat_path <- "../02_inputs/refugee/01_raw_data/indv2.csv"
  date_log_path <- "../05_outputs/01_daily_monitoring/refugee/date_log/"
  cleaning_log_path <- "../05_outputs/03_cleaning_log/refugee/"
  audit_zip_dir <- "../../02 Data Collection and Planning/Quant/4_Data_collection/Camps/0_Pilot/" # "../02_inputs/refugee/02_audit/"
  copy_zip_to <- paste0("../05_outputs/01_daily_monitoring/refugee/audit/", day_to_run,".zip")
}

path_unzip <- "../06_others/audit_temp"
audit_zipfile <- paste0(audit_zip_dir, audit_name, ".zip") #  "_", str_replace_all(day_to_run, "-", "_"),
audit_node <- paste0("/", unlist(strsplit(audit_name, "_"))[1], "/")
