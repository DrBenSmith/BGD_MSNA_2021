# MSNA 2021 - Daily Quantitative Data Checking Script
# July 2021
# Sajnin Tasnin

# This script sets the paths to the different datasets that will be quality checked.

if(population == "host"){
  hh_path <-      "02_inputs/host/01_raw_data/hh.csv"
  indv_path <-    "02_inputs/host/01_raw_data/indv.csv"
  indv2_path <-   "02_inputs/host/01_raw_data/indv2.csv"
  date_log_path <- "05_outputs/01_daily_monitoring/host/date_log/"
  path_unzip <-   "06_others/audit_temp"
  audit_zip_dir <-"02_inputs/host/02_audit/"
  audit_zipfile <-paste0(audit_zip_dir,"afeQC986YsaAoXM6wHSFLy_", str_replace_all(day_to_run, "-", "_"), ".zip")
  copy_zip_to <-  paste0("05_outputs/01_daily_monitoring/host/audit/", day_to_run,".zip")
  audit_node <-   "/afeQC986YsaAoXM6wHSFLy/"
  cleaning_log_path <- "05_outputs/03_cleaning_log/host/"
}

if(population == "refugee"){
  hh_path <- "02_inputs/refugee/01_raw_data/hh.csv"
  indv_path <- "02_inputs/refugee/01_raw_data/indv.csv"
  indv2_path <- "02_inputs/refugee/01_raw_data/indv2.csv"
  date_log_path <- "05_outputs/01_daily_monitoring/refugee/date_log/"
  path_unzip <- "06_others/audit_temp"
  audit_zip_dir <-"02_inputs/refugee/02_audit/"
  audit_zipfile <- paste0(audit_zip_dir, "aFfG7HjodP7ynJXK7vhbCB_", str_replace_all(day_to_run, "-", "_"), ".zip")
  copy_zip_to <-paste0("05_outputs/01_daily_monitoring/refugee/audit/", day_to_run,".zip")
  audit_node <-"/aFfG7HjodP7ynJXK7vhbCB/"
  cleaning_log_path <- "05_outputs/03_cleaning_log/refugee/"
}
