# MSNA 2021 - Daily Quantitative Data Checking Script
# July 2021
# Sajnin Tasnin

# This script is used to call the DailyMonitoringReport.Rmd document and produce the daily data quality checking document.
# Edit the [index] numbers first two lines of the code below to produce the document.
# The DailyMonitoringReport.Rmd will use the active_path.R file to find the data to be analysed. Make sure that you are analysing the right data.

# rm(list = ls())
# library(rmarkdown)

population<-c("host","refugee")[1]
write_csv_output<-c("yes","no")[2]
day_to_run <- Sys.Date()#-1

if(population == "host"){
  # title <- "MSNA 2020 (Host Community)"
  render(input = "02_DailyMonitoringReport.Rmd",
  output_file = paste0("../03_Daily_Monitoring_Report/host/Daily Monitoring Report - population - ", gsub("-","_", day_to_run), ".html"))
# getwd()
  # copy_to_folder<- "03_Daily_Monitoring_Report/host/"
  # renamed_copy_to_folder <- paste0("Daily Monitoring Report - population - ", gsub("-","_", day_to_run), ".html")
  # copy_frm <- "02_DailyMonitoringReport.html"
  # file.copy(from = copy_frm,to = renamed_copy_to_folder, overwrite = T)
}

if(population == "refugee"){
  title <- "MSNA 2020 (Refugee)"
  render("02_DailyMonitoringReport.Rmd")
  copy_to_folder<- "03_Daily_Monitoring_Report/refugee/"
  renamed_copy_to_folder <- paste0(copy_to_folder,str_replace_all(day_to_run,"-","_"),"_Daily Monitoring Report.html")
  copy_frm <- "02_DailyMonitoringReport.html"
  file.copy(from = copy_frm,to = renamed_copy_to_folder,overwrite = T)
}
