# Utility Functions

# This script contains several functions that are useful for the BGD data analysis.

# rm(list = ls())
library(stringr)
library(gsubfn)
library(readxl)
library(openxlsx)
library(dplyr)
# library(koboquest) # install.packages("koboquest")


# read_all_csv ------------------------------------------------------------

read_all_sheet_as_csv_format <- function(xlsx_file_path){
  sheet_name <- excel_sheets(xlsx_file_path)
  df_all <- list()
  for (i in sheet_name) {
    assign(i,read_xlsx("02_inputs/01_raw_data/BGD2101_rtata.xlsx",sheet = i))
    df <- get(i)
    colnames(df) <- colnames(df) %>% str_replace_all("/",".")
    df_st_with <- df %>% select(starts_with("_")) %>% names()
    df <- df %>% rename_at(df_st_with,~paste0("X",.))
    df_all[[i]] <- df
    assign(i,df)
  }
  list2env(df_all,.GlobalEnv)
}


# Outliers ----------------------------------------------------------------


outlier_check <- function(df,
                          kobo_tool_location=NULL,
                          cols_to_report = NULL){

  # Find the columns to check (using the kobo tool)
  if(!is.null(kobo_tool_location)) {

    survey_sheet <- read.xlsx(kobo_tool_location, sheet = "survey")
    survey_sheet$name <- survey_sheet$name %>% str_replace_all("-",".")


    interger_column_in_kobo <- (survey_sheet %>% filter(type == "integer") %>%
                                  filter(!grepl('enumerator|_instance_', name)))$name

    cols_name_exist_in_loop_kobo <- interger_column_in_kobo[interger_column_in_kobo %in% names(df)]

  }

  cols_name_exist_in_loop_numeric <- df %>% select_if(is.numeric) %>% select(-starts_with("X"))%>% names()
  cols_name_exist_in_loop_int <- df %>% select_if(is.integer) %>% select(-starts_with("X"))%>% names()

  if(!is.null(kobo_tool_location)) {
    cols_name_exist_in_loop <- c(cols_name_exist_in_loop_kobo,
                                 cols_name_exist_in_loop_numeric,
                                 cols_name_exist_in_loop_int) %>% unique()
  }


  if(is.null(kobo_tool_location)) {
    cols_name_exist_in_loop <- c(cols_name_exist_in_loop_numeric,
                                 cols_name_exist_in_loop_int) %>% unique()
  }

  outlier_checks <- list()

  for (x in cols_name_exist_in_loop) {

    print(paste0("checking_", x))

    df[[x]] <- df[[x]] %>% as.numeric()

    outlier_checks[[x]] <-  df %>% mutate(
      issue = case_when(df[[x]] %in% boxplot.stats(aa)$out~"outlier"), # I think that there may be an issue with the aa here. It is throwing errors with the MSNA analysis. it also seems odd that the %in% is there... There may also be an issue that the stats use the input data (the monitoring period in the origional MSNA monitoring script), rather than the whole data.
    ) %>% filter(issue == "outlier") %>% select(cols_to_report,issue,x) %>%
      pivot_longer(cols = paste0(x),names_to ="questions",values_to= "old_value")
  }

  outliers_cl <- do.call("bind_rows",outlier_checks)

  return(outliers_cl)
}



# other_response ----------------------------------------------------------

general_checks <- function(df,
                           date_col=NULL,
                           enumerator_id=NULL,
                           strata =NULL,
                           consent_col = NULL){

  survey_summary<- list()

  if(!is.null(date_col)){
    survey_summary[[summary_by_date]] <- df %>% group_by(!!date_col,consent_col) %>% summarise(
      number_of_survey = n())
  }

  by_strata <- c(strata,consent_col)

  survey_summary[[summary_by_stata]] <- df %>% group_by(!!by_strata) %>% summarise(number_of_survey = n())

  by_enu <- c(enumerator_id,consent_col)

  survey_summary[[summary_by_enumerator]] <- df %>% group_by(!!by_enu) %>% summarise(number_of_survey = n())

  return(list2env(survey_summary,.GlobalEnv))
}

# general_checks(df = df, enumerator_id = "enumerator_id", strata = "camps", consent_col ="consent" )


