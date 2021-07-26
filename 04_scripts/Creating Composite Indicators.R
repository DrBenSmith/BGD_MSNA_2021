# MSNA 2021
# Adding composite variables
# 26/07/2021
# Ben Smith | Cara Kielwein

# Preamble -----------------------------------------------------------------

# Load in libraries
library(tidyr)
library(dplyr)
library(readxl)
library(openxlsx)

setwd("C:/Users/Ben SMITH/SynDrive/REACH_BGD/REACH/Ongoing/70XXX - J-MSNA 2021/04 Data Analysis/") # BEN
# setwd("C:/Users/Cara.../REACH_BGD/REACH/Ongoing/70XXX - J-MSNA 2021/04 Data Analysis/04_scripts") # CARA

# Set the population of the dataset you are working with:
pop <- c("host", "refugee")[2]

# Input path (should not have to change often):
data_path <- paste0("../02 Data Collection and Planning/Quant/4_Data_collection/", pop, "/Day7/")

# Input Data (you will need to change this when you switch between datasets):
data_file_name <- "Camp_JMSNA_2021_-_latest_version_-_False_-_2021-07-25-13-17-33.xlsx"

# Output paths:
hh_path <- paste0("02_inputs/", pop, "/01_raw_data/hh.csv")
indv_path <- paste0("02_inputs/", pop, "/01_raw_data/indv.csv")
indv_dis_repeat_path <- paste0("02_inputs/", pop, "/01_raw_data/indv2.csv")

# Convert the data from xlsx to csv's:
# This is because it is read into R better as csv's, but we do not want to do this conversion manually.
# This will overwrite the previous csv's.

# hh
data_sheet = read_xlsx(paste0(data_path, data_file_name), sheet = 1); write.csv(x = data_sheet, file = hh_path, row.names = FALSE)
# indv
data_sheet = read_xlsx(paste0(data_path, data_file_name), sheet = 2); write.csv(x = data_sheet, file = indv_path, row.names = FALSE)
# indv_disability_repeat
data_sheet = read_xlsx(paste0(data_path, data_file_name), sheet = 3); write.csv(x = data_sheet, file = indv_dis_repeat_path, row.names = FALSE)

# load data from csv's:
hh <- read.csv(hh_path, stringsAsFactors = FALSE, na.strings=c("", " ", NA))
indv <- read.csv(indv_path, stringsAsFactors = FALSE, na.strings=c("", " ", NA))
indv_dis_repeat <- read.csv(indv_dis_repeat_path, stringsAsFactors = FALSE, na.strings=c("", " ", NA))

# Change some of the column names in the individual datasets:
  # 1. X_submission__uuid in indv
  names(indv) <- gsub(x =  names(indv), pattern = "X_submission__uuid", replacement = "X_uuid")
  names(indv) <- gsub(x =  names(indv), pattern = "_submission__uuid", replacement = "X_uuid") # Should be the above, but this covers whether the X is lost or not
  # 2. X_submission__uuid in indv_dis_repeat
  names(indv_dis_repeat) <- gsub(x = names(indv_dis_repeat), pattern = "X_submission__uuid", replacement = "X_uuid",)
  names(indv_dis_repeat) <- gsub(x = names(indv_dis_repeat), pattern = "_submission__uuid", replacement = "X_uuid",)
  # 3. ind_number_dis in indv_dis_repeat
  names(indv_dis_repeat) <- gsub(x = names(indv_dis_repeat), pattern = "ind_number_dis", replacement = "ind_number")

# Filter the data to only consenting households:
  # hh_consent  <- hh   %>% filter(informed_consent =="yes")
  # indv_data   <- indv %>% filter(X_uuid %in% hh_consent$X_uuid)
  # indv_dis_repeat_data <- indv_dis_repeat %>% filter(X_uuid %in% hh_consent$X_uuid)

# Household/individual information -----------------------------------------

# --- all_consenting ---  [For analysis]
# [From hh to hh. All HH
# hh_dataset$i.all_consenting	 <- hh_dataset ...	if (informed_consent == 'yes', 1, 0)
hh <- hh %>% mutate(i.all_consenting = case_when(informed_consent=="yes" ~ 1, TRUE~0))

# --- hoh_gender ---
# [From hh to hh. Head of household gender
# hh_dataset$i.hoh_gender	 <- hh_dataset ...	if (respondent_hoh == 'yes', resp_gender, hoh_gender)
hh$i.hoh_gender <- case_when((hh$respondent_hoh=="yes" &  hh$resp_gender==hh$hoh_gender)~ 1, TRUE~0) # TRUE does NOT mean true, it means 'if it exists'
  # Check using: head(hh[, c("respondent_hoh", "resp_gender", "hoh_gender", "i.hoh_gender")], 40)
  # Check using: head(hh %>% select(respondent_hoh, resp_gender, hoh_gender, i.hoh_gender) %>% filter(i.hoh_gender==1), 40)
  # Check using: View(hh %>% select(respondent_hoh, resp_gender, hoh_gender, i.hoh_gender))

# --- hoh_age ---
# [From hh to hh. Head of household age
# hh_dataset$i.hoh_age	 <- hh_dataset ...	if (respondent_hoh == 'yes', respondent_age, hoh_age)
hh$i.hoh_age = rep(0, nrow(hh))
hh$i.hoh_age[which(hh$respondent_hoh=="yes" &  hh$respondent_age==hh$hoh_age)] = 1

# --- demographics ---
# [From indv to indv. Sample demographics
# indv_dataset$i.demographics	 <- indv_dataset ...	Categorize by gender and age group (0-4, 5-11, 12-17, 18-24, 25-29, 60+), i.e. if (individual_age < 5 & ind_gender == 'female', female_0_4, if (individual_age < 12 & individual_age> 4 & ind_gender == 'female', female_5_11) … if (individual_age >= 60 & ind_gender == 'male', male_60))

# --- ind_0_17 ---
# [From indv to indv. Individuals aged 0-17
# indv_dataset$i.ind_0_17	 <- indv_dataset ...	if (individual_age < 18, 1, 0)

# --- ind_18_59 ---
# [From indv to indv. Individuals aged 18-59
# indv_dataset$i.ind_18_59	 <- indv_dataset ...	if (individual_age < 60 & individual_age > 17, 1, 0)

# --- ind_60 ---
# [From indv to indv. Individuals aged 60+
# indv_dataset$i.ind_60	 <- indv_dataset ...	if (individual_age >= 60, 1, 0)

# --- ind_females ---
# [From indv to indv. Females
# indv_dataset$i.ind_females	 <- indv_dataset ...	if (ind_gender == 'female', 1, 0)

# --- ind_males ---
# [From indv to indv. Males
# indv_dataset$i.ind_males	 <- indv_dataset ...	if (ind_gender == 'male', 1, 0)

# --- disability ---
# [From indv to indv. Persons with disabilities
# indv_dataset$i.disability	 <- indv_dataset ...	if (at least one of the following is 'a_lot_of_difficulty' or 'some_difficulty' - indv_difficulty_seeing, indv_difficulty_hearing, indv_difficulty_walking, indv_difficulty_remembering, indv_difficulty_selfcare, indv_difficulty_communicating, 1, 0)

# --- disability_males ---
# [From indv to indv. Male persons with disabilities
# indv_dataset$i.disability_males	 <- indv_dataset ...	if (i.disability == 1 & ind_gender = 'male', 1, 0)

# --- disability_females ---
# [From indv to indv. Female persons with disabilities
# indv_dataset$i.disability_females	 <- indv_dataset ...	if (i.disability == 1 & ind_gender = 'female', 1, 0)

# --- disability_5_17 ---
# [From indv to indv. Persons with disabilities aged 5-17
# indv_dataset$i.disability_5_17	 <- indv_dataset ...	if (i.disability == 1 & individual_age >= 5 & individual_age <= 17, 1, 0)

# --- disability_18_59 ---
# [From indv to indv. Persons with disabilities aged 18-59
# indv_dataset$i.disability_18_59	 <- indv_dataset ...	if (i.disability == 1 & individual_age >= 18 & individual_age <= 59, 1, 0)

# --- disability_60 ---
# [From indv to indv. Persons with disabilities aged 60+
# indv_dataset$i.disability_60	 <- indv_dataset ...	if (i.disability == 1 & individual_age >= 60, 1, 0)

# --- disability_hh ---
# [From indv to hh. HH with persons with disabilities
# hh_dataset$i.disability_hh	 <- indv_dataset ...	if (i.disability == 1 for at least 1 individual in the HH, 1, 0)

# --- hh_without_adult_males ---
# [From indv to hh. HH without adult males
# hh_dataset$i.hh_without_adult_males	 <- indv_dataset ...	if no individual in the household whose ind_gender == 'male' and individual_age < 17

# --- hh_size ---
# [From hh to hh. Large HH (5+) vs small HH (<5)
# hh_dataset$i.hh_size	 <- hh_dataset ...	if (hh_size >= 5, 'large', 'small')

# --- hh_no_bangla_english ---  [refugee]
# [From hh to hh. HH not speaking neither English nor Bangla
# if(pop=="refugee"){
#    	hh_dataset$i.hh_no_bangla_english	 <- hh_dataset ...	if (hh_languages_spoken != 'bangla' & hh_languages_spoken != 'english', 1, 0)
# }

# --- arrival_bgd_reva ---  [refugee]
# [From hh to hh. Align arrival date with REVA
# if(pop=="refugee"){
#    	hh_dataset$i.arrival_bgd_reva	 <- hh_dataset ...	3 categories: Before October 2016: bef_oct_2016; October 2016 to 24 August 2017: oct_2016_aug_2017; After 24 August 2017: sep_2017_feb_2020, mar_2020_mar_2021, after_mar_2021
# }

# --- arrival_camp_reva ---  [refugee]
# [From hh to hh. Align arrival date with REVA
if(pop=="refugee"){
   	hh_dataset$i.arrival_camp_reva	 <- hh_dataset ...	3 categories: Before October 2016: bef_oct_2016; October 2016 to 24 August 2017: oct_2016_aug_2017; After 24 August 2017: sep_2017_feb_2020, mar_2020_mar_2021, after_mar_2021
}

# --- highest_edu_hh ---  [refugee]
# [From hh to hh. Aggregate highest level of education in HH into 3 categories
# if(pop=="refugee"){
#    	hh_dataset$i.highest_edu_hh	 <- hh_dataset ...	3 categories: No formal education: No education or Madrassa only (no_education, madrassa_only); Some primary: Kindergarten up to Elementary Standard 4 (kindergarten, standard_1, standard_2, standard_3, standard_4); Primary and above: Elementary Standard 5 or more (standard_5 to standard_11, tertiary_education)
# }

# --- highest_edu_hh ---  [host]
# [From hh to hh. Aggregate highest level of education in HH into 3 categories
# if(pop=="host"){
#    	hh_dataset$i.highest_edu_hh	 <- hh_dataset ...	3 categories: Primary or less: No education, Madrassah only, level 1 to 5 (no_education, madrasah_only, 1-5); Some secondary: Level 6 to 11 (6-11); Secondary and above: 12 and above (12, above_grade_12, vocational)
# }

# Shelter/NFI --------------------------------------------------------------

# --- vulnerable_shelter ---  [host]
# [From hh to hh. HH staying in vulnerable shelter types
# if(pop=="host"){
#    	hh_dataset$i.vulnerable_shelter	 <- hh_dataset ...	if (shelter_type == 'jhuprie' | shelter_type == 'kutcha', 'vulnerable', if (shelter_type == 'pucca' | shelter_type == 'semi_pucca', 'not_vulnerable', if(shelter_type == 'none_with_other_hh' | shelter_type == 'none_in_open', 'none', NA)))
# }

# --- shelter_issues_hh ---  [refugee]
# [From hh to hh. HH with at least one shelter issue
# if(pop=="refugee"){
#    	hh_dataset$i.shelter_issues_hh	 <- hh_dataset ...	if (at least one of the following == 'yes' - leaks_during_rain, limited_ventilation, dirt_debris, lack_of_insulation, collapse_living_there, collapse_with_other_hh, with_other_hh, collapse_in_open)
# }

# --- shelter_issues_hh ---  [host]
# [From hh to hh. HH with at least one shelter issue
# if(pop=="host"){
#    	hh_dataset$i.shelter_issues_hh	 <- hh_dataset ...	if (at least one of the following == 'yes' - leaks_during_rain, limited_ventilation, dirt_debris, lack_of_insulation, collapse_living_there)
# }

# --- shelter_issue_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.shelter_issue_needs	 <- hh_dataset ...	if (collapse_in_open == 'yes', 4+, if ((collapse_with_other_hh == 'yes' | with_other_hh == 'yes') & collapse_in_open == 'no' , 4, if ((collapse_living_there == 'yes' | limited_ventilation == 'yes' | leaks_during_rain == 'yes') & collapse_in_open == 'no' & collapse_with_other_hh == 'no' & with_other_hh == 'no', 3, if ((dirt_debris == 'yes' | lack_of_insulation == 'yes') & collapse_in_open == 'no' & collapse_with_other_hh == 'no' & with_other_hh == 'no' & collapse_living_there == 'no' & limied_ventilation == 'no' & leaks_during_rain == 'no' , 2, if(all 8 types of issues == 'no', 1, NA)))))

# --- improvement_hh ---
# [From hh to hh. HH having made at least one improvement
# hh_dataset$i.improvement_hh	 <- hh_dataset ...	if (improvement != 'no_improvement' & improvement != 'dont_know', 1, 0)

# --- no_improvement_despite_issues ---
# [From hh to hh. HH not having made improvements despite reporting issues
# hh_dataset$i.no_improvement_despite_issues	 <- hh_dataset ...	if (improvement == 'no_improvement' & (at least 1 of the following == 'yes' - leaks_during_rain, limited_ventilation, dirt_debris, lack_of_insulation, collapse_living_there), 1, 0)

# --- improvement_needs ---  [For analysis]
# _dataset$i.improvement_needs	 <- _dataset ...	if (improvement_reason is any of the following - did_not_receive_any_shelter_support, sold_materials, no_money_to_pay_for_materials, quality_materials_expensive, materials_unavailable, quality_materials_unavailable, no_money_to_pay_for_labor, no_able_bodied_person, how_to_improve_the_shelter, where_to_buy_materials, who_to_ask_for_support, other, 1, if (improvement_reason == 'no_need_to_improve' | i.improvement_hh == 1, 0, NA))

# --- rent_needs ---  [For analysis]
# [From hh to hh. HH having made some form of rent payment
# hh_dataset$i.rent_needs	 <- hh_dataset ...	if (shelter_paid is one of the following - 'payment_of_cash', 'payment_through_goods', 'payment_through_labor', 'not_specified', 'other', 3, if (shelter_paid == 'no_need', 1, NA))

# --- hlp_disputes ---  [host]
# [From hh to hh. HH reporting any HLP disputes
# if(pop=="host"){
#    	hh_dataset$i.hlp_disputes	 <- hh_dataset ...	if (hlp_disputes != 'no_issues' & hlp_disputes != 'dont_know', 1, 0)
# }

# --- missing_at_least_1_NFI ---
# [From hh to hh. HH not having enough of at least 1 NFI
# hh_dataset$i.missing_at_least_1_NFI	 <- hh_dataset ...	if (at least 1 of the following == 'no' - blankets, mattresses_mats, kitchen_sets, torches_lights, solar_lamps, batteries, clothing, winter_clothing, shoes, fans, mosquito_nets, bedding_items, 1, 0)

# --- nfi_needs ---  [For analysis]
# _dataset$i.nfi_needs	 <- _dataset ...	if (blankets == 'no' | (mattresses_mats == 'no' | bedding_items == 'no') | ((torches_lights == 'no' & batteries == 'no') | solar_lamps == 'no') | (clothing == 'no' | winter_clothing == 'no') | mosquito_nets == 'no', 1, if (all NFIs == 'yes' | ONLY the following == 'no' AND all others/other combinations == 'yes' - kitchen_sets, shoes, fans, 0, NA))

# --- lpg_needs ---  [For analysis]
# [From hh to hh. HH not having received LPG or with LPG not having lasted
# hh_dataset$i.lpg_needs	 <- hh_dataset ...	if (received_lpg == 'no' | lpg_duration == 'no', 1, if (received_lpg == 'yes' & lpg_duration == 'yes', 0, NA))

# --- only_lpg ---  [host]
# [From hh to hh. HH having used exclusively LPG
# if(pop=="host"){
#    	hh_dataset$i.only_lpg	 <- hh_dataset ...	if (cooking_fuel is ONLY 'receiving_lpg_refills' OR 'buying_lpg_refills', 1, 0)
# }

# --- snfi_critical ---  [For analysis]
# [From hh to hh. https://docs.google.com/document/d/1uqNKgJqFQD4lY0dfnVnPRJLMrw_6dJqSL4m29KD1D7Q/edit
# hh_dataset$i.snfi_critical	 <- hh_dataset ...	max (i.shelter_issue_needs, i.rent_needs, na.rm = T only if maximum possible is reached)

# --- snfi_non_critical_mean ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.snfi_non_critical_mean	 <- hh_dataset ...	mean (i.improvement_needs, i.nfi_needs, i.lpg_needs, na.rm = F)

# --- snfi_non_critical ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.snfi_non_critical	 <- hh_dataset ...	if (i.snfi_non_critical_mean > 2/3, 3, if (i.snfi_non_critical_mean > 1/3 & snfi_non_critical_mean <= 2/3, 2, if(i.snfi_non_critical_mean <= 1/3, 1, NA)))

# --- snfi_overall_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.snfi_overall_needs	 <- hh_dataset ...	max (i.snfi_critical, i.snfi_non_critical, na.rm = T only if maximum possible is reached)

# --- snfi_coping_shelter ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.snfi_coping_shelter	 <- hh_dataset ...	if (shelter_materials_source == 'purchased' | shelter_materials_source == 'exchanged' | coping_reason == 'shelter', 1, if (shelter_materials_source ONLY the following - provided, reused_existing_materials - AND coping_reason != 'shelter', 0, NA))

# --- snfi_coping_nfi ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.snfi_coping_nfi	 <- hh_dataset ...	if (coping_reason is any of the following - clothing, electricity, rent, cooking_fuel, hh_items, 1, if (coping_reason is ONLY any other reason AND NOT dont_know OR all coping strategies are no_need, 0, NA))

# --- snfi_coping_total ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.snfi_coping_total	 <- hh_dataset ...	max (i.snfi_coping_shelter, i.snfi_coping_nfi, na.rm = T only if one of the two is 1)

# --- snfi_need_coping ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.snfi_need_coping	 <- hh_dataset ...	if (i.snfi_overall_needs == 1 | 2 & snfi_coping_total == 1, 1, 0)

# Site management ----------------------------------------------------------

# --- mobility_issues_children_hh ---  [refugee]
# [From hh to hh. HH reporting at least 1 mobility issue for children
# if(pop=="refugee"){
#    	hh_dataset$i.mobility_issues_children_hh	 <- hh_dataset ...	if (mobility_issues_children != 'no_issue' & mobility_issues_children != 'dont_know, 1, 0)
# }

# --- mobility_issues_women_hh ---  [refugee]
# [From hh to hh. HH reporting at least 1 mobility issue for women
# if(pop=="refugee"){
#    	hh_dataset$i.mobility_issues_women_hh	 <- hh_dataset ...	if (mobility_issues_women != 'no_issue' & mobility_issues_women != 'dont_know, 1, 0)
# }

# --- mobility_issues_men_hh ---  [refugee]
# [From hh to hh. HH reporting at least 1 mobility issue for men
# if(pop=="refugee"){
#    	hh_dataset$i.mobility_issues_men_hh	 <- hh_dataset ...	if (mobility_issues_men != 'no_issue' & mobility_issues_men != 'dont_know, 1, 0)
# }

# Health -------------------------------------------------------------------

# --- needed_healthcare_0_17 ---
# [From indv to indv. 0-17 year-olds that needed treatment in the past 3 months
# indv_dataset$i.needed_healthcare_0_17	 <- indv_dataset ...	if ((needed_healthcare == 'yes_4weeks' | needed_healthcare == 'yes_3months') & individual_age < 18, 1, 0)

# --- needed_healthcare_18_59 ---
# [From indv to indv. 18-59 year-olds that needed treatment in the past 3 months
# indv_dataset$i.needed_healthcare_18_59	 <- indv_dataset ...	if ((needed_healthcare == 'yes_4weeks' | needed_healthcare == 'yes_3months') & individual_age < 60 & individual_age > 17, 1, 0)

# --- needed_healthcare_60 ---
# [From indv to indv. 60+ year-olds that needed treatment in the past 3 months
# indv_dataset$i.needed_healthcare_60	 <- indv_dataset ...	if ((needed_healthcare == 'yes_4weeks' | needed_healthcare == 'yes_3months') & individual_age >= 60, 1, 0)

# --- needed_healthcare_females ---
# [From indv to indv. Females that needed treatment in the past 3 months
# indv_dataset$i.needed_healthcare_females	 <- indv_dataset ...	if ((needed_healthcare == 'yes_4weeks' | needed_healthcare == 'yes_3months') & ind_gender == 'female', 1, 0)

# --- needed_healthcare_males ---
# [From indv to indv. Males that needed treatment in the past 3 months
# indv_dataset$i.needed_healthcare_males	 <- indv_dataset ...	if ((needed_healthcare == 'yes_4weeks' | needed_healthcare == 'yes_3months') & ind_gender == 'male', 1, 0)

# --- needed_healthcare_0_17_4weeks ---
# [From indv to indv. 0-17 year-olds that needed treatment in the past 4 months
# indv_dataset$i.needed_healthcare_0_17_4weeks	 <- indv_dataset ...	if (needed_healthcare == 'yes_4weeks' & individual_age < 18, 1, 0)

# --- needed_healthcare_18_59_4weeks ---
# [From indv to indv. 18-59 year-olds that needed treatment in the past 4 months
# indv_dataset$i.needed_healthcare_18_59_4weeks	 <- indv_dataset ...	if (needed_healthcare == 'yes_4weeks' & individual_age < 60 & individual_age > 17, 1, 0)

# --- needed_healthcare_60_4weeks ---
# [From indv to indv. 60+ year-olds that needed treatment in the past 4 months
# indv_dataset$i.needed_healthcare_60_4weeks	 <- indv_dataset ...	if (needed_healthcare == 'yes_4weeks' & individual_age >= 60, 1, 0)

# --- needed_healthcare_females_4weeks ---
# [From indv to indv. Females that needed treatment in the past 4 months
# indv_dataset$i.needed_healthcare_females_4weeks	 <- indv_dataset ...	if (needed_healthcare == 'yes_4weeks' & ind_gender == 'female', 1, 0)

# --- needed_healthcare_males_4weeks ---
# [From indv to indv. Males that needed treatment in the past 4 months
# indv_dataset$i.needed_healthcare_males_4weeks	 <- indv_dataset ...	if (needed_healthcare == 'yes_4weeks' & ind_gender == 'male', 1, 0)

# --- needed_healthcare_3months_hh ---
# [From indv to hh. Households with at least one individual that needed health care in the past 3 months
# hh_dataset$i.needed_healthcare_3months_hh	 <- indv_dataset ...	if (needed_healthcare == 'yes_4weeks' | needed_healthcare == 'yes_3months' for at least 1 individual in the HH, 1, 0)

# --- needed_healthcare_4weeks_hh ---
# [From indv to hh. Households with at least one individual that needed health care in the past 4 weeks
# hh_dataset$i.needed_healthcare_4weeks_hh	 <- indv_dataset ...	if (needed_healthcare == 'yes_4weeks' for at least 1 individual in the HH, 1, 0)

# --- clinics_4weeks ---
# [From indv to indv. Individuals by treatment location in the past 4 weeks
# indv_dataset$i.clinics_4weeks	 <- indv_dataset ...	if (needed_healthcare == 'yes_4weeks', clinics, NA)

# --- indv_sought_treatment_4weeks ---
# [From indv to indv. Individuals that sought treatment in a clinic in the past 4 weeks
# indv_dataset$i.indv_sought_treatment_4weeks	 <- indv_dataset ...	if (needed_healthcare == 'yes_4weeks', indv_sought_treatment, NA)

# --- indv_not_sought_treatment_4weeks ---
# [From indv to indv. Individuals that did not seek treatment in a clinic in the past 4 weeks
# indv_dataset$i.indv_not_sought_treatment_4weeks	 <- indv_dataset ...	if (needed_healthcare == 'yes_4weeks', indv_not_sought_treatment, NA)

# --- not_sought_treatment_hh ---
# [From indv to hh. HH with at least one individual that did not seek health care at a clinic in the past 3 months
# hh_dataset$i.not_sought_treatment_hh	 <- indv_dataset ...	if (indv_not_sought_treatment == 1 for at least 1 individual in the HH, 1, 0)

# --- health_care_needs ---  [For analysis]
# _dataset$i.health_care_needs	 <- _dataset ...	if (indv_not_sought_treatment == 1 for at least 1 individual in the HH, 3, if ((indv_needed_treatment_count == indv_sought treatment_count | indv_needed_treatment_count == 0) & needed_healthcare != "dont_know" for all individuals, 1, NA))

# --- not_sought_treatment_4weeks_hh ---
# [From indv to hh. HH with at least one individual that did not seek health care at a clinic in the past 4 weeks
# hh_dataset$i.not_sought_treatment_4weeks_hh	 <- indv_dataset ...	if (i.indv_not_sought_treatment_4weeks == 1 for at least 1 individual in the HH, 1, 0)

# --- not_sought_treatment_at_all_hh ---
# [From indv to hh. HH with at least one individual that did not seek treatment anywhere in the past 3 months
# hh_dataset$i.not_sought_treatment_at_all_hh	 <- indv_dataset ...	if (clinics == 'nowhere' for at least 1 individual in the HH, 1, 0)

# --- not_sought_treatment_at_all_4weeks_hh ---
# [From indv to hh. HH with at least one individual that did not seek treatment anywhere in the past 4 weeks
# hh_dataset$i.not_sought_treatment_at_all_4weeks_hh	 <- indv_dataset ...	if (i.clinics_4weeks == 'nowhere' for at least 1 individual in the HH, 1, 0)

# --- ind_birth_place_hh ---
# [From indv to hh. HH with at least one child born at home
# hh_dataset$i.ind_birth_place_hh	 <- indv_dataset ...	if (ind_birth_place == 'at_home' for at lesat 1 individual in the HH, 1, 0)

# --- ind_birth_place_needs ---  [For analysis]
# _dataset$i.ind_birth_place_needs	 <- _dataset ...	if (ind_birth_place == 'at_home' for at least 1 individual in the HH, 1, if (ind_birth_place ONLY the following for ALL individuals - ngo_clinic, government_clinic, private_clinic, maternity_ward, other, 0, NA))

# --- health_distance ---
# [From hh to hh. Categorize distance by: 0 - 20 min, >20 - 30 min, >30 min
# hh_dataset$i.health_distance	 <- hh_dataset ...	if (health_distance < 20, 0_20, if (health_distance >= 20 min & health_distance <= 30, 20_30, if (health_distance > 30, more_than_30, NA)))

# --- health_distance_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.health_distance_needs	 <- hh_dataset ...	if (health_distance < 20, 1, if (health_distance >= 20 min, 2, NA))

# --- health_barriers_not_accessed_hh ---
# [From hh to hh. HH with unmet health care need reporting any barrier
# hh_dataset$i.health_barriers_not_accessed_hh	 <- hh_dataset ...	if (health_barriers_not_accessed != 'no_challenge' & health_barriers_not_accessed != 'dont_know', 1, 0)

# --- health_barriers_accessed_hh ---
# [From hh to hh. HH without unmet health care need reporting any barrier
# hh_dataset$i.health_barriers_accessed_hh	 <- hh_dataset ...	if (health_barriers_accessed != 'no_challenge' & health_barriers_accessed != 'dont_know', 1, 0)

# --- health_barriers_no_need_hh ---
# [From hh to hh. HH without health care need reporting any barrier
# hh_dataset$i.health_barriers_no_need_hh	 <- hh_dataset ...	if (health_barriers_no_need != 'no_challenge' & health_barriers_no_need != 'dont_know', 1, 0)

# --- health_barriers_any ---
# [From hh to hh. Only always 1 of the 3 questions was asked to each HH and they have the same response options, i.e. they can be taken together to calculate the overall proportions out of all HH for each response
# hh_dataset$i.health_barriers_any	 <- hh_dataset ...	merged responses of health_barriers_not_accessed, health_barriers_accessed, health_barriers_no_need

# --- health_barriers_any_hh ---
# [From hh to hh. HH reporting any barrier
# hh_dataset$i.health_barriers_any_hh	 <- hh_dataset ...	if (i.health_barriers_any != 'no_challenge' & i.health_barriers_any != 'dont_know', 1, 0)

# --- health_barrier_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.health_barrier_needs	 <- hh_dataset ...	if (sum (i.health_barriers_any.no_functional_facility_nearby, i.health_barriers_any.cannot_afford, i.health_barriers_any.pwd_access, i.health_barriers_any.safety_concerns_at_facility, i.health_barriers_any.distrust, i.health_barriers_any.language_barrier_with_health_service_staff) >= 2, 1, if (sum (i.health_barriers_any.no_functional_facility_nearby, i.health_barriers_any.cannot_afford, i.health_barriers_any.pwd_access, i.health_barriers_any.safety_concerns_at_facility, i.health_barriers_any.distrust, i.health_barriers_any.language_barrier_with_health_service_staff) <= 1 & i.health_barriers_any.dont_know != 1, 0, NA))

# --- health_critical ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.health_critical	 <- hh_dataset ...	max (i.health_care_needs, i.health_distance_needs, na.rm = T only if maximum possible is reached)

# --- health_non_critical_mean ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.health_non_critical_mean	 <- hh_dataset ...	mean (i.health_distance_needs, i.ind_birth_place_needs, na.rm = F)

# --- health_non_critical ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.health_non_critical	 <- hh_dataset ...	if (i.health_non_critical_mean > 2/3, 3, if (i.health_non_critical_mean > 1/3 & health_non_critical_mean <= 2/3, 2, if(i.health_non_critical_mean <= 1/3, 1, NA)))

# --- health_overall_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.health_overall_needs	 <- hh_dataset ...	max (i.health_critical, i.health_non_critical, na.rm = T only if maximum possible is reached)

# --- health_coping ---  [For analysis]
# [From hh to hh. Not sure yet
# hh_dataset$i.health_coping	 <- hh_dataset ...	if (paid_healthcare == 'yes' | coping_reason == 'healthcare', 1, if (paid_healthcare == 'no' & coping_reason != 'healthcare', 0, NA))

# --- health_need_coping ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.health_need_coping	 <- hh_dataset ...	if (i.health_overall_needs == 1 | 2 & health_coping_total == 1, 1, 0)

# Education ----------------------------------------------------------------

# --- enrolment_girls_3_5 ---  [refugee]
# [From hh to hh. Enrolled girls aged 3-5
# if(pop=="refugee"){
#    	hh_dataset$i.enrolment_girls_3_5	 <- hh_dataset ...	sum (enrolment_girls_3, enrolment_girls_4_5)
# }

# --- enrolment_boys_3_5 ---  [refugee]
# [From hh to hh. Enrolled boys aged 3-5
# if(pop=="refugee"){
#    	hh_dataset$i.enrolment_boys_3_5	 <- hh_dataset ...	sum (enrolment_boys_3, enrolment_boys_4_5)
# }

# --- enrolment_girls_3_24 ---  [refugee]
# [From hh to hh. Enrolled girls aged 3-24
# if(pop=="refugee"){
#    	hh_dataset$i.enrolment_girls_3_24	 <- hh_dataset ...	sum (enrolment_girls_3, enrolment_girls_4_5, enrolment_girls_6_14, enrolment_girls_15_18, enrolment_girls_19_24)
# }

# --- enrolment_boys_3_24 ---  [refugee]
# [From hh to hh. Enrolled boys aged 3-24
# if(pop=="refugee"){
#    	hh_dataset$i.enrolment_boys_3_24	 <- hh_dataset ...	sum (enrolment_boys_3, enrolment_boys_4_5, enrolment_boys_6_14, enrolment_boys_15_18, enrolment_boys_19_24)
# }

# --- enrolment_girls_6_18 ---  [refugee]
# [From hh to hh. Enrolled girls aged 6-18
# if(pop=="refugee"){
#    	hh_dataset$i.enrolment_girls_6_18	 <- hh_dataset ...	sum (enrolment_girls_6_14, enrolment_girls_15_18)
# }

# --- enrolment_boys_6_18 ---  [refugee]
# [From hh to hh. Enrolled boys aged 6-18
# if(pop=="refugee"){
#    	hh_dataset$i.enrolment_boys_6_18	 <- hh_dataset ...	sum (enrolment_boys_6_14, enrolment_boys_15_18)
# }

# --- enrolment_girls_3_24_hh ---  [refugee]
# [From hh to hh. HH with at least one girl aged 3-24 not enrolled
# if(pop=="refugee"){
#    	hh_dataset$i.enrolment_girls_3_24_hh	 <- hh_dataset ...	Use enrolment fields against individual count fields to categorize 'all_enrolled' (all girls aged 3-24 in HH enrolled) and 'not_all_enrolled' (at least one girl aged 3-24 in HH not enrolled)
# }

# --- enrolment_boys_3_24_hh ---  [refugee]
# [From hh to hh. HH with at least one boy aged 3-24 not enrolled
# if(pop=="refugee"){
#    	hh_dataset$i.enrolment_boys_3_24_hh	 <- hh_dataset ...	Use enrolment fields against individual count fields to categorize 'all_enrolled' (all boys aged 3-24 in HH enrolled) and 'not_all_enrolled' (at least one boy aged 3-24 in HH not enrolled)
# }

# --- enrolment_girls_6_18_hh ---  [refugee]
# [From hh to hh. HH with at least one girl aged 6-18 not enrolled
# if(pop=="refugee"){
#    	hh_dataset$i.enrolment_girls_6_18_hh	 <- hh_dataset ...	As above but different age range
# }

# --- enrolment_boys_6_18_hh ---  [refugee]
# [From hh to hh. HH with at least one boy aged 6-18 not enrolled
# if(pop=="refugee"){
#    	hh_dataset$i.enrolment_boys_6_18_hh	 <- hh_dataset ...	As above but different age range
# }

# --- enrolment_children_3_24_hh ---  [refugee]
# [From hh to hh. HH with at least one child aged 3-24 not enrolled
# if(pop=="refugee"){
#    	hh_dataset$i.enrolment_children_3_24_hh	 <- hh_dataset ...	As above but for boys/girls together
# }

# --- enrolment_children_6_18_hh ---  [refugee]
# [From hh to hh. HH with at least one child aged 6-18 not enrolled
# if(pop=="refugee"){
#    	hh_dataset$i.enrolment_children_6_18_hh	 <- hh_dataset ...	As above but for boys/girls together
# }

# --- enrolment_proportion ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.enrolment_proportion	 <- hh_dataset ...	sum (enrolment_girls_3, enrolment_boys_3, enrolment_girls_4_5, enrolment_boys_4_5, enrolment_girls_6_14, enrolment_boys_6_14, enrolment_girls_15_18, enrolment_boys_15_18, enrolment_girls_19_24, enrolment_boys_19_24, na.rm = F) / sum (boy_3, girl_3, boy_4_5, girl_4_5, boy_6_14, girl_6_14, boy_15_18, girl_15_18, boy_19_24, girl_19_24)

# --- enrolment_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.enrolment_needs	 <- hh_dataset ...	if (i.enrolment_proportion < 0.4, 3, if (i.enrolment_proportion >= 0.4 & i.enrolment_proportion < 0.8, 2, if (i.enrolment_proportion >= 0.8 | i.children_3_24_count == 0, 1, NA)))

# --- formal_enrolment_girls_4_18 ---  [host]
# [From hh to hh. Enrolled girls aged 4-18 (formal edu)
# if(pop=="host"){
#    	hh_dataset$i.formal_enrolment_girls_4_18	 <- hh_dataset ...	sum (formal_enrolment_girls_4_5, formal_enrolment_girls_6_14, formal_enrolment_girls_15_18)
# }

# --- formal_enrolment_boys_4_18 ---  [host]
# [From hh to hh. Enrolled boys aged 4-18 (formal edu)
# if(pop=="host"){
#    	hh_dataset$i.formal_enrolment_boys_4_18	 <- hh_dataset ...	sum (formal_enrolment_boys_4_5, formal_enrolment_boys_6_14, formal_enrolment_boys_15_18)
# }

# --- formal_enrolment_girls_6_18 ---  [host]
# [From hh to hh. Enrolled girls aged 6-18 (formal edu)
# if(pop=="host"){
#    	hh_dataset$i.formal_enrolment_girls_6_18	 <- hh_dataset ...	sum (formal_enrolment_girls_6_14, formal_enrolment_girls_15_18)
# }

# --- formal_enrolment_boys_6_18 ---  [host]
# [From hh to hh. Enrolled boys aged 6-18 (formal edu)
# if(pop=="host"){
#    	hh_dataset$i.formal_enrolment_boys_6_18	 <- hh_dataset ...	sum (formal_enrolment_boys_6_14, formal_enrolment_boys_15_18)
# }

# --- formal_enrolment_girls_4_18_hh ---  [host]
# [From hh to hh. HH with at least one girl aged 4-18 not enrolled
# if(pop=="host"){
#    	hh_dataset$i.formal_enrolment_girls_4_18_hh	 <- hh_dataset ...	Use formal enrolment fields against individual count fields to categorize 'all_enrolled' (all girls aged 4-18 in HH enrolled) and 'not_all_enrolled' (at least one girl aged 4-18 in HH not enrolled)
# }

# --- formal_enrolment_boys_4_18_hh ---  [host]
# [From hh to hh. HH with at least one boy aged 4-18 not enrolled
# if(pop=="host"){
#    	hh_dataset$i.formal_enrolment_boys_4_18_hh	 <- hh_dataset ...	Use formal enrolment fields against individual count fields to categorize 'all_enrolled' (all boys aged 4-18 in HH enrolled) and 'not_all_enrolled' (at least one boy aged 4-18 in HH not enrolled)
# }

# --- formal_enrolment_girls_6_18_hh ---  [host]
# [From hh to hh. HH with at least one girl aged 6-18 not enrolled
# if(pop=="host"){
#    	hh_dataset$i.formal_enrolment_girls_6_18_hh	 <- hh_dataset ...	As above but different age range
# }

# --- formal_enrolment_boys_6_18_hh ---  [host]
# [From hh to hh. HH with at least one boy aged 6-18 not enrolled
# if(pop=="host"){
#    	hh_dataset$i.formal_enrolment_boys_6_18_hh	 <- hh_dataset ...	As above but different age range
# }

# --- formal_enrolment_children_4_18_hh ---  [host]
# [From hh to hh. HH with at least one child aged 4-18 not enrolled
# if(pop=="host"){
#    	hh_dataset$i.formal_enrolment_children_4_18_hh	 <- hh_dataset ...	As above but for boys/girls together
# }

# --- formal_enrolment_children_6_18_hh ---  [host]
# [From hh to hh. HH with at least one child aged 6-18 not enrolled
# if(pop=="host"){
#    	hh_dataset$i.formal_enrolment_children_6_18_hh	 <- hh_dataset ...	As above but for boys/girls together
# }

# --- informal_enrolment_girls_4_18 ---  [host]
# [From hh to hh. Enrolled girls aged 4-18 (informal edu)
# if(pop=="host"){
#    	hh_dataset$i.informal_enrolment_girls_4_18	 <- hh_dataset ...	sum (informal_enrolment_girls_4_5, informal_enrolment_girls_6_14, informal_enrolment_girls_15_18)
# }

# --- informal_enrolment_boys_4_18 ---  [host]
# [From hh to hh. Enrolled boys aged 4-18 (informal edu)
# if(pop=="host"){
#    	hh_dataset$i.informal_enrolment_boys_4_18	 <- hh_dataset ...	sum (informal_enrolment_boys_4_5, informal_enrolment_boys_6_14, informal_enrolment_boys_15_18)
# }

# --- informal_enrolment_girls_6_18 ---  [host]
# [From hh to hh. Enrolled girls aged 6-18 (informal edu)
# if(pop=="host"){
#    	hh_dataset$i.informal_enrolment_girls_6_18	 <- hh_dataset ...	sum (informal_enrolment_girls_6_14, informal_enrolment_girls_15_18)
# }

# --- informal_enrolment_boys_6_18 ---  [host]
# [From hh to hh. Enrolled boys aged 6-18 (informal edu)
# if(pop=="host"){
#    	hh_dataset$i.informal_enrolment_boys_6_18	 <- hh_dataset ...	sum (informal_enrolment_boys_6_14, informal_enrolment_boys_15_18)
# }

# --- informal_enrolment_girls_4_18_hh ---  [host]
# [From hh to hh. HH with at least one girl aged 4-18 not enrolled
# if(pop=="host"){
#    	hh_dataset$i.informal_enrolment_girls_4_18_hh	 <- hh_dataset ...	Use informal enrolment fields against individual count fields to categorize 'all_enrolled' (all girls aged 4-18 in HH enrolled) and 'not_all_enrolled' (at least one girl aged 4-18 in HH not enrolled)
# }

# --- informal_enrolment_boys_4_18_hh ---  [host]
# [From hh to hh. HH with at least one boy aged 4-18 not enrolled
# if(pop=="host"){
#    	hh_dataset$i.informal_enrolment_boys_4_18_hh	 <- hh_dataset ...	Use informal enrolment fields against individual count fields to categorize 'all_enrolled' (all boys aged 4-18 in HH enrolled) and 'not_all_enrolled' (at least one boy aged 4-18 in HH not enrolled)
# }

# --- informal_enrolment_girls_6_18_hh ---  [host]
# [From hh to hh. HH with at least one girl aged 6-18 not enrolled
# if(pop=="host"){
#    	hh_dataset$i.informal_enrolment_girls_6_18_hh	 <- hh_dataset ...	As above but different age range
# }

# --- informal_enrolment_boys_6_18_hh ---  [host]
# [From hh to hh. HH with at least one boy aged 6-18 not enrolled
# if(pop=="host"){
#    	hh_dataset$i.informal_enrolment_boys_6_18_hh	 <- hh_dataset ...	As above but different age range
# }

# --- informal_enrolment_children_4_18_hh ---  [host]
# [From hh to hh. HH with at least one child aged 4-18 not enrolled
# if(pop=="host"){
#    	hh_dataset$i.informal_enrolment_children_4_18_hh	 <- hh_dataset ...	As above but for boys/girls together
# }

# --- informal_enrolment_children_6_18_hh ---  [host]
# [From hh to hh. HH with at least one child aged 6-18 not enrolled
# if(pop=="host"){
#    	hh_dataset$i.informal_enrolment_children_6_18_hh	 <- hh_dataset ...	As above but for boys/girls together
# }

# --- any_enrolment_girls_4_5 ---  [host]
# [From hh to hh. Enrolled girls aged 4-5 (any edu)
# if(pop=="host"){
#    	hh_dataset$i.any_enrolment_girls_4_5	 <- hh_dataset ...	sum (formal_enrolment_girls_4_5, informal_enrolment_girls_4_5)
# }

# --- any_enrolment_boys_4_5 ---  [host]
# [From hh to hh. Enrolled boys aged 4-5 (any edu)
# if(pop=="host"){
#    	hh_dataset$i.any_enrolment_boys_4_5	 <- hh_dataset ...	sum (formal_enrolment_boys_4_5, informal_enrolment_boys_4_5)
# }

# --- any_enrolment_girls_6_14 ---  [host]
# [From hh to hh. Enrolled girls aged 6-14 (any edu)
# if(pop=="host"){
#    	hh_dataset$i.any_enrolment_girls_6_14	 <- hh_dataset ...	sum (formal_enrolment_girls_6_14, informal_enrolment_girls_6_14)
# }

# --- any_enrolment_boys_6_14 ---  [host]
# [From hh to hh. Enrolled boys aged 6-14 (any edu)
# if(pop=="host"){
#    	hh_dataset$i.any_enrolment_boys_6_14	 <- hh_dataset ...	sum (formal_enrolment_boys_6_14, informal_enrolment_boys_6_14)
# }

# --- any_enrolment_girls_15_18 ---  [host]
# [From hh to hh. Enrolled girls aged 15-18 (any edu)
# if(pop=="host"){
#    	hh_dataset$i.any_enrolment_girls_15_18	 <- hh_dataset ...	sum (formal_enrolment_girls_15_18, informal_enrolment_girls_15_18)
# }

# --- any_enrolment_boys_15_18 ---  [host]
# [From hh to hh. Enrolled boys aged 15-18 (any edu)
# if(pop=="host"){
#    	hh_dataset$i.any_enrolment_boys_15_18	 <- hh_dataset ...	sum (formal_enrolment_boys_15_18, informal_enrolment_boys_15_18)
# }

# --- any_enrolment_girls_4_18 ---  [host]
# [From hh to hh. Enrolled girls aged 4-18 (any edu)
# if(pop=="host"){
#    	hh_dataset$i.any_enrolment_girls_4_18	 <- hh_dataset ...	sum (formal_enrolment_girls_4_18, informal_enrolment_girls_4_18)
# }

# --- any_enrolment_boys_4_18 ---  [host]
# [From hh to hh. Enrolled boys aged 4-18 (any edu)
# if(pop=="host"){
#    	hh_dataset$i.any_enrolment_boys_4_18	 <- hh_dataset ...	sum (formal_enrolment_girls_4_18, informal_enrolment_girls_4_18)
# }

# --- any_enrolment_girls_6_18 ---  [host]
# [From hh to hh. Enrolled girls aged 6-18 (any edu)
# if(pop=="host"){
#    	hh_dataset$i.any_enrolment_girls_6_18	 <- hh_dataset ...	sum (formal_enrolment_girls_6_18, informal_enrolment_girls_6_18)
# }

# --- any_enrolment_boys_6_18 ---  [host]
# [From hh to hh. Enrolled boys aged 6-18 (any edu)
# if(pop=="host"){
#    	hh_dataset$i.any_enrolment_boys_6_18	 <- hh_dataset ...	sum (formal_enrolment_girls_6_18, informal_enrolment_girls_6_18)
# }

# --- any_enrolment_girls_4_18_hh ---  [host]
# [From hh to hh. HH with at least one girl aged 4-18 not enrolled
# if(pop=="host"){
#    	hh_dataset$i.any_enrolment_girls_4_18_hh	 <- hh_dataset ...	Use any enrolment fields against individual count fields to categorize 'all_enrolled' (all girls aged 4-18 in HH enrolled) and 'not_all_enrolled' (at least one girl aged 4-18 in HH not enrolled)
# }

# --- any_enrolment_boys_4_18_hh ---  [host]
# [From hh to hh. HH with at least one boy aged 4-18 not enrolled
# if(pop=="host"){
#    	hh_dataset$i.any_enrolment_boys_4_18_hh	 <- hh_dataset ...	Use any enrolment fields against individual count fields to categorize 'all_enrolled' (all boys aged 4-18 in HH enrolled) and 'not_all_enrolled' (at least one boy aged 4-18 in HH not enrolled)
# }

# --- any_enrolment_girls_6_18_hh ---  [host]
# [From hh to hh. HH with at least one girl aged 6-18 not enrolled
# if(pop=="host"){
#    	hh_dataset$i.any_enrolment_girls_6_18_hh	 <- hh_dataset ...	As above but different age range
# }

# --- any_enrolment_boys_6_18_hh ---  [host]
# [From hh to hh. HH with at least one boy aged 6-18 not enrolled
# if(pop=="host"){
#    	hh_dataset$i.any_enrolment_boys_6_18_hh	 <- hh_dataset ...	As above but different age range
# }

# --- any_enrolment_children_4_18_hh ---  [host]
# [From hh to hh. HH with at least one child aged 4-18 not enrolled
# if(pop=="host"){
#    	hh_dataset$i.any_enrolment_children_4_18_hh	 <- hh_dataset ...	As above but for boys/girls together
# }

# --- any_enrolment_children_6_18_hh ---  [host]
# [From hh to hh. HH with at least one child aged 6-18 not enrolled
# if(pop=="host"){
#    	hh_dataset$i.any_enrolment_children_6_18_hh	 <- hh_dataset ...	As above but for boys/girls together
# }

# --- distance_learning_girls_3_5 ---  [refugee]
# [From hh to hh. Distance learning girls aged 3-5
# if(pop=="refugee"){
#    	hh_dataset$i.distance_learning_girls_3_5	 <- hh_dataset ...	sum (distance_learning_girls_3, distance_learning_girls_4_5)
# }

# --- distance_learning_boys_3_5 ---  [refugee]
# [From hh to hh. Distance learning boys aged 3-5
# if(pop=="refugee"){
#    	hh_dataset$i.distance_learning_boys_3_5	 <- hh_dataset ...	sum (distance_learning_boys_3, distance_learning_boys_4_5)
# }

# --- distance_learning_girls_3_24 ---  [refugee]
# [From hh to hh. Distance learning girls aged 3-24
# if(pop=="refugee"){
#    	hh_dataset$i.distance_learning_girls_3_24	 <- hh_dataset ...	sum (distance_learning_girls_3, distance_learning_girls_4_5, distance_learning_girls_6_14, distance_learning_girls_15_18, distance_learning_girls_19_24)
# }

# --- distance_learning_boys_3_24 ---  [refugee]
# [From hh to hh. Distance learning boys aged 3-24
# if(pop=="refugee"){
#    	hh_dataset$i.distance_learning_boys_3_24	 <- hh_dataset ...	sum (distance_learning_boys_3, distance_learning_boys_4_5, distance_learning_boys_6_14, distance_learning_boys_15_18, distance_learning_boys_19_24)
# }

# --- distance_learning_girls_4_18 ---  [host]
# [From hh to hh. Distance learning girls aged 4-18
# if(pop=="host"){
#    	hh_dataset$i.distance_learning_girls_4_18	 <- hh_dataset ...	sum (distance_learning_girls_4_5, distance_learning_girls_6_14, distance_learning_girls_15_18)
# }

# --- distance_learning_boys_4_18 ---  [host]
# [From hh to hh. Distance learning boys aged 4-18
# if(pop=="host"){
#    	hh_dataset$i.distance_learning_boys_4_18	 <- hh_dataset ...	sum (distance_learning_boys_4_5, distance_learning_boys_6_14, distance_learning_boys_15_18)
# }

# --- distance_learning_girls_6_18 ---
# [From hh to hh. Distance learning girls aged 6-18
# hh_dataset$i.distance_learning_girls_6_18	 <- hh_dataset ...	sum (distance_learning_girls_6_14, distance_learning_girls_15_18)

# --- distance_learning_boys_6_18 ---
# [From hh to hh. Distance learning boys aged 6-18
# hh_dataset$i.distance_learning_boys_6_18	 <- hh_dataset ...	sum (distance_learning_boys_6_14, distance_learning_boys_15_18)

# --- distance_learning_girls_3_24_hh ---  [refugee]
# [From hh to hh. HH with at least one girl aged 3-24 not in distance learning
# if(pop=="refugee"){
#    	hh_dataset$i.distance_learning_girls_3_24_hh	 <- hh_dataset ...	Use distance learning fields against individual count fields to categorize 'all_in_distance_learning' (all girls aged 3-24 in HH in distance learning) and 'not_all_in_distance_learning' (at least one girl aged 3-24 in HH not in distance learning)
# }

# --- distance_learning_boys_3_24_hh ---  [refugee]
# [From hh to hh. HH with at least one boy aged 3-24 not in distance learning
# if(pop=="refugee"){
#    	hh_dataset$i.distance_learning_boys_3_24_hh	 <- hh_dataset ...	Use distance learning fields against individual count fields to categorize 'all_in_distance_learning' (all boys aged 3-24 in HH in distance learning) and 'not_all_in_distance_learning' (at least one boy aged 3-24 in HH not in distance learning)
# }

# --- distance_learning_children_3_24_hh ---  [refugee]
# [From hh to hh. HH with at least one child aged 3-24 not in distance learning
# if(pop=="refugee"){
#    	hh_dataset$i.distance_learning_children_3_24_hh	 <- hh_dataset ...	As above but for boys/girls together
# }

# --- distance_learning_girls_6_18_hh ---
# [From hh to hh. HH with at least one girl aged 6-18 not in distance learning
# hh_dataset$i.distance_learning_girls_6_18_hh	 <- hh_dataset ...	As above but different age range

# --- distance_learning_boys_6_18_hh ---
# [From hh to hh. HH with at least one boy aged 6-18 not in distance learning
# hh_dataset$i.distance_learning_boys_6_18_hh	 <- hh_dataset ...	As above but different age range

# --- distance_learning_children_6_18_hh ---
# [From hh to hh. HH with at least one child aged 6-18 not in distance learning
# hh_dataset$i.distance_learning_children_6_18_hh	 <- hh_dataset ...	As above but for boys/girls together

# --- distance_learning_girls_4_18_hh ---  [host]
# [From hh to hh. HH with at least one girl aged 4-18 not in distance learning
# if(pop=="host"){
#    	hh_dataset$i.distance_learning_girls_4_18_hh	 <- hh_dataset ...	As above but different age range
# }

# --- distance_learning_boys_4_18_hh ---  [host]
# [From hh to hh. HH with at least one boy aged 4-18 not in distance learning
# if(pop=="host"){
#    	hh_dataset$i.distance_learning_boys_4_18_hh	 <- hh_dataset ...	As above but different age range
# }

# --- distance_learning_children_4_18_hh ---  [host]
# [From hh to hh. HH with at least child boy aged 4-18 not in distance learning
# if(pop=="host"){
#    	hh_dataset$i.distance_learning_children_4_18_hh	 <- hh_dataset ...	As above but for boys/girls together
# }

# --- distance_learning_proportion ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.distance_learning_proportion	 <- hh_dataset ...	sum (distance_learning_girls_3, distance_learning_boys_3, distance_learning_girls_4_5, distance_learning_boys_4_5, distance_learning_girls_6_14, distance_learning_boys_6_14, distance_learning_girls_15_18, distance_learning_boys_15_18, distance_learning_girls_19_24, distance_learning_boys_19_24, na.rm = F) / sum (boy_3, girl_3, boy_4_5, girl_4_5, boy_6_14, girl_6_14, boy_15_18, girl_15_18, boy_19_24, girl_19_24)

# --- distance_learning_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.distance_learning_needs	 <- hh_dataset ...	if (i.distance_learning_proportion < 0.4, 3, if (i.distance_learning_proportion >= 0.4 & i.distance_learning_proportion < 0.8, 2, if (i.distance_learning_proportion >= 0.8 | i.children_3_24_count == 0, 1, NA)))

# --- send_back_girls_3_5 ---  [refugee]
# [From hh to hh. Send back girls aged 3-5
# if(pop=="refugee"){
#    	hh_dataset$i.send_back_girls_3_5	 <- hh_dataset ...	sum (send_back_girls_3, send_back_girls_4_5)
# }

# --- send_back_boys_3_5 ---  [refugee]
# [From hh to hh. Send back boys aged 3-5
# if(pop=="refugee"){
#    	hh_dataset$i.send_back_boys_3_5	 <- hh_dataset ...	sum (send_back_boys_3, send_back_boys_4_5)
# }

# --- send_back_girls_3_24 ---  [refugee]
# [From hh to hh. Send back girls aged 3-24
# if(pop=="refugee"){
#    	hh_dataset$i.send_back_girls_3_24	 <- hh_dataset ...	sum (send_back_girls_3, send_back_girls_4_5, send_back_girls_6_14, send_back_girls_15_18, send_back_girls_19_24)
# }

# --- send_back_boys_3_24 ---  [refugee]
# [From hh to hh. Send back boys aged 3-24
# if(pop=="refugee"){
#    	hh_dataset$i.send_back_boys_3_24	 <- hh_dataset ...	sum (send_back_boys_3, send_back_boys_4_5, send_back_boys_6_14, send_back_boys_15_18, send_back_boys_19_24)
# }

# --- send_back_girls_4_18 ---  [host]
# [From hh to hh. Send back girls aged 4-18
# if(pop=="host"){
#    	hh_dataset$i.send_back_girls_4_18	 <- hh_dataset ...	sum (send_back_girls_4_5, send_back_girls_6_14, send_back_girls_15_18)
# }

# --- send_back_boys_4_18 ---  [host]
# [From hh to hh. Send back boys aged 4-18
# if(pop=="host"){
#    	hh_dataset$i.send_back_boys_4_18	 <- hh_dataset ...	sum (send_back_boys_4_5, send_back_boys_6_14, send_back_boys_15_18)
# }

# --- send_back_girls_6_18 ---
# [From hh to hh. Send back girls aged 6-18
# hh_dataset$i.send_back_girls_6_18	 <- hh_dataset ...	sum (send_back_girls_6_14, send_back_girls_15_18)

# --- send_back_boys_6_18 ---
# [From hh to hh. Send back boys aged 6-18
# hh_dataset$i.send_back_boys_6_18	 <- hh_dataset ...	sum (send_back_boys_6_14, send_back_boys_15_18)

# --- send_back_girls_3_24_hh ---  [refugee]
# [From hh to hh. HH with at least one girl aged 3-24 that will not be sent back
# if(pop=="refugee"){
#    	hh_dataset$i.send_back_girls_3_24_hh	 <- hh_dataset ...	Use send back fields against individual count fields to categorize 'all_in_send_back' (all girls aged 3-24 in HH in distance learning) and 'not_all_in_send_back' (at least one girl aged 3-24 in HH not in distance learning)
# }

# --- send_back_boys_3_24_hh ---  [refugee]
# [From hh to hh. HH with at least one boy aged 3-24 that will not be sent back
# if(pop=="refugee"){
#    	hh_dataset$i.send_back_boys_3_24_hh	 <- hh_dataset ...	Use send back fields against individual count fields to categorize 'all_in_send_back' (all boys aged 3-24 in HH in distance learning) and 'not_all_in_send_back' (at least one boy aged 3-24 in HH not in distance learning)
# }

# --- send_back_children_3_24_hh ---  [refugee]
# [From hh to hh. HH with at least one child aged 3-24 that will not be sent back
# if(pop=="refugee"){
#    	hh_dataset$i.send_back_children_3_24_hh	 <- hh_dataset ...	As above but for boys/girls together
# }

# --- send_back_girls_6_18_hh ---
# [From hh to hh. HH with at least one girl aged 6-18 that will not be sent back
# hh_dataset$i.send_back_girls_6_18_hh	 <- hh_dataset ...	As above but different age range

# --- send_back_boys_6_18_hh ---
# [From hh to hh. HH with at least one boy aged 6-18 that will not be sent back
# hh_dataset$i.send_back_boys_6_18_hh	 <- hh_dataset ...	As above but different age range

# --- send_back_children_6_18_hh ---
# [From hh to hh. HH with at least one child aged 6-18 that will not be sent back
# hh_dataset$i.send_back_children_6_18_hh	 <- hh_dataset ...	As above but for boys/girls together

# --- send_back_girls_4_18_hh ---  [host]
# [From hh to hh. HH with at least one girl aged 4-18 that will not be sent back
# if(pop=="host"){
#    	hh_dataset$i.send_back_girls_4_18_hh	 <- hh_dataset ...	As above but different age range
# }

# --- send_back_boys_4_18_hh ---  [host]
# [From hh to hh. HH with at least one boy aged 4-18 that will not be sent back
# if(pop=="host"){
#    	hh_dataset$i.send_back_boys_4_18_hh	 <- hh_dataset ...	As above but different age range
# }

# --- send_back_children_4_18_hh ---  [host]
# [From hh to hh. HH with at least child boy aged 4-18 that will not be sent back
# if(pop=="host"){
#    	hh_dataset$i.send_back_children_4_18_hh	 <- hh_dataset ...	As above but for boys/girls together
# }

# --- send_back_proportion ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.send_back_proportion	 <- hh_dataset ...	sum (send_back_girls_3, send_back_boys_3, send_back_girls_4_5, send_back_boys_4_5, send_back_girls_6_14, send_back_boys_6_14, send_back_girls_15_18, send_back_boys_15_18, send_back_girls_19_24, send_back_boys_19_24, na.rm = F) / sum (boy_3, girl_3, boy_4_5, girl_4_5, boy_6_14, girl_6_14, boy_15_18, girl_15_18, boy_19_24, girl_19_24)

# --- send_back_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.send_back_needs	 <- hh_dataset ...	if (((send_back_barriers_girls == 'marriage_pregnancy' | 'working_outside_home') & at least 1 girl will not be sent back BUT send_back_girls_19_24 == girl_19_24) | ((send_back_barriers_boys == 'marriage_pregnancy' | 'working_outside_home') & at least 1 boy will not be sent back BUT send_back_boys_19_24 == boy_19_24), 4, if (i.send_back_proportion < 0.4, 3, if (i.send_back_proportion >= 0.4 & i.send_back_proportion < 0.8, 2, if (i.send_back_proportion >= 0.8 | i.children_3_24_count == 0, 1, NA)))

# --- girl_3_5_count ---  [refugee]
# [From hh to hh. Count girls aged 3-5
# if(pop=="refugee"){
#    	hh_dataset$i.girl_3_5_count	 <- hh_dataset ...	sum (girl_3_count, girl_4_5_count)
# }

# --- boy_3_5_count ---  [refugee]
# [From hh to hh. Count boys aged 3-5
# if(pop=="refugee"){
#    	hh_dataset$i.boy_3_5_count	 <- hh_dataset ...	sum (boy_3_count, boy_4_5_count)
# }

# --- girl_3_24_count ---  [refugee]
# [From hh to hh. Count girls aged 3-24
# if(pop=="refugee"){
#    	hh_dataset$i.girl_3_24_count	 <- hh_dataset ...	sum (girl_3_count, girl_4_5_count, girl_6_14_count, girl_15_18_count, girl_19_24_count)
# }

# --- boy_3_24_count ---  [refugee]
# [From hh to hh. Count boys aged 3-24
# if(pop=="refugee"){
#    	hh_dataset$i.boy_3_24_count	 <- hh_dataset ...	sum (boy_3_count, boy_4_5_count, boy_6_14_count, boy_15_18_count, boy_19_24_count)
# }

# --- children_3_24_count ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.children_3_24_count	 <- hh_dataset ...	sum (i.girl_3_24_count, i.boy_3_24_count)

# --- girl_4_18_count ---  [host]
# [From hh to hh. Count girls aged 4-18
# if(pop=="host"){
#    	hh_dataset$i.girl_4_18_count	 <- hh_dataset ...	sum ( girl_4_5_count, girl_6_14_count, girl_15_18_count)
# }

# --- boy_4_18_count ---  [host]
# [From hh to hh. Count boys aged 4-18
# if(pop=="host"){
#    	hh_dataset$i.boy_4_18_count	 <- hh_dataset ...	sum (boy_4_5_count, boy_6_14_count, boy_15_18_count)
# }

# --- girl_6_18_count ---
# [From hh to hh. Count girls aged 6-18
# hh_dataset$i.girl_6_18_count	 <- hh_dataset ...	sum (girl_6_14_count, girl_15_18_count)

# --- boy_6_18_count ---
# [From hh to hh. Count boys aged 6-18
# hh_dataset$i.boy_6_18_count	 <- hh_dataset ...	sum (boy_6_14_count, boy_15_18_count)

# --- distance_learning_barriers_girls_hh ---
# [From hh to hh. HH reporting any barrier
# hh_dataset$i.distance_learning_barriers_girls_hh	 <- hh_dataset ...	if (distance_learning_barriers_girls != 'no_challenge' & distance_learning_barriers_girls != 'dont_know', 1, 0)

# --- distance_learning_barriers_boys_hh ---
# [From hh to hh. HH reporting any barrier
# hh_dataset$i.distance_learning_barriers_boys_hh	 <- hh_dataset ...	if (distance_learning_barriers_boys != 'no_challenge' & distance_learning_barriers_boys != 'dont_know', 1, 0)

# --- distance_learning_barrier_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.distance_learning_barrier_needs	 <- hh_dataset ...	"if (((distance_learning_barriers_girls == 'marriage_pregnancy' | 'working_outside_home') & girl_19_24 == 0) | ((distance_learning_barriers_boys == 'marriage_pregnancy' | 'working_outside_home') & boy_19_24 == 0), 4, if (sum (never_enrolled, not_effective, lack_content_older_children, lack_content_younger_children, needed_at_home, working_outside_home, hh_unaware_opportunities, hh_consider_edu_unimportant, marriage_pregnancy, language_barriers, lack_materials, lack_structured_schooling, lack_guidance, noone_available_to_support, lack_technology, lack_mobile_network, lack_internet_connectivity, no_offers, difficulties_pwd, too_old, too_young, other) >= 4 FOR EITHER BOYS OR GIRLS, 3, if (sum (same variables) <= 3 & sum (same variables) > 1 FOR EITHER BOYS OR GIRLS, 2, (sum (same variables) <= 1 & != 'dont_know' FOR BOYS AND GIRLS) | i.children_3_24_count == 0, 1, NA)))"

# --- send_back_barriers_girls_hh ---
# [From hh to hh. HH reporting any barrier
# hh_dataset$i.send_back_barriers_girls_hh	 <- hh_dataset ...	if (send_back_barriers_girls != 'no_challenge' & send_back_barriers_girls != 'dont_know', 1, 0)

# --- send_back_barriers_boys_hh ---
# [From hh to hh. HH reporting any barrier
# hh_dataset$i.send_back_barriers_boys_hh	 <- hh_dataset ...	if (send_back_barriers_boys != 'no_challenge' & send_back_barriers_boys != 'dont_know', 1, 0)

# --- send_back_challenges_girls_hh ---
# [From hh to hh. HH reporting any barrier
# hh_dataset$i.send_back_challenges_girls_hh	 <- hh_dataset ...	if (send_back_barriers_girls != 'no_challenge' & send_back_barriers_girls != 'dont_know', 1, 0)

# --- send_back_challenges_boys_hh ---
# [From hh to hh. HH reporting any barrier
# hh_dataset$i.send_back_challenges_boys_hh	 <- hh_dataset ...	if (send_back_barriers_boys != 'no_challenge' & send_back_barriers_boys != 'dont_know', 1, 0)

# --- send_back_barrier_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.send_back_barrier_needs	 <- hh_dataset ...	"if (((send_back_barriers_girls == 'marriage_pregnancy' | 'working_outside_home') & girl_19_24 == 0) | ((send_back_barriers_boys == 'marriage_pregnancy' | 'working_outside_home') & boy_19_24 == 0), 4, if (sum (never_enrolled, lack_content_older_children, lack_content_younger_children, needed_at_home, working_outside_home, inaccessibility, covid_risk, hh_unaware_opportunities, hh_consider_edu_unimportant, marriage_pregnancy, too_old, too_young, language_barriers, lack_materials, lack_structured_schooling, fallen_behind, poor_infrastructure, lack_qualified_staff, lack_female_staff, lack_gender_segregation_facility, lack_gender_segregation_wash, difficulties_pwd, other) >= 4 FOR EITHER BOYS OR GIRLS, 3, if (sum (same variables) <= 3 & sum (same variables) > 1 FOR EITHER BOYS OR GIRLS, 2, (sum (same variables) <= 1 & != 'dont_know' FOR BOYS AND GIRLS) | i.children_3_24_count == 0, 1, NA)))"

# --- education_overall_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.education_overall_needs	 <- hh_dataset ...	max (i.enrolment_needs, i.distance_learning_needs, i.send_back_needs, i.distance_learning_barrier_needs, i.send_back_barrier_needs, na.rm = T only if maximum possible is reached)

# --- education_coping ---  [For analysis]
# [From hh to hh. Not sure yet
# hh_dataset$i.education_coping	 <- hh_dataset ...	if (coping_reason == 'education', 1, if (coping_reason != 'education', 0, NA))

# --- education_need_coping ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.education_need_coping	 <- hh_dataset ...	if (i.education_overall_needs == 1 | 2 & education_coping_total == 1, 1, 0)

# Nutrition ----------------------------------------------------------------

# --- child_not_screened_hh ---
# [From indv to hh. HH with at least one child that wasn't screened
# hh_dataset$i.child_not_screened_hh	 <- indv_dataset ...	if (child_screened == 'no' for at least 1 individual in the HH, 1, 0)

# --- no_child_screened_hh ---
# [From indv to hh. HH with no screened children
# hh_dataset$i.no_child_screened_hh	 <- indv_dataset ...	if (child_screened == 'no' for all individuals in the HH, 1, 0)

# --- child_screened_hh ---
# [From indv to hh. HH with at least one child that was screened
# hh_dataset$i.child_screened_hh	 <- indv_dataset ...	if (child_screened == 'yes' for at least 1 individual in the HH, 1, 0)

# --- girl_screened ---
# [From indv to indv. Children screened by gender
# indv_dataset$i.girl_screened	 <- indv_dataset ...	if (ind_gender == 'female', child_screened, NA)

# --- boy_screened ---
# [From indv to indv. Children screened by gender
# indv_dataset$i.boy_screened	 <- indv_dataset ...	if (ind_gender == 'male', child_screened, NA)

# --- child_no_nutrition_treatment_hh ---
# [From indv to hh. HH with at lesat one malnourished cihld that did not receive support
# hh_dataset$i.child_no_nutrition_treatment_hh	 <- indv_dataset ...	if ((child_nutrition_treatment == 'treatment_not_received' | child_nutrition_treatment == 'child_not_taken' for at least 1 referred child (child_referred) in the HH) & nutrition_barriers_not_accessed != 'already_referred' & nutrition_barriers_not_accessed != 'not_admitted', 1, 0)

# --- no_child_nutrition_treatment_hh ---
# [From indv to hh. HH with at lesat one malnourished cihld that did not receive support
# hh_dataset$i.no_child_nutrition_treatment_hh	 <- indv_dataset ...	if ((child_nutrition_treatment == 'treatment_not_received' | child_nutrition_treatment == 'child_not_taken' for all referred children (child_referred) in the HH) & nutrition_barriers_not_accessed != 'already_referred' & nutrition_barriers_not_accessed != 'not_admitted', 1, 0)

# --- child_nutrition_treatment_hh ---
# [From indv to hh. HH with at lesat one malnourished cihld that received support
# hh_dataset$i.child_nutrition_treatment_hh	 <- indv_dataset ...	if ((child_nutrition_treatment == 'sam_treatment' | child_nutrition_treatment == 'mam_treatment' | child_nutrition_treatment == 'inpatient_treatment', 1, 0)

# --- girl_nutrition_treatment ---
# [From indv to indv. Children having received treatment by gender
# indv_dataset$i.girl_nutrition_treatment	 <- indv_dataset ...	if (ind_gender == 'female', child_nutrition_treatment, NA)

# --- boy_nutrition_treatment ---
# [From indv to indv. Children having received treatment by gender
# indv_dataset$i.boy_nutrition_treatment	 <- indv_dataset ...	if (ind_gender == 'male', child_nutrition_treatment, NA)

# --- child_nutrition_needs ---  [For analysis]
# [From hh to hh. Might need to be adjusted
# hh_dataset$i.child_nutrition_needs	 <- hh_dataset ...	if (if (i.child_nutrition_treatment_hh == 1, 4, if (i.child_not_screened_hh == 1, 3, if( child_screened == indv_6_59_months_HH_count & child_taken_after_referral_HH_count == sum (child_referred == 'yes', child_enrolled == 'yes') | indv_6_59_months_HH_count == 0, 1, NA)))

# --- nutrition_barriers_not_accessed_hh ---
# [From hh to hh. HH with unmet nutrition need reporting any barrier
# hh_dataset$i.nutrition_barriers_not_accessed_hh	 <- hh_dataset ...	if (nutrition_barriers_not_accessed != 'no_issues' & nutrition_barriers_not_accessed != 'dont_know', 1, 0)

# --- nutrition_barriers_accessed_hh ---
# [From hh to hh. HH with met nutrition need reporting any barrier
# hh_dataset$i.nutrition_barriers_accessed_hh	 <- hh_dataset ...	if (nutrition_barriers_accessed != 'no_issues' & nutrition_barriers_accessed != 'dont_know', 1, 0)

# --- nutrition_barriers_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.nutrition_barriers_needs	 <- hh_dataset ...	if (nutrition_barriers_accessed != 'no_issues' & nutrition_barriers_accessed != 'dont_know', 1, if (nutrition_barriers_accessed == 'no_issues' | indv_6_59_months_HH_count == 0, 0, NA))

# --- nutrition_barriers_any ---
# [From hh to hh. As for health, combined version of the two questions
# hh_dataset$i.nutrition_barriers_any	 <- hh_dataset ...	Can we also try and combine those two above - as for health (even though they have 3 response options that do not match)

# --- nutrition_barriers_any_hh ---
# [From hh to hh. HH reporting any barrier
# hh_dataset$i.nutrition_barriers_any_hh	 <- hh_dataset ...	if (i.nutrition_barriers_any != 'no_issues' & i.nutrition_barriers_any != 'dont_know', 1, 0)

# --- child_bsfp_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.child_bsfp_needs	 <- hh_dataset ...	if (received_bsfp == 'no', 2, if (received_bsfp == 'yes' | indv_6_59_months_HH_count == 0, 1, NA))

# --- child_nutrition_contact_any_hh ---  [refugee]
# [From hh to hh. HH reporting that they received at least one type of nutrition service for their children
# if(pop=="refugee"){
#    	hh_dataset$i.child_nutrition_contact_any_hh	 <- hh_dataset ...	if (at least 1 of the following == 'yes' - nutrition_messaging, muac_messaging, caregiver_muac_screening, received_bsfp - or i.child_screened_hh == 1 or i.child_nutrition_treatment_hh == 1, 1, 0)
# }

# --- child_nutrition_contact_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.child_nutrition_contact_needs	 <- hh_dataset ...	if (all the following == 'no' - nutrition_messaging, muac_messaging, caregiver_muac_screening, received_bsfp - AND i.no_child_screened_hh == 1 AND i.no_child_nutrition_treatment_hh == 1, 1, if (at least 1 of the following == 'yes' - nutrition_messaging, muac_messaging, caregiver_muac_screening, received_bsfp - OR i.child_screened_hh == 1 OR i.child_nutrition_treatment_hh == 1| indv_6_59_months_HH_count == 0, 1, NA))

# --- child_nutrition_contact_any_hh ---  [host]
# [From hh to hh. HH reporting that they received at least one type of nutrition service for their children
# if(pop=="host"){
#    	hh_dataset$i.child_nutrition_contact_any_hh	 <- hh_dataset ...	if (at least 1 of the following == 'yes' - nutrition_messaging, muac_messaging, caregiver_muac_screening - or i.child_screened_hh == 1 or i.child_nutrition_treatment_hh == 1, 1, 0)
# }

# --- plw_in_hh ---
# [From hh to hh. HH with PLW
# hh_dataset$i.plw_in_hh	 <- hh_dataset ...	if (plw_in_hh > 0, 1, 0)

# --- plw_sfs_hh ---
# [From hh to hh. HH with PLW that received supplementary feeding
# hh_dataset$i.plw_sfs_hh	 <- hh_dataset ...	if (plw_sfs > 0, 1, 0)

# --- plw_screened_hh ---
# [From hh to hh. HH with screened PLW
# hh_dataset$i.plw_screened_hh	 <- hh_dataset ...	if (plw_screened > 0, 1, 0)

# --- plw_referred_hh ---
# [From hh to hh. HH with referred PLW
# hh_dataset$i.plw_referred_hh	 <- hh_dataset ...	if (plw_referred > 0, 1, 0)

# --- plw_admitted_hh ---
# [From hh to hh. HH with admitted PLW
# hh_dataset$i.plw_admitted_hh	 <- hh_dataset ...	if (plw_admitted > 0, 1, 0)

# --- plw_treated_hh ---  [host]
# [From hh to hh. HH with PLW that received targeted supplementary feeding
# if(pop=="host"){
#    	hh_dataset$i.plw_treated_hh	 <- hh_dataset ...	if (plw_treated > 0, 1, 0)
# }

# --- plw_iron_folic_acid_hh ---
# [From hh to hh. HH with PLW that received IFA tablets
# hh_dataset$i.plw_iron_folic_acid_hh	 <- hh_dataset ...	if (plw_iron_folic_acid > 0, 1, 0)

# --- plw_nutrition_contact_any_hh ---
# [From hh to hh. HH reporting that they received at least one type of nutrition service for their PLW
# hh_dataset$i.plw_nutrition_contact_any_hh	 <- hh_dataset ...	if (at least 1 of the following == 1 or 'yes' - plw_nutrition_messaging, i.plw_sfs_hh, i.plw_screened_hh, i.plw_referred_hh, i.plw_admitted_hh, i.plw_iron_folic_acid_hh, 1, 0)

# --- plw_nutrition_contact_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.plw_nutrition_contact_needs	 <- hh_dataset ...	if (plw_nutrition_messaging == 'no' AND all the following == 0 - plw_sfs, plw_screened, plw_referred, plw_admitted, plw_iron_folic_acid, 1, if (plw_nutrition_messaging == 'yes' OR at least 1 of the following > 0 - plw_sfs, plw_screened, plw_referred, plw_admitted, plw_iron_folic_acid| plw_in_hh == 0, 1, NA))

# --- plw_iron_folic_acid_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.plw_iron_folic_acid_needs	 <- hh_dataset ...	if (plw_iron_folic_acid < plw_in_hh, 1, plw_iron_folic_acid == plw_in_hh | plw_in_hh == 0, 0, NA)

# --- adol_girls_folic_acid_tablets_hh ---
# [From hh to hh. HH with adolescent girls that received IFA tablets
# hh_dataset$i.adol_girls_folic_acid_tablets_hh	 <- hh_dataset ...	if (adol_girls_iron_folic_acid_tablets > 0, 1, 0)

# --- adol_girl_iron_folic_acid_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.adol_girl_iron_folic_acid_needs	 <- hh_dataset ...	if (adol_girls_iron_folic_acid_tablets < females_10_19_HH_count, 1, adol_girls_iron_folic_acid_tablets == females_10_19_HH_count | females_10_19_HH_count == 0, 0, NA)

# --- nutrition_critical ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.nutrition_critical	 <- hh_dataset ...	max (i.child_nutrition_needs, i.child_bsfp_needs, na.rm = T only if maximum possible is reached)

# --- nutrition_non_critical_mean ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.nutrition_non_critical_mean	 <- hh_dataset ...	mean (i.nutrition_barrier_needs, i.child_nutrition_contact_needs, i.plw_nutrition_contact_needs, i.plw_iron_folic_acid_needs, i.adol_girl_iron_folic_acid_needs, na.rm = F)

# --- nutrition_non_critical ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.nutrition_non_critical	 <- hh_dataset ...	if (i.nutrition_non_critical_mean > 2/3, 3, if (i.nutrition_non_critical_mean > 1/3 & nutrition_non_critical_mean <= 2/3, 2, if(i.nutrition_non_critical_mean <= 1/3, 1, NA)))

# --- nutrition_overall_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.nutrition_overall_needs	 <- hh_dataset ...	max (i.nutrition_critical, i.nutrition_non_critical, na.rm = T only if maximum possible is reached)

# Food Security/Livelihoods ------------------------------------------------

# --- cereals_and_tubers ---
# [From hh to hh. HH that consumed this food group at least once
# hh_dataset$i.cereals_and_tubers	 <- hh_dataset ...	if (cereals_and_tubers > 0, 1, 0)

# --- pulses ---
# [From hh to hh. HH that consumed this food group at least once
# hh_dataset$i.pulses	 <- hh_dataset ...	if (pulses > 0, 1, 0)

# --- vegetables ---
# [From hh to hh. HH that consumed this food group at least once
# hh_dataset$i.vegetables	 <- hh_dataset ...	if (vegetables > 0, 1, 0)

# --- fruits ---
# [From hh to hh. HH that consumed this food group at least once
# hh_dataset$i.fruits	 <- hh_dataset ...	if (fruits > 0, 1, 0)

# --- milk_and_dairy ---
# [From hh to hh. HH that consumed this food group at least once
# hh_dataset$i.milk_and_dairy	 <- hh_dataset ...	if (milk_and_dairy > 0, 1, 0)

# --- meat_or_fish ---
# [From hh to hh. HH that consumed this food group at least once
# hh_dataset$i.meat_or_fish	 <- hh_dataset ...	if (meat_or_fish > 0, 1, 0)

# --- oil_and_fats ---
# [From hh to hh. HH that consumed this food group at least once
# hh_dataset$i.oil_and_fats	 <- hh_dataset ...	if (oil_and_fats > 0, 1, 0)

# --- sweets ---
# [From hh to hh. HH that consumed this food group at least once
# hh_dataset$i.sweets	 <- hh_dataset ...	if (sweets > 0, 1, 0)

# --- spices_and_condiments ---
# [From hh to hh. HH that consumed this food group at least once
# hh_dataset$i.spices_and_condiments	 <- hh_dataset ...	if (spices_and_condiments > 0, 1, 0)

# --- fcs_value ---
# [From hh to hh. FCS score (check WFP guidance for BGD)
# hh_dataset$i.fcs_value	 <- hh_dataset ...	cereals_and_tubers * 2 + pulses * 3 + vegetables + fruits + milk_and_dairy * 4 + meat_or_fish * 4  + sweets * 0.5 + oil_and_fats * 0.5

# --- fcs_result ---
# [From hh to hh. FCS categories (check WFP guidance for BGD)
# hh_dataset$i.fcs_result	 <- hh_dataset ...	if (i.fcs_value <= 28, 'Poor', if (i.fcs_value > 28 & i.fcs_value <= 42, 'Borderline', if (i.fcs_value > 42, 'Acceptable', NA)))

# --- fcs_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.fcs_needs	 <- hh_dataset ...	if (i.fcs_result == 'Poor', 4, if (i.fcs_result == 'Borderline', 3, if (i.fcs_result == 'Acceptable', 1, NA)))

# --- food_barriers ---
# [From hh to hh. HH facing any food distribution challenges
# hh_dataset$i.food_barriers	 <- hh_dataset ...	if (food_barriers != 'no_challenge' & food_barriers != 'dont_know', 1, 0)

# --- livelihood_coping ---
# [From hh to hh. HH having adopted any livelihoods-based coping strategy
# hh_dataset$i.livelihood_coping	 <- hh_dataset ...	if (at least 1 of the following == 'yes' or 'exhausted' - selling_hh_goods, selling_jewelry, spent_savings, bought_food_on_credit, borrowed_money, sold_productive_assets, reduced_nonessential_expenditures, begging, community_support, selling_food_rations, selling_nfis, selling_labor, child_working_long_hours, adults_working_long_hours, child_marriage, risk_jobs, hh_migration, 1, 0)

# --- emergency_coping ---
# [From hh to hh. Might need to be adjusted
# hh_dataset$i.emergency_coping	 <- hh_dataset ...	if (at least 1 of the following == 'yes' or 'exhausted' - begging, child_working_long_hours, child_marriage, risk_jobs, hh_migration, 1, 0)

# --- crisis_coping ---
# [From hh to hh. Might need to be adjusted
# hh_dataset$i.crisis_coping	 <- hh_dataset ...	if (at least 1 of the following == 'yes' or 'exhausted' - sold_productive_assets, reduced_nonessential_expenditures community_support, selling_food_rations, selling_nfis, selling_labor, adults_working_long_hours, 1, 0)

# --- stress_coping ---
# [From hh to hh. Might need to be adjusted
# hh_dataset$i.stress_coping	 <- hh_dataset ...	if (at least 1 of the following == 'yes' or 'exhausted' - selling_hh_goods, selling_jewelry, spent_savings, bought_food_on_credit, borrowed_money, selling_labor, 1, 0)

# --- livelihood_coping_needs ---  [For analysis]
# [From hh to hh. Might need to be adjusted
# hh_dataset$i.livelihood_coping_needs	 <- hh_dataset ...	if (i.emergency_coping == 1, 4, if (i.crisis_coping == 1, 3, if (i.stress_coping == 1, 1, if (all coping strategies are "no_need" or "not_available", NA))))

# --- assistance_amount ---
# [From hh to hh. Per capita income by source
# hh_dataset$i.assistance_amount	 <- hh_dataset ...	assistance_amount / hh_size

# --- cfw_amount ---
# [From hh to hh. Per capita income by source
# hh_dataset$i.cfw_amount	 <- hh_dataset ...	cfw_amount / hh_size

# --- salaried_work_amount ---
# [From hh to hh. Per capita income by source
# hh_dataset$i.salaried_work_amount	 <- hh_dataset ...	salaried_work_amount / hh_size

# --- casual_labor_amount ---
# [From hh to hh. Per capita income by source
# hh_dataset$i.casual_labor_amount	 <- hh_dataset ...	casual_labor_amount / hh_size

# --- business_amount ---
# [From hh to hh. Per capita income by source
# hh_dataset$i.business_amount	 <- hh_dataset ...	business_amount / hh_size

# --- government_benefits_amount ---  [host]
# [From hh to hh. Per capita income by source
# if(pop=="host"){
#    	hh_dataset$i.government_benefits_amount	 <- hh_dataset ...	government_benefits_amount / hh_size
# }

# --- remittances_amount ---
# [From hh to hh. Per capita income by source
# hh_dataset$i.remittances_amount	 <- hh_dataset ...	remittances_amount / hh_size

# --- family_friends_amount ---
# [From hh to hh. Per capita income by source
# hh_dataset$i.family_friends_amount	 <- hh_dataset ...	family_friends_amount / hh_size

# --- donations_amount ---
# [From hh to hh. Per capita income by source
# hh_dataset$i.donations_amount	 <- hh_dataset ...	donations_amount / hh_size

# --- sale_assistance_amount ---
# [From hh to hh. Per capita income by source
# hh_dataset$i.sale_assistance_amount	 <- hh_dataset ...	sale_assistance_amount / hh_size

# --- other_amount ---
# [From hh to hh. Per capita income by source
# hh_dataset$i.other_amount	 <- hh_dataset ...	other_amount / hh_size

# --- main_livelihood ---
# [From hh to hh. Identify main income source (largest amount earned)
# hh_dataset$i.main_livelihood	 <- hh_dataset ...	variable showing the one livelihood for each HH from which the largest amount was obtained (response selected under main_income_source with the largest corresponding amount)

# --- income_total ---  [refugee]
# [From hh to hh. Total per capita income
# if(pop=="refugee"){
#    	hh_dataset$i.income_total	 <- hh_dataset ...	sum of all income source amounts / hh_size
# }

# --- income_minus_assistance ---  [refugee]
# [From hh to hh. Total per capita income minus assistance
# if(pop=="refugee"){
#    	hh_dataset$i.income_minus_assistance	 <- hh_dataset ...	sum of all income source amounts excluding assistance_amount / hh_size
# }

# --- income_total ---  [host]
# [From hh to hh. Total per capita income
# if(pop=="host"){
#    	hh_dataset$i.income_total	 <- hh_dataset ...	sum of all income source amounts / hh_size
# }

# --- income_minus_assistance ---  [host]
# [From hh to hh. Total per capita income minus assistance
# if(pop=="host"){
#    	hh_dataset$i.income_minus_assistance	 <- hh_dataset ...	sum of all income source amounts excluding assistance_amount / hh_size
# }

# --- individuals_income ---
# [From hh to hh. Total number of individuals working in the HH
# hh_dataset$i.individuals_income	 <- hh_dataset ...	total number of individuals selected under individuals_income

# --- individuals_income_hh ---
# [From hh to hh. HH with working individuals
# hh_dataset$i.individuals_income_hh	 <- hh_dataset ...	if (i.individuals_income_hh > 0, 1, 0)

# --- males_working ---
# [From hh to hh. Total number of males working in HH
# hh_dataset$i.males_working	 <- hh_dataset ...	total number of individuals selected under individuals_income with ind_gender == 'male'

# --- females_working ---
# [From hh to hh. Total number of females working in HH
# hh_dataset$i.females_working	 <- hh_dataset ...	total number of individuals selected under individuals_income with ind_gender == 'female'

# --- children_working ---
# [From hh to hh. Total number of children aged 5-17 working in the HH
# hh_dataset$i.children_working	 <- hh_dataset ...	total number of individuals selected under individuals_income aged 5-17

# --- adults_working ---
# [From hh to hh. Total number of adults aged 18+ working in the HH
# hh_dataset$i.adults_working	 <- hh_dataset ...	total number of individuals selected under individuals_income aged >18

# --- adults_to_59_working ---
# [From hh to hh. Total number of adults aged 18-59 working in the HH
# hh_dataset$i.adults_to_59_working	 <- hh_dataset ...	total number of individuals selected under individuals_income aged 18-59

# --- elderly_working ---
# [From hh to hh. Total number of adults aged 60+ working in the HH
# hh_dataset$i.elderly_working	 <- hh_dataset ...	total number of individuals selected under individuals_income aged 60+

# --- children_working_hh ---
# [From hh to hh. HH with working children
# hh_dataset$i.children_working_hh	 <- hh_dataset ...	if (i.children_working > 0, 1, 0)

# --- adults_working_hh ---
# [From hh to hh. HH with working adults (18+)
# hh_dataset$i.adults_working_hh	 <- hh_dataset ...	if (i.adults_working > 0, 1, 0)

# --- adults_to_59_working_hh ---
# [From hh to hh. HH with working adults (18-59)
# hh_dataset$i.adults_to_59_working_hh	 <- hh_dataset ...	if (i.adults_to_59_working > 0, 1, 0)

# --- elderly_working_hh ---
# [From hh to hh. HH with working elderly
# hh_dataset$i.elderly_working_hh	 <- hh_dataset ...	if (i.elderly_working > 0, 1, 0)

# --- ind_5_above ---
# [From indv to indv. Individuals aged 5 and above
# indv_dataset$i.ind_5_above	 <- indv_dataset ...	if (individual_age > 4, 1, 0)

# --- ind_5_17 ---
# [From indv to indv. Individuals aged 5-17
# indv_dataset$i.ind_5_17	 <- indv_dataset ...	if (individual_age > 4 &  individual_age < 18, 1, 0)

# --- ind_18_plus ---
# [From indv to indv. Individuals aged 18+
# indv_dataset$i.ind_18_plus	 <- indv_dataset ...	if (individual_age > 17, 1, 0)

# --- food_expenditure ---
# [From hh to hh. Per capita expenditure
# hh_dataset$i.food_expenditure	 <- hh_dataset ...	food_expenditure / hh_size

# --- food_expenditure_any ---
# [From hh to hh. HH having spent money on food
# hh_dataset$i.food_expenditure_any	 <- hh_dataset ...	if (food_expenditure > 0, 1, 0)

# --- food_expenditure_cat ---
# [From hh to hh. HH having spent money on food reporting amount
# hh_dataset$i.food_expenditure_cat	 <- hh_dataset ...	Categorize: >0 - 500, > 500 - 1000, > 1000 - 2000, > 2000 - 5000, > 5000

# --- rent_expenditure ---
# [From hh to hh. Per capita expenditure
# hh_dataset$i.rent_expenditure	 <- hh_dataset ...	rent_expenditure / hh_size

# --- rent_expenditure_any ---
# [From hh to hh. HH having spent money on rent
# hh_dataset$i.rent_expenditure_any	 <- hh_dataset ...	if (rent_expenditure > 0, 1, 0)

# --- rent_expenditure_cat ---
# [From hh to hh. HH having spent money on rent reporting amount
# hh_dataset$i.rent_expenditure_cat	 <- hh_dataset ...	Categorize: >0 - 500, > 500 - 1000, > 1000 - 2000, > 2000 - 5000, > 5000

# --- freq_nfi_expenditure ---
# [From hh to hh. Per capita expenditure
# hh_dataset$i.freq_nfi_expenditure	 <- hh_dataset ...	freq_nfi_expenditure / hh_size

# --- freq_nfi_expenditure_any ---
# [From hh to hh. HH having spent money on frequent NFIs
# hh_dataset$i.freq_nfi_expenditure_any	 <- hh_dataset ...	if (freq_nfi_expenditure > 0, 1, 0)

# --- freq_nfi_expenditure_cat ---
# [From hh to hh. HH having spent money on frequent NFIs reporting amount
# hh_dataset$i.freq_nfi_expenditure_cat	 <- hh_dataset ...	Categorize: >0 - 500, > 500 - 1000, > 1000 - 2000, > 2000 - 5000, > 5000

# --- fuel_expenditure ---
# [From hh to hh. Per capita expenditure
# hh_dataset$i.fuel_expenditure	 <- hh_dataset ...	fuel_expenditure / hh_size

# --- fuel_expenditure_any ---
# [From hh to hh. HH having spent money on fuel
# hh_dataset$i.fuel_expenditure_any	 <- hh_dataset ...	if (fuel_expenditure > 0, 1, 0)

# --- fuel_expenditure_cat ---
# [From hh to hh. HH having spent money on fuel reporting amount
# hh_dataset$i.fuel_expenditure_cat	 <- hh_dataset ...	Categorize: >0 - 500, > 500 - 1000, > 1000 - 2000, > 2000 - 5000, > 5000

# --- transportation_expenditure ---
# [From hh to hh. Per capita expenditure
# hh_dataset$i.transportation_expenditure	 <- hh_dataset ...	transportation_expenditure / hh_size

# --- transportation_expenditure_any ---
# [From hh to hh. HH having spent money on transportation
# hh_dataset$i.transportation_expenditure_any	 <- hh_dataset ...	if (transportation_expenditure > 0, 1, 0)

# --- transportation_expenditure_cat ---
# [From hh to hh. HH having spent money on transportation reporting amount
# hh_dataset$i.transportation_expenditure_cat	 <- hh_dataset ...	Categorize: >0 - 500, > 500 - 1000, > 1000 - 2000, > 2000 - 5000, > 5000

# --- food_assistance_value ---
# [From hh to hh. Per capita expenditure
# hh_dataset$i.food_assistance_value	 <- hh_dataset ...	food_assistance_value / hh_size

# --- freq_nfi_assistance_value ---
# [From hh to hh. Per capita expenditure
# hh_dataset$i.freq_nfi_assistance_value	 <- hh_dataset ...	freq_nfi_assistance_value / hh_size

# --- fuel_assistance_value ---
# [From hh to hh. Per capita expenditure
# hh_dataset$i.fuel_assistance_value	 <- hh_dataset ...	fuel_assistance_value / hh_size

# --- transportation_assistance_value ---
# [From hh to hh. Per capita expenditure
# hh_dataset$i.transportation_assistance_value	 <- hh_dataset ...	transportation_assistance_value / hh_size

# --- shelter_expenditure ---
# [From hh to hh. Per capita expenditure
# hh_dataset$i.shelter_expenditure	 <- hh_dataset ...	shelter_expenditure / hh_size

# --- infreq_nfi_expenditure ---
# [From hh to hh. Per capita expenditure
# hh_dataset$i.infreq_nfi_expenditure	 <- hh_dataset ...	infreq_nfi_expenditure / hh_size

# --- health_expenditure ---
# [From hh to hh. Per capita expenditure
# hh_dataset$i.health_expenditure	 <- hh_dataset ...	health_expenditure / hh_size

# --- education_expenditure ---
# [From hh to hh. Per capita expenditure
# hh_dataset$i.education_expenditure	 <- hh_dataset ...	education_expenditure / hh_size

# --- livelihood_expenditure ---
# [From hh to hh. Per capita expenditure
# hh_dataset$i.livelihood_expenditure	 <- hh_dataset ...	livelihood_expenditure / hh_size

# --- debt_expenditure ---
# [From hh to hh. Per capita expenditure
# hh_dataset$i.debt_expenditure	 <- hh_dataset ...	debt_expenditure / hh_size

# --- food_expenditure_total ---
# [From hh to hh. Total per capita expenditure (with imputed value of assistance)
# hh_dataset$i.food_expenditure_total	 <- hh_dataset ...	sum (food_expenditure, food_assistance_value) / hh_size

# --- freq_nfi_expenditure_total ---
# [From hh to hh. Total per capita expenditure (with imputed value of assistance)
# hh_dataset$i.freq_nfi_expenditure_total	 <- hh_dataset ...	sum (freq_nfi_expenditure, freq_nfi_assistance_value) / hh_size

# --- fuel_expenditure_total ---
# [From hh to hh. Total per capita expenditure (with imputed value of assistance)
# hh_dataset$i.fuel_expenditure_total	 <- hh_dataset ...	sum (fuel_expenditure, fuel_assistance_value) / hh_size

# --- transportation_expenditure_total ---
# [From hh to hh. Total per capita expenditure (with imputed value of assistance)
# hh_dataset$i.transportation_expenditure_total	 <- hh_dataset ...	sum (transportation_expenditure, transportation_assistance_value) / hh_size

# --- shelter_expenditure_monthly ---
# [From hh to hh. Montly per capita expenditure of long-term expenditures
# hh_dataset$i.shelter_expenditure_monthly	 <- hh_dataset ...	shelter_expenditure / 3 / hh_size

# --- shelter_expenditure_any ---
# [From hh to hh. HH having spent money on shelter
# hh_dataset$i.shelter_expenditure_any	 <- hh_dataset ...	if (i.shelter_expenditure_monthly > 0, 1, 0)

# --- shelter_expenditure_cat ---
# [From hh to hh. HH having spent money on shelter reporting amount
# hh_dataset$i.shelter_expenditure_cat	 <- hh_dataset ...	Categorize: >0 - 500, > 500 - 1000, > 1000 - 2000, > 2000 - 5000, > 5000

# --- infreq_nfi_expenditure_monthly ---
# [From hh to hh. Montly per capita expenditure of long-term expenditures
# hh_dataset$i.infreq_nfi_expenditure_monthly	 <- hh_dataset ...	infreq_nfi_expenditure / 3 / hh_size

# --- infreq_nfi_expenditure_any ---
# [From hh to hh. HH having spent money on infreq_nfi
# hh_dataset$i.infreq_nfi_expenditure_any	 <- hh_dataset ...	if (i.infreq_nfi_expenditure_monthly > 0, 1, 0)

# --- infreq_nfi_expenditure_cat ---
# [From hh to hh. HH having spent money on infreq_nfi reporting amount
# hh_dataset$i.infreq_nfi_expenditure_cat	 <- hh_dataset ...	Categorize: >0 - 500, > 500 - 1000, > 1000 - 2000, > 2000 - 5000, > 5000

# --- health_expenditure_monthly ---
# [From hh to hh. Montly per capita expenditure of long-term expenditures
# hh_dataset$i.health_expenditure_monthly	 <- hh_dataset ...	health_expenditure / 3 / hh_size

# --- health_expenditure_any ---
# [From hh to hh. HH having spent money on health
# hh_dataset$i.health_expenditure_any	 <- hh_dataset ...	if (i.health_expenditure_monthly > 0, 1, 0)

# --- health_expenditure_cat ---
# [From hh to hh. HH having spent money on health reporting amount
# hh_dataset$i.health_expenditure_cat	 <- hh_dataset ...	Categorize: >0 - 500, > 500 - 1000, > 1000 - 2000, > 2000 - 5000, > 5000

# --- education_expenditure_monthly ---
# [From hh to hh. Montly per capita expenditure of long-term expenditures
# hh_dataset$i.education_expenditure_monthly	 <- hh_dataset ...	education_expenditure / 3 / hh_size

# --- education_expenditure_any ---
# [From hh to hh. HH having spent money on education
# hh_dataset$i.education_expenditure_any	 <- hh_dataset ...	if (i.education_expenditure_monthly > 0, 1, 0)

# --- education_expenditure_cat ---
# [From hh to hh. HH having spent money on education reporting amount
# hh_dataset$i.education_expenditure_cat	 <- hh_dataset ...	Categorize: >0 - 500, > 500 - 1000, > 1000 - 2000, > 2000 - 5000, > 5000

# --- livelihood_expenditure_monthly ---
# [From hh to hh. Montly per capita expenditure of long-term expenditures
# hh_dataset$i.livelihood_expenditure_monthly	 <- hh_dataset ...	livelihood_expenditure / 3 / hh_size

# --- livelihood_expenditure_any ---
# [From hh to hh. HH having spent money on livelihood
# hh_dataset$i.livelihood_expenditure_any	 <- hh_dataset ...	if (i.livelihood_expenditure_monthly > 0, 1, 0)

# --- livelihood_expenditure_cat ---
# [From hh to hh. HH having spent money on livelihood reporting amount
# hh_dataset$i.livelihood_expenditure_cat	 <- hh_dataset ...	Categorize: >0 - 500, > 500 - 1000, > 1000 - 2000, > 2000 - 5000, > 5000

# --- debt_expenditure_monthly ---
# [From hh to hh. Montly per capita expenditure of long-term expenditures
# hh_dataset$i.debt_expenditure_monthly	 <- hh_dataset ...	debt_expenditure / 3 / hh_size

# --- debt_expenditure_any ---
# [From hh to hh. HH having spent money on debt
# hh_dataset$i.debt_expenditure_any	 <- hh_dataset ...	if (i.debt_expenditure_monthly > 0, 1, 0)

# --- debt_expenditure_cat ---
# [From hh to hh. HH having spent money on debt reporting amount
# hh_dataset$i.debt_expenditure_cat	 <- hh_dataset ...	Categorize: >0 - 500, > 500 - 1000, > 1000 - 2000, > 2000 - 5000, > 5000

# --- nfi_expenditure_total ---
# [From hh to hh. Total monthly per capita NFI expenditure (may need to see proportion of NA)
# hh_dataset$i.nfi_expenditure_total	 <- hh_dataset ...	sum (i.rent_expenditure, i.freq_nfi_expenditure, i.fuel_expenditure, i.transportation_expenditure, i.freq_nfi_assistance_value, i.fuel_assistance_value, i.transportation_assistance_value, i.shelter_expenditure_monthly, i.infreq_nfi_expenditure_monthly, i.health_expenditure_monthly, i.education_expenditure_monthly, i.livelihood_expenditure_monthly, i.debt_expenditure_monthly, na.rm = F)

# --- nfi_expenditure_minus_assistance ---
# [From hh to hh. Total monthly per capita NFI expenditure excluding assistance (may need to see proportion of NA)
# hh_dataset$i.nfi_expenditure_minus_assistance	 <- hh_dataset ...	sum (i.rent_expenditure, i.freq_nfi_expenditure, i.fuel_expenditure, i.transportation_expenditure, i.shelter_expenditure_monthly, i.infreq_nfi_expenditure_monthly, i.health_expenditure_monthly, i.education_expenditure_monthly, i.livelihood_expenditure_monthly, i.debt_expenditure_monthly, na.rm = F)

# --- total_expenditure ---
# [From hh to hh. Total monthly per capita expenditure excluding assistance (may need to see proportion of NA)
# hh_dataset$i.total_expenditure	 <- hh_dataset ...	sum (i.food_expenditure_total, i.nfi_expenditure_total, na.rm = F)

# --- total_expenditure_minus_assistance ---
# [From hh to hh. Total monthly per capita expenditure (may need to see proportion of NA)
# hh_dataset$i.total_expenditure_minus_assistance	 <- hh_dataset ...	sum (i.food_expenditure, i.nfi_expenditure_minus_assistance, na.rm = F)

# --- meb_needs ---  [For analysis]
# [From hh to hh. Might need to be adjusted
# hh_dataset$i.meb_needs	 <- hh_dataset ...	if (i.total_expenditure < 1138, 4, if (i.total_expenditure >= 1138 & i.total_expenditure < 1736, 3, if (i.total_expenditure > 1736, 1, NA)))

# --- food_exp_share ---
# [From hh to hh. Food expenditure share
# hh_dataset$i.food_exp_share	 <- hh_dataset ...	i.food_expenditure_total / sum (i.food_expenditure_total, i.nfi_expenditure_total, na.rm = F)

# --- food_exp_share_minus_assistance ---
# [From hh to hh. Food expenditure share minus assistance
# hh_dataset$i.food_exp_share_minus_assistance	 <- hh_dataset ...	i.food_expenditure / sum (i.food_expenditure, i.nfi_expenditure_minus_assistance, na.rm = F)

# --- fsl_overall_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.fsl_overall_needs	 <- hh_dataset ...	max (i.fcsneeds, i.livelihood_coping_needs, i.meb_needs, na.rm = T only if maximum possible is reached)

# WASH ---------------------------------------------------------------------

# --- improved_water_source ---
# [From hh to hh. Categorize by improved vs unimproved water source
# hh_dataset$i.improved_water_source	 <- hh_dataset ...	if (water_sources is any of the following - piped_water_tap, deep_tubewell, protected_dugwell, protected_spring, rainwater_collection, bottled_water, cart_with_small_tank_or_drum, tanker_truck, 'improved', if (water_sources is any of the following - shallow_tubewell, tubewell_unknown, unprotected_dug_well, unprotected_spring, surface_water, 'unimproved', NA))

# --- enough_water ---
# [From hh to hh. HH not having enough water for at least 1 purpose
# hh_dataset$i.enough_water	 <- hh_dataset ...	if (at least 1 of the following == 'no' - enough_water_drinking, enough_water_cooking, enough_water_bathing_facility, enough_water_bathing_shelter, enough_water_other, 'not_enough', if (all those are 'yes' or 'not_applicable', 'enough', NA))

# --- enough_water_other_purposes ---
# [From hh to hh. HH not having enough water for other purposes (not drinking)
# hh_dataset$i.enough_water_other_purposes	 <- hh_dataset ...	if (at least 1 of the following == 'no' - enough_water_cooking, enough_water_bathing_facility, enough_water_bathing_shelter, enough_water_other, 'not_enough', if (all those are 'yes' or 'not_applicable', 'enough', NA))

# --- water_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.water_needs	 <- hh_dataset ...	if (i.improved_water_source == 'unimproved' & enough_water_drinking == 'no', 4, if ((i.improved_water_source == 'improved' & enough_drinking_water == 'no') | (i.improved_water_source == 'unimproved' & enough_drinking_water == 'yes'), 3, if ((i.improved_water_source == 'improved' & enough_drinking_water == 'yes') | i.enough_water_other_purposes == 'not_enough'), 2, if (i.improved_water_source == 'improved' & i.enough_water == 'enough', 1, NA)))

# --- water_coping ---
# [From hh to hh. HH having adopted at least one coping mechanism
# hh_dataset$i.water_coping	 <- hh_dataset ...	if (water_coping != 'no_issue' & water_coping != 'dont_know', 1, 0)

# --- improved_sanitation_facility ---
# [From hh to hh. Categorize by improved vs unimproved sanitation facility
# hh_dataset$i.improved_sanitation_facility	 <- hh_dataset ...	if (sanitation_facility is any of the following - pit_latrine_no_slab, open_hole, bucket, plastic_bag, hanging_toilet, none, 'unimproved', if (sanitation_facility is any of the following - flush_toilet, pit_latrine_slab, pit_vip, potties, NA))

# --- sanitation_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.sanitation_needs	 <- hh_dataset ...	if (sanitation_facility == 'none', 4, if (i.improved_sanitation_facility == 'unimproved', 3, if (i.improved_sanitation_facility == 'improved', 1, NA)))

# --- soap_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.soap_needs	 <- hh_dataset ...	if (soap == 'no', 3, if (soap == 'yes', 1, NA))

# --- sanitation_barriers_female ---
# [From hh to hh. HH reporting at least one issue for females
# hh_dataset$i.sanitation_barriers_female	 <- hh_dataset ...	if (sanitation_barriers_female != 'no_problem' & sanitation_barriers_female != 'dont_know', 1, 0)

# --- sanitation_barriers_male ---
# [From hh to hh. HH reporting at least one issue for males
# hh_dataset$i.sanitation_barriers_male	 <- hh_dataset ...	if (sanitation_barriers_male != 'no_problem' & sanitation_barriers_male != 'dont_know', 1, 0)

# --- sanitation_barrier_critical_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.sanitation_barrier_critical_needs	 <- hh_dataset ...	if (sanitation_barriers_female OR sanitation_barriers_male at least 1 of the following == 1 - lack_facilities, pwd_difficulties, elderly_difficulties, no_gender_segregation, wall_see_through, no_lock, female_access, harassment, 3, if (none of those == 1, 1, NA))

# --- sanitation_barrier_noncritical_needs ---  [For analysis]
# _dataset$i.sanitation_barrier_noncritical_needs	 <- _dataset ...	if (sanitation_barriers_female OR sanitation_barriers_male any of the following == 1 - not_functioning, too_far, difficult_access, fear_covid, safety_concerns, 1, if (ONLY the following == 1 - no_problem, unclean, menstrual_management, light_inside, light_outside, 0, NA))

# --- bathing_barriers_females ---  [refugee]
# [From hh to hh. HH reporting at least one issue for females
# if(pop=="refugee"){
#    	hh_dataset$i.bathing_barriers_females	 <- hh_dataset ...	if (bathing_barriers_females != 'no_problem' & bathing_barriers_females != 'dont_know', 1, 0)
# }

# --- bathing_barriers_males ---  [refugee]
# [From hh to hh. HH reporting at least one issue for males
# if(pop=="refugee"){
#    	hh_dataset$i.bathing_barriers_males	 <- hh_dataset ...	if (bathing_barriers_males != 'no_problem' & bathing_barriers_males != 'dont_know', 1, 0)
# }

# --- bathing_barrier_critical_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.bathing_barrier_critical_needs	 <- hh_dataset ...	if (bathing_barriers_female OR bathing_barriers_male at least 1 of the following == 1 - lack_facilities, pwd_difficulties, elderly_difficulties, no_gender_segregation, wall_see_through, no_lock, female_access, harassment, female_shared_facility, menstrual_management, 2, if (none of those == 1, 1, NA))

# --- bathing_barrier_noncritical_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.bathing_barrier_noncritical_needs	 <- hh_dataset ...	if (bathing_barriers_female OR bathing_barriers_male any of the following == 1 - not_functioning, too_far, difficult_access, fear_covid, safety_concerns, 2, if (ONLY the following == 1 - no_problem, unclean, menstrual_management, light_inside, light_outside, 0, NA))

# --- waste_management_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.waste_management_needs	 <- hh_dataset ...	if (bin_access == 'one_bin' | (bin_access == 'several_bins' & disposal == 'hh_bin_no_segregation') | (bin_access == 'communal_bin' & disposal == communal_bin_no_segregation) | bin_access == 'no' | disposal = 'behind_shelter', 1, if (none of those combinations & (bin_access == 'several_bins' & disposal == 'hh_bin_segregation') | (bin_access == 'communal_bin' & disposal == 'communal_bin_segregation') | disposal == 'compost', 0, NA))

# --- wash_critical ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.wash_critical	 <- hh_dataset ...	max (i.water_needs, i.sanitation_needs, i.soap_needs, i.sanitation_barrier_critical_needs, i.bathing_barrier_critical_needs, na.rm = T only if maximum possible is reached)

# --- wash_non_critical_mean ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.wash_non_critical_mean	 <- hh_dataset ...	mean (i.sanitation_barrier_noncritical_needs, i.bathing_barrier_noncritical_needs, i.waste_management_needs, na.rm = F)

# --- wash_non_critical ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.wash_non_critical	 <- hh_dataset ...	if (i.wash_non_critical_mean > 2/3, 3, if (i.wash_non_critical_mean > 1/3 & wash_non_critical_mean <= 2/3, 2, if(i.wash_non_critical_mean <= 1/3, 1, NA)))

# --- wash_overall_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.wash_overall_needs	 <- hh_dataset ...	max (i.wash_critical, i.wash_non_critical, na.rm = T only if maximum possible is reached)

# --- water_coping ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.water_coping	 <- hh_dataset ...	if (water_coping == 1 for at least 1 of the following - less_preferred_drinking, less_drinking, less_use_other, mix_water - or coping_reason == 'access_to_water', 1, if (ONLY the following == 1 - no_issue, less_preferred_other, further_away, spend_money & coping_reason != dont_know, buy_water & coping_reason != dont_know, 0, NA))

# --- sanitation_coping ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.sanitation_coping	 <- hh_dataset ...	if (sanitation_coping == 1 for at least 1 of the following - less_preferred_facility, plastic_bag, open_defecation, bathing_facility, not_going_at_night, latrines_other_facilities, bury_faeces, reducing_times, 1, if(ONLY the following == 1 - communal_latrines, futher_away, household_latrine - OR BOTH sanitation_barriers_male AND sanitation_barriers_female == no_problem (or no male/female in HH), 0, NA))

# --- hygiene_coping ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.hygiene_coping	 <- hh_dataset ...	if (coping_reason == 'access_to_hygiene_items', 1, if (coping_reason != 'access_to_hygiene_items' or no coping, 0, NA))

# --- wash_coping ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.wash_coping	 <- hh_dataset ...	max (i.water_coping, i.sanitation_coping, i.hygiene_coping, na.rm = T only if maximum possible is reached)

# --- wash_need_coping ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.wash_need_coping	 <- hh_dataset ...	if (i.wash_overall_needs == 1 | 2 & wash_coping_total == 1, 1, 0)

# Protection ---------------------------------------------------------------

# --- separated_children_hh ---
# [From indv to hh. HH with separated children
# hh_dataset$i.separated_children_hh	 <- indv_dataset ...	if (separated_children > 0, 1, 0)

# --- separated_children_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.separated_children_needs	 <- hh_dataset ...	if (i.separated_children_hh == 1 & separated_reason == 'marriage' | 'violence', 4, if (i.separated_children_hh == 1 & separated_reason == 'separated' | 'employment' | 'other', 3, if (i.separated_children_hh == 0 | children_count == 0, 1, NA)))

# --- girls_married_hh ---
# [From indv to hh. HH with married girls
# hh_dataset$i.girls_married_hh	 <- indv_dataset ...	if (girls_married > 0, 1, 0)

# --- boys_married_hh ---
# [From indv to hh. HH with married boys
# hh_dataset$i.boys_married_hh	 <- indv_dataset ...	if (boys_married > 0, 1, 0)

# --- children_married_hh ---
# [From indv to hh. HH with married children
# hh_dataset$i.children_married_hh	 <- indv_dataset ...	if (sum(girls_married, boys_married) > 0, 1, 0)

# --- children_married_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.children_married_needs	 <- hh_dataset ...	if (i.married_children_hh == 4, 1, if (i.married_children_hh == 0 | children_count == 0, 1, NA))

# --- children_working_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.children_working_needs	 <- hh_dataset ...	if (i.children_working_hh == 1, 3, if (i.children_working_hh == 0 | children_count == 0, 1, NA))

# --- children_working_long_hours_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.children_working_long_hours_needs	 <- hh_dataset ...	if (child_working_long_hours == 'yes' | 'exhausted', 4, if (child_working_long_hours == 'no_need' | 'not_available', 1, NA))

# --- safety_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.safety_needs	 <- hh_dataset ...	if (safety_cange == 'worse', 1, if (safety_change == 'improved', 0, NA))

# --- protection_service_barriers_accessed_hh ---
# [From hh to hh. HH having accessed services reporting any barrier
# hh_dataset$i.protection_service_barriers_accessed_hh	 <- hh_dataset ...	if (protection_service_barriers_accessed != 'no_issue' & protection_service_barriers_accessed != 'dont_know', 1, 0)

# --- protection_service_barriers_not_accessed_hh ---
# [From hh to hh. HH not having accessed services reporting any barrier
# hh_dataset$i.protection_service_barriers_not_accessed_hh	 <- hh_dataset ...	if (protection_service_barriers_not_accessed != 'no_issue' & protection_service_barriers_not_accessed != 'dont_know', 1, 0)

# --- protection_service_barriers_any ---
# [From hh to hh. As for health, combined version of the two questions
# hh_dataset$i.protection_service_barriers_any	 <- hh_dataset ...	merged responses of protectoin_service_barriers_accessed, protection_sercive_barriers_not_accessed

# --- protection_service_barriers_any_hh ---
# [From hh to hh. HH reporting any barrier
# hh_dataset$i.protection_service_barriers_any_hh	 <- hh_dataset ...	if (protection_service_barriers_any != 'no_issue' & protection_service_barriers_any != 'dont_know', 1, 0)

# --- protection_service_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.protection_service_needs	 <- hh_dataset ...	if (access_service == 'no' or access_service == 'yes' & (protection_service_barriers_accessed == 1 for at least 1 of the following - no_staff_covid, no_staff_other_reason, dont_know_where_report, lack_trust, pwd_difficulties & i.disability_hh > 1, elderly_difficulties & at least one individual_age >= 60, females_difficulties & at least 1 ind_gender == 'female'), 3, if (access_service == 'yes' & (protection_service_barriers_accessed != 1 for all the following - no_staff_covid, no_staff_other_reason, dont_know_where_report, lack_trust, pwd_difficulties (OR == 1 & i.disability_hh == 0), elderly_difficulties (OR == 1 & individual_age < 60 for all), females_difficulties (OR == 1 & ind_gender != 'female' for all)) | report_incident == 'no', 1, NA))

# --- protection_barriers_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.protection_barriers_needs	 <- hh_dataset ...	if (i.protection_service_barriers_accessed_hh == 1, 1, if (i.protection_service_barriers_accessed_hh == 0, 0, NA))

# --- unsafe_areas_male_hh ---
# [From hh to hh. HH reporting any area where men feel unsafe
# hh_dataset$i.unsafe_areas_male_hh	 <- hh_dataset ...	if (unsafe_areas_male != 'no_area' & unsafe_areas_male != 'dont_know', 1, 0)

# --- unsafe_areas_female_hh ---
# [From hh to hh. HH reporting any area where women feel unsafe
# hh_dataset$i.unsafe_areas_female_hh	 <- hh_dataset ...	if (unsafe_areas_female != 'no_area' & unsafe_areas_female != 'dont_know', 1, 0)

# --- unsafe_areas_critical_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.unsafe_areas_critical_needs	 <- hh_dataset ...	if (unsafe_areas_male OR unsafe_areas_female == 1 for at least 1 of the following - latrines, distribution_points, water_points, homes, communal_shelters, 3, if (ONLY the folllowing == 1 - no_area, markets, community_areas, friends_house, community_kitchen, forest_open_space_farm, on_way, transportation, collect_firewood, other - OR no males/females in HH, 1, NA))

# --- unsafe_areas_noncritical_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.unsafe_areas_noncritical_needs	 <- hh_dataset ...	if (i.unsafe_areas_male_hh == 1 | i.unsafe_areas_female_hh == 1, 1, if (i.unsafe_areas_male_hh == 0 & i.unsafe_areas_female_hh == 0, 0, NA))

# --- gbv_support_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.gbv_support_needs	 <- hh_dataset ...	if (gbv_organisation ONLY the following - majhi, cic, community_based_dispute_resolution_mechanisms, police_and_security, legal_aid_service_providers, nowhere, 1, if (at least 1 of the following - health_facilities, psychosocial_service_providers, ombudsman, women_friendly_spaces, family_relatives, 0, NA))

# --- child_needs_hh ---
# [From hh to hh. HH reporting any unmet needs of children
# hh_dataset$i.child_needs_hh	 <- hh_dataset ...	if (child_needs != 'all_met' & child_needs != 'dont_know', 1, 0)

# --- child_critical_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.child_critical_needs	 <- hh_dataset ...	if (child_needs == 1 for at least 1 of the following - safety, food, shelter, alternative_care, healthcare, 3, if (ONLY the following == 1 - all_met, psychosocial_support, education, child_protection, play_areas, other - OR children_count == 0, 1, NA))

# --- child_noncritical_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.child_noncritical_needs	 <- hh_dataset ...	if (i.child_needs_hh == 1, 1, if (i.child_needs_hh == 0, 0, NA))

# --- protection_critical ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.protection_critical	 <- hh_dataset ...	max (i.separated_children_needs, i.children_married_needs, i.children_working_needs, i.children_working_long_hours_needs, i.protection_service_needs, i.child_critical_needs, i.unsafe_areas_critical_needs, na.rm = T only if maximum possible is reached)

# --- protection_non_critical_mean ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.protection_non_critical_mean	 <- hh_dataset ...	mean (i.safety_needs, i.protection_barrier_needs, i.child_noncritical_needs, i.gbv_support_needs, i.unsafe_areas_noncritical_needs, na.rm = F)

# --- protection_non_critical ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.protection_non_critical	 <- hh_dataset ...	if (i.protection_non_critical_mean > 2/3, 3, if (i.protection_non_critical_mean > 1/3 & protection_non_critical_mean <= 2/3, 2, if(i.protection_non_critical_mean <= 1/3, 1, NA)))

# --- protection_overall_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.protection_overall_needs	 <- hh_dataset ...	max (i.protection_critical, i.protection_non_critical, na.rm = T only if maximum possible is reached)

# CwC ----------------------------------------------------------------------

# --- information_need ---
# [From hh to hh. HH reporting at least one service on which they didn't have enough information
# hh_dataset$i.information_need	 <- hh_dataset ...	if (at least 1 of the following == 'no' - food_assistance, livelihood, water, sanitation, shelter, non_food_items, health_services, nutrition_services, remote_education, protection, site_management, 1, 0)

# --- information_barriers_hh ---
# [From hh to hh. HH reporting at least 1 barrier accessing information
# hh_dataset$i.information_barriers_hh	 <- hh_dataset ...	if (information_barriers != 'no_problem' & information_barriers != 'dont_know', 1, 0)

# --- feedback_hh ---
# [From hh to hh. HH reporting at least 1 challenge providing feedback
# hh_dataset$i.feedback_hh	 <- hh_dataset ...	if (feedback != 'no_challenge' & feedback != 'dont_know', 1, 0)

# Priority needs -----------------------------------------------------------

# --- protection_needs ---
# [From hh to hh. As for health, combined version of the two questions
# hh_dataset$i.protection_needs	 <- hh_dataset ...	merged responses of protection_needs_indicated, protection_needs_not_indicated

# --- overall_needs ---  [For analysis]
# [From hh to hh.
# hh_dataset$i.overall_needs	 <- hh_dataset ...	max (i.snfi_overall_needs, i.health_overall_needs, i.education_overall_needs, i.nutrition_overall_needs, i.fsl_overall_needs, i.wash_overall_needs, i.protection_overall_needs, na.rm = T only if maximum possible is reached)

# Cross-sectoral -----------------------------------------------------------

# --- language_barriers_services ---
# [From hh to hh. HH reporting language barriers when accessing different services by type of service
# hh_dataset$i.language_barriers_services	 <- hh_dataset ...	new kind of multiple select variable showing as 1 for the different services if information_barriers == language_that_i_don’t_understand, feedback == could_not_communicate_due_to_language, distance_learning_barriers_girls == 'language_barriers', distance_learning_barriers_boys == 'language_barriers', send_back_barriers_girls == 'language_barriers', send_back_barriers_boys == 'language_barriers', food_barriers == 'language_issue', i.health_barriers_any == 'language_barrier_with_health_service_staff', i.nutrition_barriers_any == 'language_barriers', i.protection_service_barriers_any == 'language_issue'

# --- too_far_services ---
# [From hh to hh. HH reporting services being too far
# hh_dataset$i.too_far_services	 <- hh_dataset ...	new kind of multiple select variable showing as 1 for the different services if food_barriers == 'too_far', i.health_barriers_any == 'health_services_are_too_far_away' | 'no_functional_facility_nearby', i.nutrition_barriers_any == 'too_far', i.protection_service_barriers_any == 'too_far', sanitation_barriers_ male OR sanitation_barriers_female == 'too_far', bathing_barriers_male OR bathing_barriers_female == 'too_far'

# --- security_concerns_services ---
# [From hh to hh. HH reporting security concerns when accessing services
# hh_dataset$i.security_concerns_services	 <- hh_dataset ...	new kind of multiple select variable showing as 1 for the different services if send_back_barriers_girls == 'security_travel', send_back_barriers_boys == 'security_travel', food_barriers == 'security_concerns', i.health_barriers_any == 'safety_concerns_at_facility' | 'safety_concerns_on_the_way_to_facilities' | 'security_concerns_at_night', i.nutrition_barriers_any == 'safety_concerns', i.protection_service_barriers_any == 'security_concerns', sanitation_barriers_ male OR sanitation_barriers_female == 'safety_concerns' | 'harassment', bathing_barriers_male OR bathing_barriers_female == 'safety_concerns' | 'harassment'

# --- covid_concerns_services ---
# [From hh to hh. HH reporting COVID concerns when accessing services
# hh_dataset$i.covid_concerns_services	 <- hh_dataset ...	new kind of multiple select variable showing as 1 for the different services if send_back_barriers_girls == 'covid_risk', send_back_barriers_boys == 'covid_risk', food_barriers == 'risk_covid', i.health_barriers_any == 'fear_of_contracting_covid_19_at_the_health_center' | 'fear_of_contracting_covid_19_on_the_way', i.nutrition_barriers_any == 'covid_fear', i.protection_service_barriers_any == 'fear_covid', sanitation_barriers_ male OR sanitation_barriers_female == 'fear_covid', bathing_barriers_male OR bathing_barriers_female == 'fear_covid'

# --- lack_awareness ---
# [From hh to hh. HH reporting lack of awareness
# hh_dataset$i.lack_awareness	 <- hh_dataset ...	new kind of multiple select variable showing as 1 for the different services if information_barriers == 'didnt_know_where_to_get_information', feedback == 'did_not_know_where' | 'process_is_too_complicated', distance_learning_barriers_girls == 'hh_unaware_opportunities', distance_learning_barriers_boys == 'hh_unaware_opportunities', send_back_barriers_girls == 'hh_unaware_opportunities', send_back_barriers_boys == 'hh_unaware_opportunities', food_barriers == 'unclear_entitlements', i.health_barriers_any == 'dont_know_where', i.nutrition_barriers_any == 'dont_know_where' | 'does_not_believed_malnourished' | 'dont_believe_in_treatment', i.protection_service_barriers_any == 'dont_know_where_report' | 'dont_understand_process'

# --- inaccessibility ---
# [From hh to hh. HH reporting inaccessibility
# hh_dataset$i.inaccessibility	 <- hh_dataset ...	new kind of multiple select variable showing as 1 for the different services if send_back_barriers_girls == 'inaccessibility', send_back_barriers_boys == 'inaccessibility', food_barriers == 'inaccessibility', i.health_barriers_any == 'inaccessibility', i.nutrition_barriers_any == 'inaccessibility', i.protection_service_barriers_any == 'inaccessibility', sanitation_barriers_ male OR sanitation_barriers_female == 'difficult_access', bathing_barriers_male OR bathing_barriers_female == 'difficult_access'

# --- lack_trust ---
# [From hh to hh. HH reporting lack of trust
# hh_dataset$i.lack_trust	 <- hh_dataset ...	new kind of multiple select variable showing as 1 for the different services if feedback == 'had_fear_about_confidentiality', i.health_barriers_any == 'distrust', i.nutrition_barriers_any == 'dont_trust_recommendation', i.protection_service_barriers_any == 'lack_trust'

