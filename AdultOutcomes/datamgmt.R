################################################################################
## Data management for outcomes analysis
##
## Summary of hard coded data fixes can be found in datamgmt_fixes.txt.
################################################################################

## Library calls
library(tidyverse)
library(stringi)

## -- Read in raw data downloaded from REDCap, create and rename factors -------
source("RawData/demog_redcap.R")
names(demog) <- gsub("_+", "_", gsub("\\.factor$", "_f", names(demog)))

source("RawData/compliance_redcap.R")
names(compliance) <-
  gsub("_+", "_", gsub("\\.factor$", "_f", names(compliance)))

## Raw data for ICU type via Christina (edited and saved to CSV by me)
icutypes <- read_csv(
  "RawData/iculib_icutypes.csv",
  col_names = c("hosp_f", "icu_type")
)

## Make sure every site has a type listed
# icutypes$hosp_f %in% unique(demog$hosp_f)
# unique(demog$hosp_f) %in% icutypes$hosp_f

## Raw data for hospital type, region via Christina (edited, saved to CSV by me)
hosp_info <- read_csv("RawData/hospital_types_regions.csv") %>%
  filter(hosp.code != '') %>%
  mutate(hosp.type = factor(gsub(' +', '', hosp.type)),
         hosp.name = gsub(' +', '', hosp.name)) %>%
  rename(hosp.f = hosp.redcap)
names(hosp_info) <- gsub(".", "_", names(hosp_info), fixed = TRUE)

## -- We're only going to use Months 1-20 (length of official Collab). ---------
## -- Add Month to compliance data and restrict both datasets. -----------------
keep_months <- paste("Month", 1:20)

compliance <-
  right_join(subset(demog, select = c(id, month_f)), compliance, by = "id") %>%
  filter(month_f %in% keep_months)

demog <- filter(demog, month_f %in% keep_months)

## -- Data management for demographic form -------------------------------------
demog <- demog %>%
  ## Merge on ICU type, hospital info from Christina
  left_join(icutypes, by = "hosp_f") %>%
  left_join(hosp_info, by = "hosp_f") %>%
  mutate(
    ## ICU type - combine as listed by Michele:
    ## - General: ICU, medical & surgical ICU, critical care unit, adult ICU,
    ##            + adult critical care unit??
    ## - Medical: Medical ICU, medical critical care unit
    ## - Surgical/trauma: Surgical ICU, trauma and life support center,
    ##                    + trauma ICU???
    ## - Neuro: Neuro ICU
    ## - Cardiac/surgery: Adult surgical heart unit, cardiac surgical ICU,
    ##                    cardiothoracic ICU, cardiovascular ICU
    ## - Other cardiac: cardio neuro ICU, cardio ICU
    icu_type_comb = factor(
      case_when(icu_type %in% c("ICU", "Medical & Surgical ICU",
                                "Critical Care Unit", "Adult ICU",
                                "Adult Critical Care Unit",
                                "Trauma and Life Support Center") ~ 1,
                icu_type %in% c("Medical ICU", "Medical Critical Care Unit") ~ 2,
                icu_type %in% c("Surgical ICU", "Trauma ICU") ~ 3,
                icu_type %in% c("Neuro ICU") ~ 4,
                icu_type %in% c("Adult Surgical Heart Unit",
                                "Cardiac Surgical ICU", "Cardiothoracic ICU",
                                "Cardiovascular ICU") ~ 5,
                icu_type %in% c("Cardio-Neuro ICU", "Cardio ICU") ~ 6,
                TRUE ~ 7),
      levels = 1:7,
      labels = c("Mixed medical/surgical",
                 "Medical",
                 "Surgical/trauma",
                 "Neuro",
                 "Cardiac/surgical",
                 "Cardiac",
                 "Unclassified")
    ),
    ## Age: combine two youngest, two oldest categories for model
    age_cat_mod = factor(
      ifelse(is.na(age_f), NA,
      ifelse(age_f %in% c("< 18", "18-29"), 1,
      ifelse(age_f == "30-39", 2,
      ifelse(age_f == "40-49", 3,
      ifelse(age_f == "50-59", 4,
      ifelse(age_f == "60-69", 5,
      ifelse(age_f == "70-79", 6, 7))))))),
      levels = 1:7,
      labels = c("<=29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
    ),
    ## Race
    race_cat = factor(ifelse(race_1 + race_2 + race_3 + race_4 + race_5 + race_6 > 1, 7,
                      ifelse(race_1, 1,
                      ifelse(race_2, 2,
                      ifelse(race_3, 3,
                      ifelse(race_4, 4,
                      ifelse(race_5, 5,
                      ifelse(race_6, 6, 8))))))),
                      levels = 1:8,
                      labels = c("American Indian/Alaskan Native",
                                 "Black/African-American",
                                 "White",
                                 "Asian",
                                 "Native Hawaiian/Pacific Islander",
                                 "Other or not specified",
                                 "Multiple races",
                                 "No race data entered")),
    ## For models, combine categories with <1% and make "no data" NA
    ## 1 = White (mode) (option 3)
    ## 2 = AA/Black (option 2)
    ## 3 = Asian (option 4)
    ## 4 = Other, specified (American Indian/AK native, HI/Pac Islander) (1, 5)
    ## 5 = Other, multiple races, or not specified (but documented) (6, sum > 1)
    race_cat_mod = factor(
      ifelse(race_6 | (race_1 + race_2 + race_3 + race_4 + race_5 + race_6 > 1),
             5,
      ifelse(race_1 | race_5, 4,
      ifelse(race_2, 2,
      ifelse(race_3, 1,
      ifelse(race_4, 3,
             NA))))),
      levels = 1:5,
      labels = c("White",
                 "Black/African-American",
                 "Asian",
                 "Amer Indian/AK Native or HI/Pacific Isl",
                 "Other race, multiple races, or not specified")
    ),

    ## Ethnicity
    hisp_cat = factor(ifelse(is.na(hispanic_f), 3,
                      ifelse(hispanic_f == "Yes", 1, 2)),
                      levels = 1:3,
                      labels = c("Hispanic",
                                 "Non-Hispanic",
                                 "No ethnicity entered")),
    ## Version for model - "none entered" = NA
    hisp_cat_mod = factor(ifelse(is.na(hispanic_f), NA,
                          ifelse(hispanic_f == "Yes", 1, 2)),
                          levels = 1:2,
                          labels = c("Hispanic", "Non-Hispanic")),

    ## BMI wt (kg) / [height in meters]^2
    ht = as.numeric(ht),
    wt = as.numeric(wt),
    ## Some of these heights look weird. After discussion with clinicians, we
    ## assume anything below 80 was accidentally entered in inches (80" = 6.5
    ## feet; amputees could legitimately be <90cm), and convert to cm.
    ht = ifelse(ht < 80, ht * 2.54, ht),
    bmi = ifelse(is.na(ht) | ht == 0 | is.na(wt) | wt == 0, NA,
                 wt / ((ht / 100)^2)),
    ## Change implausible BMIs to NA. We chose a minimum of 7 (based on anorexia
    ## data) and a maximum of 204 (highest ever recorded).
    bmi = ifelse(bmi < 7 | bmi > 204, NA, bmi),

    ## Severity of illness:
    ## - Indicator for whether any score available
    ## - Variable denoting _which_ score available
    ## - Describe levels of each score as available
    any_soi = factor(
      as.numeric(rowSums(demog[,paste0("severityscale_", c(1:4, 6))]) > 0),
      levels = 0:1,
      labels = c("No severity scale used", ">=1 severity scale")
    ),
    apacheii_avail = factor(severityscale_1,
                            levels = 0:1,
                            labels = c("None", "Yes")),
    apacheii_score = ifelse(apacheii_avail == "Yes", severityscore_a2, NA),
    apacheiii_avail = factor(severityscale_2,
                            levels = 0:1,
                            labels = c("None", "Yes")),
    apacheiii_score = ifelse(apacheiii_avail == "Yes", severityscore_a3, NA),
    apacheiv_avail = factor(severityscale_6,
                            levels = 0:1,
                            labels = c("None", "Yes")),
    apacheiv_score = ifelse(apacheiv_avail == "Yes", severityscore_a4, NA),
    sofa_avail = factor(severityscale_3,
                        levels = 0:1,
                        labels = c("None", "Yes")),
    sofa_score = ifelse(sofa_avail == "Yes", severityscore_sofa, NA),
    othersoi_avail = factor(severityscale_4,
                            levels = 0:1,
                            labels = c("No", "Yes")),

    ## Residency and mobility
    home_preadmit = factor(as.numeric(ifelse(is.na(residencypreadmit_f), NA,
                                             !(residencypreadmit_f == "Home"))),
                           levels = 0:1,
                           labels = c("Home pre-admission",
                                      "Facility pre-admission")),
    home_posticu = factor(as.numeric(ifelse(is.na(dc_loc_f), NA,
                                            !(dc_loc_f == "Home"))),
                           levels = 0:1,
                           labels = c("Home post-ICU", "Facility post-ICU")),

    mob_preadmit = factor(
      as.numeric(ifelse(is.na(mobilityadmit_f) |
                          mobilityadmit_f == "Mobility ability not documented",
                        NA,
                        mobilityadmit_f == "Mobility restrictions")),
      levels = 0:1,
      labels = c("No restrictions pre-admission",
                 "Mobility restricted pre-admission")
    ),
    mob_posticu = factor(
      as.numeric(ifelse(is.na(dc_mobility_f) |
                          dc_mobility_f == "Mobility ability not documented", NA,
                        dc_mobility_f == "Mobility restrictions")),
      levels = 0:1,
      labels = c("No restrictions post-ICU", "Mobility restricted post-ICU")
    ),

    ## Ever received invasive, noninvasive MV?
    ever_invas_mv = factor(ifelse(is.na(invas_vent_episode), NA,
                           ifelse(invas_vent_episode > 0, 1, 0)),
                           levels = 0:1, labels = c('No', 'Yes')),

    ever_noninvas_mv = factor(ifelse(is.na(noninvas_vent_episode), NA,
                              ifelse(noninvas_vent_episode > 0, 1, 0)),
                              levels = 0:1, labels = c('No', 'Yes')),

    ## Ever readmitted to the ICU?
    ever_readmit_icu = factor(
      as.numeric(ifelse(is.na(icu_readmit), NA, icu_readmit > 0)),
      levels = 0:1, labels = c("Never", ">=1 readmission")
    ),

    ## Discharged from the hospital alive?
    dc_alive = factor(
      as.numeric(ifelse(is.na(dc_status_f), NA,
                        dc_status_f == "Discharged from the hospital alive")),
      levels = 0:1,
      labels = c("Died in hospital", "Discharged alive")
    )
  ) %>%
  rename(hosp_los = hosp_losv)

## Total time on invasive, noninvasive MV
demog$hrs_invas_mv <- with(demog, {
  ifelse(is.na(invas_vent_episode) | invas_vent_episode == 0, NA,
         rowSums(demog[,paste0('invas_vent_length_', 1:6)], na.rm = TRUE)) })

demog$hrs_noninvas_mv <- with(demog, {
  ifelse(is.na(noninvas_vent_episode) | noninvas_vent_episode == 0, NA,
         rowSums(demog[,paste0('noninvas_vent_length_', 1:6)], na.rm = TRUE)) })

## Write BMIs < 10 or > 100 to CSV for clinicians to check.
subset(demog, bmi > 100 | bmi < 10, select = c(id, ht, wt, bmi)) %>%
  arrange(bmi) %>%
  write_csv(path = "check_bmis.csv")

# ## Look at ICU types
# ggplot(data = demog, aes(x = icu_type_comb)) +
#   geom_bar(stat = "count") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

## -- Admission diagnoses ------------------------------------------------------
## Categories from MB, agreed to by others:
## Sepsis/septic shock (option 1: "Sepsis /Septic shock") *OR*
##   ARDS without infection (option 2: "ARDS without infection")
## Respiratory (airway protection/obstruction, COPD/asthma, Pneumonia, PE/DVT)
## - option 3: "Airway protection/obstruction"
## - option 4: "COPD / Asthma"
## - option 5: "Pneumonia"
## - option 6: "Pulmonary embolism/DVT"
## Neurologic (neurological disease, TBI, change in mental status)
## - option 18: "Neurological disease"
## - option 19: "Traumatic Brain Injury"
## - option 20: "Change in Mental Status"
## - option 28: "Traumatic Brain Injury (isolated)"
## Cardiac (CHF, Acute MI/cardiogenic shock arrhythmia)
## - option 7: "Congestive Heart Failure"
## - option 8: "Acute MI/Cardiogenic shock"
## - option 9: "Arrhythmia"
## Gastrointestinal (GI bleed, cirrhosis/hepatic failure, pancreatitis)
## - option 11: "GI Bleed"
## - option 15: "Cirrhosis/Hepatic failure"
## - option 17: "Pancreatitis"
## Genitourinary (renal failure, metabolic/endocrine/electrolyte disturbances)
## - option 12: "Renal Failure"
## - option 14: "Metabolic / Endocrine / Electrolyte Disturbances"
## Surgery (vascular, abdominal, transplant, urologic, orthopedic, ENT, OB/GYN)
## - option 21: "Vascular surgery"
## - option 22: "Abdominal surgery"
## - option 23: "Transplant surgery"
## - option 24: "Urologic surgery"
## - option 25: "Orthopedic surgery"
## - option 26: "ENT surgery"
## - option 27: "OB/GYN surgery"
## Trauma (multi trauma, hemorrhagic shock)
## - option 29: "Multi-Trauma"
## - option 13: "Hemorrhagic shock"
## Drug overdose/withdrawal (option 30)
## Other (other infectious diseases, malignancy, other)
## - option 31: "Other Infectious Diseases"
## - option 16: "Malignancy"
## - option 99: "Other"

## Function to determine if any of specified diagnosis options are checked
sum_admitdx <- function(dxnums, df = demog){
  if(length(dxnums) == 1){
    df[,paste0("icu_dx_", dxnums)] > 0
  } else{
    rowSums(df[,paste0("icu_dx_", dxnums)]) > 0
  }
}

## Create indicators for each diagnosis category
demog$admit_sepsis <- sum_admitdx(1)
demog$admit_ards <- sum_admitdx(2)
demog$admit_resp <- sum_admitdx(3:6)
demog$admit_neuro <- sum_admitdx(c(18:20, 28))
demog$admit_cardiac <- sum_admitdx(7:9)
demog$admit_gi <- sum_admitdx(c(11, 15, 17))
demog$admit_gu <- sum_admitdx(c(12, 14))
demog$admit_surg <- sum_admitdx(21:27)
demog$admit_trauma <- sum_admitdx(c(29, 13))
demog$admit_odwd <- sum_admitdx(30)
demog$admit_other <- sum_admitdx(c(31, 16, 99))

# Data checks
# ## Function to make sure admission diagnoses are captured correctly
# check_admissiondx <- function(dxnums, dxvar, df = demog){
#   tmp <- demog[,c(paste0("icu_dx_", dxnums), dxvar)] %>%
#     gather(key = raw_dx, value = had_dx, starts_with("icu_dx")) %>%
#     filter(had_dx == 1)
#
#   ggplot(data = tmp, aes(x = raw_dx)) +
#     facet_wrap(as.formula(paste("~", dxvar))) +
#     geom_bar(stat = "count")
# }

# check_admissiondx(dxnums = 1, dxvar = "admit_sepsis")
# check_admissiondx(dxnums = 2, dxvar = "admit_ards")
# check_admissiondx(dxnums = 3:6, dxvar = "admit_resp")
# check_admissiondx(dxnums = c(18:20, 28), dxvar = "admit_neuro")
# check_admissiondx(dxnums = 7:9, dxvar = "admit_cardiac")
# check_admissiondx(dxnums = c(11, 15, 17), dxvar = "admit_gi")
# check_admissiondx(dxnums = c(12, 14), dxvar = "admit_gu")
# check_admissiondx(dxnums = 21:27, dxvar = "admit_surg")
# check_admissiondx(dxnums = c(29, 13), dxvar = "admit_trauma")
# check_admissiondx(dxnums = c(30), dxvar = "admit_odwd")
# check_admissiondx(dxnums = c(31, 16, 99), dxvar = "admit_other")

# ## How many patients had *multiple* admission diagnosis categories?
# ## Make an upset plot (http://vcg.github.io/upset/)
#
# admitdx_cols <- grep("^admit_", colnames(demog), value = TRUE)
# upset_list <- map(admitdx_cols, ~ demog[demog[,.], "id"])
# names(upset_list) <- admitdx_cols
#
# pdf(file = "admissiondx_upset.pdf", width = 9, height = 7)
# UpSetR::upset(
#   UpSetR::fromList(upset_list),
#   order.by = "freq",
#   nsets = length(admitdx_cols)
# )
# dev.off()

# table(rowSums(demog[,admitdx_cols]))

## Calculate final "primary admission diagnosis" variable, prioritizing
## diagnoses as follows:
## 1. Sepsis/septic shock *or* ARDS
## 2. Respiratory
## 3. Neurologic
## 4. Cardiac
## 5. GI
## 6. Trauma
## 7. Genitourinary
## 8. Surgery
## 9. Overdose/withdrawal
## 9. Other (will use this as reference - most common overall)
demog$primary_admit <- with(demog, {
  factor(ifelse(admit_sepsis | admit_ards, 2,
         ifelse(admit_resp, 3,
         ifelse(admit_neuro, 4,
         ifelse(admit_cardiac, 5,
         ifelse(admit_gi, 6,
         ifelse(admit_trauma, 7,
         ifelse(admit_gu, 8,
         ifelse(admit_surg, 9,
         ifelse(admit_odwd, 10,
         ifelse(admit_other, 1, NA)))))))))),
         levels = 1:10,
         labels = c("Other",
                    "Sepsis/septic shock or ARDS",
                    "Respiratory",
                    "Neurologic",
                    "Cardiac",
                    "GI",
                    "Trauma",
                    "Genitourinary",
                    "Surgery",
                    "Overdose/withdrawal"))
})

# ggplot(data = demog, aes(x = primary_admit)) +
#   geom_bar(stat = "count") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ## What categories make up most of "other"?
# otherdx <- demog %>%
#   filter(primary_admit == "Other") %>%
#   dplyr::select(paste0("icu_dx_", c(30, 31, 16, 99))) %>%
#   gather(key = other_type, other_has) %>%
#   filter(other_has == 1) %>%
#   mutate(other_type_f = factor(
#     ifelse(other_type == "icu_dx_31", 1,
#     ifelse(other_type == "icu_dx_16", 2,
#     ifelse(other_type == "icu_dx_99", 3, NA))),
#     levels = 1:3,
#     labels = c("Other infectious diseases",
#                "Malignancy",
#                "Other")
#   )
#   )
#
# ggplot(data = otherdx, aes(x = other_type_f)) +
#   geom_bar(stat = "count") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

## -- Data management for compliance form --------------------------------------
## Variables related to each bundle element done in separate chunks for easier
## changes later.
##
## Create all indicators as logicals (TRUE/FALSE); this saves space here and
## will make defining overall compliance/performance variables easier. Later,
## for some, we'll make them factors with specific labels.

## -- A: Assess, prevent, and manage pain --------------------------------------
compliance <- compliance %>%
  mutate(
    ## Indicator for whether a day should be included (must be documented as a
    ## full ICU day)
    icu_day = !is.na(icu_24_f) & icu_24_f == "Yes",

    ## Compliance: At least one of pain_valid and pain_verbal are present, and
    ## their total is >=6
    pain_asmts = ifelse(!is.na(pain_verbal) & !is.na(pain_valid),
                        pain_verbal + pain_valid,
                 ifelse(!is.na(pain_verbal), pain_verbal,
                 ifelse(!is.na(pain_valid), pain_valid, NA))),
    pain_asmts_icu = ifelse(!icu_day, NA, pain_asmts),

    comp_a = ifelse(!icu_day, NA,
             ifelse(is.na(pain_asmts), FALSE,
                    pain_asmts >= 6)),

    ## Performance: Same definition as compliance
    perf_a = comp_a,

    ## Any pain assessment done: At least one of pain_valid and pain_verbal
    ## are present and > 0
    had_painasmt = !is.na(pain_asmts) & pain_asmts > 0,
    had_painasmt_icu = ifelse(!icu_day, NA,
                              !is.na(pain_asmts_icu) & pain_asmts_icu > 0),

    ## Outcome variable: Did patient experience significant pain (verbal and/or
    ## validated instrument)?
    sigpain_verbal = ifelse(is.na(pain_verbal_sig), NA, pain_verbal_sig > 0),
    sigpain_verbal_icu = ifelse(!icu_day, NA, sigpain_verbal),
    sigpain_valid = ifelse(is.na(pain_valid_sig), NA, pain_valid_sig > 0),
    sigpain_valid_icu = ifelse(!icu_day, NA, sigpain_valid),
    sigpain = ifelse(is.na(sigpain_verbal) & is.na(sigpain_valid), NA,
                     (!is.na(sigpain_verbal) & sigpain_verbal) |
                       (!is.na(sigpain_valid) & sigpain_valid)),
    sigpain_icu = ifelse(!icu_day, NA, sigpain)
  )

## -- B: Both SAT and SBT ------------------------------------------------------
compliance <- compliance %>%
  mutate(
    ## Can't determine if patient was on sedation if none of the sedative
    ## options are checked
    on_sedation = ifelse(rowSums(compliance[,paste0("sedative_", 0:3)]) == 0, NA,
                         sedative_1 | sedative_2),
    on_sedation_icu = ifelse(!icu_day, NA, on_sedation),

    ## Did patient have an SAT/safety screen? (combine missing with "not doc")
    had_satscreen = ifelse(!on_sedation, NA,
                           !(is.na(satscreen_f) |
                              satscreen_f == "Not performed/Not documented")),
    had_satscreen_icu = ifelse(!icu_day, NA, had_satscreen),
    had_sat = ifelse(!on_sedation, NA,
                     !is.na(satperformed_f) & satperformed_f == "Yes"),
    had_sat_icu = ifelse(!icu_day, NA, had_sat),

    ## Compliance, SAT:
    ## - Both screen and performance must be documented in database
    ## - If safety screen failed, performance must be "No, failed screen..."
    ## - If safety screen passed, performance must be Yes
    ## - If safety screen not performed/documented, performance must be Yes *or*
    ##   "No, failed screen/contraindicated"
    comp_b_sat =
      ifelse(!icu_day | !on_sedation_icu, NA,
      ifelse(is.na(satscreen_f) | is.na(satperformed_f), FALSE,
      ifelse(satscreen_f == "Failed" &
               satperformed_f == "No. Patient failed the safety screen/contraindicated",
             TRUE,
      ifelse(satscreen_f == "Passed" & satperformed_f == "Yes", TRUE,
      ifelse(satscreen_f == "Not performed/Not documented" &
               satperformed_f %in%
               c("Yes", "No. Patient failed the safety screen/contraindicated"),
             TRUE,
             FALSE))))),

    ## Performance, SAT: SAT is performed (ignore safety screen)
    perf_b_sat = ifelse(!icu_day | !on_sedation_icu, NA,
                        (!is.na(satperformed_f) & satperformed_f == "Yes")),

    ## Was patient in the ICU and on MV?
    on_mv = ifelse(is.na(venttoday_f), NA, venttoday_f == "Yes"),
    on_mv_icu = ifelse(!icu_day | is.na(venttoday_f), NA, venttoday_f == "Yes"),

    ## Did patient have an SBT/safety screen? (combine missing with "not doc")
    had_sbtscreen = ifelse(!on_mv, NA,
                           !(is.na(sbtscreen_f) |
                               sbtscreen_f == "Not performed/Not documented")),
    had_sbtscreen_icu = ifelse(!icu_day, NA, had_sbtscreen),
    had_sbt = ifelse(!on_mv, NA,
                     !is.na(sbtperformed_f) & sbtperformed_f == "Yes"),
    had_sbt_icu = ifelse(!icu_day, NA, had_sbt),

    ## Compliance, SBT:
    ## - Both screen and performance must be documented in database
    ## - If safety screen failed, performance must be "No, failed screen..."
    ## - If safety screen passed, performance must be Yes
    ## - If safety screen not performed/documented, performance must be Yes *or*
    ##   "No, failed screen/contraindicated"
    comp_b_sbt =
      ifelse(!icu_day | !on_mv_icu, NA,
      ifelse(is.na(sbtscreen_f) | is.na(sbtperformed_f), FALSE,
      ifelse(sbtscreen_f == "Failed" &
               sbtperformed_f == "No. Patient failed the safety screen/contraindicated",
             TRUE,
      ifelse(sbtscreen_f == "Passed" & sbtperformed_f == "Yes", TRUE,
      ifelse(sbtscreen_f == "Not performed/Not documented" &
               sbtperformed_f %in%
               c("Yes", "No. Patient failed the safety screen/contraindicated"),
             TRUE,
             FALSE))))),

    ## Performance, SBT: SBT is performed (ignore safety screen)
    perf_b_sbt = ifelse(!icu_day | !on_mv_icu, NA,
                        (!is.na(sbtperformed_f) & sbtperformed_f == "Yes")),

    ## Was SAT performed prior to SBT (if both done)?
    sat_sbt =
      ifelse(!on_sedation | (!is.na(on_mv) & !on_mv) |
               !(!is.na(satperformed_f) & satperformed_f == "Yes") |
               !(!is.na(sbtperformed_f) & sbtperformed_f == "Yes"),
             NA,
             !is.na(satsbt_f) & satsbt_f == "Yes"),
    sat_sbt_icu = ifelse(!icu_day | !on_sedation_icu | !on_mv_icu, NA, sat_sbt)
  )

## -- C: Choice of analgesia and sedation --------------------------------------
compliance <- compliance %>%
  mutate(
    ## Number of sedation assessments per day in the ICU using a validated tool
    ## A few have a negative value - make these NA
    sed_assess_valid_icu =
      ifelse(!icu_day | (!is.na(sed_assess_valid) & sed_assess_valid < 0), NA,
             sed_assess_valid),

    ## Had any sedation assessments with validated instrument
    had_sedasmt = !is.na(sed_assess_valid_icu) & sed_assess_valid_icu > 0,
    had_sedasmt_icu = ifelse(!icu_day, NA, had_sedasmt),

    ## Compliance: Number of assessments is documented and is >=6
    comp_c = ifelse(!icu_day, NA,
                    !is.na(sed_assess_valid_icu) & sed_assess_valid_icu >= 6),

    ## Performance: Same definition
    perf_c = comp_c,

    ## Received benzos, opioids, dex, propofol,
    ##   antipsychotics (typical and/or atypical)
    rcvd_benzo_icu = ifelse(!icu_day, NA, meds_2),
    rcvd_opioid_icu = ifelse(!icu_day, NA, meds_1),
    rcvd_propofol_icu = ifelse(!icu_day, NA, meds_3),
    rcvd_dex_icu = ifelse(!icu_day, NA, meds_4),
    rcvd_antipsyc_icu = ifelse(!icu_day, NA, meds_6 | meds_7)
  )

## -- D: Assess, prevent and manage delirium -----------------------------------
compliance <- compliance %>%
  mutate(
    ## Indicator for whether patient was comatose *in the ICU*
    coma_icu = ifelse(!icu_day |
                        is.na(coma_f) | coma_f == "Not documented/unclear", NA,
                      coma_f == "Yes"),

    ## Number of delirium assessments per day in the ICU using a validated tool
    ## A few have a negative value - make these NA
    del_assess_valid_icu =
      ifelse(!icu_day |
               (!is.na(delirium_assess_valid) & delirium_assess_valid < 0), NA,
             delirium_assess_valid),

    ## Had any delirium assessments with validated instrument
    had_delasmt = !is.na(del_assess_valid_icu) & del_assess_valid_icu > 0,
    had_delasmt_icu = ifelse(!icu_day, NA, had_delasmt),

    ## Did the patient have delirium according to a validated instrument?
    delirium = ifelse(is.na(delirium_assess_valid) |
                        delirium_assess_valid < 0 |
                        is.na(delirium_present_valid_f) |
                        delirium_present_valid_f == "Not Specified",
                      NA,
                      delirium_present_valid_f == "Yes"),
    delirium_icu = ifelse(!icu_day, NA, delirium),

    ## Compliance: Number of assessments is documented and is >=2
    comp_d = ifelse(!icu_day, NA,
                    !is.na(del_assess_valid_icu) & del_assess_valid_icu >= 2),

    ## Performance: Same definition
    perf_d = comp_d
  )

## -- E: Exercise and early mobility -------------------------------------------
compliance <- compliance %>%
  mutate(
    ## Indicator for whether patient was physically restrained
    on_restraints = restraints_f == "Yes",
    on_restraints_icu = ifelse(!icu_day, NA, on_restraints),

    ## Did patient have mobility safety screen? (combine missing with "not doc")
    had_mobscreen = !(is.na(mobilityscreen_f) |
                        mobilityscreen_f == "Not performed/not documented"),
    had_mobscreen_icu = ifelse(!icu_day, NA, had_mobscreen),

    ## Highest level of mobility done on ICU days
    mobilityhighest_icu = factor(
      ifelse(!icu_day, NA,
      ifelse(is.na(mobilityhighest_f) |
               mobilityhighest_f == "Not documented /unclear",
             8,
      ifelse(mobilityhighest_f == "Active ROM - in bed", 1,
      ifelse(mobilityhighest_f == "Dangle - side of bed", 2,
      ifelse(mobilityhighest_f == "Stand at side of bed", 3,
      ifelse(mobilityhighest_f == "Active Transfer - out of bed to chair", 4,
      ifelse(mobilityhighest_f == "March in place", 5,
      ifelse(mobilityhighest_f == "Walk in room", 6,
      ifelse(mobilityhighest_f == "Walk  in hall", 7,
             NA))))))))),
      levels = 1:8,
      labels = c("Active ROM in bed",
                 "Dangle, side of bed",
                 "Stand at side of bed",
                 "Active transfer, bed to chair",
                 "March in place",
                 "Walk in room",
                 "Walk in hall",
                 "No level documented")
    ),

    ## Had mobility if mobility was performed at higher level than active ROM
    had_mobility_icu =
      ifelse(!icu_day, NA,
             !is.na(mobilityhighest_icu) &
               !(mobilityhighest_icu %in%
                   c("Active ROM in bed", "No level documented"))),

    ## Compliance:
    ## - Mobility screen, performance, and highest level (if mobility performed)
    ##   all documented
    ## - If safety screen failed, performed must be "no, failed screen..."
    ## - If safety screen passed, highest level must be > active ROM
    ## - If safety screen not documented, performed must be "no, failed
    ##   screen..." OR highest level must be > active ROM
    comp_e =
      ifelse(!icu_day, NA,
      ifelse(is.na(mobilityscreen_f) |
               is.na(mobilityperformed_f) |
               (mobilityperformed_f == "Yes" & is.na(mobilityhighest_f)),
             FALSE,
      ifelse(mobilityscreen_f == "Failed" &
               mobilityperformed_f == "No, patient failed the safety screen/contraindicated",
             TRUE,
      ifelse(mobilityscreen_f == "Passed" &
               !(is.na(mobilityhighest_f) |
                   mobilityhighest_f %in%
                   c("Not documented /unclear", "Active ROM - in bed")),
             TRUE,
      ifelse(mobilityscreen_f == "Not performed/not documented" &
               (mobilityperformed_f == "No, patient failed the safety screen/contraindicated" |
                  !(is.na(mobilityhighest_f) |
                      mobilityhighest_f %in%
                      c("Not documented /unclear", "Active ROM - in bed"))),
             TRUE,
             FALSE))))),

    ## Performance: Highest level of mobility is documented and is > active ROM
    ## (ignores safety screen)
    perf_e = ifelse(!icu_day, NA, !is.na(mobilityhighest_f) & had_mobility_icu)
  )

## -- F: Family engagement and empowerment -------------------------------------
compliance <- compliance %>%
  mutate(
    ## Indicator for whether family was documented to be present
    family_present = !is.na(familyvisit_f) & familyvisit_f == "Yes",
    family_present_icu = ifelse(!icu_day, NA, family_present),

    ## Indicators for whether family took part in various activities
    ## *Invited* to participate in rounds and/or conference
    family_invited_icu = ifelse(!family_present_icu, NA,
                                familyinvite_1 | familyinvite_2 | familyinvite_3),
    ## Took part in rounds or conference
    family_rounds_icu = ifelse(!family_present_icu, NA,
                               familyinvite_2 | familyinvite_3),
    ## Participated in care
    family_care_icu = ifelse(!family_present_icu, NA,
                             familyparticipate_1 | familyparticipate_2),
    ## Educated on any bundle elements
    family_edu_icu = ifelse(!family_present_icu, NA,
                            familyeducate_1 | familyeducate_2 |
                              familyeducate_3 | familyeducate_4 |
                              familyeducate_5),

    ## Compliance: Family member did at least one of
    ## - Took part in rounds or conference
    ## - Assisted in plan of care or ABCDEF care
    ## - Educated on pain, agitation/sedation, delirium, mobility, bundle
    comp_f = ifelse(!icu_day | !family_present_icu, NA,
                    familyinvite_2 | familyinvite_3 | familyparticipate_1 |
                    familyparticipate_2 | familyeducate_1 | familyeducate_2 |
                    familyeducate_3 | familyeducate_4 | familyeducate_5),

    ## Performance: Same definition
    perf_f = comp_f
  )

## -- Overall compliance and performance, daily --------------------------------
compliance_vars <- grep("^comp_", names(compliance), value = TRUE)
performance_vars <- grep("^perf_", names(compliance), value = TRUE)

## On a given day, how many elements were
## - eligible to be done (eg, if patient not on MV, not eligible for SBT)
## - compliant
## - performed
compliance$elements_elig <-
  ifelse(!compliance$icu_day, NA, rowSums(!is.na(compliance[,compliance_vars])))
compliance$elements_comp <-
  ifelse(!compliance$icu_day, NA,
         rowSums(compliance[ , compliance_vars], na.rm = TRUE))
compliance$elements_perf <-
  ifelse(!compliance$icu_day, NA,
         rowSums(compliance[ , performance_vars], na.rm = TRUE))

## Total compliance/performance for a given day
compliance <- compliance %>%
  mutate(
    ## Overall: *all* eligible elements must be done; yes/no
    comp_yn = ifelse(!icu_day, NA, elements_comp == elements_elig),
    perf_yn = ifelse(!icu_day, NA, elements_perf == elements_elig),

    ## Dose: proportion of eligible elements done
    comp_prop = ifelse(!icu_day, NA, elements_comp / elements_elig),
    perf_prop = ifelse(!icu_day, NA, elements_perf / elements_elig)
  )

## ICU version of comfort care
compliance$comfort_care_icu <- with(compliance, {
  ifelse(!icu_day, NA, comfort_care_f == "Yes")
})

## -- Calculate summary variables to add to demog ------------------------------
## Among all ICU days only,
## - total number and proportion of days:
##   - in ICU
##   - compliant, performed (overall + each element)
##   - had pain assessed, had significant pain
##   - on sedation, had SAT screen, had SAT
##   - on MV, had SBT screen, had SBT
##   - on benzos/opioids/propofol/dex/antipsychotics, had sedation assessed
##   - had delirium assessed, had delirium, had coma
##   - had mobility screen, had mobility performed
##   - on comfort care
##   - family present, invited, participated, educated
compliance_icu_bypt <- compliance %>%
  filter(icu_day) %>%
  dplyr::select(id, icu_day,
                matches("^comp\\_[^prop]"), matches("^perf\\_[^prop]"),
                had_painasmt_icu, sigpain_icu,
                on_sedation_icu, had_satscreen_icu, had_sat_icu,
                on_mv_icu, had_sbtscreen_icu, had_sbt_icu, sat_sbt_icu,
                rcvd_benzo_icu, rcvd_opioid_icu, rcvd_propofol_icu, rcvd_dex_icu,
                rcvd_antipsyc_icu, had_sedasmt_icu,
                had_delasmt_icu, delirium_icu, coma_icu,
                had_mobscreen_icu, had_mobility_icu,
                comfort_care_icu,
                matches("^family\\_.+\\_icu$")) %>%
  group_by(id) %>%
  summarise_all(funs(days = sum, prop = mean), na.rm = TRUE) %>%
  ungroup() %>%
  ## B-SAT, B-SBT, F shd only have values if >0 days sedation/MV/family present
  mutate(had_satscreen_icu_days =
           ifelse(on_sedation_icu_days == 0, NA, had_satscreen_icu_days),
         had_satscreen_icu_prop =
           ifelse(on_sedation_icu_days == 0, NA, had_satscreen_icu_prop),
         had_sat_icu_days =
           ifelse(on_sedation_icu_days == 0, NA, had_sat_icu_days),
         had_sat_icu_prop =
           ifelse(on_sedation_icu_days == 0, NA, had_sat_icu_prop),
         comp_b_sat_days =
           ifelse(on_sedation_icu_days == 0, NA, comp_b_sat_days),
         comp_b_sat_prop =
           ifelse(on_sedation_icu_days == 0, NA, comp_b_sat_prop),
         perf_b_sat_days =
           ifelse(on_sedation_icu_days == 0, NA, perf_b_sat_days),
         perf_b_sat_prop =
           ifelse(on_sedation_icu_days == 0, NA, perf_b_sat_prop),
         had_sbtscreen_icu_days =
           ifelse(on_mv_icu_days == 0, NA, had_sbtscreen_icu_days),
         had_sbtscreen_icu_prop =
           ifelse(on_mv_icu_days == 0, NA, had_sbtscreen_icu_prop),
         had_sbt_icu_days = ifelse(on_mv_icu_days == 0, NA, had_sbt_icu_days),
         had_sbt_icu_prop = ifelse(on_mv_icu_days == 0, NA, had_sbt_icu_prop),
         comp_b_sbt_days = ifelse(on_mv_icu_days == 0, NA, comp_b_sbt_days),
         comp_b_sbt_prop = ifelse(on_mv_icu_days == 0, NA, comp_b_sbt_prop),
         perf_b_sbt_days = ifelse(on_mv_icu_days == 0, NA, perf_b_sbt_days),
         perf_b_sbt_prop = ifelse(on_mv_icu_days == 0, NA, perf_b_sbt_prop),
         family_invited_icu_days =
           ifelse(family_present_icu_days == 0, NA, family_invited_icu_days),
         family_invited_icu_prop =
           ifelse(family_present_icu_days == 0, NA, family_invited_icu_prop),
         family_rounds_icu_days =
           ifelse(family_present_icu_days == 0, NA, family_rounds_icu_days),
         family_rounds_icu_prop =
           ifelse(family_present_icu_days == 0, NA, family_rounds_icu_prop),
         family_care_icu_days =
           ifelse(family_present_icu_days == 0, NA, family_care_icu_days),
         family_care_icu_prop =
           ifelse(family_present_icu_days == 0, NA, family_care_icu_prop),
         family_edu_icu_days =
           ifelse(family_present_icu_days == 0, NA, family_edu_icu_days),
         family_edu_icu_prop =
           ifelse(family_present_icu_days == 0, NA, family_edu_icu_prop),
         comp_f_days =
           ifelse(family_present_icu_days == 0, NA, comp_f_days),
         comp_f_prop =
           ifelse(family_present_icu_days == 0, NA, comp_f_prop),
         perf_f_days =
           ifelse(family_present_icu_days == 0, NA, perf_f_days),
         perf_f_prop =
           ifelse(family_present_icu_days == 0, NA, perf_f_prop)
  ) %>%
  dplyr::select(-icu_day_prop) %>% ## meaningless, 100% for everyone
  rename(icu_days = icu_day_days) %>%
  mutate(
    ## Indicator for whether patient was ever on comfort care in ICU
    comfort_care_icu_ever = ifelse(icu_days == 0, NA, comfort_care_icu_days > 0)
  )

## Merge onto demog; if icu_days is NA, indicates that patient never had a
## day with icu_day = Yes to include in above -> set to 0
demog <- left_join(demog, compliance_icu_bypt, by = "id") %>%
  mutate(icu_days = ifelse(is.na(icu_days), 0, icu_days))

## Among ICU days only, total number & prop of elements compliant, performed
compliance_icu_elem <- compliance %>%
  filter(icu_day) %>%
  dplyr::select(id, elements_elig, elements_comp, elements_perf) %>%
  group_by(id) %>%
  summarise_all(funs(tot = sum), na.rm = TRUE) %>%
  ungroup() %>%
  mutate(comp_elem_prop = elements_comp_tot / elements_elig_tot,
         perf_elem_prop = elements_perf_tot / elements_elig_tot)

demog <- left_join(demog, compliance_icu_elem, by = "id")

## -- Dataset for Alai - time series analysis ----------------------------------
## One row per month with numerator, denominator columns
## Numerator = number of days/elements compliant and/or performed
## Denominator = number of days/elements eligible to be compliant/performed

tsdata <- compliance %>%
  filter(icu_day) %>%
  dplyr::select(month_f,
                icu_day,
                starts_with("elements_"),
                on_sedation_icu, on_mv_icu, family_present_icu,
                matches("^comp\\_[^prop]"), matches("^perf\\_[^prop]")) %>%
  group_by(month_f) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  ungroup()

## Time series data by study site for R01 application
tsdata_site <- compliance %>%
  filter(icu_day) %>%
  left_join(dplyr::select(demog, id, hosp_f), by = "id") %>%
  dplyr::select(month_f,
                hosp_f,
                icu_day,
                starts_with("elements_"),
                on_sedation_icu, on_mv_icu, family_present_icu,
                matches("^comp\\_[^prop]"), matches("^perf\\_[^prop]")) %>%
  group_by(month_f, hosp_f) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  ungroup()

## Numerator for overall compliance/performance: comp_yn, perf_yn
## Numerator for compliance/performance "dose": elements_comp, elements_perf
## Numerator for individual bundle element compliance: comp_[a/b_sat/b_sbt/c/d/e/f]
## Numerator for individual bundle element performance: perf_[a/b_sat/b_sbt/c/d/e/f]

## Denominator for overall compliance/performance, A, C, D, E: icu_day
## Denominator for compliance/performance "dose": elements_elig
## Denominator for B - SAT: on_sedation_icu
## Denominator for B - SBT: on_mv_icu
## Denominator for F: family_present_icu

write_csv(tsdata, path = "AnalysisData/iculib_tsdata.csv", na = "")
write_csv(tsdata_site, path = "AnalysisData/iculib_tsdata_site.csv", na = "")

## -- Some T/F variables would be better as factors for later purposes ---------
make_tf_factor <- function(vname, vlevels){
  if(!(inherits(vlevels, "character") & length(vlevels == 2))){
    stop("vlevels should be a character vector of length 2", call. = FALSE)
  }

  factor(as.numeric(vname), levels = 0:1, labels = vlevels)
}

## Wrapper functions for common levels
make_tf_factor_comp <- function(vname){
  make_tf_factor(vname, vlevels = c("Noncompliant", "Compliant"))
}

make_tf_factor_perf <- function(vname){
  make_tf_factor(vname, vlevels = c("Not performed", "Performed"))
}

make_tf_factor_yn <- function(vname){
  make_tf_factor(vname, vlevels = c("No", "Yes"))
}

make_tf_factor_asmt <- function(vname){
  make_tf_factor(vname, vlevels = c("No assessment", ">=1 assessment"))
}

compliance <- compliance %>%
  mutate(
    ## All compliance/performance variables - make separate variables for
    ## descriptive tables, keep T/F for calculations
    comp_a_f = make_tf_factor_comp(comp_a),
    comp_b_sat_f = make_tf_factor_comp(comp_b_sat),
    comp_b_sbt_f = make_tf_factor_comp(comp_b_sbt),
    comp_c_f = make_tf_factor_comp(comp_c),
    comp_d_f = make_tf_factor_comp(comp_d),
    comp_e_f = make_tf_factor_comp(comp_e),
    comp_f_f = make_tf_factor_comp(comp_f),
    comp_yn_f = make_tf_factor_comp(comp_yn),

    perf_a_f = make_tf_factor_perf(perf_a),
    perf_b_sat_f = make_tf_factor_perf(perf_b_sat),
    perf_b_sbt_f = make_tf_factor_perf(perf_b_sbt),
    perf_c_f = make_tf_factor_perf(perf_c),
    perf_d_f = make_tf_factor_perf(perf_d),
    perf_e_f = make_tf_factor_perf(perf_e),
    perf_f_f = make_tf_factor_perf(perf_f),
    perf_yn_f = make_tf_factor_perf(perf_yn),

    ## A: Assess, prevent and manage pain
    had_painasmt = make_tf_factor_asmt(had_painasmt),
    had_painasmt_icu = make_tf_factor_asmt(had_painasmt_icu),
    sigpain_verbal = make_tf_factor(
      sigpain_verbal,
      c("No significant pain", ">=1 asmt with self-reported pain")
    ),
    sigpain_valid = make_tf_factor(
      sigpain_valid,
      c("No significant pain", ">=1 asmt with significant pain per BPS")
    ),
    sigpain_verbal_icu = make_tf_factor(
      sigpain_verbal_icu,
      c("No significant pain", ">=1 asmt with self-reported pain")
    ),
    sigpain_valid_icu = make_tf_factor(
      sigpain_valid_icu,
      c("No significant pain", ">=1 asmt with significant pain per BPS")
    ),
    sigpain = make_tf_factor(
      sigpain,
      c("No significant pain", "Significant pain (self-report or BPS)")
    ),
    sigpain_icu = make_tf_factor(
      sigpain_icu,
      c("No significant pain", "Significant pain (self-report or BPS)")
    ),

    ## B: Both SAT and SBT
    ## Sedation/SAT
    on_sedation = make_tf_factor(
      on_sedation,
      c("No or PRN sedation only", "Continuous/intermittent sedation")
    ),
    on_sedation_icu_f = make_tf_factor(
      on_sedation_icu,
      c("No or PRN sedation only", "Continuous/intermittent sedation")
    ),
    had_satscreen = make_tf_factor(
      had_satscreen, c("No screen documented", "SAT screen documented")
    ),
    had_satscreen_icu = make_tf_factor(
      had_satscreen_icu, c("No screen documented", "SAT screen documented")
    ),
    had_sat = make_tf_factor(had_sat, c("No SAT documented", "SAT documented")),
    had_sat_icu = make_tf_factor(
      had_sat_icu, c("No SAT documented", "SAT documented")
    ),

    ## MV/SBT
    on_mv = make_tf_factor(on_mv, c("Not on MV", "Received MV")),
    on_mv_icu_f = make_tf_factor(on_mv_icu, c("Not on MV", "Received MV")),
    had_sbtscreen = make_tf_factor(
      had_sbtscreen, c("No screen documented", "SBT screen documented")
    ),
    had_sbtscreen_icu = make_tf_factor(
      had_sbtscreen_icu, c("No screen documented", "SBT screen documented")
    ),
    had_sbt = make_tf_factor(had_sbt, c("No SBT documented", "SBT documented")),
    had_sbt_icu = make_tf_factor(
      had_sbt_icu, c("No SBT documented", "SBT documented")
    ),

    sat_sbt = make_tf_factor(sat_sbt, c("Not paired", "Paired")),
    sat_sbt_icu = make_tf_factor(sat_sbt_icu, c("Not paired", "Paired")),

    ## C: Choice of analgesia and sedation
    had_sedasmt = make_tf_factor_asmt(had_sedasmt),
    had_sedasmt_icu = make_tf_factor_asmt(had_sedasmt_icu),
    rcvd_benzo_icu = make_tf_factor_yn(rcvd_benzo_icu),
    rcvd_opioid_icu = make_tf_factor_yn(rcvd_opioid_icu),
    rcvd_propofol_icu = make_tf_factor_yn(rcvd_propofol_icu),
    rcvd_dex_icu = make_tf_factor_yn(rcvd_dex_icu),
    rcvd_antipsyc_icu = make_tf_factor_yn(rcvd_antipsyc_icu),

    ## D: Delirium - assess, prevent and manage
    coma_icu = make_tf_factor_yn(coma_icu),
    had_delasmt = make_tf_factor_asmt(had_delasmt),
    had_delasmt_icu = make_tf_factor_asmt(had_delasmt_icu),
    delirium = make_tf_factor_yn(delirium),
    delirium_icu = make_tf_factor_yn(delirium_icu),

    ## E: Exercise/early mobility
    on_restraints = make_tf_factor_yn(on_restraints),
    on_restraints_icu = make_tf_factor_yn(on_restraints_icu),
    had_mobscreen = make_tf_factor(
      had_mobscreen, c("No screen documented", "Mobility screen documented")
    ),
    had_mobscreen_icu = make_tf_factor(
      had_mobscreen_icu, c("No screen documented", "Mobility screen documented")
    ),
    had_mobility_icu = make_tf_factor(
      had_mobility_icu,
      c("No or low mobility documented", "Mobility > active ROM documented")
    ),
    family_present = make_tf_factor(
      family_present, c("Not documented present", "Present")
    ),
    family_present_icu_f = make_tf_factor(
      family_present_icu, c("Not documented present", "Present")
    ),
    family_invited_icu = make_tf_factor(
      family_invited_icu, c("No invitation documented", "Invited to participate")
    ),
    family_rounds_icu = make_tf_factor(
      family_rounds_icu, c("Did not participate", "Participated in rounds")
    ),
    family_care_icu = make_tf_factor(
      family_care_icu, c("Did not participate", "Participated in care")
    ),
    family_edu_icu = make_tf_factor(
      family_edu_icu, c("No education documented", "Educated")
    ),

    comfort_care_icu = make_tf_factor_yn(comfort_care_icu)
  )

demog$comfort_care_icu_ever <- make_tf_factor_yn(demog$comfort_care_icu_ever)

## -- Save final datasets ------------------------------------------------------
save(demog, compliance, tsdata, file = "AnalysisData/iculib.Rdata")

## Write time series data to CSV for Alai; will write separate README for that
write_csv(tsdata, path = "iculib_tsdata.csv")
