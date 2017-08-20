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

## -- We're only going to use Months 1-20 (length of official Collab). ---------
## -- Add Month to compliance data and restrict both datasets. -----------------
keep_months <- paste("Month", 1:20)

compliance <-
  right_join(subset(demog, select = c(id, month_f)), compliance, by = "id") %>%
  filter(month_f %in% keep_months)

demog <- filter(demog, month_f %in% keep_months)

## -- Data management for demographic form -------------------------------------
demog <- demog %>%
  mutate(
    ## ICU type - temporary till I get a key from Christina
    icu_type = factor(ifelse(stri_endswith_fixed(demog$hosp_f, "MICU"), 1,
                      ifelse(stri_endswith_fixed(demog$hosp_f, "SICU"), 2, 3)),
                      levels = 1:3,
                      labels = c("MICU", "SICU", "Other")),

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
    ## Ethnicity
    hisp_cat = factor(ifelse(is.na(hispanic_f), 3,
                      ifelse(hispanic_f == "Yes", 1, 2)),
                      levels = 1:3,
                      labels = c("Hispanic",
                                 "Non-Hispanic",
                                 "No ethnicity entered")),

    ## BMI wt (kg) / [height in meters]^2
    ht = as.numeric(ht),
    wt = as.numeric(wt),
    ## Some of these heights look weird. Assume anything below 90 was
    ## accidentally entered in inches (90" = 7.5 feet), and convert to cm.
    ht = ifelse(ht < 90, ht * 2.54, ht),
    bmi = ifelse(is.na(ht) | ht == 0 | is.na(wt) | wt == 0, NA,
                 wt / ((ht / 100)^2)),

    ## Admission diagnosis: trauma, surgery, sepsis/pneumonia, everything else
    admitdx = factor(
      ifelse(rowSums(demog[,paste0("icu_dx_", c(1:9, 11:31, 99))]) == 0, NA,
      ifelse(icu_dx_1 | icu_dx_5, 1,
      ifelse(icu_dx_18 | icu_dx_19 | icu_dx_20 | icu_dx_28 | icu_dx_29, 2,
      ifelse(icu_dx_21 | icu_dx_22 | icu_dx_23 | icu_dx_24 | icu_dx_25 |
               icu_dx_26 | icu_dx_27, 3, 4)))),
      levels = 1:4,
      labels = c("Sepsis/pneumonia", "Trauma/neurological",
                 "Surgery", "Anything else")),

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
      labels = c("No restrictions pre-admission",
                 "Mobility restricted pre-admission")
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
         rowSums(demog[,paste0('invas_vent_length_', 1:6)], na.rm = TRUE)) })

## Write BMIs < 10 or > 100 to CSV for clinicians to check.
subset(demog, bmi > 100 | bmi < 10, select = c(id, ht, wt, bmi)) %>%
  arrange(bmi) %>%
  write_csv(path = "check_bmis.csv")

## -- Data management for compliance form --------------------------------------
## Variables related to each bundle element done in separate chunks for easier
## changes later

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
    on_mv_icu = ifelse(!icu_day | is.na(venttoday_f), NA, venttoday_f == "Yes"),

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
    sat_before_sbt =
      ifelse(!icu_day | !on_sedation_icu | !on_mv_icu |
               !(!is.na(satperformed_f) & satperformed_f == "Yes") |
               !(!is.na(sbtperformed_f) & sbtperformed_f == "Yes"),
             NA,
             !is.na(satsbt_f) & satsbt_f == "Yes")

  )

