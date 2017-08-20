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

    ## Compliance: Number of assessments is documented and is >=6
    comp_c = ifelse(!icu_day, NA,
                    !is.na(sed_assess_valid_icu) & sed_assess_valid_icu >= 6),

    ## Performance: Same definition
    perf_c = comp_c
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
    perf_e = ifelse(!icu_day, NA,
                    !is.na(mobilityhighest_f) &
                      !(mobilityhighest_f %in%
                          c("Not documented /unclear", "Active ROM - in bed")))
  )

## -- F: Family engagement and empowerment -------------------------------------
compliance <- compliance %>%
  mutate(
    ## Indicator for whether family was documented to be present
    family_present = !is.na(familyvisit_f) & familyvisit_f == "Yes",
    family_present_icu = ifelse(!icu_day, NA, family_present),

    ## Indicators for whether family took part in various activities
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

## -- Some T/F variables would be better as factors for later purposes ---------
make_tf_factor <- function(vname, vlevels){
  if(!(inherits(vlevels, "character") & length(vlevels == 2))){
    stop("vlevels should be a character vector of length 2", call. = FALSE)
  }

  factor(as.numeric(vname), levels = 0:1, labels = vlevels)
}

compliance <- compliance %>%
  mutate(
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
    on_sedation = make_tf_factor(
      on_sedation,
      c("No or PRN sedation only", "Continuous/intermittent sedation")
    ),
    on_sedation_icu = make_tf_factor(
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
    on_mv = make_tf_factor(on_mv, c("Not on MV", "Received MV")),
    on_mv_icu = make_tf_factor(on_mv_icu, c("Not on MV", "Received MV")),
    had_sbtscreen = make_tf_factor(
      had_sbtscreen, c("No screen documented", "SBT screen documented")
    ),
    had_sbtscreen_icu = make_tf_factor(
      had_sbtscreen_icu, c("No screen documented", "SBT screen documented")
    ),
    had_sbt = make_tf_factor(had_sbt, c("No SBT documented", "SBT documented")),
    had_sbt_icu = make_tf_factor(
      had_sbt_icu, c("No SBT documented", "SBT documented")
    )
  )

