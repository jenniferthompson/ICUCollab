####################################################################################################
## ICU Collaborative: Pediatric Data Management
####################################################################################################

## -- Read in data, assign factors & labels using REDCap scripts -----------------------------------

library(JTHelpers)
library(tidyverse)

## Demographics form
demog <- read.csv('RawData/peds_demog.csv', stringsAsFactors = FALSE)
source('peds_demog_redcap.R')
names(demog) <- gsub('\\.factor', '\\.f', gsub('_+', '.', names(demog)))

## Compliance form
compliance <- read.csv('RawData/peds_compliance.csv', stringsAsFactors = FALSE)
source('peds_compliance_redcap.R')
names(compliance) <- gsub('\\.factor', '\\.f', gsub('_+', '.', names(compliance)))

## -- Remove test patients -------------------------------------------------------------------------
all.ids <- sort(unique(c(unique(demog$id), unique(compliance$id))))
remove.pts <- all.ids[grep('test', tolower(all.ids))]

demog <- demog[!(demog$id %in% remove.pts),]
compliance <- compliance[!(compliance$id %in% remove.pts),]

## -- Site-level information: In each site report, we'll have one table describing pain, -----------
## -- anxiety, withdrawal clinical practice guidelines (may be multiple values if multiple ---------
## -- ICUs involved) -------------------------------------------------------------------------------
site_guidelines <- demog %>%
  dplyr::select(hosp.f, pain.f, anxiety.f, withdrawal.f, ventilation.f, extubation.f, anxiolytic.f,
                delirium.f, mobility.f, family.f) %>%
  filter(complete.cases(.)) %>%
  unique()

label(site_guidelines$hosp.f) <- 'Hospital'
label(site_guidelines$pain.f) <- 'Pain assessment, prevention and management'
label(site_guidelines$anxiety.f) <- 'Anxiety/agitation assessment, prevention and management'
label(site_guidelines$withdrawal.f) <- 'Sedation withdrawal assessment, prevention and management'
label(site_guidelines$ventilation.f) <- 'Mechanical ventilation weaning'
label(site_guidelines$extubation.f) <- 'Extubation readiness testing'
label(site_guidelines$anxiolytic.f) <- 'Sedation/titration of analgesics and/or anxiolytics'
label(site_guidelines$delirium.f) <- 'Delirium assessment, prevention and management'
label(site_guidelines$mobility.f) <- 'Early mobility and exercise'
label(site_guidelines$family.f) <- 'Family engagement and empowerment'

## -- Demographic information ----------------------------------------------------------------------
## What's the last implementation month there could possibly be data for?
max.month <- max(as.numeric(gsub('Implementation Month ', '', levels(demog$month.f))), na.rm = TRUE)

## How many of each checkbox variable were checked?
demog$race.checked <- rowSums(demog[,grep('^race\\.[0-9]+$', names(demog))])
demog$severity.checked <- rowSums(demog[,grep('^severityscale\\.[0-9]+$', names(demog))])

demog <- demog %>%
  ## Rename insensibly named variables more sensibly
  rename(hosplos = piculos2.5b7,
         ventdays = piculos2.c4c,
         fss.preadm = ffs.score2.843,
         fss.adm = ffs.score,
         fss.dc = ffs.score2.ecb,
         popc.preadm = popc.baseline2.4dd,
         popc.adm = popc.baseline,
         popc.dc = popc.baseline2.952) %>%
  mutate(## Combine baseline months, keep implementation months separate
         month.cat = factor(ifelse(is.na(month.f), NA,
                            ifelse(gsub(' .*$', '', month.f) == 'Baseline', 0,
                                   as.numeric(gsub('^Implementation Month ', '', month.f)))),
                            levels = 0:max.month,
                            labels = c('Baseline', paste('Imp.', 1:max.month))),
         ## Dichotomous baseline vs implementation variable
         data.time = factor(ifelse(is.na(month.f), NA,
                            ifelse(month.cat == 'Baseline', 1, 2)),
                            levels = 1:2, labels = c('Baseline', 'Implementation')),
         ## Which data plan was the site on at the time of data collection?
         dataplan.combined = factor(ifelse(data.plan.f == 'Tier 1 (minimum data set)', 1,
                                    ifelse(data.plan.f == 'Tier 2 (full data set)', 2, 3)),
                                    levels = 1:3,
                                    labels = c('Tier 1 (minimum)',
                                               'Tier 2 (full)',
                                               'Tier 3 (site-specific)')),
         ## New variables for race, severity scale (checkboxes)
         race.combined = factor(ifelse(race.checked == 0, 0,
                                ifelse(race.checked > 1, 7,
                                ifelse(race.1, 1,
                                ifelse(race.2, 2,
                                ifelse(race.3, 3,
                                ifelse(race.4, 4,
                                ifelse(race.5, 5,
                                ifelse(race.6, 6, NA)))))))),
                                levels = 0:7,
                                labels = c('Unknown (no data available)',
                                           'American Indian/Alaskan Native',
                                           'Black/African American',
                                           'White',
                                           'Asian',
                                           'Native Hawaiian/Pacific Islander',
                                           'Other/not specified',
                                           'Multiple races specified')),
         severity.combined = factor(ifelse(severity.checked == 0, 0,
                                    ifelse(severityscale.1 == 1 & severityscale.2 == 1, 1,
                                    ifelse(severityscale.1 == 1 & severityscale.99 == 1, 2,
                                    ifelse(severityscale.2 == 1 & severityscale.99 == 1, 3,
                                    ifelse((severityscale.1 == 1 | severityscale.2 == 1) &
                                             severityscale.0, 0,
                                    ifelse(severityscale.1 == 1, 4,
                                    ifelse(severityscale.2 == 1, 5,
                                    ifelse(severityscale.99 == 1, 6,
                                    ifelse(severityscale.0 == 1, 7, NA))))))))),
                                    levels = 0:7,
                                    labels = c('No or conflicting severity scale information',
                                               'PRISM III + PIM 2',
                                               'PRISM III + other scale',
                                               'PIM 2 + other scale',
                                               'PRISM III only',
                                               'PIM 2 only',
                                               'Other scale',
                                               'No severity scale used')),
         popc.preadm = as.numeric(as.character(popc.preadm)),
         popc.adm = as.numeric(as.character(popc.adm)),
         popc.dc = as.numeric(as.character(popc.dc)),
         ## Ever on MV; days on MV among patients exposed
         ever.vent = factor(ifelse(is.na(ventdays), 1,
                            ifelse(ventdays == 0, 2, 3)),
                            levels = 1:3,
                            labels = c('Unknown (no data available)',
                                       'Never on MV', '>=1 day on MV')),
         ventdays.exp = ifelse(ventdays == 0, NA, ventdays)) %>%
  dplyr::select(id, hosp.f, data.time, month.cat, dataplan.combined, age.f, sex.f, race.combined,
                english.f, wt, severity.combined, piculos, hosplos, ever.vent, ventdays,
                ventdays.exp, fss.preadm, fss.adm, fss.dc, popc.preadm, popc.adm, popc.dc,
                mortality.f)

label(demog$hosp.f) <- 'Hospital'
label(demog$data.time) <- 'Time period'
label(demog$month.cat) <- 'Month'
label(demog$dataplan.combined) <- 'Data plan'
label(demog$age.f) <- 'Age category'
label(demog$sex.f) <- 'Sex'
label(demog$race.combined) <- 'Race'
label(demog$english.f) <- 'Language'
label(demog$severity.combined) <- 'SOI score used'
label(demog$piculos) <- 'PICU length of stay'
label(demog$hosplos) <- 'Hospital length of stay'
label(demog$ventdays) <- 'Days on MV, all patients'
label(demog$ever.vent) <- 'Ever on mechanical ventilation'
label(demog$ventdays.exp) <- 'Days MV among patients ever on MV'
label(demog$fss.preadm) <- 'FSS, pre-admission'
label(demog$fss.adm) <- 'FSS, PICU admission'
label(demog$fss.dc) <- 'FSS, PICU discharge'
label(demog$popc.preadm) <- 'POPC, pre-admission'
label(demog$popc.adm) <- 'POPC, PICU admission'
label(demog$popc.dc) <- 'POPC, PICU discharge'
label(demog$mortality.f) <- 'Mortality status'


## -- Daily information ----------------------------------------------------------------------------
## Indicator for whether data available for various conditions
## (several are checkboxes, can't be done inside mutate)
compliance$pain.info <- !is.na(compliance$pain.morethan5)
compliance$sedation.info <- rowSums(compliance[,paste0('sedative.', 0:3)]) > 0
compliance$mv.info <- !is.na(compliance$invasivemv.f)
compliance$anxiolysis.info <- rowSums(compliance[,paste0('anxiolysis.', 1:4)]) > 0
compliance$anxiolysis.specifics <-
  ifelse(!compliance$anxiolysis.info, NA,
         rowSums(compliance[,paste0('anxio.med.', c(1:4, 6, 9))]) > 0)

compliance <- compliance %>%
  mutate(## A:
         ## How many pain assessments per day had a score >= 5? variable: pain.morethan5
         ## How many days had at least one assessment with a score > 5?
         pain.threshold = ifelse(!pain.info, NA, pain.morethan5 > 0),
         ## B:
         ## is patient on continuous or intermittent sedation?
         on.majorsed = ifelse(!sedation.info, NA, sedative.1 == 1 | sedative.2 == 1),
         ## If so, was it interrupted either in SAT or by nurse protocol?
         ## First determine whether SAT data available (days on sedation only)
         sat.info = ifelse(is.na(on.majorsed) | !on.majorsed, NA, !is.na(sedative.interrupt.f)),
         weaning.info = ifelse(is.na(on.majorsed) | !on.majorsed, NA, !is.na(sedative.protocol.f)),
         ## If SAT data available, was sedation interrupted at least once?
         had.sat = ifelse(is.na(sat.info) | (!is.na(sat.info) & !sat.info), NA,
                          !is.na(sedative.interrupt.f) & sedative.interrupt.f == 'Yes'),
         had.weaning = ifelse(is.na(weaning.info) | (!is.na(weaning.info) & !weaning.info), NA,
                              !is.na(sedative.protocol.f) & sedative.protocol.f == 'Yes'),
         ## is patient on any kind of sedation (including PRN)?
         on.anysed = ifelse(!sedation.info, NA,
                            sedative.1 == 1 | sedative.2 == 1 | sedative.3 == 1),
         ## If so, were they ever deeply sedated?
         deepsed.info = ifelse(is.na(on.anysed) | !on.anysed, NA,
                               !is.na(sedative.deepsedation.f)),
         deeply.sedated = ifelse(is.na(deepsed.info), NA, sedative.deepsedation.f == 'Yes'),
         ## is patient on MV?
         on.mv = ifelse(is.na(invasivemv.f), NA,
                        invasivemv.f %in% c('Yes, via endotracheal tube',
                                            'Yes, via tracheostomy tube')),
         ## SBT safety screen info available?
         sbt.safety.info = ifelse(is.na(on.mv) | !on.mv, NA, !is.na(sbt.safetyscreen)),
         had.sbt.safety = ifelse(is.na(sbt.safety.info) | !sbt.safety.info, NA,
                                 sbt.safetyscreen.f %in% c("Yes: safe (Passed Screen)",
                                                           "Yes: unsafe (Failed Screen)")),
         ## SBT info available?
         sbt.info = ifelse(is.na(on.mv) | !on.mv, NA, !is.na(sbt.result)),
         had.sbt = ifelse(is.na(sbt.info) | !sbt.info, NA, sbt.result.f %in% c('Pass', 'Fail')),
         ## Followed SBT protocol (SBT safety screen; if passed, SBT)?
         sbt.protocol = ifelse(is.na(on.mv) | !on.mv, NA,
                               sbt.safety.info & had.sbt.safety &
                                 (sbt.safetyscreen.f == 'Yes: unsafe (Failed Screen)' |
                                    sbt.safetyscreen.f == 'Yes: safe (Passed Screen)' &
                                    had.sbt)),
         ## C: Choice of analgesia and sedation - largely addressed above
         ## Individual indicators for benzos, diphenhydramine, ketamine
         anxio.benz = ifelse(!anxiolysis.info, NA,
                             anxio.med.1 == 1 | anxio.med.2 == 1 | anxio.med.3 == 1 |
                               anxio.med.4 == 1),
         anxio.diphen = ifelse(!anxiolysis.info, NA, anxio.med.9 == 1),
         anxio.ketam = ifelse(!anxiolysis.info, NA, anxio.med.6 == 1),
         ## D: Delirium - assess, prevent and manage
         delirium.assessed = !is.na(delirium.capd2) | !is.na(delirium.pscam1.f) |
                              !is.na(delirium.pcam1.f) | !is.na(delirium.cam1.f),
         delirium.present = ifelse(!delirium.assessed, NA,
                                   (!is.na(delirium.capd2) &
                                      delirium.capd2 >= 9 & delirium.capd2 != 33) |
                                   (!is.na(delirium.pscam1.f) & delirium.pscam1.f == 'Yes') |
                                   (!is.na(delirium.pcam1.f) & delirium.pcam1.f == 'Yes') |
                                   (!is.na(delirium.cam1.f) & delirium.cam1.f == 'Yes')),
         ## E: Exercise/early mobility
         ## Safety screen info available?
         exer.safety.info = !is.na(mobilization.f),
         had.exer.safety = ifelse(!exer.safety.info, NA,
                                  mobilization.f %in% c("Yes: reported safe for mobilization",
                                                        "Yes: reported unsafe for mobilization")),
         ## Mobilization info available?
         exer.info = !is.na(mobz.exercise.f),
         had.exer = ifelse(!exer.info, NA, mobz.exercise.f == 'Yes'),
         ## All info for mobility protocol available?
         exer.protocol.info = exer.safety.info & exer.info,
         ## Followed mobility protocol (safety screen; if passed, therapy)?
         exer.protocol = exer.safety.info & had.exer.safety & !is.na(mobilization.f) &
                          (mobilization.f == 'Yes: reported unsafe for mobilization' |
                           (mobilization.f == 'Yes: reported safe for mobilization' & had.exer)),
         ## F: Family engagement/empowerment
         family.present.info = !is.na(familyvisit.f),
         family.present = ifelse(!family.present.info, NA, familyvisit.f == 'Yes'),
         family.inter.info = ifelse(!family.present.info | !family.present, NA,
                                    !is.na(familyintervention.f)),
         family.intervention = ifelse(is.na(family.inter.info) | !family.inter.info, NA,
                                      familyintervention.f == 'Yes')
         )

## -- Calculate drug doses -------------------------------------------------------------------------
## List of all variables denoting dose types for each drug of interest
drug_vars <- list("morphine" = c('analgesia.morphineti', 'analgesia.morphinebd'),
                  "hydromorphone" = c('analgesia.hydromorpti', 'analgesia.hydromorpbd'),
                  "fentanyl" = c('analgesia.fentanylti', 'analgesia.fentanylbd'),
                  "lorazepam" = c('anxio.lorazti', 'anxio.lorazbd'),
                  "clonazepam" = c('anxio.clonati', 'anxio.clonabd'),
                  "diazepam" = c('anxio.diazti', 'anxio.diazbd'),
                  "midazolam" = c('anxio.midazti', 'anxio.midazbd'))

## Prep: Make drug values numeric; some have commas, etc
drug_vars_all <- flatten_chr(drug_vars)

for(i in 1:length(drug_vars_all)){
  ## Regex replaces commas followed by at least three digits with nothing; replaces all other commas
  ## with decimal points
  compliance[,drug_vars_all[i]] <- as.numeric(gsub(',', '\\.',
                                                   gsub(',(?=[0-9][0-9][0-9])', '',
                                                        compliance[,drug_vars_all[i]],
                                                        perl = TRUE)))
}

## Function to total doses of a given drug (eg, infusion + bolus)
calc_drug_total <- function(drugVars){
  ifelse(rowSums(!is.na(compliance[,drugVars])) == 0, NA,
         rowSums(compliance[,drugVars], na.rm = TRUE))
}

## Calculate daily totals for each drug (original scale); add to compliance data
drug_totals <- map(.x = drug_vars, .f = calc_drug_total) %>%
  bind_cols()
names(drug_totals) <- paste0(names(drug_totals), '.total')
compliance <- bind_cols(compliance, drug_totals)

## Checks
# sample_n(compliance[,c('id', 'redcap.event.name.f', drug_vars$hydromorphone, 'hydromorphone.total')], size = 15)

## Calculate total doses of
## - benzodiazepines (lorazepam, clonazepam, diazepam, midazolam, in lorazepam equivalents)
## - opioids (morphine, hydromorphone, fentanyl, in morphine equivalents)

## Class totals can be calculated if a) weight and b) at least one drug in those classes are
## available
benzo.drugs <- c('lorazepam', 'clonazepam', 'diazepam', 'midazolam')
opioid.drugs <- c('morphine', 'hydromorphone', 'fentanyl')

compliance$has.benzo <- rowSums(!is.na(compliance[,paste0(benzo.drugs, '.total')])) > 0
compliance$has.opioid <- rowSums(!is.na(compliance[,paste0(opioid.drugs, '.total')])) > 0

## Calculate scaled doses (eg, mg/kg) and total benzo/opioids
compliance <- compliance %>%
  left_join(dplyr::select(demog, id, wt), by = 'id') %>%
  mutate(loraz.scaled = ifelse(!has.benzo | is.na(wt), NA,
                        ifelse(is.na(lorazepam.total), 0, lorazepam.total / wt)),
         clonaz.scaled = ifelse(!has.benzo | is.na(wt), NA,
                         ifelse(is.na(clonazepam.total), 0, clonazepam.total / wt)),
         diaz.scaled = ifelse(!has.benzo | is.na(wt), NA,
                       ifelse(is.na(diazepam.total), 0, diazepam.total / wt)),
         midaz.scaled = ifelse(!has.benzo | is.na(wt), NA,
                               ifelse(is.na(midazolam.total), 0, midazolam.total / wt)),
         morph.scaled = ifelse(!has.opioid | is.na(wt), NA,
                        ifelse(is.na(morphine.total), 0, morphine.total / wt)),
         hydromorph.scaled = ifelse(!has.opioid | is.na(wt), NA,
                             ifelse(is.na(hydromorphone.total), 0, hydromorphone.total / wt)),
         fent.scaled = ifelse(!has.opioid | is.na(wt), NA,
                       ifelse(is.na(fentanyl.total), 0, fentanyl.total / wt)),
         ## Calculate total benzos (lorazepam equivalents), opioids (morphine equivalents)
         benzo.total = loraz.scaled + clonaz.scaled + (diaz.scaled / 0.2) + (midaz.scaled / 0.5),
         opioid.total = morph.scaled + (hydromorph.scaled / 4) + (fent.scaled / 10))

label(compliance$pain.info) <- 'Pain data available'
label(compliance$pain.threshold) <- '>=1 assessment with pain score >=5'
label(compliance$pain.morethan5) <- 'Assessments with pain score >=5'
label(compliance$sedation.info) <- 'Sedation data available'
label(compliance$on.majorsed) <- 'On continuous/intermittent sedation'
label(compliance$sat.info) <- 'Sedation interruption data available'
label(compliance$weaning.info) <- 'Sedation weaning data available'
label(compliance$had.sat) <- 'Sedation interrupted (SAT)'
label(compliance$had.weaning) <- 'Sedation weaned via protocol'
label(compliance$on.anysed) <- 'Received any sedation (inc. PRN)'
label(compliance$deepsed.info) <- 'Deep sedation data available'
label(compliance$deeply.sedated) <- 'Deeply sedated (SBS <= -2 or RASS <= -4)'
label(compliance$mv.info) <- 'Mechanical ventilation data available'
label(compliance$on.mv) <- 'Received mechanical ventilation'
label(compliance$sbt.safety.info) <- 'SBT safety screen data available'
label(compliance$had.sbt.safety) <- 'SBT safety screen performed'
label(compliance$sbt.info) <- 'SBT result data available'
label(compliance$had.sbt) <- 'Had SBT'
label(compliance$sbt.protocol) <- 'Followed SBT protocol'
label(compliance$anxiolysis.info) <- 'Anxiolysis data available'
label(compliance$anxiolysis.specifics) <- 'Received benzos, diphenhydramine and/or ketamine'
label(compliance$anxio.benz) <- 'Received benzos'
label(compliance$anxio.diphen) <- 'Received diphenhydramine'
label(compliance$anxio.ketam) <- 'Received ketamine'
label(compliance$delirium.assessments) <- 'Number of delirium assessments'
label(compliance$delirium.assessed) <- 'Delirium assessed'
label(compliance$delirium.present) <- 'Delirium present'
label(compliance$exer.safety.info) <- 'Mobilization safety screen data available'
label(compliance$had.exer.safety) <- 'Had mobilization safety screen'
label(compliance$exer.info) <- 'Mobilization data available'
label(compliance$had.exer) <- 'Mobilization therapy performed'
label(compliance$exer.protocol) <- 'Mobilization protocol performed'
label(compliance$family.present.info) <- 'Family visit data available'
label(compliance$family.present) <- 'Family member visited'
label(compliance$family.inter.info) <- 'Family intervention data available'
label(compliance$family.intervention) <- 'Family member participation in nonpharm intervention'
label(compliance$has.benzo) <- paste('At least one dose recorded of', paste(benzo.drugs, collapse = ', '))
label(compliance$has.opioid) <- paste('At least one dose recorded of', paste(opioid.drugs, collapse = ', '))
label(compliance$lorazepam.total) <- 'Total lorazepam (mg)'
label(compliance$clonazepam.total) <- 'Total clonazepam (mg)'
label(compliance$diazepam.total) <- 'Total diazepam (mg)'
label(compliance$midazolam.total) <- 'Total midazolam (mg)'
label(compliance$morphine.total) <- 'Total morphine (mg)'
label(compliance$fentanyl.total) <- 'Total fentanyl (mg)'
label(compliance$hydromorphone.total) <- 'Total hydromorphone (mcg)'
label(compliance$loraz.scaled) <- 'Lorazepam (mg/kg)'
label(compliance$clonaz.scaled) <- 'Clonazepam (mg/kg)'
label(compliance$diaz.scaled) <- 'Diazepam (mg/kg)'
label(compliance$midaz.scaled) <- 'Midazolam (mg/kg)'
label(compliance$morph.scaled) <- 'Morphine (mg/kg)'
label(compliance$fent.scaled) <- 'Fentanyl (mg/kg)'
label(compliance$hydromorph.scaled) <- 'Hydromorphone (mcg/kg)'
label(compliance$benzo.total) <- 'Total benzodiazepines, lorazepam equivalents'
label(compliance$opioid.total) <- 'Total opioids, morphine equivalents'

## -- Save data sets for use in aggregate and site-specific reports --------------------------------
save(site_guidelines, demog, compliance, file = 'RawData/pediatric.Rdata')
