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

## Character values present in PIM2 data entry; take care of these
demog$severityscore.a3 <- as.numeric(gsub("%", "",
                                          gsub("/", ".",
                                               demog$severityscore.a3,
                                               fixed = TRUE),
                                          fixed = TRUE))

## Admission diagnosis: Lump as directed in 6/2/2017 call
these_dx <- function(dxnums){
  if(length(!is.na(dxnums)) == 1){
    demog[,paste0('icu.dx.', dxnums)]
  } else{
    rowSums(demog[,paste0('icu.dx.', dxnums)]) > 0
  }
}

demog$dx.surgery <- these_dx(c(20, 22:26))
demog$dx.infection <- these_dx(c(1, 14))
demog$dx.resp <- these_dx(2:6)
demog$dx.gi <- these_dx(c(10, 12))
demog$dx.neuro <- these_dx(c(18, 21))
demog$dx.tbi <- these_dx(17)
demog$dx.burn <- these_dx(27)
demog$dx.malig <- these_dx(16)
demog$dx.other <- these_dx(c(7:9, 11:13, 15, 19, 99))

## Want to include all "other" admission diagnoses in report; need some string
## work to head off Latex errors
demog$icu.dx.other <- gsub("&", "and", demog$icu.dx.other, fixed = TRUE)

all.dx.other <-
  unique(subset(demog, !is.na(icu.dx.other) & icu.dx.other != "")$icu.dx.other)
dx.other.descrip <-
  do.call(paste,
          lapply(1:length(all.dx.other),
                 FUN = function(x){
                   paste0(x, '. ', trimws(all.dx.other[x]), ' \\\\\\\\')
                 }))

demog <- demog %>%
  ## Rename insensibly named variables more sensibly
  rename(ventdays = piculos2.c4c,
         prism3 = severityscore.a2,
         pim2 = severityscore.a3,
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
         ## Individual respiratory failure diagnoses
         resp.rds = as.logical(icu.dx.2),
         resp.airway = as.logical(icu.dx.3),
         resp.asthma = as.logical(icu.dx.4),
         resp.cld = as.logical(icu.dx.5),
         resp.pneu = as.logical(icu.dx.6),
         ## Hospital LOS: someone entered "not discharged"
         hosplos = as.numeric(as.character(piculos2.5b7)),
         ## Ever on MV; days on MV among patients exposed
         ever.vent = factor(ifelse(is.na(ventdays), 1,
                            ifelse(ventdays == 0, 2, 3)),
                            levels = 1:3,
                            labels = c('Unknown (no data available)',
                                       'Never on MV', '>=1 day on MV')),
         ## Version of ever on MV for testing - drop patients with no data
         ever.vent.test = factor(ifelse(is.na(ventdays), NA,
                                 ifelse(ventdays == 0, 1, 2)),
                                 levels = 1:2,
                                 labels = c("Never on MV", ">=1 day on MV")),
         ventdays.exp = ifelse(ventdays == 0, NA, ventdays)) %>%
  dplyr::select(id, hosp.f, data.time, month.cat, dataplan.combined, age.f,
                sex.f, race.combined, english.f, wt, severity.combined, prism3,
                pim2, dx.surgery, dx.infection, dx.resp, dx.gi, dx.neuro,
                dx.tbi, dx.burn, dx.malig, dx.other, resp.rds, resp.airway,
                resp.asthma, resp.cld, resp.pneu, piculos, hosplos, ever.vent,
                ever.vent.test, ventdays, ventdays.exp, fss.preadm, fss.adm,
                fss.dc, popc.preadm, popc.adm, popc.dc, mortality.f)

label(demog$hosp.f) <- 'Hospital'
label(demog$data.time) <- 'Time period'
label(demog$month.cat) <- 'Month'
label(demog$dataplan.combined) <- 'Data plan'
label(demog$age.f) <- 'Age category'
label(demog$sex.f) <- 'Sex'
label(demog$race.combined) <- 'Race'
label(demog$english.f) <- 'Language'
label(demog$severity.combined) <- 'SOI score used'
label(demog$prism3) <- 'PRISM III at admission'
label(demog$pim2) <- 'PIM 2 at admission'
label(demog$dx.surgery) <- "Surgery (any type)"
label(demog$dx.infection) <- "Infection (sepsis/septic shock, pneumonia, other)"
label(demog$dx.resp) <- "Respiratory failure (any type)"
label(demog$dx.gi) <- "GI bleed/hemorrhagic shock"
label(demog$dx.neuro) <- "Neurologic disease without trauma"
label(demog$dx.tbi) <- "Traumatic brain injury"
label(demog$dx.burn) <- "Burn"
label(demog$dx.malig) <- "Malignancy"
label(demog$dx.other) <- "Other"
label(demog$resp.rds) <- "~~~RDS without infection"
label(demog$resp.airway) <- "~~~Airway protection/obstruction"
label(demog$resp.asthma) <- "~~~Asthma"
label(demog$resp.cld) <- "~~~Chronic lung disease"
label(demog$resp.pneu) <- "~~~Pneumonia"
label(demog$piculos) <- 'PICU length of stay'
label(demog$hosplos) <- 'Hospital length of stay'
label(demog$ventdays) <- 'Days on MV, all patients'
label(demog$ever.vent) <- 'Ever on mechanical ventilation'
label(demog$ever.vent.test) <- 'Ever on mechanical ventilation'
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

## Rename some "number of assessments" variables to be consistent with VAS, FPS
names(compliance) <- gsub("^pain\\.assessments$", "pain\\.nrs1",
                          names(compliance))
names(compliance) <- gsub("^pain\\.assessments2\\.4ab$", "pain\\.oucher1",
                          names(compliance))
names(compliance) <- gsub("^pain\\.assessments2\\.4ab2\\.7ca$", "pain\\.flacc1",
                          names(compliance))
names(compliance) <- gsub("^pain\\.assessments2\\.4ab2\\.9e2$", "pain\\.other1",
                          names(compliance))

## Pain: changed from pain.morethan5 only to whether anything in pain section
## is filled out, per 6/2/2017 call
pain.vars <- paste0("pain.",
                    c("opportunity",
                      paste0("scale.", 1:6),
                      "vas1", "vas2", "fps1", "fps2", "nrs1", "nrs2",
                      "oucher1", "oucher2", "flacc1", "flacc2",
                      "other1", "other2", "2hrlowestscore",
                      paste0("hightreatment.", 1:9), "highother",
                      "effective", "morethan5"))

## Checkbox variables are my nemesis
make_0_NA <- function(vname){
  ifelse(compliance[,vname] == 0, NA, compliance[,vname])
}

compliance$pain.scale.1 <- make_0_NA("pain.scale.1")
compliance$pain.scale.2 <- make_0_NA("pain.scale.2")
compliance$pain.scale.3 <- make_0_NA("pain.scale.3")
compliance$pain.scale.4 <- make_0_NA("pain.scale.4")
compliance$pain.scale.5 <- make_0_NA("pain.scale.5")
compliance$pain.scale.6 <- make_0_NA("pain.scale.6")
compliance$pain.hightreatment.1 <- make_0_NA("pain.hightreatment.1")
compliance$pain.hightreatment.2 <- make_0_NA("pain.hightreatment.2")
compliance$pain.hightreatment.3 <- make_0_NA("pain.hightreatment.3")
compliance$pain.hightreatment.4 <- make_0_NA("pain.hightreatment.4")
compliance$pain.hightreatment.5 <- make_0_NA("pain.hightreatment.5")
compliance$pain.hightreatment.6 <- make_0_NA("pain.hightreatment.6")
compliance$pain.hightreatment.7 <- make_0_NA("pain.hightreatment.7")
compliance$pain.hightreatment.8 <- make_0_NA("pain.hightreatment.8")
compliance$pain.hightreatment.9 <- make_0_NA("pain.hightreatment.9")

compliance$pain.info <- rowSums(!is.na(compliance[,pain.vars])) > 0
compliance$sedation.info <- rowSums(compliance[,paste0('sedative.', 0:3)]) > 0
compliance$mv.info <- !is.na(compliance$invasivemv.f)
compliance$anxiolysis.info <- rowSums(compliance[,paste0('anxiolysis.', 1:4)]) > 0
compliance$anxiolysis.specifics <-
  ifelse(!compliance$anxiolysis.info, NA,
         rowSums(compliance[,paste0('anxio.med.', c(1:4, 6, 9))]) > 0)

compliance <- compliance %>%
  mutate(## A:
         ## Out of all opportunities to assess pain, proportion actually taken?
         ## Several different scales -> several different sets of variables
         pain.assess.info = (!is.na(pain.opportunity) & pain.opportunity > 0) &
                             ((!is.na(pain.scale.1) & !is.na(pain.vas1)) |
                              (!is.na(pain.scale.2) & !is.na(pain.fps1)) |
                              (!is.na(pain.scale.3) & !is.na(pain.nrs1)) |
                              (!is.na(pain.scale.4) & !is.na(pain.oucher1)) |
                              (!is.na(pain.scale.5) & !is.na(pain.flacc1)) |
                              (!is.na(pain.scale.6) & !is.na(pain.other1))),
         pain.prop.assessed = ifelse(!pain.assess.info, NA,
                              ifelse(!is.na(pain.scale.1) & !is.na(pain.vas1),
                                     pain.vas1 / pain.opportunity,
                              ifelse(!is.na(pain.scale.2) & !is.na(pain.fps1),
                                     pain.fps1 / pain.opportunity,
                              ifelse(!is.na(pain.scale.3) & !is.na(pain.nrs1),
                                     pain.nrs1 / pain.opportunity,
                              ifelse(!is.na(pain.scale.4) & !is.na(pain.oucher1),
                                     pain.oucher1 / pain.opportunity,
                              ifelse(!is.na(pain.scale.5) & !is.na(pain.flacc1),
                                     pain.flacc1 / pain.opportunity,
                              ifelse(!is.na(pain.scale.6) & !is.na(pain.other1),
                                     pain.other1 / pain.opportunity, NA))))))),
         ## Cap proportion of assessments completed at 100%
         pain.prop.assessed = ifelse(!is.na(pain.prop.assessed) &
                                       pain.prop.assessed > 1, 100,
                                     pain.prop.assessed*100),
         ## Was pain score within 2 hours of highest assessment < 5?
         pain.2hrs.info = !is.na(pain.2hrlowestscore),
         pain.2hrs.threshold = factor(ifelse(!pain.2hrs.info, NA,
                                      ifelse(pain.2hrlowestscore < 5, 1,
                                      ifelse(pain.2hrlowestscore == 11, 3, 2))),
                                      levels = 1:3,
                                      labels = c('<5', '>=5', 'Not reassessed')),
         ## How many days had at least one assessment with a score > 5?
         pain.threshold.info = !is.na(pain.morethan5),
         pain.threshold = ifelse(is.na(pain.morethan5), NA, pain.morethan5 > 0),
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
## - benzodiazepines (lorazepam, midazolam, in lorazepam equivalents)
## - opioids (morphine, hydromorphone, fentanyl, in morphine equivalents)

## Class totals can be calculated if a) weight and b) at least one drug in those
## classes are available
benzo.drugs <- c('lorazepam', 'midazolam')
opioid.drugs <- c('morphine', 'hydromorphone', 'fentanyl')

compliance$has.benzo <-
  rowSums(!is.na(compliance[,paste0(benzo.drugs, '.total')])) > 0
compliance$has.opioid <-
  rowSums(!is.na(compliance[,paste0(opioid.drugs, '.total')])) > 0

## Calculate scaled doses (eg, mg/kg) and total benzo/opioids
compliance <- compliance %>%
  left_join(dplyr::select(demog, id, wt), by = 'id') %>%
  mutate(loraz.scaled = ifelse(!has.benzo | is.na(wt), NA,
                        ifelse(is.na(lorazepam.total), 0, lorazepam.total / wt)),
         loraz.rcvd = ifelse(!has.benzo, NA,
                             !is.na(lorazepam.total) & lorazepam.total > 0),
         loraz.scaled.eo = ifelse(is.na(loraz.scaled) | loraz.scaled == 0, NA,
                                  loraz.scaled),
         clonaz.scaled = ifelse(!has.benzo | is.na(wt), NA,
                         ifelse(is.na(clonazepam.total), 0,
                                clonazepam.total / wt)),
         clonaz.scaled.eo = ifelse(is.na(clonaz.scaled) | clonaz.scaled == 0, NA,
                                   clonaz.scaled),
         diaz.scaled = ifelse(!has.benzo | is.na(wt), NA,
                       ifelse(is.na(diazepam.total), 0, diazepam.total / wt)),
         ## Clonazepam/diazepam only correct if included in benzo vars above
         clonaz.rcvd = ifelse(!has.benzo, NA,
                              !is.na(clonazepam.total) & clonazepam.total > 0),
         diaz.rcvd = ifelse(!has.benzo, NA,
                            !is.na(diazepam.total) & diazepam.total > 0),
         diaz.scaled.eo = ifelse(is.na(diaz.scaled) | diaz.scaled == 0, NA,
                                 diaz.scaled),
         midaz.scaled = ifelse(!has.benzo | is.na(wt), NA,
                               ifelse(is.na(midazolam.total), 0, midazolam.total / wt)),
         midaz.rcvd = ifelse(!has.opioid, NA,
                             !is.na(midazolam.total) & midazolam.total > 0),
         midaz.scaled.eo = ifelse(is.na(midaz.scaled) | midaz.scaled == 0, NA,
                                  midaz.scaled),
         morph.scaled = ifelse(!has.opioid | is.na(wt), NA,
                        ifelse(is.na(morphine.total), 0, morphine.total / wt)),
         morph.rcvd = ifelse(!has.opioid, NA,
                             !is.na(morphine.total) & morphine.total > 0),
         morph.scaled.eo = ifelse(is.na(morph.scaled) | morph.scaled == 0, NA,
                                  morph.scaled),
         hydromorph.scaled = ifelse(!has.opioid | is.na(wt), NA,
                             ifelse(is.na(hydromorphone.total), 0,
                                    hydromorphone.total / wt)),
         hydromorph.rcvd = ifelse(!has.opioid, NA,
                                  !is.na(hydromorphone.total) &
                                    hydromorphone.total > 0),
         hydromorph.scaled.eo = ifelse(is.na(hydromorph.scaled) |
                                         hydromorph.scaled == 0, NA,
                                       hydromorph.scaled),
         fent.scaled = ifelse(!has.opioid | is.na(wt), NA,
                       ifelse(is.na(fentanyl.total), 0, fentanyl.total / wt)),
         fent.rcvd = ifelse(!has.opioid, NA,
                            !is.na(fentanyl.total) & fentanyl.total > 0),
         fent.scaled.eo = ifelse(is.na(fent.scaled) | fent.scaled == 0, NA,
                                 fent.scaled),
         ## Calculate total benzos (lorazepam equivalents), opioids (morphine equivalents)
         benzo.total = loraz.scaled + (midaz.scaled / 0.5),
         benzo.rcvd = ifelse(!has.benzo, NA,
                             !is.na(benzo.total) & benzo.total > 0),
         benzo.total.eo = ifelse(is.na(benzo.total) | benzo.total == 0, NA,
                                 benzo.total),
         opioid.total = morph.scaled +
                          (hydromorph.scaled / 4) +
                          (fent.scaled / 10),
         opioid.rcvd = ifelse(!has.opioid, NA,
                              !is.na(opioid.total) & opioid.total > 0),
         opioid.total.eo = ifelse(is.na(opioid.total) | opioid.total == 0, NA,
                                  opioid.total))

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
label(compliance$has.benzo) <-
  paste('At least one dose recorded of', paste(benzo.drugs, collapse = ', '))
label(compliance$has.opioid) <-
  paste('At least one dose recorded of', paste(opioid.drugs, collapse = ', '))
label(compliance$lorazepam.total) <- 'Total lorazepam (mg/day)'
label(compliance$clonazepam.total) <- 'Total clonazepam (mg/day)'
label(compliance$diazepam.total) <- 'Total diazepam (mg/day)'
label(compliance$midazolam.total) <- 'Total midazolam (mg/day)'
label(compliance$morphine.total) <- 'Total morphine (mg/day)'
label(compliance$fentanyl.total) <- 'Total fentanyl (mg/day)'
label(compliance$hydromorphone.total) <- 'Total hydromorphone (mcg/day)'
label(compliance$loraz.scaled) <- 'Lorazepam (mg/kg/day)'
label(compliance$clonaz.scaled) <- 'Clonazepam (mg/kg/day)'
label(compliance$diaz.scaled) <- 'Diazepam (mg/kg/day)'
label(compliance$midaz.scaled) <- 'Midazolam (mg/kg/day)'
label(compliance$morph.scaled) <- 'Morphine (mg/kg/day)'
label(compliance$fent.scaled) <- 'Fentanyl (mg/kg/day)'
label(compliance$hydromorph.scaled) <- 'Hydromorphone\\\\~~~~~~(mcg/kg/day)'
label(compliance$benzo.total) <- 'Total benzodiazepines, lorazepam equivalents, mg/kg/day'
label(compliance$opioid.total) <- 'Total opioids, morphine equivalents, mg/kg/day'
label(compliance$loraz.rcvd) <- 'Received lorazepam'
label(compliance$midaz.rcvd) <- 'Received midazolam'
label(compliance$clonaz.rcvd) <- 'Received clonazepam'
label(compliance$diaz.rcvd) <- 'Received diazepam'
label(compliance$morph.rcvd) <- 'Received morphine'
label(compliance$fent.rcvd) <- 'Received fentanyl'
label(compliance$hydromorph.rcvd) <- 'Received hydromorphone'
label(compliance$benzo.rcvd) <- 'Received benzodiazepines'
label(compliance$opioid.rcvd) <- 'Received opioids'
label(compliance$loraz.scaled.eo) <- 'Lorazepam (mg/kg/day) among exposed'
label(compliance$clonaz.scaled.eo) <- 'Clonazepam (mg/kg/day) among exposed'
label(compliance$diaz.scaled.eo) <- 'Diazepam (mg/kg/day) among exposed'
label(compliance$midaz.scaled.eo) <- 'Midazolam (mg/kg/day) among exposed'
label(compliance$morph.scaled.eo) <- 'Morphine (mg/kg/day)\\\\~~~~~~among exposed'
label(compliance$fent.scaled.eo) <- 'Fentanyl (mg/kg/day)\\\\~~~~~~among exposed'
label(compliance$hydromorph.scaled.eo) <- 'Hydromorphone\\\\~~~~~~(mcg/kg/day)\\\\~~~~~~among exposed'
label(compliance$benzo.total.eo) <- 'Total benzodiazepines, lorazepam equivalents, mg/kg/day, among exposed'
label(compliance$opioid.total.eo) <- 'Total opioids, morphine equivalents, mg/kg/day, among exposed'

## -- Save data sets for use in aggregate and site-specific reports --------------------------------
save(site_guidelines, demog, compliance, dx.other.descrip,
     file = 'RawData/pediatric.Rdata')

