## Load libraries
library(Hmisc)
library(JTHelpers)
library(tidyverse)
library(devtools)

## What's the last month that should be included in the report?
last.month <- 20

## Wrapper function for latex-ing summaryM objects
my.print.summaryM <- function(...){
  latex.summaryM(file = '', where = '!h', digits = 2, prmsd = TRUE, long = TRUE,
                 npct = 'both', what = '%', ...)
}

## -- Function to calculate & plot proportions of patient-days by time and, if needed, -------------
## -- faceting variable ----------------------------------------------------------------------------
ggplot.prop <-
  function(var,                                                  ## X variable to plot
           facet.var = NULL,                                     ## variable to facet by
           facet.col = 1,                                        ## number of columns for faceting
           use.na = c('no', 'ifany', 'always'),                  ## include missing values in facets
           dataset = subset(compliance.site, icu.24.f == 'Yes'), ## data set to use
           use.ylab = 'Percent of ICU Days',                     ## Y axis label
           use.xlab = NULL,                                      ## X axis label
           x.limits = NULL,                                      ## X axis limits
           x.breaks = NULL,                                      ## breaks to use for X axis
           x.angle = 0, x.hjust = 0.5,                           ## X axis text formatting
           axis.text.size = 7,                                   ## axis text size
           axis.title.size = 8,                                  ## axis title size
           time.colors = c('#9ecae1', '#08306b'),                ## colors for bars
           use.legend = TRUE,                                    ## include legend?
           legend.text.size = 7,                                 ## legend text size
           legend.key.cm = 0.4,                                  ## legend key size in cm
           strip.text.size = 8,                                  ## strip text size
           title.string = var,                                   ## plot title; default: X var name
           title.size = 9,                                       ## font size for main title
           title.hjust = 0){                                     ## justification for plot title

    use.na <- match.arg(use.na)

    ## Get number in each cell
    if(!is.null(facet.var)){
      crosstab.data <- eval(parse(text = paste0("with(dataset, as.data.frame(table(", facet.var, ", ",
                                                var, ", data.time, useNA = '", use.na, "')))")))
      if(nrow(crosstab.data) > 0){
        names(crosstab.data) <- gsub(facet.var, 'facetvar', names(crosstab.data))
      }
    } else{
      crosstab.data <- eval(parse(text = paste0("with(dataset, as.data.frame(table(", var,
                                                ", data.time, useNA = '", use.na, "')))")))
      if(nrow(crosstab.data) > 0){
        crosstab.data$facetvar <- 'All'
      }
    }
    if(nrow(crosstab.data) == 0){
      prop.plot <- ggplot(data = crosstab.data) +
        scale_y_continuous(name = use.ylab,
                           limits = c(0, 1),
                           breaks = seq(0, 1, 0.25),
                           labels = paste0(seq(0, 100, 25), '%')) +
        scale_fill_manual(name = NULL, values = time.colors) +
        ggtitle(title.string) +
        theme(legend.position = 'bottom',
              legend.direction = 'horizontal',
              legend.key.size = unit(legend.key.cm, 'cm'),
              legend.text = element_text(size = legend.text.size),
              strip.text = element_text(face = 'bold', size = strip.text.size),
              plot.title = element_text(hjust = title.hjust, face = 'bold', size = title.size),
              axis.text.x = element_text(angle = x.angle, hjust = x.hjust, size = axis.text.size),
              axis.text.y = element_text(size = axis.text.size),
              axis.title = element_text(size = axis.title.size))
    } else{
      names(crosstab.data) <- gsub(var, 'proplevel', tolower(names(crosstab.data)))

      ## If original X variable is numeric, force tabulated form to be numeric
      if('integer' %in% class(dataset[,var]) | 'numeric' %in% class(dataset[,var])){
        crosstab.data$proplevel <- as.numeric(as.character(crosstab.data$proplevel))
      }

      ## Get totals for each time point
      time.totals <- crosstab.data %>%
        group_by(facetvar, data.time) %>%
        summarise(time.n = sum(freq, na.rm = TRUE)) %>%
        ungroup()

      ## Merge time totals, calculate proportions
      crosstab.data <- crosstab.data %>%
        left_join(time.totals, by = c('facetvar', 'data.time'), all = TRUE) %>%
        mutate(prop = freq / time.n)

      ## Create plot
      prop.plot <- ggplot(data = crosstab.data, aes(x = proplevel, y = prop, fill = data.time)) +
        geom_bar(stat = 'identity', position = 'dodge') +
        scale_y_continuous(name = use.ylab,
                           limits = c(0, 1),
                           breaks = seq(0, 1, 0.25),
                           labels = paste0(seq(0, 100, 25), '%')) +
        scale_fill_manual(name = NULL, values = time.colors) +
        ggtitle(title.string) +
        theme(legend.position = 'bottom',
              legend.direction = 'horizontal',
              legend.key.size = unit(legend.key.cm, 'cm'),
              legend.text = element_text(size = legend.text.size),
              strip.text = element_text(face = 'bold', size = strip.text.size),
              plot.title = element_text(hjust = title.hjust, face = 'bold', size = title.size),
              axis.text.x = element_text(angle = x.angle, hjust = x.hjust, size = axis.text.size),
              axis.text.y = element_text(size = axis.text.size),
              axis.title = element_text(size = axis.title.size))

      ## Facet plot, if needed
      if(!is.null(facet.var)){
        prop.plot <- prop.plot + facet_wrap(~ facetvar, ncol = facet.col)
      }

      ## Remove legend if requested
      if(!use.legend){
        prop.plot <- prop.plot + guides(fill = 'none')
      }
    }

    ## X axis breaks and title
    if(!is.null(x.breaks)){
      if(!is.null(x.limits)){
        prop.plot <- prop.plot +
          scale_x_continuous(name = use.xlab, limits = x.limits, breaks = x.breaks)
      } else{
        prop.plot <- prop.plot + scale_x_continuous(name = use.xlab, breaks = x.breaks)
      }
    } else{
      prop.plot <- prop.plot + xlab(use.xlab)
    }

    return(prop.plot)
  }

## -- Read in data, create factors/labels using REDCap scripts -------------------------------------
## Demographics form
demog <- read.csv('RawData/demog.csv', stringsAsFactors = FALSE)
source('demog_redcap.R')
names(demog) <- gsub('\\.factor', '\\.f', gsub('_+', '.', names(demog)))

## Compliance form
compliance <- read.csv('RawData/compliance.csv', stringsAsFactors = FALSE)
source('compliance_redcap.R')
names(compliance) <- gsub('\\.factor', '\\.f', gsub('_+', '.', names(compliance)))

## Add label for complete data variable
label(compliance$daily.compliance.form.complete.f) <- 'Daily form status'

## Spreadsheet from Vishakha with hospital regions, types
hosp.types.region <- read.csv('RawData/hospital_types_regions.csv', stringsAsFactors = FALSE) %>%
  filter(hosp.code != '') %>%
  mutate(hosp.type = factor(gsub(' +', '', hosp.type)),
         hosp.name = gsub(' +', '', hosp.name))

## -- Remove any test patients ---------------------------------------------------------------------
## Get list of IDs which don't meet required format to send Vishakha
reformat.ids <-
  demog$id[grep('^[A-Z][A-Z][A-Z][0-9][0-9][0-9][0-9]$', toupper(demog$id), invert = TRUE)]

## Remove any patient with "test" in the ID
demog <- demog[grep('test', tolower(demog$id), invert = TRUE),]
compliance <- compliance[grep('test', tolower(compliance$id), invert = TRUE),]

## Which IDs are used >1 time?
multiple.pt.ids <- subset(as.data.frame(table(demog$id)), Freq > 1)

## -- Remove any data collected after last.month ---------------------------------------------------
keep.ids <- subset(demog, !is.na(month) & month <= last.month)$id
nomonth.ids <- length(subset(demog, is.na(month))$id)
## 97, 98, 99 are no longer in use
afterlastmonth.ids <- length(subset(demog, !is.na(month) & month > last.month & month < 97)$id)

demog <- subset(demog, id %in% keep.ids)
compliance <- subset(compliance, id %in% keep.ids)

## -- Data management for demographic form ---------------------------------------------------------
## Create indicator for retrospective (mos 1-6) vs. prospective (mos 7-) data collection
demog$data.time <- with(demog, {
  factor(ifelse(month <= 6, 1, 2), levels = 1:2, labels = c('Retrospective', 'Prospective')) })

## Create variable for retrospective vs each following month
max.month <- max(subset(demog, month < 97)$month, na.rm = TRUE)
demog$data.month <- with(demog, {
  factor(ifelse(is.na(month) | month %in% 97:99, NA,
                ifelse(month <= 6, 6, month)),
         levels = 6:max.month, labels = c('Retrospective', paste('Month', 7:max.month))) })
demog$data.month.short <- with(demog, {
  factor(ifelse(is.na(month) | month %in% 97:99, NA,
                ifelse(month <= 6, 6, month)),
         levels = 6:max.month, labels = c('Retro.', paste0('M', 7:max.month))) })

## Merge on hospital region, type
demog <- merge(demog, hosp.types.region,
               by.x = c('hosp.f'), by.y = c('hosp.redcap'), all.x = TRUE, all.y = FALSE)

## Relevel region variable - WC, MW, EC instead of alphabetical
demog$hosp.region <- factor(as.character(demog$hosp.region), levels = c('WC', 'MW', 'EC'))

## Recategorize age per request
demog$agecat.f <- with(demog, factor(ifelse(is.na(age.f), NA,
                                     ifelse(age.f %in% c('< 18', '18-29', '30-39', '40-49'), 1,
                                     ifelse(age.f %in% c('50-59', '60-69'), 2,
                                     ifelse(age.f %in% c('70-79', '80-89'), 3, 4)))),
                                     levels = 1:4,
                                     labels = c('18-49', '50-69', '70-89', '90+')))

## Create single race variable
demog$race.f <- with(demog, {
  factor(ifelse(rowSums(demog[,grep('^race\\.[1-6]$', names(demog))]) == 0, 8,
         ifelse(rowSums(demog[,grep('^race\\.[1-6]$', names(demog))]) > 1, 7,
         ifelse(race.1 == 1, 1,
         ifelse(race.2 == 1, 2,
         ifelse(race.3 == 1, 3,
         ifelse(race.4 == 1, 4,
         ifelse(race.5 == 1, 5, NA))))))),
         levels = 1:8,
         labels = c('American Indian/AK Native',
                    'Black/African American',
                    'White',
                    'Asian',
                    'Native HI/Pacific Islander',
                    'Other/nonspecified',
                    'Multiple races indicated',
                    'Missing data')) })

## Shorten levels as needed
levels(demog$dc.status.f) <- c('Died in ICU, collaborative stay',
                               'Died in ICU, not collaborative stay',
                               'Died during hospitalization, not ICU',
                               'Discharged alive')

## Ever received invasive, noninvasive MV?
demog$ever.invas.vent <- with(demog, factor(ifelse(is.na(invas.vent.episode), NA,
                                                   ifelse(invas.vent.episode > 0, 1, 0)),
                                            levels = 0:1, labels = c('No', 'Yes')))

demog$ever.noninvas.vent <- with(demog, factor(ifelse(is.na(noninvas.vent.episode), NA,
                                                      ifelse(noninvas.vent.episode > 0, 1, 0)),
                                               levels = 0:1, labels = c('No', 'Yes')))

## Total time on invasive, noninvasive MV
demog$hrs.invas.vent <- with(demog, {
  ifelse(is.na(invas.vent.episode) | invas.vent.episode == 0, NA,
         rowSums(demog[,paste0('invas.vent.length.', 1:6)], na.rm = TRUE)) })

demog$hrs.noninvas.vent <- with(demog, {
  ifelse(is.na(noninvas.vent.episode) | noninvas.vent.episode == 0, NA,
         rowSums(demog[,paste0('invas.vent.length.', 1:6)], na.rm = TRUE)) })

label(demog$data.time) <- 'Time of data collection'
label(demog$data.plan.f) <- 'Data plan'
label(demog$hosp.region) <- 'Hospital region'
label(demog$month.f) <- 'Month in Collaborative ICU'
label(demog$age.f) <- 'Age range'
label(demog$agecat.f) <- 'Age category'
label(demog$sex.f) <- 'Sex'
label(demog$race.f) <- 'Race'
label(demog$hispanic.f) <- 'Hispanic'
label(demog$english.f) <- 'English-speaking'
label(demog$invas.vent.episode) <- 'Episodes of invasive MV'
label(demog$ever.invas.vent) <- 'Ever received invasive MV'
label(demog$hrs.invas.vent) <- 'Total hours on invasive MV'
label(demog$noninvas.vent.episode) <- 'Episodes of noninvasive MV'
label(demog$ever.noninvas.vent) <- 'Ever received noninvasive MV'
label(demog$hrs.noninvas.vent) <- 'Total hours on noninvasive MV'
label(demog$icu.los) <- 'ICU length of stay (days)'
label(demog$hosp.losv) <- 'Hospital length of stay (days)'
label(demog$dc.status.f) <- 'Discharge/mortality status'


## -- Data management for compliance form ---------------------------------------------------------
compliance <- compliance %>%
  mutate(## Denominator for all following variables: days in ICU full 24h
         comp.tracked = !is.na(icu.24.f) & icu.24.f == 'Yes',
         ## A: Assess, prevent and manage pain ##
         ## Compliant if received >=6 pain assessments in 24h using a PAD-recommended instrument
         n.painassess.icu = ifelse(!comp.tracked, NA,
                            ifelse(!is.na(pain.valid) & !is.na(pain.verbal), pain.valid + pain.verbal,
                            ifelse(!is.na(pain.valid), pain.valid,
                            ifelse(!is.na(pain.verbal), pain.verbal, NA)))),
         ## If no pain assessment data was recorded, day is considered noncompliant
         comp.a = ifelse(!comp.tracked, NA, !is.na(n.painassess.icu) & n.painassess.icu >= 6),
         ## B: both SAT and SBT ##
         ## On sedative: In ICU all 24h + either continuous infusion or intermittent sched sedation
         on.sedative.icu = ifelse(!comp.tracked, NA, sedative.1 == 1 | sedative.2 == 1),
         ## Compliant on SAT: on sedation + SAT screen passed + SAT performed
         comp.b.sat = ifelse(!on.sedative.icu, NA,
                      ifelse(is.na(satscreen.f) | is.na(satperformed.f), FALSE,
                             satscreen.f == 'Failed' |
                               (satscreen.f == 'Passed' & satperformed.f == 'Yes'))),
         ## SAT performed: on sedation + SAT performed, regardless of safety screen
         perf.b.sat = ifelse(!on.sedative.icu, NA, !is.na(satperformed.f) & satperformed.f == 'Yes'),
         ## On mechanical ventilation: in ICU all 24h + on MV today
         on.mv.icu = ifelse(!comp.tracked, NA,
                     ifelse(is.na(venttoday.f) | venttoday.f == 'No', FALSE, TRUE)),
         ## Compliant on SBT: on MV + SBT screen passed + SBT performed
         comp.b.sbt = ifelse(!on.mv.icu, NA,
                      ifelse(is.na(sbtscreen.f) | is.na(sbtperformed.f), FALSE,
                             sbtscreen.f == 'Failed' |
                               (sbtscreen.f == 'Passed' & sbtperformed.f == 'Yes'))),
         ## SBT performed: on MV + SBT performed, regardless of safety screen
         perf.b.sbt = ifelse(!on.mv.icu, NA, !is.na(sbtperformed.f) & sbtperformed.f == 'Yes'),
         ## Compliant on SAT+SBT: SAT and SBT both done and SAT performed before an SBT
         had.satsbt.icu = ifelse(!comp.tracked, NA,
                                 !is.na(satperformed.f) & satperformed.f == 'Yes' &
                                   !is.na(sbtperformed.f) & sbtperformed.f == 'Yes'),
         comp.b.satsbt = ifelse(!had.satsbt.icu, NA, !is.na(satsbt.f) & satsbt.f == 'Yes'),

         ## C: Choice of analgesia and sedation ##
         ## Compliant if patient had >=6 PAD-recommended sedation/agitation assessments
         sed.assess.icu = ifelse(!comp.tracked, NA, sed.assess.valid),
         comp.c = ifelse(!comp.tracked, NA,
                  ifelse(is.na(sed.assess.icu) | sed.assess.icu < 6, FALSE, TRUE)),

         ## Specific analgesics/sedatives in ICU
         opioid.icu = ifelse(!comp.tracked, NA, meds.1 == 1),
         benzo.icu = ifelse(!comp.tracked, NA, meds.2 == 1),
         propofol.icu = ifelse(!comp.tracked, NA, meds.3 == 1),
         dex.icu = ifelse(!comp.tracked, NA, meds.4 == 1),
         ketam.icu = ifelse(!comp.tracked, NA, meds.5 == 1),
         antipsyc.typ.icu = ifelse(!comp.tracked, NA, meds.6 == 1),
         antipsyc.atyp.icu = ifelse(!comp.tracked, NA, meds.7 == 1),
         no.sedanal.icu = ifelse(!comp.tracked, NA,
                                 meds.99 == 1 &
                                  meds.1 == 0 & meds.2 == 0 & meds.3 == 0 & meds.4 == 0 &
                                  meds.5 == 0 & meds.6 == 0 & meds.7 == 0),

         ## D: Delirium - assess, prevent and manage ##
         ## Compliant if patient had >=2 PAD-recommended delirium assessments
         delirium.assess.icu = ifelse(!comp.tracked, NA, delirium.assess.valid),
         comp.d = ifelse(!comp.tracked, NA,
                  ifelse(is.na(delirium.assess.icu) | delirium.assess.icu < 2, FALSE, TRUE)),

         ## E: Early mobility and exercise ##
         ## Compliant if patient passed safety screen and received at least dangling for mobility
         ## (active ROM does not count for compliance)
         mobility.high.icu = factor(ifelse(!comp.tracked, NA, mobilityhighest),
                                    levels = 1:8, labels = levels(compliance$mobilityhighest.f)),
         comp.e = ifelse(!comp.tracked, NA,
                  ifelse(is.na(mobilityscreen.f) | is.na(mobilityperformed.f) |
                           is.na(mobility.high.icu), FALSE,
                         mobilityscreen.f == 'Failed' |
                           (mobilityscreen.f == 'Passed' & mobilityperformed.f == 'Yes' &
                             !(mobility.high.icu %in% c('Active ROM - in bed',
                                                        'Not documented /unclear'))))),
         ## Mobility performed: ICU day and some mobility performed other than active ROM
         perf.e = ifelse(!comp.tracked, NA,
                         !is.na(mobility.high.icu) &
                           !(mobility.high.icu %in% c('Active ROM - in bed',
                                                      'Not documented /unclear'))),

         ## F: Family engagement ##
         ## Compliant if family member/SO participated in rounds, conference, etc ##
         family.present.icu = ifelse(!comp.tracked, NA,
                              ifelse(is.na(familyvisit.f) | !(familyvisit.f == 'Yes'), FALSE,
                                     TRUE)),
         comp.f = ifelse(!family.present.icu, NA,
                         familyinvite.2 == 1 | familyinvite.3 == 1 |
                         familyparticipate.1 == 1 | familyparticipate.2 == 1 |
                         familyeducate.1 == 1 | familyeducate.2 == 1 | familyeducate.3 == 1 |
                         familyeducate.4 == 1 | familyeducate.5 == 1)) %>%
  left_join(dplyr::select(demog, id, hosp.f))

## **Overall** compliance and performance on ABCDEF bundle
## If patient on MV + sedation, all elements should be present
## If patient not on MV, ignore SBT element
## If patient not on sedation, ignore SAT element
## If family not present, ignore F element

element.suffixes <- c('a', 'b.sat', 'b.sbt', 'c', 'd', 'e', 'f')

compliance$elements.elig <- rowSums(!is.na(compliance[,paste0('comp.', element.suffixes)]))
compliance$elements.elig <- with(compliance, ifelse(!comp.tracked, NA, elements.elig))
compliance$elements.comp <- rowSums(compliance[,paste0('comp.', element.suffixes)], na.rm = TRUE)
compliance$elements.comp <- with(compliance, ifelse(!comp.tracked, NA, elements.comp))
compliance$elements.perf <- rowSums(compliance[,c(paste0('comp.', c('a', 'c', 'd', 'f')),
                                                  paste0('perf.', c('b.sat', 'b.sat', 'e')))],
                                    na.rm = TRUE)
compliance$elements.perf <- with(compliance, ifelse(!comp.tracked, NA, elements.perf))
compliance$bundle.comp <- with(compliance, elements.comp == elements.elig)
compliance$bundle.perf <- with(compliance, elements.perf == elements.elig)

## Relevel mobility variable for table (with all 18 months, runs off the page)
compliance$mobility.high.shortlabs <- compliance$mobility.high.icu
levels(compliance$mobility.high.shortlabs) <- c('Active ROM', 'Dangle', 'Stand', 'Active transfer',
                                                'March in place', 'Walk in room', 'Walk in hall',
                                                'Not documented')

label(compliance$comp.tracked) <- 'ABCDEF compliance tracked'
label(compliance$n.painassess.icu) <- 'Pain assessments/day in ICU'
label(compliance$comp.a) <- 'Pain assessment compliance'
label(compliance$on.sedative.icu) <- 'On infused or scheduled sedation in ICU'
label(compliance$comp.b.sat) <- 'SAT protocol compliance'
label(compliance$perf.b.sat) <- 'SAT performed'
label(compliance$on.mv.icu) <- 'On MV in ICU'
label(compliance$comp.b.sbt) <- 'SBT protocol compliance'
label(compliance$perf.b.sbt) <- 'SBT performed'
label(compliance$had.satsbt.icu) <- 'Eligible for SAT and SBT'
label(compliance$comp.b.satsbt) <- 'SAT prior to SBT'
label(compliance$sed.assess.icu) <- 'Sedation assessments/day in ICU'
label(compliance$comp.c) <- 'Sedation assessment compliance'
label(compliance$opioid.icu) <- 'Opioids'
label(compliance$benzo.icu) <- 'Benzodiazepines'
label(compliance$propofol.icu) <- 'Propofol'
label(compliance$dex.icu) <- 'Dexmedetomidine'
label(compliance$ketam.icu) <- 'Ketamine'
label(compliance$antipsyc.typ.icu) <- 'Typical antipsy.'
label(compliance$antipsyc.atyp.icu) <- 'Atypicals'
label(compliance$no.sedanal.icu) <- 'No sed./analgesics'
label(compliance$delirium.assess.icu) <- 'Delirium assessments/day in ICU'
label(compliance$comp.d) <- 'Delirium assessment compliance'
label(compliance$mobility.high.icu) <- 'Highest level of exercise/mobilization'
label(compliance$mobility.high.shortlabs) <- 'Highest exercise/mob.'
label(compliance$comp.e) <- 'Early mobility protocol compliance'
label(compliance$perf.e) <- 'Early mobility performed'
label(compliance$family.present.icu) <- 'Days family present in ICU'
label(compliance$comp.f) <- 'Family engagement protocol compliance'
label(compliance$elements.elig) <- 'Number of ABCDEF bundle elements eligible for compliance and performance'
label(compliance$elements.comp) <- 'Number of ABCDEF bundle elements which were compliant'
label(compliance$elements.perf) <- 'Number of ABCDEF bundle elements which were performed'
label(compliance$bundle.comp) <- 'Overall ABCDEF bundle compliance'
label(compliance$bundle.perf) <- 'Overall ABCDEF bundle performance'

## -- Calculate days of delirium, coma per compliance form -----------------------------------------
days.mental <- compliance %>%
  group_by(id) %>%
  summarise(days.delirium = ifelse(sum(!is.na(delirium.present.valid.f)) == 0, NA,
                                   sum(delirium.present.valid.f == 'Yes', na.rm = TRUE)),
            ever.delirious = factor(ifelse(is.na(days.delirium), 3,
                                           ifelse(days.delirium > 0, 2, 1)),
                                    levels = 1:3,
                                    labels = c('Never', '>=1 day', 'No assessments')),
            days.coma = ifelse(sum(!is.na(coma.f)) == 0, NA, sum(coma.f == 'Yes', na.rm = TRUE)),
            ever.comatose = factor(ifelse(is.na(days.coma), 3,
                                          ifelse(days.coma > 0, 2, 1)),
                                   levels = 1:3,
                                   labels = c('Never', '>=1 day', 'No assessments')),
            days.coma.ever = ifelse(ever.comatose != '>=1 day', NA, days.coma)) %>%
  mutate(days.delirium.ever = ifelse(ever.delirious == '>=1 day', days.delirium, NA),
         days.coma.ever = ifelse(ever.comatose == '>=1 day', days.coma, NA))

demog <- demog %>%left_join(days.mental, by = 'id')
label(demog$days.delirium) <- 'Days of delirium per PAD-recommended assessment'
label(demog$ever.delirious) <- 'Ever delirious per PAD-recommended assessment'
label(demog$days.delirium.ever) <- 'Days of delirium among those ever delirious'
label(demog$days.coma) <- 'Days of coma'
label(demog$ever.comatose) <- 'Ever comatose'
label(demog$days.coma.ever) <- 'Days of coma among those ever comatose'

## -- Create data sets for overall and region/type monthly compliance ------------------------------
compliance <- left_join(compliance,
                        demog[,c('id', 'month', 'data.month', 'data.month.short', 'hosp.type',
                                 'hosp.region')],
                        by = 'id')

## All sites - round quantiles so table will be narrower
comp.summary.overall <- compliance %>%
  group_by(data.month.short) %>%
  summarise(## Days compliance was tracked
    days.tracked = sum(comp.tracked),
    ## A: Assess, prevent and manage pain
    asmt.pain.median = round(median(n.painassess.icu, na.rm = TRUE)),
    asmt.pain.25 = round(quantile(n.painassess.icu, probs = 0.25, na.rm = TRUE)),
    asmt.pain.75 = round(quantile(n.painassess.icu, probs = 0.75, na.rm = TRUE)),
    days.comp.a = sum(comp.a, na.rm = TRUE),
    pct.comp.a = mean(comp.a, na.rm = TRUE),
    ## B: Both SAT and SBT
    days.sedatives = sum(on.sedative.icu, na.rm = TRUE),
    days.comp.b.sat = sum(comp.b.sat, na.rm = TRUE),
    pct.comp.b.sat = mean(comp.b.sat, na.rm = TRUE),
    days.perf.b.sat = sum(perf.b.sat, na.rm = TRUE),
    pct.perf.b.sat = mean(perf.b.sat, na.rm = TRUE),
    days.mv = sum(on.mv.icu, na.rm = TRUE),
    days.comp.b.sbt = sum(comp.b.sbt, na.rm = TRUE),
    pct.comp.b.sbt = mean(comp.b.sbt, na.rm = TRUE),
    days.perf.b.sbt = sum(perf.b.sbt, na.rm = TRUE),
    pct.perf.b.sbt = mean(perf.b.sbt, na.rm = TRUE),
    days.satsbt = sum(had.satsbt.icu, na.rm = TRUE),
    days.comp.b.satsbt = sum(comp.b.satsbt, na.rm = TRUE),
    pct.comp.b.satsbt = mean(comp.b.satsbt, na.rm = TRUE),
    ## C: Choice of analgesia and sedation
    days.comp.c = sum(comp.c, na.rm = TRUE),
    pct.comp.c = mean(comp.c, na.rm = TRUE),
    asmt.sed.median = round(median(sed.assess.icu, na.rm = TRUE)),
    asmt.sed.25 = round(quantile(sed.assess.icu, probs = 0.25, na.rm = TRUE)),
    asmt.sed.75 = round(quantile(sed.assess.icu, probs = 0.75, na.rm = TRUE)),
    ## D: Delirium - assess, prevent and manage
    days.comp.d = sum(comp.d, na.rm = TRUE),
    pct.comp.d = mean(comp.d, na.rm = TRUE),
    asmt.del.median = round(median(delirium.assess.icu, na.rm = TRUE)),
    asmt.del.25 = round(quantile(delirium.assess.icu, probs = 0.25, na.rm = TRUE)),
    asmt.del.75 = round(quantile(delirium.assess.icu, probs = 0.75, na.rm = TRUE)),
    ## E: Early mobility and exercise
    days.comp.e = sum(comp.e, na.rm = TRUE),
    pct.comp.e = mean(comp.e, na.rm = TRUE),
    days.perf.e = sum(perf.e, na.rm = TRUE),
    pct.perf.e = mean(perf.e, na.rm = TRUE),
    ## F: Family engagement and empowerment
    days.family.present = sum(family.present.icu, na.rm = TRUE),
    days.comp.f = sum(comp.f, na.rm = TRUE),
    pct.comp.f = mean(comp.f, na.rm = TRUE),
    ## Overall bundle compliance and performance
    days.comp.bundle = sum(bundle.comp, na.rm = TRUE),
    pct.comp.bundle = mean(bundle.comp, na.rm = TRUE),
    days.perf.bundle = sum(bundle.perf, na.rm = TRUE),
    pct.perf.bundle = mean(bundle.perf, na.rm = TRUE)) %>%
  filter(!is.na(data.month.short)) %>%
  mutate(hosp.type = 'All sites',
         line.alpha = 1)

## By region, type
comp.summary.regiontype <- compliance %>%
  group_by(data.month.short, hosp.region, hosp.type) %>%
  summarise(## Days compliance was tracked
    days.tracked = sum(comp.tracked),
    ## A: Assess, prevent and manage pain
    asmt.pain.median = round(median(n.painassess.icu, na.rm = TRUE)),
    asmt.pain.25 = round(quantile(n.painassess.icu, probs = 0.25, na.rm = TRUE)),
    asmt.pain.75 = round(quantile(n.painassess.icu, probs = 0.75, na.rm = TRUE)),
    days.comp.a = sum(comp.a, na.rm = TRUE),
    pct.comp.a = mean(comp.a, na.rm = TRUE),
    ## B: Both SAT and SBT
    days.sedatives = sum(on.sedative.icu, na.rm = TRUE),
    days.comp.b.sat = sum(comp.b.sat, na.rm = TRUE),
    pct.comp.b.sat = mean(comp.b.sat, na.rm = TRUE),
    days.perf.b.sat = sum(perf.b.sat, na.rm = TRUE),
    pct.perf.b.sat = mean(perf.b.sat, na.rm = TRUE),
    days.mv = sum(on.mv.icu, na.rm = TRUE),
    days.comp.b.sbt = sum(comp.b.sbt, na.rm = TRUE),
    pct.comp.b.sbt = mean(comp.b.sbt, na.rm = TRUE),
    days.perf.b.sbt = sum(perf.b.sbt, na.rm = TRUE),
    pct.perf.b.sbt = mean(perf.b.sbt, na.rm = TRUE),
    days.satsbt = sum(had.satsbt.icu, na.rm = TRUE),
    days.comp.b.satsbt = sum(comp.b.satsbt, na.rm = TRUE),
    pct.comp.b.satsbt = mean(comp.b.satsbt, na.rm = TRUE),
    ## C: Choice of analgesia and sedation
    days.comp.c = sum(comp.c, na.rm = TRUE),
    pct.comp.c = mean(comp.c, na.rm = TRUE),
    asmt.sed.median = round(median(sed.assess.icu, na.rm = TRUE)),
    asmt.sed.25 = round(quantile(sed.assess.icu, probs = 0.25, na.rm = TRUE)),
    asmt.sed.75 = round(quantile(sed.assess.icu, probs = 0.75, na.rm = TRUE)),
    ## D: Delirium - assess, prevent and manage
    days.comp.d = sum(comp.d, na.rm = TRUE),
    pct.comp.d = mean(comp.d, na.rm = TRUE),
    asmt.del.median = round(median(delirium.assess.icu, na.rm = TRUE)),
    asmt.del.25 = round(quantile(delirium.assess.icu, probs = 0.25, na.rm = TRUE)),
    asmt.del.75 = round(quantile(delirium.assess.icu, probs = 0.75, na.rm = TRUE)),
    ## E: Early mobility and exercise
    days.comp.e = sum(comp.e, na.rm = TRUE),
    pct.comp.e = mean(comp.e, na.rm = TRUE),
    days.perf.e = sum(perf.e, na.rm = TRUE),
    pct.perf.e = mean(perf.e, na.rm = TRUE),
    ## F: Family engagement and empowerment
    days.family.present = sum(family.present.icu, na.rm = TRUE),
    days.comp.f = sum(comp.f, na.rm = TRUE),
    pct.comp.f = mean(comp.f, na.rm = TRUE),
    ## Overall bundle compliance and performance
    days.comp.bundle = sum(bundle.comp, na.rm = TRUE),
    pct.comp.bundle = mean(bundle.comp, na.rm = TRUE),
    days.perf.bundle = sum(bundle.perf, na.rm = TRUE),
    pct.perf.bundle = mean(bundle.perf, na.rm = TRUE)) %>%
  filter(!is.na(data.month.short) & !is.na(hosp.region) & !is.na(hosp.type)) %>%
  mutate(line.alpha = 2)

## -- Create data sets for overall and by region/type hours of MV, ICU LOS -------------------------
mv.summary.overall <- demog %>%
  ## Here in case they want to include non-MV patients
  # mutate(hrs.invas.vent.all = ifelse(is.na(hrs.invas.vent) &
  #                                      !is.na(ever.invas.vent) &
  #                                      ever.invas.vent == 'No', 0,
  #                                    hrs.invas.vent)) %>%
  mutate(hrs.invas.vent.all = hrs.invas.vent) %>%
  group_by(data.month.short) %>%
  summarise(med.hrs.mv = median(hrs.invas.vent.all, na.rm = TRUE),
            q25.hrs.mv = quantile(hrs.invas.vent.all, probs = 0.25, na.rm = TRUE),
            q75.hrs.mv = quantile(hrs.invas.vent.all, probs = 0.75, na.rm = TRUE)) %>%
  mutate(hosp.type = 'All sites')

mv.summary.regiontype <- demog %>%
  ## Here in case they want to include non-MV patients
  # mutate(hrs.invas.vent.all = ifelse(is.na(hrs.invas.vent) &
  #                                      !is.na(ever.invas.vent) &
  #                                      ever.invas.vent == 'No', 0,
  #                                    hrs.invas.vent)) %>%
  mutate(hrs.invas.vent.all = hrs.invas.vent) %>%
  group_by(data.month.short, hosp.type, hosp.region) %>%
  summarise(med.hrs.mv = median(hrs.invas.vent.all, na.rm = TRUE),
            q25.hrs.mv = quantile(hrs.invas.vent.all, probs = 0.25, na.rm = TRUE),
            q75.hrs.mv = quantile(hrs.invas.vent.all, probs = 0.75, na.rm = TRUE))

iculos.summary.overall <- demog %>%
  group_by(data.month.short) %>%
  summarise(med.icu.los = median(icu.los, na.rm = TRUE),
            q25.icu.los = quantile(icu.los, probs = 0.25, na.rm = TRUE),
            q75.icu.los = quantile(icu.los, probs = 0.75, na.rm = TRUE)) %>%
  mutate(hosp.type = 'All sites')

iculos.summary.regiontype <- demog %>%
  group_by(data.month.short, hosp.type, hosp.region) %>%
  summarise(med.icu.los = median(icu.los, na.rm = TRUE),
            q25.icu.los = quantile(icu.los, probs = 0.25, na.rm = TRUE),
            q75.icu.los = quantile(icu.los, probs = 0.75, na.rm = TRUE))

## -- Create a report for each site ----------------------------------------------------------------
for(site in sort(unique(demog$hosp.f))){
  demog.site <- subset(demog, hosp.f == site & !is.na(data.time))
  n.each.time <- table(demog.site$data.time)

  ## Quantile example for explanatory text
  quantile.example <- quantile(subset(demog.site, data.time == 'Retrospective')$icu.los,
                               probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

  if(n.each.time[1] > 0 & n.each.time[2] > 0){
    compliance.site <- subset(compliance, hosp.f == site)

    ## Get compliance numbers for region/type that matches this hospital
    this.region <- subset(hosp.types.region, hosp.redcap == site)$hosp.region
    this.type <- subset(hosp.types.region, hosp.redcap == site)$hosp.type

    compliance.regiontype <- subset(comp.summary.regiontype,
                                    hosp.region == this.region & hosp.type == this.type)

    mv.regiontype <- subset(mv.summary.regiontype,
                            hosp.region == this.region & hosp.type == this.type)

    iculos.regiontype <- subset(iculos.summary.regiontype,
                                hosp.region == this.region & hosp.type == this.type)

    knit2pdf('icucollab_abcdef_bysite_v2.Rnw',
             output = paste0('icucollab_abcdef_v2_', gsub('[ -]+', '_', site), '.tex'))
  }
  cat('Finished ', site, '\n')
}
