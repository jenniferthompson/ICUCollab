## Load libraries
library(Hmisc)
library(dplyr)
library(tidyr)
library(devtools)

## Wrapper function for latex-ing summaryM objects
my.print.summaryM <- function(...){
  latex.summaryM(file = '', where = '!h', digits = 2, prmsd = TRUE, long = TRUE,
                 npct = 'both', what = '%', ...)
}

## Function to round number to and print same number of digits
rndformat <- function(x, ndigits = 2){
  format(round(x, digits = ndigits), nsmall = ndigits)
}

## Function to format p-values
formatp <- function(p){
  ifelse(p < 0.0001, '<0.0001',
         ifelse(p < 0.001, '<0.001',
                rndformat(p, 3)))
}

## multiplot() function for multiple ggplot objects on one page (from Winston Chang's Cookbook for
##  R: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## -- Function to calculate & plot proportions of patient-days by time and, if needed, -------------
## -- faceting variable ----------------------------------------------------------------------------
ggplot.prop <-
  function(var,                                             ## X variable to plot
           facet.var = NULL,                                ## variable to facet by
           facet.col = 1,                                   ## number of columns for faceting
           use.na = c('no', 'ifany', 'always'),             ## include missing values in facets
           dataset = subset(compliance, icu.24.f == 'Yes'), ## data set to use
           use.ylab = 'Percent of ICU Days',                ## Y axis label
           use.xlab = NULL,                                 ## X axis label
           x.limits = NULL,                                 ## X axis limits
           x.breaks = NULL,                                 ## breaks to use for X axis
           x.angle = 0, x.hjust = 0.5,                      ## X axis text formatting
           axis.text.size = 7,                              ## axis text size
           axis.title.size = 8,                             ## axis title size
           time.colors = c('#9ecae1', '#08306b'),           ## colors for bars
           use.legend = TRUE,                               ## include legend?
           legend.text.size = 7,                            ## legend text size
           legend.key.cm = 0.4,                             ## legend key size in cm
           strip.text.size = 8,                             ## strip text size
           title.string = var,                              ## plot title; defaults to X var name
           title.size = 9,                                  ## font size for main title
           title.hjust = 0){                                ## justification for plot title

    use.na <- match.arg(use.na)

    ## Get number in each cell
    if(!is.null(facet.var)){
      crosstab.data <- eval(parse(text = paste0("with(dataset, as.data.frame(table(", facet.var, ", ",
                                                var, ", data.time, useNA = '", use.na, "')))")))
      names(crosstab.data) <- gsub(facet.var, 'facetvar', names(crosstab.data))
    } else{
      crosstab.data <- eval(parse(text = paste0("with(dataset, as.data.frame(table(", var,
                                                ", data.time, useNA = '", use.na, "')))")))
      crosstab.data$facetvar <- 'All'
    }
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

## Spreadsheet from Vishakha showing which hospitals belong in which region
hosp.region <- read.csv('RawData/hospital_regions.csv', stringsAsFactors = FALSE) %>%
  filter(region != '') %>%
  separate(hospital, ', ', into = c('hosp.num', 'hosp.f')) %>%
  mutate(region = factor(region))
levels(hosp.region$region) <- c('East Coast', 'Midwest', 'West Coast')

## -- Remove any test patients ---------------------------------------------------------------------
## Get list of IDs which don't meet required format to send Vishakha
reformat.ids <-
  demog$id[grep('^[A-Z][A-Z][A-Z][0-9][0-9][0-9][0-9]$', toupper(demog$id), invert = TRUE)]

## Remove any patient with "test" in the ID
demog <- demog[grep('test', tolower(demog$id), invert = TRUE),]
compliance <- compliance[grep('test', tolower(compliance$id), invert = TRUE),]

## -- Data management for demographic form ---------------------------------------------------------
## Create indicator for retrospective (mos 1-6) vs. prospective (mos 7-) data collection
demog$data.time <- with(demog, {
  factor(ifelse(month <= 6, 1, 2), levels = 1:2, labels = c('Baseline', 'Prospective')) })

## Merge on hospital region
demog <- merge(demog, subset(hosp.region, select = c(region, hosp.num)),
               by.x = c('hosp'), by.y = c('hosp.num'), all.x = TRUE, all.y = FALSE)

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

## Total time on invasive, noninvasive MV
demog$days.invas.vent <- with(demog, {
  ifelse(is.na(invas.vent.episode) | invas.vent.episode == 0, NA,
         rowSums(demog[,paste0('invas.vent.length.', 1:6)], na.rm = TRUE)) })

demog$days.noninvas.vent <- with(demog, {
  ifelse(is.na(noninvas.vent.episode) | noninvas.vent.episode == 0, NA,
         rowSums(demog[,paste0('invas.vent.length.', 1:6)], na.rm = TRUE)) })

label(demog$data.time) <- 'Time of data collection'
label(demog$data.plan.f) <- 'Data plan'
label(demog$region) <- 'Hospital region'
label(demog$month.f) <- 'Month in Collaborative ICU'
label(demog$age.f) <- 'Age range'
label(demog$sex.f) <- 'Sex'
label(demog$race.f) <- 'Race'
label(demog$hispanic.f) <- 'Hispanic'
label(demog$english.f) <- 'English-speaking'
label(demog$invas.vent.episode) <- 'Episodes of invasive MV'
label(demog$days.invas.vent) <- 'Total days on invasive MV'
label(demog$noninvas.vent.episode) <- 'Episodes of noninvasive MV'
label(demog$days.noninvas.vent) <- 'Total days on noninvasive MV'
label(demog$icu.los) <- 'ICU length of stay'
label(demog$hosp.losv) <- 'Hospital length of stay'
label(demog$dc.status.f) <- 'Discharge/mortality status'

## -- Data management for compliance form ----------------------------------------------------------
## -- Preliminary variables ------------------------------------------------------------------------
## Total number of pain assessments, any type
compliance$pain.verbal.valid <-
  ifelse(rowSums(!is.na(compliance[,paste0('pain.', c('verbal', 'valid'))])) == 0, NA,
         rowSums(compliance[,paste0('pain.', c('verbal', 'valid'))], na.rm = TRUE))

## Create variable for whether patient received continuous and/or intermittent sedation
compliance$on.sedation <- rowSums(compliance[,paste0('sedative.', 1:2)]) > 0

## Indicator for whether no sedation information marked
compliance$no.sedation.checked <- rowSums(compliance[,paste0('sedative.', 0:3)]) == 0

## Indicator for whether none + some other sedation amount marked
compliance$none.some.sedation <-
  compliance$sedative.0 & rowSums(compliance[,paste0('sedative.', 0:3)]) > 1

## Total number of sedation assessments, any type
compliance$sed.assess.either <-
  ifelse(rowSums(!is.na(compliance[,paste0('sed.assess.', c('valid', 'other'))])) == 0, NA,
         rowSums(compliance[,paste0('sed.assess.', c('valid', 'other'))], na.rm = TRUE))

## Total number of delirium assessments, any type
compliance$delirium.assess.either <-
  ifelse(rowSums(!is.na(compliance[,paste0('delirium.assess.', c('valid', 'other'))])) == 0, NA,
         rowSums(compliance[,paste0('delirium.assess.', c('valid', 'other'))], na.rm = TRUE))

## Indicator for whether no family participation information marked
compliance$no.family.checked <- rowSums(compliance[,paste0('familyinvite.', 1:5)]) == 0

## Do answers to family question make sense?
compliance$family.disagree <- with(compliance, {
  (familyinvite.5 == 1 & rowSums(compliance[,paste0('familyinvite.', 1:5)]) > 1) |
    (familyinvite.1 == 1 & (familyinvite.2 == 1 | familyinvite.3 == 1 | familyinvite.4 == 1)) |
    (familyinvite.4 == 1 & (familyinvite.2 == 1 | familyinvite.3 == 1)) })

## -- Create indicators for compliance on each component -------------------------------------------
## -- Only present for ICU days, except for SAT/SBT components -------------------------------------

## Level labels for all compliance variables
compliance.levels <- c('Compliant', 'Noncompliant', 'Missing data')

compliance <- compliance %>%
  mutate(## A: Assess, prevent, and manage pain
    ##  Include all ICU days; compliance: >=6 self-reported or behavioral assessments
    a.comp.verbal = factor(ifelse(is.na(icu.24.f) | icu.24.f == 'No', NA,
                           ifelse(is.na(pain.verbal), 2,
                           ifelse(pain.verbal >= 6, 0, 1))),
                           levels = 0:2, labels = compliance.levels),
    a.comp.valid = factor(ifelse(is.na(icu.24.f) | icu.24.f == 'No', NA,
                          ifelse(is.na(pain.valid), 2,
                          ifelse(pain.valid >= 6, 0, 1))),
                          levels = 0:2, labels = compliance.levels),
    a.comp.other = factor(ifelse(is.na(icu.24.f) | icu.24.f == 'No', NA,
                          ifelse(is.na(pain.other), 2,
                          ifelse(pain.other >= 6, 0, 1))),
                          levels = 0:2, labels = compliance.levels),
    a.comp.overall = factor(ifelse(is.na(icu.24.f) | icu.24.f == 'No', NA,
                            ifelse(is.na(pain.verbal.valid), 2,
                            ifelse(pain.verbal.valid >= 6, 0, 1))),
                            levels = 0:2, labels = compliance.levels),
    ## B: Both SAT and SBT
    ##  SAT: Include days on continuous or intermittent sedation;
    ##       compliance = SAT performed or safety screen failure
    ##  SBT: Include days on MV; compliance = SBT performed or safety screen failure
    ## Overall: for days with both sedation and MV, were both SAT and SBT attempted?
    b.comp.sat = factor(ifelse(is.na(icu.24.f) | icu.24.f == 'No' | !on.sedation, NA,
                        ifelse(is.na(satperformed.f), 2,
                        ifelse(satperformed.f == 'No. Other reason/ Not documented', 1, 0))),
                        levels = 0:2, labels = compliance.levels),
    b.comp.sbt = factor(ifelse(is.na(icu.24.f) | icu.24.f == 'No' |
                                 is.na(venttoday.f) | venttoday.f == 'No', NA,
                        ifelse(is.na(sbtperformed.f), 2,
                        ifelse(sbtperformed.f == 'No. Other reason/ Not documented', 1, 0))),
                        levels = 0:2, labels = compliance.levels),
    b.comp.overall = factor(ifelse(is.na(icu.24.f) | icu.24.f == 'No' | !on.sedation |
                                     is.na(venttoday.f) | venttoday.f == 'No', NA,
                            ifelse(is.na(satperformed.f) | is.na(sbtperformed.f), 2,
                            ifelse(satperformed.f == 'No. Other reason/ Not documented' |
                                     sbtperformed.f == 'No. Other reason/ Not documented',
                                   1, 0))),
                            levels = 0:2, labels = compliance.levels),
    ## C: Choice of sedation
    c.comp.target = factor(ifelse(is.na(icu.24.f) | icu.24.f == 'No', NA,
                           ifelse(is.na(sed.target.f), 2,
                           ifelse(sed.target.f == 'Yes', 0, 1))),
                           levels = 0:2, labels = compliance.levels),
    c.comp.valid = factor(ifelse(is.na(icu.24.f) | icu.24.f == 'No', NA,
                          ifelse(is.na(sed.assess.valid), 2,
                          ifelse(sed.assess.valid >= 6, 0, 1))),
                          levels = 0:2, labels = compliance.levels),
    c.comp.other = factor(ifelse(is.na(icu.24.f) | icu.24.f == 'No', NA,
                          ifelse(is.na(sed.assess.other), 2,
                          ifelse(sed.assess.other >= 6, 0, 1))),
                          levels = 0:2, labels = compliance.levels),
    c.comp.overall = factor(ifelse(is.na(icu.24.f) | icu.24.f == 'No', NA,
                            ifelse(is.na(sed.assess.either), 2,
                            ifelse(sed.assess.either >= 6, 0, 1))),
                            levels = 0:2, labels = compliance.levels),
    ## D: Delirium: Assess, Prevent and Manage
    d.comp.valid = factor(ifelse(is.na(icu.24.f) | icu.24.f == 'No', NA,
                          ifelse(is.na(delirium.assess.valid), 2,
                          ifelse(delirium.assess.valid >= 2, 0, 1))),
                          levels = 0:2, labels = compliance.levels),
    d.comp.other = factor(ifelse(is.na(icu.24.f) | icu.24.f == 'No', NA,
                          ifelse(is.na(delirium.assess.other), 2,
                          ifelse(delirium.assess.other >= 2, 0, 1))),
                          levels = 0:2, labels = compliance.levels),
    d.comp.overall = factor(ifelse(is.na(icu.24.f) | icu.24.f == 'No', NA,
                            ifelse(is.na(delirium.assess.either), 2,
                            ifelse(delirium.assess.either >= 2, 0, 1))),
                            levels = 0:2, labels = compliance.levels),
    ## E: Early mobility and exercise
    e.comp.overall = factor(ifelse(is.na(icu.24.f) | icu.24.f == 'No', NA,
                            ifelse(is.na(mobilityperformed.f), 2,
                            ifelse(mobilityperformed.f == 'Not performed /not documented', 1, 0))),
                            levels = 0:2, labels = compliance.levels),
    ## F: Famly engagement and empowerment
    f.comp.overall = factor(ifelse(is.na(icu.24.f) | icu.24.f == 'No', NA,
                            ifelse(no.family.checked, 2,
                            ifelse(familyinvite.2 == 1 | familyinvite.3 == 1, 0, 1))),
                            levels = 0:2, labels = compliance.levels)) %>%
  ## Merge on time of data collection and hospital region
  left_join(dplyr::select(demog, id, data.time, hosp.f, region), by = 'id')

levels(compliance$meds.1.f) <- levels(compliance$meds.2.f) <- levels(compliance$meds.3.f) <-
  levels(compliance$meds.4.f) <- levels(compliance$meds.5.f) <- levels(compliance$meds.6.f) <-
  levels(compliance$meds.7.f) <- c('No', 'Yes')

label(compliance$a.comp.verbal) <- 'A: >=6 documented self-reported pain assessments'
label(compliance$a.comp.valid) <- 'A: >=6 documented behavioral pain assessments'
label(compliance$a.comp.other) <- 'A: >=6 documented other pain assessments'
label(compliance$a.comp.overall) <- 'A, overall: >=6 pain assessments, behavioral + self-report'
label(compliance$b.comp.sat) <- 'B: SAT: patient on sedation and had SAT or attempt'
label(compliance$b.comp.sbt) <- 'B: SBT: patient on MV and had SBT or attempt'
label(compliance$b.comp.overall) <- 'B, overall: patient on sedation+MV, had SAT+SBT or attempt'
label(compliance$c.comp.target) <- 'C: documented sedation target'
label(compliance$c.comp.valid) <- 'C: >=6 PAD-recommended arousal assessments'
label(compliance$c.comp.other) <- 'C: >=6 other arousal assessments'
label(compliance$c.comp.overall) <- 'C, overall: >=6 total arousal assessments'
label(compliance$meds.1) <- 'Received opioid'
label(compliance$meds.2) <- 'Received benzodiazepine'
label(compliance$meds.3) <- 'Received propofol'
label(compliance$meds.4) <- 'Received dexmedetomidine'
label(compliance$meds.5) <- 'Received ketamine'
label(compliance$meds.6) <- 'Received typical antipsychotic'
label(compliance$meds.7) <- 'Received atypical antipsychotic'
label(compliance$d.comp.valid) <- 'D: >=2 PAD-recommended delirium assessments'
label(compliance$d.comp.other) <- 'D: >=2 other delirium assessments'
label(compliance$d.comp.overall) <- 'D, overall: >=2 total delirium assessments'
label(compliance$e.comp.overall) <- 'E, overall: mobility performed or attempt'
label(compliance$mobilityhighest.f) <- 'E: Highest level of exercise/mobilization today'
label(compliance$f.comp.overall) <- 'F, overall: family participated in rounds and/or conference'

## -- Summary variables for daily data -------------------------------------------------------------
## Function for calculating mean of a given variable, setting to NA if no non-missing values
calc.mean.na <- function(var){ ifelse(sum(!is.na(var)) == 0, NA, sum(var, na.rm = TRUE)) }

comp.summary.icu <- compliance %>%
  filter(icu.24.f == 'Yes') %>%
  group_by(id) %>%
  summarise(mean.pain.verbal = calc.mean.na(pain.verbal),
            mean.pain.valid = calc.mean.na(pain.valid),
            mean.pain.other = calc.mean.na(pain.other),
            mean.pain.verbal.valid = calc.mean.na(pain.verbal.valid),
            mean.sed.assess.valid = calc.mean.na(sed.assess.valid),
            mean.sed.assess.other = calc.mean.na(sed.assess.other),
            mean.sed.assess.either = calc.mean.na(sed.assess.either),
            mean.del.assess.valid = calc.mean.na(delirium.assess.valid),
            mean.del.assess.other = calc.mean.na(delirium.assess.other),
            mean.del.assess.either = calc.mean.na(delirium.assess.either)) %>%
  ungroup()

demog <- demog %>%
  left_join(comp.summary.icu, by = 'id')

label(demog$mean.pain.verbal) <- 'Mean self-report pain asmts'
label(demog$mean.pain.valid) <- 'Mean behavioral pain asmts'
label(demog$mean.pain.other) <- 'Mean other pain asmts'
label(demog$mean.pain.verbal.valid) <- 'Mean behavioral + self-report'
label(demog$mean.sed.assess.valid) <- 'Mean PAD-rec. arousal asmts'
label(demog$mean.sed.assess.other) <- 'Mean other arousal asmts'
label(demog$mean.sed.assess.either) <- 'Mean total arousal asmts'
label(demog$mean.del.assess.valid) <- 'Mean PAD-rec. delirium asmts'
label(demog$mean.del.assess.other) <- 'Mean other delirium asmts'
label(demog$mean.del.assess.either) <- 'Mean total delirium asmts'

for(site in sort(unique(hosp.region$hosp.f))){
  demog.site <- subset(demog, hosp.f == site & !is.na(data.time))
  n.each.time <- table(demog.site$data.time)
  if(n.each.time[1] > 5 & n.each.time[2] > 5){
    compliance.site <- subset(compliance, hosp.f == site)
    knit2pdf('icucollab_abcdef_bysite.Rnw',
             output = paste0('icucollab_abcdef_', gsub('[ -]+', '_', site), '.tex'))
  }
  cat('Finished ', site, '\n')
}
