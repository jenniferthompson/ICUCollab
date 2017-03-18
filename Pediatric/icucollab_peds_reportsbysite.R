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

## Load full Rdata file
load('RawData/pediatric.Rdata')

## Ampersands make Latex do crazy things
demog$hosp.f.latex <- demog$hosp.f
levels(demog$hosp.f.latex) <- gsub('&', 'and', levels(demog$hosp.f), fixed = TRUE)

## Merge site, month onto compliance data
compliance <- left_join(compliance,
                        dplyr::select(demog, id, hosp.f, hosp.f.latex, month.cat),
                        by = 'id')

all.months <- levels(demog$month.cat)

## Short site labels to use for naming files
site.names <- c("Johns Hopkins Childrens Center" = "JohnsHopkins",
                "Le Bonheur Childrens Hospital" = "LeBonheur",
                "Mayo Eugenio Litta Childrens Hospital" = "Mayo",
                "Seattle Childrens Hospital" = "Seattle",
                "St. Louis Childrens Hospital" = "StLouis",
                "UC Davis Childrens Hospital" = "UCDavis",
                "Monroe Carell Jr. Childrens Hospital at Vanderbilt" = "VCH",
                "Weill Cornell Pediatrics" = "WeillCornell")

## -- Create a report for each site ----------------------------------------------------------------
for(site in sort(unique(demog$hosp.f.latex))){
  demog.site <- subset(demog, hosp.f.latex == site & !is.na(data.time))
  n.each.time <- table(demog.site$data.time)

  ## Quantile example for explanatory text
  quantile.example <- quantile(subset(demog.site, data.time == 'Baseline')$piculos,
                               probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

  if(n.each.time[1] > 0 & n.each.time[2] > 0){
    compliance.site <- subset(compliance, hosp.f.latex == site)

    knit2pdf('icucollab_peds_singlesite.Rnw',
             output = paste0('icucollab_peds_', site.names[[site]], '.tex'))
  }
  cat('Finished ', site, '\n')
}
