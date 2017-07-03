## Load libraries
library(knitr)
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

## Color palette based on pediatric logo
peds.colors = c("A" = "#887C90",
                "B" = "#F9F68A",
                "C" = "#C8DC9F",
                "D" = "#9BCBBB",
                "E" = "#5C82A0",
                "F" = "#565978")

## Load full Rdata file
load('RawData/pediatric.Rdata')

## Ampersands make Latex do crazy things
demog$hosp.f.latex <- demog$hosp.f
levels(demog$hosp.f.latex) <- gsub('&', 'and', levels(demog$hosp.f), fixed = TRUE)

## Merge site, month onto compliance data, keeping only patients in months 1-8
compliance <- right_join(compliance,
                         dplyr::select(demog, id, hosp.f, hosp.f.latex, month.cat),
                         by = 'id')

## Restrict both data sets to baseline and implementation months 1-8;
## not enough data entered after month 8 to draw reliable conclusions
demog <- demog %>%
  filter(month.cat %in% c("Baseline", paste("Imp.", 1:8)))

## Keep only baseline + implementation months 1-8
all.months <- c("Baseline", paste("Imp.", 1:8))

## Short site labels to use for naming files
site.names <- c("Johns Hopkins Childrens Center" = "JohnsHopkins",
                "Le Bonheur Childrens Hospital" = "LeBonheur",
                "Mayo Eugenio Litta Childrens Hospital" = "Mayo",
                "Seattle Childrens Hospital" = "Seattle",
                "St. Louis Childrens Hospital" = "StLouis",
                "UC Davis Childrens Hospital" = "UCDavis",
                "Monroe Carell Jr. Childrens Hospital at Vanderbilt" = "VCH",
                "Weill Cornell Pediatrics" = "WeillCornell")

## -- Preparation for visualizing descriptives ---------------------------------
## How many patients at each site in baseline, implementation time points?
time_totals <- demog %>%
  group_by(hosp.f.latex, data.time) %>%
  summarise(n_total = n())

## Categorical variables: Show proportion in each category
demog_categoricals <- demog %>%
  dplyr::select(id, hosp.f.latex, data.time, age.f, sex.f, race.combined,
                severity.combined, ever.vent, mortality.f) %>%
  gather(key = char_type, value = char_value, age.f:mortality.f) %>%
  group_by(hosp.f.latex, data.time, char_type, char_value) %>%
  summarise(n_value = n()) %>%
  ungroup() %>%
  left_join(time_totals, by = c("hosp.f.latex", "data.time")) %>%
  mutate(prop_value = (n_value / n_total)*100,
         data.time = factor(ifelse(data.time == "Implementation", 1, 2),
                            levels = 1:2,
                            labels = c("Implementation", "Baseline")),
         char_f = factor(ifelse(char_type == "age.f", 1,
                         ifelse(char_type == "sex.f", 2,
                         ifelse(char_type == "race.combined", 3,
                         ifelse(char_type == "english.f", 4,
                         ifelse(char_type == "severity.combined", 5,
                         ifelse(char_type == "ever.vent", 6,
                         ifelse(char_type == "mortality.f", 7,
                                char_type))))))),
                         levels = 1:7,
                         labels = c("Age", "Sex", "Race", "Language",
                                    "Severity Scale", "Ever on MV",
                                    "Mortality")),
         ## Manually create factor variables to get categories in right order
         category = factor(ifelse(char_value == "0 day - 27 days", 7,
                           ifelse(char_value == "28 days - 12 months", 6,
                           ifelse(char_value == "13 months to 2 years", 5,
                           ifelse(char_value == "2 years to 5 years", 4,
                           ifelse(char_value == "6 years to 11 years", 3,
                           ifelse(char_value == "12 years to 18 years", 2,
                           ifelse(char_value == "18 years to 21 years", 1,
                           ifelse(char_value == "English Speaking", 9,
                           ifelse(char_value == "Non-English Speaking", 8,
                           ifelse(char_value == "American Indian/Alaskan Native",
                                  17,
                           ifelse(char_value == "Black/African American", 16,
                           ifelse(char_value == "White", 15,
                           ifelse(char_value == "Asian", 14,
                           ifelse(char_value == "Native Hawaiian/Pacific Islander",
                                  13,
                           ifelse(char_value == "Other/not specified", 12,
                           ifelse(char_value == "Multiple races specified", 11,
                           ifelse(char_value == "Unknown (no data available)",
                                  10,
                           ifelse(char_value == "PIM 2 only", 25,
                           ifelse(char_value == "PIM 2 + other scale", 24,
                           ifelse(char_value == "PRISM III + PIM 2", 23,
                           ifelse(char_value == "PRISM III + other scale", 22,
                           ifelse(char_value == "PRISM III only", 21,
                           ifelse(char_value == "Other scale", 20,
                           ifelse(char_value == "No severity scale used", 19,
                           ifelse(char_value == "No or conflicting severity scale information",
                                  18,
                           ifelse(char_value == "Male", 27,
                           ifelse(char_value == "Female", 26,
                           ifelse(char_value == ">=1 day on MV", 30,
                           ifelse(char_value == "Never on MV", 29,
                           ifelse(char_value == "Unknown (no data available)",
                                  28,
                           ifelse(char_value == "Discharged alive", 32,
                           ifelse(char_value == "Died in hospital", 31,
                                  NA)))))))))))))))))))))))))))))))),
                           levels = 1:32,
                           labels = c("18 years to 21 years",
                                      "12 years to 18 years",
                                      "6 years to 11 years",
                                      "2 years to 5 years",
                                      "13 months to 2 years",
                                      "28 days - 12 months",
                                      "0 day - 27 days",
                                      "Non-English Speaking",
                                      "English Speaking",
                                      "Unknown (no data available)",
                                      "Multiple races specified",
                                      "Other/not specified",
                                      "Native Hawaiian/Pacific Islander",
                                      "Asian",
                                      "White",
                                      "Black/African American",
                                      "American Indian/Alaskan Native",
                                      "No or conflicting information",
                                      "No severity scale used",
                                      "Other scale",
                                      "PRISM III only",
                                      "PRISM III + other scale",
                                      "PRISM III + PIM 2",
                                      "PIM 2 + other scale",
                                      "PIM 2 only",
                                      "Female",
                                      "Male",
                                      "Unknown (no data)",
                                      "Never on MV",
                                      ">=1 day on MV",
                                      "Died in hospital",
                                      "Discharged alive"))
  ) %>%
  filter(!is.na(data.time) & !is.na(char_value))

demog_admitdx <- demog %>%
  filter(!is.na(data.time)) %>%
  group_by(hosp.f.latex, data.time) %>%
  summarise_at(.funs = "mean",
               .cols = paste0("dx.",
                              c("surgery", "infection", "resp", "gi", "neuro",
                                "tbi", "burn", "malig", "other"))) %>%
  gather(key = diagnosis, value = prop_value, dx.surgery:dx.other) %>%
  mutate(data.time = factor(ifelse(data.time == "Implementation", 1, 2),
                            levels = 1:2,
                            labels = c("Implementation", "Baseline")),
         prop_value = prop_value * 100,
         facet.text = "Admission Diagnoses",
         diagnosis.f = factor(ifelse(diagnosis == "dx.surgery", 9,
                              ifelse(diagnosis == "dx.infection", 8,
                              ifelse(diagnosis == "dx.resp", 7,
                              ifelse(diagnosis == "dx.gi", 6,
                              ifelse(diagnosis == "dx.neuro", 5,
                              ifelse(diagnosis == "dx.tbi", 4,
                              ifelse(diagnosis == "dx.burn", 3,
                              ifelse(diagnosis == "dx.malig", 2,
                              ifelse(diagnosis == "dx.other", 1, NA))))))))),
                              levels = 1:9,
                              labels = c("Other",
                                         "Malignancy",
                                         "Burn",
                                         "TBI",
                                         "Neurologic\n(non-trauma)",
                                         "GI bleed/\nhemorrhagic\nshock",
                                         "Respiratory\nfailure (any)",
                                         "Sepsis/\ninfection",
                                         "Surgery\n(any)")))

demog_continuous <- demog %>%
  dplyr::select(id, hosp.f.latex, data.time, prism3, pim2, fss.preadm, fss.adm,
                fss.dc, popc.preadm, popc.adm, popc.dc, piculos, hosplos,
                ventdays) %>%
  gather(key = cont_type, value = cont_value, prism3:ventdays) %>%
  filter(!is.na(data.time) & !is.na(cont_value)) %>%
  separate(cont_type, into = c("cont_type", "time"), sep = "\\.") %>%
  mutate(cont_f = factor(ifelse(cont_type == "prism3", 1,
                         ifelse(cont_type == "pim2", 2,
                         ifelse(cont_type == "fss", 3,
                         ifelse(cont_type == "popc", 4,
                         ifelse(cont_type == "piculos", 5,
                         ifelse(cont_type == "hosplos", 6,
                         ifelse(cont_type == "ventdays", 7, NA))))))),
                         levels = 1:7,
                         labels = c("PRISM III", "PIM 2", "FSS", "POPC",
                                    "PICU length of stay",
                                    "Hospital length of stay", "Days on MV")),
         time_f = factor(ifelse(is.na(time), NA,
                         ifelse(time == "preadm", 1,
                         ifelse(time == "adm", 2, 3))),
                         levels = 1:3,
                         labels = c("Preadmission", "Admission", "Discharge")))

## -- Function to plot either barchart or boxplot for a single variable --------
plot_demog_var <- function(vname){
  plot_cols = c("Baseline" = as.character(peds.colors["D"]),
                "Implementation" = as.character(peds.colors["A"]))

  ## Indicators for whether plot needs to be faceted by preadm/adm/dc, and/or
  ## variable is categorical
  if(vname %in% names(demog.site)){
    is_popc_fss <- FALSE
    is_cat <- inherits(demog[,vname], "factor")
  } else{
    is_popc_fss <- vname %in% c("popc", "fss")
    is_cat <- vname == "diagnosis.f"
  }

  ## Legend placement: Don't include for severity scales, FSS, POPC, MV, LOS
  if(vname %in% c("pim2", "prism3", "severity.combined", "fss", "popc",
                  "ever.vent", "ventdays", "piculos", "hosplos",
                  "mortality.f")){
    legendp <- "none"
  } else{
    legendp <- "bottom"
  }

  ## Barcharts for admission diagnosis (demog_admitdx) and other categoricals
  ## (demog_categoricals)
  if(is_cat){
    if(vname == "diagnosis.f"){
      xvar <- "diagnosis.f"
      df <- demog_admitdx.site
      ptitle <- "Admission Diagnosis"
    } else{
      xvar <- "category"
      df <- filter(demog_categoricals.site, char_type == vname)
      ptitle <- unique(df$char_f)
    }

    ## Initialize barchart
    p <- ggplot(data = df, aes_string(x = xvar, y = "prop_value")) +
      geom_bar(aes(fill = data.time),
               position = "dodge", stat = "identity", alpha = 0.85) +
      scale_x_discrete(name = "") +
      scale_y_continuous(name = "Percent of Patients",
                         limits = c(0, 100), breaks = seq(0, 100, 25)) +
      coord_flip()

    ## Boxplots for continuous variables (demog_continuous),
    ##  faceting by time if POPC, FSS
  } else{
    df <- demog_continuous.site %>% filter(cont_type == vname)
    ptitle <- unique(df$cont_f)

    ## Initialize boxplots
    p <- ggplot(data = df, aes(x = data.time, y = cont_value)) +
      geom_boxplot(aes(colour = data.time, fill = data.time),
                   alpha = 0.75, outlier.size = 0.75) +
      scale_x_discrete(name = NULL) +
      scale_y_continuous(name = NULL)

    ## Facet if POPC/FSS
    if(is_popc_fss){
      p <- p +
        facet_wrap(~ time_f)
    }

  }

  ## Add scales, titles, legend, other tweaks; same regardless of variable type
  p +
    scale_fill_manual(values = plot_cols) +
    scale_colour_manual(values = plot_cols) +
    labs(title = ptitle) +
    theme_minimal() +
    theme(legend.position = legendp,
          legend.direction = 'horizontal',
          legend.title = element_blank(),
          legend.text = element_text(size = 6),
          legend.key.size = unit(0.3, "cm"),
          plot.title = element_text(face = "bold"),
          axis.text = element_text(size = 7),
          axis.title = element_text(size = 9),
          panel.border = element_rect(fill = NA, colour = 'grey80'))

}

## These variables will ideally be included in all sites' descriptive tables
base_desc_formula <- "age.f + sex.f + race.combined + english.f + severity.combined + prism3 + pim2 + fss.preadm + fss.adm + popc.preadm + popc.adm ~ data.time"

## -- Create a report for each site ----------------------------------------------------------------
for(site in sort(unique(demog$hosp.f.latex))){
  demog.site <- subset(demog, hosp.f.latex == site & !is.na(data.time))
  demog_categoricals.site <- subset(demog_categoricals, hosp.f.latex == site)
  demog_admitdx.site <- subset(demog_admitdx, hosp.f.latex == site)
  demog_continuous.site <- subset(demog_continuous, hosp.f.latex == site)

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
