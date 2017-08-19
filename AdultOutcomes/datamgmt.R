################################################################################
## Data management for outcomes analysis
################################################################################

## Library calls
library(tidyverse)

## -- Read in raw data downloaded from REDCap, create and rename factors -------
source("RawData/demog_redcap.R")
names(demog) <- gsub("_+", "_", gsub("\\.factor$", "_f", names(demog)))

source("RawData/compliance_redcap.R")
names(compliance) <-
  gsub("_+", "_", gsub("\\.factor$", "_f", names(compliance)))
