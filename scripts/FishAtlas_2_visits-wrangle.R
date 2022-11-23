# -----------------------------------------------------------------------
# Title: Wrangle of NOAA's Nearshore Fish Atlas data
# Creator: Chris Guo
# Date: 2022.11.14
# Purpose: Re-format events_qc data into a QC'ed dataframe at the Site Visit level

# Notes -------------------------------------------------------------------

## Shorezone/FishAtlas web link: https://alaskafisheries.noaa.gov/mapping/sz/

# Load packages -----------------------------------------------------------

library(tidyverse)

# Define workflow paths ---------------------------------------------------

wd = getwd()
dir.output = file.path(wd, "output")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd,"data")
dir.scripts = file.path(wd,"scripts")

# Read in data ------------------------------------------------------------

# source(file = file.path(dir.scripts, "FishAtlas_1_events-wrangle.R"))


# Some EDA to begin with --------------------------------------------------

# These are all the categories that have finer resolution than VisitID,
n_distinct(events_qc$VisitID)
events_qc %>% select(VisitID, EventID) %>% n_distinct()
events_qc %>% select(VisitID, Region) %>% n_distinct()
events_qc %>% select(VisitID, Location) %>% n_distinct()
events_qc %>% select(VisitID, Habitat) %>% n_distinct()
events_qc %>% select(VisitID, TidalStage) %>% n_distinct()
events_qc %>% select(VisitID, Temp_C) %>% n_distinct()
events_qc %>% select(VisitID, Salinity) %>% n_distinct()
events_qc %>% select(VisitID, GearSpecific) %>% n_distinct()
events_qc %>% select(VisitID, ProjectName) %>% n_distinct()

# So let's nest these and deal with them one by one later,
visits.1 = events_qc %>%
  group_by(VisitID) %>%
  nest(EventID_n = EventID,
       Region_n = Region,
       Location_n = Location,
       Habitat_n = Habitat,
       TidalStage_n = TidalStage,
       Temp_C_n = Temp_C,
       Salinity_n = Salinity,
       GearSpecific_n = GearSpecific,
       ProjectName_n = ProjectName)









