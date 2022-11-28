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

# So let's nest these and deal with them one by one,
visits.1 = events_qc %>%
  group_by(VisitID) %>%
  nest(EventIDs = EventID,
       Regions = Region,
       Locations = Location,
       Habitats = Habitat,
       TidalStages = TidalStage,
       Temps= Temp_C,
       Sals = Salinity,
       GearSpecifics = GearSpecific,
       ProjectNames = ProjectName) %>%
  relocate(PointOfContact, .after = last_col())

# First we'll create a col to let us know how many seines were included per Visit,
visits.2 = visits.1 %>%
  rowwise() %>%
  mutate(n_EventIDs = map_int(EventIDs, length)) %>%
  relocate(n_EventIDs, .after = EventIDs)

# Let's find the mean T/S for visits that measured T/S (those that are not NA),
visits.3 = visits.2 %>%
  rowwise() %>%
  mutate(Temp_avg = mean(Temps$Temp_C, na.rm = TRUE),
         Temp_sd = sd(Temps$Temp_C, na.rm = TRUE),
         Sal_avg = mean(Sals$Salinity, na.rm = TRUE),
         Sal_sd = sd(Sals$Salinity, na.rm = TRUE)) %>%
  relocate(Temp_avg, Temp_sd, .after = Temps) %>%
  relocate(Sal_avg, Sal_sd, .after = Sals) %>%
  select(-Temps, -Sals)

# Now, let's tackle habitat classifications,
events_qc$Habitat %>% unique() %>% sort()
# Instead of NA's, we have 'not reported'.

# This^ is really only an issue if we have Visits with both reported and not reported habitat classes,
visits.3 %>%
  select(Habitats) %>%
  rowwise() %>%
  mutate(check_Habitats = has_element(Habitats, "not reported")) %>%
  filter(check_Habitats == TRUE) %>%
  mutate(n_not_reported = map_int(Habitats, length)) -> check
which(check$n_not_reported > 1)
# Ok great, looks like no habitat reported only occurs by itself

# We should now deal with cases where multiple habitats are reported
visits.4 = visits.3 %>%
  rowwise() %>%
  unnest_longer(Habitats) %>%
  unpack(Habitats) %>%
  arrange(VisitID, Habitat) %>%
  separate_rows(Habitat, sep = "-") %>%
  distinct() %>%
  pivot_wider(names_from = Habitat, values_from = Habitat, values_fill = NA) %>%
  replace(is.null(.), NA) %>%
  unite(col = Habitat, habitats, sep = "-", na.rm = TRUE) %>%
  relocate(Habitat, .after = Locations)


