# -----------------------------------------------------------------------
# Title: 2022 KBNERR data for the NFA
# Creator: Chris Guo
# Date: 22 June 2023
# Purpose: Wrangle KBNERR beach seine information into format for NOAA's Nearshore Fish Atlas.

# Notes -------------------------------------------------------------------

# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)

# Define workflow paths ---------------------------------------------------

wd = getwd()
dir.output = file.path(wd, "output")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd,"data")
dir.R = file.path(wd,"R")

# Utility -----------------------------------------------------------------


# Read in data ------------------------------------------------------------

# From the 2019 KPFHP project:
site = read_csv(file = file.path(dir.data, "2022_kbnerr_site.csv"))
env = read_csv(file = file.path(dir.data, "2022_kbnerr_env.csv"))
fish = read_csv(file = file.path(dir.data, "2022_kbnerr_fish.csv"))

# Fish species information
species_list = read_csv(file = file.path(dir.data, "species-list.csv"))

# KBNERR sites information
sites_list = read_csv(file = file.path(dir.data, "sites-list.csv"))


# 'Sites' datasheet -------------------------------------------------------=

# Take a look at what sites we have
unique(site$Site)

# We'll need to provide a single lat/lon coordinate per site,
# And make a distinction between 'Raw Site' and 'Location' for the NFA:
x.1 = site %>%
  group_by(Site) %>%
  mutate(avg_lat = mean(Latitude),
         avg_lon = mean(Longitude)) %>%
  ungroup() %>%
  left_join(select(sites_list, Site, nfa_Location), by = 'Site') %>%
  select(Location = nfa_Location,
         'Raw Site' = Site,
         Latitude = avg_lat,
         Longitude = avg_lon,
         Habitat) %>%
  distinct()

# It's possible to have multiple habitat classes within a site,
# but we'll want to combine them into a single 'value':

# Save the habitat types as a vector,
x.1$Habitat %>% unique() %>% sort() -> habitats
# Collapse Habitat into single value per row
x.1a = x.1 %>%
  pivot_wider(names_from = Habitat, values_from = Habitat) %>%
  replace(is.null(.), NA) %>%
  unite(col = Habitat, all_of(habitats), sep = "/", na.rm = TRUE)

# Add the SiteID identifier (1,2,etc.) the NFA uses
x.2 = x.1a %>%
  rownames_to_column(var = 'SiteID') %>%
  relocate(Location, .after = 1)

anyNA(x.2) # final check for NAs

# Export this 'Sites' df as a spreadsheet:
write_csv(x = x.2,
          file = file.path(dir.output, "2022_NFA-wrangle_Sites.csv"))


# 'Events' datasheet ------------------------------------------------------

## Most of the info we need is in our first site df (x.1):

y.1 = site %>%
  mutate(Date = mdy(Date), # The NFA asks for mm/dd/yyyy format
         Date = format(Date, '%m/%d/%Y')) %>%
  select('Raw Site' = Site,
         SeineID, # we'll rename this after we're done joining df's
         Date,
         Gear = Method,
         SetTime = Time,
         Tide = Tide1,
         Comments = Notes)

## We'll include T/S info from our env files:
TS = select(env,
            SeineID,
            Temp = Temperature_C,
            Salinity)

# Add site info, T/S data, and an empty col for 'Crew'
y.2 = select(x.2, SiteID, `Raw Site`) %>%
  left_join(y.1, ., by = 'Raw Site') %>%
  relocate(SiteID, .before = 1) %>%
  left_join(TS, by = "SeineID") %>%
  relocate(Temp, Salinity, .before = Tide) %>%
  mutate(Crew = "") %>%
  relocate(Crew, .before = Comments) %>%
  rownames_to_column(var = 'EventID')

## Lastly, we'll just clean this df a little:

y.3 = y.2 %>%
  select(-`Raw Site`) %>%
  rename(`Raw Event` = SeineID) %>%
  relocate(SiteID, .before = 1) %>%
  replace_na(list(Temp = '',
                  Salinity = '')) %>%
  mutate(Gear = 'BSEINE',
         Comments = '')


anyNA(y.3) # final check for NAs

# Export this 'Events' df as a spreadsheet:
write_csv(x = y.3,
          file = file.path(dir.output, "2022_NFA-wrangle_Events.csv"))


# 'Catch' datasheet -------------------------------------------------------

## The Catch information needs to be tied back to unique seine sets (i.e., EventID),
# so we'll start by taking this info from the df (y.2) before renaming 'SeineID':

z.1 = select(y.2, EventID, SeineID)

## We just take the useful cols from our fish data:
z.2 = select(fish,
             SeineID,
             Common,
             LifeStage,
             Count,
             Length_mm,
             Notes)

# Check for any useful notes:
(notes = unique(z.2$Notes))
# Let's remove notes that are unnecessary for the NFA...


## Add EventID, fix 'Notes', rename where appropriate, replace NAs with '':
z.3 = z.2 %>%
  left_join(., z.1, by = 'SeineID') %>%
  relocate(EventID, .before = 1) %>%
  mutate(Count = if_else(is.na(Length_mm),
                         Count,
                         0),
         Notes = if_else(Notes %in% notes[c(2,5,7)],
                         Notes,
                         '')) %>%
  mutate(Count = Count %>% as.character(),
         Length_mm = Length_mm %>% as.character()) %>%
  rename(Unmeasured = Count,
         Length = Length_mm,
         Comments = Notes) %>%
  replace_na(list(LifeStage = '',
                  Unmeasured = '',
                  Length = '',
                  Comments = ''))

anyNA(z.3) # check

## We need to add in LengthType info stored in the species_list df:
z.4 = select(species_list, Common, LengthType) %>%
  left_join(z.3, ., by = "Common") %>%
  relocate(LengthType, .after = Length) %>%
  select(-SeineID) %>%
  rename(SpeciesName = Common) %>%
  replace_na(list(LengthType = ''))

anyNA(z.4) # final check for any NAs

# Export this 'Catch' df as a spreadsheet:
write_csv(x = z.4,
          file = file.path(dir.output, "2022_NFA-wrangle_Catch.csv"))
