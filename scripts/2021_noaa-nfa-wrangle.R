# -----------------------------------------------------------------------
# Title: 2021 KBNERR data for the NFA
# Creator: Chris Guo
# Date: 07 February 2022
# Purpose: Wrangle KBNERR beach seine information into format for NOAA's Nearshore Fish Atlas. There were two projects:
# NERRS SC Catalyst Grant - Forage fish STX
# Kenai Peninsula FHP - East Cook Inlet beaches

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

# From the NERR SC Catalyst project:
stx.site = read_csv(file = file.path(dir.data, "2021_nerrsc_site.csv"))
stx.fish = read_csv(file = file.path(dir.data, "2021_nerrsc_fish.csv"))
stx.env = read_csv(file = file.path(dir.data, "2021_nerrsc_env.csv"))

# From the KPFHP east cook inlet beaches project:
eci.site = read_csv(file = file.path(dir.data, "2021_kpfhp_site.csv"))
eci.fish = read_csv(file = file.path(dir.data, "2021_kpfhp_fish.csv"))
eci.env = read_csv(file = file.path(dir.data, "2021_kpfhp_env.csv"))

# Fish species information
species_list = read_csv(file = file.path(dir.data, "species-list.csv"))

## There are data from two projects can be included for the NOAA NFA.
# The data format requires three different spreadsheets: Sites, Events, Catch


# 'Sites' datasheet -------------------------------------------------------=

# Let's check that our site vars match
colnames(eci.site) == colnames(stx.site)

mutate(stx.site, Depth_m = as.double(Depth_m)) %>% # matching var types
  bind_rows(eci.site, .) -> x.1


unique(x.1$Site) # take a look at what we have

# We'll want to keep Barabara A/B separate sites but name it the same 'Location',
# and we'll need to provide a single lat/lon coordinate per site...

x.2 = x.1 %>%
  group_by(Site) %>%
  mutate(avg_lat = mean(Latitude),
         avg_lon = mean(Longitude)) %>%
  ungroup() %>%
  select(`Raw Site` = Site,
         Latitude = avg_lat,
         Longitude = avg_lon,
         Habitat) %>%
  distinct()

x.3 = x.2 %>%
  rownames_to_column(var = 'SiteID') %>% # This is the identifier the NFA uses
  mutate(Location = if_else(`Raw Site` %in% c('Barabara A', 'Barabara B'),
                            'Barabara Point',
                            `Raw Site`)) %>%
  relocate(Location, .after = 1)

anyNA(x.3) # final check for NAs

# Export this 'Sites' df as a spreadsheet:
write_csv(x = x.3,
          file = file.path(dir.output, "2021_NFA-wrangle_Sites.csv"))


# 'Events' datasheet ------------------------------------------------------

## most of the info we need is in our combined site df:

y.1 = x.1 %>%
  mutate(Date = mdy(Date), # The NFA asks for mm/dd/yyyy format
         Date = format(Date, '%m/%d/%Y')) %>%
  select(`Raw Site` = Site,
         SeineID, # we'll rename this after we're done joining df's
         Date,
         Gear = Method,
         SetTime = Time,
         Tide = Tide1,
         Comments = Notes)

## We'll include T/S info from our env files:

length(y.1$SeineID) # Note: there are more seine obs than there are T/S obs
colnames(eci.env) == colnames(stx.env) # check the vars are the same 

TS = bind_rows(eci.env, stx.env) %>%
  select(SeineID,
         Temp = Temperature_C,
         Salinity)

y.2 = left_join(y.1, TS, by = "SeineID") %>%
  relocate(Temp, Salinity, .before = Tide)

## We should reflect that the two projects had different seine dimensions

# By adding our SiteID info, we can mutate 'Gear' depending on site:
(sites = unique(x.3$`Raw Site`)) # see what sites we're working with

kpfhp.sites = c('Anchor Point', 'Ninilchik', 'Plumb Bluff')
nerrsc.sites = sites[!sites %in% kpfhp.sites]
(nerrsc.sites = nerrsc.sites[-7]) # Dropping 'Outside Beach"

# Add info, mutate Gear, and an empty col for 'Crew'
y.3 = select(x.3, SiteID, `Raw Site`) %>%
  left_join(y.2, ., by = 'Raw Site') %>%
  relocate(SiteID, .before = 1) %>%
  mutate(Gear = case_when(`Raw Site` %in% kpfhp.sites ~ 'BSEINE_A',
                          `Raw Site` %in% nerrsc.sites ~ 'BSEINE_B',
                          `Raw Site` == 'Outside Beach' ~ 'Hand Collection'),
         Crew = "") %>%
  relocate(Crew, .before = Comments) %>%
  rownames_to_column(var = 'EventID')

## Lastly, we'll just clean this df a little:

y.4 = y.3 %>%
  select(-`Raw Site`) %>%
  rename(`Raw Event` = SeineID) %>%
  relocate(SiteID, .before = 1) %>%
  replace_na(list(Temp = '',
                  Salinity = '',
                  Comments = ''))
  
anyNA(y.4) # final checkfor NAs

# Export this 'Events' df as a spreadsheet:
write_csv(x = y.4,
          file = file.path(dir.output, "2021_NFA-wrangle_Events.csv"))


# 'Catch' datasheet -------------------------------------------------------

## The Catch information needs to be tied back to unique seine sets (i.e., EventID),
# so we'll start by taking this info from the 'Events' df:

z.1 = select(y.3, EventID, SeineID)

## We need to combine the two fish datasets, but each have slightly different cols:
z.2 = bind_rows(select(eci.fish,
                       SeineID,
                       Common,
                       LifeStage,
                       Count,
                       Length_mm,
                       Notes),
                select(stx.fish,
                       SeineID,
                       Common,
                       LifeStage,
                       Count,
                       Length_mm,
                       Notes))

# Check for any useful notes:
unique(z.2$Notes)
# Perhaps 'Hatchery' would be useful, otherwise let's remove the the notes...

## Let's add EventID, fix 'Notes', rename where appropriate, replace NAs with '':
z.3 = z.2 %>%
  left_join(., z.1, by = 'SeineID') %>%
  relocate(EventID, .before = 1) %>%
  mutate(Count = if_else(Count > 1,
                         Count,
                         0),
         Notes = if_else(Notes == 'Hatchery',
                         Notes,
                         "")) %>%
  rename(Unmeasured = Count,
         Length = Length_mm,
         Comments = Notes) %>%
  replace_na(list(LifeStage = '',
                  Unmeasured = '',
                  Length = '',
                  Comments = ''))

## We need to add in LengthType info stored in the species_list df:
z.4 = select(species_list, Common, LengthType) %>%
  left_join(z.3, ., by = "Common") %>%
  relocate(LengthType, .after = Length) %>%
  select(-SeineID) %>%
  rename(SpeciesName = Common) %>% # clean up
  mutate(LengthType = replace_na(LengthType, '')) # replace any NAs in new col

anyNA(z.4) # final check for any NAs

# Export this 'Catch' df as a spreadsheet:
write_csv(x = z.4,
          file = file.path(dir.output, "2021_NFA-wrangle_Catch.csv"))
