# -----------------------------------------------------------------------
# Title: 2021 KBNERR collection permits
# Creator: Chris Guo
# Date: 20 January 2022
# Purpose: Generate ADFG collection reports for permit nos.:
# CF-21-049 (Steve Kibler, NOAA Beaufort)
# CF-21-058 (Chris Guo, KBNERR)

# Notes -------------------------------------------------------------------

## There is an odd issue in the CF-21-058 wrangle:
# There is an NA row in the kbrr df's: row 32492 which way beyond our no. of obs.
# This could be a relic from exporting excel > csv files.
# See comments starting line 192.

# Load packages -----------------------------------------------------------

library(tidyverse)

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

# From the KPFHP east cook inlet beaches project:
eci.site = read_csv(file = file.path(dir.data, "2021_kpfhp_site.csv"))
eci.fish = read_csv(file = file.path(dir.data, "2021_kpfhp_fish.csv"))


# CF-21-049 (Steve Kibler, NOAA Beaufort) ---------------------------------

## There are data from both projects that need to be reported under this permit.

## Let's start by subsetting the ECI data for fish used in PSP analysis:
x.1 = eci.fish %>%
  filter(PSP == TRUE) %>% 
  left_join(eci.site, by = "SeineID") %>% # join the site data to the fish data
  select(Date,
         Site,
         Latitude,
         Longitude,
         `Collection Method` = Method,
         Species = Common,
         `Life Stage` = LifeStage,
         Size_mm = Length_mm,
         Number = Count,
         Disposition = Deceased,
         Comments = Notes.x) # notes from the fish data
  
unique(!is.na(x.1$Disposition)) # check for any special dispositions
unique(x.1$Comments) # check for any comments

# We'll complete/clean the fields for Life Stage, Disposition, and Comments.
# In this case, Disposition = 'sacrificed' for all instances
x.2 = x.1 %>%
  mutate(`Life Stage` = case_when(`Life Stage` %in% c('YOY', 'Juvenile') ~ 'Juvenile',
                                  `Life Stage` == 'Adult' ~ 'Adult',
                                  is.na(`Life Stage`) ~ 'Juvenile/Adult'),
         Disposition = 'sacrificed',
         Comments = '',
         Size_mm = replace_na(as.character(Size_mm), '')) # in case there are any unmeasured fish

anyNA(x.2) # check for NAs

## Next, let's reformat the STX data:

# Combine STX site data and fish data, select only relevant cols
y.1 = full_join(stx.site, stx.fish, by = 'SeineID') %>%
  select(Date,
         Site,
         Latitude,
         Longitude,
         `Collection Method` = Method,
         Species = Common,
         `Life Stage` = LifeStage,
         Size_mm = Length_mm,
         Number = Count,
         Sacrificed, `Fish Tank`, Deceased, # We'll need these to complete 'Disposition'
         Comments = Notes.y) # notes from the fish data

unique(!is.na(y.1$Deceased)) # check for any special dispositions
unique(y.1$Comments) # check for any comments
# Hand collections were already noted in `Collection Method`, no further action necessary

y.2 = y.1 %>%
  mutate(Disposition = case_when(!is.na(Sacrificed) ~ 'sacrificed',
                                 is.na(Sacrificed) & !is.na(`Fish Tank`) ~ 'released live',
                                 is.na(Sacrificed) & is.na(`Fish Tank`) ~ 'released live'),
         `Life Stage` = case_when(`Life Stage` %in% c('YOY', 'Juvenile') ~ 'Juvenile',
                                  `Life Stage` == 'Adult' ~ 'Adult',
                                  is.na(`Life Stage`) ~ 'Juvenile/Adult'),
         Comments = '',
         Size_mm = replace_na(as.character(Size_mm), '')) %>%
  select(-c(Sacrificed, `Fish Tank`, Deceased), # clean up
         Date,
         Site,
         Latitude,
         Longitude,
         `Collection Method`,
         Species,
         `Life Stage`,
         Size_mm,
         Number,
         Disposition, # making sure this is in correct order
         Comments)

anyNA(y.2) # check for NAs

## Now, we can combine the data from both projects:
psp.1 = bind_rows(x.2, y.2)

# Last reformat - we need to adjust 'Site' into 'Location' for the report:
(sites = unique(psp.1$Site)) # Most are Kachemak Bay...
kodiak = c('White Sands', 'Pillar Creek') # A couple from kodiak
cook.inlet = sites[!sites %in% kodiak]


psp.2 = psp.1 %>%
  mutate(Site = case_when(Site %in% kodiak ~ 'Kodiak',
                          Site %in% cook.inlet ~ 'Kachemak Bay/Cook Inlet')) %>%
  rename(Location = Site)

## ADFG collection reports require the data in two stages:

# The first stage is the 'Raw Data', which we've already formatted for export,
write_csv(x = psp.2,
          file = file.path(dir.output, "2021_CF-21-049_RawData.csv"))

# The second stage is a 'Summary' format,
psp.3 = psp.2 %>%
  select(Location,
         Species,
         Number) %>%
  group_by(Location, Species) %>%
  summarise(n = sum(Number)) %>%
  rename(Number = n)

# Export Summary,
write_csv(x = psp.3,
          file = file.path(dir.output, "2021_CF-21-049_Summary.csv"))

# These output files can now be combined pretty quickly into the ADFG template file.



# CF-21-058 (Chris Guo, KBNERR) -------------------------------------------

## These data only come from the KPFHP project

# We will first subset the data to remove the PSP-related fish,
# then join the paired site data
kbrr.1 = eci.fish %>%
  filter(is.na(PSP)) %>%
  left_join(eci.site, by = 'SeineID') %>%
  select(Date,
         Site,
         Latitude,
         Longitude,
         `Collection Method` = Method,
         Species = Common,
         `Life Stage` = LifeStage,
         Size_mm = Length_mm,
         Number = Count,
         Disposition = Deceased,
         Comments = Notes.x) # notes from the fish data

unique(!is.na(kbrr.1$Disposition)) # check for any special dispositions
unique(kbrr.1$Comments) # check for any comments
# Maybe 'Hatchery' could be useful information for ADFG

kbrr.2 = kbrr.1 %>%
  mutate(`Life Stage` = case_when(`Life Stage` %in% c('YOY', 'Juvenile') ~ 'Juvenile',
                                  `Life Stage` == 'Adult' ~ 'Adult',
                                  is.na(`Life Stage`) ~ 'Juvenile/Adult'),
         Disposition = 'released live',
         Comments = ifelse(Comments == "Hatchery",
                           'Hatchery',
                           NA),
         Comments = replace_na(Comments, ''),
         Size_mm = replace_na(as.character(Size_mm), ''))
  
anyNA(kbrr.2) # check for NAs

which(is.na(kbrr.2)) # This is weird! 32492 is way outside our dataset
kbrr.2[which(is.na(kbrr.2)),] # this row doesn't exist
kbrr.2[-32492,] # Dropping this row by name does not do anything
kbrr.3 = drop_na(kbrr.2) # Should be ok since the only NA is the weird row

anyNA(kbrr.3) # ^That was not an ideal fix, but we'll just move on...

# Let's adjust the Site to be Location, replacing 'Plumb Bluff' with 'Stariski,
kbrr.4 = kbrr.3 %>%
  mutate(Site = ifelse(Site == 'Plumb Bluff',
                       'Stariski',
                       Site)) %>%
  rename(Location = Site)

## ADFG collection reports require the data in two stages:

# The first stage is the 'Raw Data', which we've already formatted for export,
write_csv(x = kbrr.4,
          file = file.path(dir.output, "2021_CF-21-058_RawData.csv"))

# The second stage is a 'Summary' format,
kbrr.5 = kbrr.4 %>%
  select(Species,
         Number) %>%
  group_by(Species) %>%
  summarise(n = sum(Number)) %>%
  rename(Number = n)

# Export Summary,
write_csv(x = kbrr.5,
          file = file.path(dir.output, "2021_CF-21-058_Summary.csv"))
