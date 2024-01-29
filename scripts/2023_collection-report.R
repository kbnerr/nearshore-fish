# -----------------------------------------------------------------------
# Title: 2023 KBNERR collection permit
# Creator: Chris Guo
# Date: 29 January 2024
# Purpose: Generate ADFG collection reports for permit nos.:
# CF-23-073 (Chris Guo, KBNERR)

# Notes -------------------------------------------------------------------


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

site = read_csv(file = file.path(dir.data, "2023_kbnerr_site.csv"))
fish = read_csv(file = file.path(dir.data, "2023_kbnerr_fish.csv"))


# CF-22-066 (Chris Guo, KBNERR) -------------------------------------------

# We will first subset the data to remove the PSP-related fish,
# then join the paired site data
dat.1 = fish %>%
  left_join(site, by = 'SeineID') %>%
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

unique(!is.na(dat.1$Disposition)) # check for any special dispositions
unique(dat.1$Comments) # check for any comments
# None of these are pertinent except vouchers

dat.2 = dat.1 %>%
  mutate(`Life Stage` = case_when(`Life Stage` %in% c('YOY', 'Juvenile') ~ 'Juvenile',
                                  `Life Stage` == 'Adult' ~ 'Adult',
                                  is.na(`Life Stage`) ~ 'Juvenile/Adult'),
         Comments = ifelse(Comments == "Voucher",
                           Comments,
                           NA),
         Comments = replace_na(as.character(Comments), ''),
         Disposition = ifelse(Comments == "Voucher",
                              "sacrificed",
                              "released live"),
         Size_mm = replace_na(as.character(Size_mm), ''))

unique(dat.2$Comments) # check
unique(dat.2$Disposition)

anyNA(dat.2) # another check for NAs
# which(is.na(dat.2)) # check for phatim rows in csv file!

# Let's adjust the Site to be Location, replacing 'Plumb Bluff' with 'Stariski,
dat.3 = dat.2 %>%
  mutate(Site = ifelse(Site == 'Plumb Bluff',
                       'Stariski',
                       Site)) %>%
  rename(Location = Site)

## ADFG collection reports require the data in two stages:

# The first stage is the 'Raw Data', which we've already formatted for export,
write_csv(x = dat.3,
          file = file.path(dir.output, "2023_CF-23-073_RawData.csv"))

# The second stage is a 'Summary' format,
dat.4 = dat.3 %>%
  select(Species,
         Number) %>%
  group_by(Species) %>%
  summarise(n = sum(Number)) %>%
  rename(Number = n)

# Export Summary,
write_csv(x = dat.4,
          file = file.path(dir.output, "2023_CF-23-073_Summary.csv"))
