# -----------------------------------------------------------------------
# Title: 2022 KBNERR collection permit
# Creator: Chris Guo
# Date: 03 January 2023
# Purpose: Generate ADFG collection reports for permit nos.:
# CF-22-066 (Chris Guo, KBNERR)

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

eci.site = read_csv(file = file.path(dir.data, "2022_kbnerr_site.csv"))
eci.fish = read_csv(file = file.path(dir.data, "2022_kbnerr_fish.csv"))


# CF-22-066 (Chris Guo, KBNERR) -------------------------------------------

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
# None of these are pertinent except vouchers

kbrr.2 = kbrr.1 %>%
  mutate(`Life Stage` = case_when(`Life Stage` %in% c('YOY', 'Juvenile') ~ 'Juvenile',
                                  `Life Stage` == 'Adult' ~ 'Adult',
                                  is.na(`Life Stage`) ~ 'Juvenile/Adult'),
         Comments = ifelse(Comments == "Voucher",
                           Comments,
                           NA),
         Comments = replace_na(Comments, ''),
         Disposition = ifelse(Comments == "Voucher",
                              "sacrificed",
                              "released live"),
         Size_mm = replace_na(as.character(Size_mm), ''))

unique(kbrr.2$Comments) # check
unique(kbrr.2$Disposition)

anyNA(kbrr.2) # another check for NAs
which(is.na(kbrr.2)) # This is weird! these obs are way outside our dataset range
kbrr.2[which(is.na(kbrr.2)),] # these rows don't exist
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
          file = file.path(dir.output, "2022_CF-22-066_RawData.csv"))

# The second stage is a 'Summary' format,
kbrr.5 = kbrr.4 %>%
  select(Species,
         Number) %>%
  group_by(Species) %>%
  summarise(n = sum(Number)) %>%
  rename(Number = n)

# Export Summary,
write_csv(x = kbrr.5,
          file = file.path(dir.output, "2022_CF-22-066_Summary.csv"))
