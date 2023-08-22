# -----------------------------------------------------------------------
# Title: 2022 KBNERR collection report
# Creator: Chris Guo
# Date: 04 July 2023
# Purpose: Research results for collection permit report

# Notes -------------------------------------------------------------------


# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggmap)

# Define workflow paths ---------------------------------------------------

wd = getwd()
dir.output = file.path(wd, "output")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd,"data")

# Utility -----------------------------------------------------------------

register_google(key = api.google, write = TRUE)

# Read in data ------------------------------------------------------------

# Field data
eci.site = read_csv(file = file.path(dir.data, "2022_kbnerr_site.csv"))
eci.fish = read_csv(file = file.path(dir.data, "2022_kbnerr_fish.csv"))

# Supplementary data
species_list = read_csv(file = file.path(dir.data, "species-list.csv"))
sites_list = read_csv(file = file.path(dir.data, "sites-list.csv"))

# Data clean up -----------------------------------------------------------

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
         Common,
         LifeStage,
         Length_mm,
         Count,
         Disposition = Deceased,
         Comments = Notes.x) # notes from the fish data

unique(!anyNA(kbrr.1$Disposition)) # check for any special dispositions
unique(kbrr.1$Comments) # check for any comments
# None of these are pertinent except vouchers

kbrr.2 = kbrr.1 %>%
  mutate(LifeStage = case_when(LifeStage == 'YOY' ~ 'YOY',
                               LifeStage == 'Juvenile' ~ 'J',
                               LifeStage == 'Adult' ~ 'A',
                               is.na(LifeStage) ~ 'J/A'),
         Comments = ifelse(Comments == "Voucher",
                           Comments,
                           NA),
         Comments = replace_na(Comments, ''),
         Disposition = ifelse(Comments == "Voucher",
                              "sacrificed",
                              "released live"))

# Let's adjust the Site to be Location, replacing 'Plumb Bluff' with 'Stariski,
kbrr.3 = kbrr.2 %>%
  mutate(Site = ifelse(Site == 'Plumb Bluff',
                       'Stariski',
                       Site)) %>%
  rename(Location = Site)

# EDA & figures -----------------------------------------------------------

# Calculate counts by species and life stage:
fish.counts = kbrr.3 %>%
  group_by(Common, LifeStage) %>%
  summarise(Count = sum(Count, na.rm = TRUE))

# Calculate avg, min, max length by species and lifestage:
fish.sizes = kbrr.3 %>%
  filter(!is.na(Length_mm)) %>% # Remove obs without length measurements
  group_by(Common, LifeStage) %>%
  summarise(n = n(),
            Length.mean = round(mean(Length_mm, na.rm = TRUE), digits = 0),
            Length.min = min(Length_mm, na.rm = TRUE),
            Length.max = max(Length_mm, na.rm = TRUE)) %>%
  mutate(Length.min = if_else(n == 1,
                              NA,
                              Length.min),
         Length.max = if_else(n == 1,
                              NA,
                              Length.max))

# Next, let's join the count and size data:
# *Note that there are less observations of fish size...
fish.2 = left_join(fish.counts, fish.sizes, by = c("Common", "LifeStage"))

# Now, let's add in scientific names and fish/invert groupings
fish.3 = select(species_list, Common, Scientific, Type, LengthType) %>%
  left_join(fish.2, ., by = "Common") %>%
  relocate(Type, Scientific, .before = 1) %>%
  relocate(LengthType, .after = Length.max)

# Lastly, we need to tidy up the values for format and readability:
fish.tbl = fish.3 %>%
  mutate(n = as.character(n) %>% replace_na('0'),
         n = paste("(", n, ")", sep = ''),
         Length.mean = Length.mean %>% as.character() %>% replace_na('--'),
         Length.min = Length.min %>% as.character() %>% replace_na(''),
         Length.min = paste("(", Length.min, sep = ''),
         Length.max = Length.max %>% as.character() %>% replace_na(''),
         Length.max = paste(Length.max, ")", sep = '')) %>%
  unite(Length.min, Length.max, col = 'Range', sep = '-') %>%
  mutate(Range = ifelse(Length.mean == '--' | Count == 1, '--', Range),
         LengthType = ifelse(Length.mean == '--' | Count == 1, '--', LengthType)) %>%
  unite(Count, n, col = 'Count', sep = ' ') %>%
  rename('Group' = Type,
         'Scientific Name' = Scientific,
         'Common Name' = Common,
         'Life Stage' = LifeStage,
         'Total (# Sized)' = Count,
         'Mean' = Length.mean,
         'Range' = Range,
         'Type' = LengthType) %>%
  arrange(Group, `Scientific Name`)

# Export the table:
write_csv(x = fish.tbl, file = file.path(dir.output, "2022_collection-reprt_all-spp-tbl.csv"))

fish.3 %>% mutate(Type = as.factor(Type)) %>% group_by(Type) %>% reframe(n = Count, print(., n = 80))
