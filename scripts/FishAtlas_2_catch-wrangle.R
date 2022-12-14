# -----------------------------------------------------------------------
# Title: Wrangle of NOAA's Nearshore Fish Atlas catch data
# Creator: Chris Guo
# Date: 2022.10.05
# Purpose: Re-format catch data into different datasets for community analyses

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
source(file = file.path(dir.scripts, "utility.R"))

# A little EDA of the catch data ------------------------------------------

# Here's what we have,
str(catch)

catch$VisitID %>% n_distinct()
catch$SiteID %>% n_distinct()
catch$EventID %>% n_distinct()

# Let's see what info we might be missing,
summary(catch)
# looks like there are lots of NAs for length - not surprising

# I'm guessing we have lots of non-labeled (missing) life stages too:
unique(catch$LifeStage)
# yep.. let's see how many obs are like this,
which(catch$LifeStage == "") %>% length()

# We can figure out life stage if we have length,
# let's see if this is feasible
filter(catch, LifeStage == "" & is.na(Length_mm))
# We have ~9k obs of missing life stage.
# Some of these appear to be bc the samplers reached a max sample for measuring.
# Some are likely truly missing cases...

# We can at least reduce the missing obs by assigning an avg length where appropriate:
catch %>%
  filter(LifeStage == "") %>%
  group_by(EventID, Sp_CommonName) %>%
  summarise(avg.length = mean(Length_mm, na.rm = TRUE)) %>%
  filter(is.na(avg.length)) %>% # still around 8k obs missing lengths
  select(EventID) %>%
  n_distinct()
# looks like about 2/5 of our events have missing length and lifestage info,
# even after assigning avg lengths to missing obs

catch$Sp_CommonName %>% unique()
catch$Sp_ScientificName %>% unique() %>% sort()

fam_sci_names = catch$Fam_ScientificName %>% unique()
catch %>% filter(Sp_ScientificName %in% fam_sci_names)

catch$Fam_CommonName %>% unique()
catch$Fam_ScientificName %>% unique() 
catch %>% filter(Fam_CommonName == "") # 9 fish from the EPSCOR project. We'll drop these. 
catch %>% filter(Fam_CommonName == "bony fishes") # 10,982 fish only identified to Teleostei. We'll drop these too.

# Let's first designate a count col in the df,
catch.1 = mutate(catch, Count = ifelse(Unmeasured == 0,
                                       1,
                                       Unmeasured))

# QAQC of catch data ------------------------------------------------------

# Dropping obs that are not ID'ed to family level
catch.2 = catch.1  %>% filter(!Fam_CommonName %in% c("", "bony fishes"))
# Note- this removes EventID = 3156, VisitID = 379_2007-08-09:
catch.1 %>% filter(VisitID == '379_2007-08-09')

# Re-label "or" cases to the family level
catch.3 = catch.2 %>%
  mutate(Sp_ScientificName = ifelse(Sp_ScientificName == 'Anisarchus medius or Lumpenus fabricii',
                                    'Stichaeidae',
                                    Sp_ScientificName))

# Initial nmds plots show wrymouths and eelpouts as outlier catches.
# Let's take a look,
catch.3 %>% filter(Fam_CommonName %in% c("wrymouths", "eelpouts"))
# Wow, only two incidences of catching these... Let's see where they come from,
events_qc %>% filter(EventID %in% c("6227", "8042"))
# Was anything else caught with them?
catch.3 %>% filter(EventID %in% c("6227", "8042"))

# Also from initial nmds, I see three visits (upper left) that seem pretty different too,
events_qc %>% filter(VisitID %in% c("288_1998-08-09", "291_1998-08-09", "36_2004-03-17"))
catch.3 %>% filter(VisitID %in% c("288_1998-08-09", "291_1998-08-09", "36_2004-03-17"))
# These three cases also seem to be single taxa incidences,
# so what we want to do about extremely rare cases...
# Let's first graph family abundance and frequencies of occurrence,

catch.3 %>%
  group_by(Fam_CommonName) %>%
  summarise(Abundance = sum(Count)) %>%
  mutate(Fam_CommonName = fct_reorder(as.factor(Fam_CommonName), desc(Abundance))) %>% 
  ggplot(aes(x = Fam_CommonName, y = Abundance)) +
  geom_col() +
  geom_text(aes(label = Abundance), vjust = -0.5, size = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("nfa_fam_abundance_raw.png", plot = last_plot(), device = 'png', path = file.path(dir.figs))

fam_freq_occur = catch.3 %>%
  group_by(VisitID, Fam_CommonName) %>%
  summarise(Presence = n_distinct(Fam_CommonName)) %>%
  ungroup() %>%
  group_by(Fam_CommonName) %>%
  summarise(Occurrence = sum(Presence)) %>%
  mutate(Perc_Occurrence = Occurrence / n_distinct(catch.3$VisitID) * 100,
         Fam_CommonName = fct_reorder(as.factor(Fam_CommonName), desc(Perc_Occurrence)))

fam_freq_occur %>%
  ggplot(aes(x = Fam_CommonName, y = Perc_Occurrence)) +
  geom_col() +
  geom_text(aes(label = round(Perc_Occurrence, 1)), size = 2, vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("nfa_fam_freq_occurrence.png", plot = last_plot(), device = 'png', path = file.path(dir.figs))

# There are 10 families that occur in less than 1% of visits.
# We will consider these our extreme rare cases and remove them from the df,
rare_families = fam_freq_occur %>%
  filter(Perc_Occurrence < 1) %>%
  mutate(Fam_CommonName = as.character(Fam_CommonName)) %>%
  select(Fam_CommonName) %>%
  as_vector()

catch.4 = filter(catch.3, !Fam_CommonName %in% rare_families)

catch.4 %>% filter(VisitID %in% c("288_1998-08-09", "291_1998-08-09"))

# Moving forward we only need the most a couple versions of the data:
# QC'ed events level data, original events level data (for comparison purposes), and most recent catch level data:
catch_qc = catch.4 %>% arrange(VisitID) # Won't hurt to end by ordering our df by VisitID's

# Creating subsets of QAQC'd data -----------------------------------------

## Family level

fam_abun = catch_qc %>%
  select(VisitID,
         EventID,
         Fam_CommonName,
         Count) %>%
  group_by(VisitID, Fam_CommonName) %>%
  summarise(Abundance = sum(Count) / n_distinct(EventID)) %>%
  ungroup()
  
## Genus level

catch_qc %>%
  separate(Sp_ScientificName, into = c("Gen_ScientificName", "drop"), sep = " ", remove = FALSE) %>%
  select(-drop)
# Warning messages are cases where fish were ID'ed to family level


# Environment clean-up ----------------------------------------------------

# We'll keep these plus our wd objects:
keep = c('data',
         'events',
         'events_qc',
         'catch',
         'catch_qc',
         'fam_abun',
         'wd',
         'dir.data',
         'dir.figs',
         'dir.output',
         'dir.scripts')

# And remove the rest, so we have a clean env when sourcing this code in next steps analyses:
rm(list = ls()[!ls() %in% keep])







 
