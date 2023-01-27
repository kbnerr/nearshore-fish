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

# QAQC of catch data ------------------------------------------------------

# Let's first designate a count col in the df,
catch.1 = mutate(catch, Count = ifelse(Unmeasured == 0,
                                       1,
                                       Unmeasured))

# Dropping obs that are not ID'ed to family level
catch.2 = catch.1  %>% filter(!Fam_CommonName %in% c("", "bony fishes"))
# Note- this removes EventID = 3156, VisitID = 379_2007-08-09:
catch.1 %>% filter(VisitID == '379_2007-08-09') # check

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
# These three cases also seem to be single family incidences
# We will want to remove extremely rare cases...

# We may also want to examine at a finer taxon: genus level

# Let's first check out what Genuses we're working with,
(species = catch.3$Sp_ScientificName %>% unique() %>% sort())
# Looks to have some Family-level data mixed in. Let's isolate and view these,
species %>% word(., 1) %>% str_view("ae$", match = TRUE)
# We'll put these families into an object to subset out,
(drop_families = str_subset(word(species, 1), "ae$"))

catch.4 = catch.3 %>%
  mutate(Gen_ScientificName = word(Sp_ScientificName, 1)) %>%
  filter(!Gen_ScientificName %in% drop_families)


# Now, let's graph family abundance,
catch.4 %>%
  group_by(Fam_CommonName) %>%
  summarise(Abundance = sum(Count)) %>%
  mutate(Fam_CommonName = fct_reorder(as.factor(Fam_CommonName), desc(Abundance))) %>% 
  ggplot(aes(x = Fam_CommonName, y = Abundance)) +
  geom_col() +
  geom_text(aes(label = Abundance), vjust = -0.5, size = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ggsave("nfa_fam_abundance_raw.png", plot = last_plot(), device = 'png', path = file.path(dir.figs))

# Graph Family frequency of occurrence
fam_freq_occur = catch.4 %>%
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
# ggsave("nfa_fam_freq_occurrence.png", plot = last_plot(), device = 'png', path = file.path(dir.figs))

# Next, let's graph genus abundance
p_gen_abun = catch.4 %>%
  group_by(Gen_ScientificName) %>%
  summarise(Abundance = sum(Count)) %>%
  mutate(Gen_ScientificName = fct_reorder(as.factor(Gen_ScientificName), desc(Abundance))) %>% 
  ggplot(aes(x = Gen_ScientificName, y = Abundance)) +
  geom_col() +
  geom_text(aes(label = Abundance), vjust = -0.5, size = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ggsave("nfa_gen_abundance_raw.png", plot = p_gen_abun, device = 'png', path = file.path(dir.figs))

# Graph frequency of occurrence
gen_freq_occur = catch.4 %>%
  group_by(VisitID, Gen_ScientificName) %>%
  summarise(Presence = n_distinct(Gen_ScientificName)) %>%
  ungroup() %>%
  group_by(Gen_ScientificName) %>%
  summarise(Occurrence = sum(Presence)) %>%
  mutate(Perc_Occurrence = Occurrence / n_distinct(catch.4$VisitID) * 100,
         Gen_ScientificName = fct_reorder(as.factor(Gen_ScientificName), desc(Perc_Occurrence)))

p_gen_freq_occur = gen_freq_occur %>%
  ggplot(aes(x = Gen_ScientificName, y = Perc_Occurrence)) +
  geom_col() +
  geom_text(aes(label = round(Perc_Occurrence, 1)), size = 2, vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ggsave("nfa_gen_freq_occurrence.png", plot = p_freq_occur, device = 'png', path = file.path(dir.figs))

# View the common names for rare Genuses, i.e., occuring in <0.25% of visits
# These include 34 different taxa
rare_genus = gen_freq_occur %>%
  filter(Perc_Occurrence < 0.25) %>%
  mutate(Gen_ScientificName = as.character(Gen_ScientificName)) %>%
  select(Gen_ScientificName) %>%
  as_vector()

# There are 10 families that occur in less than 1% of visits.
# We will consider these our extreme rare cases and remove them from the df,
rare_families = fam_freq_occur %>%
  filter(Perc_Occurrence < 1) %>%
  mutate(Fam_CommonName = as.character(Fam_CommonName)) %>%
  select(Fam_CommonName) %>%
  as_vector()

# We will go with Genus level for now, removing the rarest taxa
catch.5 = filter(catch.4, !Gen_ScientificName %in% rare_genus)

# Moving forward we only need the most a couple versions of the data:
# QC'ed events level data, original events level data (for comparison purposes),
# and most recent catch level data:
catch_qc = catch.5 %>% arrange(VisitID) # Won't hurt to end by ordering our df by VisitID's


# What if we try to use biomass? ------------------------------------------

# Let's first figure out how many samples we would have...
tmp = catch_qc %>%
  group_by(VisitID, Fam_CommonName) %>%
  transmute(Abundance = sum(Count, na.rm = TRUE),
            Avg_Length = mean(Length_mm, na.rm = TRUE)) %>%
  drop_na()
tmp %>% distinct()
  anti_join(tmp, select(visits_qc, VisitID), by = "VisitID")
# Looks like we don't lose any visits...

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

gen_abun = catch_qc %>%
  select(VisitID,
         EventID,
         Gen_ScientificName,
         Count) %>%
  group_by(VisitID, Gen_ScientificName) %>%
  summarise(Abundance = (sum(Count) / n_distinct(EventID)) %>% round(2)) %>%
  ungroup()

# Environment clean-up ----------------------------------------------------

# We'll keep these plus our wd objects:
keep = c('data',
         'events_qc',
         'catch_qc',
         'gen_abun',
         'wd',
         'dir.data',
         'dir.figs',
         'dir.output',
         'dir.scripts')

# And remove the rest, so we have a clean env when sourcing this code in next steps analyses:
rm(list = ls()[!ls() %in% keep])







 
