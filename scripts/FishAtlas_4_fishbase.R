# -----------------------------------------------------------------------
# Project: NFA-Fishbase wrangle
# Creator: Chris Guo
# Date: 2023.01.27
# Purpose: Grab Length-Weight info from FishBase.org using rfishbase


# Notes -------------------------------------------------------------------

## Vignette for gghighlight found here,
# https://cran.r-project.org/web/packages/gghighlight/vignettes/gghighlight.html
# by Hiroaki Yutani

# Load packages -----------------------------------------------------------

library(tidyverse)
library(rfishbase)
library(gghighlight)

# Define workflow paths ---------------------------------------------------

wd = getwd()
dir.output = file.path(wd, "output")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd,"data")
dir.R = file.path(wd,"R")


# Read in data ------------------------------------------------------------

source(file = file.path(dir.scripts, "FishAtlas_1_events-wrangle.R"))
source(file = file.path(dir.scripts, "FishAtlas_2_catch-wrangle.R"))
source(file = file.path(dir.scripts, "FishAtlas_3_visits-wrangle.R"))
source(file = file.path(dir.scripts, "utility.R"))


# Code --------------------------------------------------------------------

# Create objects for our list of genus and species,
gen_abun %>%
  select(Gen_ScientificName) %>%
  distinct() %>% arrange(Gen_ScientificName) %>% as_vector() %>% unname() -> genus_list
catch_qc %>%
  select(Sp_ScientificName) %>%
  distinct() %>% arrange(Sp_ScientificName) %>% as_vector() %>% unname() -> species_list

# Get info from fishbase,
fishbase = length_weight(Genus = genus_list,
                         fields = c("Species", "a", "b", "Type")) %>%
  as_tibble() %>%
  drop_na() %>%
  mutate(Gen_ScientificName = word(Species, 1)) %>%
  rename(Sp_ScientificName = Species) %>%
  filter(Sp_ScientificName %in% species_list) %>%
  group_by(Gen_ScientificName) %>%
  # calculate mean a and b, and add up the number of obvs
  mutate(a.mean = mean(a),
         b.mean = mean(b),
         n = n())

# Save an object with the Genuses we have fishbase info for,
fishbase$Gen_ScientificName %>% unique() %>% sort() -> genus_list_onFB

# Perc freq of occurrence
catch_qc %>%
  group_by(VisitID, Gen_ScientificName) %>%
  summarise(Presence = n_distinct(Gen_ScientificName)) %>%
  ungroup() %>%
  group_by(Gen_ScientificName) %>%
  summarise(Occurrence = sum(Presence)) %>%
  mutate(Perc_Occurrence = Occurrence / n_distinct(catch_qc$VisitID) * 100,
         Gen_ScientificName = fct_reorder(as.factor(Gen_ScientificName), desc(Perc_Occurrence))) %>%
    ggplot(aes(x = fct_reorder(as.factor(Gen_ScientificName), Perc_Occurrence), y = Perc_Occurrence)) +
      geom_col(aes(fill = ifelse(Gen_ScientificName %in% genus_list_onFB,
                                 "Yes",
                                 "No"))) +
      scale_fill_manual(values = c("#f03b20", "#999999"), name = "Is data on FishBase?", breaks = c("Yes", "No")) +
      geom_text(aes(label = round(Perc_Occurrence, 1)), size = 2, hjust = 1.5) +
      coord_flip(expand = FALSE) +
      labs(x = "Genus", y = "Percent Frequency of Occurence")
ggsave("nfa_fishbase_perc-occur.png", plot = last_plot(), device = 'png', path = file.path(dir.figs))
  
# Abundance
catch_qc %>%
  group_by(Gen_ScientificName) %>%
  summarise(Abundance = sum(Count) %>% sqrt()) %>%
  mutate(Gen_ScientificName = fct_reorder(as.factor(Gen_ScientificName), Abundance)) %>%
  ggplot(aes(x = Gen_ScientificName, y = Abundance)) +
    geom_col(aes(fill = ifelse(Gen_ScientificName %in% genus_list_onFB,
                               "Yes",
                               "No"))) +
    scale_fill_manual(values = c("#f03b20", "#999999"), name = "Is data on FishBase?", breaks = c("Yes", "No")) +
    geom_text(aes(label = round(Abundance, 0), hjust = 1)) +
    coord_flip(expand = FALSE) +
    labs(x = "Genus", y = "sqrt(Abundance)")
ggsave("nfa_fishbase_sqrt-abun.png", plot = last_plot(), device = 'png', path = file.path(dir.figs))
  
