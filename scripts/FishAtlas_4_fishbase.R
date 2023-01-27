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



