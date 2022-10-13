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



