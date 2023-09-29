# -----------------------------------------------------------------------
# Title: GoA bio break - cluster analysis
# Creator: Chris Guo
# Date: 2023.09.28
# Purpose: 

# Notes -------------------------------------------------------------------

## 


# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(cluster)

# Define workflow paths ---------------------------------------------------

wd = getwd()
dir.output = file.path(wd, "output")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd,"data")
dir.scripts = file.path(wd,"scripts")

# Read in data ------------------------------------------------------------

source(file = file.path(dir.scripts, "FishAtlas_1_events-wrangle.R"))
source(file = file.path(dir.scripts, "FishAtlas_2_catch-wrangle.R"))
source(file = file.path(dir.scripts, "FishAtlas_3_visits-wrangle.R"))
source(file = file.path(dir.scripts, "utility.R"))



