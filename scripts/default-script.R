# -----------------------------------------------------------------------
# Title: Nearshore fish default script
# Creator: Chris Guo
# Date: 
# Purpose: 

# Notes -------------------------------------------------------------------


# Load packages -----------------------------------------------------------


# Define workflow paths ---------------------------------------------------

wd = getwd()
dir.output = file.path(wd, "output")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd,"data")
dir.R = file.path(wd,"R")
dir.reports = file.path(wd,"reports")
dir.nfa = file.path(wd,"noaa-nfa")
dir.permits = file.path(wd,"permits")

# Utility -----------------------------------------------------------------


# Read in data ------------------------------------------------------------

