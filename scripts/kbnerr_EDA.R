# -----------------------------------------------------------------------
# Title: Exploration of nearshore fish data - all years
# Creator: Chris Guo
# Date: April 5, 2022
# Purpose: Script to explore patterns in data from all years

# Notes -------------------------------------------------------------------


# Load packages -----------------------------------------------------------
library(tidyverse)
library(lubridate)
library(ggmap)
library(ggrepel)
library(ggpubr)

# Define workflow paths ---------------------------------------------------

wd = getwd()
dir.output = file.path(wd, "output")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd,"data")
dir.R = file.path(wd, "R")

# Utility -----------------------------------------------------------------

register_google(key = api.google, write = TRUE)
source(file = "../Desktop/Rlocal/scripts/utility.R")

# Read in data ------------------------------------------------------------

# Data collected
site.22 = read_csv(file = file.path(dir.data, "2022_kbnerr_site.csv"))
fish.22 = read_csv(file = file.path(dir.data, "2022_kbnerr_fish.csv"))
env.22 = read_csv(file = file.path(dir.data, "2022_kbnerr_env.csv"))
site.21a = read_csv(file = file.path(dir.data, "2021_kpfhp_site.csv"))
fish.21a = read_csv(file = file.path(dir.data, "2021_kpfhp_fish.csv"))
env.21a = read_csv(file = file.path(dir.data, "2021_kpfhp_env.csv"))
site.21b = read_csv(file = file.path(dir.data, "2021_nerrsc_site.csv"))
fish.21b = read_csv(file = file.path(dir.data, "2021_nerrsc_fish.csv"))
env.21b = read_csv(file = file.path(dir.data, "2021_nerrsc_env.csv"))
site.19 = read_csv(file = file.path(dir.data, "2019_kpfhp_site.csv"))
env.19 = read_csv(file = file.path(dir.data, "2019_kpfhp_env.csv"))
fish.19 = read_csv(file = file.path(dir.data, "2019_kpfhp_fish.csv"))
site.18 = read_csv(file = file.path(dir.data, "2018_cmi-kpfhp_site.csv"))
fish.18 = read_csv(file = file.path(dir.data, "2018_cmi-kpfhp_fish.csv"))
env.18 = read_csv(file = file.path(dir.data, "2018_cmi-kpfhp_env.csv"))

# Supplementary data
species_list = read_csv(file = file.path(dir.data, "species-list.csv"))
sites_list = read_csv(file = file.path(dir.data, "sites-list.csv"))

# Combine data ------------------------------------------------------------

## Sites
# Note: the only df that seems to differ in columns is 2019:
site.all = bind_rows(site.18, site.19, site.21a, site.21b, site.22) %>%
  mutate(Date = mdy(Date))

## Environmentals
env.all = bind_rows(env.18, env.19, env.21a, env.21b, env.22)

## Fish
fish.all = bind_rows(fish.18, fish.19, fish.21a, fish.21b, fish.22)


# Simple maps -------------------------------------------------------------

map.center = c(-152.112897, 58.823421) # location is for the catalyst data report map

sites = c(unique(site.21a$Site), unique(site.21b$Site)) # find only sites reported under the nerr sc project

# Create a tibble with site lat/lon
map.dat = site.all %>%
  filter(Site %in% sites) %>%
  select(Site, Latitude, Longitude) %>%
  group_by(Site) %>%
  summarise(lat = mean(Latitude),
            lon = mean(Longitude))

# generate map
map = get_map(location = map.center,
              source = "google", maptype = "terrain",
              crop = FALSE, zoom = 7)

# add site points
map.print = ggmap(map) +
  geom_point(data = map.dat, aes(x = lon, y = lat),
             color = "black", size = 3, shape = 19, alpha = .7) +
  labs(x = 'Longitude', y = 'Latitude')

map.print

# save as an image file
ggsave(plot = map.print,
       filename = "2021_nerrsc_data-map.png", path = dir.output,
       width = 10, height = 6, units = 'in', dpi = 'print')

# Pacific Sand Lance ------------------------------------------------------

# Let's look at PSL from East Cook Inlet for now:
psl.1 = filter(fish.all, Common == 'Pacific Sand Lance') %>%
  left_join(site.all, by = "SeineID") %>%
  left_join(select(sites_list, Site, Area), by = "Site") %>% 
  mutate(Date = mdy(Date)) %>%
  filter(Area == 'East Cook Inlet') %>%
  select(Common, SeineID, Site, Date, LifeStage, Count, Length_mm)

# Looks like all ECI PSL are from 2021 except 1...

psl.2 = filter(psl.1, Date > '2021-03-30') # Removing the one 2019 PSL

# Some summary stats:
sum(psl.2$Count)
psl.mean.TL = mean(psl.2$Length_mm, na.rm = T)
  
# Plots shared with Yumi/Lindsay USGS
psl.2 %>%
  group_by(Site) %>%
  summarise(Count = sum(Count)) %>%
  ggplot(data = ., aes(x = Site, y = Count)) +
  geom_col() + labs(title = "1. PSL count by site")

ggplot(data = psl.2, aes(x = Length_mm)) +
  geom_bar() +
  facet_grid(. ~ Site) +
  labs(title = "2. PSL size freq by site")

ggplot(data = psl.2, aes(x = Length_mm)) +
  geom_bar() +
  geom_vline(xintercept = psl.mean.TL, color = 'red', linetype = 2) +
  labs(title = "3. PSL size freq with mean")

ggplot(data = psl.2, aes(x = Length_mm)) +
  geom_bar() +
  facet_grid(. ~ month(Date)) +
  labs(title = "4. PSL size freq by month")

# Coowe request 7/19/22 ---------------------------------------------------

# How many of juvenile salmon were there from 2021 and 2022
kenai.lowlands = c("Anchor Point",
                   "Ninilchik",
                   "Plumb Bluff")
salmonids = c("Chinook Salmon",
              "Coho Salmon",
              "Sockeye Salmon",
              "Chum Salmon",
              "Pink Salmon",
              "Dolly Varden")

site.all %>%
  filter(Date > '2021-01-01') %>%
  select(SeineID, Site, Date) %>%
  filter(Site %in% kenai.lowlands) %>%
  left_join(fish.all, by = "SeineID") %>%
  filter(Common %in% salmonids) %>%
  select(-c(LifeStage, PSP, Deceased, Notes, Lavage, Sacrificed, `Fish Tank`)) %>%
  group_by(Site, Date, Common) %>%
  summarise(Total.Count = sum(Count),
            Avg.Length = mean(Length_mm) %>% round(., 0)) %>%
  arrange(Common, Site, Date) %>%
  relocate(Common, .before = 1) %>%
  write.csv(x = ., file = file.path(dir.output, "2021-22_salmonids.csv"), row.names = FALSE)

