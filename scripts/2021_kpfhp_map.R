# -----------------------------------------------------------------------
# Title: 2021 KPFHP map of study area and sites
# Creator: Chris Guo
# Date: March 7, 2022 
# Purpose: Figures for final report

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
dir.data = file.path(wd, "data")
dir.R = file.path(wd, "R")

# Utility -----------------------------------------------------------------

register_google(key = api.google, write = TRUE)

# Read in data ------------------------------------------------------------

# For the map of the study area and sites
loc.studyarea = c(lon = -151.749292, lat = 59.921010)
sites = tibble(lon = c(-151.870914, -151.761859, -151.674213),
               lat = c(59.770073, 59.912396, 60.051749),
               label = c('Anchor Point', 'Plumb Bluff', 'Ninilchik'))
kachemak.bay = tibble(lon = -151.663442,
                      lat = 59.560501,
                      label = 'Kachemak Bay')
cook.inlet = tibble(lon = -152.439149,
                    lat = 59.744073,
                    label = 'Cook Inlet')

# For the maps of the sample locations at each site
sediments = read_csv(file = file.path(dir.data, "2021_kpfhp_sediments.csv")) %>%
  unite(col = Location, Transect:Location, sep = '.')
anchor.point = sites[1,]
plumb.bluff = sites[2,]
ninilchik = sites[3,]

# Study area and sites ----------------------------------------------------

# Figure map of study area
x.1 = get_map(location = loc.studyarea,
              source = "stamen", maptype = "toner-lite",
              crop = FALSE, zoom = 9)

fig.studyarea = ggmap(x.1) +
  geom_point(data = sites, aes(x = lon, y = lat),
             color = "black", size = 3, shape = 19, alpha = .7) +
  geom_text(data = sites, aes(x = lon, y = lat, label = label),
            size = 2, hjust = 0, nudge_x = 0.05) +
  geom_text(data = kachemak.bay, aes(x = lon, y = lat, label = label),
            size = 2.5, fontface = 'bold') +
  geom_text(data = cook.inlet, aes(x = lon, y = lat, label = label),
            size = 3.25, fontface = 'bold') +
  labs(x = 'Longitude', y = 'Latitude')

ggsave(plot = fig.studyarea,
       filename = "2021_kpfhp_studyarea.png", path = dir.figs,
       width = 10, height = 6, units = 'in', dpi = 'print')
       
# Figure maps of sample locations per site:

# Anchor Point
x.2 = get_map(location = c(anchor.point$lon, anchor.point$lat),
              source = "google", maptype = "satellite",
              crop = FALSE, zoom = 16)
fig.AP = ggmap(x.2) +
  geom_point(data = filter(sediments, Site == 'Anchor Point'),
             aes(x = Longitude, y = Latitude),
             color = "red", size = 1, shape = 19, alpha = .8) +
  geom_text(data = filter(sediments, Site == 'Anchor Point'),
            aes(x = Longitude, y = Latitude, label = Location),
            size = 3, color = "red", hjust = -0.05, vjust = -0.25) +
  labs(x = '', y = 'Latitude')

# Plumb Bluff
x.3 = get_map(location = c(plumb.bluff$lon, plumb.bluff$lat),
              source = "google", maptype = "satellite",
              crop = FALSE, zoom = 16)
fig.PB = ggmap(x.3) +
  geom_point(data = filter(sediments, Site == 'Plumb Bluff'),
             aes(x = Longitude, y = Latitude),
             color = "red", size = 1, shape = 19, alpha = .8) +
  geom_text(data = filter(sediments, Site == 'Plumb Bluff'),
            aes(x = Longitude, y = Latitude, label = Location),
            size = 3, color = "red", hjust = -0.1, vjust = 0) +
  labs(x = 'Longitude', y = '')

# Ninilchik
x.4 = get_map(location = c(ninilchik$lon, ninilchik$lat),
              source = "google", maptype = "satellite",
              crop = FALSE, zoom = 16)
fig.NR = ggmap(x.4) +
  geom_point(data = filter(sediments, Site == 'Ninilchik'),
             aes(x = Longitude, y = Latitude),
             color = "red", size = 1, shape = 19, alpha = .8) +
  geom_text(data = filter(sediments, Site == 'Ninilchik'),
            aes(x = Longitude, y = Latitude, label = Location),
            size = 3, color = "red", hjust = -0.2, vjust = 0) +
  labs(x = 'Longitude', y = 'Latitude')

fig.sediments = ggarrange(fig.AP, fig.PB,
                          fig.NR, "",
                          nrow = 2, ncol = 2,
                          labels = c('A. Anchor Point',
                                     'B. Plumb Bluff',
                                     'C. Ninilchik',
                                     'D. Beach cross-section'))

# Combine and save
ggsave(plot = fig.sediments,
       filename = "2021_kpfhp_sediments.png", path = dir.figs,
       width = 10.5, height = 10.5, units = 'in', dpi = 500)



