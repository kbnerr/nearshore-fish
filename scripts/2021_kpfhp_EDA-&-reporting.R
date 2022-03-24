# -----------------------------------------------------------------------
# Title: 2021 KPFHP final report
# Creator: Chris Guo
# Date: March 24, 2022
# Purpose: For exploratory data analysis and to generate figures/tables for data collected under the  2021 KPFHP project.

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
dir.R = file.path(wd,"R")

# Utility -----------------------------------------------------------------

register_google(key = api.google, write = TRUE)

# Read in data ------------------------------------------------------------

# Data collected
eci.site = read_csv(file = file.path(dir.data, "2021_kpfhp_site.csv"))
eci.fish = read_csv(file = file.path(dir.data, "2021_kpfhp_fish.csv"))
eci.env = read_csv(file = file.path(dir.data, "2021_kpfhp_env.csv"))

# Supplementary data
species_list = read_csv(file = file.path(dir.data, "species-list.csv"))

# Locations for constructing maps
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
kenai.lowlands = tibble(lon = -150.992202,
                        lat = 59.914303,
                        label = 'Kenai \n Lowlands') 

# For sediment site maps
sediments = read_csv(file = file.path(dir.data, "2021_kpfhp_sediments.csv")) %>%
  unite(col = Location, Transect:Location, sep = '.')
anchor.point = sites[1,]
plumb.bluff = sites[2,]
ninilchik = sites[3,]


# Exploratory data analysis -----------------------------------------------

## Water quality
summary(eci.env)
anyNA(eci.env)
cor(eci.env$DO_mgL, eci.env$DO_perc)
# We should probably drop either DO_mgL or DO_perc
# Going to drop DO_perc although I don't have a strong reason for this.

## Fish community
summary(eci.fish)
eci.fish %>%
  filter(!is.na(LifeStage)) %>%
  select(Common, LifeStage) %>%
  distinct()
# ^There are a couple handful of cases where we can designate lifestages,
# For the NA, we dont know if they were Juvenile or Adult, but likely not YOY.
# We should replace LifeStage missing values with 'Juvenile/Adult'


# Water quality -----------------------------------------------------------

# Adding site and date to environmental data:
env.plot = select(eci.site, SeineID, Site, Date) %>%
  left_join(eci.env, ., by = "SeineID") %>%
  relocate(Site, Date, .after =  1) %>%
  select(-DO_perc) %>%
  mutate(Date = mdy(Date),
         Site = factor(Site, levels = c('Anchor Point', 'Plumb Bluff', 'Ninilchik')))

# Create a figure of our WQ measurements:
plot.T = select(env.plot, Site, Date, Temperature_C) %>%
  group_by(Date) %>%
  mutate(mean = mean(Temperature_C),
         sd = sd(Temperature_C)) %>%
  select(-Temperature_C) %>%
  distinct() %>%
ggplot(data = ., aes(x = Date, y = mean)) +
  geom_line(aes(linetype = Site)) +
  geom_linerange(aes(ymin = mean - sd, ymax = mean + sd, linetype = Site),
                 alpha = .8) +
  labs(y = 'Temperature (°C)', x = "") +
  theme_bw()

plot.Sal = select(env.plot, Site, Date, Salinity) %>%
  group_by(Date) %>%
  mutate(mean = mean(Salinity),
         sd = sd(Salinity)) %>%
  select(-Salinity) %>%
  distinct() %>%
  ggplot(data = ., aes(x = Date, y = mean)) +
  geom_line(aes(linetype = Site)) +
  geom_linerange(aes(ymin = mean - sd, ymax = mean + sd, linetype = Site),
                 alpha = .8) +
  labs(y = 'Salinity') +
  theme_bw()

plot.DO = select(env.plot, Site, Date, DO_mgL) %>%
  group_by(Date) %>%
  mutate(mean = mean(DO_mgL),
         sd = sd(DO_mgL)) %>%
  select(-DO_mgL) %>%
  distinct() %>%
  ggplot(data = ., aes(x = Date, y = mean)) +
  geom_line(aes(linetype = Site)) +
  geom_linerange(aes(ymin = mean - sd, ymax = mean + sd, linetype = Site),
                 alpha = .8) +
  labs(y = 'Oxygen saturation (mg/L)', x = "") +
  theme_bw()

plot.Turb = select(env.plot, Site, Date, Turbidity_FNU) %>%
  group_by(Date) %>%
  mutate(mean = mean(Turbidity_FNU),
         sd = sd(Turbidity_FNU)) %>%
  select(-Turbidity_FNU) %>%
  distinct() %>%
  ggplot(data = ., aes(x = Date, y = mean)) +
  geom_line(aes(linetype = Site)) +
  geom_linerange(aes(ymin = mean - sd, ymax = mean + sd, linetype = Site),
                 alpha = .8) +
  labs(y = 'Turbidity (FNU)') +
  theme_bw()

select(env.plot, Site, Date, Turbidity_FNU) %>%
  group_by(Date) %>%
  mutate(mean = mean(log(Turbidity_FNU)),
         sd = sd(log(Turbidity_FNU))) %>%
  select(-Turbidity_FNU) %>%
  distinct() %>%
  ggplot(data = ., aes(x = Date, y = mean)) +
  geom_line(aes(linetype = Site)) +
  geom_linerange(aes(ymin = mean - sd, ymax = mean + sd, linetype = Site),
                 alpha = .8) +
  labs(y = 'Turbidity (log[FNU])') +
  theme_bw()

env.fig = ggarrange(plot.T, plot.DO,
                    plot.Sal, plot.Turb,
                    nrow = 2, ncol = 2,
                    labels = "AUTO",
                    legend = "bottom", common.legend = TRUE)

# Export the figure:
ggsave(plot = env.fig,
       filename = "2021_kpfhp_WQ-measurements.png", path = file.path(dir.figs),
       width = 10.0, height = 6.5, dpi = 'print')


# Fish community ----------------------------------------------------------

unique(eci.fish$Notes)
# ^No information here needs to be addressed, so drop 'Notes'

# Add Site/Date info to fish data, clean up vars, and handle missing vals in LifeStage
fish.1 = select(eci.site, SeineID, Site, Date) %>%
  left_join(eci.fish, ., by = 'SeineID') %>%
  relocate(Site, Date, .after = 1) %>%
  select(-c(PSP, Deceased, Notes)) %>%
  mutate(LifeStage = case_when(LifeStage == 'YOY' ~ 'YOY',
                               LifeStage == 'Juvenile' ~ 'J',
                               LifeStage == 'Adult' ~ 'A',
                               is.na(LifeStage) ~ 'J/A'),
         LifeStage = factor(LifeStage, levels = c('YOY', 'J', 'J/A', 'A')))

# Calculate counts by species and life stage:
fish.counts = fish.1 %>%
  group_by(Common, LifeStage) %>%
  summarise(Count = sum(Count))

# Calculate avg, min, max length by species and lifestage:
fish.sizes = fish.1 %>%
  filter(!is.na(Length_mm)) %>% # Remove obs without length measurements
  group_by(Common, LifeStage) %>%
  summarise(n = n(),
            Length.mean = round(mean(Length_mm, na.rm = TRUE), digits = 0),
            Length.min = min(Length_mm, na.rm = TRUE),
            Length.max = max(Length_mm, na.rm = TRUE)) %>%
  mutate(Length.min = if_else(n == 1,
                              NaN,
                              Length.min),
         Length.max = if_else(n == 1,
                              NaN,
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
  mutate(n = replace_na(n, '0'),
         n = paste("(", n, ")", sep = ''),
         Length.mean = replace_na(Length.mean, '--'),
         Length.min = replace_na(Length.min, ''),
         Length.min = paste("(", Length.min, sep = ''),
         Length.max = replace_na(Length.max, ''),
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
write_csv(x = fish.tbl, file = file.path(dir.figs, "2021_kpfhp_spp-counts-lengths.csv"))


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
  geom_text(data = kenai.lowlands, aes(x = lon, y = lat, label = label),
            size = 4, fontface = 'bold', hjust = 0, nudge_x = -0.1) +
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



          