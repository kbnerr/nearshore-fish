# -----------------------------------------------------------------------
# Title: Exploratory data analysis of NOAA's Nearshore Fish Atlas
# Creator: Chris Guo
# Date: 2022.07.12
# Purpose: Data exploration of NOAA's nearshore fish atlas.
# Updated from a script working with an version of the database (.accdb ca 2021).

# Notes -------------------------------------------------------------------

## Shorezone/FishAtlas web link: https://alaskafisheries.noaa.gov/mapping/sz/

# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggmap)
register_google(key = api.google, write = TRUE)
library(lubridate)
library(RColorBrewer)

# Define workflow paths ---------------------------------------------------

wd = getwd()
dir.output = file.path(wd, "output")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd,"data")
dir.R = file.path(wd,"R")

# Read in data ------------------------------------------------------------

# Downloaded directly from the Fish Atlas website:
data = read.csv(file.path(dir.data, "FishAtlas_BeachSeines_2022.07.12.csv"), header = TRUE)


# Initial subset of data into events and catch ----------------------------

head(data)
colnames(data)
# We've got lots of information here. Might not need all of it.

## Let's do an initial cleaning/ordering of vars we might want,
# by splitting the data into event-level and catch-level:
events.1 = data %>%
  select(SiteID, EventID,
         Date,
         Lat, Lon = Long, Region, Location,
         Habitat, TidalStage, Temp_C, Salinity,
         GearSpecific, ProjectName, PointOfContact) %>%
  mutate(Date = mdy(Date)) %>% # re-format date
  distinct()

catch.1 = data %>%
  select(SiteID, EventID,
         Sp_CommonName, Sp_ScientificName, Fam_CommonName, Fam_ScientificName,
         Unmeasured, Length_mm, LengthType, LifeStage)


# Looking at structure of event-level data --------------------------------

## See what regions/locations we are working with
events.1$Region %>% unique()
events.1$Location %>% unique() # that's a lot... but could be useful to group seines by visit

## We also have different gear types used by different projects
events.1$GearSpecific %>% unique()
events.1$ProjectName %>% unique()

## Now let's make a map of all the beach seine events
map.ak = get_map(location = geocode("Alaska"), source = "google", maptype = "satellite", crop = FALSE, zoom = 4)
ggmap(map.ak) +
  geom_point(data = events.1, aes(x = Lon, y = Lat, color = Region))  +
  labs(title = "Event locations by Region")

## Making histograms of how the events subset
# New df adding different time vars
events.5 = events.4 %>%
  mutate(Month = month(Date, label = TRUE),
         Year = year(Date),
         Day = yday(Date))

# Seines by Year
ggplot(data = events.5, aes(x = Year)) +
  geom_histogram(aes(fill = Region), stat = "count") +
  stat_count(aes(y = ..count.., label = ..count..), geom = "text", vjust=-.5) +
  labs(title = "Events by Year")

# Seines by Month
ggplot(data = events.5, aes(x = Month)) + 
  geom_histogram(aes(fill = Region), stat = "count") +
  stat_count(aes(y = ..count.., label = ..count..), geom = "text", vjust=-.5) +
  labs(title = "Events by Month")

# Seines by Day of year
ggplot(data = events.5, aes(x = Day)) +
  geom_histogram(aes(fill = Region), stat = "count") +
  labs(title = "Events by Day of Year")


# Seines by Region
ggplot(data = events.5, aes(x = Region)) +
  geom_histogram(aes(fill = Region), stat = "count") +
  stat_count(aes(y = ..count.., label = ..count..), geom = "text", vjust=-.5) +
  labs(title = "Events by Region")

# Seines by Habitat
ggplot(data = events.5, aes(x = Habitat)) +
  geom_histogram(aes(fill = Habitat), stat = "count") +
  stat_count(aes(y = ..count.., label = ..count..), geom = "text", vjust=-.5) +
  labs(title = "Events by Habitat")

# Seines by Project
ggplot(data = events.5, aes(x = ProjectName)) +
  geom_histogram(aes(fill = ProjectName), stat = "count") +
  stat_count(aes(y = ..count.., label = ..count..), geom = "text", vjust=-.5) +
  labs(title = "Events by Project")


# Looking at structure of catch-level data --------------------------------

## We can check the fish data to check for any glaring issues:
catch.1$Fam_CommonName %>% unique()
# Looks like there are some missing family data
catch.1 %>% slice(which(data.1$Fam_CommonName == ''))
# I see- ^these are really small forage fish so the sampler didn't know what family to assign

catch.1$Sp_CommonName %>% unique()
# Hmm.. I foresee some issues at this level parsing out 'unidentifieds' and 'juveniles'

# new df with col for actual count, called 'Measured':
tmp1 = mutate(data,
              Measured = if_else(data$Unmeasured < 1,
                                 1,
                                 as.double(data$Unmeasured)),
              Date = mdy(Date),
              Week = week(Date),
              Month = month(Date, label = TRUE))

sort(unique(tmp1$SpCode))

tmp2 = tmp1 %>%
  filter(SpCode == "SANDLNCP") %>% # try: CODPAC, CODTOM, SALPINK, SALCOHO, DOLLY, SCULGRT, CAPELIN, HERRING, SANDLCP, HALIBUT, CRABDUN
  group_by(Month) %>%
  mutate(n = n_distinct(Length_mm)) %>%
  filter(n > 50)

tmp2  %>%
  ggplot(data = ., aes(x = Month, y = Measured)) + 
  geom_col() +
  labs(title = tmp2$Sp_CommonName[1])

tmp2 %>%
  ggplot(data = ., aes(x = Length_mm)) + 
  geom_freqpoly(aes(color = Month), size = 1, bins = 40) +
  labs(title = tmp2$Sp_CommonName[1]) +
  theme(panel.background = element_rect(fill = "grey25"),
        legend.key = element_rect(fill = "grey35"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "grey45")) +
  facet_wrap( ~ Month, ncol = 1)

tmp2 %>%
  filter(Length_mm < 200) %>%
  mutate(Month = month(Date, label = TRUE)) %>%
  ggplot(data = ., aes(x = Length_mm)) + 
  geom_freqpoly(aes(color = Month), size = 1, bins = 40) +
  labs(title = paste(tmp2$Sp_CommonName[1], "<200 mm")) +
  theme(panel.background = element_rect(fill = "grey25"),
        legend.key = element_rect(fill = "grey35"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "grey45")) +
  facet_wrap( ~  Month, ncol = 1)

