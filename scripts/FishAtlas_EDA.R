# -----------------------------------------------------------------------
# Title: Exploratory data analysis of NOAA's Nearshore Fish Atlas
# Creator: Chris Guo
# Date: 2022.07.12
# Purpose: Data exploration of NOAA's nearshore fish atlas.
# Updated from a script working with an version of the database (.accdb ca 2021).

# Notes -------------------------------------------------------------------

## Shorezone/FishAtlas web link: https://alaskafisheries.noaa.gov/mapping/sz/

## Each 'EventID' corresponds to a unique seine pull...
# It'd be nice if we could group seines that were close in time at the same location.


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

## We also have different gear tyoes used by different projects
events.1$GearSpecific %>% unique()
events.1$ProjectName %>% unique()

# I am unsure if all project grouped their Site/Event information similarly...
# IOW, I don't know if samplers would all consider a 'site' the same thing.
# So I think we need to define what a SiteID is:
# SiteID = a unique site visit, i.e., all samples of the same 'beach' on the same day

# Let's check what we have...
# This is the number of EventID's per SiteID and Date:
events.1 %>%
  group_by(SiteID, Date) %>%
  summarise(events = n_distinct(EventID))
# And this is the number of observations per Date and Location:
events.1 %>%
  group_by(Date, Location) %>%
  summarise(events = n())
# There is ~1000 obs difference between the two, 
# And, I think that the latter is closer to the actual number of visits.
# Let's take a look at a frequency plot of these events:
events.1 %>% group_by(Date, Location) %>% summarise(events = n()) %>%
  ggplot(data = ., aes(x = events)) +
  geom_histogram(stat = 'count') +
  labs(title = "Number of events (seines) per date and location")

# However, this grouping may not be accurate because of the loose definition of 'Location'
# Let's try to subset the data into occasions where SiteID's are different,
# but the Date and Locations are telling us they should actually be the same:

library(sf)

# Convert data to sf object:
events.sf = events.1 %>%
  select(EventID, Lon, Lat) %>%
  st_as_sf(., coords = c("Lon", "Lat"), crs = 4326)

# Set aside EventID's for renaming rows/cols of distance matrix later:
events.names = events.sf$EventID

# Create a matrix of distances among all points:
events.dist.mat = st_distance(events.sf[ ,-1])

# Convert matrix to data frame and set column and row names
events.dist.df = data.frame(events.dist.mat)
rownames(events.dist.df) = events.names
colnames(events.dist.df) = events.names

# Find the closest EventID within 250 m of any EventID
events.250m = events.dist.df %>% 
  mutate(EventID = rownames(.)) %>% 
  pivot_longer(names_to = 'Closest_EventID', values_to = 'Dist_m', -EventID) %>% 
  mutate(EventID = as.integer(EventID),
         Closest_EventID = as.integer(Closest_EventID),
         Dist_m = as.numeric(Dist_m)) %>%
  filter(Dist_m > 0 & Dist_m < 250) %>%
  group_by(EventID) %>% 
  arrange(Dist_m) %>% 
  slice(1)

# Clean up the environment:
# rm(events.sf, events.dist.df, events.dist.mat, events.names)

# Subset the data for events that have another event within 250m
events.closest = events.1 %>%
  right_join(events.250m, by = "EventID") %>%
  relocate(Closest_EventID, Dist_m, .after = EventID) %>%
  select(SiteID,
         EventID,
         Closest_EventID,
         Dist_m,
         Date)

# Now we need to find cases where these 'close' events occur on the same day...







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
catch.ak = mutate(catch.plus, Measured = if_else(catch.plus$Unmeasured < 1, 1, as.double(catch.plus$Unmeasured)))

sort(unique(catch.ak$SpCode))

tmp = filter(catch.ak, SpCode == "HERRING") # try: CODPAC, CODTOM, SALPINK, SALCOHO, DOLLY, SCULGRT, CAPELIN, HERRING, SANDLCP, HALIBUT, CRABDUN

tmp %>%
  mutate(Week = week(Date),
         Month = month(Date, label = TRUE)) %>%
  ggplot(data = ., aes(x = Month, y = Measured)) + 
  geom_col() +
  labs(title = tmp$SpCode[1])

tmp %>%
  mutate(Month = month(Date, label = TRUE)) %>%
  ggplot(data = ., aes(x = Length)) + 
  geom_freqpoly(aes(color = Month), size = 1, bins = 40) +
  labs(title = tmp$SpCode[1]) +
  theme(panel.background = element_rect(fill = "grey25"),
        legend.key = element_rect(fill = "grey35"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "grey45"))

tmp %>%
  filter(Length < 200) %>%
  mutate(Month = month(Date, label = TRUE)) %>%
  ggplot(data = ., aes(x = Length)) + 
  geom_freqpoly(aes(color = Month), size = 1, bins = 40) +
  labs(title = paste(tmp$SpCode[1], "<200 mm")) +
  theme(panel.background = element_rect(fill = "grey25"),
        legend.key = element_rect(fill = "grey35"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "grey45"))

