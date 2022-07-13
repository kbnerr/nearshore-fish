# -----------------------------------------------------------------------
# Title: Exploratory data analysis of NOAA's Nearshore Fish Atlas
# Creator: Chris Guo
# Date: 2021.11.07
# Purpose: Data query and EDA of NOAA's nearshore fish atlas

# Notes -------------------------------------------------------------------

# Each 'EventID' corresponds to a unique seine pull...
## It'd be nice if we could group seines that were close in time at the same location.

# Some data issues that may need to be resolved at some point:
events.RegNA # missing regional info -> let Darcie know 2022.07.11
events.PosIssues # SiteID's that are flagged for either none, bad, or averaged position data

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

# Create
# dir.create(dir.figs, recursive=TRUE)

# Read in data ------------------------------------------------------------

# The bulk of the data:
catch = read.csv(file.path(dir.data, "FishAtlasExpansion_2022.04.04_catch.csv"), header = TRUE)
events = read.csv(file.path(dir.data, "FishAtlasExpansion_2022.04.04_events.csv"), header = TRUE)
sites = read.csv(file.path(dir.data, "FishAtlasExpansion_2022.04.04_sites.csv"), header = TRUE)

# Lookup codes for subsetting the data:
lookup.Species = read.csv(file.path(dir.data, "FishAtlasExpansion_2022.04.04_lookup-SpCodes.csv"), header = TRUE)
lookup.Famliy = read.csv(file.path(dir.data, "FishAtlasExpansion_2022.04.04_lookup-FamCodes.csv"), header = TRUE)
lookup.Project = read.csv(file.path(dir.data, "FishAtlasExpansion_2022.04.04_lookup-Project.csv"), header = TRUE)

# Let's make a dataframe that we can explore and filter for the areas and projects that we want:
events.1 = events %>% left_join(., sites, by = "SiteID")


# Looking at all sites and events AK wide ---------------------------------

head(events.1)
# We've got lots of information here. Might not need all of it.
# Let's first see what regions we are working with:
events.1$Region %>% unique()
events.1$RegionCode %>% unique()
# NA's are odd at this broad level, so let's see which events these are...
events.RegNA = events.1[which(is.na(events.1$RegionCode)), ]
events.RegNA %>% View()
# Looks like the event and site data from two USGS projects are the cause of this.
# Let's export and let Darcie know...
write.csv(events.NA, file = file.path(dir.output, "Missing-Info.csv"))

## Let's pick the variables that could be useful and do some cleaning/ordering:
events.2 = events.1 %>%
  select(SiteID, EventID, RawEvent,
         Date = Date_,
         Lat, Lon = Long, Region, Location = FA.Locale,
         NoPosition, BadPosition, AveragedPos,
         Habitat,
         TidalStage,
         Temperature, Salinity,
         GearBasic, GearSpecific,
         Comments.event = Comments.x, Comments.site = Comments.y,
         ProjectName)

## Okay, let's go thru some of these variables and start filtering our data...

## First, we'll filter out any data that aren't beach seines, and fix the date format:
events.3 = events.2 %>%
  filter(GearBasic == 'beach seine') %>%
  separate(Date, into = c('Date', 'drop'), sep = ' ') %>%
  mutate(Date = ymd(Date)) %>%
  select(-GearBasic, -drop) # clean up
unique(events.3$GearSpecific) # We'll keep this so we can factor in gear type down the road

## Next, let's check the position information to see if there are any issues to deal with:

filter(events.3, NoPosition == TRUE) %>% View()
# Looks like most events without a position are from Katharine's project, plus one event from Von Biela.

filter(events.3, BadPosition == TRUE) %>% View()
# Many of these also seem to be Katharine's data, but there is one entry from the NFA Synthesis dataset

filter(events.3, AveragedPos == TRUE) %>% View()
# So there are lots of events from two of Vollenweider's Arctic datasets.

# We'll set aside the project info for these position issues to be looked into later:
events.PosIssues = events.3 %>% # Keep events naming convention but know this is actually at the site level
  filter(NoPosition == TRUE | BadPosition == TRUE | AveragedPos == TRUE) %>%
  select(SiteID, ProjectName) %>%
  left_join(lookup.Project, by = 'ProjectName') %>% 
  distinct()

# For now, we can just remove the obs with bad/missing postion data:
events.4 = filter(events.3, NoPosition == FALSE | BadPosition == FALSE)
# Removed 17 obs ^

## Lastly, let's just read out any comments for issues that we may not be aware of:
unique(events.4$Comments.event) # event level 
# Mostly erroneous habitat and site info,
# but there are some issues where seines were noted as badly set -> these should be removed for formal analyses

unique(events.4$Comments.site) # site level 
# Some of these are the same as event comments -> should double check these for bad sets to be removed

## Now let's make a map of all the beach seine events
map.ak = get_map(location = geocode("Alaska"), source = "google", maptype = "satellite", crop = FALSE, zoom = 4)
ggmap(map.ak) +
  geom_point(data = events.4, aes(x = Lon, y = Lat, color = Region))  +
  labs(title = "Event locations Alaska-wide")

## Making histograms of how the seines subset
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





events.4[which(is.na(events.3$Location)),] %>% View()






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

