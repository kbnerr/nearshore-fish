# -----------------------------------------------------------------------
# Title: Exploratory data analysis of NOAA's Nearshore Fish Atlas
# Creator: Chris Guo
# Date: 2021.11.07
# Purpose: Data query and EDA of NOAA's nearshore fish atlas

# Notes -------------------------------------------------------------------

# Each 'EventID' corresponds to a unique seine pull...
## It'd be nice if we could group seines that were close in time at the same location.
## 

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
catch = read.csv(file.path(dir.data, "Data_Catch.csv"), header = TRUE)
events = read.csv(file.path(dir.data, "Data_Events.csv"), header = TRUE)
sites = read.csv(file.path(dir.data, "Data_Sites.csv"), header = TRUE)
lookup.locale = read.csv(file.path(dir.data, "Lookup_Locale.csv"), header = TRUE)
lookup.region = read.csv(file.path(dir.data, "Lookup_Region.csv"), header = TRUE)

events.plus = events %>% 
  left_join(., sites, by = "SiteID") %>%
  left_join(., lookup.locale, by = "LocaleID") %>%
  left_join(., lookup.region, by = "RegionID")

catch.plus = left_join(catch, events, by = "EventID") %>%
  left_join(., sites, by = "SiteID") %>%
  left_join(., lookup.locale, by = "LocaleID") %>%
  left_join(., lookup.region, by = "RegionID")

# AK-wide events & catch --------------------------------------------------

str(events.plus)
events.plus$Region.x %>% unique()
events.plus$RegionCode %>% unique() # NAs...
events.plus[which(is.na(events.plus$RegionCode)), ] %>% View() # These are my kbay data...

map.ak = get_map(location = geocode("Alaska"), source = "google", maptype = "satellite", crop = FALSE, zoom = 4)
ggmap(map.ak) +
  geom_point(data = events.plus, aes(x = Long1, y = Lat1), color = "red")

tmp.plot = events.plus %>%
  mutate(Month = month(Date, label = TRUE),
         Year = year(Date),
         Day = yday(Date))
  
ggplot(data = tmp.plot, aes(x = Month)) + 
  geom_histogram(aes(fill = RegionCode), stat = "count") +
  labs(title = "All events")

ggplot(data = tmp.plot, aes(x = Year)) +
  geom_histogram(aes(fill = RegionCode), stat = "count")

ggplot(data = tmp.plot, aes(x = Day)) +
  geom_histogram(aes(fill = RegionCode), stat = "count")

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


#### Southcentral AK ####

events.scak = events.plus %>%
  filter(RegionCode %in% c("PWS", "SCAK")) %>%
  filter(GearBasic == "beach seine") %>%
  filter(DataType == "fish")

str(events.scak)
events.scak$Date %>% range() # dates from 1999 to 2012... not accurate
events.scak$Temp # Temperature measurements
events.scak$Salinity # Salinity measurements
events.scak$Habitat %>% unique() # Habitat class (eelgrass, kelp. bedrock, sand-gravel)
events.scak$Location %>% unique() # unique locations
events.scak$SubLocale %>% unique()

catch.scak = left_join(events.scak, catch, by = "EventID")
catch.scak$EventID %>% unique() %>% length() # 200 events
catch.scak$SpCode %>% unique() # unique taxa
catch.scak$Unmeasured # 'Unmeasured' counts
sum(catch.scak$Unmeasured == 0) # 8504
sum(catch.scak$Unmeasured) + 8504 # total individuals
catch.scak = mutate(catch.scak, Measured = if_else(catch.scak$Unmeasured < 1, 1, as.double(catch.scak$Unmeasured))) # actual counts: 'Measured'



map.scak = get_map(location = geocode("Seward, AK"), source = "google", maptype = "satellite", crop = FALSE, zoom = 7)
ggmap(map.scak) +
  geom_point(data = events.plus, aes(x = Long1, y = Lat1), color = "red")

#### Southwest AK ####

events.swak = events.plus %>%
  filter(RegionCode %in% c("SWAK", "BBAY", "AI")) %>%
  filter(GearBasic == "beach seine") %>%
  filter(DataType == "fish")

str(events.swak)
events.swak$Date %>% range() # dates from 1999 to 2012... not accurate
events.swak$Temp # Temperature measurements
events.swak$Salinity # Salinity measurements
events.swak$Habitat %>% unique() # Habitat class (eelgrass, kelp. bedrock, sand-gravel)
events.swak$Location %>% unique() # unique locations
events.swak$SubLocale %>% unique()

catch.swak = left_join(events.swak, catch, by = "EventID")
catch.swak$EventID %>% unique() %>% length() # 13 events
catch.swak$SpCode %>% unique() # unique taxa
catch.swak$Unmeasured # 'Unmeasured' counts
sum(catch.swak$Unmeasured == 0) # 1134
sum(catch.swak$Unmeasured) + 1134 # total individuals
catch.swak = mutate(catch.swak, Measured = if_else(catch.swak$Unmeasured < 1, 1, as.double(catch.swak$Unmeasured))) # actual counts: 'Measured'

#### Southeast AK ####

events.seak = events.plus %>%
  filter(RegionCode %in% c("SEAK")) %>%
  filter(GearBasic == "beach seine") %>%
  filter(DataType == "fish")

str(events.seak)
events.seak$Date %>% range() # dates from 1999 to 2014
events.seak$Temp # Temperature measurements
events.seak$Salinity # Salinity measurements
events.seak$Habitat %>% unique() # Habitat class (eelgrass, kelp. bedrock, sand-gravel)
events.seak$Location %>% unique() # unique locations
events.seak$SubLocale %>% unique()

catch.seak = left_join(events.seak, catch, by = "EventID")
catch.seak$EventID %>% unique() %>% length() # 931 events
catch.seak$SpCode %>% unique() # unique taxa
catch.seak$Unmeasured # 'Unmeasured' counts
sum(catch.seak$Unmeasured == 0, na.rm = TRUE) # 42149 after NAs removed
sum(catch.seak$Unmeasured) + 42149 # total individuals
catch.seak = mutate(catch.seak, Measured = if_else(catch.seak$Unmeasured < 1, 1, as.double(catch.seak$Unmeasured))) # actual counts: 'Measured'

#### OLD ####
scak = filter(dat, RegionID == 5) # filter for southcentral alaska

tmp = scak %>%
  dplyr::select(EventID,
                Date,
                GearSpecific,
                GearBasic,
                DayNight,
                Temp,
                Salinity,
                Locale = Locale.x,
                Location,
                SubLocale,
                Habitat) %>%
  distinct()

scak %>%
  filter(GearBasic == "beach seine") %>%
  select(EventID,
         SpCode) %>%
  distinct() %>%
  mutate(P = 1) %>%
  pivot_wider(names_from = SpCode,
              values_from = P,
              values_fill = 0)
