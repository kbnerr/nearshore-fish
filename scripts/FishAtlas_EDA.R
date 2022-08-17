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

## We also have different gear types used by different projects
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
# but the Date and location are telling us they should be similar:


# Solving the Site vs Event issue -----------------------------------------

library(sf)
library(geosphere)

# Basically, we are trying to combine Events that are currently separate SiteIDs,
# but that should be the same SiteID (i.e., samples of the same day + beach).
# We'll need to figure out what a beach is...

# It looks like we only have position data at the SiteID level...
n_distinct(events.1$EventID)
n_distinct(events.1$SiteID)
events.1 %>% distinct(Lat, Lon) %>% n_distinct()
# Looks like we have more distinct SiteID's than distinct locations (not taking Date into account)

# To thorough, let's work with Site-Event combinations to find any obs that occur on the same day on the same beach:

# Convert data to sf object:
ID.sf = events.1 %>%
  select(SiteID, EventID, Lon, Lat) %>%
  unite(col = 'ID', SiteID, EventID, sep = '_', remove = TRUE) %>% # unique Site/Event ID
  distinct() %>%
  st_as_sf(., coords = c("Lon", "Lat"), crs = 4326) # matches our n_distinct(SiteID)

# Set aside ID's for renaming rows/cols of distance matrix later:
ID.names = ID.sf$ID

# Create a matrix of distances among all points:
ID.dist.mat = st_distance(ID.sf[ ,-1])

# Convert matrix to df and set column and row names
ID.dist.df = data.frame(ID.dist.mat)
rownames(ID.dist.df) = ID.names
colnames(ID.dist.df) = ID.names

# What is a 'beach'?
# One of our largest-distanced beach is Anchor Point, where we've seined along ~800m.
# So let's find SiteID's from the same day within 1000m of each other:

ID.1km.1 = ID.dist.df %>% 
  mutate(ID = rownames(.)) %>% 
  pivot_longer(names_to = 'Closest_ID', values_to = 'Dist_m', -ID) %>% 
  mutate(Dist_m = as.numeric(Dist_m)) %>%
  filter(Dist_m > 0 & Dist_m < 1000) %>%
  arrange(ID, Dist_m)  %>% # Code to filter for pairs of the same day:
  left_join(select(events.1, SiteID, EventID, Date) %>% # add dates for the 'left side' of ID pair
              unite(col = 'ID', SiteID, EventID, sep = '_', remove = TRUE),
            by = "ID") %>% 
  select(ID.a = ID,
         Date.a = Date,
         Dist_m,
         ID = Closest_ID) %>%
  left_join(select(events.1, SiteID, EventID, Date) %>% # add dates for the 'right side' of ID pair
              unite(col = 'ID', SiteID, EventID, sep = '_', remove = TRUE),
            by = "ID") %>% 
  distinct() %>% 
  rename(ID.b = ID,
         Date.b = Date) %>%
  filter(Date.a == Date.b) %>% # Code to remove duplicate pairs:
  unite(col = 'A', ID.a, Date.a, sep = '_', remove = FALSE) %>% 
  unite(col = 'B', ID.b, Date.b, sep = '_', remove = FALSE) %>%
  rowwise() %>%
  mutate(pair = paste(sort(c(A, B)), collapse = "_")) %>% distinct(pair, .keep_all = TRUE) %>%
  select(-pair, -A, -B) %>%
  arrange(ID.a, Dist_m)

# Rename the 'right side' as our ID to replace, and use the 'eft side' for the ID to replace with (ID.a):
ID.1km.2 = ID.1km.1 %>%
  select(ID = ID.b,
         Date = Date.a,
         ID.a,
         Dist_m) %>%
  arrange(ID, Date)

# When we join this to our orig df, we end up with more observations than we began with...
select(events.1, SiteID, EventID, Date) %>%
  unite(col = 'ID', SiteID, EventID, sep = '_', remove = TRUE) %>%
  left_join(., ID.1km.2, by = c('ID'))

# This means that there are duplicated obs in our 'A' dataset. Which ones:
check = ID.1km.2$ID %>% duplicated() %>% which()

ID.1km.3 = ID.1km.2[check,] %>% arrange(Date)
ID.1km.3 %>% arrange(desc(Dist_m))
# ^These are the the obs with multiple ID's that will might need to be grouped;
# however, we only know of pairs that are <1km of each other: A within 1km of B, and B within 1km of C,
# But we do not know if A is within 1km of C... and we do not really care.
# We just want to find all sites within 1km within the same day. 

ID.1km.3 %>%
  group_by(Date) %>%
  summarise(total.dist = sum(Dist_m)) %>%
  arrange(desc(total.dist))
# Here, we see dates that likely contain multiple groups, which needs to be delineated


# Hierarchical clustering approach to grouping SiteID's and Locations --------

library(sp)
library(rgdal)

# Our issue is that location info is at the SiteID level and Date information is at the EventID level.
# So not all Events belonging to a SiteID should be grouped together.. only those occurring on the same day.

# We will use a clustering approach to define a distance threshold for which sites to group.
# Code provided by Jeffrey Evans from https://gis.stackexchange.com/questions/64392/finding-clusters-of-points-based-distance-rule-using-r

# But first we need to reshape our list of Site-Event ID's...
# Let's go back to our ID.1km.1, which has all of our obs within 1000m of other obs.

# We will bind ID-Dates from both A and B sides to get a list of all Site-Date-Locations to feed our clustering algorithm
A = select(ID.1km.1,
           ID = ID.a,
           Date = Date.a) %>%
  unite(col = 'ID_Date', ID, Date, sep = '_', remove = TRUE)
B = select(ID.1km.1,
           ID = ID.b,
           Date = Date.b) %>%
  unite(col = 'ID_Date', ID, Date, sep = '_', remove = TRUE)
ID.1km.4 = bind_rows(A, B) %>%
  distinct() %>%
  arrange(ID_Date) %>%
  left_join(select(events.1, SiteID, EventID, Date, Lat, Lon) %>%
              unite(col = 'ID', SiteID, EventID, sep = '_', remove = TRUE) %>%
              unite(col = "ID_Date", ID, Date, remove = TRUE),
            by = "ID_Date")
rm(A, B)

# Extract location data and transform into projected coordinate system
lon = ID.1km.4$Lon
lat = ID.1km.4$Lat
ID_Date = ID.1km.4$ID_Date
dist = 1000 # This is using the 'furthest neighbor' algorithm,
# where clusters are defined by the 'dist' between an obs within that cluster and an obs in another cluster

xy = SpatialPointsDataFrame(matrix(c(lon, lat), ncol = 2),
                            data.frame(ID_Date = ID_Date),
                            proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

xy = spTransform(xy, CRS("+init=epsg:27700 +datum=WGS84"))

xy.clust = hclust(dist(data.frame(rownames = rownames(xy@data),
                                  x = coordinates(xy)[, 1],
                                  y = coordinates(xy)[, 2])),
                  method = "complete")

# Distance with a 1000m furthest neighbor threshold  
xy.1000m = cutree(xy.clust, h = dist)

# Join results to the sp points
xy@data = data.frame(xy@data, Cluster = xy.1000m)

# Make a new df containing the clusters with associated lat/lon's:
clust.1 = xy@data %>%
  left_join(ID.1km.4, by = 'ID_Date') %>%
  separate(ID_Date, into = c('SiteID', 'EventID', 'Date'), sep = '_') %>%
  mutate(SiteID = as.double(SiteID),
         EventID = as.double(EventID),
         Date = ymd(Date)) %>%
  relocate(Cluster, .before = 1) %>%
  arrange(Cluster, Date)

# Nest the data by Cluster (space) and Date (time):
clust.2 = clust.1 %>%
  group_by(Cluster, Date) %>%
  nest(Sites = SiteID,
       Events = EventID,
       Lats = Lat,
       Lons = Lon) %>%
  ungroup()

# Replace SiteID's with the lowest common ID number, and find mean Lat/Lon positions.
# Then unnest the data to the EventID level
clust.3 = clust.2 %>% 
  rowwise() %>%
  mutate(SiteID.clust = min(Sites),
         mean.Lat = mean(Lats$Lat),
         mean.Lon = mean(Lons$Lon)) %>%
  select(-c(Sites, Lats, Lons)) %>%
  unnest_longer(Events) %>%
  unpack(cols = Events) %>%
  select(-Cluster, -Date)

# Join the new SiteID's, Lat's, and Lon's to the original df by EventID,
# and replace the SiteID's and location info where appropriate:
events.2 = left_join(events.1, clust.3, by = c('Date', 'EventID')) %>%
  mutate(SiteID = ifelse(is.na(SiteID.clust),
                         SiteID,
                         SiteID.clust),
         Lat = ifelse(is.na(mean.Lat),
                      Lat,
                      mean.Lat),
         Lon = ifelse(is.na(mean.Lon),
                      Lon,
                      mean.Lon)) %>%
  select(-c(Cluster, SiteID.clust, mean.Lat, mean.Lon))
  
# Great! But this join only really solves the spatial issue..
# i.e., we have correctly grouped Site IDs and locations that were within 1km on the same day.
# But we still need to separate SiteID obs into temporally explicit 'Visits':

events.3 = unite(events.2, col = 'VisitID', SiteID, Date, remove = FALSE)

# Now we can label our catch data with VisitIDs:
catch.2 = left_join(catch.1, select(events.3, EventID, VisitID), by = 'EventID') %>%
  relocate(VisitID, .before = 1)





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

