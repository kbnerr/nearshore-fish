# -----------------------------------------------------------------------
# Title: Wrangle of NOAA's Nearshore Fish Atlas
# Creator: Chris Guo
# Date: 2022.09.01
# Purpose: Data wrangle of NOAA's nearshore fish atlas.

# Notes -------------------------------------------------------------------

## Shorezone/FishAtlas web link: https://alaskafisheries.noaa.gov/mapping/sz/

## Package rgdal will be retired by end of 2023.
# Make sure the code relying on this package is updated.
# See projections ~ line 240

# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(sf)
library(geosphere)
library(sp)
library(rgdal)

# Define workflow paths ---------------------------------------------------

wd = getwd()
dir.output = file.path(wd, "output")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd,"data")
dir.scripts = file.path(wd,"scripts")

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


# Site-level VS Event-level information -----------------------------------

## I am unsure if all project grouped their Site/Event information similarly...
# IOW, I don't know if samplers would all consider a 'site' the same thing.
# So I think we need to define what a SiteID is:

# A Site should be a unique place, i.e., all samples of the same 'beach'
# This is distinct from a 'Visit' which is all samples of the same beach on the same day.


## Let's check what we have...

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
events.1 %>% group_by(SiteID, Date) %>% summarise(events = n_distinct(EventID)) %>%
  ggplot(data = ., aes(x = events)) +
  geom_histogram(stat = 'count') +
  labs(title = "Frequency of SiteID-Date pairs with # of events (seines)")
# Looks like a lot of single events. And some more intensive 6-8 sets...

events.1 %>% group_by(Date, Location) %>% summarise(events = n()) %>%
  ggplot(data = ., aes(x = events)) +
  geom_histogram(stat = 'count') +
  labs(title = "Frequency of Date-Location pairs with # of events (seines)")
# Slightly different based on Date-Location...
# This grouping may not be accurate because of the loose definition of 'Location'.

# We'll try to subset the data into occasions where SiteID's are different,
# but the Date and location are telling us they should be similar.


# Solving the Site vs Event issue -----------------------------------------

## Basically, we are trying to combine Sites/Events that are currently separate SiteIDs.
# Soime should have the same SiteID, (i.e., samples of the same beach),
# but have different VisitID's within it (i.e., samples of the same beach AND day),
# and within VisitID's are the EventID's (i.e., unique seines pulled)

# It looks like we only have position data at the SiteID level...
n_distinct(events.1$EventID)
n_distinct(events.1$SiteID)
events.1 %>% distinct(Lat, Lon) %>% n_distinct()
# Looks like we have more distinct SiteID's than distinct locations, i.e., events are not spatially explicit

# Convert data to sf object:
ID.sf = events.1 %>%
  select(SiteID, EventID, Lon, Lat) %>%
  unite(col = 'ID', SiteID, EventID, sep = '_', remove = TRUE) %>% # unique Site/Event ID
  distinct() %>%
  st_as_sf(., coords = c("Lon", "Lat"), crs = 4326) # matches our n_distinct(SiteID)
# To be thorough, we combined Site-Events so that all obs are unique but still tied to site-level location

# Set aside ID's for renaming rows/cols of distance matrix later:
ID.names = ID.sf$ID

# Create a matrix of distances among all points:
ID.dist.mat = st_distance(ID.sf[ ,-1])

# Convert matrix to df and set column and row names:
ID.dist.df = data.frame(ID.dist.mat)
rownames(ID.dist.df) = ID.names
colnames(ID.dist.df) = ID.names

## Now we have to decide, what is a 'beach'?
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

# Rename the 'right side' as our ID to replace, and use the 'left side' for the ID to replace with (ID.a):
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

# This means that there are duplicated obs in our 'A' dataset. Which ones?:
check = ID.1km.2$ID %>% duplicated() %>% which()

ID.1km.3 = ID.1km.2[check,] %>% arrange(Date)
ID.1km.3 %>% arrange(desc(Dist_m))
# ^These are the the obs with multiple ID's that might need to be grouped;
# however, we only know of pairs that are <1km of each other: A within 1km of B, and B within 1km of C,
# But we do not know if A is within 1km of C... and we do not really care.
# We just want to find all sites within 1km of each other within the same day. 

ID.1km.3 %>%
  group_by(Date) %>%
  summarise(total.dist = sum(Dist_m)) %>%
  arrange(desc(total.dist))
# Here, we see dates that likely contain multiple groups. We can use this as a QA/QC check later.


# Grouping SiteID's based on distance -------------------------------------

## We will use a Hierarchical Clustering Analysis to define a distance threshold for which sites to group.
# Code provided by Jeffrey Evans from https://gis.stackexchange.com/questions/64392/finding-clusters-of-points-based-distance-rule-using-r

## Our issue is that location info is at the SiteID level and Date information is at the EventID level.
# So not all Events belonging to a SiteID should be grouped together.. only those occurring on the same day.

# First, we need to reshape our list of Site-Event ID's...
# Let's go back to our ID.1km.1, which has all of our obs within 1000m of other obs.

# We will bind ID-Dates from both A and B sides to get a list of all Site-Event-Dates to feed our clustering algorithm
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
rm(A, B) # clean up

## Next, we extract location data and transform into projected coordinate system

lon = ID.1km.4$Lon
lat = ID.1km.4$Lat
ID_Date = ID.1km.4$ID_Date
dist = 1000 # This is using the 'furthest neighbor' algorithm,
# where clusters are defined by the 'dist' between an obs within that cluster and an obs in another cluster

# Make a Spatial df using the sp package:
xy = SpatialPointsDataFrame(matrix(c(lon, lat), ncol = 2),
                            data.frame(ID_Date = ID_Date),
                            proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
# Project into WGS84:
xy = spTransform(xy, CRS("+init=epsg:27700 +datum=WGS84"))

# Perform Hierarchical Clustering Analysis on xy distances:
xy.clust = hclust(dist(data.frame(rownames = rownames(xy@data),
                                  x = coordinates(xy)[, 1],
                                  y = coordinates(xy)[, 2])),
                  method = "complete") # This defines the 'furthest neighbor' agglomeration method

# Define clusters based on 1000m 'furthest neighbor' threshold  
xy.1000m = cutree(xy.clust, h = dist)

# Adjust the xy data using the defined clusters:
xy@data = data.frame(xy@data, Cluster = xy.1000m)


# Re-classification of SiteID's based on HCA ------------------------------

## Now we can adjust our data to the spatially distinct ID's based on our HCA results.
# Remember, this only occurs at the SiteID level bc Events are not spatially unique.

# Make a new df adding clusters as a column:
clust.1 = xy@data %>%
  left_join(ID.1km.4, by = 'ID_Date') %>%
  separate(ID_Date, into = c('SiteID', 'EventID', 'Date'), sep = '_') %>%
  mutate(SiteID = as.double(SiteID),
         EventID = as.double(EventID),
         Date = ymd(Date)) %>%
  relocate(Cluster, .before = 1) %>%
  arrange(Cluster, Date)

# Nest the df by Cluster (space) and Date (time):
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
  unpack(cols = Events)

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

filter(events.2, SiteID %in% which(events.1$SiteID != events.2$SiteID))
# ^These are the SitesIDs that have been updated.

## Great! But this join only really solves the spatial issue..
# i.e., we have correctly grouped SiteIDs that were within 1km.
# But we still need to separate SiteID obs into temporally explicit 'Visits':

events.3 = unite(events.2, col = 'VisitID', SiteID, Date, remove = FALSE)

## Finally, let's re-label our catch data with the VisitIDs:
catch.2 = left_join(catch.1, select(events.3, EventID, VisitID), by = 'EventID') %>%
  relocate(VisitID, .before = 1)


# Re-visualize using new SiteIDs & VisitIDs -------------------------------

# This is the number of EventID's per SiteID and Date:
events.3 %>%
  group_by(VisitID) %>%
  summarise(events = n_distinct(EventID))
# Slightly different.. we can see in the first obs that there are two eventsnow instead of one

# Let's take a look at a frequency plot of these events:
events.3 %>% group_by(VisitID) %>% summarise(events = n_distinct(EventID)) %>%
  ggplot(data = ., aes(x = events)) +
  geom_histogram(stat = 'count') +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Frequency of visits with # of events (seines)")
# Compare this to our previous grouping...

events.1 %>% group_by(SiteID, Date) %>% summarise(events = n_distinct(EventID)) %>%
  ggplot(data = ., aes(x = events)) +
  geom_histogram(stat = 'count') +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Frequency of SiteID-Date pairs with # of events (seines)")
# Wow. Glad we combined some of these Sites! There are way less single set data points. 


# Environment clean-up ----------------------------------------------------

# Moving forward we only need the most recent version of the events and catch data:
events = events.3
catch = catch.2

# We'll keep these plus our wd objects:
keep = c('events',
         'catch',
         'wd',
         'dir.data',
         'dir.figs',
         'dir.output',
         'dir.scripts')

# And remove the rest, so we have a clean env when source in this code in next steps analyses:
rm(list = ls()[!ls() %in% keep])


