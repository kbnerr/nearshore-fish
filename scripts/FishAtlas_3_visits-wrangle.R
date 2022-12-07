# -----------------------------------------------------------------------
# Title: Wrangle of NOAA's Nearshore Fish Atlas data
# Creator: Chris Guo
# Date: 2022.11.14
# Purpose: Re-format events_qc data into a QC'ed dataframe at the Site Visit level

# Notes -------------------------------------------------------------------

## Shorezone/FishAtlas web link: https://alaskafisheries.noaa.gov/mapping/sz/

# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggmap) # Remember to register api key with google if needed

# Define workflow paths ---------------------------------------------------

wd = getwd()
dir.output = file.path(wd, "output")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd,"data")
dir.scripts = file.path(wd,"scripts")

# Read in data ------------------------------------------------------------

# source(file = file.path(dir.scripts, "FishAtlas_1_events-wrangle.R"))
# source(file = file.path(dir.scripts, "FishAtlas_2_catch-wrangle.R"))


# Some EDA to begin with --------------------------------------------------

# These are all the categories that have finer resolution than VisitID,
n_distinct(events_qc$VisitID)
events_qc %>% select(VisitID, EventID) %>% n_distinct()
events_qc %>% select(VisitID, Region) %>% n_distinct()
events_qc %>% select(VisitID, Location) %>% n_distinct()
events_qc %>% select(VisitID, Habitat) %>% n_distinct()
events_qc %>% select(VisitID, TidalStage) %>% n_distinct()
events_qc %>% select(VisitID, Temp_C) %>% n_distinct()
events_qc %>% select(VisitID, Salinity) %>% n_distinct()
events_qc %>% select(VisitID, GearSpecific) %>% n_distinct()
events_qc %>% select(VisitID, ProjectName) %>% n_distinct()


# 1) Nest and remove empty catch visits -----------------------------------

# So let's nest these and deal with them one by one,
visits.1 = events_qc %>%
  inner_join(catch_qc %>% select(VisitID) %>% distinct(), by = "VisitID") %>% # Drop any visits removed during catch QAQC
  group_by(VisitID) %>%
  nest(EventIDs = EventID,
       Regions = Region,
       Locations = Location,
       Habitats = Habitat,
       TidalStages = TidalStage,
       Temps= Temp_C,
       Sals = Salinity,
       GearSpecifics = GearSpecific,
       ProjectNames = ProjectName) %>%
  relocate(PointOfContact, .after = last_col())


# 2) Events ---------------------------------------------------------------

# First we'll create a col to let us know how many seines were included per Visit,
visits.2 = visits.1 %>%
  rowwise() %>%
  mutate(n_Events = map_int(EventIDs, length),
         EventIDs = list(as_vector(EventIDs))) %>%
  relocate(n_Events, .after = Lon) %>%
  relocate(EventIDs, .after = last_col())
  

# 3) Mean temperature and salinity ----------------------------------------

# Let's find the mean T/S for visits that measured T/S (those that are not NA),
visits.3 = visits.2 %>%
  rowwise() %>%
  mutate(Temp_avg = mean(Temps$Temp_C, na.rm = TRUE),
         Temp_sd = sd(Temps$Temp_C, na.rm = TRUE),
         Sal_avg = mean(Sals$Salinity, na.rm = TRUE),
         Sal_sd = sd(Sals$Salinity, na.rm = TRUE)) %>%
  relocate(Temp_avg, Temp_sd, .after = Temps) %>%
  relocate(Sal_avg, Sal_sd, .after = Sals) %>%
  select(-Temps, -Sals)


# 4) Habitat classification -----------------------------------------------

# Now, let's tackle habitat classifications,
events_qc$Habitat %>% unique() %>% sort()
# Note, instead of NA's, we have 'not reported'.

# We should deal with cases where multiple habitats are reported.
# First we need to break down all habitat types as single classes to be reassigned to visits,
visits.4a = visits.3 %>%
  rowwise() %>%
  unnest_longer(Habitats) %>%
  unpack(Habitats) %>%
  arrange(VisitID, Habitat) %>%
  separate_rows(Habitat, sep = "-") %>%
  mutate(Habitat = ifelse(Habitat == "not reported", NA, Habitat)) %>%
  distinct()
# Save the habitat types as a vector,
visits.4a$Habitat %>% unique() %>% sort() -> habitats
# Recombine habitat combinations when visits include multiple habitat types,
visits.4b = visits.4a %>%
  pivot_wider(names_from = Habitat, values_from = Habitat, values_fill = NA) %>%
  replace(is.null(.), NA) %>%
  unite(col = Habitat, all_of(habitats), sep = "-", na.rm = TRUE) %>%
  mutate(Habitat = ifelse(Habitat == "", "not reported", Habitat)) %>%
  relocate(Habitat, .after = Locations) %>%
  select(-"NA")

# let's check out the different habitat classes we now have,
visits.4b$Habitat %>% unique()


# 5) Region classification ------------------------------------------------

# Let's figure out which visits combine regions - this should only be a few cases,
visits.5a = visits.4b %>%
  rowwise %>%
  unnest_longer(Regions) %>%
  unpack(Regions) %>%
  arrange(VisitID, Region) %>%
  distinct()
# Save the region classes as a vector
visits.5a$Region %>% unique() %>% sort() -> regions
# Recombine region to see which visits contain multiple classes
visits.5b = visits.5a %>%
  pivot_wider(names_from = Region, values_from = Region) %>%
  replace(is.null(.), NA) %>%
  unite(col = Region, all_of(regions), sep = "_", na.rm = TRUE) %>%
  relocate(Region, .after = n_Events)
visits.5b$Region %>% unique()
# hmm looks like we have visits where seines occurred in both the beaufort and chukchi regions.
# Let's take a look at them on a map,
map.arctic = get_map(location = geocode("Utqiagvik"), source = "google", maptype = "satellite", color = "bw", crop = FALSE, zoom = 7)
ggmap(map.arctic) +
  geom_point(data = filter(visits.5b, Region %in% c('Chukchi Sea', 'Beaufort Sea', 'Beaufort Sea_Chukchi Sea')),
             aes(x = Lon, y = Lat, color = Region, alpha = Region, size = Region)) +
  scale_color_manual(values = c("yellow", "green", "blue")) +
  scale_alpha_manual(values = c(0.4, 1, 0.4)) +
  scale_size_manual(values = c(1, 3, 1)) +
  labs(title = "NFA Visits in the Arctic")
ggsave("nfa_map_beaufort-chukchi_overlap.png", plot = last_plot(), device = 'png', path = file.path(dir.figs))

# These double regions seem to occur around Utqiavik.
# It makes sense to me that Beaufort and Chukchi visits could be combined to an 'Arctic' region,
visits.5c = visits.5b %>%
  mutate(Region = ifelse(Region %in% c('Chukchi Sea', 'Beaufort Sea', 'Beaufort Sea_Chukchi Sea'),
                         "Arctic",
                         Region))

# Let's take a look at all our visits by Region,
map.ak = get_map(location = geocode("Alaska"), source = "google", maptype = "hybrid", color = "bw", crop = FALSE, zoom = 4)
ggmap(map.ak) +
  geom_point(data = visits.5c, aes(x = Lon, y = Lat, color = Region)) +
  geom_vline(aes(xintercept = -142), color = "white", alpha = 0.5) + # add a vert longitude line at -142 degrees
  labs(title = "All NFA visits by Region")
# ggsave("nfa_map_combined-arctic_combined-goa.png", plot = last_plot(), device = 'png', path = file.path(dir.figs))

# We may want to split the GoA visits (for now),
visits.5d = visits.5c %>%
  mutate(Region = ifelse(Region == "Gulf of Alaska" & Lon < -142,
                         "West GoA",
                         ifelse(Region == "Gulf of Alaska" & Lon > -142,
                                "East GoA",
                                Region)))
# Let's take a look at the map again,
ggmap(map.ak) +
  geom_point(data = visits.5d, aes(x = Lon, y = Lat, color = Region)) +
  labs(title = "All NFA visits by Region")
ggsave("nfa_map_split_goa.png", plot = last_plot(), device = 'png', path = file.path(dir.figs))
# Looks ok for now.. we will want to address how we treat the samples in space at some point,
# seems kinda subjective that we have scattered patches of Arctic samples,
# but consider it one region when the Bering Sea is its own region.


# TidalStage --------------------------------------------------------------

# Now, let's tackle Tide Stage classifications (this will be similar as our protocol for Habitat),
events_qc$TidalStage %>% unique() %>% sort()
# These Tidal stages are inconsistent and pretty incomplete...
# I think we just drop TidalStage,
visits.6 = select(visits.5d, -TidalStages)


# Location ----------------------------------------------------------------

# We may be interested in Location down the line, but onyl as a QAQC check,
visits.7 = visits.6 %>%
  rowwise() %>%
  mutate(Locations = list(as_vector(Locations) %>% unique()))


# Project Name ------------------------------------------------------------

# We'll do the same processing as Region to find combos of Projects,
visits.8a = visits.7 %>%
  rowwise() %>%
  unnest_longer(ProjectNames) %>%
  unpack(ProjectNames) %>%
  arrange(VisitID, ProjectName) %>%
  distinct()
visits.8a$ProjectName %>% unique() %>% sort() -> projects
visits.8b = visits.8a %>%
  pivot_wider(names_from = ProjectName, values_from = ProjectName) %>%
  replace(is.null(.), NA) %>%
  unite(col = ProjectName, all_of(projects), sep = "_", na.rm = TRUE)

# let's take a look,
visits.8b$ProjectName %>% unique()

mult_projects = visits.8b %>%
  filter(ProjectName %in% c("CBJ eelgrass_SYNTHESIS", "ABL Nearshore Task - SEAK_SYNTHESIS")) %>%
  .$EventIDs %>% as_vector() %>% unname() %>% sort()

filter(catch_qc, EventID %in% mult_projects) %>% View()
filter(events_qc, EventID %in% mult_projects) %>% View()
filter(data, EventID %in% mult_projects) %>% View()


# Gear Specific -----------------------------------------------------------

# Same process as Region/Project to learn where we have combo gear types
visits.9a = visits.8c %>%
  rowwise() %>%
  unnest_longer(GearSpecifics) %>%
  unpack(GearSpecifics) %>%
  arrange(VisitID, GearSpecific) %>%
  distinct()
visits.9a$GearSpecific %>% unique() %>% sort() -> gear
visits.9b = visits.9a %>%
  pivot_wider(names_from = GearSpecific, values_from = GearSpecific) %>%
  replace(is.null(.), NA) %>%
  unite(col = GearSpecific, all_of(gear), sep = "_", na.rm = TRUE)
visits.9b$GearSpecific %>% unique()
# Let's take a look,
mult_gear = visits.9b %>%
  filter(str_detect(GearSpecific, "_")) %>%
  .$GearSpecific %>% as_vector() %>% unname() %>% unique() %>% sort()
filter(visits.9b, GearSpecific %in% mult_gear) %>% View()



# Environment clean-up ----------------------------------------------------

# Moving forward we only need the most a couple versions of the data:
# QC'ed events level data, original events level data (for comparison purposes), and most recent catch level data:
visits_qc = placeholder

# We'll keep these plus our wd objects:
keep = c('data',
         'events',
         'events_qc',
         'catch',
         'catch_qc',
         'fam_abun',
         'visits',
         'visits_qc',
         'wd',
         'dir.data',
         'dir.figs',
         'dir.output',
         'dir.scripts')

# And remove the rest, so we have a clean env when sourcing this code in next steps analyses:
rm(list = ls()[!ls() %in% keep])










  
  