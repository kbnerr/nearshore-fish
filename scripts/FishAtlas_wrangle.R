## ----include = FALSE-------------------------------------------------------------------------------
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(size = "scriptsize")


## --------------------------------------------------------------------------------------------------
# Packages
library(tidyverse)
library(lubridate)
library(here)
library(skimr)
library(sf)
library(DT)

# Directory
wd = here()
dirs = wd %>% list.files() %>% str_subset(pattern = "^README|^LICENSE|.md$|.Rproj$", negate = TRUE)
for (i in seq_along(dirs)) {
  name = str_replace_all(dirs[i], "^", "dir.")
  path = str_replace_all(dirs[i], "^", str_c(wd, "/"))
  assign(name, path)
  rm(name, path, i)
}

# Options

# Source



## --------------------------------------------------------------------------------------------------
data = read_csv(file.path(dir.data, "FishAtlas_BeachSeines_2022.07.12.csv"),
               show_col_types = FALSE)
GearLookup = read_csv(file.path(dir.data, "FishAtlasExpansion_040422_Lookup_Gear.csv"),
                      show_col_types = FALSE)


## --------------------------------------------------------------------------------------------------
events.1 = data %>%
  select(SiteID, EventID,
         Date,
         Lat, Lon = Long, Region, Location,
         Habitat, TidalStage, Temp_C, Salinity,
         GearSpecific, ProjectName, PointOfContact) %>%
  mutate(Date = mdy(Date)) %>% # re-format date
  distinct()
glimpse(events.1)

catch.1 = data %>%
  select(EventID,
         Sp_CommonName, Sp_ScientificName, Fam_CommonName, Fam_ScientificName,
         Unmeasured, Length_mm, LengthType, LifeStage)
glimpse(catch.1)


## ----results = 'hide'------------------------------------------------------------------------------
# Join the mesh size info from our gear data to the site information
gear.1 = left_join(events.1, select(GearLookup, GearSpecific, MeshSize), by = "GearSpecific")

# Subset for gear related info and take a look
gear.2 = select(gear.1, EventID, MeshSize, GearSpecific, ProjectName, PointOfContact)
glimpse(gear.2)
# How many different mesh sizes are there?
gear.2$MeshSize %>% unique()

# Looks like we have an NA to take care of...
filter(gear.2, is.na(MeshSize))
# These are all from the GOAIERP project (PI Olav Ormseth). The report from that project reports 6 mm stretched mesh.
# We'll just go ahead and fix it here instead of changing the GearLookup file,
gear.3 = mutate(gear.2, MeshSize = ifelse(is.na(MeshSize), 6, MeshSize))

# Check our mesh sizes again,
gear.3$MeshSize %>% unique()
# Great, no more NA's.

# We can now join this information with our site data,
events.2 = left_join(events.1, select(gear.3, EventID, MeshSize), by = "EventID")


## --------------------------------------------------------------------------------------------------
skim(events.2)


## ----echo = FALSE----------------------------------------------------------------------------------
events.2 %>% summarise(Events = n_distinct(EventID), .by = c(SiteID, Date)) %>%
  ggplot(data = ., aes(x = Events)) +
  geom_bar(aes(y = after_stat(count))) +
  geom_text(stat = 'Count', aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Frequency of events (replicates) per SiteID-Date pair (samples)")

events.2 %>% summarise(Events = n_distinct(EventID), .by = c(Date, Location)) %>%
  ggplot(data = ., aes(x = Events)) +
  geom_bar(aes(y = after_stat(count))) +
  geom_text(stat = 'Count', aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Frequency of events per Location-Date pair")


## --------------------------------------------------------------------------------------------------
# Make a new tibble containing SiteID geometries:
sites.sf = events.2 %>%
  select(SiteID, Lat, Lon) %>%
  # Create sf object from lat/lon:
  st_as_sf(., coords = c("Lon", "Lat"), crs = 4326) %>% # WGS84
  # Project into Alaska Albers 3338
  st_transform(crs = 3338) %>%
  distinct()

# Define a buffer of 1km around each point
sites.sf.buf = st_buffer(sites.sf, dist = 1000)

# Create a tibble with clusters of overlapping buffer areas 
sites.sf.cl = sites.sf.buf %>%
  st_union() %>% # unite buffer areas
  st_cast(to = "POLYGON") %>% # turn them into polygon features
  st_as_sf() %>% # return geometry set features to sfc
  rownames_to_column(var = "Cluster") %>% # create col for cluster groups
  mutate(Cluster = as.factor(Cluster)) # use factor class

# Assign SiteID points to buffer area clusters
sites.sf.cl.join = st_join(sites.sf, sites.sf.cl, left = TRUE)

# Drop sf class from joined data
sites.cl = st_drop_geometry(sites.sf.cl.join)

# See what we got:
glimpse(sites.cl)


## --------------------------------------------------------------------------------------------------
events.3 = left_join(events.2, sites.cl, by = "SiteID")


## --------------------------------------------------------------------------------------------------
# Create a date-cluster tibble containing location and event info to be combined,
orig = select(events.3, Date, Cluster, SiteID, EventID, Lat, Lon)

# Nest the df by Date and Cluster,
orig.nest = orig %>%
  group_by(Date, Cluster) %>%
  nest(Sites = SiteID,
       Events = EventID,
       Lats = Lat,
       Lons = Lon) %>%
  ungroup()


## --------------------------------------------------------------------------------------------------
new = orig.nest %>% 
  rowwise() %>% # Operates a row-at-a-time (per sample)
  mutate(SiteID.cl = min(Sites), 
         Lat.mean = mean(Lats$Lat),
         Lon.mean = mean(Lons$Lon)) %>%
  select(-c(Sites, Lats, Lons)) %>% # Remove original info in nested variables
  unite(col = VisitID, SiteID.cl, Date, sep = "_", remove = FALSE) %>% # create VisitID
  select(-c(Date, SiteID.cl, Cluster)) %>% # Remove info now captured by VisitID
  unnest_longer(Events) %>% # Unnest to the EventID level
  unpack(cols = Events) # change tibble back to vector

# Join our new identifier to the rest of the site data,
events = left_join(new, events.3, by = "EventID") %>%
  mutate(Lat = Lat.mean, Lon = Lon.mean) %>% # Replace old lat/lon with new mean lat/lon
  select(-c(Lat.mean, Lon.mean))

# Check our our new data structure
head(events, 12)


## ----echo = FALSE----------------------------------------------------------------------------------
# Plot events per SiteID-Date same as before,
events.3 %>% summarise(Events = n_distinct(EventID), .by = c(SiteID, Date)) %>%
  ggplot(data = ., aes(x = Events)) +
  geom_bar(aes(y = after_stat(count))) +
  geom_text(stat = 'Count', aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Frequency of events per sample in original data structure")
# ggsave("nfa_freq-Site&Date-by-seines.png", plot = last_plot(), device = 'png', path = file.path(dir.figs))

# And events per visit
events %>% summarise(Events = n_distinct(EventID), .by = VisitID) %>%
  ggplot(data = ., aes(x = Events)) +
  geom_bar(aes(y = after_stat(count))) +
  geom_text(stat = 'Count', aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Frequency of events per sample after solving Site-Event problem")
# ggsave("nfa_freq-visits-by-seines.png", plot = last_plot(), device = 'png', path = file.path(dir.figs))



## --------------------------------------------------------------------------------------------------
# Clean up our environment for the next step:
rm(list = ls()[!ls() %in% c('events', 'catch.1', 'data')])

# Reset directory
wd = here()
dirs = wd %>% list.files() %>% str_subset(pattern = "^README|^LICENSE|.md$|.Rproj$", negate = TRUE)
for (i in seq_along(dirs)) {
  name = str_replace_all(dirs[i], "^", "dir.")
  path = str_replace_all(dirs[i], "^", str_c(wd, "/"))
  assign(name, path)
  rm(path, i)
}


## --------------------------------------------------------------------------------------------------
skim(catch.1)


## ----echo = FALSE----------------------------------------------------------------------------------
catch.1 %>%
  select(Fam_ScientificName, Fam_CommonName) %>%
  rename(`Scientific Name` = Fam_ScientificName, `Common Name` = Fam_CommonName) %>%
  distinct() %>%
  arrange(`Scientific Name`) %>%
  datatable(rownames = FALSE,
            extensions = 'Scroller',
            options = list(deferRender = TRUE,
                           scrollY = 500,
                           scroller = TRUE))


## ----echo = FALSE----------------------------------------------------------------------------------
catch.1 %>%
  select(Sp_ScientificName, Sp_CommonName) %>%
  rename(`Scientific Name` = Sp_ScientificName, `Common Name` = Sp_CommonName) %>%
  distinct() %>%
  arrange(`Scientific Name`) %>%
  datatable(rownames = FALSE,
            extensions = 'Scroller',
            options = list(deferRender = TRUE,
                           scrollY = 500,
                           scroller = TRUE))


## ----results = "hide"------------------------------------------------------------------------------
# Check out the cases where family is missing
filter(catch.1, is.na(Fam_ScientificName))
# Look up the visit info for these cases
filter(events, EventID %in% catch.1$EventID[which(is.na(catch.1$Fam_ScientificName))])
# Check out everything else caught during these visits
filter(catch.1, EventID %in% catch.1$EventID[which(is.na(catch.1$Fam_ScientificName))])
# Since these nine NAs make up a small portion of their samples,
# The simplest solve here is to just remove the data.

# Create a new object after edits made to taxonomy
catch.2 = catch.1 %>%
  # Drop the nine instances of NA catch
  drop_na(Fam_ScientificName) %>%
  # Find the 'or' species and replace with Fam_ScientificName value
  mutate(Sp_ScientificName = ifelse(str_detect(Sp_ScientificName, pattern = " or "),
                                    Fam_ScientificName,
                                    Sp_ScientificName),
         # Create genus class by taking first word of Sp_ScientificName
         Gen_ScientificName = word(Sp_ScientificName, 1))


## ----echo = FALSE----------------------------------------------------------------------------------
catch.2 %>%
  select(Fam_ScientificName, Fam_CommonName) %>%
  rename(`Scientific Name` = Fam_ScientificName, `Common Name` = Fam_CommonName) %>%
  distinct() %>%
  arrange(`Scientific Name`) %>%
  datatable(rownames = FALSE,
            extensions = 'Scroller',
            options = list(deferRender = TRUE,
                           scrollY = 500,
                           scroller = TRUE))


## ----echo = FALSE----------------------------------------------------------------------------------
catch.2 %>%
  select(`Scientific Name` = Gen_ScientificName) %>%
  distinct() %>%
  arrange(`Scientific Name`) %>%
  datatable(rownames = FALSE,
            extensions = 'Scroller',
            options = list(deferRender = TRUE,
                           scrollY = 500,
                           scroller = TRUE))


## ----echo = FALSE----------------------------------------------------------------------------------
catch.2 %>%
  select(Sp_ScientificName, Sp_CommonName) %>%
  rename(`Scientific Name` = Sp_ScientificName, `Common Name` = Sp_CommonName) %>%
  distinct() %>%
  arrange(`Scientific Name`) %>%
  datatable(rownames = FALSE,
            extensions = 'Scroller',
            options = list(deferRender = TRUE,
                           scrollY = 500,
                           scroller = TRUE))


## ----results = "hide"------------------------------------------------------------------------------
catch.3 = mutate(catch.2, Count = ifelse(Unmeasured == 0, 1, Unmeasured)) %>%
  select(-Unmeasured) # remove unused parameter


## ----echo = FALSE, fig.width = 12, fig.height = 8--------------------------------------------------
# Family abundance
fam.abun = catch.3 %>%
  summarise(Abundance = sum(Count), .by = Fam_ScientificName) %>%
  mutate(Fam_ScientificName = fct_reorder(as.factor(Fam_ScientificName), desc(Abundance)))
# Plot
ggplot(dat = fam.abun, aes(x = Fam_ScientificName, y = Abundance)) +
  geom_col() +
  geom_text(aes(label = Abundance), vjust = -0.5, size = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = 'Family abundance', x = 'Family')
# ggsave("nfa_fam_abun_raw.png", plot = last_plot(), device = 'png', path = file.path(dir.figs))

# Family frequency of occurrence
fam.freq.occur = left_join(catch.3, select(events, VisitID, EventID), by = "EventID") %>%
  summarise(Presence = n_distinct(Fam_ScientificName), .by = c(VisitID, Fam_ScientificName)) %>%
  summarise(Occurrence = sum(Presence), .by = Fam_ScientificName) %>%
  mutate(Perc_Occurrence = round(Occurrence / n_distinct(events$VisitID) * 100, 2),
         Fam_ScientificName = fct_reorder(as.factor(Fam_ScientificName), desc(Perc_Occurrence)))
# Plot
ggplot(data = fam.freq.occur, aes(x = Fam_ScientificName, y = Perc_Occurrence)) +
  geom_col() +
  geom_text(aes(label = round(Perc_Occurrence, 1)), size = 2, vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = 'Family frequency of occurrence', x = 'Family', y = 'Percent occurrence')
# ggsave("nfa_fam_freq-occur.png", plot = last_plot(), device = 'png', path = file.path(dir.figs))

# Table of family abundance and occurrence
full_join(fam.abun, fam.freq.occur, by = 'Fam_ScientificName') %>%
  select(`Scientific Name` = Fam_ScientificName, `Percent Occurrence` = Perc_Occurrence, Abundance) %>%
  arrange(as.character(`Scientific Name`)) %>%
  datatable(rownames = FALSE,
            extensions = 'Scroller',
            options = list(deferRender = TRUE,
                           scrollY = 500,
                           scroller = TRUE))


## ----echo = FALSE, fig.width = 12, fig.height = 8--------------------------------------------------
# Genus abundance
gen.abun = catch.3 %>%
  summarise(Abundance = sum(Count), .by = Gen_ScientificName) %>%
  mutate(Gen_ScientificName = fct_reorder(as.factor(Gen_ScientificName), desc(Abundance)))
# Plot
ggplot(data = gen.abun, aes(x = Gen_ScientificName, y = Abundance)) +
  geom_col() +
  geom_text(aes(label = Abundance), vjust = -0.5, size = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = 'Genus abundance', x = 'Genus')
# ggsave("nfa_gen_abun_raw.png", plot = last_plot(), device = 'png', path = file.path(dir.figs))

# Genus frequency of occurrence
gen.freq.occur = left_join(catch.3, select(events, VisitID, EventID), by = "EventID") %>%
  summarise(Presence = n_distinct(Gen_ScientificName), .by = c(VisitID, Gen_ScientificName)) %>%
  summarise(Occurrence = sum(Presence), .by = Gen_ScientificName) %>%
  mutate(Perc_Occurrence = round(Occurrence / n_distinct(events$VisitID) * 100, 2),
         Gen_ScientificName = fct_reorder(as.factor(Gen_ScientificName), desc(Perc_Occurrence)))
# Plot 
ggplot(data = gen.freq.occur, aes(x = Gen_ScientificName, y = Perc_Occurrence)) +
  geom_col() +
  geom_text(aes(label = round(Perc_Occurrence, 1)), size = 2, vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = 'Genus frequency of occurrence', x = 'Genus', y = 'Percent occurrence')
# ggsave("nfa_gen_freq-occur.png", plot = last_plot(), device = 'png', path = file.path(dir.figs))

# Table of genus abundance and occurrence
full_join(gen.abun, gen.freq.occur, by = 'Gen_ScientificName') %>%
  select(`Scientific Name` = Gen_ScientificName, `Percent Occurrence` = Perc_Occurrence, Abundance) %>%
  arrange(as.character(`Scientific Name`)) %>%
  datatable(rownames = FALSE,
            extensions = 'Scroller',
            options = list(deferRender = TRUE,
                           scrollY = 500,
                           scroller = TRUE))


## ----echo = FALSE, fig.width = 12, fig.height = 8--------------------------------------------------
# Species abundance
sp.abun = catch.3 %>%
  summarise(Abundance = sum(Count), .by = Sp_ScientificName) %>%
  mutate(Sp_ScientificName = fct_reorder(as.factor(Sp_ScientificName), desc(Abundance)))
# Plot
ggplot(data = sp.abun, aes(x = Sp_ScientificName, y = Abundance)) +
  geom_col() +
  geom_text(aes(label = Abundance), vjust = -0.5, size = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = 'Species abundance', x = 'Species')
# ggsave("nfa_sp_abun_raw.png", plot = last_plot(), device = 'png', path = file.path(dir.figs))

# Species frequency of occurrence
sp.freq.occur = left_join(catch.3, select(events, VisitID, EventID), by = "EventID") %>%
  summarise(Presence = n_distinct(Sp_ScientificName), .by = c(VisitID, Sp_ScientificName)) %>%
  summarise(Occurrence = sum(Presence), .by = Sp_ScientificName) %>%
  mutate(Perc_Occurrence = round(Occurrence / n_distinct(events$VisitID) * 100, 2),
         Sp_ScientificName = fct_reorder(as.factor(Sp_ScientificName), desc(Perc_Occurrence)))
# Plot
ggplot(data = sp.freq.occur, aes(x = Sp_ScientificName, y = Perc_Occurrence)) +
  geom_col() +
  geom_text(aes(label = round(Perc_Occurrence, 1)), size = 2, vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = 'Species frequency of occurrence', x = 'Species', y = 'Percent occurrence')
# ggsave("nfa_sp_freq-occur.png", plot = last_plot(), device = 'png', path = file.path(dir.figs))

# Table of species abundance and occurrence
full_join(sp.abun, sp.freq.occur, by = 'Sp_ScientificName') %>%
  select(`Scientific Name` = Sp_ScientificName, `Percent Occurrence` = Perc_Occurrence, Abundance) %>%
  arrange(as.character(`Scientific Name`)) %>%
  datatable(rownames = FALSE,
            extensions = 'Scroller',
            options = list(deferRender = TRUE,
                           scrollY = 500,
                           scroller = TRUE))


## ----echo = FALSE----------------------------------------------------------------------------------
# Subset rare families
rare.fam = full_join(fam.abun, fam.freq.occur, by = 'Fam_ScientificName') %>%
  filter(Perc_Occurrence < 0.25) %>%
  select(Fam_ScientificName, Perc_Occurrence, Abundance)
# Table of rare family abundance and occurrence
rename(rare.fam, `Scientific Name` = Fam_ScientificName, `Percent Occurrence` = Perc_Occurrence) %>%
  arrange(as.character(`Scientific Name`)) %>%
  datatable(rownames = FALSE,
            extensions = 'Scroller',
            options = list(deferRender = TRUE,
                           scrollY = 250,
                           scroller = TRUE))


## ----echo = FALSE----------------------------------------------------------------------------------
# Subset rare genus
rare.gen = full_join(gen.abun, gen.freq.occur, by = 'Gen_ScientificName') %>%
  filter(Perc_Occurrence < 0.25) %>%
  select(Gen_ScientificName, Perc_Occurrence, Abundance)
# Table of rare genus abundance and occurrence
rename(rare.gen, `Scientific Name` = Gen_ScientificName, `Percent Occurrence` = Perc_Occurrence) %>%
  arrange(as.character(`Scientific Name`)) %>%
  datatable(rownames = FALSE,
            extensions = 'Scroller',
            options = list(deferRender = TRUE,
                           scrollY = 250,
                           scroller = TRUE))


## ----echo = FALSE----------------------------------------------------------------------------------
# Subset rare species
rare.sp = full_join(sp.abun, sp.freq.occur, by = 'Sp_ScientificName') %>%
  filter(Perc_Occurrence < 0.25) %>%
  select(Sp_ScientificName, Perc_Occurrence, Abundance)
# Table of rare genus abundance and occurrence
rename(rare.sp, `Scientific Name` = Sp_ScientificName, `Percent Occurrence` = Perc_Occurrence) %>%
  arrange(as.character(`Scientific Name`)) %>%
  datatable(rownames = FALSE,
            extensions = 'Scroller',
            options = list(deferRender = TRUE,
                           scrollY = 250,
                           scroller = TRUE))


## ----echo = FALSE, fig.width = 12, fig.height = 8--------------------------------------------------
# Plot abundance
anti_join(fam.abun, rare.fam, by = "Fam_ScientificName") %>%
  ggplot(aes(x = Fam_ScientificName, y = Abundance)) +
  geom_col() +
  geom_text(aes(label = Abundance), vjust = -0.5, size = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = 'Family abundance excluding rare taxa', x = 'Family')
# ggsave("nfa_fam_abun_raw_ex-rare.png", plot = last_plot(), device = 'png', path = file.path(dir.figs))

# Plot frequency of occurrence
anti_join(fam.freq.occur, rare.fam, by = "Fam_ScientificName") %>%
  ggplot(aes(x = Fam_ScientificName, y = Perc_Occurrence)) +
  geom_col() +
  geom_text(aes(label = round(Perc_Occurrence, 1)), size = 2, vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = 'Family frequency of occurrence excluding rare taxa', x = 'Family', y = 'Percent occurrence')
# ggsave("nfa_fam_freq-occur_ex-rare.png", plot = last_plot(), device = 'png', path = file.path(dir.figs))


## ----echo = FALSE, fig.width = 12, fig.height = 8--------------------------------------------------
# Plot abundance
anti_join(gen.abun, rare.gen) %>%
  ggplot(aes(x = Gen_ScientificName, y = Abundance)) +
  geom_col() +
  geom_text(aes(label = Abundance), vjust = -0.5, size = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = 'Genus abundance excluding rare taxa', x = 'Genus')
# ggsave("nfa_gen_abun_raw_ex-rare.png", plot = last_plot(), device = 'png', path = file.path(dir.figs))

# Plot frequency of occurrence
anti_join(gen.freq.occur, rare.gen) %>%
  ggplot(aes(x = Gen_ScientificName, y = Perc_Occurrence)) +
  geom_col() +
  geom_text(aes(label = round(Perc_Occurrence, 1)), size = 2, vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = 'Genus frequency of occurrence excluding rare taxa', x = 'Genus', y = 'Percent occurrence')
# ggsave("nfa_gen_freq-occur_ex-rare.png", plot = last_plot(), device = 'png', path = file.path(dir.figs))


## ----echo = FALSE, fig.width = 12, fig.height = 8--------------------------------------------------
# Plot abundance
anti_join(sp.abun, rare.sp) %>%
  ggplot(aes(x = Sp_ScientificName, y = Abundance)) +
  geom_col() +
  geom_text(aes(label = Abundance), vjust = -0.5, size = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = 'Species abundance excluding rare taxa', x = 'Species')
# ggsave("nfa_sp_abun_raw_ex-rare.png", plot = last_plot(), device = 'png', path = file.path(dir.figs))

# Plot frequency of occurrence
anti_join(sp.freq.occur, rare.sp) %>%
  ggplot(aes(x = Sp_ScientificName, y = Perc_Occurrence)) +
  geom_col() +
  geom_text(aes(label = round(Perc_Occurrence, 1)), size = 2, vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = 'Species frequency of occurrence excluding rare taxa', x = 'Species', y = 'Percent occurrence')
# ggsave("nfa_sp_freq-occur_ex-rare.png", plot = last_plot(), device = 'png', path = file.path(dir.figs))


## --------------------------------------------------------------------------------------------------
## Family abundance per visit
# Save an object of all the rare families
rare.families = unique(rare.fam$Fam_ScientificName) %>% as.character()
# Create a tibble of family abundance excluding rare families
catch.fam = left_join(select(events, VisitID, EventID), catch.3, by = "EventID") %>%
  select(VisitID,
         EventID,
         Fam_ScientificName,
         Count) %>%
  filter(!Fam_ScientificName %in% rare.families) %>% # remove rare taxa
  summarise(Abundance = sum(Count) / n_distinct(EventID), .by = c(VisitID, Fam_ScientificName))
  
## Genus abundance per visit
# Save an object of all the rare genus
rare.genus = unique(rare.gen$Gen_ScientificName) %>% as.character()
# Create a tibble of genus abundance excluding rare genus
catch.gen = left_join(select(events, VisitID, EventID), catch.3, by = "EventID") %>%
  select(VisitID,
         EventID,
         Gen_ScientificName,
         Count) %>%
  filter(!Gen_ScientificName %in% rare.genus) %>% # remove rare taxa
  ungroup() %>%
  summarise(Abundance = (sum(Count) / n_distinct(EventID)), .by=c(VisitID, Gen_ScientificName))

## Species abundance per visit
# Save an object of all the rare species
rare.species = unique(rare.sp$Sp_ScientificName) %>% as.character()
# Create a tibble of species abundance excluding rare species
catch.sp = left_join(select(events, VisitID, EventID), catch.3, by = "EventID") %>%
  select(VisitID,
         EventID,
         Sp_ScientificName,
         Count) %>%
  filter(!Sp_ScientificName %in% rare.species) %>% # remove rare taxa
  ungroup() %>%
  summarise(Abundance = (sum(Count) / n_distinct(EventID)), .by = c(VisitID, Sp_ScientificName))


## --------------------------------------------------------------------------------------------------
# Re-assign working version of catch to a cleaner name
assign("catch", catch.3)


## --------------------------------------------------------------------------------------------------
# Clean up our environment for the next step:
rm(list = ls()[!ls() %in% c('data', 'events', 'catch', 'catch.fam', 'catch.gen', 'catch.sp', 'rare.families', 'rare.genus', 'rare.species')])

# Reset directory
wd = here()
dirs = wd %>% list.files() %>% str_subset(pattern = "^README|^LICENSE|.md$|.Rproj$", negate = TRUE)
for (i in seq_along(dirs)) {
  name = str_replace_all(dirs[i], "^", "dir.")
  path = str_replace_all(dirs[i], "^", str_c(wd, "/"))
  assign(name, path)
  rm(path, i)
}


## --------------------------------------------------------------------------------------------------
# New tibble for visit level wrangling,
visits.1 = select(events, VisitID, EventID) %>%
  summarise(Replicates = n_distinct(EventID), .by = VisitID) %>%
  right_join(select(events, -SiteID, -EventID), by = "VisitID") %>%
  distinct()
visits.1 %>% nrow()


## --------------------------------------------------------------------------------------------------
skim(visits.1)


## --------------------------------------------------------------------------------------------------
# Update tibble to remove unnecessary site info,
visits.2 = select(visits.1, -c(ProjectName, PointOfContact, GearSpecific, TidalStage)) %>% distinct()
visits.2 %>% nrow()


## --------------------------------------------------------------------------------------------------
# Update tibble to remove 'problem' variables,
visits.3 = select(visits.2, -c(Region, Location, Habitat, Temperature = Temp_C, Salinity)) %>% distinct()
visits.3 %>% nrow()


## --------------------------------------------------------------------------------------------------
# New object for visits with multiple mesh size
mult.mesh = select(visits.3, VisitID, MeshSize) %>%
  summarise(`n Mesh Sizes` = n_distinct(MeshSize), .by = VisitID) %>%
  filter(`n Mesh Sizes` > 1)
# View
mult.mesh


## --------------------------------------------------------------------------------------------------
# Join multiple mesh events with other event level info,
mult.mesh.events = select(mult.mesh, VisitID) %>% left_join(events, by = "VisitID")
# Skim the project and date info,
select(mult.mesh.events, GearSpecific, ProjectName, Date) %>%
  mutate(across(c(GearSpecific, ProjectName), ~ as_factor(.x))) %>%
  skim()
# Summary of gear and mesh size per visit,
select(mult.mesh.events, VisitID, GearSpecific, MeshSize) %>% 
  summarise(n_visits = n_distinct(VisitID), .by = c(GearSpecific, MeshSize))


## --------------------------------------------------------------------------------------------------
# Create a tibble to store the VisitID's to be fixed
mult.mesh.fix = select(mult.mesh.events, VisitID, EventID, GearSpecific, MeshSize) %>%
  # Create a unique suffix that identifies mesh size used per event
  mutate(suffix = gsub("BSEINE-", "", GearSpecific)) %>%
  # Add the suffix to VisitID, creating new VisitID's
  unite(col = VisitID_new, VisitID, suffix, sep = "_") %>%
  # Retain only the VisitID's and their EventID's
  select(EventID, VisitID_new)

# Update the 'events' tibble with fixed VisitID's,
events = left_join(select(events, EventID, VisitID), mult.mesh.fix, by = "EventID") %>%
  # Where VisitID's were not fixed, fill in with old VisitID's
  mutate(VisitID_new = ifelse(is.na(VisitID_new), VisitID, VisitID_new)) %>%
  # Remove old VisitID column
  select(-VisitID) %>%
  # Re-join events data (w/o the old VisitID's) with the new VisitID's,
  left_join(select(events, -VisitID), by = c("EventID")) %>%
  # Rename (this could be done earlier but seems clearer here)
  rename(VisitID = VisitID_new)


## ----echo = FALSE----------------------------------------------------------------------------------
## Re-summarize catch
# Abundance
fam.abun = summarise(catch, Abundance = sum(Count), .by = Fam_ScientificName)
# Frequency of occurrence
fam.freq.occur = left_join(catch, select(events, VisitID, EventID), by = "EventID") %>%
  summarise(Presence = n_distinct(Fam_ScientificName), .by = c(VisitID, Fam_ScientificName)) %>%
  summarise(Occurrence = sum(Presence), .by = Fam_ScientificName) %>%
  mutate(Perc_Occurrence = round(Occurrence / n_distinct(events$VisitID) * 100, 2))
# Combine calculated variables and define rare taxa
fam.tbl = full_join(fam.abun, fam.freq.occur, by = 'Fam_ScientificName') %>%
  mutate(Rare = ifelse(Occurrence < 5, "Yes", "No"))
# Save rare taxa as vector
rare.families = filter(fam.tbl, Rare == "Yes") %>% pull(Fam_ScientificName)
# Summary table
fam.tbl %>%
  select(`Scientific Name` = Fam_ScientificName, `Percent Occurrence` = Perc_Occurrence, Abundance, Rare) %>%
  arrange(as.character(`Scientific Name`)) %>%
  datatable(rownames = FALSE,
            extensions = 'Scroller',
            options = list(deferRender = TRUE,
                           scrollY = 500,
                           scroller = TRUE,
                           columnDefs = list(list(targets = 'Rare', visible = FALSE)))) %>% 
  formatStyle(columns = "Rare",
              target = "row",
              backgroundColor = styleEqual("Yes", "goldenrod"))


## ----echo = FALSE----------------------------------------------------------------------------------
## Re-summarize catch
# Abundance
gen.abun = summarise(catch, Abundance = sum(Count), .by = Gen_ScientificName)
# Frequency of occurrence
gen.freq.occur = left_join(catch, select(events, VisitID, EventID), by = "EventID") %>%
  summarise(Presence = n_distinct(Gen_ScientificName), .by = c(VisitID, Gen_ScientificName)) %>%
  summarise(Occurrence = sum(Presence), .by = Gen_ScientificName) %>%
  mutate(Perc_Occurrence = round(Occurrence / n_distinct(events$VisitID) * 100, 2))
# Combine calculated variables and define rare taxa
gen.tbl = full_join(gen.abun, gen.freq.occur, by = 'Gen_ScientificName') %>%
  mutate(Rare = ifelse(Occurrence < 5, "Yes", "No"))
# Save rare taxa as vector
rare.genus = filter(gen.tbl, Rare == "Yes") %>% pull(Gen_ScientificName)
# Summary table
gen.tbl %>%
  select(`Scientific Name` = Gen_ScientificName, `Percent Occurrence` = Perc_Occurrence, Abundance, Rare) %>%
  arrange(as.character(`Scientific Name`)) %>%
  datatable(rownames = FALSE,
            extensions = 'Scroller',
            options = list(deferRender = TRUE,
                           scrollY = 500,
                           scroller = TRUE,
                           columnDefs = list(list(targets = 'Rare', visible = FALSE)))) %>% 
  formatStyle(columns = "Rare",
              target = "row",
              backgroundColor = styleEqual("Yes", "goldenrod"))


## ----echo = FALSE----------------------------------------------------------------------------------
## Re-summarize catch
# Abundance
sp.abun = summarise(catch, Abundance = sum(Count), .by = Sp_ScientificName)
# Frequency of occurrence
sp.freq.occur = left_join(catch, select(events, VisitID, EventID), by = "EventID") %>%
  summarise(Presence = n_distinct(Sp_ScientificName), .by = c(VisitID, Sp_ScientificName)) %>%
  summarise(Occurrence = sum(Presence), .by = Sp_ScientificName) %>%
  mutate(Perc_Occurrence = round(Occurrence / n_distinct(events$VisitID) * 100, 2))
# Combine calculated variables and define rare taxa
sp.tbl = full_join(sp.abun, sp.freq.occur, by = 'Sp_ScientificName') %>%
  mutate(Rare = ifelse(Occurrence < 5, "Yes", "No"))
# Save rare taxa as vector
rare.species = filter(sp.tbl, Rare == "Yes") %>% pull(Sp_ScientificName)
# Summary table
sp.tbl %>%
  select(`Scientific Name` = Sp_ScientificName, `Percent Occurrence` = Perc_Occurrence, Abundance, Rare) %>%
  arrange(as.character(`Scientific Name`)) %>%
  datatable(rownames = FALSE,
            extensions = 'Scroller',
            options = list(deferRender = TRUE,
                           scrollY = 500,
                           scroller = TRUE,
                           columnDefs = list(list(targets = 'Rare', visible = FALSE)))) %>% 
  formatStyle(columns = "Rare",
              target = "row",
              backgroundColor = styleEqual("Yes", "goldenrod"))


## --------------------------------------------------------------------------------------------------
# Create a tibble of mean abundance per visit for each family excluding rare taxa
abun.fam = left_join(select(events, VisitID, EventID), catch, by = "EventID") %>%
  select(VisitID,
         EventID,
         Fam_ScientificName,
         Count) %>%
  filter(!Fam_ScientificName %in% rare.families) %>%
  ungroup() %>%
  summarise(Abundance = (sum(Count) / n_distinct(EventID)), .by = c(VisitID, Fam_ScientificName))

# Create a tibble of mean abundance per visit for each Genus excluding rare taxa
abun.gen = left_join(select(events, VisitID, EventID), catch, by = "EventID") %>%
  select(VisitID,
         EventID,
         Gen_ScientificName,
         Count) %>%
  filter(!Gen_ScientificName %in% rare.genus) %>% # remove rare taxa
  ungroup() %>%
  summarise(Abundance = (sum(Count) / n_distinct(EventID)), .by = c(VisitID, Gen_ScientificName))

# Create a tibble of mean abundance per visit for each Species excluding rare taxa
abun.sp = left_join(select(events, VisitID, EventID), catch, by = "EventID") %>%
  select(VisitID,
         EventID,
         Sp_ScientificName,
         Count) %>%
  filter(!Sp_ScientificName %in% rare.species) %>% # remove rare taxa
  ungroup() %>%
  summarise(Abundance = (sum(Count) / n_distinct(EventID)), .by = c(VisitID, Sp_ScientificName))


## --------------------------------------------------------------------------------------------------
# Re-run visits.1 code,
visits.4 = select(events, VisitID, EventID) %>%
  summarise(Replicates = n_distinct(EventID), .by = VisitID) %>%
  right_join(select(events, -SiteID, -EventID), by = "VisitID") %>%
  distinct() %>%
  # Re-run visits.2 & visits.3 code,
  select(-c(ProjectName, PointOfContact, GearSpecific, TidalStage,
            Region, Location, Habitat, Temperature = Temp_C, Salinity)) %>%
  distinct()


## --------------------------------------------------------------------------------------------------
n_distinct(visits.4$VisitID) == nrow(visits.4)


## ----echo = FALSE----------------------------------------------------------------------------------
# And events per visit
events %>% summarise(Events = n_distinct(EventID), .by = VisitID) %>%
  ggplot(data = ., aes(x = Events)) +
  geom_bar(aes(y = after_stat(count))) +
  geom_text(stat = 'Count', aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Frequency of events per sample after solving multiple gear-type problem")
# ggsave("nfa_freq-events-per-sample.png", plot = last_plot(), device = 'png', path = file.path(dir.figs))


## --------------------------------------------------------------------------------------------------
visits = visits.4


## --------------------------------------------------------------------------------------------------
# Clean up our environment for the next step:
rm(list = ls()[!ls() %in% c('data', 'events', 'catch', 'visits',
                            'abun.fam', 'abun.gen', 'abun.sp',
                            'fam.tbl', 'gen.tbl', 'sp.tbl')])

# Reset directory
wd = here()
dirs = wd %>% list.files() %>% str_subset(pattern = "^README|^LICENSE|.md$|.Rproj$", negate = TRUE)
for (i in seq_along(dirs)) {
  name = str_replace_all(dirs[i], "^", "dir.")
  path = str_replace_all(dirs[i], "^", str_c(wd, "/"))
  assign(name, path)
  rm(path, i)
}

