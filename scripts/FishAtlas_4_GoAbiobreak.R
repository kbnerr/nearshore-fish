# -----------------------------------------------------------------------
# Title: GoA bio break - cluster analysis
# Creator: Chris Guo
# Date: 2023.09.28
# Purpose: 

# Notes -------------------------------------------------------------------

# Spatial EDA was done using the following: 

# sf package, install.packages("sf", type = "source", configure.args = "--with-proj-lib=$(brew --prefix)/lib/")

# NCEAS tutorial at https://learning.nceas.ucsb.edu/2020-11-RRCourse/session-13-geospatial-analysis-in-r.html

# 


# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(cluster)
library(vegan)
library(sf)
library(scales)
# remotes::install_github("dkahle/ggmap")
# library(ggmap)
# register_google(key = api.google)

# Define workflow paths ---------------------------------------------------

wd = getwd()
dir.output = file.path(wd, "output")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd, "data")
dir.data.ignore = file.path(wd, "data.ignore")
dir.scripts = file.path(wd,"scripts")


# Run source codes --------------------------------------------------------

source(file = file.path(dir.scripts, "FishAtlas_1_events-wrangle.R"))
source(file = file.path(dir.scripts, "FishAtlas_2_catch-wrangle.R"))
source(file = file.path(dir.scripts, "FishAtlas_3_visits-wrangle.R"))
source(file = file.path(dir.scripts, "utility.R"))


# Utility -----------------------------------------------------------------

# GoA bbox
bbox.GoA = c(-180, 52, -125, 62)

## This is from NCEAS: https://learning.nceas.ucsb.edu/2020-11-RRCourse/session-13-geospatial-analysis-in-r.html
# Define a function to fix the bbox to be in EPSG:3857
# See https://github.com/dkahle/ggmap/issues/160#issuecomment-397055208
ggmap_bbox_to_3857 <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

# Data preparation --------------------------------------------------------

## Spatial
# ggmap of AK
# map_gg_ak = get_map(location = geocode("Alaska"), source = "google", maptype = "hybrid", color = "bw", crop = FALSE, zoom = 4)
# map_gg_GoA = get_map(location = bbox.GoA, source = "google", maptype = "terrain", crop = FALSE, zoom = 4)

# NCEAS shapefile of AK
map_nceas_4326 = read_sf(dsn = file.path(dir.data.ignore, "nceas_ak_regions_simp.shp"))

# Fish Atlas
st_layers(dsn = file.path(dir.data.ignore, "fishatlas2020.gdb"))
sf_regions_3857 = st_read(dsn = file.path(dir.data.ignore, "fishatlas2020.gdb"), layer = "_REGIONS_FISHATLAS")

# Shorezone
st_layers(dsn = file.path(dir.data.ignore, "shorezone.gdb"))

# ADNR coastline maps
map_adnr_3338 = read_sf(dsn = file.path(dir.data.ignore, "adnr_ak_cst63", "adnr_ak_cst63.shp"))

## Fish
# Subset catch for genus abundance,
gen_abun = catch_qc %>%
  select(VisitID,
         EventID,
         Gen_ScientificName,
         Count) %>%
  group_by(VisitID, Gen_ScientificName) %>%
  # Visit abundance = all taxa caught averaged by no. of events
  summarise(Abundance = (sum(Count) / n_distinct(EventID)) %>% round(2)) %>%
  ungroup()

# and species abundance,
spp_abun = catch_qc %>%
  select(VisitID,
         EventID,
         Sp_ScientificName,
         Count) %>%
  group_by(VisitID, Sp_ScientificName) %>%
  # Visit abundance = all taxa caught averaged by no. of events
  summarise(Abundance = (sum(Count) / n_distinct(EventID)) %>% round(2)) %>%
  ungroup()


# Spatial EDA (maps) ------------------------------------------------------

# We have AK maps from 3 different sources,
# First, we have the NCEAS shp file..
map_nceas_4326 %>% head()
# get it in pseduo mercator
map_nceas_3857 = map_nceas_4326 %>% st_transform(crs = 3857)
# get it in AK albers
map_nceas_3338 = map_nceas_4326 %>% st_transform(crs = 3338)

# Second, we have ADNR coastline map- a relatively dense dataset
st_crs(map_adnr_3338)
# project to pseudo mercator
map_adnr_3857 = st_transform(map_adnr_3338, crs = 3857)

# Third, we have google maps,
# st_crs(gg_map_ak) # note no crs
# map_gg_ak_3857 = ggmap_bbox_to_3857(map_gg_ak)

# Let's also add in other AK shp files
st_crs(sf_regions_3857)
# get the nfa regions in  AK albers projection
sf_regions_3338 = sf_regions_3857 %>% st_transform(crs = 3338)

# Grab visit coordinates, make into sf object, project into both AK albers and pseudo mercator
sf_visits_4326 = visits_qc %>%
  select(VisitID, Lat, Lon) %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326, remove = FALSE)
st_crs(sf_visits_4326) # check
sf_visits_3857 = st_transform(sf_visits_4326, crs = 3857)
sf_visits_3338 = st_transform(sf_visits_4326, crs = 3338)

# visits on nceas mgmt map
ggplot() +
  geom_sf(data = map_nceas_3338, aes(fill = mgmt_area)) +
  geom_sf(data = sf_visits_3338, aes(), color = "red", size = 0.5)

# adnr map
ggplot() +
  geom_sf(data = map_adnr_3857, aes(), color = "black", alpha = 0) +
  geom_sf(data = sf_regions_3857, color = "blue", alpha = 0)


# visits with nfa regions on nceas map
ggplot() +
  geom_sf(data = sf_regions_3338, aes(fill = Region), alpha = 10) +
  geom_sf(data = map_nceas_3338, aes(), color = "grey20", alpha = 0) +
  geom_sf(data = sf_visits_3338, aes(), color = "red", size = 0.5)

# let's try joining the region and visits, using the ak albers proj
sf_joined_3857 = st_join(sf_visits_3857, sf_regions_3857, join = st_within)

plot(sf_joined_3857)

# Treatment of catch data -------------------------------------------------

## We'll take a look at catch distribution for each genera,
# and decide how to transform or standardize the data,
# the idea being that we want to remove effects of the largest catches
# but still allow for abundances to have some effect.

## Current data process (steps 1-3):
# 1a. "Weight" abundances by the 0.95 quantile for each genera
# 1b. Use raw abundances
# 2. Hellinger transformation (sqrt of abundance/rowsum(abundance)) -- NOT IMPLEMENTED YET!
# 3a. Calculate distance matrix using Bray-Curtis (weighted abundances)
# 3b. Calculate distance matrix using Robust Aitchison (raw abundances -> robust centered log ratio)

### Step 1: standardization

# First, set aside a temporary df to calculate different quantiles,
tmp = gen_abun %>%
  group_by(Gen_ScientificName) %>%
  mutate(q.75_abun = quantile(Abundance, 0.75, names = FALSE),
         k_abun = quantile(Abundance, 0.75, names = FALSE) + IQR(Abundance) * 1.5,
         q.90_abun = quantile(Abundance, 0.90, names = FALSE),
         q.95_abun = quantile(Abundance, 0.95, names = FALSE),
         max_abun = max(Abundance)) %>% 
  ungroup()

# Write a function to store boxplots for each genera abundance,
# marking optional distribution statistics to use,
plot_abun = function(x, var, xintercept = NULL) {
  names = str_which(names(x), "Name") %>% x[.] %>%
    unlist(use.names = FALSE) %>%
    unique() %>% sort()
  tmp_list = rep_len(list(), length.out = length(names))
  names(tmp_list) = names
  var = rlang::sym(var)
  if (!is.null(xintercept)) {
    xint = rlang::sym(xintercept)
    for (i in names) {
      tmp_list[[i]] = ggplot(data = filter(x, Gen_ScientificName == i),
                             aes(x = !! var)) +
        geom_boxplot() +
        geom_vline(aes(xintercept = !! xint), color = "red") +
        labs(title = i)
    }
  } else {
      for (i in names) {
        tmp_list[[i]] = ggplot(data = filter(x, Gen_ScientificName == i), aes(x = !! var)) +
          geom_boxplot() +
          labs(title = i)
      }
    }
}
# Run the function
plot_abun(x = tmp, var = "Abundance", xintercept = "q.95_abun")

# I like where the q.95_abun sits,
# so we'll transform abundances by the 95th quantile for each genus,
# I'll call this 'weighting' but this may not be accurate...
gen_abun_wt = gen_abun %>%
  group_by(Gen_ScientificName) %>%
  mutate(q.95_abun = quantile(Abundance, 0.95, names = FALSE)) %>% 
  ungroup() %>%
  mutate(wt_abun = Abundance/q.95_abun) %>%
  select(-Abundance, -q.95_abun)

# Plot abundance after transforming
p_gen_abun_wt = gen_abun_wt %>%
  group_by(Gen_ScientificName) %>%
  summarise(wt_abun_sum = sum(wt_abun)) %>%
  mutate(Gen_ScientificName = fct_reorder(as.factor(Gen_ScientificName), dplyr::desc(wt_abun_sum))) %>% 
  ggplot(aes(x = Gen_ScientificName, y = wt_abun_sum)) +
  geom_col() +
  geom_text(aes(label = round(wt_abun_sum, 0)), vjust = -0.5, size = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ggsave("nfa_gen_abun_wt.png", plot = p_gen_abun_wt, device = 'png', path = file.path(dir.figs))

# I want to check whether abundances look normal if we use the rclr pre-processing,
# for both raw and weighted abundances,
gen_abun_wide[-1] %>%
  decostand(method = "rclr") %>%
  pivot_longer(cols = everything(),
               names_to = "Gen_ScientificName",
               values_to = "log_abun") %>%
  filter(log_abun != 0) %>%
  plot_abun(var = "log_abun")

gen_abun_wt %>%
  pivot_longer(cols = 2:length(gen_abun_wide),
               names_to = "Gen_ScientificName",
               values_to = "log_abun") %>%
  filter(log_abun != 0) %>%
  plot_abun(var = "log_wt_abun")

# Pivot our data into wide format (and save),
# For transformed abundances
gen_abun_wt_wide = pivot_wider(gen_abun_wt,
            id_cols = VisitID,
            names_from = Gen_ScientificName, names_sort = TRUE,
            values_from = wt_abun, values_fill = 0)
# And un-transformed abundances
gen_abun_wide = pivot_wider(gen_abun,
                            id_cols = VisitID,
                            names_from = Gen_ScientificName, names_sort = TRUE,
                            values_from = Abundance, values_fill = 0)

### Step 2: transformation


### Step 3: distance calculation

# Presence/absence
# Jaccard index (metric)
gen_dist_jac = vegdist(x = gen_abun_wt_wide[-1], method = "jaccard", binary = TRUE)
# Sorenson
gen_dist_sor = vegdist(x = gen_abun_wt_wide[-1], method = "bray", binary = TRUE)
# Chao
gen_dist_chao = vegdist(x = gen_abun_wt_wide[-1], method = "chao", binary = TRUE)

# Abundances
# Bray-Curtis (semi-metric)
gen_dist_bray = vegdist(x = gen_abun_wt_wide[-1], method = "bray", binary = FALSE)
# Cao 
gen_dist_cao = vegdist(x = gen_abun_wide[-1], method = "cao", binary = FALSE)
# Raup-Grick
gen_dist_raup = vegdist(x = gen_abun_wt_wide[-1], method = "raup")
# Robust Aitchison (see Martino et al. 2019)
gen_dist_rait = vegdist(x = gen_abun_wide[-1], method = "robust.aitchison", binary = FALSE)

# Analysis of treated catch -----------------------------------------------

## Current method (steps 4 - ?):
# 4a. Run agglomertative algorithm, hclust(method = ward.D2) -- distances squared before cluster updates
# 4b. Run divisive algorithm, diana(k.stop.at = ?)
## Current analysis:
# 5. pvclust() -- 


hc = hclust(d = gen_dist_bray, method = "ward.D2")
dc = diana(x = gen_dist_bray, stop.at.k = 4)
?kmeans()

plot(dc)
