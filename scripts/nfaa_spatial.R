library(tidyverse)
library(sf)

### NOTES
# Should try to simplify shorezone nodes to a reasonable distance... how to decide
# Fish migrate to different distances - use largest? estimate SZ unit length? subjective distance?
# Best way to account for this is functional trait for migration range - possible with FishLife?


# Directory
wd = here::here()
dirs = wd %>% list.files() %>% str_subset(pattern = "^README|^LICENSE|.md$|.Rproj$", negate = TRUE)
for (i in seq_along(dirs)) {
  name = str_replace_all(dirs[i], "^", "dir.")
  path = str_replace_all(dirs[i], "^", str_c(wd, "/"))
  assign(name, path)
  rm(name, path, i)
}

# Load NFAA data
load(file = file.path(dir.data, "nfaa_2.rda"))

# ADNR coastline map
adnr = read_sf(dsn = file.path(dir.data.ignore, "adnr_ak_cst63", "adnr_ak_cst63xsi.shp")) %>%
  # project to pseudo mercator
  st_transform(crs = 3857)

# View object
adnr; st_crs(adnr)

# Simplify
# adnr = st_simplify(adnr, preserveTopology = TRUE, dTolerance = 50)
# object.size(adnr) %>% format(units = "MB") # Check again

# Define bbox
# bbox = c(xmin = -180, ymin = 52, xmax = -125, ymax =  62) %>% st_bbox(crs = 4326) %>% st_as_sfc() %>% st_transform(crs = 3857) %>% st_bbox()

# Crop to bbox
# adnr = st_crop(adnr, bbox)

# Cast to linesting
# adnr = st_cast(adnr, "MULTILINESTRING")

# ADNR map
# ggplot(data = adnr) + aes() + geom_sf(color = "black")


# Fishbase
st_layers(dsn = file.path(dir.data.ignore, "fishatlas2020.gdb"))

# Grab regions layer
fishatlas = st_read(dsn = file.path(dir.data.ignore, "fishatlas2020.gdb"), layer = "_REGIONS_FISHATLAS")

# View object
fishatlas; st_crs(fishatlas)

# Filter for GOA and Aleutian regions
fishatlas = filter(fishatlas, Region %in% c('Aleutian Islands', 'Gulf of Alaska'))

# Fishbase region map
ggplot(data = fishatlas) +
  aes(fill = Region) +
  geom_sf()

# Merge the two regions into single polygon
fishatlas = st_union(fishatlas, by_feature = FALSE)
# Plot again to see merged polygon
ggplot(data = fishatlas) +
  aes() +
  geom_sf()

# Retain adnr coastline that intersects with goa regions
goa.coast = st_intersection(adnr, fishatlas)

# Don't need adnr ojbect anynmore,
rm(adnr)

# Check size
object.size(goa.coast) %>% format(units = "MB")

# View a simplified map of this
goa_map = st_simplify(goa.coast, preserveTopology = TRUE, dTolerance = 100) %>%
  ggplot(data = .) +
  aes() +
  geom_sf(color = "black")
goa_map





# ShoreZone
st_layers(dsn = file.path(dir.data.ignore, "shorezone.gdb"))

# There are few layers we want...
sz.mls = st_read(dsn = file.path(dir.data.ignore, "shorezone.gdb"), layer = "_AK_UNIT_LINES_WATTRS_1_1_1")
sz.pts = st_read(dsn = file.path(dir.data.ignore, "shorezone.gdb"), layer = "_AK_UNIT_BOUNDS")

# Remove columns
sz.mls = sz.mls %>%
  select(OBJECTID_1,
         PHY_IDENT,
         StartX, StartY,
         EndX, EndY,
         LENGTH_M,
         ESI,
         ORI,
         EXP_BIO,
         HabClass,
         HAB_CLASS_Name,
         Exposure,
         SHORETYPE)

# View line string object
sz.mls; st_crs(sz.mls)

# Same as with ADNR coastline, retain features that intersect with the GOA region
sz.mls = st_intersection(sz.mls, fishatlas)

# Check size
object.size(sz.mls) %>% format(units = "MB")




# The shorezone db is huge- so let's just grab the feature fields for our sample points:
# Read in samples -> assign geometry -> project into pseudo mercator crs 3857 ->
# snap to nearest shorezone feature -> add shorezone fields to samples -> export as tibble
# Use this ojbect for later analyses

# Create an vector of Region names for GOA and Aleutian samples
goa.regions = visits %>% pull(Region) %>% unique() %>% str_subset("GOA|Aleutians")

# Visit coordinates as point geometry
samples = visits %>%
  # Filter for Gulf of Alaska samples
  filter(Region %in% goa.regions) %>%
  # Create geometry from lat/lon in WGS 84
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>%
  # Project to pseudo mercator
  st_transform(crs = 3857)

# Generate map of samples
goa_map + 
  geom_sf(data = samples, aes(color = Region), alpha = 0.6) +
  coord_sf(expand = FALSE)

# Pull ID from shorezone nearest each sample
mls_ids = st_nearest_feature(samples, sz.mls)
# Filter shorezone to samples
sz.samples = filter(sz.mls, OBJECTID_1 %in% mls_ids)

# Add this reduced SZ layer to our map
goa_map + 
  geom_sf(data = sz.samples, color = "green") +
  coord_sf(expand = FALSE)

# Issues to address: some mls samples not in AK, not all samples have associated features- which ones?
# try other forms of st_join, like st_contains_properly, st_is_within_distance, etc





# Separately, we want to explore spatial auto-correlation with a variogram:
# Read in samples -> assign geometry -> project into pseudo mercator crs 3857;
# Create a geospatial network with samples (as new nodes) and coastline (edges) ->
# Try st_triangulate to create connected edges (possible routes) among nodes ->
# Calculate distances based off of this network -> produce variogram.

# Code for testing time to plot
bmplot = function(x) {
  plot = goa_map + 
  geom_sf(data = x, color = "green") +
  coord_sf(expand = FALSE)
  return(plot)
}
res = microbenchmark::microbenchmark(bmplot(sz.pts), unit = "seconds")
print(res)

# Plot shorezone coverage in the GOA
goa_map + 
  geom_sf(data = sz.pts, color = "green", alpha = 0.2) +
  coord_sf(expand = FALSE)

# Make an sfnetwork
library(sfnetworks)

# Cast shorezone from MultiLineString to LineString
sz.ls = st_cast(sz.mls, to = "LINESTRING")

# Create sf network from shorezone edges
net = as_sfnetwork(sz.ls, directed = FALSE)

# Quick plot of network
library(plotly)
plot_net = autoplot(net)
ggplotly(plot_net)





