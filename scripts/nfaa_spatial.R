library(tidyverse)
library(sf)
library(smoothr)

# Directory
wd = here::here()
dirs = wd %>% list.files() %>% str_subset(pattern = "^README|^LICENSE|.md$|.Rproj$", negate = TRUE)
for (i in seq_along(dirs)) {
  name = str_replace_all(dirs[i], "^", "dir.")
  path = str_replace_all(dirs[i], "^", str_c(wd, "/"))
  assign(name, path)
  rm(name, path, i)
}


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


# Retain adnr coastline that intersects with goa regions
goa_coast = st_intersection(adnr, fishatlas)

# Check size
object.size(goa_coast) %>% format(units = "MB")

# View a simplified map of this
st_simplify(goa_coast, preserveTopology = TRUE, dTolerance = 100) %>%
  ggplot(data = .) +
  aes() +
  geom_sf(color = "black")






# ShoreZone
st_layers(dsn = file.path(dir.data.ignore, "shorezone.gdb"))

# Create a vector with the names of the layers we want to read in
sz.layers = st_layers(dsn = file.path(dir.data.ignore, "shorezone.gdb")) %>%
  pull(name) %>%
  str_subset("wXSHR")

# Read in data
shorezone = st_read(dsn = file.path(dir.data.ignore, "shorezone.gdb"), layer = sz.layers)

# Remove columns
shorezone = shorezone %>%
  select(OBJECTID_1,
         LENGTH_M,
         ESI,
         ORI,
         EXP_BIO,
         HabClass,
         HAB_CLASS_Name,
         SHORETYPE,
         ShoreType_desc)

# View object
shorezone; st_crs(shorezone)

# Same as with ADNR coastline, retain features that intersect with the GOA region
shorezone = st_intersection(shorezone, fishatlas)

# Check size
object.size(shorezone) %>% format(units = "MB")




# The shorezone db is huge- so let's just grab the feature fields for our sample points:
# Read in samples -> assign geometry -> project into pseudo mercator crs 3857 ->
# snap to nearest shorezone feature -> add shorezone fields to samples -> export as tibble
# Use this ojbect for later analyses

# Separately, we want to explore spatial auto-correlation with a variogram:
# Read in samples -> assign geometry -> project into pseudo mercator crs 3857;
# Create a geospatial network with samples (as new nodes) and coastline (edges) ->
# Try st_triangulate to create connected edges (possible routes) among nodes ->
# Calculate distances based off of this network -> produce variogram.


