library(rgdal)
library(sf)
library(tmap)
library(grid)
library(tidyverse)

file.AKcoast = "/Users/chguo/nearshore-fish-communities/2018_cmi/Data/KBcoast/mv_alaska_63360_xsi_py.shp"

AK.sub = readOGR(dsn = file.AKcoast)
proj4string(AK.sub) # not sure what datum we are in.. let's set it to be sure.
kbay = spTransform(AK.sub, CRS("+init=epsg:4326"))# set datum to WGS84 
proj4string(kbay) # checking 

kbay_bbox = st_bbox(c(xmin = -151.9, xmax = -150.9, # set long limits
                      ymin = 59.35, ymax = 59.82), # set lat limits
                    crs = 4326) %>% # set coordinate reference system
  st_as_sfc() # convert to sfc object

kbay.map = tm_shape(kbay, bbox = kbay_bbox) + 
  tm_borders() +
  tm_compass(type = "arrow", size = 1,
             position = c(.1, .925)) +
  tm_scale_bar(position = c(.2, .92))
kbay.map

# Make a simple AK map to project onto KBay map:
file.AKsimple = "/Users/chguo/nearshore-fish-communities/2018_cmi/Data/AKsimplified/mv_alaska_simplified_py.shp"
AK.simp = readOGR((dsn = file.AKsimple))

KB.loc <- st_bbox(c(xmin = 115000, xmax = 175000,
                    ymin = 1040000, ymax = 1095000),
                  crs = 3467) %>%
  st_as_sfc()

AK.inset = tm_shape(AK.simp) +
  tm_fill(col="grey") +
  tm_borders() +
  tm_shape(KB.loc) +
  tm_symbols(col = "black", shape = 15, size = .5, alpha = .65) +
  tm_layout(frame = FALSE, bg.color = "transparent")

# Set aside site coordinates:
sites = c("Barabara", "China Poot", "Glacier Spit", "Halibut Cove", "Seldovia Harbor", "Tutka Bay")
site.lat = c(59.481, 59.578, 59.653, 59.593, 59.435, 59.421)
site.lon = c(-151.659, -151.319, -151.192, -151.170, -151.714, -151.305)
site.table = cbind(sites, site.lat, site.lon)
site.table = as.data.frame(site.table)
site.coord = st_as_sf(site.table, coords = c("site.lon", "site.lat"))
st_crs(site.coord) = "+proj=longlat +datum=WGS84 +no_defs"

# Create a "Kachemak Bay" label
KB.label = data.frame(-151.65, 59.59, "Kachemak Bay", 8)
colnames(KB.label) <- c("Lon", "Lat", "Place", "Size")
KB.label = st_as_sf(KB.label, coords = c("Lon","Lat"))
st_crs(KB.label) = "+proj=longlat +datum=WGS84 +no_defs"

# Add all the pieces together
tm_shape(kbay, bbox = kbay_bbox) +
  tm_graticules(n.x = 3, n.y = 3, lines = FALSE) +
  tm_fill() +
  tm_borders(col = "darkgrey") +
  tm_shape(site.coord) +
  tm_symbols(size = .5, col = "black", alpha = .8) +
  tm_shape(site.coord) +
  tm_text("sites", size = .75, just = "left", xmod = .3, ymod = .2) +
  tm_compass(type = "4star", size = 2.5, position = c("left", "top"), just = c(0,1)) +
  tm_scale_bar(text.size = .5, position = c("right", "bottom"), just = c(1,0))
print(AK.inset, vp = viewport(0.4, 0.85, width = .3, height = .3))
plot = grDevices::recordPlot()

# Save the plot using tiff() and dev.off()
tiff(filename = "/Users/chguo/nearshore-fish-communities/2018_cmi/Figures/F1 Map.tiff",
     res = 500, width = 140, height = 102.6667, units = "mm" , pointsize = 12)
plot
dev.off()

# If this coiuld be done in ggsave()...
#ggsave(filename = "/Users/chguo/nearshore-fish-communities/2018_cmi/Figures/F1 Map.tiff",
#       plot = plot.grid, dpi = 500,
#       scale = 1, width = 140, height = 102.6667, units = "mm")
