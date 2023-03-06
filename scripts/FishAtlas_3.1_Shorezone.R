library(tidyverse)

tmp = read_csv(file = file.path(dir.data, "qgis_spatialjoin.csv"))
tmp2 = tmp %>%
  filter(Join_Count == 0) %>%
  select(SiteID) %>% distinct() %>%
  left_join(select(data, SiteID, Region, Location, PointOfContact, PI), by = "SiteID") %>% 
  distinct()
write_csv(x = tmp2, file = file.path(dir.output, "FishAtlas_Shorezone_sites-not-joined.csv"))

tmp = visits_qc %>%
  select(VisitID, Lat, Lon) %>%
  distinct()
write_csv(x = tmp, file = file.path(dir.output, "FishAtlas_Shorezone_sites-to-be-joined.csv"))

library(foreign)

tmp = read.dbf(file = file.path(dir.data, "FishAtlas_sites_1km_enriched.dbf"))
