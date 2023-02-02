library(tidyverse)

out = visits_qc %>%
  select(SiteID, Lat, Lon) %>%
  distinct()

write_csv(x = out,
          file = file.path(dir.output, "NFA_SiteID-Lat-Lon.csv"))

data %>%
  filter(SiteID == "1393") %>%
  select(Date, Lat, Long) %>%
  distinct() -> tmp1

visits_qc %>%
  filter(SiteID == "1393") %>%
  select(Date, Lat, Lon) %>%
  distinct() -> tmp2

library("XML")

snapped = xmlToDataFrame(file.path(dir.data, "FishAtlas_Shorezone_snapped-sites_2023.01.30.xml"))

library("foreign")

snapped = read.dbf(file.path(dir.data, "FishAtlas_Shorezone_snapped-sites_2023.01.30.dbf"))

tmp = read_csv(file = file.path(dir.data, "qgis_spatialjoin.csv"))
tmp2 = tmp %>%
  filter(Join_Count == 0) %>%
  select(SiteID) %>% distinct() %>%
  left_join(select(data, SiteID, Region, Location, PointOfContact, PI), by = "SiteID") %>% 
  distinct()
write_csv(x = tmp2, file = file.path(dir.output, "FishAtlas_Shorezone_sites-not-joined.csv"))
