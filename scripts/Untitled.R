library(ggmap)
library(tidyverse)

dir.data = file.path(wd,"data")
nfa.sites = read_csv(file = file.path(dir.data, "2022_nfa_sites.csv"))

map.ak = get_map(
  location = geocode("Alaska"),
  source = "google",
  maptype = "satellite",
  crop = FALSE,
  zoom = 4
)

ggmap(map.ak) +
  geom_point(data = nfa.sites, aes(x = Long, y = Lat), color = "red")
