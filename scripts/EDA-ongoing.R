# -----------------------------------------------------------------------
# Title: Exploration of nearshore fish data - all years
# Creator: Chris Guo
# Date: April 5, 2022
# Purpose: Script to explore patterns in data from all years

# Notes -------------------------------------------------------------------


# Load packages -----------------------------------------------------------


# Define workflow paths ---------------------------------------------------

wd = getwd()
dir.output = file.path(wd, "output")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd,"data")
dir.R = file.path(wd, "R")

# Utility -----------------------------------------------------------------

register_google(key = api.google, write = TRUE)
source(file = "../Desktop/Rlocal/scripts/utility.R")

# Read in data ------------------------------------------------------------

# Data collected
site.21a = read_csv(file = file.path(dir.data, "2021_kpfhp_site.csv"))
fish.21a = read_csv(file = file.path(dir.data, "2021_kpfhp_fish.csv"))
env.21a = read_csv(file = file.path(dir.data, "2021_kpfhp_env.csv"))
site.21b = read_csv(file = file.path(dir.data, "2021_nerrsc_site.csv"))
fish.21b = read_csv(file = file.path(dir.data, "2021_nerrsc_fish.csv"))
env.21b = read_csv(file = file.path(dir.data, "2021_nerrsc_env.csv"))
site.19 = read_csv(file = file.path(dir.data, "2019_kpfhp_site.csv"))
env.19 = read_csv(file = file.path(dir.data, "2019_kpfhp_env.csv"))
fish.19 = read_csv(file = file.path(dir.data, "2019_kpfhp_fish.csv"))
site.18 = read_csv(file = file.path(dir.data, "2018_cmi-kpfhp_site.csv"))
fish.18 = read_csv(file = file.path(dir.data, "2018_cmi-kpfhp_fish.csv"))
env.18 = read_csv(file = file.path(dir.data, "2018_cmi-kpfhp_env.csv"))

# Supplementary data
species_list = read_csv(file = file.path(dir.data, "species-list.csv"))
sites_list = read_csv(file = file.path(dir.data, "sites-list.csv"))

# Combine data ------------------------------------------------------------

## Sites
# Note: the only df that seems to differ in columns is 2019:
site.all = bind_rows(site.18, site.19, site.21a, site.21b)

## Environmentals
env.all = bind_rows(env.18, env.19, env.21a, env.21b)

## Fish
fish.all = bind_rows(fish.18, fish.19, fish.21a, fish.21b)

# Pacific Sand Lance ------------------------------------------------------

# Let's look at PSL from East Cook Inlet for now:
psl.1 = filter(fish.all, Common == 'Pacific Sand Lance') %>%
  left_join(site.all, by = "SeineID") %>%
  left_join(select(sites_list, Site, Area), by = "Site") %>% 
  mutate(Date = mdy(Date)) %>%
  filter(Area == 'East Cook Inlet') %>%
  select(Common, SeineID, Site, Date, LifeStage, Count, Length_mm)

# Looks like all ECI PSL are from 2021 except 1...

psl.2 = filter(psl.1, Date > '2021-03-30') # Removing the one 2019 PSL

# Some summary stats:
sum(psl.2$Count)
psl.mean.TL = mean(psl.2$Length_mm, na.rm = T)
  
# Plots shared with Yumi/Lindsay USGS
psl.2 %>%
  group_by(Site) %>%
  summarise(Count = sum(Count)) %>%
  ggplot(data = ., aes(x = Site, y = Count)) +
  geom_col() + labs(title = "1. PSL count by site")

ggplot(data = psl.2, aes(x = Length_mm)) +
  geom_bar() +
  facet_grid(. ~ Site) +
  labs(title = "2. PSL size freq by site")

ggplot(data = psl.2, aes(x = Length_mm)) +
  geom_bar() +
  geom_vline(xintercept = psl.mean.TL, color = 'red', linetype = 2) +
  labs(title = "3. PSL size freq with mean")

ggplot(data = psl.2, aes(x = Length_mm)) +
  geom_bar() +
  facet_grid(. ~ month(Date)) +
  labs(title = "4. PSL size freq by month")


