library(tidyverse)

tmp = cmi %>%
  filter(!SiteID %in% c('CMI2Hal', 'CMI3Hal')) %>%
  select(Site,
         Temperature_C,
         Salinity_psu,
         DO_mgl,
         DO_percent,
         Turbidity_fnu) %>%
  group_by(Site) %>%
  summarise(Temperature = mean(Temperature_C, na.rm = TRUE),
            sd.Temperature = sd(Temperature_C, na.rm = TRUE),
            Salinity = mean(Salinity_psu, na.rm = TRUE),
            sd.Salinity = sd(Salinity_psu, na.rm = TRUE),
            DO.mgL = mean(DO_mgl, na.rm = TRUE),
            sd.DO.mgL = sd(DO_mgl, na.rm = TRUE),
            DO.perc = mean(DO_percent, na.rm = TRUE),
            sd.DO.perc = sd(DO_percent, na.rm = TRUE),
            Turbidity = mean(Turbidity_fnu, na.rm = TRUE),
            sd.Turbidity = sd(Turbidity_fnu, na.rm = TRUE),           
            log.Turbidity = mean(log(Turbidity_fnu + 1), na.rm = TRUE),
            sd.log.Turbidity = sd(log(Turbidity_fnu + 1), na.rm = TRUE))
write.csv(tmp, "/Users/chrisguo/Desktop/Directory/KBNERR/Nearshore-year1/Figures/T1. env-measurements.csv", row.names = FALSE)

tmp = cmi %>%
  filter(!SiteID %in% c('CMI2Hal', 'CMI3Hal')) %>%
  select(Site, SiteID, SeineID)

# Tally number of site visits (SiteID) per site
tmp %>%
  select(Site, SiteID) %>%
  distinct() %>%
  group_by(Site) %>%
  tally()