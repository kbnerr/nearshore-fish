library(tidyverse)
library(chron)
library(lubridate)

## Read in MLLW tide data
Tides.Seldovia = read.csv(file = paste(getwd(), "/Data/NOAA Tide Data/Seldovia May to Nov 2018.txt", sep = ""), sep = "\t")
Tides.Seldovia = Tides.Seldovia %>% # Keep only the columns we are interested in
  select(Date, Time, Pred) %>%
  unite(datetime, 1:2, sep = " ", remove = FALSE) %>%
  rename(mllw.ft = Pred) %>%
  select(datetime, mllw.ft)
Tides.Seldovia$datetime = as.POSIXct(strptime(Tides.Seldovia$datetime, # Convert dates and time to POSIXct
                                              format = "%Y/%m/%d %H:%M",
                                              tz = "America/Anchorage"))

## Tutka Bay Data
Tutka = read.csv(file = paste(getwd(), "/Data/TCM1 Data/2018/Current/1803154_Tutka.txt", sep = ""))
Tutka = Tutka %>%
  rename(datetime = ISO.8601.Time,
         speed.cms = Speed..cm.s.,
         bearing.degrees = Bearing..degrees.,
         N.velocity = Velocity.N..cm.s.,
         E.velocity = Velocity.E..cm.s.)
Tutka$datetime = as.POSIXct(strptime(Tutka$datetime, format = "%Y-%m-%dT%H:%M", tz = "America/Anchorage"))

# ggplot(Tutka) + geom_line(aes(x = datetime, y = speed.cms)) # Visualize unfiltered dataset

Tutka.temp = read_csv(file = paste(getwd(), "/Data/Tidbit Data/Subtidal4_10243482.csv", sep = "")) # Read in temperature data
Tutka.temp = Tutka.temp %>%
  transmute(datetime = `Date Time, GMT-08:00`,
            temp = `Temp, °C (LGR S/N: 10243482, SEN S/N: 10243482)`)
Tutka.temp$datetime = as.POSIXct(strptime(Tutka.temp$datetime, format = "%m/%d/%y %H:%M", tz = "America/Anchorage"))

Tutka = Tutka %>% # Add columns for tidbit temperature, NOAA MLLW tide, and Site ID
  left_join(., Tutka.temp, by = "datetime") %>%
  left_join(., Tides.Seldovia, by = "datetime") %>%
  add_column(., Site = "Tutka Bay", .before = 2)

Tutka.fil = Tutka %>% # Filter dataset
  filter(datetime >= '2018-05-21 00:00:00' & datetime <= '2018-10-24 23:45:00') %>%   # Trim deploy and retrieval times
  mutate(   # Adding a column conditional for handling time
    drop.na = case_when(     
      datetime < '2018-05-31 13:00:00' ~ TRUE,
      datetime >= '2018-05-31 13:00:00' & datetime <= '2018-05-31 14:00:00' ~ NA,  # Handling time
      datetime > '2018-05-31 14:00:00' & datetime < '2018-06-23 14:00:00' ~ TRUE,
      datetime >= '2018-06-23 14:00:00' & datetime <= '2018-06-23 14:45:00' ~ NA,  # Handling time
      datetime > '2018-06-23 14:45:00' ~ TRUE
    )) %>%
  drop_na(drop.na) %>%   # Removing rows defined as handling time
  select(-drop.na) %>%  # Remove the conditional column for handling time from the dataset
  filter(mllw.ft >= -1)  # Remove data compromised by low tides

# ggplot(Tutka.fil) + geom_line(aes(x = datetime, y = speed.cms)) # Visualize filtered data

## Halibut Cove Data
Halibut = read.csv(file = paste(getwd(), "/Data/TCM1 Data/2018/Current/1803155_Halibut.txt", sep = ""))
Halibut = Halibut %>%
  rename(datetime = ISO.8601.Time,
         speed.cms = Speed..cm.s.,
         bearing.degrees = Bearing..degrees.,
         N.velocity = Velocity.N..cm.s.,
         E.velocity = Velocity.E..cm.s.)
Halibut$datetime = as.POSIXct(strptime(Halibut$datetime, format = "%Y-%m-%dT%H:%M", tz = "America/Anchorage"))

# ggplot(Halibut) + geom_line(aes(x = datetime, y = speed.cms)) # Visualize unfiltered dataset

Halibut.temp = read_csv(file = paste(getwd(), "/Data/Tidbit Data/Subtidal2_10243483.csv", sep = "")) # Read in temperature data
Halibut.temp = Halibut.temp %>%
  transmute(datetime = `Date Time, GMT-08:00`,
            temp = `Temp, °C (LGR S/N: 10243483, SEN S/N: 10243483)`)
Halibut.temp$datetime = as.POSIXct(strptime(Halibut.temp$datetime, format = "%m/%d/%y %H:%M", tz = "America/Anchorage"))

Halibut = Halibut %>% # Add columns for tidbit temperature, NOAA MLLW tide, and Site ID
  left_join(., Halibut.temp, by = "datetime") %>%
  left_join(., Tides.Seldovia, by = "datetime") %>%
  add_column(., Site = "Halibut Cove", .before = 2)

Halibut.fil = Halibut %>% # Filter dataset
  filter(datetime >= '2018-05-21 00:00:00' & datetime <= '2018-10-24 23:45:00') %>%   # Trim deploy and retrieval times
  mutate( # Adding a column conditional for handling time
    drop.na = case_when(     
      datetime < '2018-05-31 09:00:00' ~ TRUE,
      datetime >= '2018-05-31 09:00:00' & datetime <= '2018-05-31 10:00:00' ~ NA,  # Handling time
      datetime > '2018-05-31 10:00:00' & datetime < '2018-06-23 14:00:00' ~ TRUE,
      datetime >= '2018-06-23 14:00:00' & datetime <= '2018-06-23 15:00:00' ~ NA,  # Handling time
      datetime > '2018-06-23 15:00:00' & datetime < '2018-07-13 08:30:00' ~ TRUE,
      datetime >= '2018-07-13 08:30:00' & datetime <= '2018-08-13 11:15:00' ~ NA,  # Compromised due to low tides
      datetime > '2018-08-13 11:15:00' & datetime < '2018-09-06 10:45:00' ~ TRUE,
      datetime >= '2018-09-06 10:45:00' & datetime <= '2018-09-06 11:45:00' ~ NA,  # Handling time
      datetime > '2018-09-06 11:45:00' & datetime < '2018-09-10 09:30:00' ~ TRUE,
      datetime >= '2018-09-10 09:30:00' & datetime <= '2018-09-24 19:15:00' ~ NA,  # Compromised due to low tides
      datetime > '2018-09-24 19:15:00' ~ TRUE
    )) %>%
  drop_na(drop.na) %>%   # Removing rows defined as handling time
  select(-drop.na) %>%  # Remove the conditional column for handling time from the dataset
  filter(mllw.ft >= -0.5)  # Remove data compromised by low tides

# ggplot(Halibut.fil) + geom_line(aes(x = datetime, y = speed.cms)) # Visualize filtered data

## Seldovia Harbor Data
Seldovia = read.csv(file = paste(getwd(), "/Data/TCM1 Data/2018/Current/1805203_Seldovia.txt", sep = ""))
Seldovia = Seldovia %>%
  rename(datetime = ISO.8601.Time,
         speed.cms = Speed..cm.s.,
         bearing.degrees = Bearing..degrees.,
         N.velocity = Velocity.N..cm.s.,
         E.velocity = Velocity.E..cm.s.)
Seldovia$datetime = as.POSIXct(strptime(Seldovia$datetime, format = "%Y-%m-%dT%H:%M", tz = "America/Anchorage"))

# ggplot(Seldovia) + geom_line(aes(x = datetime, y = speed.cms))  # Visualize unfiltered dataset

Seldovia.temp = read_csv(file = paste(getwd(), "/Data/Tidbit Data/Subtidal6_10255554.csv", sep = "")) # Read in temperature data
Seldovia.temp = Seldovia.temp %>%
  transmute(datetime = `Date Time, GMT-08:00`,
            temp = `Temp, °C (LGR S/N: 10255554, SEN S/N: 10255554)`)
Seldovia.temp$datetime = as.POSIXct(strptime(Seldovia.temp$datetime, format = "%m/%d/%y %H:%M", tz = "America/Anchorage"))

Seldovia = Seldovia %>% # Add columns for tidbit temperature, NOAA MLLW tide, and Site ID
  left_join(., Seldovia.temp, by = "datetime") %>%
  left_join(., Tides.Seldovia, by = "datetime") %>%
  add_column(., Site = "Seldovia Harbor", .before = 2)

Seldovia.fil = Seldovia %>% # Filter dataset
  filter(datetime >= '2018-05-21 00:00:00' & datetime <= '2018-10-24 23:45:00') %>%   # Trim deploy and retrieval times
  mutate(   # Adding a column conditional for handling time
    drop.na = case_when(     
      datetime < '2018-05-29 11:15:00' ~ TRUE,
      datetime >= '2018-05-29 11:15:00' & datetime <= '2018-05-29 12:15:00' ~ NA,  # Handling time
      datetime > '2018-05-29 12:15:00' & datetime < '2018-06-15 12:30:00' ~ TRUE,
      datetime >= '2018-06-15 12:30:00' & datetime <= '2018-06-15 19:15:00' ~ NA,  # Erroneous data... due to animal?
      datetime > '2018-06-15 19:15:00' & datetime < '2018-06-23 16:15:00' ~ TRUE,
      datetime >= '2018-06-23 16:15:00' & datetime <= '2018-06-23 17:15:00' ~ NA,  # Handling time
      datetime > '2018-06-23 17:15:00' ~ TRUE
    )) %>%
  drop_na(drop.na) %>%   # Removing rows defined as handling time
  select(-drop.na) %>%  # Remove the conditional column for handling time from the dataset
  filter(mllw.ft >= -0.5)  # Remove data compromised by low tides

# ggplot(Seldovia.fil) + geom_line(aes(x = datetime, y = speed.cms)) # Visualize filtered data

## Barabara Data
Barabara = read.csv(file = paste(getwd(), "/Data/TCM1 Data/2018/Current/1803158_Barabara.txt", sep = ""))
Barabara = Barabara %>%
  rename(datetime = ISO.8601.Time,
         speed.cms = Speed..cm.s.,
         bearing.degrees = Bearing..degrees.,
         N.velocity = Velocity.N..cm.s.,
         E.velocity = Velocity.E..cm.s.)
Barabara$datetime = as.POSIXct(strptime(Barabara$datetime, format = "%Y-%m-%dT%H:%M", tz = "America/Anchorage"))

# ggplot(Barabara) + geom_line(aes(x = datetime, y = speed.cms)) # Visualize unfiltered dataset

Barabara.temp = read_csv(file = paste(getwd(), "/Data/Tidbit Data/Subtidal3_9913517.csv", sep = "")) # Read in temperature data
Barabara.temp = Barabara.temp %>%
  transmute(datetime = `Date Time, GMT-08:00`,
            temp = `Temp, °C (LGR S/N: 9913517, SEN S/N: 9913517)`)
Barabara.temp$datetime = as.POSIXct(strptime(Barabara.temp$datetime, format = "%m/%d/%y %H:%M", tz = "America/Anchorage"))

Barabara = Barabara %>% # Add columns for tidbit temperature, NOAA MLLW tide, and Site ID
  left_join(., Barabara.temp, by = "datetime") %>%
  left_join(., Tides.Seldovia, by = "datetime") %>%
  add_column(., Site = "Barabara", .before = 2)

Barabara.fil = Barabara %>% # Filter dataset
  filter(datetime >= '2018-05-21 00:00:00' & datetime <= '2018-10-24 23:45:00') %>%   # Trim deploy and retrieval times
  mutate(   # Adding a column conditional for handling time
    drop.na = case_when(     
      datetime < '2018-05-29 14:15:00' ~ TRUE,
      datetime >= '2018-05-29 14:15:00' & datetime <= '2018-05-29 15:15:00' ~ NA,  # Handling time
      datetime > '2018-05-29 15:15:00' & datetime < '2018-06-23 15:15:00' ~ TRUE,
      datetime >= '2018-06-23 15:15:00' & datetime <= '2018-06-23 16:00:00' ~ NA,  # Handling time
      datetime > '2018-06-23 16:00:00' ~ TRUE
    )) %>%
  drop_na(drop.na) %>%   # Removing rows defined as handling time
  select(-drop.na) %>%  # Remove the conditional column for handling time from the dataset
  filter(mllw.ft >= -3)  # Remove data compromised by low tides

# ggplot(Barabara.fil) + geom_line(aes(x = datetime, y = speed.cms)) # Visualize filtered data

## China Poot Data
ChinaPoot = read.csv(file = paste(getwd(), "/Data/TCM1 Data/2018/Current/1803157_ChinaPoot.txt", sep = ""))
ChinaPoot = ChinaPoot %>%
  rename(datetime = ISO.8601.Time,
         speed.cms = Speed..cm.s.,
         bearing.degrees = Bearing..degrees.,
         N.velocity = Velocity.N..cm.s.,
         E.velocity = Velocity.E..cm.s.)
ChinaPoot$datetime = as.POSIXct(strptime(ChinaPoot$datetime, format = "%Y-%m-%dT%H:%M", tz = "America/Anchorage"))

# ggplot(ChinaPoot) + geom_line(aes(x = datetime, y = speed.cms))  # Visualize unfiltered dataset

ChinaPoot.temp = read_csv(file = paste(getwd(), "/Data/Tidbit Data/Subtidal5_10177550.csv", sep = "")) # Read in temperature data
ChinaPoot.temp = ChinaPoot.temp %>%
  transmute(datetime = `Date Time, GMT-08:00`,
            temp = `Temp, °C (LGR S/N: 10177550, SEN S/N: 10177550)`)
ChinaPoot.temp$datetime = as.POSIXct(strptime(ChinaPoot.temp$datetime, format = "%m/%d/%y %H:%M", tz = "America/Anchorage"))

ChinaPoot = ChinaPoot %>% # Add columns for tidbit temperature, NOAA MLLW tide, and Site ID
  left_join(., ChinaPoot.temp, by = "datetime") %>%
  left_join(., Tides.Seldovia, by = "datetime") %>%
  add_column(., Site = "China Poot", .before = 2)

ChinaPoot.fil = ChinaPoot %>% # Filter dataset
  filter(datetime >= '2018-05-21 00:00:00' & datetime <= '2018-10-24 23:45:00') %>%   # Trim deploy and retrieval times
  mutate(   # Adding a column conditional for handling time
    drop.na = case_when(     
      datetime < '2018-05-31 10:30:00' ~ TRUE,
      datetime >= '2018-05-31 10:30:00' & datetime <= '2018-05-31 11:15:00' ~ NA,  # Handling time
      datetime > '2018-05-31 11:15:00' & datetime < '2018-06-23 12:45:00' ~ TRUE,
      datetime >= '2018-06-23 12:45:00' & datetime <= '2018-06-23 13:30:00' ~ NA,  # Handling time
      datetime > '2018-06-23 13:30:00' ~ TRUE
    )) %>%
  drop_na(drop.na) %>%   # Removing rows defined as handling time
  select(-drop.na) %>%  # Remove the conditional column for handling time from the dataset
  filter(mllw.ft >= -3)  # Remove data compromised by low tides

# ggplot(ChinaPoot.fil) + geom_line(aes(x = datetime, y = speed.cms))  # Visualize filtered data

## Glacier Spit Data
GlacierSpit = read.csv(file = paste(getwd(), "/Data/TCM1 Data/2018/Current/1803156_GlacierSpit.txt", sep = ""))
GlacierSpit = GlacierSpit %>%
  rename(datetime = ISO.8601.Time,
         speed.cms = Speed..cm.s.,
         bearing.degrees = Bearing..degrees.,
         N.velocity = Velocity.N..cm.s.,
         E.velocity = Velocity.E..cm.s.)
GlacierSpit$datetime = as.POSIXct(strptime(GlacierSpit$datetime, format = "%Y-%m-%dT%H:%M", tz = "America/Anchorage"))

# ggplot(GlacierSpit) + geom_line(aes(x = datetime, y = speed.cms))  # Visualize unfiltered dataset

GlacierSpit = GlacierSpit %>% # Add columns for NOAA MLLW tide and Site ID
  left_join(., Tides.Seldovia, by = "datetime") %>%
  add_column(., Site = "Glacier Spit", .before = 2)

GlacierSpit.fil = GlacierSpit %>% # Filter dataset
  filter(datetime >= '2018-05-21 00:00:00') %>%   # Trim deploy and retrieval times
  mutate(   # Adding a column conditional for handling time
    drop.na = case_when(     
      datetime <= '2018-05-21 15:00:00' ~ NA,  # Compromised data... unsure why
      datetime > '2018-05-21 15:00:00' & datetime < '2018-05-31 09:45:00' ~ TRUE,
      datetime >= '2018-05-31 09:45:00' & datetime <= '2018-05-31 10:30:00' ~ NA,  # Handling time
      datetime > '2018-05-31 10:30:00' & datetime < '2018-06-12 08:15:00' ~ TRUE,
      datetime >= '2018-06-12 08:15:00' & datetime <= '2018-06-14 10:30:00' ~ NA,  # Compromised due to low tides
      datetime > '2018-06-14 10:30:00' & datetime < '2018-06-19 13:15:00' ~ TRUE,
      datetime >= '2018-06-19 13:15:00' ~ NA  # Meter buried in sediment
    )) %>%
  drop_na(drop.na) %>%   # Removing rows defined as handling time
  select(-drop.na) %>%  # Remove the conditional column for handling time from the dataset
  filter(mllw.ft >= -2.5)  # Remove data compromised by low tides

# ggplot(GlacierSpit.fil) + geom_line(aes(x = datetime, y = speed.cms)) # Visualize filtered data

## Combine filtered datasets
flow.all = bind_rows(Barabara.fil,
                     ChinaPoot.fil,
                     GlacierSpit.fil,
                     Halibut.fil,
                     Seldovia.fil,
                     Tutka.fil)

flow.all = flow.all %>% rename(Speed = speed.cms,
                               Bearing = bearing.degrees)

## Clean up workspace
rm(Barabara, Barabara.fil, Barabara.temp,
   ChinaPoot, ChinaPoot.fil, ChinaPoot.temp,
   GlacierSpit, GlacierSpit.fil,
   Halibut, Halibut.fil, Halibut.temp,
   Seldovia, Seldovia.fil, Seldovia.temp,
   Tutka, Tutka.fil, Tutka.temp,
   Tides.Seldovia)

## Create a new dataset trimmed to time frame where all sites overlap
flow.trim = flow.all %>% filter(datetime >= '2018-05-21 15:15:00' & datetime <= '2018-06-19 13:00:00')
ggplot(flow.trim, aes(datetime, Speed, color = Site)) + # visualize the trimmed data
  geom_smooth() +
  labs(x = "Date", y = "Speed (cm/s)") +
  theme(legend.position = "bottom")

flow.trim = flow.trim %>%
  group_by(Site) %>%
  mutate(CstExp = case_when(Site %in% c("Barabara", "China Poot", "Glacier Spit") ~ "High",
                            Site %in% c("Halibut Cove", "Seldovia Harbor", "Tutka Bay") ~ "Low"))

# t-tests of means
flow.trim$CstExp = ordered(flow.trim$CstExp, levels = c("High", "Low"))
t.test(Speed ~ CstExp, flow.trim)

pairwise.t.test(flow.trim$Speed, flow.trim$Site, p.adjust.method = "none")
pairwise.t.test(flow.trim$Speed, flow.trim$Site, p.adjust.method = "bonf")
pairwise.t.test(flow.trim$Speed, flow.trim$Site, p.adjust.method = "holm")

# One-way ANOVA test
flow.lm = lm(Speed ~ CstExp, data = flow.trim)
summary(flow.lm)
anova(flow.lm)
(flow.tukey = TukeyHSD(aov(Speed ~ Site, flow.trim)))
plot(flow.tukey)

flow.lm.2 = lm(Speed ~ Site, data = flow.trim)
summary(flow.lm.2)
anova(flow.lm.2)

# Levene's test for homogeniety
library(car)
leveneTest(Speed ~ CstExp, center = median, data = flow.trim)
leveneTest(Speed ~ CstExp, center = mean, data = flow.trim)
leveneTest(Speed ~ CstExp*Site, center = median, data = filter(flow.trim, CstExp == "High"))
leveneTest(Speed ~ CstExp*Site, center = median, data = filter(flow.trim, CstExp == "Low"))

# Pairwise Levene test:
# code from https://stackoverflow.com/questions/43646987/multiple-comparison-post-hoc-test-for-levenes-test
tmp = flow.trim
tmp = tmp %>%
  drop_na() %>%
  group_by(Site) %>% 
  mutate(Speed.med = median(Speed, na.rm = TRUE))
tmp$Speed.med.res <- abs(tmp$Speed - tmp$Speed.med)
# Then we run an ANOVA, and post-hoc if necessary:
levene.Speed.aov <- aov(Speed.med.res ~ Site, data = tmp)
summary(levene.Speed.aov)
TukeyHSD(levene.Speed.aov)
# The only site pair not significant is Tutka Bay - Halibut Cove

# Plotting
flow.cols = flow.trim %>%
  group_by(Site) %>%
  summarise(FS = mean(Speed),
            sd.FS = sd(Speed)) %>%
  mutate(var.FS = sd.FS^2,
         CstExp = case_when(Site %in% c("Barabara", "China Poot", "Glacier Spit") ~ "High",
                          Site %in% c("Halibut Cove", "Seldovia Harbor", "Tutka Bay") ~ "Low"))

ggplot(flow.trim) + # visualize the trimmed data
  stat_smooth(aes(x = datetime, y = Speed, color = Site)) +
  labs(x = "Date", y = "Speed (cm/s)") +
  scale_color_manual(values = flow.colors,
                     guide = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(legend.position = c(.30, .925),
        legend.direction = "horizontal",
        text = element_text(family = "Times",
                            size = 12))

ggplot(flow.trim) + # visualize the trimmed data in black & white
  stat_smooth(aes(x = datetime, y = Speed, color = Site, linetype = Site),
              fill = "dimgray") +
  labs(x = "Date", y = "Speed (cm/s)") +
  scale_color_manual(values = flow.bw,
                     name = "Site",
                     guide = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_linetype_manual(values = flow.linetypes,
                        name = "Site") +
  theme_bw() +
  theme(legend.position = c(.20, .925),
        legend.direction = "horizontal",
        legend.background = element_rect(colour = "black", linetype = "solid", size = 0.25),
        text = element_text(family = "Times",
                            size = 12))

ggplot(flow.trim) + # visualize the trimmed data in black & white
  stat_smooth(aes(x = datetime, y = Speed, color = Site, linetype = CstExp),
              fill = "dimgray") +
  labs(x = "Date", y = "Speed (cm/s)") +
  scale_color_manual(values = site.colors.cb2,
                     name = "Site, Current Type",
                     labels = F2.labels) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE,
                              override.aes = list(linetype = F2.linetype.override))) +
  scale_linetype(guide = FALSE) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.justification = c(0,1),
        legend.position = c(.025, .975),
        legend.direction = "vertical",
        legend.background = element_rect(colour = "black", linetype = "solid", size = 0.25))

tmp <-
ggplot(data = flow.trim, aes(x = datetime, y = Speed)) + # visualize the trimmed data in black & white
  stat_smooth(aes(color = Site, linetype = CstExp),
              fill = "dimgray") +
  scale_color_manual(values = c(rep("#000000", 6)), guide = FALSE) +
  labs(x = "Date", y = "Speed (cm/s)") +
  guides(linetype = guide_legend(title = "Current Type", nrow = 1, byrow = TRUE,
                                 override.aes = list(color = "#000000"))) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.justification = c(0,1),
        legend.position = c(.025, .975),
        legend.direction = "vertical",
        legend.background = element_rect(colour = "black", linetype = "solid", size = 0.25))

tmp2 = ggplot_build(tmp)$data[[1]]
tmp3 = tmp2 %>%
  group_by(group) %>%
  filter(x == max(x)) %>%
  add_column(label = c("Barabara", "China Poot", "Glacier Spit", "Halibut Cove", "Seldovia Harbor", "Tutka Bay"))

tmp + annotate("text", x = as.POSIXct(tmp3$x, origin = origin), y = tmp3$y, label = tmp3$label, hjust = -.05) +
  coord_cartesian(xlim = as.POSIXct(c('2018-05-21','2018-06-25')))

ggsave(filename = "/Users/chguo/nearshore-fish-communities/2018_cmi/Figures/F2. Water speeds new.tiff",
       plot = last_plot(),
       width = 90, height = 90, units = "mm",
       scale = 2.0,
       dpi = 500)


