# -----------------------------------------------------------------------
# Project: Diversity  of nearshore fish communities
# Creator: Chris Guo
# Date: 2021.11.07
# Purpose: Calculate and explore diversity indices in NOAA's Nearshore Fish Atlas database


# Notes -------------------------------------------------------------------

# Much of the script is borrowed from Kindt and Roe 2005 BiodiversityR
# Walk through by Kindt is found at https://rpubs.com/Roeland-KINDT/694066

# For more measures of diversity, check out package 'diverse'.

## For BiodiversityR::renyicomp(), look up which Simpson index is calculated at scale 2

## When looking at R.reg, Bristol Bay is NA, and maybe combine PWS into southcentral

# Load packages -----------------------------------------------------------

library(BiodiversityR) # includes vegan
library(tidyverse)
library(lubridate)
library(ggsci)


# Utility -----------------------------------------------------------------

theme.BioR = theme(
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.line = element_line("gray25"),
  text = element_text(size = 12),
  axis.text = element_text(size = 10, colour = "gray25"),
  axis.title = element_text(size = 14, colour = "gray25"),
  legend.title = element_text(size = 14),
  legend.text = element_text(size = 14),
  legend.key = element_blank()
)

winter = c("December", "January", "February")
spring = c("March", "April", "May")
summer = c("June", "July", "August")
fall = c("September", "October", "November")

# Define workflow paths ---------------------------------------------------

wd = getwd()
dir.output = file.path(wd, "output")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd,"data")
dir.R = file.path(wd,"R")

# Create
# dir.create(dir.figs, recursive=TRUE)
# dir.create(dir.output, recursive=TRUE)

# Read in Data =================================================================

# Event-level beach seine counts for AK-wide
dat1 = read.csv(file.path(dir.data, "NFA_beach-seines_counts.csv"),
                header = TRUE, colClasses = "numeric")
head(dat1)
# Let's make another df without the EventIDs:
bs.counts.ak = dat1[ , -1] # This includes AK-wide data

# Event-level site information for AK-wide
dat2 = read.csv(file.path(dir.data, "NFA_beach-seines_site-info.csv"), header = TRUE)
# Let's make our categories into class factor,
# and drop EventID and lat/lon's:
bs.env.ak = dat2 %>%
  mutate(Region = as.factor(Region),
         Location = as.factor(Location),
         Habitat = as.factor(Habitat),
         Month = as.factor(month(Date, label = TRUE, abbr = FALSE)),
         Season = as.factor(case_when(Month %in% winter ~ "Winter",
                                      Month %in% spring ~ "Spring",
                                      Month %in% summer ~ "Summer",
                                      Month %in% fall ~ "Fall"))
         ) %>%
  select(-EventID,
         -Date,
         -Lat,
         -Lon)


## Arctic samples

bs.env.arc = bs.env.ak %>%
  mutate(tmp = rownames(.)) %>%
  filter(Region == "Arctic") %>%
  mutate(Month = as.factor(as.double(Month)),
         Season = as.factor(as.double(Season)))

bs.counts.arc = bs.counts.ak %>%
  mutate(tmp = rownames(.)) %>%
  semi_join(bs.env.arc, by = 'tmp') %>%
  select(-tmp)

bs.env.arc = select(bs.env.arc, -tmp, -Region)


## Southcentral samples

bs.env.scak = bs.env.ak %>%
  mutate(tmp = rownames(.)) %>%
  filter(Region == "southcentral Alaska") %>%
  mutate(Month = as.factor(as.double(Month)),
         Season = as.factor(as.double(Season)))

bs.counts.scak = bs.counts.ak %>%
  mutate(tmp = rownames(.)) %>%
  semi_join(bs.env.scak, by = 'tmp') %>%
  select(-tmp)

bs.env.scak = select(bs.env.scak, -tmp, -Region)


## Southeast samples

bs.env.seak = bs.env.ak %>%
  mutate(tmp = rownames(.)) %>%
  filter(Region == "southeastern Alaska") %>%
  mutate(Month = as.factor(as.double(Month)),
         Season = as.factor(as.double(Season)))

bs.counts.seak = bs.counts.ak %>%
  mutate(tmp = rownames(.)) %>%
  semi_join(bs.env.seak, by = 'tmp') %>%
  select(-tmp)

bs.env.seak = select(bs.env.seak, -tmp, -Region)


# Vegan -------------------------------------------------------------------

## All AK regions

# Shannon-Weaver
H = diversity(bs.counts.ak, index = "shannon") # b = ln(), see help file

# Simpson's
D1 = diversity(bs.counts.ak, index = "simpson") # 1 - D
D2 = diversity(bs.counts.ak, index = "invsimpson") # 1/D

# Species richness
S = specnumber(bs.counts.ak) # rowSums(x > 0) can replace specnumber() 

# Pielou's evenness
J = H/log(S) 

# Comparing Shannon, Simpsons, and inverse Simpsons
pairs(cbind(H, D1, D2), pch = "+", col = "blue")

# Rényi diversity
k = sample(nrow(bs.counts.ak), 18)
R = renyi(bs.counts.ak[k, ])
plot(R)

## At scale 0, reflects species richness
## At scale 1, reflects Shannon (H)
## At scale 2, reflects Simpson (?)
## At scale Inf, reflects Berger-Parker


# BiodiversityR -----------------------------------------------------------

## Factor group: Region

R.reg = renyicomp(bs.counts.ak, y = bs.env.ak, factor = 'Region',
                  scales = c(0, 0.25, 0.5, 1, 2, 4, 8, Inf),
                  permutations = 1000,
                  plotit = FALSE)

R.reg

R.reg.long = renyicomp.long(R.reg, label.freq = 1)
head(R.reg.long)

# Region: with upper/lower bounds
ggplot(data = R.reg.long, aes(x = Scales, y = Diversity, ymax = UPR, ymin = LWR)) + 
  scale_x_discrete() +
  scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = NULL)) +
  geom_line(aes(x = Obs, colour = Grouping), 
            size = 1) +
  geom_point(data = subset(R.reg.long, labelit == TRUE), 
             aes(colour = Grouping, shape = Grouping), 
             size = 3) +
  geom_ribbon(aes(x = Obs, colour = Grouping, fill = after_scale(alpha(colour, 0.1))), 
              show.legend = FALSE) + 
  theme.BioR +
  scale_colour_npg() +
  labs(x = expression(alpha), y = "Diversity", colour = "Region", shape = "Region") -> R.reg.plot1

R.reg.plot1

ggsave(filename = "Renyi-Region-1",
       path = file.path(dir.figs),
       plot = R.reg.plot1,
       device = "png")
dev.off()

# Region: without upper/lower bounds

ggplot(data = R.reg.long, aes(x = Scales, y = Diversity, ymax = UPR, ymin = LWR)) + 
  scale_x_discrete() +
  scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = NULL)) +
  geom_line(aes(x = Obs, colour = Grouping), 
            size = 1) +
  geom_point(data = subset(R.reg.long, labelit == TRUE), 
             aes(colour = Grouping, shape = Grouping), 
             alpha = 0.8, size = 3) +
  theme.BioR +
  scale_colour_npg() +
  labs(x = expression(alpha), y = "Diversity", colour = "Region", shape = "Region") -> R.reg.plot2

R.reg.plot2

ggsave(filename = "Renyi-Region-2",
       path = file.path(dir.figs),
       plot = R.reg.plot2,
       device = "png")
dev.off()

## Factor group: Season

bs.env.ak %>%
  group_by(Season) %>%
  summarise(n = n())
# Winter n = 13

R.sea = renyicomp(bs.counts.ak, y = bs.env.ak, factor = 'Season',
                  scales = c(0, 0.25, 0.5, 1, 2, 4, 8, Inf),
                  plotit = FALSE)

R.sea

R.sea.long = renyicomp.long(R.sea, label.freq = 1)
head(R.sea.long)

# Season: with upper/lower bounds
ggplot(data = R.sea.long, aes(x = Scales, y = Diversity, ymax = UPR, ymin = LWR)) + 
  scale_x_discrete() +
  scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = NULL)) +
  geom_line(aes(x = Obs, colour = Grouping), 
            size = 1) +
  geom_point(data = subset(R.sea.long, labelit == TRUE), 
             aes(colour = Grouping, shape = Grouping), 
             size = 3) +
  geom_ribbon(aes(x = Obs, colour = Grouping, fill = after_scale(alpha(colour, 0.1))), 
              show.legend = FALSE) + 
  theme.BioR +
  scale_colour_npg() +
  labs(x = expression(alpha), y = "Diversity", colour = "Season", shape = "Season") -> R.sea.plot1

R.sea.plot1

ggsave(filename = "Renyi-Season-1",
       path = file.path(dir.figs),
       plot = R.sea.plot1,
       device = "png")
dev.off()

# Season: without upper/lower bounds

ggplot(data = R.sea.long, aes(x = Scales, y = Diversity, ymax = UPR, ymin = LWR)) + 
  scale_x_discrete() +
  scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = NULL)) +
  geom_line(aes(x = Obs, colour = Grouping), 
            size = 1) +
  geom_point(data = subset(R.sea.long, labelit == TRUE), 
             aes(colour = Grouping, shape = Grouping), 
             alpha = 0.8, size = 3) +
  theme.BioR +
  scale_colour_npg() +
  labs(x = expression(alpha), y = "Diversity", colour = "Season", shape = "Season") -> R.sea.plot2

R.sea.plot2

ggsave(filename = "Renyi-Season-2",
       path = file.path(dir.figs),
       plot = R.sea.plot2,
       device = "png")
dev.off()

## Factor group: Month

bs.env.ak %>%
  group_by(Month) %>%
  summarise(n = n())
# Sparse samples from Oct (16), Nov (2), Dec (0), Jan (6), and Feb (7)
months.drop = c("October", "November", "December", "January", "February")

# New bs.env.ak df without the months with sparse sampling
bs.env.ak %>%
  mutate(tmp = row.names(.)) %>%
  filter(!Month %in% months.drop) %>%
  mutate(Month = as.double(Month)) %>%
  mutate(Month = as.factor(Month)) -> bs.env.ak.2

# Making new bs.counts.ak df match bs.env.ak.2

bs.counts.ak %>%
  mutate(tmp = row.names(.)) %>%
  semi_join(bs.env.ak.2, by = "tmp") -> bs.counts.ak.2

# Clean up tmp columns
bs.env.ak.2 = select(bs.env.ak.2, -tmp)
bs.counts.ak.2 = select(bs.counts.ak.2, -tmp)

# Now calculate Renyi Diversity using new df's:
R.mon = renyicomp(bs.counts.ak.2, y = bs.env.ak.2, factor = 'Month',
                  scales = c(0, 0.25, 0.5, 1, 2, 4, 8, Inf),
                  plotit = FALSE)

R.mon

R.mon.long = renyicomp.long(R.mon, label.freq = 1)
head(R.mon.long)

# Month: with upper/lower bounds
ggplot(data = R.mon.long, aes(x = Scales, y = Diversity, ymax = UPR, ymin = LWR)) + 
  scale_x_discrete() +
  scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = NULL)) +
  geom_line(aes(x = Obs, colour = Grouping), 
            size = 1) +
  geom_point(data = subset(R.mon.long, labelit == TRUE), 
             aes(colour = Grouping, shape = Grouping), 
             size = 3) +
  geom_ribbon(aes(x = Obs, colour = Grouping, fill = after_scale(alpha(colour, 0.1))), 
              show.legend = FALSE) + 
  theme.BioR +
  scale_colour_npg() +
  labs(x = expression(alpha), y = "Diversity", colour = "Month", shape = "Month") -> R.mon.plot1

R.mon.plot1

ggsave(filename = "Renyi-Month-1",
       path = file.path(dir.figs),
       plot = R.mon.plot1,
       device = "png")
dev.off()

# Month: without upper/lower bounds

ggplot(data = R.mon.long, aes(x = Scales, y = Diversity, ymax = UPR, ymin = LWR)) + 
  scale_x_discrete() +
  scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = NULL)) +
  geom_line(aes(x = Obs, colour = Grouping), 
            size = 1) +
  geom_point(data = subset(R.mon.long, labelit == TRUE), 
             aes(colour = Grouping, shape = Grouping), 
             alpha = 0.8, size = 3) +
  theme.BioR +
  scale_colour_npg() +
  labs(x = expression(alpha), y = "Diversity", colour = "Month", shape = "Month") -> R.mon.plot2

R.mon.plot2

ggsave(filename = "Renyi-Month-2",
       path = file.path(dir.figs),
       plot = R.mon.plot2,
       device = "png")
dev.off()


## Arctic region, factor = Month

R.arc.mon = renyicomp(bs.counts.arc, y = bs.env.arc, factor = 'Month',
                  scales = c(0, 0.25, 0.5, 1, 2, 4, 8, Inf),
                  plotit = FALSE)

R.arc.mon

R.arc.mon.long = renyicomp.long(R.arc.mon, label.freq = 1)
head(R.arc.mon.long)

ggplot(data = R.arc.mon.long, aes(x = Scales, y = Diversity, ymax = UPR, ymin = LWR)) + 
  scale_x_discrete() +
  scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = NULL)) +
  geom_line(aes(x = Obs, colour = Grouping), 
            size = 1) +
  geom_point(data = subset(R.arc.mon.long, labelit == TRUE), 
             aes(colour = Grouping, shape = Grouping), 
             alpha = 0.8, size = 3) +
  theme.BioR +
  scale_colour_npg() +
  labs(x = expression(alpha), y = "Diversity", colour = "Month", shape = "Month") -> R.arc.mon.plot2

R.arc.mon.plot2


## Southcentral region, factor = Month

R.scak.mon = renyicomp(bs.counts.scak, y = bs.env.scak, factor = 'Month',
                      scales = c(0, 0.25, 0.5, 1, 2, 4, 8, Inf),
                      plotit = FALSE)

R.scak.mon

R.scak.mon.long = renyicomp.long(R.scak.mon, label.freq = 1)
head(R.scak.mon.long)

ggplot(data = R.scak.mon.long, aes(x = Scales, y = Diversity, ymax = UPR, ymin = LWR)) + 
  scale_x_discrete() +
  scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = NULL)) +
  geom_line(aes(x = Obs, colour = Grouping), 
            size = 1) +
  geom_point(data = subset(R.scak.mon.long, labelit == TRUE), 
             aes(colour = Grouping, shape = Grouping), 
             alpha = 0.8, size = 3) +
  theme.BioR +
  scale_colour_npg() +
  labs(x = expression(alpha), y = "Diversity", colour = "Month", shape = "Month") -> R.scak.mon.plot2

R.scak.mon.plot2


## Southeastern region, factor = Month

R.seak.mon = renyicomp(bs.counts.seak, y = bs.env.seak, factor = 'Month',
                       scales = c(0, 0.25, 0.5, 1, 2, 4, 8, Inf),
                       plotit = FALSE)

R.seak.mon

R.seak.mon.long = renyicomp.long(R.seak.mon, label.freq = 1)
head(R.seak.mon.long)

ggplot(data = R.seak.mon.long, aes(x = Scales, y = Diversity, ymax = UPR, ymin = LWR)) + 
  scale_x_discrete() +
  scale_y_continuous(sec.axis = dup_axis(labels = NULL, name = NULL)) +
  geom_line(aes(x = Obs, colour = Grouping), 
            size = 1) +
  geom_point(data = subset(R.seak.mon.long, labelit == TRUE), 
             aes(colour = Grouping, shape = Grouping), 
             alpha = 0.8, size = 3) +
  theme.BioR +
  scale_colour_npg() +
  labs(x = expression(alpha), y = "Diversity", colour = "Month", shape = "Month") -> R.seak.mon.plot2

R.seak.mon.plot2
