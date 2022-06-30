library(readxl)
library(lubridate)
library(reshape2)
library(tidyverse)

############# Flow data, site data #############
source(paste(getwd(), "/R-scripts/1_data-flow-cleanup.R", sep = ""))

## Import data file containing explanatory variables and grouping categories
cmi = read_csv(file = paste(getwd(), "/Data/Field Sampling/Site-Info.csv", sep = ""))

# Defining our date information:
cmi = cmi %>% unite(datetime, 8:9, sep = " ", remove = T)
cmi$datetime = gsub('1899-12-31 ', '', cmi$datetime)
cmi$datetime = as.POSIXct(strptime(cmi$datetime, format = "%m/%d/%y %H:%M:%S", tz = "America/Anchorage"))

# Defining our sampling reach:
cmi = cmi %>%
  mutate(Reach = as.numeric(gsub('m rope', '', cmi$Reach))) %>%
  mutate(Reach = Reach + (35/2)/(2*pi))

# Creating a site key and filtering the data for the CMI project:
cmi$Site = substr(cmi$Site, 1, 3)
cmi = cmi %>%
  unite(SeineID, 1:4, sep = "", remove = F) %>%
  unite(SiteID, 2:4, sep = "", remove = F) %>%
  mutate(Site = case_when(Site == "Hal" ~ "Halibut Cove",
                          Site == "Bar" ~ "Barabara",
                          Site == "Gla" ~ "Glacier Spit",
                          Site == "Tut" ~ "Tutka Bay",
                          Site == "Sel" ~ "Seldovia Harbor",
                          Site == "Chi" ~ "China Poot"),
         NetDepth_m = as.numeric(sub('Max', '5', NetDepth_m)),
         Date = date(datetime)) %>% 
  filter(Project == "CMI")

############# Environmental data #############

# We'll set aside the site info as a temporary df:
tmp = cmi %>%
  select(SiteID,
         Site,
         Date) %>%
  distinct()

# next create the actual df called 'env':
env = cmi %>%
  select(SiteID,
         Temperature_C,
         Salinity_psu,
         DO_mgl,
         DO_percent,
         Turbidity_fnu) %>%
  group_by(SiteID) %>%
  summarise(Temperature = mean(Temperature_C, na.rm = TRUE),
            sd.Temperature = sd(Temperature_C, na.rm = TRUE),
            Salinity = mean(Salinity_psu, na.rm = TRUE),
            sd.Salinity = sd(Salinity_psu, na.rm = TRUE),
            DO.mgL = mean(DO_mgl, na.rm = TRUE),
            sd.DO.mgL = sd(DO_mgl, na.rm = TRUE),
            DO.perc = mean(DO_percent, na.rm = TRUE),
            sd.DO.perc = sd(DO_percent, na.rm = TRUE),
            Turbidity = mean(Turbidity_fnu, na.rm = TRUE),
            sd.Turbidity = sd(Turbidity_fnu, na.rm = TRUE))

# Add back the site info and the FS and error from the flow db:
env = env %>%
  left_join(tmp, by = "SiteID") %>%
  left_join(flow.cols, by = "Site")
rm(tmp)

# Check the df
env

# One more thing we can do is drop the 'CMI' from SiteID:
env$SiteID = substr(env$SiteID, 4, 7)

# Looks like site visits 'CMI2Hal' and 'CMI3Hal' are missing lots of values.
# Sadly, the NA's are from the same site (Halibut Cove) but we still need to remove them:
env = filter(env, !SiteID %in% c('2Hal', '3Hal'))

# Let's set aside the site info from our env factors:
site = env %>% select(SiteID,
                      Site,
                      Date,
                      CstExp)

site = mutate(site, BayLoc = case_when(Site %in% c("Halibut Cove", "Glacier Spit", "China Poot") ~ "Inner",
                                       Site %in% c("Barabara", "Tutka Bay", "Seldovia Harbor") ~ "Outer"))


# and set aside our error columns for use later...
env.sd = env %>% select(sd.Temperature,
                        sd.Salinity,
                        sd.DO.mgL,
                        sd.DO.perc,
                        sd.Turbidity,
                        sd.FS)

# and isolate the factors in env:
env = env %>% select(SiteID,
                     Temperature,
                     Salinity,
                     DO.mgL,
                     DO.perc,
                     Turbidity,
                     FS,
                     var.FS)

# Check that our site df is ordered the same as env
site$SiteID == env$SiteID

# Let's take a look at a pairwise comparison for these variables.
library(corrgram)
corrgram(env[-1], lower.panel = panel.pts)
# looks like Turbidity has an outlier... let's log transform it.
env = env %>% 
  mutate(log.Turbidity = log(Turbidity + 1)) %>%
  select(-Turbidity)
# Looking at the correlation comparisons again:
corrgram(env[-c(1)], lower.panel = panel.pts)

# We'll add Date as a variable we can use in analyses
env$Date = julian(site$Date)
# Check the correlations one more time:
corrgram(env[-c(1,9)], lower.panel = panel.pts) # Without Date

# Great, let's move on to standardizing:
env.matrix = scale(data.matrix(env[-c(1)]))
env.std = env %>% mutate(Temperature = env.matrix[,1],
                         Salinity = env.matrix[,2],
                         DO.mgL = env.matrix[,3],
                         DO.perc = env.matrix[,4],
                         FS = env.matrix[,5],
                         var.FS = env.matrix[,6],
                         log.Turbidity = env.matrix[,7],
                         Date = env.matrix[,8])
# Check the data!
env
env.std
env.matrix

# Creating the database we'll use for IndVal tests
env.indval = env %>%
  left_join(site, by = "SiteID") %>%
  mutate(Month = factor(month.name[month(Date.y)], levels = month.name),
         Season = case_when(Month %in% c("June", "July", "August") ~ "Summer",
                            Month %in% c("September", "October") ~ "Fall")) %>%
  select(SiteID, BayLoc, CstExp, Month, Season)

# There are a couple instances of high correlation...
library(psych)
corr.test(env.matrix, adjust = "none", method = "spearman")
# Turbidity and Salinity are pretty well correlated - could be a FW input signal,
# Date correlates with DO, which is expected in Kbay marine waters,
# FS also correlates with DO, although we would expect it to be positive. Could be chance correlation...
# Obviously the two DO measurements are highly correlated... let's drop DO.mgL and keep the rest:
env = select(env, -DO.mgL)
env.matrix = env.matrix[, !colnames(env.matrix) %in% "DO.mgL"]
# Now our environmental data sets are ready!

# Here's a corr diagram for reporting:
corrgram(env[-1], lower.panel = panel.pts, method = "spearman") # With Date

############# Fish species CPUE data #############

fish = read_csv(file = paste(getwd(), "/Data/Field Sampling/Fish-Combined-v2.csv", sep = ""))
fish$Site = substr(fish$Site, 1, 3)
fish = fish %>%
  unite(SeineID, 1:4, sep = "", remove = F) %>%
  unite(SiteID, 2:4, sep = "", remove = F) %>%
  mutate(Site = case_when(Site == "Hal" ~ "Halibut Cove",
                          Site == "Bar" ~ "Barabara",
                          Site == "Gla" ~ "Glacier Spit",
                          Site == "Tut" ~ "Tutka Bay",
                          Site == "Sel" ~ "Seldovia Harbor",
                          Site == "Chi" ~ "China Poot")) %>% 
  filter(Project == "CMI")
# One more thing we can do is drop the 'CMI' from SiteID:
fish$SiteID = substr(fish$SiteID, 4, 7)

# We need to delineate adult, juveniles, and larval fish:
# Ecologically this makes sense because adults and juveniles have different roles in the estuary.

# Let's read in our Species info:
SpeciesList = read_csv(file = paste(getwd(), "/Data/Field Sampling/Species-List.csv", sep = ""))
# Left join our fish data with the species info
fish = left_join(fish, SpeciesList, by = "Common")
# Make a new column for avg.lengths based on seine catches (SeineID):
fish %>%
  group_by(SeineID, Common) %>%
  mutate(avg.Length = mean(Length_mm, na.rm = TRUE)) -> fish
# Use the avg length in cases where there are not individual lengths
ifelse(is.na(fish$Length_mm),
       fish$avg.Length,
       fish$Length_mm) -> fish$Length_mm

# try a case_when() function to define LifeStage:
unidentified = c("Unidentified Sculpin", "Unidentified Flatfish", "Unidentified Greenling", "Snailfish")
fish = mutate(fish, LifeStage = case_when(Common == "Larval Fish" ~ "(L)",
                                          Common %in% unidentified ~ "(J)",
                                          Common == "Capelin" & Length_mm == "NaN" ~ "(J)",
                                          Length_mm < atMaturity ~ "(J)",
                                          Length_mm >= atMaturity ~ "(A)"))

fish = unite(fish, Common, 7,20, sep = " ")
unique(fish$Common)

# We also need to calculate a sampling reach for each site visit (each sample):
Reach = cmi %>% select(SeineID, Reach)
reach = left_join(fish, Reach, by = "SeineID") %>%
  select(SeineID, SiteID, Reach) %>%
  distinct()

# This is the full transformed species CPUE data
spp.full = fish %>% 
  select(SeineID, Common, Count) %>%
  group_by(SeineID, Common) %>%
  mutate(Seine.Count = sum(Count, na.rm = T)) %>%
  select(-Count) %>%
  distinct() %>% 
  left_join(., reach, by = "SeineID") %>%
  mutate(CPUE = (Seine.Count/Reach)^0.25) %>%
  select(SiteID, SeineID, Common, CPUE)

# Here is our spp CPUE data in long format:
spp.long = spp.full %>%
  group_by(SiteID, Common) %>%
  mutate(avg.CPUE = mean(CPUE, na.rm = T),
         sd.CPUE = sd(CPUE, na.rm = T),
         sum.CPUE = sum(CPUE, na.rm = T)) %>%
  select(SiteID, Common, avg.CPUE, sd.CPUE, sum.CPUE) %>%
  distinct()

rm(reach, Reach) # Don't need these anymore

# Let's take a look at the raw species CPUE:
spp.long %>%
  ungroup() %>%
  select(Common, avg.CPUE) %>%
  group_by(Common) %>%
  summarise(sum.avg.CPUE = sum(avg.CPUE)) %>%
  ggplot(data = ., aes(x = Common, y = sum.avg.CPUE)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=90, hjust = 1, vjust = 0.25)) +
  labs(x = "Species")

# Don't forget that we have two sites to drop...
spp.long = spp.long %>% filter(!SiteID %in% c("2Hal", "3Hal"))
# And drop them from the full CPUE dataset:
spp.full = spp.full %>% filter(!SiteID %in% c("2Hal", "3Hal"))

# Let's take a look at that CPUE ~ Species graph again to see if dropping the sites changed much:
spp.long %>%
  ungroup() %>%
  select(Common, avg.CPUE) %>%
  group_by(Common) %>%
  summarise(sum.avg.CPUE = sum(avg.CPUE)) %>%
  ggplot(data = ., aes(x = Common, y = sum.avg.CPUE)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=90, hjust = 1, vjust = 0.25)) +
  labs(x = "Species", y = "4th-Root CPUE")
# Looks like we may have dropped a huge catch of Herring.
# Seems okay, except that we may want to drop the unidentified species of which we could not define lifestage...
spp.long = spp.long %>%
  filter(!Common %in% c("Unidentified Sculpin", "Unidentified Flatfish", "Unidentified Greenling"))
# One final check:
spp.long %>%
  ungroup() %>%
  select(Common, avg.CPUE) %>%
  group_by(Common) %>%
  summarise(sum.avg.CPUE = sum(avg.CPUE)) %>%
  ggplot(data = ., aes(x = Common, y = sum.avg.CPUE)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=90, hjust = 1, vjust = 0.25)) +
  labs(x = "Species", y = "Summed Average 4th-Root CPUE")

spp.long %>%
  ungroup() %>%
  select(Common, sum.CPUE) %>%
  group_by(Common) %>%
  summarise(sum.sum.CPUE = sum(sum.CPUE)) %>%
  ggplot(data = ., aes(x = Common, y = sum.sum.CPUE)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=90, hjust = 1, vjust = 0.25)) +
  labs(x = "Species", y = "Total 4th-Root CPUE")

# Great, now let's create our site x species data frame. We'll use the site average CPUE:
spp = spp.long %>%
  select(SiteID, Common, avg.CPUE) %>%
  spread(., Common, avg.CPUE)
spp[is.na(spp)] = 0

# view the data
spp # Woo! lots of zero's

# We should check to see if any of these species do not occur at any sites now that two are removed...
colSums(spp[-1])
# Let's view instances of single catch species...
colSums(spp[-1] > 0)
# Chinook Juveniles, Coho Adults, Longfin Smelt, Penpoint Gunnel, Sharpnose Sculpin, and Slender Eelblenny all have 1 catches of 1
# rare.species = c("Buffalo Sculpin (A)", # rare spp subset not used
#                 "Chinook Salmon (J)",
#                 "Coho Salmon (A)",
#                 "Longfin Smelt (J)",
#                 "Padded Sculpin (J)",
#                 "Penpoint Gunnel (J)",
#                 "Sharpnose Sculpin (J)",
#                 "Slender Eelblenny (J)",
#                 "Snake Prickleback (A)",
#                 "Tidepool Sculpin (A)",
#                 "Tubenose Poacher (A)",
#                 "Tubenose Poacher (J)",
#                 "Unidentified Greenling (J)")

# we can take  look in the raw data:
#fish %>% 
#  select(SeineID, Common, Count) %>%
#  filter(Common %in% rare.species)
#spp.long %>%
#  filter(Common %in% rare.species)

# Actually, it looks like when they were caught there were more than one
# The Avg CPUE is low and most are missing errors
# What if we take these 6 rare species out...
# We'll call it spp.rare
#spp.rare = spp.long %>% filter(!Common %in% rare.species)
#spp.rare = spp.rare %>%
#  select(SiteID, Common, avg.CPUE) %>%
#  spread(., Common, avg.CPUE)
#spp.rare[is.na(spp.rare)] = 0
#spp.rare
#spp.rare.matrix = data.matrix(spp.rare[-1])

# Okay, back to the spp CPUE data...
# Let's double check that our long and spread data have the same species:
colnames(spp[,-1])
sort(unique(spp.long$Common))
colnames(spp[,-1]) == sort(unique(spp.long$Common))
# Looks good

# Let's make a data.matrix version of the spp CPUE data:
spp.matrix = data.matrix(spp[-1])

# Now our spp CPUE data is ready!

############# Objects for plotting #############

flow.colors = c("darkred", "red2", "darksalmon", "skyblue", "royalblue2", "royalblue4")
factor.flow.colors = factor(flow.colors, levels = c("darkred", "red2", "darksalmon", "skyblue", "royalblue2", "royalblue4"))
flow.shapes = c(15, 16, 17, 22, 21, 24)
factor.flow.shapes = factor(flow.shapes, levels = c(15, 16, 17, 22, 21, 24))
flow.linetypes = c(1,2,3,1,2,3)
flow.bw = c(rep("black", 3), rep("gray90", 3))

site.colors = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33")
factor.site.colors = factor(site.colors, levels = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33"))
site.colors.cb = c("#d73027", "#fc8d59", "#fee090", "#e0f3f8", "#91bfdb", "#4575b4")
factor.site.colors.cb = factor(site.colors.cb, levels = c("#d73027", "#fc8d59", "#fee090", "#e0f3f8", "#91bfdb", "#4575b4"))
site.colors.cb2 = c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

F2.labels = c("Barabara, High", "China Poot, High", "Glacier Spit, High",
              "Halibut Cove, Low", "Seldovia Harbor, Low", "Tutka Bay, Low")
F2.linetype.override = c(rep(1,3), rep(4,3))

expsr.shapes = c(24, 22, 22, 22, 24, 24)
factor.expsr.shapes = factor(expsr.shapes)

pca_theme = theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        )

nmds_theme = theme(axis.title = element_text(face = NULL, colour = "black"), 
                    panel.background = element_blank(),
                    panel.border = element_rect(fill = NA, colour = "black"), 
                    axis.ticks = element_blank(),
                    axis.text = element_blank(),
                    legend.key = element_blank(), 
                    legend.title = element_text(face = NULL, colour = "black"), 
                    legend.text = element_text(colour = "black"))

############# Extra #############
#for loop to look at individual species
#common = unique(spp.long$Common)
#for (i in common) {
#  tmp = filter(spp.long, Common == i)
#  tmp = left_join(tmp, site, by = "SiteID")
#  p = ggplot(data = tmp, aes(x = Date, y = CPUE)) +
#    geom_col() +
#    labs(title = paste(i, " CPUE"))
#  print(p)
#}
