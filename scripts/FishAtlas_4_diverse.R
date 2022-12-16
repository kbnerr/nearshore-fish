# -----------------------------------------------------------------------
# Title: Diversity in NOAA's Nearshore Fish Atlas
# Creator: Chris Guo
# Date: 2022.11.10
# Purpose: To explore diversity measures and trends in fish community using data collected
# by beach seines from NOAA's Nearshore Fish Atlas database.

# Notes -------------------------------------------------------------------

## Shorezone/FishAtlas web link: https://alaskafisheries.noaa.gov/mapping/sz/
# We use the 'diverse' package to do some initial EDA, generally following their journal article:
# https://www.researchgate.net/publication/312496164_diverse_an_R_Package_to_Measure_Diversity_in_Complex_Systems


# Load packages -----------------------------------------------------------

library(tidyverse)
library(diverse)
library(lubridate)

# Define workflow paths ---------------------------------------------------

wd = getwd()
dir.output = file.path(wd, "output")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd,"data")
dir.scripts = file.path(wd,"scripts")

# Read in data ------------------------------------------------------------

source(file = file.path(dir.scripts, "utility.R"))
source(file = file.path(dir.scripts, "FishAtlas_1_events-wrangle.R"))
source(file = file.path(dir.scripts, "FishAtlas_2_catch-wrangle.R"))
source(file = file.path(dir.scripts, "FishAtlas_3_visits-wrangle.R"))

# Calculating diversity measures ------------------------------------------

# package 'diverse' needs entities (Site Visits) and categories (Family) to be factors,

str(fam_abun) # check

fam_abun_div.1 = fam_abun %>% # might as well rename while we're at it...
  mutate(VisitID = as.factor(VisitID),
         Fam_CommonName = as.factor(Fam_CommonName)) %>%
  as.data.frame()

str(fam_abun_div.1) # check

# Calculate all diversity indices,
fam_abun_div.2 = diversity(data = fam_abun_div.1)

# The entropy diversity indices (HCDT & renyi) and Hill numbers should be parsed into cols for q = 0, 1, and 2
# Let's first remove these indices from our df,
remove_names = diversity(data = slice(fam_abun_div.2, 1), type = c("td", "re", "hcdt")) %>% colnames()
fam_abun_div.3 = select(fam_abun_div.2, -all_of(remove_names))

# Calculate diversity for HCDT, renyi, and Hill numbers when q = 0
fam_abun_q0 = diversity(data = fam_abun_div.1, type = c("td", "re", "hcdt"), q = 0)
colnames(fam_abun_q0) = paste(colnames(fam_abun_q0), "q0", sep = '.')

# Calculate diversity for HCDT, renyi, and Hill numbers when q = 1
fam_abun_q1 = diversity(data = fam_abun_div.1, type = c("td", "re", "hcdt"), q = 1)
colnames(fam_abun_q1) = paste(colnames(fam_abun_q1), "q1", sep = '.')

# Calculate diversity for HCDT, renyi, and Hill numbers when q = 2
fam_abun_q2 = diversity(data = fam_abun_div.1, type = c("td", "re", "hcdt"), q = 2)
colnames(fam_abun_q2) = paste(colnames(fam_abun_q2), "q2", sep = '.')

# Add these to the full diversity df,
fam_abun_div.4 = bind_cols(fam_abun_q0, fam_abun_q1, fam_abun_q2) %>%
  select(colnames(.) %>% sort()) %>% # re-order the indices so same is next to same
  bind_cols(fam_abun_div.3, .)

fam_abun_div.5 = fam_abun_div.4 %>%
  mutate(VisitID = row.names(fam_abun_div.4)) %>%
  relocate(VisitID, .before = 1)

# Rename our final version of the df,
fam_abun_diverse = fam_abun_div.5

rm(fam_abun_q0, fam_abun_q1, fam_abun_q2, remove_names,
   fam_abun_div.1, fam_abun_div.2, fam_abun_div.3, fam_abun_div.4, fam_abun_div.5) # clean up


# diverse EDA -------------------------------------------------------------

# Proportion of familers over time:
left_join(fam_abun, select(visits_qc, VisitID, Date), by = "VisitID") %>%
  mutate(Year = year(Date)) %>%
  filter(Year > 1980) %>%
  group_by(Year) %>%
  mutate(Year_Tot_Abun = sum(Abundance),
         Year_n_Visits = n_distinct(VisitID)) %>%
  group_by(Year, Fam_CommonName) %>%
  mutate(Year_Fam_Abun = sum(Abundance)) %>%
  ungroup() %>%
  select(Year, Fam_CommonName, Year_Fam_Abun, Year_Tot_Abun, Year_n_Visits) %>%
  distinct() %>%
  {
    ggplot(data = ., aes(x = Year, y = Year_Fam_Abun/Year_Tot_Abun, fill = Fam_CommonName)) +
      geom_col(aes(fill = Fam_CommonName)) +
      geom_text(data = filter(., Year_Fam_Abun/Year_Tot_Abun > 0.1),
                aes(label = round(Year_Fam_Abun/Year_Tot_Abun, 2)),
                position = position_stack(), size = 3)
  }
  


boxplot.n <- function(x){
  return(c(y = max(x), label = length(x)))
}

# Let's start with Richness over time in years, and display it with Region groups:
left_join(select(visits_qc, VisitID, Date, Region), select(fam_abun_diverse, VisitID, variety)) %>%
  mutate(Year = year(Date)) %>%
  filter(Year > 1980) %>%
  ggplot(data = ., aes(x = Year, y = variety, group = Year)) + 
  geom_boxplot() +
  stat_summary(fun.data = boxplot.n, geom = "text", vjust = -0.5) +
  geom_jitter(aes(color = Region), size = 0.5, alpha = 0.75)

# Next, let's do Shannon's Index of diversity, same subsetting as above:
left_join(select(visits_qc, VisitID, Date, Region), select(fam_abun_diverse, VisitID, entropy)) %>%
  mutate(Year = year(Date)) %>%
  filter(Year > 1980) %>%
  ggplot(data = ., aes(x = Year, y = entropy, group = Year)) + 
    geom_boxplot() +
    stat_summary(fun.data = boxplot.n, geom = "text", vjust = -0.5) +
    geom_jitter(aes(color = Region), size = 0.5, alpha = 0.75)

# Now let's try Gini-Simposon,
left_join(select(visits_qc, VisitID, Date, Region), select(fam_abun_diverse, VisitID, gini.simpson)) %>%
  mutate(Year = year(Date)) %>%
  filter(Year > 1980) %>%
  ggplot(data = ., aes(x = Year, y = gini.simpson, group = Year)) + 
  geom_boxplot() +
  stat_summary(fun.data = boxplot.n, geom = "text", vjust = -0.5) +
  geom_jitter(aes(color = Region), size = 0.5, alpha = 0.75)

# And lastly, let's try Berger-Parker,
left_join(select(visits_qc, VisitID, Date, Region), select(fam_abun_diverse, VisitID, berger.parker.D)) %>%
  mutate(Year = year(Date)) %>%
  filter(Year > 1980) %>%
  ggplot(data = ., aes(x = Year, y = berger.parker.D, group = Year)) + 
  geom_boxplot() +
  stat_summary(fun.data = boxplot.n, geom = "text", vjust = -0.5) +
  geom_jitter(aes(color = Region), size = 0.5, alpha = 0.75)



# Now let's check out Richness over month:
left_join(select(visits_qc, VisitID, Date, Region), select(fam_abun_diverse, VisitID, variety)) %>%
  mutate(Month = month(Date)) %>%
  ggplot(data = ., aes(x = Month, y = variety, group = Month)) + 
  geom_boxplot() +
  stat_summary(fun.data = boxplot.n, geom = "text", vjust = -0.5) +
  geom_jitter(aes(color = Region), size = 0.5, alpha = 0.75) +
  scale_x_discrete(limits = month.abb)

# Next, let's do Shannon's Index of diversity, same subsetting as above:
left_join(select(visits_qc, VisitID, Date, Region), select(fam_abun_diverse, VisitID, entropy)) %>%
  mutate(Month = month(Date)) %>%
  ggplot(data = ., aes(x = Month, y = entropy, group = Month)) + 
  geom_boxplot() +
  stat_summary(fun.data = boxplot.n, geom = "text", vjust = -0.5) +
  geom_jitter(aes(color = Region), size = 0.5, alpha = 0.75) +
  scale_x_discrete(limits = month.abb)

# Now let's try Gini-Simposon,
left_join(select(visits_qc, VisitID, Date, Region), select(fam_abun_diverse, VisitID, gini.simpson)) %>%
  mutate(Month = month(Date)) %>%
  ggplot(data = ., aes(x = Month, y = gini.simpson, group = Month)) + 
  geom_boxplot() +
  stat_summary(fun.data = boxplot.n, geom = "text", vjust = -0.5) +
  geom_jitter(aes(color = Region), size = 0.5, alpha = 0.75) +
  scale_x_discrete(limits = month.abb)

# And lastly, let's try Berger-Parker,
left_join(select(visits_qc, VisitID, Date, Region), select(fam_abun_diverse, VisitID, berger.parker.D)) %>%
  mutate(Month = month(Date)) %>%
  ggplot(data = ., aes(x = Month, y = berger.parker.D, group = Month)) + 
  geom_boxplot() +
  stat_summary(fun.data = boxplot.n, geom = "text", vjust = -0.5) +
  geom_jitter(aes(color = Region), size = 0.5, alpha = 0.75) +
  scale_x_discrete(limits = month.abb)


