# -----------------------------------------------------------------------
# Title: GoA bio break - cluster analysis
# Creator: Chris Guo
# Date: 2023.09.28
# Purpose: 

# Notes -------------------------------------------------------------------

## 


# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(cluster)
library(magrittr)
library(vegan)

# Define workflow paths ---------------------------------------------------

wd = getwd()
dir.output = file.path(wd, "output")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd,"data")
dir.scripts = file.path(wd,"scripts")

# Read in data ------------------------------------------------------------

source(file = file.path(dir.scripts, "FishAtlas_1_events-wrangle.R"))
source(file = file.path(dir.scripts, "FishAtlas_2_catch-wrangle.R"))
source(file = file.path(dir.scripts, "FishAtlas_3_visits-wrangle.R"))
source(file = file.path(dir.scripts, "utility.R"))

# Data preparation --------------------------------------------------------

# We subset the catch_qc for genus abundance,
gen_abun = catch_qc %>%
  select(VisitID,
         EventID,
         Gen_ScientificName,
         Count) %>%
  group_by(VisitID, Gen_ScientificName) %>%
  # Visit abundance = all genus caught averaged by no. of events
  summarise(Abundance = (sum(Count) / n_distinct(EventID)) %>% round(2)) %>%
  ungroup()

## We'll take a look at catch distribution for each genera,
# and decide how to transformorm standardize the data,
# the idea being that we want to remove effects of the largest catches
# but still allow for abundances to have some effect.

# First, set aside a temporary df to calculate different quantiles,
tmp = gen_abun %>%
  group_by(Gen_ScientificName) %>%
  mutate(q.75_abun = quantile(Abundance, 0.75, names = FALSE),
         k_abun = quantile(Abundance, 0.75, names = FALSE) + IQR(Abundance) * 1.5,
         q.90_abun = quantile(Abundance, 0.90, names = FALSE),
         q.95_abun = quantile(Abundance, 0.95, names = FALSE),
         max_abun = max(Abundance)) %>% 
  ungroup()

# Write a function to store boxplots for each genera abundance,
# marking optional distribution statistics to use,
plot_abun = function(x) {
  names = str_which(names(x), "Name") %>% x[.] %>%
    unlist(use.names = FALSE) %>%
    unique() %>% sort()
  tmp_list = list()
  tmp_list = map(purrr::set_names(names), ~ {})
  for (i in 1:length(names)) {
    tmp_list[[i]] = ggplot(data = filter(x, Gen_ScientificName == i), aes(x = Abundance)) +
      geom_boxplot() +
      geom_vline(aes(xintercept = q.95_abun), color = "red") +
      geom_vline(aes(xintercept = k_abun), color = "green") +
      labs(title = i)
  }
}
# Run the function
plot_abun(tmp)

# I like where the q.95_abun sits, so we will weight abundances by the 95th quantile for each genus,
gen_abun_wt = gen_abun %>%
  group_by(Gen_ScientificName) %>%
  mutate(q.95_abun = quantile(Abundance, 0.95, names = FALSE)) %>% 
  ungroup() %>%
  mutate(wt_abun = Abundance/q.95_abun) %>%
  select(-Abundance, -q.95_abun)

# Plot abundance after standardizing
p_gen_abun_wt = gen_abun_wt %>%
  group_by(Gen_ScientificName) %>%
  summarise(wt_abun_sum = sum(wt_abun)) %>%
  mutate(Gen_ScientificName = fct_reorder(as.factor(Gen_ScientificName), dplyr::desc(wt_abun_sum))) %>% 
  ggplot(aes(x = Gen_ScientificName, y = wt_abun_sum)) +
  geom_col() +
  geom_text(aes(label = round(wt_abun_sum, 0)), vjust = -0.5, size = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ggsave("nfa_gen_abun_wt.png", plot = p_gen_abun_wt, device = 'png', path = file.path(dir.figs))

# Plot frequency of occurrence (should not be different than before weights)
gen_abun_wt_freq = gen_abun_wt %>%
  group_by(VisitID, Gen_ScientificName) %>%
  summarise(Presence = n_distinct(Gen_ScientificName)) %>%
  ungroup() %>%
  group_by(Gen_ScientificName) %>%
  summarise(Occurrence = sum(Presence)) %>%
  mutate(Perc_Occurrence = Occurrence / n_distinct(gen_abun_wt$VisitID) * 100,
         Gen_ScientificName = fct_reorder(as.factor(Gen_ScientificName), desc(Perc_Occurrence)))

p_gen_abun_wt_freq = gen_abun_wt_freq %>%
  ggplot(aes(x = Gen_ScientificName, y = Perc_Occurrence)) +
  geom_col() +
  geom_text(aes(label = round(Perc_Occurrence, 1)), size = 2, vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ggsave("nfa_gen_abun_wt_freq.png", plot = p_gen_abun_wt_freq, device = 'png', path = file.path(dir.figs))

# Pivot the weighted abundances into wide format (and save),
gen_abun_wt_wide = pivot_wider(gen_abun_wt,
            id_cols = VisitID,
            names_from = Gen_ScientificName, names_sort = TRUE,
            values_from = wt_abun, values_fill = 0)

# For now we move forward without transformation - we can come back to change this,

gen_dist_bray = vegdist(x = gen_abun_wt_wide[-1], method = "bray")

gen_dist_jac = vegdist(x = gen_abun_wt_wide[-1], method = "jaccard")

gen_dist_cao = vegdist(x = gen_abun_wt_wide[-1], method = "clark")

gen_dist_raup = vegdist(x = gen_abun_wt_wide[-1], method = "raup")





