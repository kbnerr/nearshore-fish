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
# and decide how to transform or standardize the data,
# the idea being that we want to remove effects of the largest catches
# but still allow for abundances to have some effect.

## Current data process:
# 1a. "Weight" abundances by the 0.95 quantile for each genera
# 1b. Use raw abundances
# 2. Hellinger transformation (sqrt of abundance/rowsum(abundance)) -- NOT IMPLEMENTED YET!
# 3a. Calculate distance matris using Bray-Curtis (weighted abundances)
# 3b. Calculate distance matrix using Robust Aitchison (raw abundances -> robust centered log ratio)
## Current method:
# 4a. Run agglomertative algorithm, hclust(method = ward.D2) -- distances squared before cluster updates
# 4b. Run divisive algorithm, diana(k.stop.at = ?)
## Current analysis:
# 5. pvclust() -- 

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
plot_abun = function(x, var, xintercept = NULL) {
  names = str_which(names(x), "Name") %>% x[.] %>%
    unlist(use.names = FALSE) %>%
    unique() %>% sort()
  tmp_list = rep_len(list(), length.out = length(names))
  names(tmp_list) = names
  var = rlang::sym(var)
  if (!is.null(xintercept)) {
    xint = rlang::sym(xintercept)
    for (i in names) {
      tmp_list[[i]] = ggplot(data = filter(x, Gen_ScientificName == i),
                             aes(x = !! var)) +
        geom_boxplot() +
        geom_vline(aes(xintercept = !! xint), color = "red") +
        labs(title = i)
    }
  } else {
      for (i in names) {
        tmp_list[[i]] = ggplot(data = filter(x, Gen_ScientificName == i), aes(x = !! var)) +
          geom_boxplot() +
          labs(title = i)
      }
    }
  tmp_list
}
# Run the function
plot_abun(x = tmp, var = "Abundance", xintercept = "q.95_abun")

# I like where the q.95_abun sits,
# so we'll transform abundances by the 95th quantile for each genus,
# I'll call this 'weighting' but this may not be accurate...
gen_abun_wt = gen_abun %>%
  group_by(Gen_ScientificName) %>%
  mutate(q.95_abun = quantile(Abundance, 0.95, names = FALSE)) %>% 
  ungroup() %>%
  mutate(wt_abun = Abundance/q.95_abun) %>%
  select(-Abundance, -q.95_abun)

# Plot abundance after transforming
p_gen_abun_wt = gen_abun_wt %>%
  group_by(Gen_ScientificName) %>%
  summarise(wt_abun_sum = sum(wt_abun)) %>%
  mutate(Gen_ScientificName = fct_reorder(as.factor(Gen_ScientificName), dplyr::desc(wt_abun_sum))) %>% 
  ggplot(aes(x = Gen_ScientificName, y = wt_abun_sum)) +
  geom_col() +
  geom_text(aes(label = round(wt_abun_sum, 0)), vjust = -0.5, size = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ggsave("nfa_gen_abun_wt.png", plot = p_gen_abun_wt, device = 'png', path = file.path(dir.figs))

# I want to check whether abundances look normal if we use the rclr pre-processing,
# for both raw and weighted abundances,
gen_abun_wide[-1] %>%
  decostand(method = "rclr") %>%
  pivot_longer(cols = everything(),
               names_to = "Gen_ScientificName",
               values_to = "log_abun") %>%
  filter(log_abun != 0) %>%
  plot_abun(var = "log_abun")

gen_abun_wt %>%
  pivot_longer(cols = 2:length(gen_abun_wide),
               names_to = "Gen_ScientificName",
               values_to = "log_abun") %>%
  filter(log_abun != 0) %>%
  plot_abun(var = "log_wt_abun")

# Pivot our data into wide format (and save),
# For transformed abundances
gen_abun_wt_wide = pivot_wider(gen_abun_wt,
            id_cols = VisitID,
            names_from = Gen_ScientificName, names_sort = TRUE,
            values_from = wt_abun, values_fill = 0)
# And un-transformed abundances
gen_abun_wide = pivot_wider(gen_abun,
                            id_cols = VisitID,
                            names_from = Gen_ScientificName, names_sort = TRUE,
                            values_from = Abundance, values_fill = 0)

## Calculate community distances

# Presence/absence
# Jaccard index (metric)
gen_dist_jac = vegdist(x = gen_abun_wt_wide[-1], method = "jaccard", binary = TRUE)
# Sorenson
gen_dist_sor = vegdist(x = gen_abun_wt_wide[-1], method = "bray", binary = TRUE)
# Chao
gen_dist_chao = vegdist(x = gen_abun_wt_wide[-1], method = "chao", binary = TRUE)

# Abundances
# Bray-Curtis (semi-metric)
gen_dist_bray = vegdist(x = gen_abun_wt_wide[-1], method = "bray", binary = FALSE)
# Cao 
gen_dist_cao = vegdist(x = gen_abun_wide[-1], method = "cao", binary = FALSE)
# Raup-Grick
gen_dist_raup = vegdist(x = gen_abun_wt_wide[-1], method = "raup")
# Robust Aitchison (see Martino et al. 2019)
gen_dist_rait = vegdist(x = gen_abun_wide[-1], method = "robust.aitchison", binary = FALSE)

hc = hclust(d = gen_dist_bray, method = "ward.D2")
dc = diana(x = gen_dist_bray, stop.at.k = 4)
?kmeans()


plot(dc)

