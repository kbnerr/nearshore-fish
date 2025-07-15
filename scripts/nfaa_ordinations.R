# Packages
library(tidyverse)
library(vegan)

# Directory
wd = here::here()
dirs = wd %>% list.files() %>% str_subset(pattern = "^README|^LICENSE|.md$|.Rproj$", negate = TRUE)
for (i in seq_along(dirs)) {
  name = str_replace_all(dirs[i], "^", "dir.")
  path = str_replace_all(dirs[i], "^", str_c(wd, "/"))
  assign(name, path)
  rm(name, path, i)
}

# Data



## MV Ordinations



### Create catch matrix based on average abundances

# Create a data.frame in sample x species format (e.g., vegan)
abun.mat = abun %>%
  pivot_wider(names_from = Sp_ScientificName,
              values_from = Abundance,
              values_fill = 0) %>%
  column_to_rownames(var = "VisitID")



### Standardize catch

# presence/absence
pa = decostand(abun.mat, method = "pa")

# 4th root
rt4 = mutate(abun.mat, across(everything(), ~ .x^(1/4)))

# robust CLR
rclr = decostand(abun.mat, method = "rclr")



### Calculate distances

# Jaccard distances on P/A data
jac = vegdist(as.matrix(pa), method = "jaccard")

# Bray-Curtis distance on 4th rt transformed data
bc = vegdist(as.matrix(rt4), method = "bray")

# Euclidean distance on RCLR transformed data, i.e., robust aitchison distance
rait = vegdist(as.matrix(rclr), method = "euclidean")



### Ordinations

nmds.jac = metaMDS(jac)
plot(nmds.jac, type = "t")

nmds.bc = metaMDS(bc)
plot(nmds.bc, type = "t")

## plot using rda() abd biplot()
# pca.rclr = rda(as.matrix(rclr))
# biplot(pca.rclr, display = c("sites", "species"), type = c("text", "points"))

mds.rclr = metaMDS(rait)
plot(mds.rclr, type = "t")



### Look at the outlier sample

filter(visits, VisitID == "846_2015-08-15") %>%
  left_join(catch.2, by = "VisitID") %>%
  glimpse()
