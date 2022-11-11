library(vegan)
library(tidyverse)
library(goeveg)
library(ggpubr)
library(lubridate)
library(ggrepel)
library(metR)
library(cowplot)

# Code was borrowed heavily from 'Jackie' and found here: 'https://jkzorz.github.io/2020/04/04/NMDS-extras.html'

#### Data prep ####
# Let's add rownames for our spp matrix:
env$SiteID == spp$SiteID # double checking that our env and spp order of samples matches up
rownames(spp.matrix) = spp$SiteID

#### Perform NMDS checks and create df's for ordination ####
# Checking our our dimensions:
dimcheckMDS(spp.matrix)
# Not great, our stress in 2 dimensions is just above 0.2
# We will proceed with caution as our ordination in 2 dimensions is suspect

# NMDS in 2 dimensions:
set.seed(456)
(nmds2 = metaMDS(spp.matrix, distance = "bray", trymax = 500))
stressplot(nmds2)
# our non-metric fit is ok (R2 = 0.947), linear fit is no beuno (R2 = 0.72)
scores(nmds2)
plot(nmds2, type = "t")
nmds2.res = as.data.frame(scores(nmds2))

# Add env variables to df of nmds scores:
(nmds2.res = cbind(nmds2.res, envfit.env)) # grabbing the env variables from the envfit analysis

# Create another df with mds species scores:
(nmds2.spp = as.data.frame(scores(nmds2, "species"))) # extracting the species scores from the mds object
(nmds2.spp$Species = rownames(nmds2.spp)) # making df column with the species names
# drop (J) in names for easier visualization
nmds2.spp$Species = sub(" (J)", "", nmds2.spp$Species, fixed =TRUE)

# Create separate df's with ENVFIT results:
(envfit.con = as.data.frame(scores(envfit, "vectors")) * ordiArrowMul(envfit, fill = .5)) # continuous variables
(envfit.cat = as.data.frame(scores(envfit, "factors")) * ordiArrowMul(envfit, fill = .5)) # categorical variables
# ordiArrowMul() is a multiplier that is analysis specific.
# Here, we multiplied the scores by this multiplier to keep the coordinates in the correct proportion to plot.

#### Create NMDS plots ####

# Recreate the base plot of nmds results, species scores, and envfit results
ggplot(nmds2.res, aes(x = NMDS1, y = NMDS2)) +
  geom_point(size = 1, alpha = 1) +
  geom_text_repel(label = rownames(nmds2.res)) +
  geom_text(data = nmds2.spp,
            aes(x = NMDS1, y = NMDS2, label = Species),
            alpha = .5, check_overlap = NULL) +
  geom_segment(data = envfit.con,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(angle = 20, length = unit(2, "mm"), ends = "last", type = "open"),
               size = .5, alpha = 0.5, colour = "red") +
  geom_text_repel(data = envfit.con, aes(x = NMDS1, y = NMDS2),
                  label = row.names(envfit.con), colour = "red", fontface = "bold") + 
  geom_point(data = envfit.cat, aes(x = NMDS1, y = NMDS2), 
             shape = "diamond", size = 3, alpha = 0.5, colour = "navy") +
  geom_text_repel(data = envfit.cat, aes(x = NMDS1, y = NMDS2), 
                  label = row.names(envfit.cat),
                  colour = "navy", fontface = "bold") + 
  nmds_theme + 
  coord_fixed()

# There are lots of species in the background, some of which are cut off at the plot edges.
# Maybe we should only show some of the more abundant species.

spp.long %>%
  ungroup() %>%
  select(Common, avg.CPUE) %>%
  group_by(Common) %>%
  summarise(sum.avg.CPUE = sum(avg.CPUE)) %>%
  slice_max(order_by = sum.avg.CPUE, prop = .50) -> tmp
tmp$Common -> reduced.spp # kept 50% of species by abundance
reduced.spp = sub(" (J)", "", reduced.spp, fixed = TRUE) # remember to drop (J)'s
reduced.spp = sub(" (L)", "", reduced.spp, fixed = TRUE) # also drop (L) for good measure


# Let's simplify the plot:
# Reduced the spp labeled to proportion in reduced.spp
# remove the categorical factors and site labels
# retain the cont. variables that have a significant correlation in envfit.
# add shapes to the site

ggplot(nmds2.res, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(shape = Site), size = 3, alpha = 1) +
  geom_text_repel(data = filter(nmds2.spp, Species %in% reduced.spp),
                  aes(x = NMDS1, y = NMDS2, label = Species),
                  alpha = .5) +
  geom_segment(data = envfit.con[c("Date",  "DO.perc"),],
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(angle = 20, length = unit(2, "mm"), ends = "last", type = "open"),
               size = .5, alpha = 0.5, colour = "#D55E00") +
  geom_text_repel(data = envfit.con[c("Date", "DO.perc"),], aes(x = NMDS1, y = NMDS2),
                  label = row.names(envfit.con[c("Date", "DO.perc"),]),
                  colour = "#D55E00", fontface = "bold") +
  nmds_theme +
  coord_fixed(xlim = c(-1., 1.)) 

# This looks better, but I think we can make it more informative by dropping DO- the weakest correlative var
# Instead lets show smoothed contours for sampling date:

# NMDS with Date contour using ordisurf() in package vegan, and with package metR

# Dates in b/w, no site shapes
ggplot(nmds2.res, aes(x = NMDS1, y = NMDS2)) +
  geom_point(size = 3, alpha = 1) +
  geom_text_repel(data = filter(nmds2.spp, Species %in% reduced.spp),
                  aes(x = NMDS1, y = NMDS2, label = Species),
                  alpha = .67) +
  geom_contour(data = contours, aes(x, y, z = z, color = as.Date(..level.., origin = origin))) +
  scale_color_gradient(name = "Date", low = "#bdbdbd", high = "#000000",
                       breaks = c(17713, 17744, 17775, 17805), labels = c("Jul", "Aug", "Sep", "Oct")) +
  nmds_theme +
  theme(legend.direction = "horizontal",
        legend.box.just = "right",
        legend.justification = c(1,0),
        legend.position = c(.975, .025)) +
  coord_fixed(xlim = c(-1., 1.))-> F4
F4

# Dates in b/w, site shapes in b/w
ggplot(nmds2.res, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(shape = Site), size = 3, alpha = 1) +
  geom_text_repel(data = filter(nmds2.spp, Species %in% reduced.spp),
                  aes(x = NMDS1, y = NMDS2, label = Species),
                  alpha = .67) +
  geom_contour(data = contours, aes(x, y, z = z, color = as.Date(..level.., origin = origin))) +
  scale_color_gradient(name = "Date", low = "#bdbdbd", high = "#000000",
                       breaks = c(17713, 17744, 17775, 17805), labels = c("Jul", "Aug", "Sep", "Oct")) +
  nmds_theme +
  theme(legend.direction = "horizontal",
        legend.box.just = "right",
        legend.justification = c(1,0),
        legend.position = c(.975, .025)) +
  coord_fixed(xlim = c(-1., 1.))

# Dates in color, no label
ggplot(nmds2.res, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(shape = Site), size = 3, alpha = 1) +
  geom_text_repel(data = filter(nmds2.spp, Species %in% reduced.spp),
                  aes(x = NMDS1, y = NMDS2, label = Species),
                  alpha = .5) +
  geom_contour(data = contours, aes(x, y, z = z, color = as.Date(..level.., origin = origin))) +
  scale_color_gradient(name = "Date", low = "#fdae6b", high = "#7f2704",
                       breaks = TRUE, labels = FALSE) +
  nmds_theme +
  theme(legend.direction = "horizontal",
        legend.justification = c(0,1),
        legend.position = "bottom") +
  coord_fixed(xlim = c(-1., 1.))

# Plot that exhibits categorical factor: Site
ggplot(nmds2.res, aes(x = NMDS1, y = NMDS2, color = Site)) +
  geom_point(size = 3, alpha = 1) +
  scale_color_manual(values = site.colors.cb2) +
  nmds_theme +
  theme(legend.direction = "vertical",
        legend.justification = c(0,1),
        legend.position = "bottom") + 
  coord_fixed() +
  guides(color = guide_legend(nrow = 3))

# Site shapes in b/w
ggplot(nmds2.res, aes(x = NMDS1, y = NMDS2, shape = Site)) +
  geom_point(size = 3, alpha = 1) +
  nmds_theme +
  theme(legend.direction = "vertical",
        legend.justification = c(.5, 1),
        legend.position = "bottom") + 
  coord_fixed() +
  guides(shape = guide_legend(title = "Site", nrow = 3)) -> nmds.A
nmds.A

# Plot that exhibits categorical factor: Bay Location
ggplot(nmds2.res, aes(x = NMDS1, y = NMDS2, color = BayLoc)) +
  geom_point(aes(shape = BayLoc), size = 3, alpha = 1) +
  scale_color_manual(values = c("#0072B2", "#D55E00")) +
  scale_shape_manual(values = c(16,17)) +
  geom_segment(data = envfit.con[c("Date", "Salinity", "DO.perc"),],
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(angle = 20, length = unit(2, "mm"), ends = "last", type = "open"),
               size = .5, alpha = 0.5, colour = "grey50") +
  nmds_theme +
  theme(legend.direction = "vertical",
        legend.justification = c(0,1),
        legend.position = "bottom") + 
  coord_fixed() +
  guides(color = guide_legend(title = "Bay Location", nrow = 1),
         shape = guide_legend(title = "Bay Location"))

# Bay Location shapes in b/w
ggplot(nmds2.res, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(shape = BayLoc), size = 3, alpha = 1) +
  scale_shape_manual(values = c(21,17)) +
  nmds_theme +
  theme(legend.direction = "vertical",
        legend.justification = c(.5, 1),
        legend.position = "bottom") + 
  coord_fixed() +
  guides(shape = guide_legend(title = "Bay Location", nrow = 1)) -> nmds.B
nmds.B

# Plot that exhibits categorical factor: Current Type
ggplot(nmds2.res, aes(x = NMDS1, y = NMDS2, color = CstExp)) +
  geom_point(aes(shape = CstExp), size = 3, alpha = 1) +
  scale_color_manual(values = c("#009E73", "#CC79A7")) +
  scale_shape_manual(values = c(16,17)) +
  geom_segment(data = envfit.con[c("Date", "Salinity", "DO.perc"),],
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(angle = 20, length = unit(2, "mm"), ends = "last", type = "open"),
               size = .5, alpha = 0.5, colour = "grey50") +
  nmds_theme +
  theme(legend.direction = "vertical",
        legend.justification = c(0,1),
        legend.position = "bottom") + 
  coord_fixed() +
  guides(color = guide_legend(title = "Current Type", nrow = 1),
         shape = guide_legend(title = "Current Type"))

# Current Type shapes in b/w
ggplot(nmds2.res, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(shape = CstExp), size = 3, alpha = 1) +
  scale_shape_manual(values = c(21,17)) +
  nmds_theme +
  theme(legend.direction = "vertical",
        legend.justification = c(.5, 1),
        legend.position = "bottom") + 
  coord_fixed() +
  guides(shape = guide_legend(title = "Current Type", nrow = 1)) -> nmds.C
nmds.C

F5 = plot_grid(nmds.A, nmds.B, nmds.C,
               labels = c("B","C","D"), label_size = 12,
               nrow = 1, align = "h")
F5

F4.pub = plot_grid(F4, F5, ncol = 1,
                   rel_heights = c(2, 1), rel_widths = c(1, 1),
                   labels = c("A", ""), label_size = 12)

ggsave(F4.pub,
       filename = "/Users/chguo/nearshore-fish-communities/2018_cmi/Figures/F4. NMDS plots.tiff",
       width = 190, height = 237.5, units = "mm", dpi = 500,
       scale = 1)

