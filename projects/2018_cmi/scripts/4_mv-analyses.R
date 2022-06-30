library(vegan)
library(tidyverse)

#### Data prep: new env matrix with pc results included & calc bray-curtis dist for spp ####
tmp = pca.res$ind$contrib
colnames(tmp) = c("PC1", "PC2", "PC3", "PC4")
tmp = scale(tmp)
mv.matrix = cbind(tmp[ , 1:2], env.matrix[ , c(1:3, 6, 7, 4:5)])
rm(tmp)

spp.bc = vegdist(spp.matrix, method = "bray")


#### BIOENV test for cont. vars 'best' correlated to species CPUE BC dissimilarities ####
(bioenv.spp = bioenv(comm = spp.matrix, env = mv.matrix[ , c(1:7)],
                     index = "bray", method = "spearman"))
summary(bioenv.spp) # related to Species CPUE matrix
bioenvdist(bioenv.spp, which = "best")


#(bioenv.fam = bioenv(fam.matrix, env = mv.matrix[ , c(1:6)], method = "spearman"))
#summary(bioenv.fam) # related to Family CPUE matrix

(bioenv.spp.pc = bioenv(comm = spp.matrix, env = mv.matrix[ , 1:2], method = "spearman"))
summary(bioenv.spp.pc) # Testing PC1 and PC2 relation to Species CPUE matrix


#### Mantel test on the ENV variables related to species CPUE BC dissimilarities ####

# Let's re-examine the result of the BIOENV test:
summary(bioenv.spp)

# Using the 'best' set of vars, i.e., Date only
env.best = bioenvdist(bioenv.spp, which = "best")
(mantel.best = mantel(xdis = spp.bc, ydis = env.best, 
                      method  = "spearman", permutations = 4999))
hist(mantel.best$perm, xlim=c(-0.4, 1)) # Freq plot
abline(v = mantel.best$statistic, col=4, lwd=2) # vertical abline showing the mantel statistic

# Using next best set of vars, model 2: PC2, Date
env.2 = bioenvdist(bioenv.spp, which = 2)
(mantel.2 = mantel(xdis = spp.bc, ydis = env.2, 
                   method  = "spearman", permutations = 4999))
hist(mantel.2$perm, xlim=c(-0.4, 1)) # Freq plot
abline(v = mantel.2$statistic, col=4, lwd=2) # vertical abline showing the mantel statistic
# Still highly signifcant correlation- we should include these in our PERMANOVA tests

# Using an 'okay' set of vars, model 3: PC2, log.turbidity, Date
env.3 = bioenvdist(bioenv.spp, which = 3)
(mantel.3 = mantel(xdis = spp.bc, ydis = env.3, 
                   method  = "spearman", permutations = 4999))
hist(mantel.3$perm, xlim=c(-0.4, 1)) # Freq plot
abline(v = mantel.3$statistic, col=4, lwd=2) # vertical abline showing the mantel statistic
# Still highly signifcant correlation- we should include these in our PERMANOVA tests

# Using all vars: PC1, PC2, Temperature, Salinity, %DO, log.Turbidity, Date
env.all = bioenvdist(bioenv.spp, which = 7)
(mantel.all = mantel(xdis = spp.bc, ydis = env.all, 
                   method  = "spearman", permutations = 4999))
hist(mantel.all$perm, xlim=c(-0.4, 1)) # Freq plot
abline(v = mantel.all$statistic, col=4, lwd=2) # vertical abline showing the mantel statistic

# Using PC1 and PC2
env.pc = dist(mv.matrix[ , 1:2])
(mantel.pc = mantel(xdis = spp.bc, ydis = env.pc, 
                    method  = "spearman", permutations = 4999))
hist(mantel.pc$perm, xlim=c(-0.4, 1)) # Freq plot
abline(v = mantel.pc$statistic, col=4, lwd=2) # vertical abline showing the mantel statistic


#### ENVFIT/ORDISURF: fitting env vectors onto nmds ordination of spp cpue BC dissimilarities ####

envfit.nmds = metaMDS(spp.matrix, distance = "bray", trymax = 500)
envfit.env = cbind(pca, mv.matrix[,1:2])

(envfit = envfit(ord = envfit.nmds, env = envfit.env, permutations = 4999))
plot(envfit.nmds)
plot(envfit)

# Date, DO.perc, Temp, Sal have the largest vectors; however, only Date and DO are strongly significant.
# A better nmds visualization is made in a different script file

# Here we visualize using ordisurf()
ordisurf(x = envfit.nmds, envfit.env$Date, bubble = T)
ordisurf(x = envfit.nmds, envfit.env$DO.perc, bubble = T)
ordisurf(x = envfit.nmds, envfit.env$Salinity, bubble = T)
# Adding all 3 to one plot...
ordisurf(x = envfit.nmds, envfit.env$Date, col = "red")
ordisurf(x = envfit.nmds, envfit.env$DO.perc, col = "blue", add = T)
ordisurf(x = envfit.nmds, envfit.env$Salinity, col = "green", add = T)

# Extract contour information for envfit$Date
# Code from C. Chizinski available at https://chrischizinski.github.io/rstats/ordisurf/

ordsrf.date = ordisurf(x = envfit.nmds, envfit.env$Date, plot = FALSE, nlevels = 2)

extract.xyz <- function(obj) {
  xy <- expand.grid(x = obj$grid$x, y = obj$grid$y)
  xyz <- cbind(xy, c(obj$grid$z))
  names(xyz) <- c("x", "y", "z")
  return(xyz)
}

contours <- extract.xyz(obj = ordsrf.date)
head(contours)

library(lubridate)
ggplot(data = contours, aes(x, y, z = z)) + 
  stat_contour(aes(color = as.Date(..level.., origin = origin))) + 
  coord_cartesian(xlim = c(-2, 2), ylim = c(-1, 1.5)) + 
  nmds_theme


#### BETADISPER test for homogeneity in multivariate dispersion ####

# Data prep:
spp.bc # check that we have this object

site$SiteID == env.indval$SiteID # that rows match

beta = cbind(env.indval, site[, c("Site", "Date")])
mutate(beta, Half = case_when(Date <= median(Date) ~ "Early",
                              Date > median(Date) ~ "Late")) -> beta

# Spatial groups

# For group: site
beta.site = betadisper(spp.bc, group = as.factor(beta$Site), type = "centroid")
permutest(beta.site, permutations = 4999)
anova(beta.site)
TukeyHSD(beta.site, which = "group", ordered = FALSE, conf.level = 0.95)
# Not significantly different among centroids
# No pairwise differences among centroids
plot(beta.site)
boxplot(beta.site)

# For group: Bay Location
beta.bayloc = betadisper(spp.bc, group = as.factor(beta$BayLoc), type = "centroid")
permutest(beta.bayloc, permutations = 4999)
anova(beta.bayloc)
# Not significantly different among centroids
plot(beta.bayloc)
boxplot(beta.bayloc)

# For group: Coastal Exposure
beta.cstexp = betadisper(spp.bc, group = as.factor(beta$CstExp), type = "centroid")
permutest(beta.cstexp, permutations = 4999)
anova(beta.cstexp)
# Not significantly different among centroids
plot(beta.cstexp)
boxplot(beta.cstexp)

# Temporal groups

# For group: Month
beta.month = betadisper(spp.bc, group = as.factor(beta$Month), type = "centroid")
permutest(beta.month, permutations = 4999)
anova(beta.month)
TukeyHSD(beta.month, which = "group", ordered = FALSE, conf.level = 0.95)
# Not significantly different among centroids
# # No pairwise differences among centroids
plot(beta.month)
boxplot(beta.month)

# For group: Season (Summer vs Fall)
beta.season = betadisper(spp.bc, group = as.factor(beta$Season), type = "centroid")
anova(beta.season)
# Difference in summer vs fall, F = 6.0119, p = 0.0195
plot(beta.season)
boxplot(beta.season)

# For group: Half (early half vs later half)
beta.half = betadisper(spp.bc, group = as.factor(beta$Half), type = "centroid")
anova(beta.half)
# Difference in early vs late, F = 6.8796, p = 0.01296
plot(beta.half)
boxplot(beta.half)

#### ADONIS and ADONIS2 ####

# Let's make a dataframe that includes all of our environmental factors: local, PCs, and categorical
mv.df = cbind(as.data.frame(mv.matrix), # converting mv.matrix to class dataframe
              env.indval) # adding categorical vars
mv.df = cbind(mv.df, site[ , "Site"]) # adding Site vars
mutate(mv.df, # Converting category vectors into factors
       Site = as.factor(Site),
       BayLoc = as.factor(BayLoc),
       CstExp = as.factor(CstExp),
       Month = as.factor(Month)) -> mv.df

# Tests for overall effect
adonis2(spp.bc ~ Temperature, data = mv.df, by = NULL, permutations = 4999)
adonis2(spp.bc ~ Salinity, data = mv.df, by = NULL, permutations = 4999)
adonis2(spp.bc ~ DO.perc, data = mv.df, by = NULL, permutations = 4999) # significant
adonis2(spp.bc ~ log.Turbidity, data = mv.df, by = NULL, permutations = 4999) # significant
adonis2(spp.bc ~ PC1, data = mv.df, by = NULL, permutations = 4999)
adonis2(spp.bc ~ PC2, data = mv.df, by = NULL, permutations = 4999)
adonis2(spp.bc ~ Date, data = mv.df, by = NULL, permutations = 4999)
adonis2(spp.bc ~ Month, data = mv.df, by = NULL, permutations = 4999)
adonis2(spp.bc ~ Site, data = mv.df, by = NULL, permutations = 4999)
adonis2(spp.bc ~ BayLoc, data = mv.df, by = NULL, permutations = 4999)
adonis2(spp.bc ~ CstExp, data = mv.df, by = NULL, permutations = 4999)

adonis2(spp.bc ~ Site %in% BayLoc, data = mv.df, by = NULL, permutations = 4999) # random Site nested in fixed Bay Location
adonis2(spp.bc ~ Site %in% CstExp, data = mv.df, by = NULL, permutations = 4999) # random Site nested in fixed Coastal Exposure

# Sequential tests for multiple sources
adonis2(spp.bc ~ Date * DO.perc, data = mv.df, by = "term", permutations = 4999) # DO not signif if added after Date
adonis2(spp.bc ~ Date * log.Turbidity, data = mv.df, by = "term", permutations = 4999) # Turbidity IS signif if added after Date

adonis2(spp.bc ~ Month * Site, data = mv.df, by = "term", permutations = 4999) # Includes interaction (marginal)
adonis2(spp.bc ~ Month * BayLoc, data = mv.df, by = "term", permutations = 4999) # Includes interaction (marginal)
adonis2(spp.bc ~ Month * CstExp, data = mv.df, by = "term", permutations = 4999) # Includes interaction (marginal)

# Marginal tests for time x space sources
adonis2(spp.bc ~ Month + Site, data = mv.df, by = "margin", permutations = 4999)
adonis2(spp.bc ~ Month + BayLoc, data = mv.df, by = "margin", permutations = 4999)
adonis2(spp.bc ~ Month + CstExp, data = mv.df, by = "margin", permutations = 4999) 


#### Ordiplot3d ####
# Leftover code, object names are different but may still be useful in creating 3D figures.

# We need to save an ENVFIT object using 3 dimensions for 3-D plotting:
#ef1.3d = envfit(cpue.mds3, env[,c("Date", "FS")], choices = 1:3, permutations = 999)


#library(vegan3d)
#ordiplot3d(cpue.mds3)

# data(dune, dune.env)
# ord <- cca(dune ~ A1 + Moisture, dune.env)
# ordiplot3d(ord)
## A boxed 'pin' version
# ordiplot3d(ord, type = "h")
## More user control
# pl <- ordiplot3d(ord, scaling = "symmetric", angle=15, type="n")
# points(pl, "points", pch=16, col="red", cex = 0.7)
## identify(pl, "arrows", col="blue") would put labels in better positions
# text(pl, "arrows", col="blue", pos=3)
# text(pl, "centroids", col="blue", pos=1, cex = 1)

#pcol = cpue.mds3.scores %>% 
#  mutate(pcol = case_when(`Flow Class` == "High" ~ "red",
#                          `Flow Class` == "Low" ~ "blue")) %>%
#  select(pcol)
#pcol = pcol$pcol
#
#ordiplot3d(cpue.mds3, envfit = ef1.3d, scaling = "symmetric",
#           tick.marks = FALSE, angle = 25)
#ordiplot3d(cpue.mds3, display = "sites", choices = 1:3, 
#           col = pcol, type = "p", ax.col = "black",
#           angle = 25)
#
#ordirgl(cpue.mds3, display = "sites", typ = "p",
#        ax.col = "green", label = TRUE)
#with(env, orglspider(cpue.mds3, groups = `Flow Class`, col = c("red", "blue"), label = TRUE))
