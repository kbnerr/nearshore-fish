# EDA on environmental variables using PCA.
# Protocols follow examples using the 'FactoMineR' and 'factoextra' packages.
# Example can be accessed http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

#### Initial code ####

# Packages needed
library(tidyverse)
library(FactoMineR)
library(factoextra)

# Creating the dataframe
pca = left_join(select(site, -Date), env, by = "SiteID") # these are created in file: "~\Data Prep.R"
pca = select(pca, # active data frame
             SiteID, # used to define rownames
             Site, # supplementary qualitative
             BayLoc, # supplementary qualitative
             CstExp, # supplementary qualitative
             Date, # supplementary quantitative
             Temperature, # active quantitative
             Salinity, # active quantitative
             DO.perc, # active quantitative
             log.Turbidity)  # active quantitative
pca$SiteID = substr(pca$SiteID, 4, 7)
pca = as.data.frame(pca)
row.names(pca) <- pca$SiteID
(pca = select(pca, -SiteID)) # view all variables in our df

# We subset the active variables in the df
(pca.active = pca[1:36, 5:8])

# PCA() in FactoMineR can automatically scale, define dimensions with 'ncp', and graph
(pca.res = PCA(pca.active, scale.unit = TRUE, ncp = 4, graph = TRUE))

# Data visualization and interpretation #

# Eigenvalues / variances:
(pca.eig.val = get_eigenvalue(pca.res))
# scree plot
fviz_eig(pca.res, addlabels = TRUE, ylim = c(0, 50))


#### Graphing variables ####

# Results:
(pca.var = get_pca_var(pca.res))
head(pca.var$coord) # coordinates
head(pca.var$cos2) # represents the quality of representation for variables on the factor map
# note: var.cos2 = var.coord^2 (squared coordinates)
head(var$contrib) # percent contributions of the variables to the PCs


# Correlation circle and other viz:
fviz_pca_var(pca.res, col.var = "black")
library(corrplot)
corrplot(pca.var$cos2, is.corr=FALSE)
# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(pca.res, choice = "var", axes = 1:2)
# Color by cos2 values: quality on the factor map
fviz_pca_var(pca.res, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)
# Change the transparency by cos2 values
fviz_pca_var(pca.res, alpha.var = "cos2", repel = TRUE)


# Contribution of variables to the PCs:
corrplot(pca.var$contrib, is.corr=FALSE) 
fviz_contrib(pca.res, choice = "var", axes = 1) # Contributions of variables to PC1
fviz_contrib(pca.res, choice = "var", axes = 2, top = 10) # Contributions of variables to PC2
fviz_contrib(pca.res, choice = "var", axes = 1:2) # total contrib to both PC1 and PC2
# red dashed line signifies the expected avg contirbution i.e., 1/length(vars) or 1/4
fviz_pca_var(pca.res, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))


# Dimension description: most significantly associated vars for given PCs
pca.dimdesc = dimdesc(pca.res, axes = c(1,2), proba = 0.05)
pca.dimdesc$Dim.1 # Dimension 1
pca.dimdesc$Dim.2 # Dimension 2


#### Graph of individuals ####

# Results:
(pca.ind = get_pca_ind(pca.res))
pca.ind$coord # Coordinates of individuals
pca.ind$cos2 # Quality of individuals
pca.ind$contrib # Contributions of individuals

# Plots for quality and contribution
fviz_pca_ind(pca.res) # a simple plot
# color points by quality of representation (cos2)
fviz_pca_ind(pca.res, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
# same graph but with point size
fviz_pca_ind(pca.res, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE)
# both color and size
fviz_pca_ind(pca.res, col.ind = "cos2", pointsize = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
# bar plot for cos2 values
fviz_cos2(pca.res, choice = "ind")
# Total contribution on PC1 and PC2
fviz_contrib(pca.res, choice = "ind", axes = 1:2)

# Color by groups
fviz_pca_ind(pca.res,
             geom.ind = "point", # show points only (but not "text")
             col.ind = pca$Site, # color by site
             palette = site.colors,
             addEllipses = TRUE, # Concentration ellipses (change type with ellipse.type = )
             mean.point = FALSE,
             legend.title = "Groups")

# See reference webpage for further customization.
# Graphical parameters
pca.p = fviz_pca_ind(pca.res, geom = "point", col.ind = pca$Site)
ggpubr::ggpar(pca.p,
              title = "Principal Component Analysis",
              subtitle = "Local estuarine conditions",
              caption = "Samples from Kachemak Bay 2018",
              xlab = "PC1", ylab = "PC2",
              legend.title = "Site", legend.position = "top",
              ggtheme = theme_bw(), palette = site.colors)

# Simple biplot
fviz_pca_biplot(pca.res, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
                )
# Note: the coordinate of individuals and variables are not constructed on the same space.
# an individual that is on the same side of a given variable has a high value for this variable;
# an individual that is on the opposite side of a given variable has a low value for this variable.

#Customized biplot
fviz_pca_biplot(pca.res, 
                col.ind = pca$BayLoc, palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Bay Location")
fviz_pca_biplot(pca.res, 
                col.ind = pca$CstExp, palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Current Type")

#### Supplementary elements ####

# NOTE: this is now replacing our pca results (pca.res) from above!
pca.res = PCA(pca, ind.sup = NULL, 
               quanti.sup = 4, quali.sup = 1:3, graph = TRUE)
# Predicted results for our quantitative supplementary variable: Date
pca.res$quanti.sup
fviz_pca_var(pca.res) # Visualize all variables, where suppl quantitative vars are blue-dashed
fviz_pca_var(pca.res,
             col.var = "black",     # Active variables
             col.quanti.sup = "red" # Suppl. quantitative variables
             )

# Qualitative variables:
pca.res$quali.sup # results concerning supplementary variables
# Habillage is french for 'dressing'. Set habillage to a column number for a suppl. qualitative variable
# Color by site pca[,1]
fviz_pca_ind(pca.res, habillage = 1,
             addEllipses = TRUE, ellipse.type = "confidence",
             palette = site.colors.cb2, repel = TRUE,
             mean.point = TRUE)
# Color by bay location pca[,2]
fviz_pca_ind(pca.res, habillage = 2,
             addEllipses = TRUE, ellipse.type = "confidence",
             palette = "jco", repel = TRUE,
             legend.title = "Bay Location")

# Color by coastal exposure pca[,3]
fviz_pca_ind(pca.res, habillage = 3,
             addEllipses = TRUE, ellipse.type = "confidence",
             palette = "jco", repel = TRUE,
             legend.title = "Current Type")

#### Plots for export ####

fviz_pca_biplot(pca.res, repel = TRUE,
                col.var = "black", # Variables color
                col.ind = "grey50",  # Individuals color
                col.quanti.sup = "black",
                title = NULL,
                axes.linetype = "dotted"
                ) -> pca.biplot
pca.biplot
ggpubr::ggpar(pca.biplot,
              title = NULL,
              subtitle = NULL,
              caption = NULL,
              xlab = "PC1", ylab = "PC2",
              legend.title = "",
              ggtheme = pca_theme, palette = NULL) -> F3.a
F3.a

# Sites in color
fviz_pca_biplot(pca.res, 
                # Fill individuals by groups
                geom.ind = "point",
                habillage = 1,
                pointsize = 3,
                col.ind = pca$Site,
                col.var = "black",
                col.quanti.sup = "black",
                addEllipses = TRUE, ellipse.type = "confidence",
                mean.point = FALSE,
                title = NULL,
                legend.title = list(fill = "Site", color = "Site"),
                axes.linetype = "dotted",
                repel = TRUE) +
  ggpubr::color_palette(site.colors.cb2) +
  ggpubr::fill_palette(site.colors.cb2)

# Sites in b/w
fviz_pca_biplot(pca.res, 
                # Fill individuals by groups
                geom.ind = "point",
                habillage = 1,
                pointsize = 3,
                col.var = "black",
                col.quanti.sup = "black",
                addEllipses = FALSE, ellipse.type = "confidence",
                mean.point = FALSE,
                title = NULL,
                legend.title = list(fill = "Site", color = "Site"),
                axes.linetype = "dotted",
                repel = TRUE) + 
  ggplot2::scale_color_manual(values = c(rep("#000000", 6))) +
  ggplot2::scale_fill_manual(values = c(rep("grey60", 6)))-> pca.site
pca.site
ggpubr::ggpar(pca.site,
              title = NULL,
              subtitle = NULL,
              caption = NULL,
              xlab = "PC1", ylab = "PC2",
              legend.title = "Site",
              ggtheme = pca_theme +
                theme(legend.direction = "vertical",
                      legend.justification = c(0,0),
                      legend.position = c(.025,.025)),
              palette = NULL) -> F3.b
F3.b

fviz_pca_ind(pca.res, habillage = 2,
             geom = "point", pointsize = 2.5,
             addEllipses = TRUE, ellipse.type = "confidence", mean.point = TRUE,
             palette = c("#000000", "#000000"), repel = TRUE, # old colors "#0072B2", "#D55E00"
             legend.title = "Bay Location",
             title = "",
             axes.linetype = "dotted"
             ) +
  ggplot2::scale_shape_manual(values = c(21,17)) -> pca.bayloc
pca.bayloc
ggpubr::ggpar(pca.bayloc,
              title =  NULL,
              subtitle = NULL,
              caption = NULL,
              xlab = "PC1", ylab = "PC2",
              legend.title = "Bay Location",
              ggtheme = pca_theme +
                theme(legend.direction = "vertical",
                      legend.justification = c(0,0),
                      legend.position = c(.025,.025)),
              palette = NULL) -> F3.c
F3.c

fviz_pca_ind(pca.res, habillage = 3,
             geom = "point", pointsize = 2.5,
             addEllipses = TRUE, ellipse.type = "confidence", mean.point = TRUE,
             palette = c("#000000", "#000000"), repel = TRUE, # old colors "#009E73", "#CC79A7"
             legend.title = "Current Type",
             title = "",
             axes.linetype = "dotted"
             )  +
  ggplot2::scale_shape_manual(values = c(21,17)) -> pca.cstexp
pca.cstexp
ggpubr::ggpar(pca.cstexp,
              title =  NULL,
              subtitle = NULL,
              caption = NULL,
              xlab = "PC1", ylab = "PC2",
              legend.title = "Current Type",
              ggtheme = pca_theme +
                theme(legend.direction = "vertical",
                      legend.justification = c(0,0),
                      legend.position = c(.025,.025)),
              palette = NULL) -> F3.d
F3.d

cowplot::plot_grid(F3.b, F3.c, F3.d, ncol = 1,
                   labels = c('A', 'B', 'C'), label_size = 12,
                   align = "hv", axis = "trbl") -> F3.pub
ggsave(F3.pub,
       filename = "/Users/chguo/nearshore-fish-communities/2018_cmi/Figures/F3. tmp.tiff",
       width = 90, height = 270, units = "mm", dpi = 500,
       scale = 1.5)
