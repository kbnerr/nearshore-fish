library(tidyverse)
library(indicspecies)

# Data prep

# We can use our MV df but need to return some of the colummns to vector format:
mutate(mv.df, # Converting category vectos into factors
       Site = as.vector(Site),
       BayLoc = as.vector(BayLoc),
       CstExp = as.vector(CstExp),
       Month = as.vector(Month)) -> indval.df
rownames(spp.matrix) == indval.df$SiteID # check that our df's are ordered the same.

# multipatt by Site:
indval.site = multipatt(x = spp.matrix, cluster = indval.df$Site,
                      func = "IndVal.g",
                      max.order = 1,
                      control = how(nperm = 4999))
summary(indval.site, indvalcomp = TRUE, alpha = 1)
indval.site$sign

sigass.site = signassoc(X = spp.matrix, cluster = indval.df$Site,
                        mode = 0, control = how(nperm = 4999))
sigass.site

# multipatt by Bay Location
indval.bayloc = multipatt(x = spp.matrix, cluster = indval.df$BayLoc,
                      func = "IndVal.g",
                      max.order = 1,
                      control = how(nperm = 4999))
summary(indval.bayloc, indvalcomp = TRUE, alpha = 1)


# multipatt by Current Type
indval.cstexp = multipatt(x = spp.matrix, cluster = indval.df$CstExp,
                        func = "IndVal.g",
                        max.order = 1,
                        control = how(nperm = 4999))
summary(indval.cstexp, indvalcomp = TRUE, alpha = 1)

# multipatt by Month
indval.month = multipatt(x = spp.matrix, cluster = indval.df$Month,
                        func = "IndVal.g",
                        duleg = TRUE,
                        control = how(nperm = 4999))
summary(indval.month, indvalcomp = TRUE, alpha = 1)
indval.month$sign

sigass.month = signassoc(X = spp.matrix, cluster = indval.df$Month,
                         mode = 0, control = how(nperm = 4999))
sigass.month

# combine indicators for June
indic.June = indicators(X = spp.matrix, cluster = indval.df$Month, group = "June",
                        max.order = 5, At = .2, Bt = .2,
                        func = "IndVal.g", control = how(nperm = 4999), verbose = TRUE)
print(indic.June, sqrtIVt = 0)

# combine indicators for July
indic.July = indicators(X = spp.matrix, cluster = indval.df$Month, group = "July",
                        max.order = 5, At = .2, Bt = .2,
                        func = "IndVal.g", control = how(nperm = 4999), verbose = TRUE)
print(indic.July, sqrtIVt = 0)

# combine indicators for August
indic.Aug = indicators(X = spp.matrix, cluster = indval.df$Month, group = "August",
                       max.order = 5, At = .2, Bt = .2,
                       func = "IndVal.g", control = how(nperm = 4999), verbose = TRUE)
print(indic.Aug, sqrtIVt = 0)

# combine indicators for September
indic.Sep = indicators(X = spp.matrix, cluster = indval.df$Month, group = "September",
                       max.order = 5, At = .2, Bt = .2,
                       func = "IndVal.g", control = how(nperm = 4999), verbose = TRUE)
print(indic.Sep, sqrtIVt = 0)

# combine indicators for October
indic.Oct = indicators(X = spp.matrix, cluster = indval.df$Month, group = "October",
                       max.order = 5, At = 0, Bt = 0,
                       func = "IndVal.g", control = how(nperm = 4999), verbose = TRUE)
print(indic.Oct, sqrtIVt = 0)

