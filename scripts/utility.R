# -----------------------------------------------------------------------
# Project: Utility script
# Creator: Chris Guo
# Updated: 2022.02.23
# Purpose: Clearing house for jack knife functions and objects

# Notes -------------------------------------------------------------------

## Functions that include the 'quiet' argument can provide an interpretation of the function purpose.
# Keep 'quiet = T' to silence this interpretation.

## Contents:
# (1) 

# Load packages -----------------------------------------------------------

library(tidyverse)

# Define workflow paths ---------------------------------------------------

wd = getwd()
dir.output = file.path(wd, "output")
dir.figs = file.path(wd, "figs")
dir.data = file.path(wd,"data")
dir.R = file.path(wd,"R")

# Create
# dir.create(dir.figs, recursive=TRUE)


# For plotting ------------------------------------------------------------

## For adding labels to boxplot graphs,
boxplot.n.max <- function(x){
  return(c(y = max(x), label = length(x)))
}
boxplot.n.min <- function(x){
  return(c(y = min(x), label = length(x)))
}
boxplot.n.median <- function(x){
  return(c(y = median(x), label = length(x)))
}

## Themes
pca_theme = theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

nmds_theme = theme(axis.title = element_text(face = NULL, colour = "black"), 
                   panel.background = element_blank(),
                   panel.border = element_rect(fill = NA, colour = "black"), 
                   axis.ticks = element_blank(),
                   axis.text = element_blank(),
                   legend.key = element_blank(), 
                   legend.title = element_text(face = NULL, colour = "black"), 
                   legend.text = element_text(colour = "black"))


# Useful spatial objects --------------------------------------------------

# Bounding Boxes
# All boxes in (Lng, Lat) and EPSG: 4326 - WGS 84
# Check out bboxfinder.com to quickly make new bboxes
bbox.CookInlet = c(-154.715792, 58.501428, -148.574435, 61.566390)

# (1) Trim and winsorize --------------------------------------------------

# Trimmed estimates
trim = function(x, by, fun = mean, quiet = T) {
  # Interpretations:
    i = paste("Applying the 'fun' provided after removing the uppermost ", by*100,
              "% and the lowermost ", by*100, "% of values. ",
              "Try functions for mean(), var(), sd(), or skew().",
              sep = "")
    if(!quiet) {print(i, quote = FALSE)}
  # Calculation:
    n = length(x)
    lo = floor(n * by) + 1
    hi = n + 1 - lo
    sort(x)[lo:hi] %>%
      fun()
}

# Trimmed outliers
trim.outliers = function(x, k = 1.5, quiet = T) {
  # Interpretations:
    i = paste("Kurtosis is highly sensitive to values in the extreme of a distribution. ",
              "If we trim the tails, then we may drastically distort the kurtosis estimate. ",
              "This raises the question of what exactly is an outlier. ",
              "A rule of thumb is to disregard values beyond k = 1.5x the IQR of the 1st and 3rd quartile. ",
              "However, these values may not indicate anomalousness per se, ",
              "but rather they may simply be characterstic of a heavy-tailed distribution. ",
              "Thus, we may change k to be more stringent (e.g., k = 3.0) in our definition of outliers. ",
              "Here, we trim values that are ", k, "x the IQR above or below the 3rd and 1st quartiles, respectively.",
              sep = "")
    if(!quiet) {print(i, quote = FALSE)}
  # Calculation:
    iqr = IQR(x)
    limits = quantile(x, probs = c(1, 3) / 4) + c(-1, 1) * k * iqr
    x[ (x > limits[1]) & (x < limits[2])]
}

# Winsorized estimates
winsorize = function(x, at, fun = mean, quiet = T) {
  # Interpretation
    i = paste("Applying the 'fun' provided after replacing values below and above the ",
              at*100, "th and ", (1-at)*100, "th percentile with the value of the ",
              at*100, "th and ", (1-at)*100, "th percentile, respectively.",
              "Try functions for mean(), var(), sd(), or skew().",
              sep = "")
    if(!quiet) {print(i, quote = FALSE)}
  # Calculation:
    lo = quantile(x, probs = at)
    hi = quantile(x, probs = 1 - at)
    x[x < lo] = lo
    x[x > hi] = hi
    fun(x)
}

winsorize.outliers = function(x, k = 1.5, quiet = T) {
  # Interpretations:
    i = paste("Kurtosis is highly sensitive to values in the extreme of a distribution. ",
              "If we winsorize the tails, then we may drastically distort the kurtosis estimate. ",
              "This raises the question of what exactly is an outlier. ",
              "A rule of thumb is to disregard values beyond k = 1.5x the IQR of the 1st and 3rd quartile. ",
              "However, these values may not indicate anomalousness per se, ",
              "but rather they may simply be characterstic of a heavy-tailed distribution. ",
              "Thus, we may change k to be more stringent (e.g., k = 3.0) in our definition of outliers. ",
              "Here, we winzorise values at ", k, "x the IQR above or below the 3rd and 1st quartiles, respectively.",
              sep = "")
    if(!quiet) {print(i, quote = FALSE)}
  # Calculation:
    iqr = IQR(x)
    limits = quantile(x, probs = c(1, 3) / 4) + c(-1, 1) * k * iqr
    outlier_index = (x < limits[1]) | (x > limits[2])
    x[outlier_index] = median(x)
    x
}

# (2) Measures of central tendency ----------------------------------------

# Interquartile mean:
mean_iqr = function(x, quiet = T) {
  # Interpretation
    i = paste("The interquartile mean is calculated after values below Q1 and above Q3 are discarded.")
    if(!quiet) {print(i, quote = FALSE)}
  # Calculation:
    q1 = quantile(x, probs = 0.25)
    q3 = quantile(x, probs = 0.75)
    x[x > q1 & x < q3] %>%
      mean()
}

# Mid-hinge:
midhinge = function(x, quiet = T) {
  # Interpretation:
    i = paste("The midhinge is the center point between Q1 and Q3. Equivalent to the mean of Q1 and Q3.")
    if(!quiet) {print(i, quote = FALSE)}
  # Calculation:
    quantile(x, probs = c(0.25, 0.75)) %>%
      mean()
}

# Mid-range:
midrange = function(x, quiet = T) {
  # Interpretation:
    i = paste("The midrange is the center of the full range of values. Equivalent to the mean of the min and max.",
              "It is less robust than the midhinge.", sep = "")
    if(!quiet) {print(i, quote = FALSE)}
  # Calculation:
    range(x) %>% mean()
}

# Tri-mean
tri_mean = function(x, quiet = T) {
  # Interpretation:
    i = paste("The trimean is the average of the median and midhinge. In other words, ",
              "it is the weighted average of Q1, Q2, and Q3.", sep = "")
    if(!quiet) {print(i, quote = FALSE)}
  # Calculation:
    c(midhinge(x), median(x)) %>% mean()
}


# (3) Measures of dispersion ----------------------------------------------

# Median absolute deviation (MAD)
mad_sample = function(x, quiet = T) {
  # Interpretation:
    i = paste("MAD is the median of the absolute differences of all values of x from the median of x,",
              "where exactly half of all the values are less than or more than the MAD value.",
              "For a normal distribution, MAD is approx the sd/1.48. Thus, MAD is often scaled by 1.4826",
              "so as to act as a robust estimator of the standard deviation: stats::mad() does this by default.")
    if(!quiet) {print(i, quote = FALSE)}
  # Calculation:
    median(abs(x - median(x)))
}

# Quantile range
quantile_range = function(x, lower, upper, quiet = T) {
  # Interpretation:
    i = paste("The return value is the range from the ", lower*100, "th to the ", upper*100, "th percentile, ",
              "which is using the ", (upper - lower)*100, "% inner range of values. ",
              "When the 25 and 75th percentile are used, the difference is the interquartile range (IQR). ",
              "This be calculated using IQR()  in base R. In a normal distribution, we can get a robust estimator of ",
              "the standard deviation by dividing IQR by 1.349.",
              sep = "")
    if(!quiet) {print(i, quote = FALSE)}
  # Calculation:
    quantile(x, probs = c(lower, upper)) %>%
      unname() %>%
      diff()
}


# (4) Measures of skewness ------------------------------------------------

# Skewness
skew = function(x, df = 1, quiet = T) {
  # Interpretation:
    i = paste("The 3rd standardized momement: skewness is a measure of asymmetry of a distribution. ",
              "When positive, we have a right-skewed or right-tailed distribution, or positive skew. ",
              "When negative, we have a left-skewed or left-tailed distribution, or negative skew. ",
              "See also psych::skew() and moments::skewness().",
              sep = "")
     if(!quiet) {print(i, quote = FALSE)}
  # Calculation:
    xbar = mean(x)
    s = sd(x)
    mean((x - xbar)^3)/s^3
}

# Quantile skewness
skew.quantile = function(x, p = 0.25, quiet = T) {
  # Interpretation:
    i = paste("In a symmetric distribution, the median would be in the exact center of any quantile range. ",
              "Here, we calculate a skewness value based on the ", p*100, "th and ", (1-p)*100, "th quantile. ",
              "Try p = 1/8 for an octile skew, or p = 1/10 for a decile skew.",
              sep = "")
   if(!quiet) {print(i, quote = FALSE)}
  # Calculation:
    Q = quantile(x, probs = c(p, 0.5, 1 - p)) %>%
      unname()
    Q_l = Q[1]; m = Q[2]; Q_u = Q[3]
    ((Q_u - m) - (m - Q_l)) / (Q_u - Q_l)
}

# Nonparametric skewness
skew.np = function(x, quiet = T) {
  # Interpretation:
    i = paste("Nonparametric skewness:  positive if the mean is the the right of the median, ",
              "and negative if the mean is to the left of the median. ",
              "The value is bounded by -1 and 1 since the median is always less than 1 sd from the mean. ",
              "Note: this value does not correspond to other measures of skewness, such as skew() or skew.quantile().",
              sep = "")
    if(!quiet) {print(i, quote = FALSE)}
  # Calculation:
    (mean(x) - median(x))/sd(x) 
}

# Pearson's 2nd skewness coefficient
skew.pearson2 = function(x, quiet = T) {
  # Interpretation:
    i = paste("Pearson's 2nd skewness coefficient: 3x the nonparametric skewness measure. ",
              "As such, the value is bounded by -3 and 3. ",
              "See skew.np() for more interpretation.",
              sep = "")
    if(!quiet) {print(i, quote = FALSE)}
  # Calculation:
    3*skew.np(x)
}


# (5) Measures of kurtosis ------------------------------------------------

# Kurtosis
kurtosis = function(x, quiet = T) {
  # Interpretation:
    i = paste("The 4th standardized momement: kurtosis relates to the mass of a distribution's tails. ",
              "In a normal distribution, kurtosis has a value of 3, see kurtosis.excess(). ",
              "See moments::kurtosis() for another measure of kurtosis.",
              sep = "")
    if(!quiet) {print(i, quote = FALSE)}
  # Calculation:
    z = (x - mean(x))/sd(x)
    mean(z^4)
}

# Excess kurtosis
kurtosis.excess = function(x, quiet = T) {
  # Interpretation:
    i = paste("In a normal distribution, kurtosis has a value of 3, see kurtosis(). ",
              "As such, it is conventional to subtract 3 from the kurtosis function. ",
              "'Excess kurtosis' is often being implemented rather than kurtosis, although this may not be clear. ",
              "Distributions with 0 or near 0 excess kurtosis are known as 'mesokurtic'. Those with ",
              "positive excess kurtosis are 'leptokurtic', and negative excess kurtosis are 'platykurtic'.",
              sep = "")
    if(!quiet) {print(i, quote = FALSE)}
  # Calculation:
    kurtosis(x) - 3
}


