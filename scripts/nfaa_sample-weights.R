## ----include = FALSE-----------------------------------------------------------------------------------------
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(size = "scriptsize")


## ----results = 'hide'----------------------------------------------------------------------------------------
# Packages
library(MASS)
library(lubridate)
library(here)
library(broom)
library(multcomp)
library(tidyverse)
library(tweedie)
library(statmod)

# Directory
wd = here()
dirs = wd %>% list.files() %>% str_subset(pattern = "^README|^LICENSE|.md$|.Rproj$", negate = TRUE)
for (i in seq_along(dirs)) {
  name = str_replace_all(dirs[i], "^", "dir.")
  path = str_replace_all(dirs[i], "^", str_c(wd, "/"))
  assign(name, path)
  rm(name, path, i)
}

# Options

# Source/Load
richness.0 = readRDS(file.path(dir.data, "nfaa_richness.rds"))


## ------------------------------------------------------------------------------------------------------------
# Summary
glimpse(richness.0)

# Tabulate number of samples by replicate freq
summarise(richness.0, n = n(), .by = Replicates) %>% 
  arrange(Replicates)


## ------------------------------------------------------------------------------------------------------------
# Visualize richness by replicates
boxplot(S ~ Replicates, data = richness.0)

# Combine samples with 7 or more replicates for a more balanced test
richness.1 = mutate(richness.0,
       Replicates = as.numeric(Replicates),
       f.R = ifelse(Replicates > 6, "7+", Replicates) %>% as.ordered())

# Tabulate number of samples by factored replicates
summarise(richness.1, n = n(), .by = f.R) %>% 
  arrange(f.R)

# Re-visualize
boxplot(S ~ f.R, data = richness.1)


## ------------------------------------------------------------------------------------------------------------
# analysis of variance
aov = aov(S ~ f.R, data = richness.1) # effects may be unbalanced
summary(aov)

# pairwise test
TukeyHSD(aov) %>% 
  tidy() %>% 
  select(-term) %>% 
  knitr::kable(digits = 3)

# residuals
aov.res = residuals(aov)
plot(aov.res)
par(mfrow = c(2, 2))
plot(aov)

# check for homogeneity
shapiro.test(aov.res)
# result: violation of homogeneity


## ------------------------------------------------------------------------------------------------------------
# kruskal-wallis test
kruskal.test(S ~ f.R, data = richness.1)

# one way t-test not assuming equal variances
oneway.test(S ~ f.R, data = richness.1)

# pairwise test
pairwise.t.test(richness.1$S, richness.1$f.R, p.adjust.method = "BH", pool.sd = FALSE)

# plot t.test results
ggplot(richness.1) +
  aes(x = f.R, y = S, group = f.R) +
  geom_boxplot() +
  ggpubr::geom_signif(test = "t.test",
              comparisons = list(c(1, 2),
                                 c(2, 3),
                                 c(3, 4),
                                 c(4, 6)),
              y_position = c(33, 36, 39, 42),
              map_signif_level = TRUE)


## ------------------------------------------------------------------------------------------------------------
# plot Richness freq distribution
richness.1 %>% pull(S) %>% table() %>% barplot(xlab = "S", ylab = "Frequency")
# plot Richness freq distribution facet by replicates
ggplot(richness.1) +
  aes(x = S) +
  geom_histogram(bins = 31) +
  facet_wrap(~ f.R, scales = "free")


## ------------------------------------------------------------------------------------------------------------
# Model S ~ Replicates

# lm
lm = lm(S ~ f.R, data = richness.1)
summary(lm)
plot(lm)
# link to interp of coefficients by the-mad-statter, https://stackoverflow.com/a/57513718/24244712

# poisson
pois = glm(S ~ f.R, data = richness.1, family = poisson(link = log))
summary(pois)
plot(pois)

# inverse gamma
invg = glm(S ~ f.R, data = richness.1, family = Gamma(link = "inverse"))
summary(invg)
tidy(invg)
plot(invg)

# AIC test
AIC(lm, pois, invg) # inv gamma better

# Null glm test
null = glm(S ~ 1, data = richness.1, family = Gamma(link = "inverse"))
anova(null, invg, test = "LRT") # sig

# Predicted S + se and residuals
invg.pred = augment(invg, type.predict = "response", se_fit = TRUE)

# Plot residuals
ggplot(invg.pred) +
  aes(x = f.R, y = .resid) +
  geom_point() +
  geom_hline(aes(yintercept = 0))
invg.pred %>%
  distinct(f.R, .fitted, .se.fit) %>%
  group_by(f.R) %>%
  summarise(fit = .fitted,
            lower_ci = .fitted - 1.96 * .se.fit,
            upper_ci = .fitted + 1.96 * .se.fit)
# Plot observed S and fitted S w/ CI
ggplot(invg.pred) +
  aes(x = f.R) +
  geom_point(aes(y = S), position = position_nudge(x = -0.1)) +
  geom_point(aes(y = .fitted + .resid), color = "blue", position = position_nudge(x = 0.1))

# Multcomp
glht(model = invg, linfct = mcp(f.R = "Tukey")) %>% summary()



## ------------------------------------------------------------------------------------------------------------
# plot Richness freq distr + Estimate facet by replicates
ggplot(invg.pred) +
  aes(x = S) +
  geom_histogram() +
  geom_vline(aes(xintercept = .fitted), color = "red") +
  facet_wrap(~ f.R, scales = "free_y")

# MASS
gamma.shape(invg)
gamma.dispersion(invg)


## ------------------------------------------------------------------------------------------------------------
# Tweedie

# Estimate xi 
twd.profile = tweedie.profile(S ~ f.R, data = richness.1, do.plot = TRUE)

# The index parameter, xi
(xi = twd.profile$xi.max)

# Phi
(phi = twd.profile$phi.max)

twd = glm(S ~ f.R, data = richness.1, family = tweedie(var.power = xi))
plot(twd)
# S.twd = cplm::cpglm(S ~ f.Replicates, link = "log", data = tmp)
(twd.sum = summary(twd))

# AIC test
AIC(invg); AICtweedie(twd)

# Multcomp
glht(model = invg, linfct = mcp(f.R = "Tukey")) %>% summary()

# Modelled probability of P(Y=0)
twd.aug = augment(twd, type.predict = "response", se_fit = TRUE)
# tidy(twd, conf.int = TRUE) # another option to augment()

# Mean estimates 

# predict() method
# predict(twd, newdata = new, type = "response")

# Use augment()
twd.fit = select(twd.aug, f.R, mu.fR = .fitted) %>% distinct() %>% arrange(f.R)
# Save mean estimates as vector
mu.fR = twd.fit %>% pull(mu.fR)
names(mu.fR) = twd.fit %>% pull(f.R)
mu.fR

# Compare estimates to data means
summarise(richness.1, S.mean = mean(S), .by = f.R) %>% 
  arrange(f.R) %>%
  add_column(mu.fR)

# Underlying poisson and gamma distributions
tweedie.convert(xi = xi, mu = mu.fR, phi = phi) %>%
  as_tibble() %>% 
  add_column(f.R = names(mu.fR), .before = 1)


## ------------------------------------------------------------------------------------------------------------
y = seq(1, 31, 1)
dt = dtweedie(y = y, power = xi, mu = mu.fR[1], phi = phi)
n = pull(richness.1, f.R) %>% table()
plot(x, dt * n[1])
n.tbl = as_tibble(n) %>% 
  rename(f.R = ".") %>% 
  mutate(f.R = as.ordered(f.R))

S.freq = richness.1 %>% 
  count(f.R, S, name = "freq") %>%
  complete(f.R, S = y, fill = list(freq = 0)) %>%
  rename(x = S)

S.dens = map(mu.fR, function (x) dtweedie(y = y, power = xi, mu = x, phi = phi)) %>%
  as_tibble() %>%
  rowid_to_column(var = "x") %>%
  pivot_longer(cols = matches("[^x]"), names_to = "f.R", values_to = "y") %>%
  mutate(f.R = as.ordered(f.R)) %>%
  left_join(n.tbl, by = join_by(f.R))

full_join(S.freq, S.dens, by = join_by(f.R, x)) %>%
  ggplot(aes(x = x)) +
  geom_line(aes(y = y * n), col = "red") +
  geom_col(aes(y = freq)) +
  facet_wrap(~ f.R, scales = "free_y") +
  labs(x = "Frequency", y = "Richness")



## ------------------------------------------------------------------------------------------------------------
#### Re-run tweedie model for three groups Replicates

# Re-factor Replicates
richness.2 = mutate(richness.1, f.R = ifelse(Replicates > 2, "3+", Replicates) %>% as.ordered())

# Estimate xi 
twd.profile = tweedie.profile(S ~ f.R, data = richness.2, do.plot = TRUE)

# Xi and Phi, these overwrite previous profile estimates
(xi = twd.profile$xi.max); (phi = twd.profile$phi.max)

twd2 = glm(S ~ f.R, data = richness.2, family = tweedie(var.power = xi))
plot(twd2)
summary(twd2)

# AIC test - is this OK to compare?
AICtweedie(twd); AICtweedie(twd2)

# Modelled probatwd2.profile

# Modelled probability of P(Y=0)
twd2.aug = augment(twd2, type.predict = "response", se_fit = TRUE)
# Use augment()
twd2.fit = select(twd2.aug, f.R, fit = .fitted, se.fit = .se.fit) %>% distinct() %>% arrange(f.R)

# Save mean estimates as vector (these overwrite previous objects)
mu.fR = twd2.fit %>% pull(fit)
names(mu.fR) = twd2.fit %>% pull(f.R)
mu.fR

# Compare estimates to data means
summarise(richness.2, S.mean = mean(S), .by = f.R) %>% 
  arrange(f.R) %>%
  add_column(mu.fR)

# Interpret underlying poisson and gamma distributions
tweedie.convert(xi = xi, mu = mu.fR, phi = phi) %>%
  as_tibble() %>% 
  add_column(mu.fR, .before = 1)

# Plot tweedie predicted values
y = seq(1, 31, 1)
for (i in 1:length(mu.fR)) {
  dt = dtweedie(y = y, power = xi, mu = mu.fR[i], phi = phi)
  n = pull(richness.2, f.R) %>% table()
  plot(y, dt * n[i],
       xlab = "Richness", ylab = "Predicted frequency",
       main = str_c("Predicted sample frequency when replicates = ", names(mu.fR)[i]))
}



## ------------------------------------------------------------------------------------------------------------
# Define dataframes for plotting:
# n samples per Replicate group for scaling
n.tbl = as_tibble(n) %>% 
  rename(f.R = ".") %>% 
  mutate(f.R = as.ordered(f.R))

# Recreate S frequency data
S.freq = richness.2 %>% 
  count(f.R, S, name = "freq") %>%
  complete(f.R, S = y, fill = list(freq = 0)) %>%
  rename(x = S)

# Calculate tweedie densities based on mean estimates
S.dens = map(mu.fR, function (x) dtweedie(y = y, power = xi, mu = x, phi = phi)) %>%
  as_tibble() %>%
  rowid_to_column(var = "x") %>%
  pivot_longer(cols = matches("[^x]"),
               names_to = "f.R", names_transform = list(f.R = as.ordered),
               values_to = "y") %>%
  left_join(n.tbl, by = join_by(f.R))

# Mean and 95% confidence interval
S.est = twd2.fit %>%
  mutate(lower_ci = fit - 1.96 * se.fit,
         upper_ci = fit + 1.96 * se.fit,
         se_pi = sqrt(se.fit^2 + (fit * sigma(twd2)^2)),
         lower_pi = fit - 1.96 * se_pi,
         upper_pi = fit + 1.96 * se_pi)

# Plot density with mean estimates
ggplot(twd2.aug) +
  aes(x = S, group = f.R) +
  geom_histogram(bins = 31) +
  geom_vline(aes(xintercept = .fitted), color = "red") +
  facet_wrap(~ f.R, scales = "free_y")

# Plot frequency with mean est + 95% CI
full_join(S.freq, S.dens, by = join_by(f.R, x)) %>%
  ggplot(aes(x = x, group = f.R)) +
  geom_line(aes(y = y * n), col = "blue") +
  geom_col(aes(y = freq)) +
  geom_vline(data = S.est, aes(xintercept = fit), color = "red", linetype = "solid") +
  geom_vline(data = S.est, aes(xintercept = lower_ci), color = "green", linetype = "dashed") +
  geom_vline(data = S.est, aes(xintercept = upper_ci), color = "green", linetype = "dashed") +
  facet_wrap(~ f.R) +
  labs(x = "Frequency", y = "Richness")

# Plot frequency with mean est + 95% PI
full_join(S.freq, S.dens, by = join_by(f.R, x)) %>%
  ggplot(aes(x = x, group = f.R)) +
  geom_line(aes(y = y * n), col = "blue") +
  geom_col(aes(y = freq)) +
  geom_vline(data = S.est, aes(xintercept = fit), color = "red", linetype = "solid") +
  geom_vline(data = S.est, aes(xintercept = lower_pi), color = "green", linetype = "dashed") +
  geom_vline(data = S.est, aes(xintercept = upper_pi), color = "green", linetype = "dashed") +
  facet_wrap(~ f.R) +
  labs(x = "Frequency", y = "Richness")

# Approximate sample weights by factored replicates
mu.fR / max(mu.fR)


## ------------------------------------------------------------------------------------------------------------
# Frequencies of richness factored by replicates
freq = richness.2 %>% 
  count(f.R, S, name = "freq")

# Try different models for each replicate group
models = richness.2 %>%
  group_by(f.R) %>%
  nest() %>%
  mutate(poisson = map(data, ~ glm(S ~ 1, data = ., family = poisson(link = "log"))),
         AIC.poisson = map(poisson, ~ AIC(.x)),
         gamma = map(data, ~ glm(S ~ 1, data = ., family = Gamma(link = "inverse"))),
         AIC.gamma = map(gamma, ~ AIC(.x)),
         xi = map(data, ~ tweedie.profile(S ~ 1, data = .)$xi.max),
         phi = map(data, ~ tweedie.profile(S ~ 1, data = .)$phi.max),
         tweedie = map(data, ~ glm(S ~ 1, data = ., family = tweedie(var.power = pluck(xi, 1)))),
         AIC.tweedie = map(tweedie, ~ AICtweedie(.x))) %>%
  ungroup()

# Compare model results
models %>%
  unnest(c(AIC.poisson, AIC.gamma, AIC.tweedie)) %>%
  select(f.R, contains("AIC"))


## ------------------------------------------------------------------------------------------------------------
# Augment tweedie models
tweedie = models %>%
  mutate(fit = tweedie) %>%
  select(f.R, fit, xi, phi) %>%
  mutate(augment = map(fit, ~ augment(.x, type.predict = "response")),
         mu = map(augment, pluck(".fitted")) %>% map(pluck, 1))
# Calculate probability densities
tweedie.dens = tweedie %>%
  mutate(y = list(y)) %>%
  mutate(density = pmap(list(y, xi, mu, phi), ~ dtweedie(y = ..1, power = ..2, mu = ..3, phi = ..4)),
         probs = map(density, cumsum)) %>%
  select(f.R, S = y, density, probs) %>%
  unnest(everything()) %>%
  left_join(n.tbl, by = join_by(f.R)) %>%
  mutate(pred.freq = density * n) %>%
  left_join(S.freq, by = join_by(f.R, x))


## ------------------------------------------------------------------------------------------------------------
# Max
estimates = tweedie.dens %>%
  filter(density == max(density), .by = f.R) %>%
  select(f.R, max = S) %>%
  # Mean
  left_join(summarise(richness.2, mean = mean(S), .by = f.R), by = join_by(f.R)) %>%
  # Median
  left_join(summarise(richness.2, median = median(S), .by = f.R), by = join_by(f.R))

# View
estimates


## ------------------------------------------------------------------------------------------------------------
# Plot predicted richness frequency based on tweedie estimates
ggplot(tweedie.dens) +
  aes(x = S, group = f.R) +
  geom_line(aes(y = pred.freq), col = "blue") +
  geom_col(aes(y = freq)) +
  facet_wrap(~ f.R) +
  labs(y = "Frequency", x = "Richness")

# Plot probabilty distributions
ggplot(tweedie.dens) +
  aes(x = S, col = f.R, group = f.R) +
  geom_line(aes(y = probs))

# Pull out expected probabilities for 3+ replicate samples
expected = filter(tweedie.dens, f.R == '3+') %>% pull(probs)

# Plot richness probabilities relative to expected - these will be our sample weights (?)
ggplot(tweedie.dens) +
  aes(x = S, col = f.R, group = f.R) +
  geom_line(aes(y = (probs + expected)/(2 * probs)))


## ------------------------------------------------------------------------------------------------------------
# Pull out the weights
weights = tweedie.dens %>%
  mutate(weights = (probs + expected)/(2 * probs), .by = f.R) %>%
  select(f.R, S, weights) %>%
  arrange(f.R)

# View and save
weights; saveRDS(weights, file = file.path(dir.data, "nfaa_weights.rds"))


## ------------------------------------------------------------------------------------------------------------
knitr::purl(input = file.path(dir.scripts, "nfaa_sample-weights.Rmd"))

