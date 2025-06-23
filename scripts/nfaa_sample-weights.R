## ----include = FALSE--------------------------------------------------------------------------------------------------------
library(knitr)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(size = "scriptsize")


## ----results = 'hide'-------------------------------------------------------------------------------------------------------
# Packages
library(here)
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


## ---------------------------------------------------------------------------------------------------------------------------
# Summary
head(richness.0, 10)

# Tabulate number of samples by replicate freq
summarise(richness.0, n = n(), .by = Replicates) %>% 
  arrange(Replicates)

# Graph richness by replicates
boxplot(S ~ Replicates, data = richness.0)


## ---------------------------------------------------------------------------------------------------------------------------
# Combine samples with 7 or more replicates
richness.1 = mutate(richness.0,
       Replicates = as.numeric(Replicates),
       f.R = ifelse(Replicates > 6, "7+", Replicates) %>%
         as.ordered())

# Re-visualize
boxplot(S ~ f.R, data = richness.1)


## ---------------------------------------------------------------------------------------------------------------------------
library(rstatix)

# Shapiro-Wilk normality test
richness.1 %>%
  group_by(f.R) %>%
  shapiro_test(S) %>%
  kable(digits = 3)

# Analysis of Variance
aov(data = richness.1, S ~ f.R) %>%
  anova_summary()

# Tukey Honest Significant Differences
tukey_hsd(richness.1, S ~ f.R) %>%
  kable(digits = 3)

# Kruskal-Wallis rank sum test
kruskal_test(richness.1, S ~ f.R)

# Dunn's test of multiple comparisons
dunn_test(richness.1, S ~ f.R,
          p.adjust.method = "hochberg") %>%
  kable(digits = 3)


## ---------------------------------------------------------------------------------------------------------------------------
# Poisson
pois = glm(S ~ f.R, data = richness.1, family = poisson(link = log))
summary(pois)

# Inverse gamma
invg = glm(S ~ f.R, data = richness.1, family = Gamma(link = "inverse"))
summary(invg)

# AIC test
AIC(aov, pois, invg)


## ---------------------------------------------------------------------------------------------------------------------------
# Null GLM test
null = glm(S ~ 1, data = richness.1, family = Gamma(link = "inverse"))
anova(null, invg, test = "LRT")


## ---------------------------------------------------------------------------------------------------------------------------
# Multiple comparisons - Tukey all pairwise combinations
multcomp::glht(model = invg, linfct = mcp(f.R = "Tukey")) %>% summary()

# Multiple comparisons - sequential pairwise combinations
multcomp::glht(model = invg, linfct = mcp(f.R = "Sequen")) %>% summary()

# To try other compensation methods, see multcomp::contrMat


## ---------------------------------------------------------------------------------------------------------------------------
library(broom)
# Predicted S + se and residuals
invg.pred = augment(invg, type.predict = "response", se_fit = TRUE)
  
# Plot observed S and fitted S + std residuals
ggplot(invg.pred) +
  aes(x = f.R) +
  geom_violin(aes(y = S)) +
  geom_point(aes(y = .fitted), color= "blue", shape = "\U2014", size = 5) +
  geom_point(aes(y = .fitted + .std.resid), color = "blue")

# Plot residuals
ggplot(invg.pred) +
  aes(x = f.R, y = .resid) +
  geom_point() +
  geom_hline(aes(yintercept = 0))


## ----echo = FALSE-----------------------------------------------------------------------------------------------------------
# Mutate df as new factor order
richness.2 = richness.1 %>%
  mutate(f.R = ifelse(Replicates > 2,"3+", Replicates) %>%
           as.ordered())


## ---------------------------------------------------------------------------------------------------------------------------
# Estimate parameters 
twd.profile = tweedie.profile(S ~ f.R, data = richness.2, do.plot = TRUE)

# Xi and Phi estimates
(xi = twd.profile$xi.max); (phi = twd.profile$phi.max)

# Fit the model
twd = glm(S ~ f.R, data = richness.2, family = tweedie(var.power = xi))

# Model summary
summary(twd)

# PLot model results
plot(twd)


## ---------------------------------------------------------------------------------------------------------------------------
# Modelled probability of P(Y=0)
twd.fit = augment(twd, type.predict = "response", se_fit = TRUE) %>%
  select(f.R, fit = .fitted, se.fit = .se.fit) %>% 
  distinct() %>% 
  arrange(f.R)

# Save mean estimates as vector (these overwrite previous objects)
mu.fR = twd.fit %>% pull(fit)
names(mu.fR) = twd.fit %>% pull(f.R)

# Compare estimates to data means
summarise(richness.2, S.mean = mean(S), .by = f.R) %>% 
  arrange(f.R) %>%
  add_column(mu.fR)

# Our range of richness values to predict on
y = seq(1, 31, 1)

# Plot tweedie predicted values
for (i in 1:length(mu.fR)) {
  dt = dtweedie(y = y, power = xi, mu = mu.fR[i], phi = phi)
  n = pull(richness.2, f.R) %>% table()
  plot(y, dt * n[i],
       xlab = "Richness", ylab = "Predicted frequency",
       main = str_c("Predicted sample frequency when replicates = ", names(mu.fR)[i]))
}


## ---------------------------------------------------------------------------------------------------------------------------
## Define dataframes for plotting:

# n samples per Replicate group for scaling
n.tbl = as_tibble(n) %>% 
  rename(f.R = ".") %>% 
  mutate(f.R = as.ordered(f.R))

# Recreate S frequency data
S.freq = richness.2 %>% 
  count(f.R, S, name = "freq") %>%
  complete(f.R, S = y, fill = list(freq = 0))

# Calculate tweedie densities based on mean estimates
S.dens = map(mu.fR, function (x) dtweedie(y = y, power = xi, mu = x, phi = phi)) %>%
  as_tibble() %>%
  rowid_to_column(var = "S") %>%
  pivot_longer(cols = matches("[^S]"),
               names_to = "f.R", names_transform = list(f.R = as.ordered),
               values_to = "y") %>%
  left_join(n.tbl, by = join_by(f.R))

# Mean and 95% confidence interval
S.est = twd.fit %>%
  mutate(lower_ci = fit - 1.96 * se.fit,
         upper_ci = fit + 1.96 * se.fit,
         se_pi = sqrt(se.fit^2 + (fit * sigma(twd)^2)),
         lower_pi = fit - 1.96 * se_pi,
         upper_pi = fit + 1.96 * se_pi)

## Create plots:

# Plot density with mean estimates
full_join(S.freq, S.dens, by = join_by(f.R, S)) %>%
  ggplot(aes(x = S, group = f.R)) +
  geom_line(aes(y = y * n), col = "blue") +
  geom_col(aes(y = freq)) +
  facet_wrap(~ f.R, scales = "free_y") +
  labs(x = "Frequency", y = "Richness", title = "Model predictions")

# Plot frequency with mean est + 95% CI
full_join(S.freq, S.dens, by = join_by(f.R, S)) %>%
  ggplot(aes(x = S, group = f.R)) +
  geom_line(aes(y = y * n), col = "blue") +
  geom_col(aes(y = freq)) +
  geom_vline(data = S.est, aes(xintercept = fit), color = "red", linetype = "solid") +
  geom_vline(data = S.est, aes(xintercept = lower_ci), color = "green", linetype = "dashed") +
  geom_vline(data = S.est, aes(xintercept = upper_ci), color = "green", linetype = "dashed") +
  facet_wrap(~ f.R) +
  labs(x = "Frequency", y = "Richness", title = "Model predictions + 95% confidence interval")

# Plot frequency with mean est + 95% PI
full_join(S.freq, S.dens, by = join_by(f.R, S)) %>%
  ggplot(aes(x = S, group = f.R)) +
  geom_line(aes(y = y * n), col = "blue") +
  geom_col(aes(y = freq)) +
  geom_vline(data = S.est, aes(xintercept = fit), color = "red", linetype = "solid") +
  geom_vline(data = S.est, aes(xintercept = lower_pi), color = "green", linetype = "dashed") +
  geom_vline(data = S.est, aes(xintercept = upper_pi), color = "green", linetype = "dashed") +
  facet_wrap(~ f.R) +
  labs(x = "Frequency", y = "Richness", title = "Model predictions + 95% prediction interval")


## ----warning=FALSE----------------------------------------------------------------------------------------------------------
# Use purrr functions to map the 3 models over the 3 datasets
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


## ---------------------------------------------------------------------------------------------------------------------------
# Augment tweedie models
tweedie = models %>%
  mutate(fit = tweedie) %>%
  select(f.R, fit, xi, phi) %>%
  mutate(augment = map(fit, ~ augment(.x, type.predict = "response")),
         mu = map(augment, pluck(".fitted")) %>% map(pluck, 1))

# Calculate probability densities
tweedie.dens = tweedie %>%
  mutate(y = list(y)) %>%
  mutate(density = pmap(list(y, xi, mu, phi), ~ dtweedie(y = ..1, power = ..2, mu = ..3, phi = ..4))) %>%
  select(f.R, S = y, density) %>%
  unnest(everything()) %>%
  left_join(n.tbl, by = join_by(f.R)) %>%
  mutate(pred.freq = density * n) %>%
  left_join(S.freq, by = join_by(f.R, S))

# Plot predicted richness frequency
ggplot(tweedie.dens) +
  aes(x = S, group = f.R) +
  geom_line(aes(y = pred.freq), col = "blue") +
  geom_col(aes(y = freq)) +
  facet_wrap(~ f.R) +
  labs(y = "Frequency", x = "Richness")


## ---------------------------------------------------------------------------------------------------------------------------
# Calculate cumulative probabilities based on density functions
tweedie.prob = tweedie %>%
  mutate(y = list(y)) %>%
  mutate(dens = pmap(list(y, xi, mu, phi), ~ dtweedie(y = ..1, power = ..2, mu = ..3, phi = ..4)),
         prob = map(dens, cumsum)) %>%
  select(f.R, S = y, dens, prob) %>%
  unnest(everything())

# Plot probability distributions
ggplot(tweedie.prob) +
  aes(x = S, col = f.R, group = f.R) +
  geom_line(aes(y = prob)) +
  labs(y = "Cumulative probability", x = "Richness")


## ---------------------------------------------------------------------------------------------------------------------------
# Pull out 'ideal' probabilities for 3+ replicate samples
ideal.prob = filter(tweedie.prob, f.R == '3+') %>% pull(prob)
ideal.dens = filter(tweedie.prob, f.R == '3+') %>% pull(dens)

# Save plotting aesthetics
p = ggplot(tweedie.prob) + aes(x = S, col = f.R, group = f.R)

# Start with CDF - probabilities of samples being informative
p + geom_line(aes(y = prob))

# Adjust the CDF by adding together with 'ideal' probabilities,
# i.e., two possibilities of non-ideal OR ideal sample
p + geom_line(aes(y = (prob + ideal.prob)))

# Make probabilities relative to replicate group
p + geom_line(aes(y = (prob + ideal.prob) / prob))

# Return to max probability of 1
p + geom_line(aes(y = (prob + ideal.prob) / (2 * prob)))


## ---------------------------------------------------------------------------------------------------------------------------
# Pull out the weights
weights = tweedie.prob %>%
  mutate(weights = (prob + ideal)/(2 * prob), .by = f.R) %>%
  select(f.R, S, weights) %>%
  arrange(f.R)

# View and save
weights; saveRDS(weights, file = file.path(dir.data, "nfaa_weights.rds"))


## ---------------------------------------------------------------------------------------------------------------------------
knitr::purl(input = file.path(dir.scripts, "nfaa_sample-weights.Rmd"))

