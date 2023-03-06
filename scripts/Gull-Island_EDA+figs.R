library(tidyverse)
library(lubridate)
library(stringr)

# Gull Island analyses For birders group

tmp = data %>%
  filter(SiteID %in% c(638:641,
                       1384,
                       1387,
                       1393,
                       1590:1592,
                       1737))

tmp$Sp_CommonName %>% unique() %>% sort()
tmp$Sp_ScientificName %>% unique() %>% sort()

tmp.1 = mutate(tmp, Count = ifelse(Unmeasured == 0,
                                   1,
                                   Unmeasured),
               Date = mdy(Date),
               Week = week(Date),
               Month = month(Date, label = TRUE))

tmp.1 %>%
  group_by(Sp_CommonName) %>%
  summarise(Abundance = sum(Count)) %>%
  mutate(Sp_CommonName = fct_reorder(as.factor(Sp_CommonName), desc(Abundance))) %>% 
  ggplot(aes(x = Sp_CommonName, y = Abundance)) +
  geom_col() +
  geom_text(aes(label = Abundance), vjust = -0.5, size = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Species Common Name", y = "Abundance",
       title = "Abundance of species at sites near Gull Island")
# ggsave("GullIsland_Sp_abundance_raw.png", plot = last_plot(), device = 'png', path = file.path(dir.figs))

# Graph Family frequency of occurrence
tmp.1a = tmp.1 %>%
  group_by(EventID, Sp_CommonName) %>%
  summarise(Presence = n_distinct(Sp_CommonName)) %>%
  ungroup() %>%
  group_by(Sp_CommonName) %>%
  summarise(Occurrence = sum(Presence)) %>%
  mutate(Perc_Occurrence = Occurrence / n_distinct(tmp.1$EventID) * 100,
         Sp_CommonName = fct_reorder(as.factor(Sp_CommonName), desc(Perc_Occurrence)))

tmp.1a %>%
  ggplot(aes(x = Sp_CommonName, y = Perc_Occurrence)) +
  geom_col() +
  geom_text(aes(label = round(Perc_Occurrence, 1)), size = 2, vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Species Common Name", y = "Percent frequency occurrence",
       title = "Occurrence of species at sites near Gull Island")
# ggsave("GullIsland_Sp_freq_occurrence.png", plot = last_plot(), device = 'png', path = file.path(dir.figs))

# Sand Lance
tmp.2 = tmp.1 %>%
  filter(Sp_CommonName == "Pacific sand lance") %>%
  group_by(Month) %>%
  mutate(n = n_distinct(Length_mm))

tmp.2  %>%
  ggplot(data = ., aes(x = Month, y = Count)) + 
  geom_col() +
  labs(title = tmp.2$Sp_CommonName[1])

tmp.2 %>%
  filter(Month > "Apr" & Month < "Nov") %>%
  group_by(Month) %>%
  mutate(mean.Length = mean(Length_mm, na.rm = TRUE)) %>%
  ggplot(data = ., aes(x = Length_mm)) + 
  geom_freqpoly(aes(color = Month), binwidth = 2.5) +
  geom_vline(aes(xintercept = mean.Length, color = Month), linetype = "dashed") +
  theme(panel.background = element_rect(fill = "grey40"),
        legend.key = element_rect(fill = "grey40"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "grey50")) +
  facet_wrap( ~ Month, ncol = 1) -> p.2
p.2 + labs(x = "Length (mm)", y = paste("Frequency (binwidth = ", p.2$layers[[1]]$stat_params$binwidth, ")", sep = ""),
           title = paste("Size frequency of ", str_to_title(tmp.2$Sp_CommonName[1])))

# Herring
tmp.3 = tmp.1 %>%
  filter(Sp_CommonName == "Pacific herring") %>%
  group_by(Month) %>%
  mutate(n = n_distinct(Length_mm))

tmp.3  %>%
  ggplot(data = ., aes(x = Month, y = Count)) + 
  geom_col() +
  labs(title = tmp.3$Sp_CommonName[1])

tmp.3 %>%
  filter(Month > "Apr" & Month < "Nov") %>%
  group_by(Month) %>%
  mutate(mean.Length = mean(Length_mm, na.rm = TRUE)) %>%
  ggplot(data = ., aes(x = Length_mm)) + 
  geom_freqpoly(aes(color = Month), binwidth = 5) +
  geom_vline(aes(xintercept = mean.Length, color = Month), linetype = "dashed") +
  theme(panel.background = element_rect(fill = "grey40"),
        legend.key = element_rect(fill = "grey40"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "grey50")) +
  facet_wrap( ~ Month, ncol = 1) -> p.3
p.3 + labs(x = "Length (mm)", y = paste("Frequency (binwidth = ", p.3$layers[[1]]$stat_params$binwidth, ")", sep = ""),
           title = paste("Size frequency of", str_to_title(tmp.3$Sp_CommonName[1])))


# Dolly Varden
tmp.4 = tmp.1 %>%
  filter(Sp_CommonName == "Dolly Varden") %>%
  group_by(Month) %>%
  mutate(n = n_distinct(Length_mm))

tmp.4  %>%
  ggplot(data = ., aes(x = Month, y = Count)) + 
  geom_col() +
  labs(title = tmp.4$Sp_CommonName[1])

tmp.4 %>%
  filter(Month > "Apr" & Month < "Oct") %>%
  group_by(Month) %>%
  mutate(mean.Length = mean(Length_mm, na.rm = TRUE)) %>%
  ggplot(data = ., aes(x = Length_mm)) + 
  geom_freqpoly(aes(color = Month), binwidth = 25) +
  geom_vline(aes(xintercept = mean.Length, color = Month), linetype = "dashed") +
  theme(panel.background = element_rect(fill = "grey40"),
        legend.key = element_rect(fill = "grey40"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "grey50")) +
  facet_wrap( ~ Month, ncol = 1) -> p.4a
p.4a + labs(x = "Length (mm)", y = paste("Frequency (binwidth = ", p.4a$layers[[1]]$stat_params$binwidth, ")", sep = ""),
            title = paste("Size frequency of juvenile & adult", str_to_title(tmp.4$Sp_CommonName[1])))

tmp.4 %>%
  filter(Month > "Apr" & Month < "Oct") %>%
  filter(Length_mm < 200) %>%
  group_by(Month) %>%
  mutate(mean.Length = mean(Length_mm, na.rm = TRUE)) %>%
  ggplot(data = ., aes(x = Length_mm)) + 
  geom_freqpoly(aes(color = Month), binwidth = 10) +
  geom_vline(aes(xintercept = mean.Length, color = Month), linetype = "dashed") +
  theme(panel.background = element_rect(fill = "grey40"),
        legend.key = element_rect(fill = "grey40"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "grey50")) +
  facet_wrap( ~ Month, ncol = 1) -> p.4b
p.4b + labs(x = "Length (mm)", y = paste("Frequency (binwidth = ", p.4b$layers[[1]]$stat_params$binwidth, ")", sep = ""),
            title = paste("Size frequency of juvenile", str_to_title(tmp.4$Sp_CommonName[1]), "only"))


# Capelin
tmp.5 = tmp.1 %>%
  filter(Sp_CommonName == "Capelin") %>%
  group_by(Month) %>%
  mutate(n = n_distinct(Length_mm))

tmp.5  %>%
  ggplot(data = ., aes(x = Month, y = Count)) + 
  geom_col() +
  labs(title = tmp.5$Sp_CommonName[1])

tmp.5 %>%
  filter(Month == "Feb" | Month < "Nov") %>%
  group_by(Month) %>%
  mutate(mean.Length = mean(Length_mm, na.rm = TRUE)) %>%
  ggplot(data = ., aes(x = Length_mm)) + 
  geom_freqpoly(aes(color = Month), binwidth = 5) +
  geom_vline(aes(xintercept = mean.Length, color = Month), linetype = "dashed") +
  theme(panel.background = element_rect(fill = "grey40"),
        legend.key = element_rect(fill = "grey40"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "grey50")) +
  facet_wrap( ~ Month, ncol = 1) -> p.5
p.5 + labs(x = "Length (mm)", y = paste("Frequency (binwidth = ", p.5$layers[[1]]$stat_params$binwidth, ")", sep = ""),
           title = paste("Size frequency of", str_to_title(tmp.5$Sp_CommonName[1])))


# Pacific cod
tmp.6 = tmp.1 %>%
  filter(Sp_CommonName == "Pacific cod") %>%
  group_by(Month) %>%
  mutate(n = n_distinct(Length_mm))

tmp.6  %>%
  ggplot(data = ., aes(x = Month, y = Count)) + 
  geom_col() +
  labs(title = tmp.2$Sp_CommonName[1])

tmp.6 %>%
  filter(Month > "Apr" & Month < "Oct") %>%
  filter(Length_mm < 150) %>%
  group_by(Month) %>%
  mutate(mean.Length = mean(Length_mm, na.rm = TRUE)) %>%
  ggplot(data = ., aes(x = Length_mm)) + 
  geom_freqpoly(aes(color = Month), binwidth = 5) +
  geom_vline(aes(xintercept = mean.Length, color = Month), linetype = "dashed") +
  theme(panel.background = element_rect(fill = "grey40"),
        legend.key = element_rect(fill = "grey40"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "grey50")) +
  facet_wrap( ~ Month, ncol = 1) -> p.6
p.6 + labs(x = "Length (mm)", y = paste("Frequency (binwidth = ", p.6$layers[[1]]$stat_params$binwidth, ")", sep = ""),
           title = paste("Size frequency of", str_to_title(tmp.6$Sp_CommonName[1])))


# Pink salmon
tmp.7 = tmp.1 %>%
  filter(Sp_CommonName == "Pink salmon") %>%
  group_by(Month) %>%
  mutate(n = n_distinct(Length_mm))

tmp.7 %>%
  ggplot(data = ., aes(x = Month, y = Count)) + 
  geom_col() +
  labs(title = tmp.7$Sp_CommonName[1])

tmp.7 %>%
  filter(Month > "Mar" & Month < "Sep") %>%
  filter(Length_mm < 150) %>%
  group_by(Month) %>%
  mutate(mean.Length = mean(Length_mm, na.rm = TRUE)) %>%
  ggplot(data = ., aes(x = Length_mm)) + 
  geom_freqpoly(aes(color = Month), binwidth = 5) +
  geom_vline(aes(xintercept = mean.Length, color = Month), linetype = "dashed") +
  theme(panel.background = element_rect(fill = "grey40"),
        legend.key = element_rect(fill = "grey40"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "grey50")) +
  facet_wrap( ~ Month, ncol = 1) -> p.7
p.7 + labs(x = "Length (mm)", y = paste("Frequency (binwidth = ", p.7$layers[[1]]$stat_params$binwidth, ")", sep = ""),
           title = paste("Size frequency of", str_to_title(tmp.7$Sp_CommonName[1])))



