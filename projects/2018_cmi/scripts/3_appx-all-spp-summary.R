### Appendix 1 - all fish caught 

library(tidyverse)

Reach = cmi %>% select(SeineID, Reach)
reach = left_join(fish, Reach, by = "SeineID") %>%
  select(SeineID, SiteID, Reach) %>%
  distinct()

tmp = fish %>% 
  select(SeineID, Common, Count) %>%
  group_by(SeineID, Common) %>%
  mutate(Seine.Count = sum(Count, na.rm = T)) %>%
  select(-Count) %>%
  distinct() %>% 
  left_join(., reach, by = "SeineID") %>%
  mutate(CPUE = (Seine.Count/Reach)) %>%
  select(SiteID, SeineID, Common, CPUE)

rm(Reach, reach)

# Let's first calculate the number of juveniles (J) fish compared to the total...
tmp %>%
  filter(grepl("(J)", Common)) %>%
  ungroup() %>%
  summarise(J.CPUE = sum(CPUE, na.rm = TRUE)) -> J
tmp %>%
  ungroup() %>%
  summarise(ALL.CPUE = sum(CPUE, na.rm = TRUE)) -> All
J/All

# the J/A classes will hinder our use of SpeciesList data...
# call Common something else like 'fishes' bc SpeciesList also uses 'Common'
tmp = rename(tmp, fishes = Common)

# Now we summarize the info we want:
# The tmp1 df only lists taxa and summed cpue
tmp1 = tmp %>%
  group_by(fishes) %>%
  summarise(sum.CPUE = sum(CPUE, na.rm = TRUE))

# subtract last four characters of 'fishes' then make it its own column to join w/ SpeciesList
tmp1$Common = substr(tmp1$fishes, 1, nchar(tmp1$fishes)-4)
tmp1 = left_join(tmp1, SpeciesList, by = "Common") 

# let's clean up the unused columns:
tmp1 = tmp1 %>%
  select(Family,
         fishes,
         Scientific,
         Total.CPUE = sum.CPUE) %>%
  ungroup() # not sure if this is necessary. leftover from old combined code

# now we can create our table:
tmp1 = tmp1 %>%
  group_by(Family) %>%
  mutate(fam.CPUE = sum(Total.CPUE)) %>%
  ungroup() %>%
  mutate(perc.fam.CPUE = 100*fam.CPUE/sum(Total.CPUE)) %>%
  select(Family,
         fam.CPUE,
         perc.fam.CPUE,
         Common = fishes, # re-label 'fishes' as 'Common'
         Scientific,
         Total.CPUE) %>%
  arrange(desc(perc.fam.CPUE),
          desc(Total.CPUE))

# Write the table
write.csv(tmp1, "/Users/chguo/nearshore-fish-communities/2018_cmi/Figures/appx1.csv", row.names = FALSE)

# Let's make another df tmp2 a summary of taxa cpue by site...

