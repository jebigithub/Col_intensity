 # Investigating colonization intensity of tree species across fragments
 # Author : Mercy Korir
 # Last modified: 6.04.2024


# Loading Libraries -------------------------------------------------------

library(tidyverse)
library(vegan)
library(agricolae)
library(corrplot)


# Loading data ------------------------------------------------------------

intensity <- read_csv('data/intensity.csv')
intensity %>% 
  select(fragments, tree_species, col_intensity)

# Summary of colonization intensity(ANOVA)--------------------------------------

summary(aov(col_intensity ~ fragments * tree_species, intensity))
percentage <- intensity %>% 
  group_by(fragments, tree_species) %>% 
  summarise(n = mean(col_intensity)) %>% 
  arrange(-n) 

# Creating data output ----------------------------------------------------

write.csv(percentage, 'output/percentage.csv', row.names = TRUE)


# Building aov_model ------------------------------------------------------

aov_model <- aov(col_intensity ~ tree_species, data = intensity) 

# Testing for significance difference across tree species -----------------

TukeyHSD(aov_model)
intensity_lsd <-  LSD.test(aov_model, trt = 'tree_species')
intensity_lsd$groups
intensity_lsd$statistics

# Building aov_model ------------------------------------------------------

aov_model <- aov(col_intensity ~ fragments, data = intensity) 
intensity_lsd <-  LSD.test(aov_model, trt = 'fragments')

# Testing for significance difference across forest fragments -------------
TukeyHSD(aov_model)
intensity_lsd$groups
intensity_lsd$statistics
intensity %>% 
  mutate(intensity = (col_intensity)) %>% 
  group_by(fragments) %>% 
  summarise(sd = sd(intensity))


# Building aov_model -----------------------------------------------------

aov_model <- aov(col_intensity ~ fragments * tree_species, data = intensity) 
intensity_lsd <-  LSD.test(aov_model, trt = c('fragments', 'tree_species'))

# Testing for significance difference  ------------------------------------
TukeyHSD(aov_model)
intensity_lsd$groups
intensity_lsd$statistics

intensity %>% 
  mutate(intensity = (col_intensity)) %>% 
  #group_by(fragments * tree_species) %>% 
  summarise(sd = sd(intensity))


# Colonization intensity of tree species in Ngangao fragments -------------

# Loading data ------------------------------------------------------------

colng <- read_csv('data/colng.csv')
colng <- read_csv('data/colng.csv') %>% 
  select(fragments, tree_species, col_intensity)

# Summary of Colonization intensity(ANOVA) --------------------------------

summary(aov(col_intensity ~ tree_species, colng))
percentage <- intensity %>% 
  group_by(fragments, tree_species) %>% 
  summarise(n = mean(col_intensity)) %>% 
  arrange(-n) 

# Building aov_model ------------------------------------------------------

aov_model <- aov(col_intensity ~ tree_species, data = colng)
summary(aov(col_intensity ~ tree_species, data = colng))

# Tukey test --------------------------------------------------------------

TukeyHSD(aov_model)
colng_lsd <-  LSD.test(aov_model, trt = 'tree_species')
colng_lsd$groups
colng_lsd$statistics

# Colonization intensity of tree species in Chawia fragment --------------

colch <- read_csv('data/colch.csv')
colch <- read_csv('data/colch.csv') %>% 
  select(fragments, tree_species, col_intensity)

# Summary of colonization intensity(ANOVA) --------------------------------

summary(aov(col_intensity ~ tree_species, colch))
percentage <- colch %>% 
  group_by(fragments, tree_species) %>% 
  summarise(n = mean(col_intensity)) %>% 
  arrange(-n) 

# Building aov_model ------------------------------------------------------

aov_model <- aov(col_intensity ~ tree_species, data = colch)
summary(aov(col_intensity ~ tree_species, data = colch))

# Tukey test --------------------------------------------------------------

TukeyHSD(aov_model)
colch_lsd <-  LSD.test(aov_model, trt = 'tree_species')
colch_lsd$groups
colch_lsd$statistics

# Colonization intensity of tree species in Fururu fragment  --------------


# Loading data ------------------------------------------------------------

colfur <- read_csv('data/colfur.csv')
colfur <- read_csv('data/colfur.csv') %>% 
  select(fragments, tree_species, col_intensity)

# Summary of colonization intensity(ANOVA) --------------------------------

summary(aov(col_intensity ~ tree_species, colfur))
percentage <- colfur %>% 
  group_by(fragments, tree_species) %>% 
  summarise(n = mean(col_intensity)) %>% 
  arrange(-n) 

# Building aov_model ------------------------------------------------------

aov_model <- aov(col_intensity ~ tree_species, data = colfur)

# Tukey test --------------------------------------------------------------

TukeyHSD(aov_model)
colfur_lsd <-  LSD.test(aov_model, trt = 'tree_species')
colfur_lsd$groups
colfur_lsd$statistics





