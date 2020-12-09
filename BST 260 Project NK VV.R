library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)

#Importing ACS data 
pob_acs <- read.csv("BST 260 Project Data.csv")

#Importing FGM Prev data 
fgm_prev <- read_xlsx("FGM-prevalence.xlsx")

#Filtering data set down to countries that practice FGM 
pob_fgm <- pob_acs %>% filter(Place.of.birth %in% c("Yemen",
                                                  "Iraq",
                                                  "Cameroon",
                                                  "Egypt",
                                                  "Ethiopia",
                                                  "Eritrea",
                                                  "Gambia",
                                                  "Ghana",
                                                  "Guinea",
                                                  "Ivory Coast (2017 or later)",
                                                  "Kenya",
                                                  "Liberia",
                                                  "Nigeria",
                                                  "Senegal",
                                                  "Sierra Leone",
                                                  "Somalia",
                                                  "Sudan",
                                                  "Tanzania",
                                                  "Togo",
                                                  "Uganda"))

#Changing the variable names 
pob_fgm <- pob_fgm %>% rename(Country = Place.of.birth)
fgm_prev <- fgm_prev %>% rename("fgm_prevalence" = "FGM prevalence among girls and women (%)")

# Changing the names of countries to match 
pob_fgm$Country <- str_replace_all(pob_fgm$Country, "Ivory Coast \\(2017 or later\\)", "Côte d'Ivoire")
fgm_prev$Country <- str_replace_all(fgm_prev$Country, "United Republic of Tanzania", "Tanzania")

#Join the dataset 
fgm_combined <- inner_join(pob_fgm, fgm_prev, by = c("Country"))

#Remove male and total population columns 
fgm_combined <- fgm_combined[-c(2,3)]

#Estimate the number of women who have experienced FGM in US 
fgm_combined$fgm_estimate <- fgm_combined$Female*(fgm_combined$fgm_prevalence/100)

#Total number of women in the US who are at risk/have experienced FGM 
fgm_combined %>% summarise(sum(fgm_estimate))

#Creating some plots
fgm_combined %>% 
  ggplot() +
  geom_bar(aes(x=Country, y = Female), stat = "identity", fill = "lightsteelblue") + 
  xlab("Country of Birth") + 
  ylab("Number of Women") +
  theme(axis.text.x = element_text(size = 6.5, angle = 65, hjust=1))+
  ggtitle("Number of women who migrated to the US by Country of Birth") 

fgm_combined %>% 
  ggplot() +
  geom_bar(aes(x=Country, y = fgm_prevalence), stat = "identity", fill = "lightsteelblue") + 
  xlab("Country") + 
  ylab("Prevalence of FGM (%)") +
  theme(axis.text.x = element_text(size = 6.5, angle = 65, hjust=1))+
  ggtitle("Prevalence of FGM by Country") 

fgm_combined %>% 
  ggplot() +
  geom_bar(aes(x=Country, y = fgm_estimate), stat = "identity", fill = "lightsteelblue") + 
  xlab("Country of Birth") + 
  ylab("Number of Women") +
  theme(axis.text.x = element_text(size = 6.5, angle = 65, hjust=1))+
  ggtitle("Estimate of women in US who have experienced FGM by Country of Birth") 




