
#Project Title: US Estimates of the number of females who have experienced FGM  
#Names: Neena Kapoor & Valentina Vargas 
#TA: Jane 

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)


# Import the ACS data. These data are from American Community Survey (ACS), a survey conducted by the US census. From this data source, we extracted estimates of the place of birth of men and women in the United States. The place of birth includes both US states and countries outside of the US. ACS provides yearly estimates, but we chose a 5-year estimate from 2018 because these data have the largest sample size and the data is collected for all areas in the US, despite population size. Thus, we thought these data would give us the most accurate estimates of US women's place of birth. 
#Importing ACS data 
pob_acs <- read.csv("BST 260 Project Data.csv")

# Import data on FGM Prevalence. We obtained these data from UNICEF, who compiled the prevalence of FGM in countries. They used data sources such as DHS, MICS, EDSF, which are large-scale population based global surveys. 
#Importing FGM Prev data 
fgm_prev <- read_xlsx("FGM-prevalence.xlsx")

# Here we filter the ACS dataset to only include countries that practice FGM, according to the UNICEF dataset. Thus, we are left with a filtered dataset of ACS estimates of women in the US who were born in countries that practice FGM. 
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

# We changed the Place.of.birth variable name to Country, for future merging of the data. We also change the FGM prevalence among girls and women (%) variable name to fgm_prevalence, to make future coding easier. 
#Changing the variable names 
pob_fgm <- pob_fgm %>% rename(Country = Place.of.birth)
fgm_prev <- fgm_prev %>% rename("fgm_prevalence" = "FGM prevalence among girls and women (%)")

# In order to make the country names match in the ACS data and UNICEF data, we changed the names of a few countries, so that we could later merge the datasets without error. 
# Changing the names of countries to match 
pob_fgm$Country <- str_replace_all(pob_fgm$Country, "Ivory Coast \\(2017 or later\\)", "Côte d'Ivoire")
fgm_prev$Country <- str_replace_all(fgm_prev$Country, "United Republic of Tanzania", "Tanzania")

# We joined the datasets into one final dataset for analysis. This final dataset has the number of women in the US who were born in countries that practice FGM, as well as the prevalence of FGM in that country. 
#Join the dataset 
fgm_combined <- inner_join(pob_fgm, fgm_prev, by = c("Country"))

# We removed the male and total column that are not needed for analysis. 
#Remove male and total population columns 
fgm_combined <- fgm_combined[-c(2,3)]

# To obtain the estimate the number of women living in the US who had experienced FGM, we created a new variable that was the multiplication of the country FGM estimate and the number of women born in that country, living in the US. 
#Estimate the number of women who have experienced FGM in US 
fgm_combined$fgm_estimate <- fgm_combined$Female*(fgm_combined$fgm_prevalence/100)

# We summarized (sum) the new variable calculated above to obtain the total number of women in the US who have experienced FGM. 
#Total number of women in the US who are have experienced FGM 
fgm_combined %>% summarise(sum(fgm_estimate))

#Creating some plots

# Here we graph the number of women living in the US by their place of birth, only including countries that practice FGM. 
fgm_combined %>% 
  ggplot() +
  geom_bar(aes(x=Country, y = Female), stat = "identity", fill = "lightsteelblue") + 
  xlab("Country of Birth") + 
  ylab("Number of Women") +
  theme(axis.text.x = element_text(size = 6.5, angle = 65, hjust=1))+
  ggtitle("Number of women living in the US by Country of Birth") 

# Here we graph the prevalence of FGM (%) in each country that practices FGM. 
fgm_combined %>% 
  ggplot() +
  geom_bar(aes(x=Country, y = fgm_prevalence), stat = "identity", fill = "lightsteelblue") + 
  xlab("Country") + 
  ylab("Prevalence of FGM (%)") +
  theme(axis.text.x = element_text(size = 6.5, angle = 65, hjust=1))+
  ggtitle("Prevalence of FGM by Country") 

# Here we graph the estimates of women living in the US who have experienced FGM by their country of birth.
fgm_combined %>% 
  ggplot() +
  geom_bar(aes(x=Country, y = fgm_estimate), stat = "identity", fill = "lightsteelblue") + 
  xlab("Country of Birth") + 
  ylab("Number of Women") +
  theme(axis.text.x = element_text(size = 6.5, angle = 65, hjust=1))+
  ggtitle("Estimate of women in US who have experienced FGM by Country of Birth") 


