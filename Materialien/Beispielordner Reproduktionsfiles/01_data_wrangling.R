#### "Conceptualizing and Measuring Autocratization Episodes" ####
# authors: "Pelke, Lars; Croissant, Aurel"
# date: 2021-01-12
# journal: Swiss Political Science Review
# written under "R version 3.6.0 (2019-12-12)"

#### Preliminaries ####

R.version$version.string

# clear workspace
rm(list=ls())

# set working directory

# downloading,installing and loading packages
# Cave: Do not procede the following code if you do not want to install the following packages


if(!is.element("tidyverse", installed.packages()[,1]))
{install.packages("tidyverse")
}else {print("The requested library is already installed.")}
library(tidyverse)
if(!is.element("countrycode", installed.packages()[,1]))
{install.packages("countrycode")
}else {print("The requested library is already installed.")}
library(countrycode)
if(!is.element("viridis", installed.packages()[,1]))
{install.packages("viridis")
}else {print("The requested library is already installed.")}
library(viridis)
if(!is.element("ggpubr", installed.packages()[,1]))
{install.packages("ggpubr")
}else {print("The requested library is already installed.")}
library(ggpubr)
if(!is.element("texreg", installed.packages()[,1]))
{install.packages("texreg")
}else {print("The requested library is already installed.")}
library(texreg)
if(!is.element("sjPlot", installed.packages()[,1]))
{install.packages("sjPlot")
}else {print("The requested library is already installed.")}
library(sjPlot)
if(!is.element("ggeffects", installed.packages()[,1]))
{install.packages("ggeffects")
}else {print("The requested library is already installed.")}
library(ggeffects)
if(!is.element("scales", installed.packages()[,1]))
{install.packages("scales")
}else {print("The requested library is already installed.")}
library(scales)
if(!is.element("Hmisc", installed.packages()[,1]))
{install.packages("Hmisc")
}else {print("The requested library is already installed.")}
library(Hmisc)
if(!is.element("corrplot", installed.packages()[,1]))
{install.packages("corrplot")
}else {print("The requested library is already installed.")}
library(corrplot)
if(!is.element("stargazer", installed.packages()[,1]))
{install.packages("stargazer")
}else {print("The requested library is already installed.")}
library(stargazer)


#### Import Data ####

# Load VDem data
vdem <- readRDS("data/vdem_10/V-Dem-CY-Full+Others-v10.rds") 
vdem$cowcode <- vdem$COWcode

#### Prepare V-DEM Data ####

vdem <- distinct(vdem, country_id, year, .keep_all= TRUE) # Distinct observations

#### store vdem_original data ####
vdem_org <- vdem

#############################################################################################
#### Definition Lührmann and Lindberg (2019): EDI decline 0.1 ####
#############################################################################################

vdem <- vdem_org 

#### Autocratization Periods ####

vdem <- vdem %>%
  select(country_name, year, v2x_polyarchy, v2x_libdem, v2x_regime) %>%
  filter(year >= 1900) %>%
  group_by(country_name) %>%
  mutate(EDI_1 = v2x_polyarchy - lag(v2x_polyarchy, 1), 
         start_auto = ifelse(EDI_1<=-0.01, 1, NA), 
         min_1 = lead(EDI_1, 1), 
         min_2 = lead(EDI_1, 2), 
         min_3 = lead(EDI_1, 3), 
         min_4 = lead(EDI_1, 4), 
         auto_end = ifelse(min_1<=-0.01 | min_2<=-0.01 | min_3<=-0.01 | min_4<=-0.01, 0, 1),
         auto_end2 = ifelse(EDI_1>=0.02, 1, 0)) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(start_auto_1 = ifelse(lag(start_auto)==1, 1, 0), 
         start_auto_2 = ifelse(lag(start_auto, 2)==1, 1, 0), 
         start_auto_3 = ifelse(lag(start_auto, 3)==1, 1, 0), 
         start_auto_4 = ifelse(lag(start_auto, 4)==1, 1, 0), 
         start_auto_four = coalesce(start_auto_1, start_auto_2, start_auto_3, start_auto_4), 
         start_auto_four = ifelse(is.na(start_auto_four), 0, start_auto_four))  %>%
  select(-c(start_auto_1, start_auto_2, start_auto_3, start_auto_4)) %>%
  select(-c(min_1, min_2, min_3, min_4)) %>%
  fill(auto_end)

vdem <- as.data.frame(vdem)
for (i in 1:nrow(vdem)) {
  vdem[i,][is.na(vdem[i,]) & vdem$auto_end[i-1] == 0 & vdem$start_auto_four[i] == 1 & vdem$start_auto[i-1] ==1 &
             vdem$auto_end2[i] != 1 ] <- 1 
}

table(vdem$start_auto)

## Autocratization Periods with a total magnitude of 0.1 (10% decrease)  ##
# nessearcy conditions: one year before autocratization started -  the decline at the end of the episode >=0.1

# first step: create auto_period ID
vdem <- vdem %>%
  mutate(start_auto = ifelse(is.na(start_auto), 0, start_auto)) %>%
  group_by(country_name) %>%
  mutate(group_id = cumsum(start_auto != lag(start_auto, default = FALSE))) # group_id for each autocra_period

vdem <- vdem %>%
  mutate(auto_period_id = str_c(country_name, group_id, sep = "_"))

# second step: create dummy in which autocratization was greater than 0.1

vdem <- vdem %>%
  group_by(auto_period_id) %>%
  mutate(last_v2x_polyarchy = last(v2x_polyarchy), 
         last_v2x_regime = last(v2x_regime))

vdem <- vdem %>%
  ungroup()%>%
  mutate(auto_dum01 = last_v2x_polyarchy - lag(last_v2x_polyarchy)) %>%
  select(-c(group_id, last_v2x_polyarchy)) 

vdem <- vdem %>%
  group_by(auto_period_id) %>%
  mutate(auto_period01 = ifelse(start_auto==1 & first(auto_dum01) <= -0.1, 1, 0 )) %>%
  rename(auto_decline = auto_dum01)

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_v2x_polyarchy = lag(v2x_polyarchy, 1), 
         lag_v2x_regime = lag(v2x_regime, 1)) 

vdem %>%
  ungroup() %>%
  filter(auto_period01==1) %>%
  count() # 954 country-years

auto_periods_EDI01 <- vdem %>%
  filter(auto_period01==1) %>%
  group_by(auto_period_id) %>%
  dplyr::summarize(country_name = first (country_name), 
            start_year = first(year), 
            end_year = last(year), 
            EDI_before = first(lag_v2x_polyarchy),
            EDI_end = last(v2x_polyarchy), 
            decline = first(auto_decline)) %>%
  dplyr::mutate(country_year = str_c(country_name, start_year, end_year, sep = "_"))

auto_countries_EDI01 <- vdem %>%
  filter(auto_period01==1) %>%
  group_by(country_name) %>%
  dplyr::summarize(countries = first (country_name)) 

# 243 autocratization episodes in 123 countries

vdem_auto_EDI01 <- vdem %>%
  select(country_name, year, auto_period_id, auto_decline, auto_period01, lag_v2x_regime, last_v2x_regime)

#############################################################################################
#### Definition Laebens and Lührmann (2020): EDI decline 0.05 ####
#############################################################################################

vdem <- vdem_org
#### Autocratization Periods ####

vdem <- vdem %>%
  select(country_name, year, v2x_polyarchy, v2x_libdem, v2x_polyarchy_codelow, v2x_polyarchy_codehigh, v2x_regime) %>%
  filter(year >= 1900) %>%
  group_by(country_name) %>%
  mutate(EDI_1 = v2x_polyarchy - lag(v2x_polyarchy, 1), 
         start_auto = ifelse(EDI_1<=-0.01, 1, NA), 
         min_1 = lead(EDI_1, 1), 
         min_2 = lead(EDI_1, 2), 
         min_3 = lead(EDI_1, 3), 
         min_4 = lead(EDI_1, 4), 
         auto_end = ifelse(min_1<=-0.01 | min_2<=-0.01 | min_3<=-0.01 | min_4<=-0.01, 0, 1),
         auto_end2 = ifelse(EDI_1>=0.02, 1, 0)) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(start_auto_1 = ifelse(lag(start_auto)==1, 1, 0), 
         start_auto_2 = ifelse(lag(start_auto, 2)==1, 1, 0), 
         start_auto_3 = ifelse(lag(start_auto, 3)==1, 1, 0), 
         start_auto_4 = ifelse(lag(start_auto, 4)==1, 1, 0), 
         start_auto_four = coalesce(start_auto_1, start_auto_2, start_auto_3, start_auto_4), 
         start_auto_four = ifelse(is.na(start_auto_four), 0, start_auto_four))  %>%
  select(-c(start_auto_1, start_auto_2, start_auto_3, start_auto_4)) %>%
  select(-c(min_1, min_2, min_3, min_4)) %>%
  fill(auto_end)

vdem <- as.data.frame(vdem)
for (i in 1:nrow(vdem)) {
  vdem[i,][is.na(vdem[i,]) & vdem$auto_end[i-1] == 0 & vdem$start_auto_four[i] == 1 & vdem$start_auto[i-1] ==1 &
             vdem$auto_end2[i] != 1] <- 1 
}

table(vdem$start_auto)

## Autocratization Periods with a total magnitude of 0.05 (5% decrease)  ##
# nessearcy conditions: one year before autocratization started -  the decline at the end of the episode >=0.05 and
# no overlap of confidence intervals  according to Laebens and Lührmann 

# first step: create auto_period ID
vdem <- vdem %>%
  mutate(start_auto = ifelse(is.na(start_auto), 0, start_auto)) %>%
  group_by(country_name) %>%
  mutate(group_id = cumsum(start_auto != lag(start_auto, default = FALSE))) # group_id for each autocra_period

vdem <- vdem %>%
  mutate(auto_period_id005 = str_c(country_name, group_id, sep = "_"))

# second step: create dummy in which autocratization was greater than 0.1

vdem <- vdem %>%
  group_by(auto_period_id005) %>%
  mutate(last_v2x_polyarchy = last(v2x_polyarchy), 
         last_v2x_polyarchy_codelow = last(v2x_polyarchy_codelow), 
         last_v2x_polyarchy_codehigh = last(v2x_polyarchy_codehigh), 
         last_v2x_regime = last(v2x_regime))

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(auto_confidence = lag(last_v2x_polyarchy_codelow) - last_v2x_polyarchy_codehigh)

vdem <- vdem %>%
  ungroup()%>%
  mutate(auto_dum005 = last_v2x_polyarchy - lag(last_v2x_polyarchy)) %>%
  select(-c(group_id)) 

summary(vdem$auto_confidence) # whenever auto_confidence is positive, the confidence intervals do not overlag each other

vdem <- vdem %>%
  group_by(auto_period_id005) %>%
  mutate(auto_period005 = ifelse(start_auto==1 & first(auto_dum005) <= -0.05 & first(auto_confidence) >0 , 1, 0 )) %>% 
  rename(auto_decline005 = auto_dum005) # no overlap of confidence intervals before and after autocratization 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_v2x_polyarchy = lag(v2x_polyarchy, 1), 
         lag_v2x_regime = lag(v2x_regime, 1))

# summary statistics #
table(vdem$auto_period005) # 1299 country-years

auto_periods_EDI005 <- vdem %>%
  filter(auto_period005==1) %>%
  group_by(auto_period_id005) %>%
  dplyr::summarize(country_name = first (country_name), 
            start_year = first(year), 
            end_year = last(year), 
            EDI_before = first(lag_v2x_polyarchy),
            EDI_end = last(v2x_polyarchy), 
            decline = first(auto_decline005))  %>%
  dplyr::mutate(country_year = str_c(country_name, start_year, sep = "_"))

auto_countries_EDI005 <- vdem %>%
  filter(auto_period005==1) %>%
  group_by(country_name) %>%
  dplyr::summarize(countries = first (country_name)) 

# 344 autocratization episodes in 143 countries

vdem_auto_EDI005 <- vdem %>%
  select(country_name, year, auto_period_id005, auto_decline005, auto_period005, last_v2x_regime, lag_v2x_regime)


#############################################################################################
#### Own defintion according to Lührmann and Lindberg (2019): EDI decline 0.1 + CI ####
#############################################################################################

vdem <- vdem_org
#### Autocratization Periods ####

vdem <- vdem %>%
  select(country_name, year, v2x_polyarchy, v2x_libdem, v2x_polyarchy_codelow, v2x_polyarchy_codehigh, v2x_regime) %>%
  filter(year >= 1900) %>%
  group_by(country_name) %>%
  mutate(EDI_1 = v2x_polyarchy - lag(v2x_polyarchy, 1), 
         start_auto = ifelse(EDI_1<=-0.01, 1, NA), 
         min_1 = lead(EDI_1, 1), 
         min_2 = lead(EDI_1, 2), 
         min_3 = lead(EDI_1, 3), 
         min_4 = lead(EDI_1, 4), 
         auto_end = ifelse(min_1<=-0.01 | min_2<=-0.01 | min_3<=-0.01 | min_4<=-0.01, 0, 1),
         auto_end2 = ifelse(EDI_1>=0.02, 1, 0)) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(start_auto_1 = ifelse(lag(start_auto)==1, 1, 0), 
         start_auto_2 = ifelse(lag(start_auto, 2)==1, 1, 0), 
         start_auto_3 = ifelse(lag(start_auto, 3)==1, 1, 0), 
         start_auto_4 = ifelse(lag(start_auto, 4)==1, 1, 0), 
         start_auto_four = coalesce(start_auto_1, start_auto_2, start_auto_3, start_auto_4), 
         start_auto_four = ifelse(is.na(start_auto_four), 0, start_auto_four))  %>%
  select(-c(start_auto_1, start_auto_2, start_auto_3, start_auto_4)) %>%
  select(-c(min_1, min_2, min_3, min_4)) %>%
  fill(auto_end)

vdem <- as.data.frame(vdem)
for (i in 1:nrow(vdem)) {
  vdem[i,][is.na(vdem[i,]) & vdem$auto_end[i-1] == 0 & vdem$start_auto_four[i] == 1 & vdem$start_auto[i-1] ==1 &
             vdem$auto_end2[i] != 1] <- 1 
}

table(vdem$start_auto)

## Autocratization Periods with a total magnitude of 0.1 (1% decrease)  ##
# nessearcy conditions: one year before autocratization started -  the decline at the end of the episode >=0.1 and
# no overlap of confidence intervals  according to Laebens and Lührmann 

# first step: create auto_period ID
vdem <- vdem %>%
  mutate(start_auto = ifelse(is.na(start_auto), 0, start_auto)) %>%
  group_by(country_name) %>%
  mutate(group_id = cumsum(start_auto != lag(start_auto, default = FALSE))) # group_id for each autocra_period

vdem <- vdem %>%
  mutate(auto_period_id01ci = str_c(country_name, group_id, sep = "_"))

# second step: create dummy in which autocratization was greater than 0.1

vdem <- vdem %>%
  group_by(auto_period_id01ci) %>%
  mutate(last_v2x_polyarchy = last(v2x_polyarchy), 
         last_v2x_polyarchy_codelow = last(v2x_polyarchy_codelow), 
         last_v2x_polyarchy_codehigh = last(v2x_polyarchy_codehigh),
         last_v2x_regime = last(v2x_regime))

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(auto_confidence = lag(last_v2x_polyarchy_codelow) - last_v2x_polyarchy_codehigh)

vdem <- vdem %>%
  ungroup()%>%
  mutate(auto_dum01ci = last_v2x_polyarchy - lag(last_v2x_polyarchy)) %>%
  select(-c(group_id)) 

summary(vdem$auto_confidence) # whenever auto_confidence is positive, the confidence intervals do not overlag each other

vdem <- vdem %>%
  group_by(auto_period_id01ci) %>%
  mutate(auto_period01ci = ifelse(start_auto==1 & first(auto_dum01ci) <= -0.1 & first(auto_confidence) >0 , 1, 0 )) %>% 
  rename(auto_decline01ci = auto_dum01ci) # no overlap of confidence intervals before and after autocratization 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_v2x_polyarchy = lag(v2x_polyarchy, 1), 
         lag_v2x_regime = lag(v2x_regime, 1))

# summary statistics #
table(vdem$auto_period01ci)

auto_periods_EDI01ci <- vdem %>%
  filter(auto_period01ci==1) %>%
  group_by(auto_period_id01ci) %>%
  dplyr:: summarize(country_name = first (country_name), 
            start_year = first(year), 
            end_year = last(year), 
            EDI_before = first(lag_v2x_polyarchy),
            EDI_end = last(v2x_polyarchy), 
            decline = first(auto_decline01ci))  %>%
  dplyr::mutate(country_year = str_c(country_name, start_year, sep = "_"))

anti_join(auto_periods_EDI01, auto_periods_EDI01ci, by = "country_year")

auto_countries_EDI01ci <- vdem %>%
  filter(auto_period01ci==1) %>%
  group_by(country_name) %>%
  dplyr::summarize(countries = first (country_name)) 

# 238 autocratization episodes in 122 countries

vdem_auto_EDI01ci <- vdem %>%
  select(country_name, year, auto_period_id01ci, auto_decline01ci, auto_period01ci, last_v2x_regime, lag_v2x_regime)

#############################################################################################
#### Own defintion according to Lührmann and Lindberg (2019): EDI decline 0.15 + CI ####
#############################################################################################

vdem <- vdem_org
#### Autocratization Periods ####

vdem <- vdem %>%
  select(country_name, year, v2x_polyarchy, v2x_libdem, v2x_polyarchy_codelow, v2x_polyarchy_codehigh, v2x_regime) %>%
  filter(year >= 1900) %>%
  group_by(country_name) %>%
  mutate(EDI_1 = v2x_polyarchy - lag(v2x_polyarchy, 1), 
         start_auto = ifelse(EDI_1<=-0.01, 1, NA), 
         min_1 = lead(EDI_1, 1), 
         min_2 = lead(EDI_1, 2), 
         min_3 = lead(EDI_1, 3), 
         min_4 = lead(EDI_1, 4), 
         auto_end = ifelse(min_1<=-0.01 | min_2<=-0.01 | min_3<=-0.01 | min_4<=-0.01, 0, 1),
         auto_end2 = ifelse(EDI_1>=0.02, 1, 0)) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(start_auto_1 = ifelse(lag(start_auto)==1, 1, 0), 
         start_auto_2 = ifelse(lag(start_auto, 2)==1, 1, 0), 
         start_auto_3 = ifelse(lag(start_auto, 3)==1, 1, 0), 
         start_auto_4 = ifelse(lag(start_auto, 4)==1, 1, 0), 
         start_auto_four = coalesce(start_auto_1, start_auto_2, start_auto_3, start_auto_4), 
         start_auto_four = ifelse(is.na(start_auto_four), 0, start_auto_four))  %>%
  select(-c(start_auto_1, start_auto_2, start_auto_3, start_auto_4)) %>%
  select(-c(min_1, min_2, min_3, min_4)) %>%
  fill(auto_end)

vdem <- as.data.frame(vdem)
for (i in 1:nrow(vdem)) {
  vdem[i,][is.na(vdem[i,]) & vdem$auto_end[i-1] == 0 & vdem$start_auto_four[i] == 1 & vdem$start_auto[i-1] ==1 &
             vdem$auto_end2[i] != 1] <- 1 
}

table(vdem$start_auto)

## Autocratization Periods with a total magnitude of 0.15 (15% decrease)  ##
# nessearcy conditions: one year before autocratization started -  the decline at the end of the episode >=0.15 and
# no overlap of confidence intervals according to Laebens and Lührmann 

# first step: create auto_period ID
vdem <- vdem %>%
  mutate(start_auto = ifelse(is.na(start_auto), 0, start_auto)) %>%
  group_by(country_name) %>%
  mutate(group_id = cumsum(start_auto != lag(start_auto, default = FALSE))) # group_id for each autocra_period

vdem <- vdem %>%
  mutate(auto_period_id015ci = str_c(country_name, group_id, sep = "_"))

# second step: create dummy in which autocratization was greater than 0.15

vdem <- vdem %>%
  group_by(auto_period_id015ci) %>%
  mutate(last_v2x_polyarchy = last(v2x_polyarchy), 
         last_v2x_polyarchy_codelow = last(v2x_polyarchy_codelow), 
         last_v2x_polyarchy_codehigh = last(v2x_polyarchy_codehigh),
         last_v2x_regime = last(v2x_regime))

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(auto_confidence = lag(last_v2x_polyarchy_codelow) - last_v2x_polyarchy_codehigh)

vdem <- vdem %>%
  ungroup()%>%
  mutate(auto_dum015ci = last_v2x_polyarchy - lag(last_v2x_polyarchy)) %>%
  select(-c(group_id)) 

summary(vdem$auto_confidence) # whenever auto_confidence is positive, the confidence intervals do not overlag each other

vdem <- vdem %>%
  group_by(auto_period_id015ci) %>%
  mutate(auto_period015ci = ifelse(start_auto==1 & first(auto_dum015ci) <= -0.15 & first(auto_confidence) >0 , 1, 0 )) %>% 
  rename(auto_decline015ci = auto_dum015ci) # no overlap of confidence intervals before and after autocratization 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_v2x_polyarchy = lag(v2x_polyarchy, 1), 
         lag_v2x_regime = lag(v2x_regime, 1))

# summary statistics #
table(vdem$auto_period015ci)

auto_periods_EDI015ci <- vdem %>%
  filter(auto_period015ci==1) %>%
  group_by(auto_period_id015ci) %>%
  dplyr:: summarize(country_name = first (country_name), 
                    start_year = first(year), 
                    end_year = last(year), 
                    EDI_before = first(lag_v2x_polyarchy),
                    EDI_end = last(v2x_polyarchy), 
                    decline = first(auto_decline015ci))  %>%
  dplyr::mutate(country_year = str_c(country_name, start_year, sep = "_"))

anti_join(auto_periods_EDI01, auto_periods_EDI015ci, by = "country_year")

auto_countries_EDI015ci <- vdem %>%
  filter(auto_period015ci==1) %>%
  group_by(country_name) %>%
  dplyr::summarize(countries = first (country_name)) 

# 136 autocratization episodes in 88 countries

vdem_auto_EDI015ci <- vdem %>%
  select(country_name, year, auto_period_id015ci, auto_decline015ci, auto_period015ci, last_v2x_regime, lag_v2x_regime)


#############################################################################################
#### Own defintion according to Lührmann and Lindberg (2019): EDI decline 0.2 + CI ####
#############################################################################################

vdem <- vdem_org
#### Autocratization Periods ####

vdem <- vdem %>%
  select(country_name, year, v2x_polyarchy, v2x_libdem, v2x_polyarchy_codelow, v2x_polyarchy_codehigh, v2x_regime) %>%
  filter(year >= 1900) %>%
  group_by(country_name) %>%
  mutate(EDI_1 = v2x_polyarchy - lag(v2x_polyarchy, 1), 
         start_auto = ifelse(EDI_1<=-0.01, 1, NA), 
         min_1 = lead(EDI_1, 1), 
         min_2 = lead(EDI_1, 2), 
         min_3 = lead(EDI_1, 3), 
         min_4 = lead(EDI_1, 4), 
         auto_end = ifelse(min_1<=-0.01 | min_2<=-0.01 | min_3<=-0.01 | min_4<=-0.01, 0, 1),
         auto_end2 = ifelse(EDI_1>=0.02, 1, 0)) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(start_auto_1 = ifelse(lag(start_auto)==1, 1, 0), 
         start_auto_2 = ifelse(lag(start_auto, 2)==1, 1, 0), 
         start_auto_3 = ifelse(lag(start_auto, 3)==1, 1, 0), 
         start_auto_4 = ifelse(lag(start_auto, 4)==1, 1, 0), 
         start_auto_four = coalesce(start_auto_1, start_auto_2, start_auto_3, start_auto_4), 
         start_auto_four = ifelse(is.na(start_auto_four), 0, start_auto_four))  %>%
  select(-c(start_auto_1, start_auto_2, start_auto_3, start_auto_4)) %>%
  select(-c(min_1, min_2, min_3, min_4)) %>%
  fill(auto_end)

vdem <- as.data.frame(vdem)
for (i in 1:nrow(vdem)) {
  vdem[i,][is.na(vdem[i,]) & vdem$auto_end[i-1] == 0 & vdem$start_auto_four[i] == 1 & vdem$start_auto[i-1] ==1 &
             vdem$auto_end2[i] != 1] <- 1 
}

table(vdem$start_auto)

## Autocratization Periods with a total magnitude of 0.2 (20% decrease)  ##
# nessearcy conditions: one year before autocratization started -  the decline at the end of the episode >=0.2 and
# no overlap of confidence intervals according to Laebens and Lührmann 

# first step: create auto_period ID
vdem <- vdem %>%
  mutate(start_auto = ifelse(is.na(start_auto), 0, start_auto)) %>%
  group_by(country_name) %>%
  mutate(group_id = cumsum(start_auto != lag(start_auto, default = FALSE))) # group_id for each autocra_period

vdem <- vdem %>%
  mutate(auto_period_id02ci = str_c(country_name, group_id, sep = "_"))

# second step: create dummy in which autocratization was greater than 0.2

vdem <- vdem %>%
  group_by(auto_period_id02ci) %>%
  mutate(last_v2x_polyarchy = last(v2x_polyarchy), 
         last_v2x_polyarchy_codelow = last(v2x_polyarchy_codelow), 
         last_v2x_polyarchy_codehigh = last(v2x_polyarchy_codehigh),
         last_v2x_regime = last(v2x_regime))

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(auto_confidence = lag(last_v2x_polyarchy_codelow) - last_v2x_polyarchy_codehigh)

vdem <- vdem %>%
  ungroup()%>%
  mutate(auto_dum02ci = last_v2x_polyarchy - lag(last_v2x_polyarchy)) %>%
  select(-c(group_id)) 

summary(vdem$auto_confidence) # whenever auto_confidence is negative, the confidence intervals do not overlap each other

vdem <- vdem %>%
  group_by(auto_period_id02ci) %>%
  mutate(auto_period02ci = ifelse(start_auto==1 & first(auto_dum02ci) <= -0.2 & first(auto_confidence) >0 , 1, 0 )) %>% 
  rename(auto_decline02ci = auto_dum02ci) # no overlap of confidence intervals before and after autocratization 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_v2x_polyarchy = lag(v2x_polyarchy, 1), 
         lag_v2x_regime = lag(v2x_regime, 1))

# summary statistics #
table(vdem$auto_period02ci)

auto_periods_EDI02ci <- vdem %>%
  filter(auto_period02ci==1) %>%
  group_by(auto_period_id02ci) %>%
  dplyr:: summarize(country_name = first (country_name), 
                    start_year = first(year), 
                    end_year = last(year), 
                    EDI_before = first(lag_v2x_polyarchy),
                    EDI_end = last(v2x_polyarchy), 
                    decline = first(auto_decline02ci))  %>%
  dplyr::mutate(country_year = str_c(country_name, start_year, sep = "_"))

anti_join(auto_periods_EDI015ci, auto_periods_EDI02ci, by = "country_year")

auto_countries_EDI02ci <- vdem %>%
  filter(auto_period02ci==1) %>%
  group_by(country_name) %>%
  dplyr::summarize(countries = first (country_name)) 

# 112 autocratization episodes in 75 countries

vdem_auto_EDI02ci <- vdem %>%
  select(country_name, year, auto_period_id02ci, auto_decline02ci, auto_period02ci, last_v2x_regime, lag_v2x_regime)


#############################################################################################
#### Own defintion according to Lührmann and Lindberg (2019): EDI decline 0.1 + CI, start_incl = 0.05 ####
#############################################################################################

vdem <- vdem_org
#### Autocratization Periods ####

vdem <- vdem %>%
  select(country_name, year, v2x_polyarchy, v2x_libdem, v2x_polyarchy_codelow, v2x_polyarchy_codehigh, v2x_regime) %>%
  filter(year >= 1900) %>%
  group_by(country_name) %>%
  mutate(EDI_1 = v2x_polyarchy - lag(v2x_polyarchy, 1), 
         start_auto = ifelse(EDI_1<=-0.05, 1, NA), 
         min_1 = lead(EDI_1, 1), 
         min_2 = lead(EDI_1, 2), 
         min_3 = lead(EDI_1, 3), 
         min_4 = lead(EDI_1, 4), 
         auto_end = ifelse(min_1<=-0.05 | min_2<=-0.05 | min_3<=-0.05 | min_4<=-0.05, 0, 1),
         auto_end2 = ifelse(EDI_1>=0.08, 1, 0)) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(start_auto_1 = ifelse(lag(start_auto)==1, 1, 0), 
         start_auto_2 = ifelse(lag(start_auto, 2)==1, 1, 0), 
         start_auto_3 = ifelse(lag(start_auto, 3)==1, 1, 0), 
         start_auto_4 = ifelse(lag(start_auto, 4)==1, 1, 0), 
         start_auto_four = coalesce(start_auto_1, start_auto_2, start_auto_3, start_auto_4), 
         start_auto_four = ifelse(is.na(start_auto_four), 0, start_auto_four))  %>%
  select(-c(start_auto_1, start_auto_2, start_auto_3, start_auto_4)) %>%
  select(-c(min_1, min_2, min_3, min_4)) %>%
  fill(auto_end)

vdem <- as.data.frame(vdem)
for (i in 1:nrow(vdem)) {
  vdem[i,][is.na(vdem[i,]) & vdem$auto_end[i-1] == 0 & vdem$start_auto_four[i] == 1 & vdem$start_auto[i-1] ==1 &
             vdem$auto_end2[i] != 1] <- 1 
}

table(vdem$start_auto)

## Autocratization Periods with a total magnitude of 0.1 (1% decrease)  ##
# nessearcy conditions: one year before autocratization started -  the decline at the end of the episode >=0.1 and
# no overlap of confidence intervals  according to Laebens and Lührmann 

# first step: create auto_period ID
vdem <- vdem %>%
  mutate(start_auto = ifelse(is.na(start_auto), 0, start_auto)) %>%
  group_by(country_name) %>%
  mutate(group_id = cumsum(start_auto != lag(start_auto, default = FALSE))) # group_id for each autocra_period

vdem <- vdem %>%
  mutate(auto_period_id01ci_start005 = str_c(country_name, group_id, sep = "_"))

# second step: create dummy in which autocratization was greater than 0.1

vdem <- vdem %>%
  group_by(auto_period_id01ci_start005) %>%
  mutate(last_v2x_polyarchy = last(v2x_polyarchy), 
         last_v2x_polyarchy_codelow = last(v2x_polyarchy_codelow), 
         last_v2x_polyarchy_codehigh = last(v2x_polyarchy_codehigh),
         last_v2x_regime = last(v2x_regime))

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(auto_confidence = lag(last_v2x_polyarchy_codelow) - last_v2x_polyarchy_codehigh)

vdem <- vdem %>%
  ungroup()%>%
  mutate(auto_dum01ci_start005 = last_v2x_polyarchy - lag(last_v2x_polyarchy)) %>%
  select(-c(group_id)) 

summary(vdem$auto_confidence) # whenever auto_confidence is positive, the confidence intervals do not overlag each other

vdem <- vdem %>%
  group_by(auto_period_id01ci_start005) %>%
  mutate(auto_period01ci_start005 = ifelse(start_auto==1 & first(auto_dum01ci_start005) <= -0.1 & first(auto_confidence) >0 , 1, 0 )) %>% 
  rename(auto_decline01ci_start005 = auto_dum01ci_start005) # no overlap of confidence intervals before and after autocratization 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_v2x_polyarchy = lag(v2x_polyarchy, 1), 
         lag_v2x_regime = lag(v2x_regime, 1))

# summary statistics #
table(vdem$auto_period01ci_start005)

auto_periods_EDI01ci_start005 <- vdem %>%
  filter(auto_period01ci_start005==1) %>%
  group_by(auto_period_id01ci_start005) %>%
  dplyr:: summarize(country_name = first (country_name), 
                    start_year = first(year), 
                    end_year = last(year), 
                    EDI_before = first(lag_v2x_polyarchy),
                    EDI_end = last(v2x_polyarchy), 
                    decline = first(auto_decline01ci_start005))  %>%
  dplyr::mutate(country_year = str_c(country_name, start_year, sep = "_"))

anti_join(auto_periods_EDI01ci, auto_periods_EDI01ci_start005, by = "country_year")

auto_countries_EDI01ci_start005 <- vdem %>%
  filter(auto_period01ci_start005==1) %>%
  group_by(country_name) %>%
  dplyr::summarize(countries = first (country_name)) 

# 179 autocratization episodes in 105 countries

vdem_auto_EDI01ci_start005 <- vdem %>%
  select(country_name, year, auto_period_id01ci_start005, auto_decline01ci_start005, auto_period01ci_start005, last_v2x_regime, lag_v2x_regime)

#############################################################################################
#### Own defintion according to Lührmann and Lindberg (2019): EDI decline 0.15 + CI, start_incl = 0.05 ####
#############################################################################################

vdem <- vdem_org
#### Autocratization Periods ####

vdem <- vdem %>%
  select(country_name, year, v2x_polyarchy, v2x_libdem, v2x_polyarchy_codelow, v2x_polyarchy_codehigh, v2x_regime) %>%
  filter(year >= 1900) %>%
  group_by(country_name) %>%
  mutate(EDI_1 = v2x_polyarchy - lag(v2x_polyarchy, 1), 
         start_auto = ifelse(EDI_1<=-0.05, 1, NA), 
         min_1 = lead(EDI_1, 1), 
         min_2 = lead(EDI_1, 2), 
         min_3 = lead(EDI_1, 3), 
         min_4 = lead(EDI_1, 4), 
         auto_end = ifelse(min_1<=-0.05 | min_2<=-0.05 | min_3<=-0.05 | min_4<=-0.05, 0, 1),
         auto_end2 = ifelse(EDI_1>=0.08, 1, 0)) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(start_auto_1 = ifelse(lag(start_auto)==1, 1, 0), 
         start_auto_2 = ifelse(lag(start_auto, 2)==1, 1, 0), 
         start_auto_3 = ifelse(lag(start_auto, 3)==1, 1, 0), 
         start_auto_4 = ifelse(lag(start_auto, 4)==1, 1, 0), 
         start_auto_four = coalesce(start_auto_1, start_auto_2, start_auto_3, start_auto_4), 
         start_auto_four = ifelse(is.na(start_auto_four), 0, start_auto_four))  %>%
  select(-c(start_auto_1, start_auto_2, start_auto_3, start_auto_4)) %>%
  select(-c(min_1, min_2, min_3, min_4)) %>%
  fill(auto_end)

vdem <- as.data.frame(vdem)
for (i in 1:nrow(vdem)) {
  vdem[i,][is.na(vdem[i,]) & vdem$auto_end[i-1] == 0 & vdem$start_auto_four[i] == 1 & vdem$start_auto[i-1] ==1 &
             vdem$auto_end2[i] != 1] <- 1 
}

table(vdem$start_auto)

## Autocratization Periods with a total magnitude of 0.15 (15% decrease)  ##
# nessearcy conditions: one year before autocratization started -  the decline at the end of the episode >=0.15 and
# no overlap of confidence intervals according to Laebens and Lührmann 

# first step: create auto_period ID
vdem <- vdem %>%
  mutate(start_auto = ifelse(is.na(start_auto), 0, start_auto)) %>%
  group_by(country_name) %>%
  mutate(group_id = cumsum(start_auto != lag(start_auto, default = FALSE))) # group_id for each autocra_period

vdem <- vdem %>%
  mutate(auto_period_id015ci_start005 = str_c(country_name, group_id, sep = "_"))

# second step: create dummy in which autocratization was greater than 0.15

vdem <- vdem %>%
  group_by(auto_period_id015ci_start005) %>%
  mutate(last_v2x_polyarchy = last(v2x_polyarchy), 
         last_v2x_polyarchy_codelow = last(v2x_polyarchy_codelow), 
         last_v2x_polyarchy_codehigh = last(v2x_polyarchy_codehigh),
         last_v2x_regime = last(v2x_regime))

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(auto_confidence = lag(last_v2x_polyarchy_codelow) - last_v2x_polyarchy_codehigh)

vdem <- vdem %>%
  ungroup()%>%
  mutate(auto_dum015ci_start005 = last_v2x_polyarchy - lag(last_v2x_polyarchy)) %>%
  select(-c(group_id)) 

summary(vdem$auto_confidence) # whenever auto_confidence is positive, the confidence intervals do not overlag each other

vdem <- vdem %>%
  group_by(auto_period_id015ci_start005) %>%
  mutate(auto_period015ci_start005 = ifelse(start_auto==1 & first(auto_dum015ci_start005) <= -0.15 & first(auto_confidence) >0 , 1, 0 )) %>% 
  rename(auto_decline015ci_start005 = auto_dum015ci_start005) # no overlap of confidence intervals before and after autocratization 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_v2x_polyarchy = lag(v2x_polyarchy, 1), 
         lag_v2x_regime = lag(v2x_regime, 1))

# summary statistics #
table(vdem$auto_period015ci_start005)

auto_periods_EDI015ci_start005 <- vdem %>%
  filter(auto_period015ci_start005==1) %>%
  group_by(auto_period_id015ci_start005) %>%
  dplyr:: summarize(country_name = first (country_name), 
                    start_year = first(year), 
                    end_year = last(year), 
                    EDI_before = first(lag_v2x_polyarchy),
                    EDI_end = last(v2x_polyarchy), 
                    decline = first(auto_decline015ci_start005))  %>%
  dplyr::mutate(country_year = str_c(country_name, start_year, sep = "_"))

anti_join(auto_periods_EDI015ci, auto_periods_EDI015ci_start005, by = "country_year")

auto_countries_EDI015ci_start005 <- vdem %>%
  filter(auto_period015ci_start005==1) %>%
  group_by(country_name) %>%
  dplyr::summarize(countries = first (country_name)) 

# 121 autocratization episodes in 80 countries

vdem_auto_EDI015ci_start005 <- vdem %>%
  select(country_name, year, auto_period_id015ci_start005, auto_decline015ci_start005, auto_period015ci_start005, last_v2x_regime, lag_v2x_regime)


#############################################################################################
#### Own defintion according to Lührmann and Lindberg (2019): EDI decline 0.2 + CI, start_incl = 0.05 ####
#############################################################################################

vdem <- vdem_org
#### Autocratization Periods ####

vdem <- vdem %>%
  select(country_name, year, v2x_polyarchy, v2x_libdem, v2x_polyarchy_codelow, v2x_polyarchy_codehigh, v2x_regime) %>%
  filter(year >= 1900) %>%
  group_by(country_name) %>%
  mutate(EDI_1 = v2x_polyarchy - lag(v2x_polyarchy, 1), 
         start_auto = ifelse(EDI_1<=-0.05, 1, NA), 
         min_1 = lead(EDI_1, 1), 
         min_2 = lead(EDI_1, 2), 
         min_3 = lead(EDI_1, 3), 
         min_4 = lead(EDI_1, 4), 
         auto_end = ifelse(min_1<=-0.05 | min_2<=-0.05 | min_3<=-0.05 | min_4<=-0.05, 0, 1),
         auto_end2 = ifelse(EDI_1>=0.08, 1, 0)) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(start_auto_1 = ifelse(lag(start_auto)==1, 1, 0), 
         start_auto_2 = ifelse(lag(start_auto, 2)==1, 1, 0), 
         start_auto_3 = ifelse(lag(start_auto, 3)==1, 1, 0), 
         start_auto_4 = ifelse(lag(start_auto, 4)==1, 1, 0), 
         start_auto_four = coalesce(start_auto_1, start_auto_2, start_auto_3, start_auto_4), 
         start_auto_four = ifelse(is.na(start_auto_four), 0, start_auto_four))  %>%
  select(-c(start_auto_1, start_auto_2, start_auto_3, start_auto_4)) %>%
  select(-c(min_1, min_2, min_3, min_4)) %>%
  fill(auto_end)

vdem <- as.data.frame(vdem)
for (i in 1:nrow(vdem)) {
  vdem[i,][is.na(vdem[i,]) & vdem$auto_end[i-1] == 0 & vdem$start_auto_four[i] == 1 & vdem$start_auto[i-1] ==1 &
             vdem$auto_end2[i] != 1] <- 1 
}

table(vdem$start_auto)

## Autocratization Periods with a total magnitude of 0.2 (20% decrease)  ##
# nessearcy conditions: one year before autocratization started -  the decline at the end of the episode >=0.2 and
# no overlap of confidence intervals according to Laebens and Lührmann 

# first step: create auto_period ID
vdem <- vdem %>%
  mutate(start_auto = ifelse(is.na(start_auto), 0, start_auto)) %>%
  group_by(country_name) %>%
  mutate(group_id = cumsum(start_auto != lag(start_auto, default = FALSE))) # group_id for each autocra_period

vdem <- vdem %>%
  mutate(auto_period_id02ci_start005 = str_c(country_name, group_id, sep = "_"))

# second step: create dummy in which autocratization was greater than 0.2

vdem <- vdem %>%
  group_by(auto_period_id02ci_start005) %>%
  mutate(last_v2x_polyarchy = last(v2x_polyarchy), 
         last_v2x_polyarchy_codelow = last(v2x_polyarchy_codelow), 
         last_v2x_polyarchy_codehigh = last(v2x_polyarchy_codehigh),
         last_v2x_regime = last(v2x_regime))

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(auto_confidence = lag(last_v2x_polyarchy_codelow) - last_v2x_polyarchy_codehigh)

vdem <- vdem %>%
  ungroup()%>%
  mutate(auto_dum02ci_start005 = last_v2x_polyarchy - lag(last_v2x_polyarchy)) %>%
  select(-c(group_id)) 

summary(vdem$auto_confidence) # whenever auto_confidence is negative, the confidence intervals do not overlap each other

vdem <- vdem %>%
  group_by(auto_period_id02ci_start005) %>%
  mutate(auto_period02ci_start005 = ifelse(start_auto==1 & first(auto_dum02ci_start005) <= -0.2 & first(auto_confidence) >0 , 1, 0 )) %>% 
  rename(auto_decline02ci_start005 = auto_dum02ci_start005) # no overlap of confidence intervals before and after autocratization 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_v2x_polyarchy = lag(v2x_polyarchy, 1), 
         lag_v2x_regime = lag(v2x_regime, 1))

# summary statistics #
table(vdem$auto_period02ci_start005)

auto_periods_EDI02ci_start005 <- vdem %>%
  filter(auto_period02ci_start005==1) %>%
  group_by(auto_period_id02ci_start005) %>%
  dplyr:: summarize(country_name = first (country_name), 
                    start_year = first(year), 
                    end_year = last(year), 
                    EDI_before = first(lag_v2x_polyarchy),
                    EDI_end = last(v2x_polyarchy), 
                    decline = first(auto_decline02ci_start005))  %>%
  dplyr::mutate(country_year = str_c(country_name, start_year, sep = "_"))

anti_join(auto_periods_EDI02ci, auto_periods_EDI02ci_start005, by = "country_year")

auto_countries_EDI02ci_start005 <- vdem %>%
  filter(auto_period02ci_start005==1) %>%
  group_by(country_name) %>%
  dplyr::summarize(countries = first (country_name)) 

# 85 autocratization episodes in 61 countries

vdem_auto_EDI02ci_start005 <- vdem %>%
  select(country_name, year, auto_period_id02ci_start005, auto_decline02ci_start005, auto_period02ci_start005, last_v2x_regime, lag_v2x_regime)


#############################################################################################
#### Definition Lührmann and Lindberg adapted  (2019): LDI decline 0.1 ####
#############################################################################################

vdem <- vdem_org

#### Autocratization Periods ####

vdem <- vdem %>%
  select(country_name, year, v2x_polyarchy, v2x_libdem, v2x_regime) %>%
  filter(year >= 1900) %>%
  group_by(country_name) %>%
  mutate(LDI_1 = v2x_libdem - lag(v2x_libdem, 1), 
         start_auto = ifelse(LDI_1<=-0.01, 1, NA), 
         min_1 = lead(LDI_1, 1), 
         min_2 = lead(LDI_1, 2), 
         min_3 = lead(LDI_1, 3), 
         min_4 = lead(LDI_1, 4), 
         auto_end = ifelse(min_1<=-0.01 | min_2<=-0.01 | min_3<=-0.01 | min_4<=-0.01, 0, 1),
         auto_end2 = ifelse(LDI_1>=0.02, 1, 0)) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(start_auto_1 = ifelse(lag(start_auto)==1, 1, 0), 
         start_auto_2 = ifelse(lag(start_auto, 2)==1, 1, 0), 
         start_auto_3 = ifelse(lag(start_auto, 3)==1, 1, 0), 
         start_auto_4 = ifelse(lag(start_auto, 4)==1, 1, 0), 
         start_auto_four = coalesce(start_auto_1, start_auto_2, start_auto_3, start_auto_4), 
         start_auto_four = ifelse(is.na(start_auto_four), 0, start_auto_four))  %>%
  select(-c(start_auto_1, start_auto_2, start_auto_3, start_auto_4)) %>%
  select(-c(min_1, min_2, min_3, min_4)) %>%
  fill(auto_end)

vdem <- as.data.frame(vdem)
for (i in 1:nrow(vdem)) {
  vdem[i,][is.na(vdem[i,]) & vdem$auto_end[i-1] == 0 & vdem$start_auto_four[i] == 1 & vdem$start_auto[i-1] ==1 &
             vdem$auto_end2[i] != 1] <- 1 
}

table(vdem$start_auto)

## Autocratization Periods with a total magnitude of 0.1 (10% decrease)  ##
# nessearcy conditions: one year before autocratization started - the decline at the end of the episode >=0.1

# first step: create auto_period ID
vdem <- vdem %>%
  mutate(start_auto = ifelse(is.na(start_auto), 0, start_auto)) %>%
  group_by(country_name) %>%
  mutate(group_id = cumsum(start_auto != lag(start_auto, default = FALSE))) # group_id for each autocra_period

vdem <- vdem %>%
  mutate(auto_period_id_libdem = str_c(country_name, group_id, sep = "_"))

# second step: create dummy in which autocratization was greater than 0.1

vdem <- vdem %>%
  group_by(auto_period_id_libdem) %>%
  mutate(last_v2x_libdem = last(v2x_libdem), 
         last_v2x_regime = last(v2x_regime))

vdem <- vdem %>%
  ungroup()%>%
  mutate(auto_dum01_libdem = last_v2x_libdem - lag(last_v2x_libdem)) %>%
  select(-c(group_id, last_v2x_libdem)) 

vdem <- vdem %>%
  group_by(auto_period_id_libdem) %>%
  mutate(auto_period01_libdem = ifelse(start_auto==1 & first(auto_dum01_libdem) <= -0.1, 1, 0 )) %>%
  rename(auto_decline_libdem = auto_dum01_libdem)

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_v2x_polyarchy = lag(v2x_polyarchy, 1), 
         lag_v2x_regime = lag(v2x_regime, 1))

# summary statistics #
table(vdem$auto_period01_libdem)

auto_periods_LDI01 <- vdem %>%
  filter(auto_period01_libdem==1) %>%
  group_by(auto_period_id_libdem) %>%
  dplyr::summarize(country_name = first (country_name), 
            start_year = first(year), 
            end_year = last(year), 
            EDI_before = first(lag_v2x_polyarchy),
            EDI_end = last(v2x_polyarchy), 
            decline = first(auto_decline_libdem)) %>%
  dplyr::mutate(country_year = str_c(country_name, start_year, sep = "_"))

auto_countries_LDI01 <- vdem %>%
  filter(auto_period01_libdem==1) %>%
  group_by(country_name) %>%
  dplyr::summarize(countries = first (country_name)) 

# 165 autocratization episodes in 100 countries

vdem_auto_LDI01 <- vdem %>%
  select(country_name, year, auto_period_id_libdem, auto_decline_libdem, auto_period01_libdem, last_v2x_regime, lag_v2x_regime)

#############################################################################################
#### Definition Laebens and Lührmann adopted  (2020): LDI decline 0.05 ####
#############################################################################################

vdem <- vdem_org

#### Autocratization Periods ####

vdem <- vdem %>%
  select(country_name, year, v2x_polyarchy, v2x_libdem, v2x_libdem_codelow, v2x_libdem_codehigh, v2x_regime) %>%
  filter(year >= 1900) %>%
  group_by(country_name) %>%
  mutate(LDI_1 = v2x_libdem - lag(v2x_libdem, 1), 
         start_auto = ifelse(LDI_1<=-0.01, 1, NA), 
         min_1 = lead(LDI_1, 1), 
         min_2 = lead(LDI_1, 2), 
         min_3 = lead(LDI_1, 3), 
         min_4 = lead(LDI_1, 4), 
         auto_end = ifelse(min_1<=-0.01 | min_2<=-0.01 | min_3<=-0.01 | min_4<=-0.01, 0, 1),
         auto_end2 = ifelse(LDI_1>=0.02, 1, 0)) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(start_auto_1 = ifelse(lag(start_auto)==1, 1, 0), 
         start_auto_2 = ifelse(lag(start_auto, 2)==1, 1, 0), 
         start_auto_3 = ifelse(lag(start_auto, 3)==1, 1, 0), 
         start_auto_4 = ifelse(lag(start_auto, 4)==1, 1, 0), 
         start_auto_four = coalesce(start_auto_1, start_auto_2, start_auto_3, start_auto_4), 
         start_auto_four = ifelse(is.na(start_auto_four), 0, start_auto_four))  %>%
  select(-c(start_auto_1, start_auto_2, start_auto_3, start_auto_4)) %>%
  select(-c(min_1, min_2, min_3, min_4)) %>%
  fill(auto_end)

vdem <- as.data.frame(vdem)
for (i in 1:nrow(vdem)) {
  vdem[i,][is.na(vdem[i,]) & vdem$auto_end[i-1] == 0 & vdem$start_auto_four[i] == 1 & vdem$start_auto[i-1] ==1 &
             vdem$auto_end2[i] != 1] <- 1 
}

table(vdem$start_auto)

## Autocratization Periods with a total magnitude of 0.05 (5% decrease) in LDI  ##
# nessearcy conditions: one year before autocratization started -  the decline at the end of the episode >=0.05 and
# no overlap of confidence intervals  according to Laebens and Lührmann 

# first step: create auto_period ID
vdem <- vdem %>%
  mutate(start_auto = ifelse(is.na(start_auto), 0, start_auto)) %>%
  group_by(country_name) %>%
  mutate(group_id = cumsum(start_auto != lag(start_auto, default = FALSE))) # group_id for each autocra_period

vdem <- vdem %>%
  mutate(auto_period_id005_libdem = str_c(country_name, group_id, sep = "_"))

# second step: create dummy in which autocratization was greater than 0.1

vdem <- vdem %>%
  group_by(auto_period_id005_libdem) %>%
  mutate(last_v2x_libdem = last(v2x_libdem), 
         last_v2x_libdem_codelow = last(v2x_libdem_codelow), 
         last_v2x_libdem_codehigh = last(v2x_libdem_codehigh), 
         last_v2x_regime = last(v2x_regime))

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(auto_confidence = lag(last_v2x_libdem_codelow) - last_v2x_libdem_codehigh)


vdem <- vdem %>%
  ungroup()%>%
  mutate(auto_dum005_libdem = last_v2x_libdem - lag(last_v2x_libdem)) %>%
  select(-c(group_id)) 

summary(vdem$auto_confidence) # whenever auto_confidence is positive, the confidence intervals do not overlag each other

vdem <- vdem %>%
  group_by(auto_period_id005_libdem) %>%
  mutate(auto_period005_libdem = ifelse(start_auto==1 & first(auto_dum005_libdem) <= -0.05 & first(auto_confidence) >0 , 1, 0 )) %>% 
  rename(auto_decline005_libdem = auto_dum005_libdem) # no overlap of confidence intervals before and after autocratization 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_v2x_polyarchy = lag(v2x_polyarchy, 1), 
         lag_v2x_regime = lag(v2x_regime, 1))


# summary statistics #
table(vdem$auto_period005_libdem)

auto_periods_LDI005 <- vdem %>%
  filter(auto_period005_libdem==1) %>%
  group_by(auto_period_id005_libdem) %>%
  dplyr::summarize(country_name = first (country_name), 
            start_year = first(year), 
            end_year = last(year), 
            EDI_before = first(lag_v2x_polyarchy),
            EDI_end = last(v2x_polyarchy), 
            decline = first(auto_decline005_libdem)) %>%
  dplyr::mutate(country_year = str_c(country_name, start_year, sep = "_"))


auto_countries_LDI005 <- vdem %>%
  filter(auto_period005_libdem==1) %>%
  group_by(country_name) %>%
  dplyr::summarize(countries = first (country_name)) 

# 265 autocratization episodes in 131 countries

vdem_auto_LDI005 <- vdem %>%
  select(country_name, year, auto_period_id005_libdem, auto_decline005_libdem, auto_period005_libdem, 
         last_v2x_regime, lag_v2x_regime)


#############################################################################################
#### Definition Lührmann and Lindberg adopted  (2019): LDI decline 0.1 + CI ####
#############################################################################################

vdem <- vdem_org

#### Autocratization Periods ####

vdem <- vdem %>%
  select(country_name, year, v2x_polyarchy, v2x_libdem, v2x_libdem_codelow, v2x_libdem_codehigh, v2x_regime) %>%
  filter(year >= 1900) %>%
  group_by(country_name) %>%
  mutate(LDI_1 = v2x_libdem - lag(v2x_libdem, 1), 
         start_auto = ifelse(LDI_1<=-0.01, 1, NA), 
         min_1 = lead(LDI_1, 1), 
         min_2 = lead(LDI_1, 2), 
         min_3 = lead(LDI_1, 3), 
         min_4 = lead(LDI_1, 4), 
         auto_end = ifelse(min_1<=-0.01 | min_2<=-0.01 | min_3<=-0.01 | min_4<=-0.01, 0, 1),
         auto_end2 = ifelse(LDI_1>=0.02, 1, 0)) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(start_auto_1 = ifelse(lag(start_auto)==1, 1, 0), 
         start_auto_2 = ifelse(lag(start_auto, 2)==1, 1, 0), 
         start_auto_3 = ifelse(lag(start_auto, 3)==1, 1, 0), 
         start_auto_4 = ifelse(lag(start_auto, 4)==1, 1, 0), 
         start_auto_four = coalesce(start_auto_1, start_auto_2, start_auto_3, start_auto_4), 
         start_auto_four = ifelse(is.na(start_auto_four), 0, start_auto_four))  %>%
  select(-c(start_auto_1, start_auto_2, start_auto_3, start_auto_4)) %>%
  select(-c(min_1, min_2, min_3, min_4)) %>%
  fill(auto_end)

vdem <- as.data.frame(vdem)
for (i in 1:nrow(vdem)) {
  vdem[i,][is.na(vdem[i,]) & vdem$auto_end[i-1] == 0 & vdem$start_auto_four[i] == 1 & vdem$start_auto[i-1] ==1 &
             vdem$auto_end2[i] != 1] <- 1 
}

table(vdem$start_auto)

## Autocratization Periods with a total magnitude of 0.1 (10% decrease) in LDI  ##
# nessearcy conditions: one year before autocratization started -  the decline at the end of the episode >=0.1 and
# no overlap of confidence intervals  according to Laebens and Lührmann 

# first step: create auto_period ID
vdem <- vdem %>%
  mutate(start_auto = ifelse(is.na(start_auto), 0, start_auto)) %>%
  group_by(country_name) %>%
  mutate(group_id = cumsum(start_auto != lag(start_auto, default = FALSE))) # group_id for each autocra_period

vdem <- vdem %>%
  mutate(auto_period_id01_libdem_ci = str_c(country_name, group_id, sep = "_"))

# second step: create dummy in which autocratization was greater than 0.1

vdem <- vdem %>%
  group_by(auto_period_id01_libdem_ci) %>%
  mutate(last_v2x_libdem = last(v2x_libdem), 
         last_v2x_libdem_codelow = last(v2x_libdem_codelow), 
         last_v2x_libdem_codehigh = last(v2x_libdem_codehigh), 
         last_v2x_regime = last(v2x_regime))

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(auto_confidence = lag(last_v2x_libdem_codelow) - last_v2x_libdem_codehigh)


vdem <- vdem %>%
  ungroup()%>%
  mutate(auto_dum01_libdem_ci = last_v2x_libdem - lag(last_v2x_libdem)) %>%
  select(-c(group_id)) 

summary(vdem$auto_confidence) # whenever auto_confidence is positive, the confidence intervals do not overlag each other

vdem <- vdem %>%
  group_by(auto_period_id01_libdem_ci) %>%
  mutate(auto_period01_libdem_ci = ifelse(start_auto==1 & first(auto_dum01_libdem_ci) <= -0.1 & first(auto_confidence) >0 , 1, 0 )) %>% 
  rename(auto_decline01_libdem_ci = auto_dum01_libdem_ci) # no overlap of confidence intervals before and after autocratization 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_v2x_polyarchy = lag(v2x_polyarchy, 1),
         lag_v2x_regime = lag(v2x_regime, 1))

# summary statistics #
table(vdem$auto_period01_libdem_ci)

auto_periods_LDI01ci <- vdem %>%
  filter(auto_period01_libdem_ci ==1) %>%
  group_by(auto_period_id01_libdem_ci) %>%
  dplyr::summarize(country_name = first (country_name), 
            start_year = first(year), 
            end_year = last(year), 
            EDI_before = first(lag_v2x_polyarchy),
            EDI_end = last(v2x_polyarchy), 
            decline = first(auto_decline01_libdem_ci)) %>%
  dplyr::mutate(country_year = str_c(country_name, start_year, sep = "_"))

anti_join(auto_periods_LDI01, auto_periods_LDI01ci, by = "country_year")

auto_countries_LDI01ci <- vdem %>%
  filter(auto_period01_libdem_ci==1) %>%
  group_by(country_name) %>%
  dplyr::summarize(countries = first (country_name)) 

# 162 autocratization episodes in 98 countries

vdem_auto_LDI01ci <- vdem %>%
  select(country_name, year, auto_period01_libdem_ci, auto_decline01_libdem_ci, auto_period_id01_libdem_ci, 
         last_v2x_regime, lag_v2x_regime)

#############################################################################################
#### Definition Lührmann and Lindberg adopted  (2019): LDI decline 0.15 + CI ####
#############################################################################################

vdem <- vdem_org

#### Autocratization Periods ####

vdem <- vdem %>%
  select(country_name, year, v2x_polyarchy, v2x_libdem, v2x_libdem_codelow, v2x_libdem_codehigh, v2x_regime) %>%
  filter(year >= 1900) %>%
  group_by(country_name) %>%
  mutate(LDI_1 = v2x_libdem - lag(v2x_libdem, 1), 
         start_auto = ifelse(LDI_1<=-0.01, 1, NA), 
         min_1 = lead(LDI_1, 1), 
         min_2 = lead(LDI_1, 2), 
         min_3 = lead(LDI_1, 3), 
         min_4 = lead(LDI_1, 4), 
         auto_end = ifelse(min_1<=-0.01 | min_2<=-0.01 | min_3<=-0.01 | min_4<=-0.01, 0, 1),
         auto_end2 = ifelse(LDI_1>=0.02, 1, 0)) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(start_auto_1 = ifelse(lag(start_auto)==1, 1, 0), 
         start_auto_2 = ifelse(lag(start_auto, 2)==1, 1, 0), 
         start_auto_3 = ifelse(lag(start_auto, 3)==1, 1, 0), 
         start_auto_4 = ifelse(lag(start_auto, 4)==1, 1, 0), 
         start_auto_four = coalesce(start_auto_1, start_auto_2, start_auto_3, start_auto_4), 
         start_auto_four = ifelse(is.na(start_auto_four), 0, start_auto_four))  %>%
  select(-c(start_auto_1, start_auto_2, start_auto_3, start_auto_4)) %>%
  select(-c(min_1, min_2, min_3, min_4)) %>%
  fill(auto_end)

vdem <- as.data.frame(vdem)
for (i in 1:nrow(vdem)) {
  vdem[i,][is.na(vdem[i,]) & vdem$auto_end[i-1] == 0 & vdem$start_auto_four[i] == 1 & vdem$start_auto[i-1] ==1 &
             vdem$auto_end2[i] != 1] <- 1 
}

table(vdem$start_auto)

## Autocratization Periods with a total magnitude of 0.15 (15% decrease) in LDI  ##
# nessearcy conditions: one year before autocratization started -  the decline at the end of the episode >=0.15 and
# no overlap of confidence intervals  according to Laebens and Lührmann 

# first step: create auto_period ID
vdem <- vdem %>%
  mutate(start_auto = ifelse(is.na(start_auto), 0, start_auto)) %>%
  group_by(country_name) %>%
  mutate(group_id = cumsum(start_auto != lag(start_auto, default = FALSE))) # group_id for each autocra_period

vdem <- vdem %>%
  mutate(auto_period_id015_libdem_ci = str_c(country_name, group_id, sep = "_"))

# second step: create dummy in which autocratization was greater than 0.15

vdem <- vdem %>%
  group_by(auto_period_id015_libdem_ci) %>%
  mutate(last_v2x_libdem = last(v2x_libdem), 
         last_v2x_libdem_codelow = last(v2x_libdem_codelow), 
         last_v2x_libdem_codehigh = last(v2x_libdem_codehigh), 
         last_v2x_regime = last(v2x_regime))

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(auto_confidence = lag(last_v2x_libdem_codelow) - last_v2x_libdem_codehigh)


vdem <- vdem %>%
  ungroup()%>%
  mutate(auto_dum015_libdem_ci = last_v2x_libdem - lag(last_v2x_libdem)) %>%
  select(-c(group_id)) 

summary(vdem$auto_confidence) # whenever auto_confidence is positive, the confidence intervals do not overlag each other

vdem <- vdem %>%
  group_by(auto_period_id015_libdem_ci) %>%
  mutate(auto_period015_libdem_ci = ifelse(start_auto==1 & first(auto_dum015_libdem_ci) <= -0.15 & first(auto_confidence) >0 , 1, 0 )) %>% 
  rename(auto_decline015_libdem_ci = auto_dum015_libdem_ci) # no overlap of confidence intervals before and after autocratization 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_v2x_polyarchy = lag(v2x_polyarchy, 1),
         lag_v2x_regime = lag(v2x_regime, 1))

# summary statistics #
table(vdem$auto_period015_libdem_ci)

auto_periods_LDI015ci <- vdem %>%
  filter(auto_period015_libdem_ci ==1) %>%
  group_by(auto_period_id015_libdem_ci) %>%
  dplyr::summarize(country_name = first (country_name), 
                   start_year = first(year), 
                   end_year = last(year), 
                   EDI_before = first(lag_v2x_polyarchy),
                   EDI_end = last(v2x_polyarchy), 
                   decline = first(auto_decline015_libdem_ci)) %>%
  dplyr::mutate(country_year = str_c(country_name, start_year, sep = "_"))

anti_join(auto_periods_LDI01, auto_periods_LDI015ci, by = "country_year")

auto_countries_LDI015ci <- vdem %>%
  filter(auto_period015_libdem_ci==1) %>%
  group_by(country_name) %>%
  dplyr::summarize(countries = first (country_name)) 

# 102 autocratization episodes in 71 countries

vdem_auto_LDI015ci <- vdem %>%
  select(country_name, year, auto_period015_libdem_ci, auto_decline015_libdem_ci, auto_period_id015_libdem_ci, 
         last_v2x_regime, lag_v2x_regime)


#############################################################################################
#### Definition Lührmann and Lindberg adopted  (2019): LDI decline 0.2 + CI ####
#############################################################################################

vdem <- vdem_org

#### Autocratization Periods ####

vdem <- vdem %>%
  select(country_name, year, v2x_polyarchy, v2x_libdem, v2x_libdem_codelow, v2x_libdem_codehigh, v2x_regime) %>%
  filter(year >= 1900) %>%
  group_by(country_name) %>%
  mutate(LDI_1 = v2x_libdem - lag(v2x_libdem, 1), 
         start_auto = ifelse(LDI_1<=-0.01, 1, NA), 
         min_1 = lead(LDI_1, 1), 
         min_2 = lead(LDI_1, 2), 
         min_3 = lead(LDI_1, 3), 
         min_4 = lead(LDI_1, 4), 
         auto_end = ifelse(min_1<=-0.01 | min_2<=-0.01 | min_3<=-0.01 | min_4<=-0.01, 0, 1),
         auto_end2 = ifelse(LDI_1>=0.02, 1, 0)) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(start_auto_1 = ifelse(lag(start_auto)==1, 1, 0), 
         start_auto_2 = ifelse(lag(start_auto, 2)==1, 1, 0), 
         start_auto_3 = ifelse(lag(start_auto, 3)==1, 1, 0), 
         start_auto_4 = ifelse(lag(start_auto, 4)==1, 1, 0), 
         start_auto_four = coalesce(start_auto_1, start_auto_2, start_auto_3, start_auto_4), 
         start_auto_four = ifelse(is.na(start_auto_four), 0, start_auto_four))  %>%
  select(-c(start_auto_1, start_auto_2, start_auto_3, start_auto_4)) %>%
  select(-c(min_1, min_2, min_3, min_4)) %>%
  fill(auto_end)

vdem <- as.data.frame(vdem)
for (i in 1:nrow(vdem)) {
  vdem[i,][is.na(vdem[i,]) & vdem$auto_end[i-1] == 0 & vdem$start_auto_four[i] == 1 & vdem$start_auto[i-1] ==1 &
             vdem$auto_end2[i] != 1] <- 1 
}

table(vdem$start_auto)

## Autocratization Periods with a total magnitude of 0.2 (20% decrease) in LDI  ##
# nessearcy conditions: one year before autocratization started -  the decline at the end of the episode >=0.2 and
# no overlap of confidence intervals  according to Laebens and Lührmann 

# first step: create auto_period ID
vdem <- vdem %>%
  mutate(start_auto = ifelse(is.na(start_auto), 0, start_auto)) %>%
  group_by(country_name) %>%
  mutate(group_id = cumsum(start_auto != lag(start_auto, default = FALSE))) # group_id for each autocra_period

vdem <- vdem %>%
  mutate(auto_period_id02_libdem_ci = str_c(country_name, group_id, sep = "_"))

# second step: create dummy in which autocratization was greater than 0.15

vdem <- vdem %>%
  group_by(auto_period_id02_libdem_ci) %>%
  mutate(last_v2x_libdem = last(v2x_libdem), 
         last_v2x_libdem_codelow = last(v2x_libdem_codelow), 
         last_v2x_libdem_codehigh = last(v2x_libdem_codehigh), 
         last_v2x_regime = last(v2x_regime))

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(auto_confidence = lag(last_v2x_libdem_codelow) - last_v2x_libdem_codehigh)


vdem <- vdem %>%
  ungroup()%>%
  mutate(auto_dum02_libdem_ci = last_v2x_libdem - lag(last_v2x_libdem)) %>%
  select(-c(group_id)) 

summary(vdem$auto_confidence) # whenever auto_confidence is positive, the confidence intervals do not overlag each other

vdem <- vdem %>%
  group_by(auto_period_id02_libdem_ci) %>%
  mutate(auto_period02_libdem_ci = ifelse(start_auto==1 & first(auto_dum02_libdem_ci) <= -0.2 & first(auto_confidence) >0 , 1, 0 )) %>% 
  rename(auto_decline02_libdem_ci = auto_dum02_libdem_ci) # no overlap of confidence intervals before and after autocratization 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_v2x_polyarchy = lag(v2x_polyarchy, 1),
         lag_v2x_regime = lag(v2x_regime, 1))

# summary statistics #
table(vdem$auto_period02_libdem_ci)

auto_periods_LDI02ci <- vdem %>%
  filter(auto_period02_libdem_ci ==1) %>%
  group_by(auto_period_id02_libdem_ci) %>%
  dplyr::summarize(country_name = first (country_name), 
                   start_year = first(year), 
                   end_year = last(year), 
                   EDI_before = first(lag_v2x_polyarchy),
                   EDI_end = last(v2x_polyarchy), 
                   decline = first(auto_decline02_libdem_ci)) %>%
  dplyr::mutate(country_year = str_c(country_name, start_year, sep = "_"))

anti_join(auto_periods_LDI01, auto_periods_LDI02ci, by = "country_year")

auto_countries_LDI02ci <- vdem %>%
  filter(auto_period02_libdem_ci==1) %>%
  group_by(country_name) %>%
  dplyr::summarize(countries = first (country_name)) 

# 73 autocratization episodes in 55 countries

vdem_auto_LDI02ci <- vdem %>%
  select(country_name, year, auto_period02_libdem_ci, auto_decline02_libdem_ci, auto_period_id02_libdem_ci, 
         last_v2x_regime, lag_v2x_regime)

#############################################################################################
#### Definition Lührmann and Lindberg adopted  (2019): LDI decline 0.1 + CI, start_incl = 0.05 ####
#############################################################################################

vdem <- vdem_org

#### Autocratization Periods ####

vdem <- vdem %>%
  select(country_name, year, v2x_polyarchy, v2x_libdem, v2x_libdem_codelow, v2x_libdem_codehigh, v2x_regime) %>%
  filter(year >= 1900) %>%
  group_by(country_name) %>%
  mutate(LDI_1 = v2x_libdem - lag(v2x_libdem, 1), 
         start_auto = ifelse(LDI_1<=-0.05, 1, NA), 
         min_1 = lead(LDI_1, 1), 
         min_2 = lead(LDI_1, 2), 
         min_3 = lead(LDI_1, 3), 
         min_4 = lead(LDI_1, 4), 
         auto_end = ifelse(min_1<=-0.05 | min_2<=-0.05 | min_3<=-0.05 | min_4<=-0.05, 0, 1),
         auto_end2 = ifelse(LDI_1>=0.08, 1, 0)) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(start_auto_1 = ifelse(lag(start_auto)==1, 1, 0), 
         start_auto_2 = ifelse(lag(start_auto, 2)==1, 1, 0), 
         start_auto_3 = ifelse(lag(start_auto, 3)==1, 1, 0), 
         start_auto_4 = ifelse(lag(start_auto, 4)==1, 1, 0), 
         start_auto_four = coalesce(start_auto_1, start_auto_2, start_auto_3, start_auto_4), 
         start_auto_four = ifelse(is.na(start_auto_four), 0, start_auto_four))  %>%
  select(-c(start_auto_1, start_auto_2, start_auto_3, start_auto_4)) %>%
  select(-c(min_1, min_2, min_3, min_4)) %>%
  fill(auto_end)

vdem <- as.data.frame(vdem)
for (i in 1:nrow(vdem)) {
  vdem[i,][is.na(vdem[i,]) & vdem$auto_end[i-1] == 0 & vdem$start_auto_four[i] == 1 & vdem$start_auto[i-1] ==1 &
             vdem$auto_end2[i] != 1] <- 1 
}

table(vdem$start_auto)

## Autocratization Periods with a total magnitude of 0.1 (10% decrease) in LDI  ##
# nessearcy conditions: one year before autocratization started -  the decline at the end of the episode >=0.1 and
# no overlap of confidence intervals  according to Laebens and Lührmann 

# first step: create auto_period ID
vdem <- vdem %>%
  mutate(start_auto = ifelse(is.na(start_auto), 0, start_auto)) %>%
  group_by(country_name) %>%
  mutate(group_id = cumsum(start_auto != lag(start_auto, default = FALSE))) # group_id for each autocra_period

vdem <- vdem %>%
  mutate(auto_period_id01_libdem_ci_start005 = str_c(country_name, group_id, sep = "_"))

# second step: create dummy in which autocratization was greater than 0.1

vdem <- vdem %>%
  group_by(auto_period_id01_libdem_ci_start005) %>%
  mutate(last_v2x_libdem = last(v2x_libdem), 
         last_v2x_libdem_codelow = last(v2x_libdem_codelow), 
         last_v2x_libdem_codehigh = last(v2x_libdem_codehigh), 
         last_v2x_regime = last(v2x_regime))

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(auto_confidence = lag(last_v2x_libdem_codelow) - last_v2x_libdem_codehigh)


vdem <- vdem %>%
  ungroup()%>%
  mutate(auto_dum01_libdem_ci_start005 = last_v2x_libdem - lag(last_v2x_libdem)) %>%
  select(-c(group_id)) 

summary(vdem$auto_confidence) # whenever auto_confidence is positive, the confidence intervals do not overlag each other

vdem <- vdem %>%
  group_by(auto_period_id01_libdem_ci_start005) %>%
  mutate(auto_period01_libdem_ci_start005 = ifelse(start_auto==1 & first(auto_dum01_libdem_ci_start005) <= -0.1 & first(auto_confidence) >0 , 1, 0 )) %>% 
  rename(auto_decline01_libdem_ci_start005 = auto_dum01_libdem_ci_start005) # no overlap of confidence intervals before and after autocratization 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_v2x_polyarchy = lag(v2x_polyarchy, 1),
         lag_v2x_regime = lag(v2x_regime, 1))

# summary statistics #
table(vdem$auto_period01_libdem_ci_start005)

auto_periods_LDI01ci_start005 <- vdem %>%
  filter(auto_period01_libdem_ci_start005 ==1) %>%
  group_by(auto_period_id01_libdem_ci_start005) %>%
  dplyr::summarize(country_name = first (country_name), 
                   start_year = first(year), 
                   end_year = last(year), 
                   EDI_before = first(lag_v2x_polyarchy),
                   EDI_end = last(v2x_polyarchy), 
                   decline = first(auto_decline01_libdem_ci_start005)) %>%
  dplyr::mutate(country_year = str_c(country_name, start_year, sep = "_"))

anti_join(auto_periods_LDI01ci, auto_periods_LDI01ci_start005, by = "country_year")

auto_countries_LDI01ci_start005 <- vdem %>%
  filter(auto_period01_libdem_ci_start005==1) %>%
  group_by(country_name) %>%
  dplyr::summarize(countries = first (country_name)) 

# 125 autocratization episodes in 81 countries

vdem_auto_LDI01ci_start005 <- vdem %>%
  select(country_name, year, auto_period01_libdem_ci_start005, auto_decline01_libdem_ci_start005, auto_period_id01_libdem_ci_start005, 
         last_v2x_regime, lag_v2x_regime)

#############################################################################################
#### Definition Lührmann and Lindberg adopted  (2019): LDI decline 0.15 + CI, start_incl = 0.05 ####
#############################################################################################

vdem <- vdem_org

#### Autocratization Periods ####

vdem <- vdem %>%
  select(country_name, year, v2x_polyarchy, v2x_libdem, v2x_libdem_codelow, v2x_libdem_codehigh, v2x_regime) %>%
  filter(year >= 1900) %>%
  group_by(country_name) %>%
  mutate(LDI_1 = v2x_libdem - lag(v2x_libdem, 1), 
         start_auto = ifelse(LDI_1<=-0.05, 1, NA), 
         min_1 = lead(LDI_1, 1), 
         min_2 = lead(LDI_1, 2), 
         min_3 = lead(LDI_1, 3), 
         min_4 = lead(LDI_1, 4), 
         auto_end = ifelse(min_1<=-0.05 | min_2<=-0.05 | min_3<=-0.05 | min_4<=-0.05, 0, 1),
         auto_end2 = ifelse(LDI_1>=0.08, 1, 0)) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(start_auto_1 = ifelse(lag(start_auto)==1, 1, 0), 
         start_auto_2 = ifelse(lag(start_auto, 2)==1, 1, 0), 
         start_auto_3 = ifelse(lag(start_auto, 3)==1, 1, 0), 
         start_auto_4 = ifelse(lag(start_auto, 4)==1, 1, 0), 
         start_auto_four = coalesce(start_auto_1, start_auto_2, start_auto_3, start_auto_4), 
         start_auto_four = ifelse(is.na(start_auto_four), 0, start_auto_four))  %>%
  select(-c(start_auto_1, start_auto_2, start_auto_3, start_auto_4)) %>%
  select(-c(min_1, min_2, min_3, min_4)) %>%
  fill(auto_end)

vdem <- as.data.frame(vdem)
for (i in 1:nrow(vdem)) {
  vdem[i,][is.na(vdem[i,]) & vdem$auto_end[i-1] == 0 & vdem$start_auto_four[i] == 1 & vdem$start_auto[i-1] ==1 &
             vdem$auto_end2[i] != 1] <- 1 
}

table(vdem$start_auto)

## Autocratization Periods with a total magnitude of 0.15 (15% decrease) in LDI  ##
# nessearcy conditions: one year before autocratization started -  the decline at the end of the episode >=0.15 and
# no overlap of confidence intervals  according to Laebens and Lührmann 

# first step: create auto_period ID
vdem <- vdem %>%
  mutate(start_auto = ifelse(is.na(start_auto), 0, start_auto)) %>%
  group_by(country_name) %>%
  mutate(group_id = cumsum(start_auto != lag(start_auto, default = FALSE))) # group_id for each autocra_period

vdem <- vdem %>%
  mutate(auto_period_id015_libdem_ci_start005 = str_c(country_name, group_id, sep = "_"))

# second step: create dummy in which autocratization was greater than 0.15

vdem <- vdem %>%
  group_by(auto_period_id015_libdem_ci_start005) %>%
  mutate(last_v2x_libdem = last(v2x_libdem), 
         last_v2x_libdem_codelow = last(v2x_libdem_codelow), 
         last_v2x_libdem_codehigh = last(v2x_libdem_codehigh), 
         last_v2x_regime = last(v2x_regime))

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(auto_confidence = lag(last_v2x_libdem_codelow) - last_v2x_libdem_codehigh)


vdem <- vdem %>%
  ungroup()%>%
  mutate(auto_dum015_libdem_ci_start005 = last_v2x_libdem - lag(last_v2x_libdem)) %>%
  select(-c(group_id)) 

summary(vdem$auto_confidence) # whenever auto_confidence is positive, the confidence intervals do not overlag each other

vdem <- vdem %>%
  group_by(auto_period_id015_libdem_ci_start005) %>%
  mutate(auto_period015_libdem_ci_start005 = ifelse(start_auto==1 & first(auto_dum015_libdem_ci_start005) <= -0.15 & first(auto_confidence) >0 , 1, 0 )) %>% 
  rename(auto_decline015_libdem_ci_start005 = auto_dum015_libdem_ci_start005) # no overlap of confidence intervals before and after autocratization 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_v2x_polyarchy = lag(v2x_polyarchy, 1),
         lag_v2x_regime = lag(v2x_regime, 1))

# summary statistics #
table(vdem$auto_period015_libdem_ci_start005)

auto_periods_LDI015ci_start005 <- vdem %>%
  filter(auto_period015_libdem_ci_start005 ==1) %>%
  group_by(auto_period_id015_libdem_ci_start005) %>%
  dplyr::summarize(country_name = first (country_name), 
                   start_year = first(year), 
                   end_year = last(year), 
                   EDI_before = first(lag_v2x_polyarchy),
                   EDI_end = last(v2x_polyarchy), 
                   decline = first(auto_decline015_libdem_ci_start005)) %>%
  dplyr::mutate(country_year = str_c(country_name, start_year, sep = "_"))

anti_join(auto_periods_LDI015ci, auto_periods_LDI015ci_start005, by = "country_year")

auto_countries_LDI015ci_start005 <- vdem %>%
  filter(auto_period015_libdem_ci_start005==1) %>%
  group_by(country_name) %>%
  dplyr::summarize(countries = first (country_name)) 

# 73 autocratization episodes in 52 countries

vdem_auto_LDI015ci_start005 <- vdem %>%
  select(country_name, year, auto_period015_libdem_ci_start005, auto_decline015_libdem_ci_start005, auto_period_id015_libdem_ci_start005, 
         last_v2x_regime, lag_v2x_regime)


#############################################################################################
#### Definition Lührmann and Lindberg adopted  (2019): LDI decline 0.2 + CI, start_incl = 0.05  ####
#############################################################################################

vdem <- vdem_org

#### Autocratization Periods ####

vdem <- vdem %>%
  select(country_name, year, v2x_polyarchy, v2x_libdem, v2x_libdem_codelow, v2x_libdem_codehigh, v2x_regime) %>%
  filter(year >= 1900) %>%
  group_by(country_name) %>%
  mutate(LDI_1 = v2x_libdem - lag(v2x_libdem, 1), 
         start_auto = ifelse(LDI_1<=-0.05, 1, NA), 
         min_1 = lead(LDI_1, 1), 
         min_2 = lead(LDI_1, 2), 
         min_3 = lead(LDI_1, 3), 
         min_4 = lead(LDI_1, 4), 
         auto_end = ifelse(min_1<=-0.05 | min_2<=-0.05 | min_3<=-0.05 | min_4<=-0.05, 0, 1),
         auto_end2 = ifelse(LDI_1>=0.08, 1, 0)) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(start_auto_1 = ifelse(lag(start_auto)==1, 1, 0), 
         start_auto_2 = ifelse(lag(start_auto, 2)==1, 1, 0), 
         start_auto_3 = ifelse(lag(start_auto, 3)==1, 1, 0), 
         start_auto_4 = ifelse(lag(start_auto, 4)==1, 1, 0), 
         start_auto_four = coalesce(start_auto_1, start_auto_2, start_auto_3, start_auto_4), 
         start_auto_four = ifelse(is.na(start_auto_four), 0, start_auto_four))  %>%
  select(-c(start_auto_1, start_auto_2, start_auto_3, start_auto_4)) %>%
  select(-c(min_1, min_2, min_3, min_4)) %>%
  fill(auto_end)

vdem <- as.data.frame(vdem)
for (i in 1:nrow(vdem)) {
  vdem[i,][is.na(vdem[i,]) & vdem$auto_end[i-1] == 0 & vdem$start_auto_four[i] == 1 & vdem$start_auto[i-1] ==1 &
             vdem$auto_end2[i] != 1] <- 1 
}

table(vdem$start_auto)

## Autocratization Periods with a total magnitude of 0.2 (20% decrease) in LDI  ##
# nessearcy conditions: one year before autocratization started -  the decline at the end of the episode >=0.2 and
# no overlap of confidence intervals  according to Laebens and Lührmann 

# first step: create auto_period ID
vdem <- vdem %>%
  mutate(start_auto = ifelse(is.na(start_auto), 0, start_auto)) %>%
  group_by(country_name) %>%
  mutate(group_id = cumsum(start_auto != lag(start_auto, default = FALSE))) # group_id for each autocra_period

vdem <- vdem %>%
  mutate(auto_period_id02_libdem_ci_start005 = str_c(country_name, group_id, sep = "_"))

# second step: create dummy in which autocratization was greater than 0.15

vdem <- vdem %>%
  group_by(auto_period_id02_libdem_ci_start005) %>%
  mutate(last_v2x_libdem = last(v2x_libdem), 
         last_v2x_libdem_codelow = last(v2x_libdem_codelow), 
         last_v2x_libdem_codehigh = last(v2x_libdem_codehigh), 
         last_v2x_regime = last(v2x_regime))

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(auto_confidence = lag(last_v2x_libdem_codelow) - last_v2x_libdem_codehigh)


vdem <- vdem %>%
  ungroup()%>%
  mutate(auto_dum02_libdem_ci_start005 = last_v2x_libdem - lag(last_v2x_libdem)) %>%
  select(-c(group_id)) 

summary(vdem$auto_confidence) # whenever auto_confidence is positive, the confidence intervals do not overlag each other

vdem <- vdem %>%
  group_by(auto_period_id02_libdem_ci_start005) %>%
  mutate(auto_period02_libdem_ci_start005 = ifelse(start_auto==1 & first(auto_dum02_libdem_ci_start005) <= -0.2 & first(auto_confidence) >0 , 1, 0 )) %>% 
  rename(auto_decline02_libdem_ci_start005 = auto_dum02_libdem_ci_start005) # no overlap of confidence intervals before and after autocratization 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_v2x_polyarchy = lag(v2x_polyarchy, 1),
         lag_v2x_regime = lag(v2x_regime, 1))

# summary statistics #
table(vdem$auto_period02_libdem_ci_start005)

auto_periods_LDI02ci_start005 <- vdem %>%
  filter(auto_period02_libdem_ci_start005 ==1) %>%
  group_by(auto_period_id02_libdem_ci_start005) %>%
  dplyr::summarize(country_name = first (country_name), 
                   start_year = first(year), 
                   end_year = last(year), 
                   EDI_before = first(lag_v2x_polyarchy),
                   EDI_end = last(v2x_polyarchy), 
                   decline = first(auto_decline02_libdem_ci_start005)) %>%
  dplyr::mutate(country_year = str_c(country_name, start_year, sep = "_"))

anti_join(auto_periods_LDI02ci, auto_periods_LDI02ci_start005, by = "country_year")

auto_countries_LDI02ci_start005 <- vdem %>%
  filter(auto_period02_libdem_ci_start005==1) %>%
  group_by(country_name) %>%
  dplyr::summarize(countries = first (country_name)) 

# 58 autocratization episodes in 42 countries

vdem_auto_LDI02ci_start005 <- vdem %>%
  select(country_name, year, auto_period02_libdem_ci_start005, auto_decline02_libdem_ci_start005, auto_period_id02_libdem_ci_start005, 
         last_v2x_regime, lag_v2x_regime)

#############################################################################################
#### Polity IV: 0.1 decline on rescaled scores ####
#############################################################################################

vdem <- vdem_org 

#### Rescale Polity IV to [0,1] ####

vdem <- vdem %>%
  mutate(e_p_polity = ifelse(e_p_polity <= -11, NA, e_p_polity))

vdem$e_p_polity <- rescale(vdem$e_p_polity, to = c(0,1)) # rescaled Polity IV Variable to [0,1]
summary(vdem$e_p_polity)


#### Autocratization Periods ####

vdem <- vdem %>%
  select(country_name, year, e_p_polity, v2x_regime) %>%
  filter(year >= 1900 & year != 2019) %>%
  group_by(country_name) %>%
  mutate(polity_1 = e_p_polity - lag(e_p_polity, 1), 
         start_auto = ifelse(polity_1<=-0.01, 1, NA), 
         min_1 = lead(polity_1, 1), 
         min_2 = lead(polity_1, 2), 
         min_3 = lead(polity_1, 3), 
         min_4 = lead(polity_1, 4), 
         auto_end = ifelse(min_1<=-0.01 | min_2<=-0.01 | min_3<=-0.01 | min_4<=-0.01, 0, 1),
         auto_end2 = ifelse(e_p_polity>=0.02, 1, 0)) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(start_auto_1 = ifelse(lag(start_auto)==1, 1, 0), 
         start_auto_2 = ifelse(lag(start_auto, 2)==1, 1, 0), 
         start_auto_3 = ifelse(lag(start_auto, 3)==1, 1, 0), 
         start_auto_4 = ifelse(lag(start_auto, 4)==1, 1, 0), 
         start_auto_four = coalesce(start_auto_1, start_auto_2, start_auto_3, start_auto_4), 
         start_auto_four = ifelse(is.na(start_auto_four), 0, start_auto_four))  %>%
  select(-c(start_auto_1, start_auto_2, start_auto_3, start_auto_4)) %>%
  select(-c(min_1, min_2, min_3, min_4)) %>%
  fill(auto_end)

vdem <- as.data.frame(vdem)
for (i in 1:nrow(vdem)) {
  vdem[i,][is.na(vdem[i,]) & vdem$auto_end[i-1] == 0 & vdem$start_auto_four[i] == 1 & vdem$start_auto[i-1] ==1 &
             vdem$auto_end2[i] != 1] <- 1 
}

table(vdem$start_auto)


## Autocratization Periods with a total magnitude of 10% decrease in Polity IV  ##
# nessearcy conditions: one year before autocratization started -  the decline at the end of the episode >=0.1 

# first step: create auto_period ID
vdem <- vdem %>%
  mutate(start_auto = ifelse(is.na(start_auto), 0, start_auto)) %>%
  group_by(country_name) %>%
  mutate(group_id = cumsum(start_auto != lag(start_auto, default = FALSE))) # group_id for each autocra_period

vdem <- vdem %>%
  mutate(auto_period_polity_id = str_c(country_name, group_id, sep = "_"))

# second step: create dummy in which autocratization was greater than 0.1

vdem <- vdem %>%
  group_by(auto_period_polity_id) %>%
  mutate(last_polity = last(e_p_polity), 
         last_v2x_regime = last(v2x_regime))

vdem <- vdem %>%
  ungroup()%>%
  mutate(auto_dum_polity = last_polity - lag(last_polity)) %>%
  select(-c(group_id)) 


vdem <- vdem %>%
  group_by(auto_period_polity_id) %>%
  mutate(auto_period_polity = ifelse(start_auto==1 & first(auto_dum_polity) < -0.0999999 , 1, 0 )) %>% 
  rename(auto_decline_polity = auto_dum_polity) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_e_p_polity = lag(e_p_polity, 1), 
         lag_v2x_regime = lag(v2x_regime, 1))


# summary statistics #
table(vdem$auto_period_polity)

auto_periods_polity <- vdem %>%
  filter(auto_period_polity ==1) %>%
  group_by(auto_period_polity_id) %>%
  dplyr::summarize(country_name = first (country_name), 
            start_year = first(year), 
            end_year = last(year), 
            Polity_before = first(lag_e_p_polity),
            Polity_end = last(e_p_polity), 
            decline = first(auto_decline_polity)) %>%
  mutate(country_year = str_c(country_name, start_year, sep = "_"))

anti_join(auto_periods_EDI01, auto_periods_polity, by = "country_year")

auto_countries_polity <- vdem %>%
  filter(auto_period_polity==1) %>%
  group_by(country_name) %>%
  dplyr::summarize(countries = first (country_name)) 

# 213 autocratization episodes in 103 countries

vdem_auto_polity <- vdem %>%
  select(country_name, year, auto_period_polity_id, auto_decline_polity, auto_period_polity, 
         last_v2x_regime, lag_v2x_regime)

#############################################################################################
#### Polity IV: 0.05 decline on rescaled scores ####
#############################################################################################

vdem <- vdem_org 

#### Rescale Polity IV to [0,1] ####

vdem <- vdem %>%
  mutate(e_p_polity = ifelse(e_p_polity <= -11, NA, e_p_polity))

vdem$e_p_polity <- rescale(vdem$e_p_polity, to = c(0,1)) # rescaled Polity IV Variable to [0,1]
summary(vdem$e_p_polity)


#### Autocratization Periods ####

vdem <- vdem %>%
  select(country_name, year, e_p_polity, v2x_regime) %>%
  filter(year >= 1900 & year != 2019) %>%
  group_by(country_name) %>%
  mutate(polity_1 = e_p_polity - lag(e_p_polity, 1), 
         start_auto = ifelse(polity_1<=-0.01, 1, NA), 
         min_1 = lead(polity_1, 1), 
         min_2 = lead(polity_1, 2), 
         min_3 = lead(polity_1, 3), 
         min_4 = lead(polity_1, 4), 
         auto_end = ifelse(min_1<=-0.01 | min_2<=-0.01 | min_3<=-0.01 | min_4<=-0.01, 0, 1),
         auto_end2 = ifelse(e_p_polity>=0.02, 1, 0)) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(start_auto_1 = ifelse(lag(start_auto)==1, 1, 0), 
         start_auto_2 = ifelse(lag(start_auto, 2)==1, 1, 0), 
         start_auto_3 = ifelse(lag(start_auto, 3)==1, 1, 0), 
         start_auto_4 = ifelse(lag(start_auto, 4)==1, 1, 0), 
         start_auto_four = coalesce(start_auto_1, start_auto_2, start_auto_3, start_auto_4), 
         start_auto_four = ifelse(is.na(start_auto_four), 0, start_auto_four))  %>%
  select(-c(start_auto_1, start_auto_2, start_auto_3, start_auto_4)) %>%
  select(-c(min_1, min_2, min_3, min_4)) %>%
  fill(auto_end)

vdem <- as.data.frame(vdem)
for (i in 1:nrow(vdem)) {
  vdem[i,][is.na(vdem[i,]) & vdem$auto_end[i-1] == 0 & vdem$start_auto_four[i] == 1 & vdem$start_auto[i-1] ==1 &
             vdem$auto_end2[i] != 1] <- 1 
}

table(vdem$start_auto)


## Autocratization Periods with a total magnitude of 5% decrease in Polity IV  ##
# nessearcy conditions: one year before autocratization started -  the decline at the end of the episode >=0.05 

# first step: create auto_period ID
vdem <- vdem %>%
  mutate(start_auto = ifelse(is.na(start_auto), 0, start_auto)) %>%
  group_by(country_name) %>%
  mutate(group_id = cumsum(start_auto != lag(start_auto, default = FALSE))) # group_id for each autocra_period

vdem <- vdem %>%
  mutate(auto_period_polity_id005 = str_c(country_name, group_id, sep = "_"))

# second step: create dummy in which autocratization was greater than 0.1

vdem <- vdem %>%
  group_by(auto_period_polity_id005) %>%
  mutate(last_polity = last(e_p_polity), 
         last_v2x_regime = last(v2x_regime))

vdem <- vdem %>%
  ungroup()%>%
  mutate(auto_dum_polity005 = last_polity - lag(last_polity)) %>%
  select(-c(group_id)) 


vdem <- vdem %>%
  group_by(auto_period_polity_id005) %>%
  mutate(auto_period_polity005 = ifelse(start_auto==1 & first(auto_dum_polity005) < -0.0499999 , 1, 0 )) %>% 
  rename(auto_decline_polity005 = auto_dum_polity005) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_e_p_polity = lag(e_p_polity, 1), 
         lag_v2x_regime = lag(v2x_regime, 1))


# summary statistics #
table(vdem$auto_period_polity005)

auto_periods_polity005 <- vdem %>%
  filter(auto_period_polity005 ==1) %>%
  group_by(auto_period_polity_id005) %>%
  dplyr::summarize(country_name = first (country_name), 
                   start_year = first(year), 
                   end_year = last(year), 
                   Polity_before = first(lag_e_p_polity),
                   Polity_end = last(e_p_polity), 
                   decline = first(auto_decline_polity005)) %>%
  mutate(country_year = str_c(country_name, start_year, sep = "_"))

anti_join(auto_periods_EDI01, auto_periods_polity, by = "country_year")

auto_countries_polity005 <- vdem %>%
  filter(auto_period_polity005==1) %>%
  group_by(country_name) %>%
  dplyr::summarize(countries = first (country_name)) 

# 311 autocratization episodes in 118 countries

vdem_auto_polity005 <- vdem %>%
  select(country_name, year, auto_period_polity_id005, auto_decline_polity005, auto_period_polity005, 
         last_v2x_regime, lag_v2x_regime)

#############################################################################################
#### Polity IV: 0.15 decline on rescaled scores ####
#############################################################################################

vdem <- vdem_org 

#### Rescale Polity IV to [0,1] ####

vdem <- vdem %>%
  mutate(e_p_polity = ifelse(e_p_polity <= -11, NA, e_p_polity))

vdem$e_p_polity <- rescale(vdem$e_p_polity, to = c(0,1)) # rescaled Polity IV Variable to [0,1]
summary(vdem$e_p_polity)


#### Autocratization Periods ####

vdem <- vdem %>%
  select(country_name, year, e_p_polity, v2x_regime) %>%
  filter(year >= 1900 & year != 2019) %>%
  group_by(country_name) %>%
  mutate(polity_1 = e_p_polity - lag(e_p_polity, 1), 
         start_auto = ifelse(polity_1<=-0.01, 1, NA), 
         min_1 = lead(polity_1, 1), 
         min_2 = lead(polity_1, 2), 
         min_3 = lead(polity_1, 3), 
         min_4 = lead(polity_1, 4), 
         auto_end = ifelse(min_1<=-0.01 | min_2<=-0.01 | min_3<=-0.01 | min_4<=-0.01, 0, 1),
         auto_end2 = ifelse(e_p_polity>=0.02, 1, 0)) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(start_auto_1 = ifelse(lag(start_auto)==1, 1, 0), 
         start_auto_2 = ifelse(lag(start_auto, 2)==1, 1, 0), 
         start_auto_3 = ifelse(lag(start_auto, 3)==1, 1, 0), 
         start_auto_4 = ifelse(lag(start_auto, 4)==1, 1, 0), 
         start_auto_four = coalesce(start_auto_1, start_auto_2, start_auto_3, start_auto_4), 
         start_auto_four = ifelse(is.na(start_auto_four), 0, start_auto_four))  %>%
  select(-c(start_auto_1, start_auto_2, start_auto_3, start_auto_4)) %>%
  select(-c(min_1, min_2, min_3, min_4)) %>%
  fill(auto_end)

vdem <- as.data.frame(vdem)
for (i in 1:nrow(vdem)) {
  vdem[i,][is.na(vdem[i,]) & vdem$auto_end[i-1] == 0 & vdem$start_auto_four[i] == 1 & vdem$start_auto[i-1] ==1 &
             vdem$auto_end2[i] != 1] <- 1 
}

table(vdem$start_auto)


## Autocratization Periods with a total magnitude of 15% decrease in Polity IV  ##
# nessearcy conditions: one year before autocratization started -  the decline at the end of the episode >=0.15 

# first step: create auto_period ID
vdem <- vdem %>%
  mutate(start_auto = ifelse(is.na(start_auto), 0, start_auto)) %>%
  group_by(country_name) %>%
  mutate(group_id = cumsum(start_auto != lag(start_auto, default = FALSE))) # group_id for each autocra_period

vdem <- vdem %>%
  mutate(auto_period_polity_id015 = str_c(country_name, group_id, sep = "_"))

# second step: create dummy in which autocratization was greater than 0.1

vdem <- vdem %>%
  group_by(auto_period_polity_id015) %>%
  mutate(last_polity = last(e_p_polity), 
         last_v2x_regime = last(v2x_regime))

vdem <- vdem %>%
  ungroup()%>%
  mutate(auto_dum_polity015 = last_polity - lag(last_polity)) %>%
  select(-c(group_id)) 


vdem <- vdem %>%
  group_by(auto_period_polity_id015) %>%
  mutate(auto_period_polity015 = ifelse(start_auto==1 & first(auto_dum_polity015) < -0.1499999 , 1, 0 )) %>% 
  rename(auto_decline_polity015 = auto_dum_polity015) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_e_p_polity = lag(e_p_polity, 1), 
         lag_v2x_regime = lag(v2x_regime, 1))


# summary statistics #
table(vdem$auto_period_polity015)

auto_periods_polity015 <- vdem %>%
  filter(auto_period_polity015 ==1) %>%
  group_by(auto_period_polity_id015) %>%
  dplyr::summarize(country_name = first (country_name), 
                   start_year = first(year), 
                   end_year = last(year), 
                   Polity_before = first(lag_e_p_polity),
                   Polity_end = last(e_p_polity), 
                   decline = first(auto_decline_polity015)) %>%
  mutate(country_year = str_c(country_name, start_year, sep = "_"))

anti_join(auto_periods_EDI015ci, auto_periods_polity015, by = "country_year")

auto_countries_polity015 <- vdem %>%
  filter(auto_period_polity015==1) %>%
  group_by(country_name) %>%
  dplyr::summarize(countries = first (country_name)) 

# 157 autocratization episodes in 82 countries

vdem_auto_polity015 <- vdem %>%
  select(country_name, year, auto_period_polity_id015, auto_decline_polity015, auto_period_polity015, 
         last_v2x_regime, lag_v2x_regime)

#############################################################################################
#### Polity IV: 0.2 decline on rescaled scores ####
#############################################################################################

vdem <- vdem_org 

#### Rescale Polity IV to [0,1] ####

vdem <- vdem %>%
  mutate(e_p_polity = ifelse(e_p_polity <= -11, NA, e_p_polity))

vdem$e_p_polity <- rescale(vdem$e_p_polity, to = c(0,1)) # rescaled Polity IV Variable to [0,1]
summary(vdem$e_p_polity)


#### Autocratization Periods ####

vdem <- vdem %>%
  select(country_name, year, e_p_polity, v2x_regime) %>%
  filter(year >= 1900 & year != 2019) %>%
  group_by(country_name) %>%
  mutate(polity_1 = e_p_polity - lag(e_p_polity, 1), 
         start_auto = ifelse(polity_1<=-0.01, 1, NA), 
         min_1 = lead(polity_1, 1), 
         min_2 = lead(polity_1, 2), 
         min_3 = lead(polity_1, 3), 
         min_4 = lead(polity_1, 4), 
         auto_end = ifelse(min_1<=-0.01 | min_2<=-0.01 | min_3<=-0.01 | min_4<=-0.01, 0, 1),
         auto_end2 = ifelse(e_p_polity>=0.02, 1, 0)) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(start_auto_1 = ifelse(lag(start_auto)==1, 1, 0), 
         start_auto_2 = ifelse(lag(start_auto, 2)==1, 1, 0), 
         start_auto_3 = ifelse(lag(start_auto, 3)==1, 1, 0), 
         start_auto_4 = ifelse(lag(start_auto, 4)==1, 1, 0), 
         start_auto_four = coalesce(start_auto_1, start_auto_2, start_auto_3, start_auto_4), 
         start_auto_four = ifelse(is.na(start_auto_four), 0, start_auto_four))  %>%
  select(-c(start_auto_1, start_auto_2, start_auto_3, start_auto_4)) %>%
  select(-c(min_1, min_2, min_3, min_4)) %>%
  fill(auto_end)

vdem <- as.data.frame(vdem)
for (i in 1:nrow(vdem)) {
  vdem[i,][is.na(vdem[i,]) & vdem$auto_end[i-1] == 0 & vdem$start_auto_four[i] == 1 & vdem$start_auto[i-1] ==1 &
             vdem$auto_end2[i] != 1] <- 1 
}

table(vdem$start_auto)


## Autocratization Periods with a total magnitude of 20% decrease in Polity IV  ##
# nessearcy conditions: one year before autocratization started -  the decline at the end of the episode >=0.2 

# first step: create auto_period ID
vdem <- vdem %>%
  mutate(start_auto = ifelse(is.na(start_auto), 0, start_auto)) %>%
  group_by(country_name) %>%
  mutate(group_id = cumsum(start_auto != lag(start_auto, default = FALSE))) # group_id for each autocra_period

vdem <- vdem %>%
  mutate(auto_period_polity_id02 = str_c(country_name, group_id, sep = "_"))

# second step: create dummy in which autocratization was greater than 0.1

vdem <- vdem %>%
  group_by(auto_period_polity_id02) %>%
  mutate(last_polity = last(e_p_polity), 
         last_v2x_regime = last(v2x_regime))

vdem <- vdem %>%
  ungroup()%>%
  mutate(auto_dum_polity02 = last_polity - lag(last_polity)) %>%
  select(-c(group_id)) 


vdem <- vdem %>%
  group_by(auto_period_polity_id02) %>%
  mutate(auto_period_polity02 = ifelse(start_auto==1 & first(auto_dum_polity02) < -0.1999999 , 1, 0 )) %>% 
  rename(auto_decline_polity02 = auto_dum_polity02) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_e_p_polity = lag(e_p_polity, 1), 
         lag_v2x_regime = lag(v2x_regime, 1))


# summary statistics #
table(vdem$auto_period_polity02)

auto_periods_polity02 <- vdem %>%
  filter(auto_period_polity02 ==1) %>%
  group_by(auto_period_polity_id02) %>%
  dplyr::summarize(country_name = first (country_name), 
                   start_year = first(year), 
                   end_year = last(year), 
                   Polity_before = first(lag_e_p_polity),
                   Polity_end = last(e_p_polity), 
                   decline = first(auto_decline_polity02)) %>%
  mutate(country_year = str_c(country_name, start_year, sep = "_"))

anti_join(auto_periods_EDI015ci, auto_periods_polity02, by = "country_year")

auto_countries_polity02 <- vdem %>%
  filter(auto_period_polity02==1) %>%
  group_by(country_name) %>%
  dplyr::summarize(countries = first (country_name)) 

# 128 autocratization episodes in 76 countries

vdem_auto_polity02 <- vdem %>%
  select(country_name, year, auto_period_polity_id02, auto_decline_polity02, auto_period_polity02, 
         last_v2x_regime, lag_v2x_regime)

#############################################################################################
#### Polity IV: 0.1 decline on rescaled scores, start_incl = 0.05 ####
#############################################################################################

vdem <- vdem_org 

#### Rescale Polity IV to [0,1] ####

vdem <- vdem %>%
  mutate(e_p_polity = ifelse(e_p_polity <= -11, NA, e_p_polity))

vdem$e_p_polity <- rescale(vdem$e_p_polity, to = c(0,1)) # rescaled Polity IV Variable to [0,1]
summary(vdem$e_p_polity)


#### Autocratization Periods ####

vdem <- vdem %>%
  select(country_name, year, e_p_polity, v2x_regime) %>%
  filter(year >= 1900 & year != 2019) %>%
  group_by(country_name) %>%
  mutate(polity_1 = e_p_polity - lag(e_p_polity, 1), 
         start_auto = ifelse(polity_1<=-0.05, 1, NA), 
         min_1 = lead(polity_1, 1), 
         min_2 = lead(polity_1, 2), 
         min_3 = lead(polity_1, 3), 
         min_4 = lead(polity_1, 4), 
         auto_end = ifelse(min_1<=-0.05 | min_2<=-0.05 | min_3<=-0.05 | min_4<=-0.05, 0, 1),
         auto_end2 = ifelse(e_p_polity>=0.8, 1, 0)) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(start_auto_1 = ifelse(lag(start_auto)==1, 1, 0), 
         start_auto_2 = ifelse(lag(start_auto, 2)==1, 1, 0), 
         start_auto_3 = ifelse(lag(start_auto, 3)==1, 1, 0), 
         start_auto_4 = ifelse(lag(start_auto, 4)==1, 1, 0), 
         start_auto_four = coalesce(start_auto_1, start_auto_2, start_auto_3, start_auto_4), 
         start_auto_four = ifelse(is.na(start_auto_four), 0, start_auto_four))  %>%
  select(-c(start_auto_1, start_auto_2, start_auto_3, start_auto_4)) %>%
  select(-c(min_1, min_2, min_3, min_4)) %>%
  fill(auto_end)

vdem <- as.data.frame(vdem)
for (i in 1:nrow(vdem)) {
  vdem[i,][is.na(vdem[i,]) & vdem$auto_end[i-1] == 0 & vdem$start_auto_four[i] == 1 & vdem$start_auto[i-1] ==1 &
             vdem$auto_end2[i] != 1] <- 1 
}

table(vdem$start_auto)


## Autocratization Periods with a total magnitude of 10% decrease in Polity IV  ##
# nessearcy conditions: one year before autocratization started -  the decline at the end of the episode >=0.1 

# first step: create auto_period ID
vdem <- vdem %>%
  mutate(start_auto = ifelse(is.na(start_auto), 0, start_auto)) %>%
  group_by(country_name) %>%
  mutate(group_id = cumsum(start_auto != lag(start_auto, default = FALSE))) # group_id for each autocra_period

vdem <- vdem %>%
  mutate(auto_period_polity_id01_st005 = str_c(country_name, group_id, sep = "_"))

# second step: create dummy in which autocratization was greater than 0.1

vdem <- vdem %>%
  group_by(auto_period_polity_id01_st005) %>%
  mutate(last_polity = last(e_p_polity), 
         last_v2x_regime = last(v2x_regime))

vdem <- vdem %>%
  ungroup()%>%
  mutate(auto_dum_polity01_st005 = last_polity - lag(last_polity)) %>%
  select(-c(group_id)) 


vdem <- vdem %>%
  group_by(auto_period_polity_id01_st005) %>%
  mutate(auto_period_polity01_st005 = ifelse(start_auto==1 & first(auto_dum_polity01_st005) < -0.09999999 , 1, 0 )) %>% 
  rename(auto_decline_polity01_st005 = auto_dum_polity01_st005) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_e_p_polity = lag(e_p_polity, 1), 
         lag_v2x_regime = lag(v2x_regime, 1))


# summary statistics #
table(vdem$auto_period_polity01_st005)

auto_periods_polity01_st005 <- vdem %>%
  filter(auto_period_polity01_st005 ==1) %>%
  group_by(auto_period_polity_id01_st005) %>%
  dplyr::summarize(country_name = first (country_name), 
                   start_year = first(year), 
                   end_year = last(year), 
                   Polity_before = first(lag_e_p_polity),
                   Polity_end = last(e_p_polity), 
                   decline = first(auto_decline_polity01_st005)) %>%
  mutate(country_year = str_c(country_name, start_year, sep = "_"))

anti_join(auto_periods_polity, auto_periods_polity01_st005, by = "country_year")

auto_countries_polity01_st005 <- vdem %>%
  filter(auto_period_polity01_st005==1) %>%
  group_by(country_name) %>%
  dplyr::summarize(countries = first (country_name)) 

# 192 autocratization episodes in 102 countries

vdem_auto_polity01_st005 <- vdem %>%
  select(country_name, year, auto_period_polity_id01_st005, auto_decline_polity01_st005, auto_period_polity01_st005, 
         last_v2x_regime, lag_v2x_regime)

#############################################################################################
#### Polity IV: 0.15 decline on rescaled scores, start_incl = 0.05 ####
#############################################################################################

vdem <- vdem_org 

#### Rescale Polity IV to [0,1] ####

vdem <- vdem %>%
  mutate(e_p_polity = ifelse(e_p_polity <= -11, NA, e_p_polity))

vdem$e_p_polity <- rescale(vdem$e_p_polity, to = c(0,1)) # rescaled Polity IV Variable to [0,1]
summary(vdem$e_p_polity)


#### Autocratization Periods ####

vdem <- vdem %>%
  select(country_name, year, e_p_polity, v2x_regime) %>%
  filter(year >= 1900 & year != 2019) %>%
  group_by(country_name) %>%
  mutate(polity_1 = e_p_polity - lag(e_p_polity, 1), 
         start_auto = ifelse(polity_1<=-0.05, 1, NA), 
         min_1 = lead(polity_1, 1), 
         min_2 = lead(polity_1, 2), 
         min_3 = lead(polity_1, 3), 
         min_4 = lead(polity_1, 4), 
         auto_end = ifelse(min_1<=-0.05 | min_2<=-0.05 | min_3<=-0.05 | min_4<=-0.05, 0, 1),
         auto_end2 = ifelse(e_p_polity>=0.08, 1, 0)) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(start_auto_1 = ifelse(lag(start_auto)==1, 1, 0), 
         start_auto_2 = ifelse(lag(start_auto, 2)==1, 1, 0), 
         start_auto_3 = ifelse(lag(start_auto, 3)==1, 1, 0), 
         start_auto_4 = ifelse(lag(start_auto, 4)==1, 1, 0), 
         start_auto_four = coalesce(start_auto_1, start_auto_2, start_auto_3, start_auto_4), 
         start_auto_four = ifelse(is.na(start_auto_four), 0, start_auto_four))  %>%
  select(-c(start_auto_1, start_auto_2, start_auto_3, start_auto_4)) %>%
  select(-c(min_1, min_2, min_3, min_4)) %>%
  fill(auto_end)

vdem <- as.data.frame(vdem)
for (i in 1:nrow(vdem)) {
  vdem[i,][is.na(vdem[i,]) & vdem$auto_end[i-1] == 0 & vdem$start_auto_four[i] == 1 & vdem$start_auto[i-1] ==1 &
             vdem$auto_end2[i] != 1] <- 1 
}

table(vdem$start_auto)


## Autocratization Periods with a total magnitude of 15% decrease in Polity IV  ##
# nessearcy conditions: one year before autocratization started -  the decline at the end of the episode >=0.15 

# first step: create auto_period ID
vdem <- vdem %>%
  mutate(start_auto = ifelse(is.na(start_auto), 0, start_auto)) %>%
  group_by(country_name) %>%
  mutate(group_id = cumsum(start_auto != lag(start_auto, default = FALSE))) # group_id for each autocra_period

vdem <- vdem %>%
  mutate(auto_period_polity_id015_st005 = str_c(country_name, group_id, sep = "_"))

# second step: create dummy in which autocratization was greater than 0.1

vdem <- vdem %>%
  group_by(auto_period_polity_id015_st005) %>%
  mutate(last_polity = last(e_p_polity), 
         last_v2x_regime = last(v2x_regime))

vdem <- vdem %>%
  ungroup()%>%
  mutate(auto_dum_polity015_st005 = last_polity - lag(last_polity)) %>%
  select(-c(group_id)) 


vdem <- vdem %>%
  group_by(auto_period_polity_id015_st005) %>%
  mutate(auto_period_polity015_st005 = ifelse(start_auto==1 & first(auto_dum_polity015_st005) < -0.149999999 , 1, 0 )) %>% 
  rename(auto_decline_polity015_st005 = auto_dum_polity015_st005) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_e_p_polity = lag(e_p_polity, 1), 
         lag_v2x_regime = lag(v2x_regime, 1))


# summary statistics #
table(vdem$auto_period_polity015_st005)

auto_periods_polity015_st005 <- vdem %>%
  filter(auto_period_polity015_st005 ==1) %>%
  group_by(auto_period_polity_id015_st005) %>%
  dplyr::summarize(country_name = first (country_name), 
                   start_year = first(year), 
                   end_year = last(year), 
                   Polity_before = first(lag_e_p_polity),
                   Polity_end = last(e_p_polity), 
                   decline = first(auto_decline_polity015_st005)) %>%
  mutate(country_year = str_c(country_name, start_year, sep = "_"))

anti_join(auto_periods_polity015, auto_periods_polity015_st005, by = "country_year")

auto_countries_polity015_st005 <- vdem %>%
  filter(auto_period_polity015_st005==1) %>%
  group_by(country_name) %>%
  dplyr::summarize(countries = first (country_name)) 

# 156 autocratization episodes in 102 countries

vdem_auto_polity015_st005 <- vdem %>%
  select(country_name, year, auto_period_polity_id015_st005, auto_decline_polity015_st005, auto_period_polity015_st005, 
         last_v2x_regime, lag_v2x_regime)


#############################################################################################
#### Polity IV: 0.2 decline on rescaled scores, start_incl = 0.05 ####
#############################################################################################

vdem <- vdem_org 

#### Rescale Polity IV to [0,1] ####

vdem <- vdem %>%
  mutate(e_p_polity = ifelse(e_p_polity <= -11, NA, e_p_polity))

vdem$e_p_polity <- rescale(vdem$e_p_polity, to = c(0,1)) # rescaled Polity IV Variable to [0,1]
summary(vdem$e_p_polity)


#### Autocratization Periods ####

vdem <- vdem %>%
  select(country_name, year, e_p_polity, v2x_regime) %>%
  filter(year >= 1900 & year != 2019) %>%
  group_by(country_name) %>%
  mutate(polity_1 = e_p_polity - lag(e_p_polity, 1), 
         start_auto = ifelse(polity_1<=-0.05, 1, NA), 
         min_1 = lead(polity_1, 1), 
         min_2 = lead(polity_1, 2), 
         min_3 = lead(polity_1, 3), 
         min_4 = lead(polity_1, 4), 
         auto_end = ifelse(min_1<=-0.05 | min_2<=-0.05 | min_3<=-0.05 | min_4<=-0.05, 0, 1),
         auto_end2 = ifelse(e_p_polity>=0.08, 1, 0)) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(start_auto_1 = ifelse(lag(start_auto)==1, 1, 0), 
         start_auto_2 = ifelse(lag(start_auto, 2)==1, 1, 0), 
         start_auto_3 = ifelse(lag(start_auto, 3)==1, 1, 0), 
         start_auto_4 = ifelse(lag(start_auto, 4)==1, 1, 0), 
         start_auto_four = coalesce(start_auto_1, start_auto_2, start_auto_3, start_auto_4), 
         start_auto_four = ifelse(is.na(start_auto_four), 0, start_auto_four))  %>%
  select(-c(start_auto_1, start_auto_2, start_auto_3, start_auto_4)) %>%
  select(-c(min_1, min_2, min_3, min_4)) %>%
  fill(auto_end)

vdem <- as.data.frame(vdem)
for (i in 1:nrow(vdem)) {
  vdem[i,][is.na(vdem[i,]) & vdem$auto_end[i-1] == 0 & vdem$start_auto_four[i] == 1 & vdem$start_auto[i-1] ==1 &
             vdem$auto_end2[i] != 1] <- 1 
}

table(vdem$start_auto)


## Autocratization Periods with a total magnitude of 20% decrease in Polity IV  ##
# nessearcy conditions: one year before autocratization started -  the decline at the end of the episode >=0.2 

# first step: create auto_period ID
vdem <- vdem %>%
  mutate(start_auto = ifelse(is.na(start_auto), 0, start_auto)) %>%
  group_by(country_name) %>%
  mutate(group_id = cumsum(start_auto != lag(start_auto, default = FALSE))) # group_id for each autocra_period

vdem <- vdem %>%
  mutate(auto_period_polity_id02_st005 = str_c(country_name, group_id, sep = "_"))

# second step: create dummy in which autocratization was greater than 0.2

vdem <- vdem %>%
  group_by(auto_period_polity_id02_st005) %>%
  mutate(last_polity = last(e_p_polity), 
         last_v2x_regime = last(v2x_regime))

vdem <- vdem %>%
  ungroup()%>%
  mutate(auto_dum_polity02_st005 = last_polity - lag(last_polity)) %>%
  select(-c(group_id)) 


vdem <- vdem %>%
  group_by(auto_period_polity_id02_st005) %>%
  mutate(auto_period_polity02_st005 = ifelse(start_auto==1 & first(auto_dum_polity02_st005) < -0.19999999 , 1, 0 )) %>% 
  rename(auto_decline_polity02_st005 = auto_dum_polity02_st005) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_e_p_polity = lag(e_p_polity, 1), 
         lag_v2x_regime = lag(v2x_regime, 1))


# summary statistics #
table(vdem$auto_period_polity02_st005)

auto_periods_polity02_st005 <- vdem %>%
  filter(auto_period_polity02_st005 ==1) %>%
  group_by(auto_period_polity_id02_st005) %>%
  dplyr::summarize(country_name = first (country_name), 
                   start_year = first(year), 
                   end_year = last(year), 
                   Polity_before = first(lag_e_p_polity),
                   Polity_end = last(e_p_polity), 
                   decline = first(auto_decline_polity02_st005)) %>%
  mutate(country_year = str_c(country_name, start_year, sep = "_"))

anti_join(auto_periods_polity02, auto_periods_polity02_st005, by = "country_year")

auto_countries_polity02_st005 <- vdem %>%
  filter(auto_period_polity02_st005==1) %>%
  group_by(country_name) %>%
  dplyr::summarize(countries = first (country_name)) 

# 128 autocratization episodes in 76 countries

vdem_auto_polity02_st005 <- vdem %>%
  select(country_name, year, auto_period_polity_id02_st005, auto_decline_polity02_st005, auto_period_polity02_st005, 
         last_v2x_regime, lag_v2x_regime)


#############################################################################################
#### Freedom House Measure: Decline of 0.1, other rules remain
#############################################################################################

vdem <- vdem_org 

#### Autocratization Periods ####

#### Rescale Freedom House ####
table(vdem$e_fh_cl)
table(vdem$e_fh_pr)

vdem <- vdem %>%
  mutate(e_fh_cl = ifelse(e_fh_cl==7, 1, 
                          ifelse(e_fh_cl==6, 2,
                                 ifelse(e_fh_cl==5, 3,
                                        ifelse(e_fh_cl==4, 4,
                                               ifelse(e_fh_cl==3, 5,
                                                      ifelse(e_fh_cl==2, 6, 
                                                             ifelse(e_fh_cl==1, 7, NA))))))))

vdem <- vdem %>%
  mutate(e_fh_pr = ifelse(e_fh_pr==7, 1, 
                          ifelse(e_fh_pr==6, 2,
                                 ifelse(e_fh_pr==5, 3,
                                        ifelse(e_fh_pr==4, 4,
                                               ifelse(e_fh_pr==3, 5,
                                                      ifelse(e_fh_pr==2, 6, 
                                                             ifelse(e_fh_pr==1, 7, NA))))))))


vdem <- vdem %>%
    mutate(e_fh_com = e_fh_cl + e_fh_pr) 
summary(vdem$e_fh_com)

vdem$e_fh_com <- rescale(vdem$e_fh_com, to = c(0,1)) # rescaled Polity IV Variable to [0,1]

summary(vdem$e_fh_com)

#### Autocratization Periods ####

vdem <- vdem %>%
  select(country_name, year, e_fh_com, v2x_regime) %>%
  filter(year >= 1972 & year != 2019) %>%
  group_by(country_name) %>%
  mutate(e_fh_com_1 = e_fh_com - lag(e_fh_com, 1), 
         start_auto = ifelse(e_fh_com_1<=-0.01, 1, NA), 
         min_1 = lead(e_fh_com_1, 1), 
         min_2 = lead(e_fh_com_1, 2), 
         min_3 = lead(e_fh_com_1, 3), 
         min_4 = lead(e_fh_com_1, 4), 
         auto_end = ifelse(min_1<=-0.01 | min_2<=-0.01 | min_3<=-0.01 | min_4<=-0.01, 0, 1),
         auto_end = ifelse(is.na(auto_end), 0, auto_end),
         auto_end2 = ifelse(e_fh_com_1>=0.02, 1, 0)) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(start_auto_1 = ifelse(lag(start_auto)==1, 1, 0), 
         start_auto_2 = ifelse(lag(start_auto, 2)==1, 1, 0), 
         start_auto_3 = ifelse(lag(start_auto, 3)==1, 1, 0), 
         start_auto_4 = ifelse(lag(start_auto, 4)==1, 1, 0), 
         start_auto_four = coalesce(start_auto_1, start_auto_2, start_auto_3, start_auto_4), 
         start_auto_four = ifelse(is.na(start_auto_four), 0, start_auto_four))  %>%
  select(-c(start_auto_1, start_auto_2, start_auto_3, start_auto_4)) %>%
  select(-c(min_1, min_2, min_3, min_4)) %>%
  fill(auto_end)

vdem <- as.data.frame(vdem)
for (i in 1:nrow(vdem)) {
  vdem[i,][is.na(vdem[i,]) & vdem$auto_end[i-1] == 0 & vdem$start_auto_four[i] == 1 & vdem$start_auto[i-1] ==1 &
             vdem$auto_end2[i] != 1] <- 1 
}

table(vdem$start_auto)


## Autocratization Periods with a total magnitude of 10% in Freedom House  ##
# nessearcy conditions: one year before autocratization started -  the decline at the end of the episode >=0.1 

# first step: create auto_period ID
vdem <- vdem %>%
  mutate(start_auto = ifelse(is.na(start_auto), 0, start_auto)) %>%
  group_by(country_name) %>%
  mutate(group_id = cumsum(start_auto != lag(start_auto, default = FALSE))) # group_id for each autocra_period

vdem <- vdem %>%
  mutate(auto_period_fh_id = str_c(country_name, group_id, sep = "_"))

# second step: create dummy in which autocratization was greater than 0.1

vdem <- vdem %>%
  group_by(auto_period_fh_id) %>%
  mutate(last_e_fh_com = last(e_fh_com), 
         last_v2x_regime = last(v2x_regime))

vdem <- vdem %>%
  ungroup()%>%
  mutate(auto_dum_fh = last_e_fh_com - lag(last_e_fh_com)) %>%
  select(-c(group_id)) 


vdem <- vdem %>%
  group_by(auto_period_fh_id) %>%
  mutate(auto_period_fh = ifelse(start_auto==1 & first(auto_dum_fh) <= -0.1 , 1, 0 )) %>% 
  rename(auto_decline_fh = auto_dum_fh) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_e_fh_com = lag(e_fh_com, 1), 
         lag_v2x_regime = lag(v2x_regime, 1))


# summary statistics #
table(vdem$auto_period_fh)

auto_periods_fh <- vdem %>%
  filter(auto_period_fh ==1) %>%
  group_by(auto_period_fh_id) %>%
  dplyr::summarize(country_name = first (country_name), 
            start_year = first(year), 
            end_year = last(year), 
            FH_before = first(lag_e_fh_com),
            FH_end = last(e_fh_com), 
            decline = first(auto_decline_fh)) %>%
  mutate(country_year = str_c(country_name, start_year, sep = "_"))

anti_join(auto_periods_EDI01, auto_periods_fh, by = "country_year")

auto_countries_fh <- vdem %>%
  filter(auto_period_fh==1) %>%
  group_by(country_name) %>%
  dplyr::summarize(countries = first (country_name)) 

# 247 autocratization episodes in 130 countries after 1972

vdem_auto_fh <- vdem %>%
  select(country_name, year, auto_period_fh_id, auto_decline_fh, auto_period_fh, last_v2x_regime, lag_v2x_regime)

#############################################################################################
#### Freedom House Measure: Decline of 0.15, other rules remain
#############################################################################################

vdem <- vdem_org 

#### Autocratization Periods ####

#### Rescale Freedom House ####
table(vdem$e_fh_cl)
table(vdem$e_fh_pr)

vdem <- vdem %>%
  mutate(e_fh_cl = ifelse(e_fh_cl==7, 1, 
                          ifelse(e_fh_cl==6, 2,
                                 ifelse(e_fh_cl==5, 3,
                                        ifelse(e_fh_cl==4, 4,
                                               ifelse(e_fh_cl==3, 5,
                                                      ifelse(e_fh_cl==2, 6, 
                                                             ifelse(e_fh_cl==1, 7, NA))))))))

vdem <- vdem %>%
  mutate(e_fh_pr = ifelse(e_fh_pr==7, 1, 
                          ifelse(e_fh_pr==6, 2,
                                 ifelse(e_fh_pr==5, 3,
                                        ifelse(e_fh_pr==4, 4,
                                               ifelse(e_fh_pr==3, 5,
                                                      ifelse(e_fh_pr==2, 6, 
                                                             ifelse(e_fh_pr==1, 7, NA))))))))


vdem <- vdem %>%
  mutate(e_fh_com = e_fh_cl + e_fh_pr) 
summary(vdem$e_fh_com)

vdem$e_fh_com <- rescale(vdem$e_fh_com, to = c(0,1)) # rescaled Polity IV Variable to [0,1]

summary(vdem$e_fh_com)

#### Autocratization Periods ####

vdem <- vdem %>%
  select(country_name, year, e_fh_com, v2x_regime) %>%
  filter(year >= 1972 & year != 2019) %>%
  group_by(country_name) %>%
  mutate(e_fh_com_1 = e_fh_com - lag(e_fh_com, 1), 
         start_auto = ifelse(e_fh_com_1<=-0.01, 1, NA), 
         min_1 = lead(e_fh_com_1, 1), 
         min_2 = lead(e_fh_com_1, 2), 
         min_3 = lead(e_fh_com_1, 3), 
         min_4 = lead(e_fh_com_1, 4), 
         auto_end = ifelse(min_1<=-0.01 | min_2<=-0.01 | min_3<=-0.01 | min_4<=-0.01, 0, 1),
         auto_end = ifelse(is.na(auto_end), 0, auto_end),
         auto_end2 = ifelse(e_fh_com_1>=0.02, 1, 0)) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(start_auto_1 = ifelse(lag(start_auto)==1, 1, 0), 
         start_auto_2 = ifelse(lag(start_auto, 2)==1, 1, 0), 
         start_auto_3 = ifelse(lag(start_auto, 3)==1, 1, 0), 
         start_auto_4 = ifelse(lag(start_auto, 4)==1, 1, 0), 
         start_auto_four = coalesce(start_auto_1, start_auto_2, start_auto_3, start_auto_4), 
         start_auto_four = ifelse(is.na(start_auto_four), 0, start_auto_four))  %>%
  select(-c(start_auto_1, start_auto_2, start_auto_3, start_auto_4)) %>%
  select(-c(min_1, min_2, min_3, min_4)) %>%
  fill(auto_end)

vdem <- as.data.frame(vdem)
for (i in 1:nrow(vdem)) {
  vdem[i,][is.na(vdem[i,]) & vdem$auto_end[i-1] == 0 & vdem$start_auto_four[i] == 1 & vdem$start_auto[i-1] ==1 &
             vdem$auto_end2[i] != 1] <- 1 
}

table(vdem$start_auto)


## Autocratization Periods with a total magnitude of 15% in Freedom House  ##
# nessearcy conditions: one year before autocratization started -  the decline at the end of the episode >=0.15 

# first step: create auto_period ID
vdem <- vdem %>%
  mutate(start_auto = ifelse(is.na(start_auto), 0, start_auto)) %>%
  group_by(country_name) %>%
  mutate(group_id = cumsum(start_auto != lag(start_auto, default = FALSE))) # group_id for each autocra_period

vdem <- vdem %>%
  mutate(auto_period_fh_id015 = str_c(country_name, group_id, sep = "_"))

# second step: create dummy in which autocratization was greater than 0.15

vdem <- vdem %>%
  group_by(auto_period_fh_id015) %>%
  mutate(last_e_fh_com = last(e_fh_com), 
         last_v2x_regime = last(v2x_regime))

vdem <- vdem %>%
  ungroup()%>%
  mutate(auto_dum_fh015 = last_e_fh_com - lag(last_e_fh_com)) %>%
  select(-c(group_id)) 


vdem <- vdem %>%
  group_by(auto_period_fh_id015) %>%
  mutate(auto_period_fh015 = ifelse(start_auto==1 & first(auto_dum_fh015) <= -0.15 , 1, 0 )) %>% 
  rename(auto_decline_fh015 = auto_dum_fh015) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_e_fh_com = lag(e_fh_com, 1), 
         lag_v2x_regime = lag(v2x_regime, 1))


# summary statistics #
table(vdem$auto_period_fh015)

auto_periods_fh015 <- vdem %>%
  filter(auto_period_fh015 ==1) %>%
  group_by(auto_period_fh_id015) %>%
  dplyr::summarize(country_name = first (country_name), 
                   start_year = first(year), 
                   end_year = last(year), 
                   FH_before = first(lag_e_fh_com),
                   FH_end = last(e_fh_com), 
                   decline = first(auto_decline_fh015)) %>%
  mutate(country_year = str_c(country_name, start_year, sep = "_"))


auto_countries_fh015 <- vdem %>%
  filter(auto_period_fh015==1) %>%
  group_by(country_name) %>%
  dplyr::summarize(countries = first (country_name)) 

# 247 autocratization episodes in 130 countries after 1972

vdem_auto_fh015 <- vdem %>%
  select(country_name, year, auto_period_fh_id015, auto_decline_fh015, auto_period_fh015, last_v2x_regime, lag_v2x_regime)


#############################################################################################
#### Freedom House Measure: Decline of 0.2, other rules remain
#############################################################################################

vdem <- vdem_org 

#### Autocratization Periods ####

#### Rescale Freedom House ####
table(vdem$e_fh_cl)
table(vdem$e_fh_pr)

vdem <- vdem %>%
  mutate(e_fh_cl = ifelse(e_fh_cl==7, 1, 
                          ifelse(e_fh_cl==6, 2,
                                 ifelse(e_fh_cl==5, 3,
                                        ifelse(e_fh_cl==4, 4,
                                               ifelse(e_fh_cl==3, 5,
                                                      ifelse(e_fh_cl==2, 6, 
                                                             ifelse(e_fh_cl==1, 7, NA))))))))

vdem <- vdem %>%
  mutate(e_fh_pr = ifelse(e_fh_pr==7, 1, 
                          ifelse(e_fh_pr==6, 2,
                                 ifelse(e_fh_pr==5, 3,
                                        ifelse(e_fh_pr==4, 4,
                                               ifelse(e_fh_pr==3, 5,
                                                      ifelse(e_fh_pr==2, 6, 
                                                             ifelse(e_fh_pr==1, 7, NA))))))))


vdem <- vdem %>%
  mutate(e_fh_com = e_fh_cl + e_fh_pr) 
summary(vdem$e_fh_com)

vdem$e_fh_com <- rescale(vdem$e_fh_com, to = c(0,1)) # rescaled Polity IV Variable to [0,1]

summary(vdem$e_fh_com)

#### Autocratization Periods ####

vdem <- vdem %>%
  select(country_name, year, e_fh_com, v2x_regime) %>%
  filter(year >= 1972 & year != 2019) %>%
  group_by(country_name) %>%
  mutate(e_fh_com_1 = e_fh_com - lag(e_fh_com, 1), 
         start_auto = ifelse(e_fh_com_1<=-0.01, 1, NA), 
         min_1 = lead(e_fh_com_1, 1), 
         min_2 = lead(e_fh_com_1, 2), 
         min_3 = lead(e_fh_com_1, 3), 
         min_4 = lead(e_fh_com_1, 4), 
         auto_end = ifelse(min_1<=-0.01 | min_2<=-0.01 | min_3<=-0.01 | min_4<=-0.01, 0, 1),
         auto_end = ifelse(is.na(auto_end), 0, auto_end),
         auto_end2 = ifelse(e_fh_com_1>=0.02, 1, 0)) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(start_auto_1 = ifelse(lag(start_auto)==1, 1, 0), 
         start_auto_2 = ifelse(lag(start_auto, 2)==1, 1, 0), 
         start_auto_3 = ifelse(lag(start_auto, 3)==1, 1, 0), 
         start_auto_4 = ifelse(lag(start_auto, 4)==1, 1, 0), 
         start_auto_four = coalesce(start_auto_1, start_auto_2, start_auto_3, start_auto_4), 
         start_auto_four = ifelse(is.na(start_auto_four), 0, start_auto_four))  %>%
  select(-c(start_auto_1, start_auto_2, start_auto_3, start_auto_4)) %>%
  select(-c(min_1, min_2, min_3, min_4)) %>%
  fill(auto_end)

vdem <- as.data.frame(vdem)
for (i in 1:nrow(vdem)) {
  vdem[i,][is.na(vdem[i,]) & vdem$auto_end[i-1] == 0 & vdem$start_auto_four[i] == 1 & vdem$start_auto[i-1] ==1 &
             vdem$auto_end2[i] != 1] <- 1 
}

table(vdem$start_auto)


## Autocratization Periods with a total magnitude of 20% in Freedom House  ##
# nessearcy conditions: one year before autocratization started -  the decline at the end of the episode >=0.2 

# first step: create auto_period ID
vdem <- vdem %>%
  mutate(start_auto = ifelse(is.na(start_auto), 0, start_auto)) %>%
  group_by(country_name) %>%
  mutate(group_id = cumsum(start_auto != lag(start_auto, default = FALSE))) # group_id for each autocra_period

vdem <- vdem %>%
  mutate(auto_period_fh_id02 = str_c(country_name, group_id, sep = "_"))

# second step: create dummy in which autocratization was greater than 0.2

vdem <- vdem %>%
  group_by(auto_period_fh_id02) %>%
  mutate(last_e_fh_com = last(e_fh_com), 
         last_v2x_regime = last(v2x_regime))

vdem <- vdem %>%
  ungroup()%>%
  mutate(auto_dum_fh02 = last_e_fh_com - lag(last_e_fh_com)) %>%
  select(-c(group_id)) 


vdem <- vdem %>%
  group_by(auto_period_fh_id02) %>%
  mutate(auto_period_fh02 = ifelse(start_auto==1 & first(auto_dum_fh02) <= -0.2 , 1, 0 )) %>% 
  rename(auto_decline_fh02 = auto_dum_fh02) 

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_e_fh_com = lag(e_fh_com, 1), 
         lag_v2x_regime = lag(v2x_regime, 1))


# summary statistics #
table(vdem$auto_period_fh02)

auto_periods_fh02 <- vdem %>%
  filter(auto_period_fh02 ==1) %>%
  group_by(auto_period_fh_id02) %>%
  dplyr::summarize(country_name = first (country_name), 
                   start_year = first(year), 
                   end_year = last(year), 
                   FH_before = first(lag_e_fh_com),
                   FH_end = last(e_fh_com), 
                   decline = first(auto_decline_fh02)) %>%
  mutate(country_year = str_c(country_name, start_year, sep = "_"))


auto_countries_fh02 <- vdem %>%
  filter(auto_period_fh02==1) %>%
  group_by(country_name) %>%
  dplyr::summarize(countries = first (country_name)) 

# 125 autocratization episodes in 76 countries after 1972

vdem_auto_fh02 <- vdem %>%
  select(country_name, year, auto_period_fh_id02, auto_decline_fh02, auto_period_fh02, last_v2x_regime, lag_v2x_regime)


#############################################################################################
#############################################################################################
#############################################################################################

#set you WD#
setwd("")


#### Merging Different Autocratization Definitions ####

vdem <- vdem_org 

#EDI#
vdem_auto_EDI005 <- vdem_auto_EDI005 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_EDI01 <- vdem_auto_EDI01 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_EDI01ci <- vdem_auto_EDI01ci %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_EDI01ci_start005 <- vdem_auto_EDI01ci_start005 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_EDI015ci <- vdem_auto_EDI015ci %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_EDI015ci_start005 <- vdem_auto_EDI015ci_start005 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_EDI02ci <- vdem_auto_EDI02ci %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_EDI02ci_start005 <- vdem_auto_EDI02ci_start005 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))

#FH#
vdem_auto_fh <- vdem_auto_fh %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_fh015 <- vdem_auto_fh015 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_fh02 <- vdem_auto_fh02 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))

#LDI#
vdem_auto_LDI005 <- vdem_auto_LDI005 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_LDI01 <- vdem_auto_LDI01 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_LDI01ci <- vdem_auto_LDI01ci %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_LDI01ci_start005 <- vdem_auto_LDI01ci_start005 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_LDI015ci <- vdem_auto_LDI015ci %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_LDI015ci_start005 <- vdem_auto_LDI015ci_start005 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_LDI02ci <- vdem_auto_LDI02ci %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_LDI02ci_start005 <- vdem_auto_LDI02ci_start005 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))

#Polty IV#
vdem_auto_polity <- vdem_auto_polity %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_polity005 <- vdem_auto_polity005 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_polity01_st005 <- vdem_auto_polity01_st005 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_polity015 <- vdem_auto_polity015 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_polity015_st005 <- vdem_auto_polity015_st005 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_polity02 <- vdem_auto_polity02 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_polity02_st005 <- vdem_auto_polity02_st005 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))

## Join to VDem Dataset ##

vdem <- vdem %>%
  left_join(vdem_auto_EDI01, by = c("country_name", "year")) %>%
  left_join(vdem_auto_EDI01ci, by = c("country_name", "year")) %>%
  left_join(vdem_auto_EDI01ci_start005, by = c("country_name", "year")) %>%
  left_join(vdem_auto_EDI005, by = c("country_name", "year")) %>%
  left_join(vdem_auto_EDI015ci, by = c("country_name", "year")) %>%
  left_join(vdem_auto_EDI015ci_start005, by = c("country_name", "year")) %>%
  left_join(vdem_auto_EDI02ci, by = c("country_name", "year")) %>%
  left_join(vdem_auto_EDI02ci_start005, by = c("country_name", "year")) %>%
  left_join(vdem_auto_LDI01, by = c("country_name", "year")) %>%
  left_join(vdem_auto_LDI01ci, by = c("country_name", "year")) %>%
  left_join(vdem_auto_LDI01ci_start005, by = c("country_name", "year")) %>%
  left_join(vdem_auto_LDI005, by = c("country_name", "year")) %>%
  left_join(vdem_auto_LDI015ci, by = c("country_name", "year")) %>%
  left_join(vdem_auto_LDI015ci_start005, by = c("country_name", "year")) %>%
  left_join(vdem_auto_LDI02ci, by = c("country_name", "year")) %>%
  left_join(vdem_auto_LDI02ci_start005, by = c("country_name", "year")) %>%
  left_join(vdem_auto_polity, by = c("country_name", "year")) %>%
  left_join(vdem_auto_polity01_st005, by = c("country_name", "year")) %>%
  left_join(vdem_auto_polity005, by = c("country_name", "year")) %>%
  left_join(vdem_auto_polity015, by = c("country_name", "year")) %>%
  left_join(vdem_auto_polity015_st005, by = c("country_name", "year")) %>%
  left_join(vdem_auto_polity02, by = c("country_name", "year")) %>%
  left_join(vdem_auto_polity02_st005, by = c("country_name", "year")) %>%
  left_join(vdem_auto_fh, by = c("country_name", "year")) %>%
  left_join(vdem_auto_fh015, by = c("country_name", "year")) %>%
  left_join(vdem_auto_fh02, by = c("country_name", "year")) 

saveRDS(vdem, "VDem_autocratization10.rds")

#### Table A2 ####
# comment, Table A2, but not comphrensive for this table
vdem %>%
  filter(auto_period01==1) %>%
  count()

vdem %>%
  filter(auto_period005==1) %>%
  count()

vdem %>%
  filter(auto_period01ci==1) %>%
  count()

vdem %>%
  filter(auto_period01_libdem==1) %>%
  count()

vdem %>%
  filter(auto_period01_libdem_ci==1) %>%
  count()

vdem %>%
  filter(auto_period005_libdem==1) %>%
  count()

vdem %>%
  filter(auto_period_polity==1) %>%
  count()

vdem %>%
  filter(auto_period_polity005==1) %>%
  count()

vdem %>%
  filter(auto_period_fh==1) %>%
  count()


#### Figure 2 Main Paper ####

vdem <- vdem %>%
  select(country_name, year, starts_with("auto_period"), starts_with("auto_decline")) %>%
  filter(year >= 1900)

vdem <- vdem %>%
  mutate(auto_period_fh = ifelse(year<1972, NA, auto_period_fh), 
         auto_period_fh015 = ifelse(year<1972, NA, auto_period_fh015), 
         auto_period_fh02 = ifelse(year<1972, NA, auto_period_fh02))

vdem_sum_fig <- vdem %>%
  group_by(year) %>%
  dplyr::summarize(num_auto_periodEDI01 = sum(auto_period01, na.rm = TRUE), 
            num_auto_periodEDI01ci = sum(auto_period01ci, na.rm = TRUE),
            num_auto_periodEDI01ci_start005 = sum(auto_period01ci_start005, na.rm = TRUE),
            num_auto_periodEDI005 = sum(auto_period005, na.rm = TRUE), 
            num_auto_periodEDI015ci = sum(auto_period015ci, na.rm = TRUE), 
            num_auto_periodEDI015ci_start005 = sum(auto_period015ci_start005, na.rm = TRUE), 
            num_auto_periodEDI02ci = sum(auto_period02ci, na.rm = TRUE), 
            num_auto_periodEDI02ci_start005 = sum(auto_period02ci_start005, na.rm = TRUE), 
            num_auto_periodLDI01 = sum(auto_period01_libdem, na.rm = TRUE),
            num_auto_periodLDI01ci = sum(auto_period01_libdem_ci, na.rm = TRUE),
            num_auto_periodLDI01ci_start005 = sum(auto_period01_libdem_ci_start005, na.rm = TRUE),
            num_auto_periodLDI005 = sum(auto_period005_libdem, na.rm = TRUE),
            num_auto_periodLDI015ci = sum(auto_period015_libdem_ci, na.rm = TRUE), 
            num_auto_periodLDI015ci_start005 = sum(auto_period015_libdem_ci_start005, na.rm = TRUE), 
            num_auto_periodLDI02ci = sum(auto_period02_libdem_ci, na.rm = TRUE), 
            num_auto_periodLDI02ci_start005 = sum(auto_period02_libdem_ci_start005, na.rm = TRUE), 
            num_auto_period_polity = sum(auto_period_polity, na.rm = TRUE),
            num_auto_period_polity01_start005 = sum(auto_period_polity01_st005, na.rm = TRUE),
            num_auto_period_polity005 = sum(auto_period_polity005, na.rm = TRUE),
            num_auto_period_polity015 = sum(auto_period_polity015, na.rm = TRUE),
            num_auto_period_polity015_start005 = sum(auto_period_polity015_st005, na.rm = TRUE),
            num_auto_period_polity02 = sum(auto_period_polity02, na.rm = TRUE),
            num_auto_period_polity02_start005 = sum(auto_period_polity02_st005, na.rm = TRUE),
            num_auto_period_FH = sum(auto_period_fh, na.rm = TRUE), 
            num_auto_period_FH015 = sum(auto_period_fh015, na.rm = TRUE), 
            num_auto_period_FH02 = sum(auto_period_fh02, na.rm = TRUE)) %>%
  mutate(num_auto_period_FH = ifelse(year < 1972, NA, num_auto_period_FH), 
         num_auto_period_FH = ifelse(year ==2019, NA, num_auto_period_FH), 
         num_auto_period_FH015 = ifelse(year < 1972, NA, num_auto_period_FH015), 
         num_auto_period_FH015 = ifelse(year ==2019, NA, num_auto_period_FH015), 
         num_auto_period_FH02 = ifelse(year < 1972, NA, num_auto_period_FH02), 
         num_auto_period_FH02 = ifelse(year ==2019, NA, num_auto_period_FH02), 
         num_auto_period_polity = ifelse(year ==2019 , NA, num_auto_period_polity), 
         num_auto_period_polity01_start005 = ifelse(year ==2019 , NA, num_auto_period_polity01_start005), 
         num_auto_period_polity005 = ifelse(year ==2019 , NA, num_auto_period_polity005), 
         num_auto_period_polity015 = ifelse(year ==2019 , NA, num_auto_period_polity015), 
         num_auto_period_polity015_start005 = ifelse(year ==2019 , NA, num_auto_period_polity015_start005), 
         num_auto_period_polity02 = ifelse(year ==2019 , NA, num_auto_period_polity02), 
         num_auto_period_polity02_start005 = ifelse(year ==2019 , NA, num_auto_period_polity02_start005))



## Figure 2A##

vdem_sum_figF1 <- vdem_sum_fig %>%
  select(num_auto_periodEDI01, num_auto_periodEDI01ci, num_auto_periodEDI005, num_auto_periodEDI015ci, 
         num_auto_periodEDI02ci, year) %>%
  pivot_longer( cols = starts_with("num"),names_to = "measure", values_to = "measure_count") %>%
  mutate(measure = case_when(measure == "num_auto_periodEDI01" ~ "10%", 
                             measure == "num_auto_periodEDI01ci" ~ "10% CI", 
                             measure == "num_auto_periodEDI005" ~ "5% CI", 
                             measure == "num_auto_periodEDI015ci" ~ "15% CI", 
                             measure == "num_auto_periodEDI02ci" ~ "20% CI"))

F1 <- ggplot(data = vdem_sum_figF1, aes(x = year, y = measure_count, color = measure)) +
  geom_line(aes(linetype = measure)) +
  theme_pubr() +
  ylim(0, 45) +
  labs( 
    color = "", linetype ="",
    title = "EDI (Start Episode = 0.01)",
    x = "Year", 
    y = "Number of countries") 

## Figure 2B
vdem_sum_figF2 <- vdem_sum_fig %>%
  select(num_auto_periodLDI01, num_auto_periodLDI01ci, num_auto_periodLDI005, num_auto_periodLDI015ci, 
         num_auto_periodLDI02ci, year) %>%
  pivot_longer( cols = starts_with("num"),names_to = "measure", values_to = "measure_count") %>%
  mutate(measure = case_when(measure == "num_auto_periodLDI01" ~ "10%", 
                             measure == "num_auto_periodLDI01ci" ~ "10% CI", 
                             measure == "num_auto_periodLDI005" ~ "5% CI", 
                             measure == "num_auto_periodLDI015ci" ~ "15% CI", 
                             measure == "num_auto_periodLDI02ci" ~ "20% CI"))

F2<- ggplot(data = vdem_sum_figF2, aes(x = year, y = measure_count, color = measure)) +
  geom_line(aes(linetype = measure)) +
  theme_pubr() +
  ylim(0, 45) +
  labs( 
    color = "", linetype ="",
    title = "LDI (Start Episode = 0.01)",
    x = "Year", 
    y = "Number of countries") 

## Figure 2C
vdem_sum_figF3 <- vdem_sum_fig %>%
  select(num_auto_period_polity, num_auto_period_polity005, num_auto_period_polity015, num_auto_period_polity02, 
         year) %>%
  pivot_longer( cols = starts_with("num"),names_to = "measure", values_to = "measure_count") %>%
  mutate(measure = case_when(measure == "num_auto_period_polity" ~ "10%", 
                             measure == "num_auto_period_polity005" ~ "5%", 
                             measure == "num_auto_period_polity015" ~ "15%", 
                             measure == "num_auto_period_polity02" ~ "20%"))

F3<- ggplot(data = vdem_sum_figF3, aes(x = year, y = measure_count, color = measure)) +
  geom_line(aes(linetype = measure)) +
  theme_pubr() +
  ylim(0, 45) +
  labs( 
    color = "", linetype ="",
    title = "Polity IV (Start Episode = 0.01)",
    x = "Year", 
    y = "Number of countries") 

## Figure 2D
vdem_sum_figF4 <- vdem_sum_fig %>%
  select(num_auto_period_FH, num_auto_period_FH015, num_auto_period_FH02, 
         year) %>%
  pivot_longer( cols = starts_with("num"),names_to = "measure", values_to = "measure_count") %>%
  mutate(measure = case_when(measure == "num_auto_period_FH" ~ "10%", 
                             measure == "num_auto_period_FH015" ~ "15%", 
                             measure == "num_auto_period_FH02" ~ "20%"))

F4 <- ggplot(data = vdem_sum_figF4, aes(x = year, y = measure_count, color = measure)) +
  geom_line(aes(linetype = measure)) +
  theme_pubr() +
  ylim(0, 45) +
  labs( 
    color = "", linetype ="",
    title = "Freedom House (Start Episode = 0.01)",
    x = "Year", 
    y = "Number of countries") 

## Figure 2E
vdem_sum_figF5 <- vdem_sum_fig %>%
  select(num_auto_periodEDI01ci_start005, num_auto_periodEDI015ci_start005, num_auto_periodEDI02ci_start005, year) %>%
  pivot_longer( cols = starts_with("num"),names_to = "measure", values_to = "measure_count") %>%
  mutate(measure = case_when(measure == "num_auto_periodEDI01ci_start005" ~ "10% CI", 
                             measure == "num_auto_periodEDI015ci_start005" ~ "15% CI", 
                             measure == "num_auto_periodEDI02ci_start005" ~ "20% CI"))

F5 <- ggplot(data = vdem_sum_figF5, aes(x = year, y = measure_count, color = measure)) +
  geom_line(aes(linetype = measure)) +
  theme_pubr() +
  ylim(0, 20) +
  labs( 
    color = "", linetype ="",
    title = "EDI (Start Episode = 0.05)",
    x = "Year", 
    y = "Number of countries") 


## Figure 2F
vdem_sum_figF6<- vdem_sum_fig %>%
  select(num_auto_periodLDI01ci_start005, num_auto_periodLDI015ci_start005, num_auto_periodLDI02ci_start005, year) %>%
  pivot_longer( cols = starts_with("num"),names_to = "measure", values_to = "measure_count") %>%
  mutate(measure = case_when(measure == "num_auto_periodLDI01ci_start005" ~ "10% CI", 
                             measure == "num_auto_periodLDI015ci_start005" ~ "15% CI", 
                             measure == "num_auto_periodLDI02ci_start005" ~ "20% CI"))

F6 <- ggplot(data = vdem_sum_figF6, aes(x = year, y = measure_count, color = measure)) +
  geom_line(aes(linetype = measure)) +
  theme_pubr() +
  ylim(0, 20) +
  labs( 
    color = "", linetype ="",
    title = "LDI (Start Episode = 0.05)",
    x = "Year", 
    y = "Number of countries") 

## Figure 2F
vdem_sum_figF7<- vdem_sum_fig %>%
  select(num_auto_period_polity01_start005, num_auto_period_polity015_start005, num_auto_period_polity02_start005, year) %>%
  pivot_longer( cols = starts_with("num"),names_to = "measure", values_to = "measure_count") %>%
  mutate(measure = case_when(measure == "num_auto_period_polity01_start005" ~ "10%", 
                             measure == "num_auto_period_polity015_start005" ~ "15%", 
                             measure == "num_auto_period_polity02_start005" ~ "20%"))

F7 <- ggplot(data = vdem_sum_figF7, aes(x = year, y = measure_count, color = measure)) +
  geom_line(aes(linetype = measure)) +
  theme_pubr() +
  ylim(0, 20) +
  labs( 
    color = "", linetype ="",
    title = "Polity IV (Start Episode = 0.05)",
    x = "Year", 
    y = "Number of countries") 


Figures2paper <- ggarrange(F1, F2, F3, F4, F5, F6, F7, 
          ncol = 4, nrow = 2)

ggsave("outputs/Figure2.png",dpi = 1200, width = 40, height = 25, units = "cm")
ggsave("outputs/Figure2.pdf",dpi = 1200, width = 40, height = 25, units = "cm")


###########################################################################################
###########################################################################################

#### Preparation Shiny Web App ####

vdem <- vdem_org 

#EDI#
vdem_auto_EDI005 <- vdem_auto_EDI005 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_EDI01 <- vdem_auto_EDI01 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_EDI01ci <- vdem_auto_EDI01ci %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_EDI01ci_start005 <- vdem_auto_EDI01ci_start005 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_EDI015ci <- vdem_auto_EDI015ci %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_EDI015ci_start005 <- vdem_auto_EDI015ci_start005 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_EDI02ci <- vdem_auto_EDI02ci %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_EDI02ci_start005 <- vdem_auto_EDI02ci_start005 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))

#FH#
vdem_auto_fh <- vdem_auto_fh %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_fh015 <- vdem_auto_fh015 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_fh02 <- vdem_auto_fh02 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))

#LDI#
vdem_auto_LDI005 <- vdem_auto_LDI005 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_LDI01 <- vdem_auto_LDI01 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_LDI01ci <- vdem_auto_LDI01ci %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_LDI01ci_start005 <- vdem_auto_LDI01ci_start005 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_LDI015ci <- vdem_auto_LDI015ci %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_LDI015ci_start005 <- vdem_auto_LDI015ci_start005 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_LDI02ci <- vdem_auto_LDI02ci %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_LDI02ci_start005 <- vdem_auto_LDI02ci_start005 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))

#Polty IV#
vdem_auto_polity <- vdem_auto_polity %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_polity005 <- vdem_auto_polity005 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_polity01_st005 <- vdem_auto_polity01_st005 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_polity015 <- vdem_auto_polity015 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_polity015_st005 <- vdem_auto_polity015_st005 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_polity02 <- vdem_auto_polity02 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))
vdem_auto_polity02_st005 <- vdem_auto_polity02_st005 %>%
  select(-c(lag_v2x_regime, last_v2x_regime))

## Join to VDem Dataset ##

vdem <- vdem %>%
  left_join(vdem_auto_EDI01, by = c("country_name", "year")) %>%
  left_join(vdem_auto_EDI01ci, by = c("country_name", "year")) %>%
  left_join(vdem_auto_EDI01ci_start005, by = c("country_name", "year")) %>%
  left_join(vdem_auto_EDI005, by = c("country_name", "year")) %>%
  left_join(vdem_auto_EDI015ci, by = c("country_name", "year")) %>%
  left_join(vdem_auto_EDI015ci_start005, by = c("country_name", "year")) %>%
  left_join(vdem_auto_EDI02ci, by = c("country_name", "year")) %>%
  left_join(vdem_auto_EDI02ci_start005, by = c("country_name", "year")) %>%
  left_join(vdem_auto_LDI01, by = c("country_name", "year")) %>%
  left_join(vdem_auto_LDI01ci, by = c("country_name", "year")) %>%
  left_join(vdem_auto_LDI01ci_start005, by = c("country_name", "year")) %>%
  left_join(vdem_auto_LDI005, by = c("country_name", "year")) %>%
  left_join(vdem_auto_LDI015ci, by = c("country_name", "year")) %>%
  left_join(vdem_auto_LDI015ci_start005, by = c("country_name", "year")) %>%
  left_join(vdem_auto_LDI02ci, by = c("country_name", "year")) %>%
  left_join(vdem_auto_LDI02ci_start005, by = c("country_name", "year")) %>%
  left_join(vdem_auto_polity, by = c("country_name", "year")) %>%
  left_join(vdem_auto_polity01_st005, by = c("country_name", "year")) %>%
  left_join(vdem_auto_polity005, by = c("country_name", "year")) %>%
  left_join(vdem_auto_polity015, by = c("country_name", "year")) %>%
  left_join(vdem_auto_polity015_st005, by = c("country_name", "year")) %>%
  left_join(vdem_auto_polity02, by = c("country_name", "year")) %>%
  left_join(vdem_auto_polity02_st005, by = c("country_name", "year")) %>%
  left_join(vdem_auto_fh, by = c("country_name", "year")) %>%
  left_join(vdem_auto_fh015, by = c("country_name", "year")) %>%
  left_join(vdem_auto_fh02, by = c("country_name", "year")) 

saveRDS(vdem, "VDem_autocratization10.rds")
write.csv(vdem, "VDem_autocratization10.csv")


vdem <- vdem %>%
  select(country_name, year, starts_with("auto_period"), starts_with("auto_decline")) %>%
  filter(year >= 1900)

vdem <- vdem %>%
  mutate(auto_period_fh = ifelse(year<1972, NA, auto_period_fh), 
         auto_period_fh015 = ifelse(year<1972, NA, auto_period_fh015), 
         auto_period_fh02 = ifelse(year<1972, NA, auto_period_fh02))

vdem_sum_fig <- vdem %>%
  group_by(year) %>%
  dplyr::summarize(num_auto_periodEDI01 = sum(auto_period01, na.rm = TRUE), 
                   num_auto_periodEDI01ci = sum(auto_period01ci, na.rm = TRUE),
                   num_auto_periodEDI01ci_start005 = sum(auto_period01ci_start005, na.rm = TRUE),
                   num_auto_periodEDI005 = sum(auto_period005, na.rm = TRUE), 
                   num_auto_periodEDI015ci = sum(auto_period015ci, na.rm = TRUE), 
                   num_auto_periodEDI015ci_start005 = sum(auto_period015ci_start005, na.rm = TRUE), 
                   num_auto_periodEDI02ci = sum(auto_period02ci, na.rm = TRUE), 
                   num_auto_periodEDI02ci_start005 = sum(auto_period02ci_start005, na.rm = TRUE), 
                   num_auto_periodLDI01 = sum(auto_period01_libdem, na.rm = TRUE),
                   num_auto_periodLDI01ci = sum(auto_period01_libdem_ci, na.rm = TRUE),
                   num_auto_periodLDI01ci_start005 = sum(auto_period01_libdem_ci_start005, na.rm = TRUE),
                   num_auto_periodLDI005 = sum(auto_period005_libdem, na.rm = TRUE),
                   num_auto_periodLDI015ci = sum(auto_period015_libdem_ci, na.rm = TRUE), 
                   num_auto_periodLDI015ci_start005 = sum(auto_period015_libdem_ci_start005, na.rm = TRUE), 
                   num_auto_periodLDI02ci = sum(auto_period02_libdem_ci, na.rm = TRUE), 
                   num_auto_periodLDI02ci_start005 = sum(auto_period02_libdem_ci_start005, na.rm = TRUE), 
                   num_auto_period_polity = sum(auto_period_polity, na.rm = TRUE),
                   num_auto_period_polity01_start005 = sum(auto_period_polity01_st005, na.rm = TRUE),
                   num_auto_period_polity005 = sum(auto_period_polity005, na.rm = TRUE),
                   num_auto_period_polity015 = sum(auto_period_polity015, na.rm = TRUE),
                   num_auto_period_polity015_start005 = sum(auto_period_polity015_st005, na.rm = TRUE),
                   num_auto_period_polity02 = sum(auto_period_polity02, na.rm = TRUE),
                   num_auto_period_polity02_start005 = sum(auto_period_polity02_st005, na.rm = TRUE),
                   num_auto_period_FH = sum(auto_period_fh, na.rm = TRUE), 
                   num_auto_period_FH015 = sum(auto_period_fh015, na.rm = TRUE), 
                   num_auto_period_FH02 = sum(auto_period_fh02, na.rm = TRUE)) %>%
  mutate(num_auto_period_FH = ifelse(year < 1972, NA, num_auto_period_FH), 
         num_auto_period_FH = ifelse(year ==2019, NA, num_auto_period_FH), 
         num_auto_period_FH015 = ifelse(year < 1972, NA, num_auto_period_FH015), 
         num_auto_period_FH015 = ifelse(year ==2019, NA, num_auto_period_FH015), 
         num_auto_period_FH02 = ifelse(year < 1972, NA, num_auto_period_FH02), 
         num_auto_period_FH02 = ifelse(year ==2019, NA, num_auto_period_FH02), 
         num_auto_period_polity = ifelse(year ==2019 , NA, num_auto_period_polity), 
         num_auto_period_polity01_start005 = ifelse(year ==2019 , NA, num_auto_period_polity01_start005), 
         num_auto_period_polity005 = ifelse(year ==2019 , NA, num_auto_period_polity005), 
         num_auto_period_polity015 = ifelse(year ==2019 , NA, num_auto_period_polity015), 
         num_auto_period_polity015_start005 = ifelse(year ==2019 , NA, num_auto_period_polity015_start005), 
         num_auto_period_polity02 = ifelse(year ==2019 , NA, num_auto_period_polity02), 
         num_auto_period_polity02_start005 = ifelse(year ==2019 , NA, num_auto_period_polity02_start005)) %>%
  rename("EDI 0.1" = num_auto_periodEDI01, 
         "EDI 0.1 + CI" = num_auto_periodEDI01ci, 
         "EDI 0.05 + CI" = num_auto_periodEDI005, 
         "EDI 0.15 + CI" = num_auto_periodEDI015ci, 
         "EDI 0.2 + CI" = num_auto_periodEDI02ci, 
         "EDI 0.1 + CI (S=0.05)" = num_auto_periodEDI01ci_start005,
         "EDI 0.15 + CI (S=0.05)" = num_auto_periodEDI015ci_start005, 
         "EDI 0.2 + CI (S=0.05)" = num_auto_periodEDI02ci_start005, 
         "LDI 0.1" = num_auto_periodLDI01, 
         "LDI 0.1 + CI" = num_auto_periodLDI01ci, 
         "LDI 0.05 + CI" = num_auto_periodLDI005, 
         "LDI 0.15 + CI" = num_auto_periodLDI015ci, 
         "LDI 0.2 + CI" = num_auto_periodLDI02ci, 
         "LDI 0.1 + CI (S=0.05)" = num_auto_periodLDI01ci_start005, 
         "LDI 0.15 + CI (S=0.05)" = num_auto_periodLDI015ci_start005, 
         "LDI 0.2 + CI (S=0.05)" = num_auto_periodLDI02ci_start005, 
         "Polity 0.1" = num_auto_period_polity, 
         "Polity 0.05" = num_auto_period_polity005,
         "Polity 0.15" = num_auto_period_polity015, 
         "Polity 0.2" = num_auto_period_polity02, 
         "Polity 0.1 (S=0.05)" = num_auto_period_polity01_start005, 
         "Polity 0.15 (S=0.05)" = num_auto_period_polity015_start005, 
         "Polity 0.2 (S=0.05)" = num_auto_period_polity02_start005, 
         "Freedom House 0.1" = num_auto_period_FH, 
         "Freedom House 0.15" = num_auto_period_FH015, 
         "Freedom House 0.2" = num_auto_period_FH02)
 
vdem_sub_fig_shiny <- vdem_sum_fig %>%
  pivot_longer(-year, names_to = "auto_def", values_to = "auto_num")

saveRDS(vdem_sub_fig_shiny, "data/vdem_sub_fig_shiny10.rds")

########################################################################################################
########################################################################################################

#### Correlation Table ####

vdem_corr001 <- vdem %>%
  select(auto_period01, auto_period01ci, auto_period005, auto_period015ci, auto_period02ci, 
         auto_period01_libdem, auto_period01_libdem_ci, auto_period005_libdem, auto_period015_libdem_ci, 
         auto_period02_libdem_ci, auto_period_polity, auto_period_polity005, auto_period_polity015, 
         auto_period_polity02, auto_period_fh, auto_period_fh015, auto_period_fh02, year) %>%
  mutate(auto_period_polity = ifelse(year ==2019, NA, auto_period_polity), 
         auto_period_polity015 = ifelse(year ==2019 , NA, auto_period_polity015),
         auto_period_polity02 = ifelse(year ==2019 , NA, auto_period_polity02), 
         auto_period_fh = ifelse(year < 1972, NA, auto_period_fh), 
         auto_period_fh = ifelse(year ==2019, NA, auto_period_fh), 
         auto_period_fh015 = ifelse(year < 1972, NA, auto_period_fh015), 
         auto_period_fh015 = ifelse(year ==2019, NA, auto_period_fh015), 
         auto_period_fh02 = ifelse(year < 1972, NA, auto_period_fh02), 
         auto_period_fh02 = ifelse(year ==2019, NA, auto_period_fh02)) %>%
  select(-c(year)) %>%
  rename("EDI 0.1" = auto_period01, 
         "EDI 0.1 + CI" = auto_period01ci, 
         "EDI 0.05 + CI" = auto_period005, 
         "EDI 0.15 + CI" = auto_period015ci, 
         "EDI 0.2 + CI" = auto_period02ci, 
         "LDI 0.1" = auto_period01_libdem, 
         "LDI 0.1 + CI" = auto_period01_libdem_ci, 
         "LDI 0.05 + CI" = auto_period005_libdem, 
         "LDI 0.15 + CI" = auto_period015_libdem_ci, 
         "LDI 0.2 + CI" = auto_period02_libdem_ci, 
         "Polity IV 0.1" = auto_period_polity,
         "Polity IV 0.05" = auto_period_polity005,
         "Polity IV 0.15" = auto_period_polity015,
         "Polity IV 0.2" = auto_period_polity02,
         "FH 0.1" = auto_period_fh, 
         "FH 0.15" = auto_period_fh015,
         "FH 0.2" = auto_period_fh02)

res <- rcorr(as.matrix(vdem_corr001), type = "pearson")

cor_plot <- corrplot.mixed(res$r, lower.col = "black", number.cex = .7,
               tl.pos = "lt", tl.col="black", upper = "square")

cor_plot
dev.copy(pdf,'outputs/Figure_C1.pdf', width = 10, height =10)
dev.off()


vdem_corr005 <- vdem %>%
  select(auto_period01ci_start005, auto_period015ci_start005, auto_period02ci_start005, 
         auto_period01_libdem_ci_start005, auto_period015_libdem_ci_start005, 
         auto_period02_libdem_ci_start005, auto_period_polity01_st005, auto_period_polity015_st005, 
         auto_period_polity02_st005,year) %>%
  mutate(auto_period_polity01_st005 = ifelse(year ==2019, NA, auto_period_polity01_st005), 
         auto_period_polity015_st005 = ifelse(year ==2019 , NA, auto_period_polity015_st005),
         auto_period_polity02_st005 = ifelse(year ==2019 , NA, auto_period_polity02_st005)) %>%
  select(-c(year)) %>%
  rename("EDI 0.1 + CI" = auto_period01ci_start005, 
         "EDI 0.15 + CI" = auto_period015ci_start005, 
         "EDI 0.2 + CI" = auto_period02ci_start005, 
         "LDI 0.1 + CI" = auto_period01_libdem_ci_start005, 
         "LDI 0.15 + CI" = auto_period015_libdem_ci_start005, 
         "LDI 0.2 + CI" = auto_period02_libdem_ci_start005, 
         "Polity IV 0.1" = auto_period_polity01_st005,
         "Polity IV 0.15" = auto_period_polity015_st005,
         "Polity IV 0.2" = auto_period_polity02_st005)

res <- rcorr(as.matrix(vdem_corr005), type = "pearson")

cor_plot <- corrplot.mixed(res$r, lower.col = "black", number.cex = .7,
                           tl.pos = "lt", tl.col="black", upper = "square")

cor_plot
dev.copy(pdf,'outputs/Figure_C2.pdf', width = 10, height =10)
dev.off()

#### Appendix C: Correlation Plots Subsets of Data ####

#### Figure XY (additional figure, not used in paper) ####

vdem_corr001_pre1946 <- vdem %>%
  filter(year >= 1900 & year < 1946) %>%
  select(auto_period01, auto_period01ci, auto_period005, auto_period015ci, auto_period02ci, 
         auto_period01_libdem, auto_period01_libdem_ci, auto_period005_libdem, auto_period015_libdem_ci, 
         auto_period02_libdem_ci, auto_period_polity, auto_period_polity005, auto_period_polity015, 
         auto_period_polity02, year) %>%
  select(-c(year)) %>%
  rename("EDI 0.1" = auto_period01, 
         "EDI 0.1 + CI" = auto_period01ci, 
         "EDI 0.05 + CI" = auto_period005, 
         "EDI 0.15 + CI" = auto_period015ci, 
         "EDI 0.2 + CI" = auto_period02ci, 
         "LDI 0.1" = auto_period01_libdem, 
         "LDI 0.1 + CI" = auto_period01_libdem_ci, 
         "LDI 0.05 + CI" = auto_period005_libdem, 
         "LDI 0.15 + CI" = auto_period015_libdem_ci, 
         "LDI 0.2 + CI" = auto_period02_libdem_ci, 
         "Polity IV 0.1" = auto_period_polity,
         "Polity IV 0.05" = auto_period_polity005,
         "Polity IV 0.15" = auto_period_polity015,
         "Polity IV 0.2" = auto_period_polity02) %>%
  ungroup() %>%
  select(-c(country_name))

res <- rcorr(as.matrix(vdem_corr001_pre1946), type = "pearson")

cor_plot <- corrplot.mixed(res$r, lower.col = "black", number.cex = .7,
                           tl.pos = "lt", tl.col="black", upper = "square")

cor_plot
dev.copy(pdf,'outputs/C1_pre1946.pdf', width = 10, height =10)
dev.off()


#### Figure Figure XY (additional figure, not used in paper) ####

vdem_corr001_post1946 <- vdem %>%
  filter(year >= 1946) %>%
  select(auto_period01, auto_period01ci, auto_period005, auto_period015ci, auto_period02ci, 
         auto_period01_libdem, auto_period01_libdem_ci, auto_period005_libdem, auto_period015_libdem_ci, 
         auto_period02_libdem_ci, auto_period_polity, auto_period_polity005, auto_period_polity015, 
         auto_period_polity02,auto_period_fh, auto_period_fh015, auto_period_fh02, year) %>%
  select(-c(year)) %>%
  rename("EDI 0.1" = auto_period01, 
         "EDI 0.1 + CI" = auto_period01ci, 
         "EDI 0.05 + CI" = auto_period005, 
         "EDI 0.15 + CI" = auto_period015ci, 
         "EDI 0.2 + CI" = auto_period02ci, 
         "LDI 0.1" = auto_period01_libdem, 
         "LDI 0.1 + CI" = auto_period01_libdem_ci, 
         "LDI 0.05 + CI" = auto_period005_libdem, 
         "LDI 0.15 + CI" = auto_period015_libdem_ci, 
         "LDI 0.2 + CI" = auto_period02_libdem_ci, 
         "Polity IV 0.1" = auto_period_polity,
         "Polity IV 0.05" = auto_period_polity005,
         "Polity IV 0.15" = auto_period_polity015,
         "Polity IV 0.2" = auto_period_polity02, 
         "FH 0.1" = auto_period_fh, 
         "FH 0.15" = auto_period_fh015,
         "FH 0.2" = auto_period_fh02) %>%
  ungroup() %>%
  select(-c(country_name))

res <- rcorr(as.matrix(vdem_corr001_post1946), type = "pearson")

cor_plot <- corrplot.mixed(res$r, lower.col = "black", number.cex = .7,
                           tl.pos = "lt", tl.col="black", upper = "square")

cor_plot
dev.copy(pdf,'outputs/C2_post1946.pdf', width = 10, height =10)
dev.off()


#### Figure Figure XY (additional figure, not used in paper) Latin America ####

vdem_corr001_LatinAmerica <- vdem %>%
  filter(e_regionpol_6C == 2)  %>%
  select(auto_period01, auto_period01ci, auto_period005, auto_period015ci, auto_period02ci, 
         auto_period01_libdem, auto_period01_libdem_ci, auto_period005_libdem, auto_period015_libdem_ci, 
         auto_period02_libdem_ci, auto_period_polity, auto_period_polity005, auto_period_polity015, 
         auto_period_polity02,auto_period_fh, auto_period_fh015, auto_period_fh02, year) %>%
  select(-c(year)) %>%
  rename("EDI 0.1" = auto_period01, 
         "EDI 0.1 + CI" = auto_period01ci, 
         "EDI 0.05 + CI" = auto_period005, 
         "EDI 0.15 + CI" = auto_period015ci, 
         "EDI 0.2 + CI" = auto_period02ci, 
         "LDI 0.1" = auto_period01_libdem, 
         "LDI 0.1 + CI" = auto_period01_libdem_ci, 
         "LDI 0.05 + CI" = auto_period005_libdem, 
         "LDI 0.15 + CI" = auto_period015_libdem_ci, 
         "LDI 0.2 + CI" = auto_period02_libdem_ci, 
         "Polity IV 0.1" = auto_period_polity,
         "Polity IV 0.05" = auto_period_polity005,
         "Polity IV 0.15" = auto_period_polity015,
         "Polity IV 0.2" = auto_period_polity02, 
         "FH 0.1" = auto_period_fh, 
         "FH 0.15" = auto_period_fh015,
         "FH 0.2" = auto_period_fh02) %>%
  ungroup() %>%
  select(-c(country_name))

res <- rcorr(as.matrix(vdem_corr001_LatinAmerica), type = "pearson")

cor_plot <- corrplot.mixed(res$r, lower.col = "black", number.cex = .7,
                           tl.pos = "lt", tl.col="black", upper = "square")

cor_plot
dev.copy(pdf,'outputs/C3_LatinAmerica.pdf', width = 10, height =10)
dev.off()


#### Figure Figure XY (additional figure, not used in paper) Eastern Europe and Central Asia ####

vdem_corr001_EasternEurope <- vdem %>%
  filter(e_regionpol_6C == 1)  %>%
  select(auto_period01, auto_period01ci, auto_period005, auto_period015ci, auto_period02ci, 
         auto_period01_libdem, auto_period01_libdem_ci, auto_period005_libdem, auto_period015_libdem_ci, 
         auto_period02_libdem_ci, auto_period_polity, auto_period_polity005, auto_period_polity015, 
         auto_period_polity02,auto_period_fh, auto_period_fh015, auto_period_fh02, year) %>%
  select(-c(year)) %>%
  rename("EDI 0.1" = auto_period01, 
         "EDI 0.1 + CI" = auto_period01ci, 
         "EDI 0.05 + CI" = auto_period005, 
         "EDI 0.15 + CI" = auto_period015ci, 
         "EDI 0.2 + CI" = auto_period02ci, 
         "LDI 0.1" = auto_period01_libdem, 
         "LDI 0.1 + CI" = auto_period01_libdem_ci, 
         "LDI 0.05 + CI" = auto_period005_libdem, 
         "LDI 0.15 + CI" = auto_period015_libdem_ci, 
         "LDI 0.2 + CI" = auto_period02_libdem_ci, 
         "Polity IV 0.1" = auto_period_polity,
         "Polity IV 0.05" = auto_period_polity005,
         "Polity IV 0.15" = auto_period_polity015,
         "Polity IV 0.2" = auto_period_polity02, 
         "FH 0.1" = auto_period_fh, 
         "FH 0.15" = auto_period_fh015,
         "FH 0.2" = auto_period_fh02) %>%
  ungroup() %>%
  select(-c(country_name))

res <- rcorr(as.matrix(vdem_corr001_EasternEurope), type = "pearson")

cor_plot <- corrplot.mixed(res$r, lower.col = "black", number.cex = .7,
                           tl.pos = "lt", tl.col="black", upper = "square")

cor_plot
dev.copy(pdf,'outputs/C4_EasternEurope.pdf', width = 10, height =10)
dev.off()

#### Figure Figure XY (additional figure, not used in paper) MENA ####

vdem_corr001_MENA<- vdem %>%
  filter(e_regionpol_6C == 3)  %>%
  select(auto_period01, auto_period01ci, auto_period005, auto_period015ci, auto_period02ci, 
         auto_period01_libdem, auto_period01_libdem_ci, auto_period005_libdem, auto_period015_libdem_ci, 
         auto_period02_libdem_ci, auto_period_polity, auto_period_polity005, auto_period_polity015, 
         auto_period_polity02,auto_period_fh, auto_period_fh015, auto_period_fh02, year) %>%
  select(-c(year)) %>%
  rename("EDI 0.1" = auto_period01, 
         "EDI 0.1 + CI" = auto_period01ci, 
         "EDI 0.05 + CI" = auto_period005, 
         "EDI 0.15 + CI" = auto_period015ci, 
         "EDI 0.2 + CI" = auto_period02ci, 
         "LDI 0.1" = auto_period01_libdem, 
         "LDI 0.1 + CI" = auto_period01_libdem_ci, 
         "LDI 0.05 + CI" = auto_period005_libdem, 
         "LDI 0.15 + CI" = auto_period015_libdem_ci, 
         "LDI 0.2 + CI" = auto_period02_libdem_ci, 
         "Polity IV 0.1" = auto_period_polity,
         "Polity IV 0.05" = auto_period_polity005,
         "Polity IV 0.15" = auto_period_polity015,
         "Polity IV 0.2" = auto_period_polity02, 
         "FH 0.1" = auto_period_fh, 
         "FH 0.15" = auto_period_fh015,
         "FH 0.2" = auto_period_fh02) %>%
  ungroup() %>%
  select(-c(country_name))

res <- rcorr(as.matrix(vdem_corr001_MENA), type = "pearson")

cor_plot <- corrplot.mixed(res$r, lower.col = "black", number.cex = .7,
                           tl.pos = "lt", tl.col="black", upper = "square")

cor_plot
dev.copy(pdf,'outputs/C5_MENA.pdf', width = 10, height =10)
dev.off()

#### Figure Figure XY (additional figure, not used in paper) SSA ####

vdem_corr001_SSA <- vdem %>%
  filter(e_regionpol_6C == 4)  %>%
  select(auto_period01, auto_period01ci, auto_period005, auto_period015ci, auto_period02ci, 
         auto_period01_libdem, auto_period01_libdem_ci, auto_period005_libdem, auto_period015_libdem_ci, 
         auto_period02_libdem_ci, auto_period_polity, auto_period_polity005, auto_period_polity015, 
         auto_period_polity02,auto_period_fh, auto_period_fh015, auto_period_fh02, year) %>%
  select(-c(year)) %>%
  rename("EDI 0.1" = auto_period01, 
         "EDI 0.1 + CI" = auto_period01ci, 
         "EDI 0.05 + CI" = auto_period005, 
         "EDI 0.15 + CI" = auto_period015ci, 
         "EDI 0.2 + CI" = auto_period02ci, 
         "LDI 0.1" = auto_period01_libdem, 
         "LDI 0.1 + CI" = auto_period01_libdem_ci, 
         "LDI 0.05 + CI" = auto_period005_libdem, 
         "LDI 0.15 + CI" = auto_period015_libdem_ci, 
         "LDI 0.2 + CI" = auto_period02_libdem_ci, 
         "Polity IV 0.1" = auto_period_polity,
         "Polity IV 0.05" = auto_period_polity005,
         "Polity IV 0.15" = auto_period_polity015,
         "Polity IV 0.2" = auto_period_polity02, 
         "FH 0.1" = auto_period_fh, 
         "FH 0.15" = auto_period_fh015,
         "FH 0.2" = auto_period_fh02) %>%
  ungroup() %>%
  select(-c(country_name))

res <- rcorr(as.matrix(vdem_corr001_SSA), type = "pearson")

cor_plot <- corrplot.mixed(res$r, lower.col = "black", number.cex = .7,
                           tl.pos = "lt", tl.col="black", upper = "square")

cor_plot
dev.copy(pdf,'outputs/C6_SSA.pdf', width = 10, height =10)
dev.off()

#### Figure Figure XY (additional figure, not used in paper) Western World ####

vdem_corr001_WesternWorld <- vdem %>%
  filter(e_regionpol_6C == 5)  %>%
  select(auto_period01, auto_period01ci, auto_period005, auto_period015ci, auto_period02ci, 
         auto_period01_libdem, auto_period01_libdem_ci, auto_period005_libdem, auto_period015_libdem_ci, 
         auto_period02_libdem_ci, auto_period_polity, auto_period_polity005, auto_period_polity015, 
         auto_period_polity02,auto_period_fh, auto_period_fh015, auto_period_fh02, year) %>%
  select(-c(year)) %>%
  rename("EDI 0.1" = auto_period01, 
         "EDI 0.1 + CI" = auto_period01ci, 
         "EDI 0.05 + CI" = auto_period005, 
         "EDI 0.15 + CI" = auto_period015ci, 
         "EDI 0.2 + CI" = auto_period02ci, 
         "LDI 0.1" = auto_period01_libdem, 
         "LDI 0.1 + CI" = auto_period01_libdem_ci, 
         "LDI 0.05 + CI" = auto_period005_libdem, 
         "LDI 0.15 + CI" = auto_period015_libdem_ci, 
         "LDI 0.2 + CI" = auto_period02_libdem_ci, 
         "Polity IV 0.1" = auto_period_polity,
         "Polity IV 0.05" = auto_period_polity005,
         "Polity IV 0.15" = auto_period_polity015,
         "Polity IV 0.2" = auto_period_polity02, 
         "FH 0.1" = auto_period_fh, 
         "FH 0.15" = auto_period_fh015,
         "FH 0.2" = auto_period_fh02) %>%
  ungroup() %>%
  select(-c(country_name))

res <- rcorr(as.matrix(vdem_corr001_WesternWorld), type = "pearson")

cor_plot <- corrplot.mixed(res$r, lower.col = "black", number.cex = .7,
                           tl.pos = "lt", tl.col="black", upper = "square")

cor_plot
dev.copy(pdf,'outputs/C7_WesternEurope.pdf', width = 10, height =10)
dev.off()

#### Figure Figure XY (additional figure, not used in paper) Asia Pacific ####

vdem_corr001_AsiaPacific <- vdem %>%
  filter(e_regionpol_6C == 6)  %>%
  select(auto_period01, auto_period01ci, auto_period005, auto_period015ci, auto_period02ci, 
         auto_period01_libdem, auto_period01_libdem_ci, auto_period005_libdem, auto_period015_libdem_ci, 
         auto_period02_libdem_ci, auto_period_polity, auto_period_polity005, auto_period_polity015, 
         auto_period_polity02,auto_period_fh, auto_period_fh015, auto_period_fh02, year) %>%
  select(-c(year)) %>%
  rename("EDI 0.1" = auto_period01, 
         "EDI 0.1 + CI" = auto_period01ci, 
         "EDI 0.05 + CI" = auto_period005, 
         "EDI 0.15 + CI" = auto_period015ci, 
         "EDI 0.2 + CI" = auto_period02ci, 
         "LDI 0.1" = auto_period01_libdem, 
         "LDI 0.1 + CI" = auto_period01_libdem_ci, 
         "LDI 0.05 + CI" = auto_period005_libdem, 
         "LDI 0.15 + CI" = auto_period015_libdem_ci, 
         "LDI 0.2 + CI" = auto_period02_libdem_ci, 
         "Polity IV 0.1" = auto_period_polity,
         "Polity IV 0.05" = auto_period_polity005,
         "Polity IV 0.15" = auto_period_polity015,
         "Polity IV 0.2" = auto_period_polity02, 
         "FH 0.1" = auto_period_fh, 
         "FH 0.15" = auto_period_fh015,
         "FH 0.2" = auto_period_fh02) %>%
  ungroup() %>%
  select(-c(country_name))

res <- rcorr(as.matrix(vdem_corr001_AsiaPacific), type = "pearson")

cor_plot <- corrplot.mixed(res$r, lower.col = "black", number.cex = .7,
                           tl.pos = "lt", tl.col="black", upper = "square")

cor_plot
dev.copy(pdf,'outputs/C8_AsiaPacific.pdf', width = 10, height =10)
dev.off()


#############################################################################################
#############################################################################################

#### Figure 3 and 4 ####

vdem_org <- readRDS("VDem_autocratization10.rds")

vdem <- vdem_org

vdem <- vdem %>%
  select(country_name, year, starts_with("auto_period"), starts_with("auto_decline"), v2x_regime) %>%
  filter(year >= 1900)

vdem_EDI01 <- vdem %>%
  filter(auto_period01==1) %>%
  group_by(v2x_regime) %>%
  dplyr:: summarize(n_edi01 =n())

vdem_EDI01ci <-  vdem %>%
  filter(auto_period01ci==1) %>%
  group_by(v2x_regime) %>%
  dplyr:: summarize(n_edi01ci =n())

vdem_EDI005 <-  vdem %>%
  filter(auto_period005==1) %>%
  group_by(v2x_regime) %>%
  dplyr:: summarize(n_edi005 =n())

vdem_EDI015ci <-  vdem %>%
  filter(auto_period015ci==1) %>%
  group_by(v2x_regime) %>%
  dplyr:: summarize(n_edi015ci =n())

vdem_EDI02ci <-  vdem %>%
  filter(auto_period02ci==1) %>%
  group_by(v2x_regime) %>%
  dplyr:: summarize(n_edi02ci =n())

vdem_LDI01 <-  vdem %>%
  filter(auto_period01_libdem==1) %>%
  group_by(v2x_regime) %>%
  dplyr:: summarize(n_ldi01 =n())

vdem_LDI01ci <-  vdem %>%
  filter(auto_period01_libdem_ci==1) %>%
  group_by(v2x_regime) %>%
  dplyr:: summarize(n_ldi01ci =n())

vdem_LDI005 <-  vdem %>%
  filter(auto_period005_libdem==1) %>%
  group_by(v2x_regime) %>%
  dplyr:: summarize(n_ldi005 =n())

vdem_LDI015ci <-  vdem %>%
  filter(auto_period015_libdem_ci==1) %>%
  group_by(v2x_regime) %>%
  dplyr:: summarize(n_ldi015ci =n())

vdem_LDI02ci <-  vdem %>%
  filter(auto_period02_libdem_ci==1) %>%
  group_by(v2x_regime) %>%
  dplyr:: summarize(n_ldi02ci =n())

vdem_FH <-  vdem %>%
  filter(auto_period_fh==1) %>%
  group_by(v2x_regime) %>%
  dplyr:: summarize(n_fh =n())

vdem_FH015 <-  vdem %>%
  filter(auto_period_fh015==1) %>%
  group_by(v2x_regime) %>%
  dplyr:: summarize(n_fh015 =n())

vdem_FH02 <-  vdem %>%
  filter(auto_period_fh02==1) %>%
  group_by(v2x_regime) %>%
  dplyr:: summarize(n_fh02 =n())

vdem_Polity <-  vdem %>%
  filter(auto_period_polity==1) %>%
  group_by(v2x_regime) %>%
  dplyr:: summarize(n_polity =n())

vdem_Polity005 <-  vdem %>%
  filter(auto_period_polity005==1) %>%
  group_by(v2x_regime) %>%
  dplyr:: summarize(n_polity005 =n())

vdem_Polity015 <-  vdem %>%
  filter(auto_period_polity015==1) %>%
  group_by(v2x_regime) %>%
  dplyr:: summarize(n_polity015 =n())

vdem_Polity02 <-  vdem %>%
  filter(auto_period_polity02==1) %>%
  group_by(v2x_regime) %>%
  dplyr:: summarize(n_polity02 =n())

vdem_fig3a <- vdem_EDI01 %>%
  left_join(vdem_EDI01ci, by = "v2x_regime") %>%
  left_join(vdem_EDI005, by = "v2x_regime")%>%
  left_join(vdem_EDI015ci, by = "v2x_regime")%>%
  left_join(vdem_EDI02ci, by = "v2x_regime")%>%
  left_join(vdem_LDI01, by = "v2x_regime") %>%
  left_join(vdem_LDI01ci, by = "v2x_regime") %>%
  left_join(vdem_LDI005, by = "v2x_regime") %>%
  left_join(vdem_LDI015ci, by = "v2x_regime") %>%
  left_join(vdem_LDI02ci, by = "v2x_regime") %>%
  left_join(vdem_FH, by = "v2x_regime") %>%
  left_join(vdem_FH015, by = "v2x_regime") %>%
  left_join(vdem_FH02, by = "v2x_regime") %>%
  left_join(vdem_Polity, by = "v2x_regime") %>%
  left_join(vdem_Polity005, by = "v2x_regime") %>%
  left_join(vdem_Polity015, by = "v2x_regime") %>%
  left_join(vdem_Polity02, by = "v2x_regime") %>%
  rename("EDI 0.1" = n_edi01, 
         "EDI 0.1 + CI" = n_edi01ci, 
         "EDI 0.05 + CI" = n_edi005, 
         "EDI 0.15 + CI" = n_edi015ci, 
         "EDI 0.2 + CI" = n_edi02ci, 
         "LDI 0.1" = n_ldi01, 
         "LDI 0.1 + CI" = n_ldi01ci, 
         "LDI 0.05 + CI" = n_ldi005, 
         "LDI 0.15 + CI" = n_ldi015ci, 
         "LDI 0.2 + CI" = n_ldi02ci, 
         "Polity 0.1" = n_polity, 
         "Polity 0.05" = n_polity005,
         "Polity 0.15" = n_polity015,
         "Polity 0.2" = n_polity02,
         "FH 0.1" = n_fh, 
         "FH 0.15" = n_fh015, 
         "FH 0.2" = n_fh02)

## Figure 3 ##

vdem_fig3a <- vdem_fig3a %>%
  mutate(v2x_regime = ifelse(v2x_regime==0, "Closed Autocracy", 
                             ifelse(v2x_regime==1, "Electoral Autocracy",
                                    ifelse(v2x_regime==2, "Electoral Democracy", 
                                           ifelse(v2x_regime==3, "Liberal Democracy", 0)))))

vdem_fig3a <- vdem_fig3a %>%
  pivot_longer(-v2x_regime, names_to = "Index", values_to = "number") %>%
  group_by(Index) %>%
  mutate(label_y = cumsum(number))


ggplot(vdem_fig3a, aes(x = Index, y = number, fill = v2x_regime)) +
  geom_col() +
  theme_pubr() +
  scale_fill_brewer(palette = "Dark2") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_x_discrete(limits = c("EDI 0.05 + CI","EDI 0.1", "EDI 0.1 + CI", "EDI 0.15 + CI", 
                              "EDI 0.2 + CI", "LDI 0.05 + CI", "LDI 0.1","LDI 0.1 + CI", 
                              "LDI 0.15 + CI", "LDI 0.2 + CI", "Polity 0.05", "Polity 0.1","Polity 0.15",
                              "Polity 0.2","FH 0.1","FH 0.15", "FH 0.2")) +
  labs(fill = "Regime Types", 
       x = "", 
       y = "Number of Country-Years")

ggsave("outputs/Figures3.png",dpi = 1200, width = 40, height = 25, units = "cm" )
ggsave("outputs/Figures3.pdf",dpi = 1200, width = 40, height = 25, units = "cm" )

## Figure 4 ##

vdem_EDI01ci <- vdem %>%
  filter(auto_period01ci_start005==1) %>%
  group_by(v2x_regime) %>%
  dplyr:: summarize(n_edi01ci =n())

vdem_EDI015ci <-  vdem %>%
  filter(auto_period015ci_start005==1) %>%
  group_by(v2x_regime) %>%
  dplyr:: summarize(n_edi015ci =n())

vdem_EDI02ci <-  vdem %>%
  filter(auto_period02ci_start005==1) %>%
  group_by(v2x_regime) %>%
  dplyr:: summarize(n_edi02ci =n())

vdem_LDI01ci <-  vdem %>%
  filter(auto_period01_libdem_ci_start005==1) %>%
  group_by(v2x_regime) %>%
  dplyr:: summarize(n_ldi01ci =n())

vdem_LDI015ci <-  vdem %>%
  filter(auto_period015_libdem_ci_start005==1) %>%
  group_by(v2x_regime) %>%
  dplyr:: summarize(n_ldi015ci =n())

vdem_LDI02ci <-  vdem %>%
  filter(auto_period02_libdem_ci_start005==1) %>%
  group_by(v2x_regime) %>%
  dplyr:: summarize(n_ldi02ci =n())

vdem_Polity <-  vdem %>%
  filter(auto_period_polity01_st005==1) %>%
  group_by(v2x_regime) %>%
  dplyr:: summarize(n_polity01 =n())

vdem_Polity015 <-  vdem %>%
  filter(auto_period_polity015_st005==1) %>%
  group_by(v2x_regime) %>%
  dplyr:: summarize(n_polity015 =n())

vdem_Polity02 <-  vdem %>%
  filter(auto_period_polity02_st005==1) %>%
  group_by(v2x_regime) %>%
  dplyr:: summarize(n_polity02 =n())

vdem_fig3b <- vdem_EDI01 %>%
  left_join(vdem_EDI01ci, by = "v2x_regime") %>%
  left_join(vdem_EDI015ci, by = "v2x_regime")%>%
  left_join(vdem_EDI02ci, by = "v2x_regime")%>%
  left_join(vdem_LDI01ci, by = "v2x_regime") %>%
  left_join(vdem_LDI015ci, by = "v2x_regime") %>%
  left_join(vdem_LDI02ci, by = "v2x_regime") %>%
  left_join(vdem_Polity, by = "v2x_regime") %>%
  left_join(vdem_Polity015, by = "v2x_regime") %>%
  left_join(vdem_Polity02, by = "v2x_regime") %>%
  rename("EDI 0.1 + CI" = n_edi01ci, 
         "EDI 0.15 + CI" = n_edi015ci, 
         "EDI 0.2 + CI" = n_edi02ci, 
         "LDI 0.1 + CI" = n_ldi01ci, 
         "LDI 0.15 + CI" = n_ldi015ci, 
         "LDI 0.2 + CI" = n_ldi02ci, 
         "Polity 0.1" = n_polity01, 
         "Polity 0.15" = n_polity015,
         "Polity 0.2" = n_polity02)

vdem_fig3b <- vdem_fig3b %>%
  mutate(v2x_regime = ifelse(v2x_regime==0, "Closed Autocracy", 
                             ifelse(v2x_regime==1, "Electoral Autocracy",
                                    ifelse(v2x_regime==2, "Electoral Democracy", 
                                           ifelse(v2x_regime==3, "Liberal Democracy", 0)))))

vdem_fig3b <- vdem_fig3b %>%
  pivot_longer(-v2x_regime, names_to = "Index", values_to = "number") %>%
  group_by(Index) %>%
  mutate(label_y = cumsum(number))

ggplot(vdem_fig3b, aes(x = Index, y = number, fill = v2x_regime)) +
  geom_col() +
  theme_pubr() +
  scale_fill_brewer(palette = "Dark2") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_x_discrete(limits = c("EDI 0.1 + CI", "EDI 0.15 + CI", 
                              "EDI 0.2 + CI", "LDI 0.1 + CI", 
                              "LDI 0.15 + CI", "LDI 0.2 + CI", "Polity 0.1","Polity 0.15",
                              "Polity 0.2")) +
  labs(fill = "Regime Types", 
       x = "", 
       y = "Number of Country-Years")

ggsave("outputs/Figure4.png",dpi = 1200, width = 40, height = 25, units = "cm" )
ggsave("outputs/Figure4.pdf",dpi = 1200, width = 40, height = 25, units = "cm" )


#### Convergence Measures and World Map ####
#### Figure 5 ####

vdem <- readRDS("VDem_autocratization10.rds")

vdem <- vdem %>% 
  group_by(country_name) %>%
  mutate(lag_year = dplyr::lag(year), 
         lag_year = ifelse(is.na(lag_year), year, lag_year))

vdem_con <- vdem %>%
  filter(auto_period01==1 | auto_period01ci==1 | auto_period01ci_start005==1 | auto_period005==1 | 
           auto_period015ci == 1 | auto_period015ci_start005==1 | auto_period02ci ==1 | auto_period02ci_start005==1 |
           auto_period01_libdem==1 | auto_period01_libdem_ci==1 | auto_period01_libdem_ci_start005==1 | 
           auto_period005_libdem== 1 | auto_period015_libdem_ci==1 | auto_period015_libdem_ci_start005== 1 |
           auto_period02_libdem_ci== 1 | auto_period02_libdem_ci_start005== 1 | auto_period_polity == 1 | 
           auto_period_polity01_st005 == 1 | auto_period_polity005 == 1 | auto_period_polity015 == 1 |
           auto_period_polity015_st005 == 1 | auto_period_polity02 == 1 | auto_period_polity02_st005 == 1 |
           auto_period_fh == 1 | auto_period_fh015 == 1 | auto_period_fh02 == 1 )

## 2214 country years which were identified as autocratization year 

vdem_con <- vdem_con %>%
  select(country_name, country_id, year, starts_with("auto_period"), starts_with("auto_decline"), lag_year)
  
## Building measurement-unspecific periods ##

vdem_con <- vdem_con %>% 
  group_by(country_name) %>%
  mutate(lag_year = dplyr::lag(year), 
         lag_year = ifelse(is.na(lag_year), year, lag_year), 
         year_diff = lag_year-year, 
         year_diff = ifelse(year_diff == 0 | year_diff ==-1, 0, 1)) %>%
  mutate(group_id = cumsum(year_diff)) # group_id by each country for connected year 

vdem_con <- vdem_con %>% 
  mutate(auto_period_id_all = str_c(country_name, group_id, sep = "_")) %>%
  select(-c(lag_year, year_diff, group_id)) %>%
  select(country_name, country_id, year, auto_period_id_all, starts_with("auto_period"), starts_with("auto_decline"))


min.new <- function(v) {
  if (all(is.na(v))) { return(NA) } else { return(min(v, na.rm=T)) }
} 

max.new <- function(v) {
  if (all(is.na(v))) { return(NA) } else { return(max(v, na.rm=T)) }
}

mean.new <- function(v) {
  if (all(is.na(v))) { return(NA) } else { return(mean(v, na.rm=T)) }
}

sum.new <- function(v) {
  if (all(is.na(v))) { return(NA) } else { return(sum(v, na.rm=T)) }
}


vdem_con_test <- vdem_con %>%
  select(country_name, country_id, year, auto_period_id_all, auto_period01, auto_period01ci, auto_period01ci_start005,
         auto_period005, auto_period015ci, auto_period015ci_start005, auto_period02ci, auto_period02ci_start005,
         auto_period01_libdem, auto_period01_libdem_ci, auto_period01_libdem_ci_start005,auto_period005_libdem,
         auto_period015_libdem_ci, auto_period015_libdem_ci_start005, auto_period02_libdem_ci, 
         auto_period02_libdem_ci_start005, auto_period_polity, auto_period_polity01_st005, auto_period_polity005, 
         auto_period_polity015, auto_period_polity015_st005, auto_period_polity02, auto_period_polity02_st005, 
         auto_period_fh, auto_period_fh015, auto_period_fh02) 

vdem_con_test <- vdem_con_test%>%
  group_by(auto_period_id_all) %>%
  summarise_at(vars(auto_period01:auto_period_fh02), max.new) # 568 autocratization periods across different operationalization 


vdem_con_test <- vdem_con_test %>%
  select(-auto_period_id_all)

vdem_con_test <- vdem_con_test %>%
  rename("EDI 0.1" = auto_period01, 
         "EDI 0.1 + CI" = auto_period01ci, 
         "EDI 0.05 + CI" = auto_period005, 
        "EDI 0.15 + CI" = auto_period015ci, 
        "EDI 0.2 + CI" = auto_period02ci, 
        "LDI 0.1" = auto_period01_libdem, 
        "LDI 0.1 + CI" = auto_period01_libdem_ci, 
        "LDI 0.05 + CI" = auto_period005_libdem, 
        "LDI 0.15 + CI" = auto_period015_libdem_ci, 
        "LDI 0.2 + CI" = auto_period02_libdem_ci, 
        "Polity IV 0.1" = auto_period_polity,
        "Polity IV 0.05" = auto_period_polity005,
        "Polity IV 0.15" = auto_period_polity015,
        "Polity IV 0.2" = auto_period_polity02,
        "FH 0.1" = auto_period_fh, 
        "FH 0.15" = auto_period_fh015,
        "FH 0.2" = auto_period_fh02, 
        "EDI 0.1 + CI [S=0.05]" = auto_period01ci_start005, 
        "EDI 0.15 + CI [S=0.05]" = auto_period015ci_start005, 
        "EDI 0.2 + CI [S=0.05]" = auto_period02ci_start005, 
        "LDI 0.1 + CI [S=0.05]" = auto_period01_libdem_ci_start005, 
        "LDI 0.15 + CI [S=0.05]" = auto_period015_libdem_ci_start005, 
        "LDI 0.2 + CI [S=0.05]" = auto_period02_libdem_ci_start005, 
        "Polity IV 0.1 [S=0.05]" = auto_period_polity01_st005,
        "Polity IV 0.15 [S=0.05]" = auto_period_polity015_st005,
        "Polity IV 0.2 [S=0.05]" = auto_period_polity02_st005)

res <- rcorr(as.matrix(vdem_con_test), type = "pearson")

cor_plot <- corrplot.mixed(res$r, lower.col = "black", number.cex = .5,
                           tl.pos = "lt", tl.col="black", upper = "square")

par(mfrow=c(1,1)) 
cor_plot
dev.copy(pdf,'outputs/99_Figure_7_period_correlation.pdf', width = 10, height =10)
dev.off()

## correlations by country ##

vdem_con_test01 <- vdem_con %>%
  select(country_name, country_id, year, auto_period_id_all, auto_period01, auto_period01ci, auto_period01ci_start005,
         auto_period01_libdem, auto_period01_libdem_ci, auto_period01_libdem_ci_start005,auto_period_polity, 
         auto_period_polity01_st005, auto_period_fh) # 9 measures

vdem_con_test015 <- vdem_con %>%
  select(country_name, country_id, year, auto_period_id_all, auto_period015ci, auto_period015ci_start005, 
         auto_period015_libdem_ci, auto_period015_libdem_ci_start005, auto_period_polity015, 
         auto_period_polity015_st005, auto_period_fh015) # 7 measures

vdem_con_test005 <- vdem_con %>%
  select(country_name, country_id, year, auto_period_id_all, auto_period005, auto_period005_libdem,
         auto_period_polity005)  # 3 measures


vdem_con_test02 <- vdem_con %>%
  select(country_name, country_id, year, auto_period_id_all, auto_period02ci, auto_period02ci_start005,
         auto_period02_libdem_ci, auto_period02_libdem_ci_start005, auto_period_polity02, 
         auto_period_polity02_st005, auto_period_fh02) # 7 measures

## prepare datasets ##

vdem_con_test01  <- vdem_con_test01 %>%
  group_by(auto_period_id_all) %>%
  summarise_at(vars(auto_period01:auto_period_fh), max.new) %>%
  mutate(country_name = str_split(auto_period_id_all, "_"))

vdem_con_test015  <- vdem_con_test015 %>%
  group_by(auto_period_id_all) %>%
  summarise_at(vars(auto_period015ci:auto_period_fh015), max.new) %>%
  mutate(country_name = str_split(auto_period_id_all, "_"))

vdem_con_test005  <- vdem_con_test005 %>%
  group_by(auto_period_id_all) %>%
  summarise_at(vars(auto_period005:auto_period_polity005), max.new) %>%
  mutate(country_name = str_split(auto_period_id_all, "_"))

vdem_con_test02  <- vdem_con_test02 %>%
  group_by(auto_period_id_all) %>%
  summarise_at(vars(auto_period02ci:auto_period_fh02), max.new) %>%
  mutate(country_name = str_split(auto_period_id_all, "_"))

vdem_con_test01$country_name <- unlist(vdem_con_test01$country_name)[c(TRUE,FALSE)]
vdem_con_test015$country_name <- unlist(vdem_con_test015$country_name)[c(TRUE,FALSE)]
vdem_con_test005$country_name <- unlist(vdem_con_test005$country_name)[c(TRUE,FALSE)]
vdem_con_test02$country_name <- unlist(vdem_con_test02$country_name)[c(TRUE,FALSE)]


## Calculate Overlap of Autocratization periods for 0.1 threshold measures ##
#CC_ = Country_Convergence

##EDI 01 ##

CC_auto_period01 <- vdem_con_test01 %>% 
  select(-auto_period_id_all) %>%
  dplyr::rename(auto_period01EDI = auto_period01) %>%
  mutate_at(vars(-matches("auto_period01EDI"), -matches("country_name")), list(dif = ~ . - auto_period01EDI)) %>%
  select(country_name, ends_with("dif"))

colnames(CC_auto_period01) <- paste("EDI01", colnames(CC_auto_period01), sep = "_")

##EDI 01CI ##

CC_auto_period01ci <- vdem_con_test01 %>% 
  select(-auto_period_id_all) %>%
  dplyr::rename(auto_period01EDI = auto_period01, 
                auto_period01ciEDI = auto_period01ci) %>%  
  mutate_at(vars(-matches("auto_period01ciEDI"), -matches("country_name"), -matches("auto_period01EDI")), list(dif = ~ . - auto_period01ciEDI)) %>%
  select(country_name, ends_with("dif"))

colnames(CC_auto_period01ci) <- paste("EDI01CI", colnames(CC_auto_period01ci), sep = "_")

##auto_period01ci_start005 ##

CC_auto_period01ci_start005 <- vdem_con_test01 %>% 
  select(-auto_period_id_all) %>%
  dplyr::rename(auto_period01EDI = auto_period01, 
                auto_period01ciEDI = auto_period01ci) %>%
  mutate_at(vars(-matches("auto_period01ci_start005"), -matches("country_name"), 
                 -matches("auto_period01EDI"), -matches("auto_period01ciEDI")), list(dif = ~ . - auto_period01ci_start005)) %>%
  select(country_name, ends_with("dif"))

colnames(CC_auto_period01ci_start005) <- paste("EDI01CI005", colnames(CC_auto_period01ci_start005), sep = "_")

##auto_period01_libdemLDI ##

CC_auto_period01_libdem<- vdem_con_test01 %>% 
  select(-auto_period_id_all) %>%
  dplyr::rename(auto_period01EDI = auto_period01, 
                auto_period01ciEDI = auto_period01ci, 
                auto_period01_libdemLDI = auto_period01_libdem) %>%
  mutate_at(vars(-matches("auto_period01_libdemLDI"), -matches("country_name"), 
                 -matches("auto_period01EDI"), -matches("auto_period01ciEDI"), 
                 -matches("auto_period01ci_start005")), 
            list(dif = ~ . - auto_period01_libdemLDI)) %>%
  select(country_name, ends_with("dif"))

colnames(CC_auto_period01_libdem) <- paste("LDI01", colnames(CC_auto_period01_libdem), sep = "_")

##auto_period01_libdem_ciLDI ##

CC_auto_period01_libdem_ci<- vdem_con_test01 %>% 
  select(-auto_period_id_all) %>%
  dplyr::rename(auto_period01EDI = auto_period01, 
                auto_period01ciEDI = auto_period01ci, 
                auto_period01_libdemLDI = auto_period01_libdem, 
                auto_period01_libdem_ciLDI = auto_period01_libdem_ci) %>%
  mutate_at(vars(-matches("auto_period01_libdem_ciLDI"), -matches("country_name"), 
                 -matches("auto_period01EDI"), -matches("auto_period01ciEDI"), 
                 -matches("auto_period01ci_start005"), -matches("auto_period01_libdemLDI")), 
            list(dif = ~ . - auto_period01_libdem_ciLDI)) %>%
  select(country_name, ends_with("dif"))

colnames(CC_auto_period01_libdem_ci) <- paste("LDI01ci", colnames(CC_auto_period01_libdem_ci), sep = "_")


##LDI01CI Start005 ##

CC_auto_period01_libdem_ci_start005<- vdem_con_test01 %>% 
  select(-auto_period_id_all) %>%
  dplyr::rename(auto_period01EDI = auto_period01, 
                auto_period01ciEDI = auto_period01ci, 
                auto_period01_libdemLDI = auto_period01_libdem, 
                auto_period01_libdem_ciLDI = auto_period01_libdem_ci) %>%
  mutate_at(vars(-matches("auto_period01_libdem_ci_start005"), -matches("country_name"), 
                 -matches("auto_period01EDI"), -matches("auto_period01ciEDI"), 
                 -matches("auto_period01ci_start005"), -matches("auto_period01_libdemLDI"), 
                 -matches("auto_period01_libdem_ciLDI")), 
            list(dif = ~ . - auto_period01_libdem_ci_start005)) %>%
  select(country_name, ends_with("dif"))

colnames(CC_auto_period01_libdem_ci_start005) <- paste("LDI01ci_start005", colnames(CC_auto_period01_libdem_ci_start005), sep = "_")


### Polity01
CC_auto_period_polity <- vdem_con_test01 %>% 
  select(-auto_period_id_all) %>%
  dplyr::rename(auto_period01EDI = auto_period01, 
                auto_period01ciEDI = auto_period01ci, 
                auto_period01_libdemLDI = auto_period01_libdem, 
                auto_period01_libdem_ciLDI = auto_period01_libdem_ci, 
                auto_period_polityPP = auto_period_polity) %>%
  mutate_at(vars(-matches("auto_period_polityPP"), -matches("country_name"), 
                 -matches("auto_period01EDI"), -matches("auto_period01ciEDI"), 
                 -matches("auto_period01ci_start005"), -matches("auto_period01_libdemLDI"), 
                 -matches("auto_period01_libdem_ciLDI"), 
                 -matches("auto_period01_libdem_ci_start005")), 
            list(dif = ~ . - auto_period_polityPP)) %>%
  select(country_name, ends_with("dif"))

colnames(CC_auto_period_polity) <- paste("Polity01", colnames(CC_auto_period_polity), sep = "_")

### Polity01_start005
CC_auto_period_polity_start005 <- vdem_con_test01 %>% 
  select(-auto_period_id_all) %>%
  dplyr::rename(auto_period01EDI = auto_period01, 
                auto_period01ciEDI = auto_period01ci, 
                auto_period01_libdemLDI = auto_period01_libdem, 
                auto_period01_libdem_ciLDI = auto_period01_libdem_ci, 
                auto_period_polityPP = auto_period_polity) %>%
  mutate_at(vars(-matches("auto_period_polity01_st005"), -matches("country_name"), 
                 -matches("auto_period01EDI"), -matches("auto_period01ciEDI"), 
                 -matches("auto_period01ci_start005"), -matches("auto_period01_libdemLDI"),
                 -matches("auto_period01_libdem_ciLDI"), 
                 -matches("auto_period01_libdem_ci_start005"), -matches("auto_period_polityPP")), 
            list(dif = ~ . - auto_period_polity01_st005)) %>%
  select(country_name, ends_with("dif"))

colnames(CC_auto_period_polity_start005) <- paste("Polity01_start005", colnames(CC_auto_period_polity_start005), sep = "_")

#### 0.05 measures ####

##auto_period005##

CC_auto_period005 <- vdem_con_test005 %>% 
  select(-auto_period_id_all) %>%
  dplyr::rename(auto_period005EDI = auto_period005 ) %>%
  mutate_at(vars(-matches("auto_period005EDI"), -matches("country_name")), list(dif = ~ . - auto_period005EDI)) %>%
  select(country_name, ends_with("dif"))

colnames(CC_auto_period005) <- paste("EDI005", colnames(CC_auto_period005), sep = "_")

##LDI005 ##

CC_auto_period005_libdem<- vdem_con_test005 %>% 
  select(-auto_period_id_all) %>%
  dplyr::rename(auto_period005EDI = auto_period005) %>%
  mutate_at(vars(-matches("auto_period005_libdem"), -matches("country_name"), 
                 -matches("auto_period005EDI")), 
            list(dif = ~ . - auto_period005_libdem)) %>%
  select(country_name, ends_with("dif"))

colnames(CC_auto_period005_libdem) <- paste("LDI005", colnames(CC_auto_period005_libdem), sep = "_")

#### 0.15 measures ####

##auto_period015ci ##

CC_auto_period015ci <- vdem_con_test015 %>% 
  select(-auto_period_id_all) %>%
  dplyr::rename(auto_period015ciEDI = auto_period015ci) %>%
  mutate_at(vars(-matches("auto_period015ciEDI"), -matches("country_name")), 
            list(dif = ~ . - auto_period015ciEDI)) %>%
  select(country_name, ends_with("dif"))

colnames(CC_auto_period015ci) <- paste("EDI015ci", colnames(CC_auto_period015ci), sep = "_")

##auto_period015ci_start005 ##

CC_auto_period015ci_start005 <- vdem_con_test015 %>% 
  select(-auto_period_id_all) %>%
  dplyr::rename(auto_period015ciEDI = auto_period015ci) %>%
  mutate_at(vars(-matches("auto_period015ci_start005"), -matches("country_name"), 
                 -matches("auto_period015ciEDI")), 
            list(dif = ~ . - auto_period015ci_start005)) %>%
  select(country_name, ends_with("dif"))

colnames(CC_auto_period015ci_start005) <- paste("EDI015ci_start005", colnames(CC_auto_period015ci_start005), sep = "_")

##LDI015ci ##

CC_auto_period015_libdem_ci<- vdem_con_test015 %>% 
  select(-auto_period_id_all) %>%
  dplyr::rename(auto_period015ciEDI = auto_period015ci, 
                auto_period015_libdem_ciLDI = auto_period015_libdem_ci) %>%
  mutate_at(vars(-matches("auto_period015_libdem_ciLDI"), -matches("country_name"), 
                 -matches("auto_period015ciEDI"), -matches("auto_period015ci_start005")), 
            list(dif = ~ . - auto_period015_libdem_ciLDI)) %>%
  select(country_name, ends_with("dif"))

colnames(CC_auto_period015_libdem_ci) <- paste("LDI015ci", colnames(CC_auto_period015_libdem_ci), sep = "_")

### LDI 015 CI Start005
CC_auto_period015_libdem_ci_start005<- vdem_con_test015 %>% 
  select(-auto_period_id_all) %>%
  dplyr::rename(auto_period015ciEDI = auto_period015ci, 
                auto_period015_libdem_ciLDI = auto_period015_libdem_ci) %>%
  mutate_at(vars(-matches("auto_period015_libdem_ci_start005"), -matches("country_name"), 
                 -matches("auto_period015ciEDI"), -matches("auto_period015ci_start005"), 
                 -matches("auto_period015_libdem_ciLDI")), 
            list(dif = ~ . - auto_period015_libdem_ci_start005)) %>%
  select(country_name, ends_with("dif"))

colnames(CC_auto_period015_libdem_ci_start005) <- paste("LDI015ci_start005", colnames(CC_auto_period015_libdem_ci_start005), sep = "_")

### Polity015
CC_auto_period_polity015 <- vdem_con_test015 %>% 
  select(-auto_period_id_all) %>%
  dplyr::rename(auto_period015ciEDI = auto_period015ci, 
                auto_period015_libdem_ciLDI = auto_period015_libdem_ci, 
                auto_period_polity015PP = auto_period_polity015) %>%
  mutate_at(vars(-matches("auto_period_polity015PP"), -matches("country_name"), 
                 -matches("auto_period015ciEDI"), -matches("auto_period015ci_start005"), 
                 -matches("auto_period015_libdem_ciLDI"), -matches("auto_period015_libdem_ci_start005")), 
            list(dif = ~ . - auto_period_polity015PP)) %>%
  select(country_name, ends_with("dif"))

colnames(CC_auto_period_polity015) <- paste("Polity015", colnames(CC_auto_period_polity015), sep = "_")


### Polity015 start 005
CC_auto_period_polity015_start005 <- vdem_con_test015 %>% 
  select(-auto_period_id_all) %>%
  dplyr::rename(auto_period015ciEDI = auto_period015ci, 
                auto_period015_libdem_ciLDI = auto_period015_libdem_ci, 
                auto_period_polity015PP = auto_period_polity015) %>%
  mutate_at(vars(-matches("auto_period_polity015_st005"), -matches("country_name"), 
                 -matches("auto_period015ciEDI"), -matches("auto_period015ci_start005"), 
                 -matches("auto_period015_libdem_ciLDI"), -matches("auto_period015_libdem_ci_start005"), 
                 -matches("auto_period_polity015PP")), 
            list(dif = ~ . - auto_period_polity015_st005)) %>%
  select(country_name, ends_with("dif"))

colnames(CC_auto_period_polity015_start005) <- paste("Polity015_start005", colnames(CC_auto_period_polity015_start005), sep = "_")


#### 0.2 measures ####

##auto_period02ci ##

CC_auto_period02ci <- vdem_con_test02 %>% 
  select(-auto_period_id_all) %>%
  dplyr::rename(auto_period02ciEDI = auto_period02ci) %>%
  mutate_at(vars(-matches("auto_period02ciEDI"), -matches("country_name")), 
            list(dif = ~ . - auto_period02ciEDI)) %>%
  select(country_name, ends_with("dif"))

colnames(CC_auto_period02ci) <- paste("EDI02ci", colnames(CC_auto_period02ci), sep = "_")


##auto_period02ci_start005 ##

CC_auto_period02ci_start005 <- vdem_con_test02 %>% 
  select(-auto_period_id_all) %>%
  dplyr::rename(auto_period02ciEDI = auto_period02ci) %>%
  mutate_at(vars(-matches("auto_period02ci_start005"), -matches("country_name"), 
                 -matches("auto_period02ciEDI")), 
            list(dif = ~ . - auto_period02ci_start005)) %>%
  select(country_name, ends_with("dif"))

colnames(CC_auto_period02ci_start005) <- paste("EDI02ci_start005", colnames(CC_auto_period02ci_start005), sep = "_")


### LDI 02 CI 
CC_auto_period02_libdem_ci<- vdem_con_test02 %>% 
  select(-auto_period_id_all) %>%
  dplyr::rename(auto_period02ciEDI = auto_period02ci, 
                auto_period02_libdem_ciLDI = auto_period02_libdem_ci) %>%
  mutate_at(vars(-matches("auto_period02_libdem_ciLDI"), -matches("country_name"), 
                 -matches("auto_period02ciEDI"), -matches("auto_period02ci_start005")), 
            list(dif = ~ . - auto_period02_libdem_ciLDI)) %>%
  select(country_name, ends_with("dif"))

colnames(CC_auto_period02_libdem_ci) <- paste("LDI02ci", colnames(CC_auto_period02_libdem_ci), sep = "_")


### LDI 02 CI_start005
CC_auto_period02_libdem_ci_start005<- vdem_con_test02 %>% 
  select(-auto_period_id_all) %>%
  dplyr::rename(auto_period02ciEDI = auto_period02ci, 
                auto_period02_libdem_ciLDI = auto_period02_libdem_ci) %>%
  mutate_at(vars(-matches("auto_period02_libdem_ci_start005"), -matches("country_name"), 
                 -matches("auto_period02ciEDI"), -matches("auto_period02ci_start005"), 
                 -matches("auto_period02_libdem_ciLDI")), 
            list(dif = ~ . - auto_period02_libdem_ci_start005)) %>%
  select(country_name, ends_with("dif"))

colnames(CC_auto_period02_libdem_ci_start005) <- paste("LDI02ci_start005", colnames(CC_auto_period02_libdem_ci_start005), sep = "_")


### Polity02
CC_auto_period_polity02 <- vdem_con_test02 %>% 
  select(-auto_period_id_all) %>%
  dplyr::rename(auto_period02ciEDI = auto_period02ci, 
                auto_period02_libdem_ciLDI = auto_period02_libdem_ci,
                auto_period_polity02PP = auto_period_polity02) %>%
  mutate_at(vars(-matches("auto_period_polity02PP"), -matches("auto_period02_libdem_ci_start005"), -matches("country_name"), 
                 -matches("auto_period02ciEDI"), -matches("auto_period02ci_start005"), 
                 -matches("auto_period02_libdem_ciLDI")),
            list(dif = ~ . - auto_period_polity02PP)) %>%
  select(country_name, ends_with("dif"))

colnames(CC_auto_period_polity02) <- paste("Polity02", colnames(CC_auto_period_polity02), sep = "_")


### Polity02 start 005
CC_auto_period_polity02_start005 <- vdem_con_test02 %>% 
  select(-auto_period_id_all) %>%
  dplyr::rename(auto_period02ciEDI = auto_period02ci, 
                auto_period02_libdem_ciLDI = auto_period02_libdem_ci,
                auto_period_polity02PP = auto_period_polity02) %>%
  mutate_at(vars(-matches("auto_period_polity015_st005"), --matches("auto_period_polity02PP"), 
                 -matches("auto_period02_libdem_ci_start005"), -matches("country_name"), 
                 -matches("auto_period02ciEDI"), -matches("auto_period02ci_start005"), 
                 -matches("auto_period02_libdem_ciLDI")), 
            list(dif = ~ . - auto_period_polity02_st005)) %>%
  select(country_name, ends_with("dif"))

colnames(CC_auto_period_polity02_start005) <- paste("Polity02_start005", colnames(CC_auto_period_polity02_start005), sep = "_")


#### Build one dataframe for Country Congruence between measures ####

## 0.1 Threshold ##
CC_autp_periods_df01 <- cbind(CC_auto_period01, CC_auto_period01ci, CC_auto_period01ci_start005,
                              CC_auto_period01_libdem, CC_auto_period01_libdem_ci, CC_auto_period01_libdem_ci_start005, 
                              CC_auto_period_polity, CC_auto_period_polity_start005)

CC_autp_periods_df01 <- CC_autp_periods_df01 %>%
  rename(country = EDI01_country_name)

CC_autp_periods_df01 <- CC_autp_periods_df01 %>%
  dplyr::select(-ends_with("_country_name"))

## 0.05 Threshold ##
CC_autp_periods_df005 <- cbind(CC_auto_period005, CC_auto_period005_libdem)

CC_autp_periods_df005 <- CC_autp_periods_df005 %>%
  rename(country = EDI005_country_name)

CC_autp_periods_df005 <- CC_autp_periods_df005 %>%
  dplyr::select(-ends_with("_country_name"))

## 0.15 Threshold ##
CC_autp_periods_df015 <- cbind(CC_auto_period015ci, CC_auto_period015ci_start005,
                               CC_auto_period015_libdem_ci, CC_auto_period015_libdem_ci_start005, 
                               CC_auto_period_polity015, CC_auto_period_polity015_start005)

CC_autp_periods_df015 <- CC_autp_periods_df015 %>%
  rename(country = EDI015ci_country_name)

CC_autp_periods_df015 <- CC_autp_periods_df015 %>%
  dplyr::select(-ends_with("_country_name"))

## 0.2 Threshold ##
CC_autp_periods_df02 <- cbind(CC_auto_period02ci, CC_auto_period02ci_start005, 
                              CC_auto_period02_libdem_ci, CC_auto_period02_libdem_ci_start005,
                              CC_auto_period_polity02, CC_auto_period_polity02_start005)

CC_autp_periods_df02 <- CC_autp_periods_df02 %>%
  rename(country = EDI02ci_country_name)

CC_autp_periods_df02 <- CC_autp_periods_df02 %>%
  dplyr::select(-ends_with("_country_name"))

#### Count Number of 1 and -1 in each Row ####

CC_autp_periods_df01$count_convergence <- rowSums(CC_autp_periods_df01[-1] == -1 | CC_autp_periods_df01[-1] == 1 , na.rm = TRUE )
CC_autp_periods_df005$count_convergence <- rowSums(CC_autp_periods_df005[-1] == -1 | CC_autp_periods_df005[-1] == 1 , na.rm = TRUE )
CC_autp_periods_df015$count_convergence <- rowSums(CC_autp_periods_df015[-1] == -1 | CC_autp_periods_df015[-1] == 1 , na.rm = TRUE )
CC_autp_periods_df02$count_convergence <- rowSums(CC_autp_periods_df02[-1] == -1 | CC_autp_periods_df02[-1] == 1 , na.rm = TRUE )


## 0.1 Threshold ##
summary(CC_autp_periods_df01$count_convergence) # min possible 0; max empirical is 20,
# max theoretical divergence is: 36

## Rescale count_congruence to 0 to 100 (interpreted as percentage of congruence by for each autocratization period) ##

CC_autp_periods_df01$count_convergence_res01 <- rescale(CC_autp_periods_df01$count_convergence, to = c(100, 0), from = range(c(0,36), na.rm = TRUE, finite = TRUE))

summary(CC_autp_periods_df01$count_convergence_res01) # min empirical congruence after rescaling: 44%, max: 100%, 
#higher values indicate greater congreunce while lower values indictate smaller congruence between measures. 

## 0.05 Threshold ##

summary(CC_autp_periods_df005$count_convergence) # min possible 0; max empirical is 2,
# max theoretical divergence is: 3

## Rescale count_congruence to 0 to 100 (interpreted as percentage of congruence by for each autocratization period) ##

CC_autp_periods_df005$count_convergence_res005 <- rescale(CC_autp_periods_df005$count_convergence, to = c(100, 0), from = range(c(0,3), na.rm = TRUE, finite = TRUE))

summary(CC_autp_periods_df005$count_convergence_res005) # min empirical congruence after rescaling: 33%, max: 100%, 
#higher values indicate greater congreunce while lower values indictate smaller congruence between measures. 


## 0.15 Threshold ##

summary(CC_autp_periods_df015$count_convergence) # min possible 0; max empirical is 12,
# max theoretical divergence is: 21

## Rescale count_congruence to 0 to 100 (interpreted as percentage of congruence by for each autocratization period) ##

CC_autp_periods_df015$count_convergence_res015 <- rescale(CC_autp_periods_df015$count_convergence, to = c(100, 0), from = range(c(0,21), na.rm = TRUE, finite = TRUE))

summary(CC_autp_periods_df015$count_convergence_res015) # min empirical congruence after rescaling: 42.86%, max: 100%, 
#higher values indicate greater congreunce while lower values indictate smaller congruence between measures. 

## 0.2 Threshold ##

summary(CC_autp_periods_df02$count_convergence) # min possible 0; max empirical is 12,
# max theoretical divergence is: 21

## Rescale count_congruence to 0 to 100 (interpreted as percentage of congruence by for each autocratization period) ##

CC_autp_periods_df02$count_convergence_res02 <- rescale(CC_autp_periods_df02$count_convergence, to = c(100, 0), from = range(c(0,21), na.rm = TRUE, finite = TRUE))

summary(CC_autp_periods_df02$count_convergence_res02) # min empirical congruence after rescaling: 42.86%, max: 100%, 
#higher values indicate greater congreunce while lower values indictate smaller congruence between measures. 


## Merging data ##

CC_auto_periods_df <- cbind(CC_autp_periods_df01, CC_autp_periods_df005, CC_autp_periods_df015, CC_autp_periods_df02)

CC_auto_periods_df <- CC_auto_periods_df[, -c(40,46,70)] 

CC_auto_periods_df <- CC_auto_periods_df %>%
  dplyr::select(country, count_convergence_res01, count_convergence_res005, count_convergence_res015,
                count_convergence_res02)

CC_auto_periods_df <- CC_auto_periods_df %>%
  rowwise() %>%
  mutate(mean_congruence = mean.new(c(count_convergence_res01, count_convergence_res005, count_convergence_res015,
                                    count_convergence_res02)))


CC_country <- CC_auto_periods_df %>%
  group_by(country) %>%
  dplyr::summarize(country_convergence = mean.new(mean_congruence), 
                   count_country_periods = n()) 

summary(CC_country$country_convergence)

saveRDS(CC_country, "data/CC_country.rds")


#### prepare CC country_data for different thresholds ####

## 0.05 ##
CC_auto_periods_df005 <- CC_autp_periods_df005 %>%
  dplyr::select(country, count_convergence_res005)

CC_auto_periods_df005 <- CC_auto_periods_df005 %>%
  group_by(country) %>%
  dplyr::summarize(country_convergence = mean.new(count_convergence_res005), 
                   count_country_periods = n()) 

saveRDS(CC_auto_periods_df005, "data/CC_auto_periods_df005.rds")

## 0.1 ##
CC_auto_periods_df01 <- CC_autp_periods_df01 %>%
  dplyr::select(country, count_convergence_res01)

CC_auto_periods_df01 <- CC_auto_periods_df01 %>%
  group_by(country) %>%
  dplyr::summarize(country_convergence = mean.new(count_convergence_res01), 
                   count_country_periods = n()) 

saveRDS(CC_auto_periods_df01, "data/CC_auto_periods_df01.rds")

## 0.1 ##
CC_auto_periods_df015 <- CC_autp_periods_df015 %>%
  dplyr::select(country, count_convergence_res015)

CC_auto_periods_df015 <- CC_auto_periods_df015 %>%
  group_by(country) %>%
  dplyr::summarize(country_convergence = mean.new(count_convergence_res015), 
                   count_country_periods = n()) 

saveRDS(CC_auto_periods_df015, "data/CC_auto_periods_df015.rds")

## 0.2 ##
CC_auto_periods_df02 <- CC_autp_periods_df02 %>%
  dplyr::select(country, count_convergence_res02)

CC_auto_periods_df02 <- CC_auto_periods_df02 %>%
  group_by(country) %>%
  dplyr::summarize(country_convergence = mean.new(count_convergence_res02), 
                   count_country_periods = n()) 

saveRDS(CC_auto_periods_df02, "data/CC_auto_periods_df02.rds")


#### Plotting World Map: Figure 5 in Main Text ####

require(maps)
require(viridis)
theme_set(
  theme_void()
)


## Load World Map ##

world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="white", colour = "black")

#Reduce data to mean_data_frame
CC_country_map <- CC_country %>%
  rename(region = country) %>%
  mutate(region = ifelse(region == "United States of America", "USA",
                         ifelse(region =="United Kingdom", "UK", region)))

#Merge data with maps

CC_country_map <- world_map %>%
  left_join(CC_country_map, by = "region") %>%
  filter(region!= "Antarctica")

class(CC_country_map)

# Mapping 
title <- theme(
  plot.title = element_text(hjust = 0.5),
  plot.subtitle = element_text(hjust = 0.5), 
  legend.position = "right"
)


#Map
map_Figure8 <- ggplot(CC_country_map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = country_convergence), color = "black")+
  scale_fill_viridis_c(option = "viridis", na.value="grey") +
  labs(fill = "Percentage\nCongruence",
       title = "Autocratization Episodes ",
       subtitle = "Mean Congruence between Different Measures by Episodes") + 
  title +
  theme(legend.position="bottom")

ggsave("outputs/Figure5.png",dpi = 1200, width = 40, height = 23, units = "cm" )
ggsave("outputs/Figure5.pdf",dpi = 1200, width = 40, height = 23, units = "cm" )


###############################################################################################
###############################################################################################

#### Different set of Backsliders: Table A1 ####


vdem <- readRDS("VDem_autocratization10.rds")

vdem <- vdem %>% 
  group_by(country_name) %>%
  mutate(lag_year = dplyr::lag(year), 
         lag_year = ifelse(is.na(lag_year), year, lag_year))

vdem_con <- vdem %>%
  filter(auto_period01ci==1  | auto_period01_libdem_ci==1 |  auto_period_polity == 1 | 
           auto_period_fh == 1 )

## 1789 country years which were identified as autocratization year 

vdem_con <- vdem_con %>%
  dplyr::select(country_name, country_id, year, auto_period01ci, auto_decline01ci, 
                auto_period01_libdem_ci, auto_decline01_libdem_ci, auto_period_polity, 
         auto_decline_polity, auto_period_fh, auto_decline_fh, lag_year)


## Building measurement-unspecific periods ##

vdem_con <- vdem_con %>% 
  group_by(country_name) %>%
  mutate(lag_year = dplyr::lag(year), 
         lag_year = ifelse(is.na(lag_year), year, lag_year), 
         year_diff = lag_year-year, 
         year_diff = ifelse(year_diff == 0 | year_diff ==-1, 0, 1)) %>%
  mutate(group_id = cumsum(year_diff)) # group_id by each country for connected year 

vdem_con <- vdem_con %>% 
  mutate(auto_period_id_all = str_c(country_name, group_id, sep = "_")) %>%
  select(-c(lag_year, year_diff, group_id)) %>%
  select(country_name, country_id, year, auto_period_id_all, starts_with("auto_period"), starts_with("auto_decline"))


min.new <- function(v) {
  if (all(is.na(v))) { return(NA) } else { return(min(v, na.rm=T)) }
} 

max.new <- function(v) {
  if (all(is.na(v))) { return(NA) } else { return(max(v, na.rm=T)) }
}

mean.new <- function(v) {
  if (all(is.na(v))) { return(NA) } else { return(mean(v, na.rm=T)) }
}

sum.new <- function(v) {
  if (all(is.na(v))) { return(NA) } else { return(sum(v, na.rm=T)) }
}


vdem_con_test <- vdem_con %>%
  select(country_name, country_id, year, auto_period_id_all, auto_period01ci, 
         auto_period01_libdem_ci, auto_period_polity, auto_period_fh, auto_decline01ci,
         auto_decline01_libdem_ci, auto_decline_polity, auto_decline_fh) 

vdem_con_test <- vdem_con_test%>%
  group_by(auto_period_id_all) %>%
  summarise(across(c("auto_period01ci", "auto_period01_libdem_ci","auto_period_polity", "auto_period_fh"),
                   ~max.new(.x)))

vdem_con_years <- vdem_con %>%
  group_by(auto_period_id_all) %>%
  summarise(start_year = first(year), 
            end_year = last(year))

vdem_con_test <- vdem_con_test %>%
  left_join(vdem_con_years, by = "auto_period_id_all")


vdem_con_test <- vdem_con_test %>%
  mutate("Country" = str_split(auto_period_id_all, "_"))

vdem_con_test$Country <- unlist(vdem_con_test$Country)[c(TRUE,FALSE)]

vdem_con_test <- vdem_con_test %>%
  select(Country, everything(), - auto_period_id_all)

vdem_con_test <- vdem_con_test %>%
  mutate(congruence = ifelse(auto_period01ci ==1 & auto_period01_libdem_ci ==1 & auto_period_polity ==1 & 
                               auto_period_fh ==1, 1, 0),
         congruence = ifelse(is.na(congruence), 1, congruence)) %>%
  filter(congruence != 1)

vdem_con_test <- vdem_con_test %>%
  select(Country, start_year, end_year, auto_period01ci, auto_period01_libdem_ci, 
         auto_period_polity, auto_period_fh) %>%
  rename("Year Begin" = start_year, 
         "Year End" = end_year, 
         "EDI 0.1 + CI" = auto_period01ci, 
       "LDI 0.1 + CI" = auto_period01_libdem_ci, 
       "Polity IV 0.1" = auto_period_polity,
       "FH 0.1" = auto_period_fh) # 423 autocraitzation episodes: different set of backsliders

saveRDS(vdem_con_test, "data/CC_country_list.rds")


stargazer(vdem_con_test, summary=FALSE)

table(vdem_con_test$`EDI 0.1 + CI`)
table(vdem_con_test$`LDI 0.1 + CI`)
table(vdem_con_test$`Polity IV 0.1`)
table(vdem_con_test$`FH 0.1`)


#### Figure 6 Main Paper ####
#### Country Plots USA and Hungary ####

vdem <- readRDS("VDem_autocratization10.rds")


vdem <- vdem %>%
  mutate(e_fh_cl = ifelse(e_fh_cl==7, 1, 
                          ifelse(e_fh_cl==6, 2,
                                 ifelse(e_fh_cl==5, 3,
                                        ifelse(e_fh_cl==4, 4,
                                               ifelse(e_fh_cl==3, 5,
                                                      ifelse(e_fh_cl==2, 6, 
                                                             ifelse(e_fh_cl==1, 7, NA))))))))

vdem <- vdem %>%
  mutate(e_fh_pr = ifelse(e_fh_pr==7, 1, 
                          ifelse(e_fh_pr==6, 2,
                                 ifelse(e_fh_pr==5, 3,
                                        ifelse(e_fh_pr==4, 4,
                                               ifelse(e_fh_pr==3, 5,
                                                      ifelse(e_fh_pr==2, 6, 
                                                             ifelse(e_fh_pr==1, 7, NA))))))))


vdem <- vdem %>%
  mutate(e_fh_com = e_fh_cl + e_fh_pr) 
summary(vdem$e_fh_com)
summary(vdem$e_polity2)

table(vdem$e_polity2)

vdem$e_polity2  = (vdem$e_polity2--10)/(10--10)

vdem$e_polity2_res <- rescale(vdem$e_polity2 , to = c(0,1), from =  (c(-10, 10)))
vdem$e_fh_com <- rescale(vdem$e_fh_com , to = c(0,1), from = (c(2, 14)))


vdem_cases <- vdem %>%
  filter(country_name %in% c("United States of America", "Hungary")) %>%
  filter(year>=1990) %>%
  select(country_name, year, v2x_polyarchy, v2x_libdem, e_polity2, e_fh_com, auto_period01ci, 
         auto_period01_libdem_ci, auto_period_polity, auto_period_fh, 
         v2x_freexp_altinf, v2x_frassoc_thick, v2x_suffr, v2xel_frefair, v2x_elecoff, 
         v2xcl_rol, v2x_jucon, v2xlg_legcon, v2lgbicam)

vdem_cases_usa <- vdem_cases %>%
  filter(country_name == "United States of America")

vdem_cases_usa_changes <- vdem_cases_usa %>%
  mutate(v2x_freexp_altinf_lag = dplyr::lag(v2x_freexp_altinf), 
         v2x_frassoc_thick_lag = dplyr::lag(v2x_frassoc_thick),
         v2x_suffr_lag = dplyr::lag(v2x_suffr), 
         v2xel_frefair_lag = dplyr::lag(v2xel_frefair), 
         v2x_elecoff_lag = dplyr::lag(v2x_elecoff), 
         v2xcl_rol_lag = dplyr::lag(v2xcl_rol), 
         v2x_jucon_lag = dplyr::lag(v2x_jucon),
         v2xlg_legcon_lag = dplyr::lag(v2xlg_legcon),
         v2lgbicam_lag = dplyr::lag(v2lgbicam)) %>%
  filter(auto_period01ci==1) 
  

vdem_cases_hungary <- vdem_cases %>%
  filter(country_name == "Hungary")


vdem_cases_hungary_changes <- vdem_cases_hungary %>%
  mutate(v2x_freexp_altinf_lag = dplyr::lag(v2x_freexp_altinf), 
         v2x_frassoc_thick_lag = dplyr::lag(v2x_frassoc_thick),
         v2x_suffr_lag = dplyr::lag(v2x_suffr), 
         v2xel_frefair_lag = dplyr::lag(v2xel_frefair), 
         v2x_elecoff_lag = dplyr::lag(v2x_elecoff), 
         v2xcl_rol_lag = dplyr::lag(v2xcl_rol), 
         v2x_jucon_lag = dplyr::lag(v2x_jucon),
         v2xlg_legcon_lag = dplyr::lag(v2xlg_legcon),
         v2lgbicam_lag = dplyr::lag(v2lgbicam)) %>%
  filter(auto_period01ci==1) 

## United States ##

colors <- c("v2x_polyarchy" = "black", "v2x_libdem" = "green", "e_polity2" = "blue", "e_fh_com" = "grey")

plot1 <- ggplot(vdem_cases_usa) +
  geom_line(aes(x = year, y = v2x_polyarchy, color = "EDI"), linetype = "solid", size = 1) +
  geom_line(aes(x = year, y = v2x_libdem, color = "LDI"), linetype = "dashed", size = 1) +
  geom_line(aes(x = year, y = e_polity2, color = "Polity IV"), linetype = "dotted",size = 1) +
  geom_line(aes(x = year, y = e_fh_com, color = "FH"), linetype = "longdash",size = 1) +
  ylim(0.25,1) +
  geom_line(data = vdem_cases_usa[vdem_cases_usa$year >= 2015,], 
            aes(x = year, y = v2x_polyarchy), color = "red", size =1) +
  geom_line(data = vdem_cases_usa[vdem_cases_usa$year >= 2015,],
            aes(x = year, y = v2x_libdem), color = "red", size = 1) +
  geom_line(data = vdem_cases_usa[vdem_cases_usa$year >=2015 & vdem_cases_usa$year <=2016,],
            aes(x = year, y = e_polity2), color = "red", size = 1) +
  geom_text(aes(x=2020, label="EDI", y=0.8)) +
  geom_text(aes(x=2020, label="LDI", y=0.7)) +
  geom_text(aes(x=2020, label="FH", y=0.92)) +
  geom_text(aes(x=2020, label="Polity", y=0.89)) +
  theme_classic() +
  labs(y = "Democracy Score", 
       x = "Year", 
       color = "", 
       title = " Unites States of America")


## Hungary ##

label_scores <- c("EDI", "LDI", "FH", "Polity")

plot2 <- ggplot(vdem_cases_hungary) +
  geom_line(aes(x = year, y = v2x_polyarchy, color = "EDI"), linetype = "solid", size = 1) +
  geom_line(aes(x = year, y = v2x_libdem, color = "LDI"), linetype = "dashed", size = 1) +
  geom_line(aes(x = year, y = e_polity2, color = "Polity IV"), linetype = "dotted",size = 1) +
  geom_line(aes(x = year, y = e_fh_com, color = "FH"), linetype = "longdash",size = 1) +
  ylim(0.25,1) +
  geom_line(data = vdem_cases_hungary[vdem_cases_hungary$year >= 2006,], 
            aes(x = year, y = v2x_polyarchy), color = "red", size =1) +
  geom_line(data = vdem_cases_hungary[vdem_cases_hungary$year >= 2009 & vdem_cases_usa$year <=2015,],
            aes(x = year, y = v2x_libdem), color = "red", size = 1) +
  geom_line(data = vdem_cases_hungary[vdem_cases_hungary$year >= 2010,], 
            aes(x = year, y = e_fh_com), color = "red", size =1) +
  geom_text(aes(x=2020, label="EDI", y=0.5)) +
  geom_text(aes(x=2020, label="LDI", y=0.4)) +
  geom_text(aes(x=2020, label="FH", y=0.66)) +
  geom_text(aes(x=2020, label="Polity", y=1)) +
  theme_classic() +
  labs(y = "Democracy Score", 
       x = "Year", 
       color = "", 
       title = "Hungary")


Figure9_paper <- ggarrange(plot1, plot2, 
                           ncol = 2, nrow = 1, 
                           common.legend = TRUE)

ggsave("outputs/Figure6.png",dpi = 1200, width = 30, height = 15, units = "cm")
ggsave("outputs/Figure6.pdf",dpi = 1200, width = 30, height = 15, units = "cm")


#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################

