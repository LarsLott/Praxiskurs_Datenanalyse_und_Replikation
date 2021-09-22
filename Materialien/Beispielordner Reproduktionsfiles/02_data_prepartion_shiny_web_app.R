#### "Conceptualizing and Measuring Autocratization Episodes" ####
# authors: "Pelke, Lars; Croissant, Aurel"
# date: 20201-01-12
# journal: Swiss Political Science Review
# written under "R version 3.6.0 (2019-07-05)"

#### Preliminaries ####

R.version$version.string

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
if(!is.element("scales", installed.packages()[,1]))
{install.packages("scales")
}else {print("The requested library is already installed.")}
library(scales)
if(!is.element("plotly", installed.packages()[,1]))
{install.packages("plotly")
}else {print("The requested library is already installed.")}
library(plotly)
if(!is.element("sf", installed.packages()[,1]))
{install.packages("sf")
}else {print("The requested library is already installed.")}
library(sf)
if(!is.element("rnaturalearth", installed.packages()[,1]))
{install.packages("rnaturalearth")
}else {print("The requested library is already installed.")}
library(rnaturalearth)
if(!is.element("htmlwidgets", installed.packages()[,1]))
{install.packages("htmlwidgets")
}else {print("The requested library is already installed.")}
library(htmlwidgets)

#### Prerequisites ####

# run 01_data_wrangling before starting this file 

#### Data Preparation ####

#### Load Map ####

world_map <- ne_countries(returnclass = "sf")
class(world_map)

world_map$cown <- countrycode(world_map$name_long,"country.name", "cown", warn = TRUE )

# change cow codes for countries with warnings
world_map$cown[world_map$name_long == "Dem. Rep. Korea"] <- 731
world_map$cown[world_map$name_long == "Palestine"] <- 1020
world_map$cown[world_map$name_long == "Serbia"] <- 345

# drop Antarctica 
world_map <- world_map %>%
  filter(subunit != "Antarctica") 


#### Data preparation EDI, LDI, Polity IV and Freedom House ####

#### EDI 0.1 # CI ####

vdem <- vdem_org

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_v2x_polyarchy = lag(v2x_polyarchy, 1))

vdem <- vdem %>%
  dplyr::left_join(vdem_auto_EDI01ci, by = c("country_name", "year")) %>%
  select(country_name, year, v2x_polyarchy, lag_v2x_polyarchy, auto_period01ci, auto_decline01ci, auto_period_id01ci, 
         last_v2x_regime, lag_v2x_regime) %>%
  filter(year >= 1990) %>%
  group_by(auto_period_id01ci) %>%
  mutate(auto_decline_period = first(auto_decline01ci)) %>%
  ungroup()

vdem$cown <- countrycode(vdem$country_name,"country.name", "cown", warn = TRUE )

# change cow codes for countries with warnings
vdem$cown[vdem$country_name == "Hong Kong"] <- 715
vdem$cown[vdem$country_name == "Palestine/British Mandate"] <- 1020
vdem$cown[vdem$country_name == "Palestine/Gaza"] <- 1020
vdem$cown[vdem$country_name == "Palestine/West Bank"] <- 1020
vdem$cown[vdem$country_name == "Republic of Vietnam"] <- 817
vdem$cown[vdem$country_name == "Serbia"] <- 345

## Left Join World Map 

vdem_map1 <- world_map %>%
  dplyr::left_join(vdem, by = c("cown")) %>%
  dplyr::mutate(auto_period = ifelse(auto_period01ci ==0, NA, auto_period01ci)) %>%
  dplyr::group_by(auto_period_id01ci) %>%
  dplyr::mutate(first_year = first(year), 
                last_year = last(year)) %>%
  ungroup() %>%
  select(geounit, auto_period, name, year, first_year, last_year, auto_decline_period, last_v2x_regime, 
         lag_v2x_regime, geometry) %>%
  mutate(Index = "EDI 0.1 + CI")

vdem_map1$year <- as.integer(vdem_map1$year)
class(vdem_map1$year)

#### EDI 0.1 # CI start 005 ####

vdem <- vdem_org

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_v2x_polyarchy = lag(v2x_polyarchy, 1))

vdem <- vdem %>%
  dplyr::left_join(vdem_auto_EDI01ci_start005, by = c("country_name", "year")) %>%
  select(country_name, year, v2x_polyarchy, lag_v2x_polyarchy, auto_period01ci_start005, 
         auto_decline01ci_start005, auto_period_id01ci_start005, 
         last_v2x_regime, lag_v2x_regime) %>%
  filter(year >= 1990) %>%
  group_by(auto_period_id01ci_start005) %>%
  mutate(auto_decline_period = first(auto_decline01ci_start005)) %>%
  ungroup()

vdem$cown <- countrycode(vdem$country_name,"country.name", "cown", warn = TRUE )

# change cow codes for countries with warnings
vdem$cown[vdem$country_name == "Hong Kong"] <- 715
vdem$cown[vdem$country_name == "Palestine/British Mandate"] <- 1020
vdem$cown[vdem$country_name == "Palestine/Gaza"] <- 1020
vdem$cown[vdem$country_name == "Palestine/West Bank"] <- 1020
vdem$cown[vdem$country_name == "Republic of Vietnam"] <- 817
vdem$cown[vdem$country_name == "Serbia"] <- 345

## Left Join World Map 

vdem_map2 <- world_map %>%
  dplyr::left_join(vdem, by = c("cown")) %>%
  dplyr::mutate(auto_period = ifelse(auto_period01ci_start005 ==0, NA, auto_period01ci_start005)) %>%
  dplyr::group_by(auto_period_id01ci_start005) %>%
  dplyr::mutate(first_year = first(year), 
                last_year = last(year)) %>%
  ungroup() %>%
  select(geounit, auto_period, name, year, first_year, last_year, auto_decline_period, last_v2x_regime, 
         lag_v2x_regime, geometry) %>%
  mutate(Index = "EDI 0.1 + CI (start 0.05)")

vdem_map2$year <- as.integer(vdem_map2$year)
class(vdem_map2$year)

###########################################################################################

#### LDI 0.1 # CI ####
vdem <- vdem_org

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_v2x_libdem = lag(v2x_libdem, 1))

vdem <- vdem %>%
  dplyr::left_join(vdem_auto_LDI01ci, by = c("country_name", "year")) %>%
  select(country_name, year, v2x_libdem, lag_v2x_libdem, auto_period01_libdem_ci, auto_decline01_libdem_ci, auto_period_id01_libdem_ci, 
         last_v2x_regime, lag_v2x_regime) %>%
  filter(year >= 1990) %>%
  group_by(auto_period_id01_libdem_ci) %>%
  mutate(auto_decline_period = first(auto_decline01_libdem_ci)) %>%
  ungroup()

vdem$cown <- countrycode(vdem$country_name,"country.name", "cown", warn = TRUE )

# change cow codes for countries with warnings
vdem$cown[vdem$country_name == "Hong Kong"] <- 715
vdem$cown[vdem$country_name == "Palestine/British Mandate"] <- 1020
vdem$cown[vdem$country_name == "Palestine/Gaza"] <- 1020
vdem$cown[vdem$country_name == "Palestine/West Bank"] <- 1020
vdem$cown[vdem$country_name == "Republic of Vietnam"] <- 817
vdem$cown[vdem$country_name == "Serbia"] <- 345

## Left Join World Map 

vdem_map3 <- world_map %>%
  dplyr::left_join(vdem, by = c("cown")) %>%
  dplyr::mutate(auto_period = ifelse(auto_period01_libdem_ci ==0, NA, auto_period01_libdem_ci)) %>%
  dplyr::group_by(auto_period_id01_libdem_ci) %>%
  dplyr::mutate(first_year = first(year), 
                last_year = last(year)) %>%
  ungroup() %>%
  select(geounit, auto_period, name, year, first_year, last_year, auto_decline_period, last_v2x_regime, lag_v2x_regime, 
         geometry) %>%
  mutate(Index = "LDI 0.1 + CI")

vdem_map3$year <- as.integer(vdem_map3$year)
class(vdem_map3$year)


#### LDI 0.1 # CI Start 005 ####
vdem <- vdem_org

vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_v2x_libdem = lag(v2x_libdem, 1))

vdem <- vdem %>%
  dplyr::left_join(vdem_auto_LDI01ci_start005, by = c("country_name", "year")) %>%
  select(country_name, year, v2x_libdem, lag_v2x_libdem, auto_period01_libdem_ci_start005, 
         auto_decline01_libdem_ci_start005, auto_period_id01_libdem_ci_start005, 
         last_v2x_regime, lag_v2x_regime) %>%
  filter(year >= 1990) %>%
  group_by(auto_period_id01_libdem_ci_start005) %>%
  mutate(auto_decline_period = first(auto_decline01_libdem_ci_start005)) %>%
  ungroup()

vdem$cown <- countrycode(vdem$country_name,"country.name", "cown", warn = TRUE )

# change cow codes for countries with warnings
vdem$cown[vdem$country_name == "Hong Kong"] <- 715
vdem$cown[vdem$country_name == "Palestine/British Mandate"] <- 1020
vdem$cown[vdem$country_name == "Palestine/Gaza"] <- 1020
vdem$cown[vdem$country_name == "Palestine/West Bank"] <- 1020
vdem$cown[vdem$country_name == "Republic of Vietnam"] <- 817
vdem$cown[vdem$country_name == "Serbia"] <- 345

## Left Join World Map 

vdem_map4 <- world_map %>%
  dplyr::left_join(vdem, by = c("cown")) %>%
  dplyr::mutate(auto_period = ifelse(auto_period01_libdem_ci_start005 ==0, NA, auto_period01_libdem_ci_start005)) %>%
  dplyr::group_by(auto_period_id01_libdem_ci_start005) %>%
  dplyr::mutate(first_year = first(year), 
                last_year = last(year)) %>%
  ungroup() %>%
  select(geounit, auto_period, name, year, first_year, last_year, auto_decline_period, last_v2x_regime, lag_v2x_regime, 
         geometry) %>%
  mutate(Index = "LDI 0.1 + CI (Start 0.05)")

vdem_map4$year <- as.integer(vdem_map4$year)
class(vdem_map11$year)

###############################################################
## Polity 0.1 ##

vdem <- vdem_org 

#### Rescale Polity IV to [0,1] ####

vdem <- vdem %>%
  mutate(e_p_polity = ifelse(e_p_polity <= -11, NA, e_p_polity))

vdem$e_p_polity <- rescale(vdem$e_p_polity, to = c(0,1)) # rescaled Polity IV Variable to [0,1]
summary(vdem$e_p_polity)


vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_e_p_polity = lag(e_p_polity, 1))

vdem <- vdem %>%
  dplyr::left_join(vdem_auto_polity, by = c("country_name", "year")) %>%
  select(country_name, year, e_p_polity, lag_e_p_polity, auto_period_polity_id, auto_decline_polity, auto_period_polity, 
         last_v2x_regime, lag_v2x_regime) %>%
  filter(year >= 1990) %>%
  group_by(auto_period_polity_id) %>%
  mutate(auto_decline_period = first(auto_decline_polity)) %>%
  ungroup()

vdem$cown <- countrycode(vdem$country_name,"country.name", "cown", warn = TRUE )

# change cow codes for countries with warnings
vdem$cown[vdem$country_name == "Hong Kong"] <- 715
vdem$cown[vdem$country_name == "Palestine/British Mandate"] <- 1020
vdem$cown[vdem$country_name == "Palestine/Gaza"] <- 1020
vdem$cown[vdem$country_name == "Palestine/West Bank"] <- 1020
vdem$cown[vdem$country_name == "Republic of Vietnam"] <- 817
vdem$cown[vdem$country_name == "Serbia"] <- 345

## Left Join World Map 

vdem_map5 <- world_map %>%
  dplyr::left_join(vdem, by = c("cown")) %>%
  dplyr::mutate(auto_period = ifelse(auto_period_polity ==0, NA, auto_period_polity)) %>%
  dplyr::group_by(auto_period_polity_id) %>%
  dplyr::mutate(first_year = first(year), 
                last_year = last(year)) %>%
  ungroup() %>%
  select(geounit, auto_period, name, year, first_year, last_year, auto_decline_period, last_v2x_regime, lag_v2x_regime, 
         geometry) %>%
  mutate(Index = "Polity 0.1")

vdem_map5$year <- as.integer(vdem_map5$year)
class(vdem_map5$year)

###############################################################
## Polity 0.1 (Start 005)##

vdem <- vdem_org 

#### Rescale Polity IV to [0,1] ####

vdem <- vdem %>%
  mutate(e_p_polity = ifelse(e_p_polity <= -11, NA, e_p_polity))

vdem$e_p_polity <- rescale(vdem$e_p_polity, to = c(0,1)) # rescaled Polity IV Variable to [0,1]
summary(vdem$e_p_polity)


vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_e_p_polity = lag(e_p_polity, 1))

vdem <- vdem %>%
  dplyr::left_join(vdem_auto_polity01_st005, by = c("country_name", "year")) %>%
  select(country_name, year, e_p_polity, lag_e_p_polity, auto_period_polity_id01_st005, 
         auto_decline_polity01_st005, auto_period_polity01_st005, 
         last_v2x_regime, lag_v2x_regime) %>%
  filter(year >= 1990) %>%
  group_by(auto_period_polity_id01_st005) %>%
  mutate(auto_decline_period = first(auto_decline_polity01_st005)) %>%
  ungroup()

vdem$cown <- countrycode(vdem$country_name,"country.name", "cown", warn = TRUE )

# change cow codes for countries with warnings
vdem$cown[vdem$country_name == "Hong Kong"] <- 715
vdem$cown[vdem$country_name == "Palestine/British Mandate"] <- 1020
vdem$cown[vdem$country_name == "Palestine/Gaza"] <- 1020
vdem$cown[vdem$country_name == "Palestine/West Bank"] <- 1020
vdem$cown[vdem$country_name == "Republic of Vietnam"] <- 817
vdem$cown[vdem$country_name == "Serbia"] <- 345

## Left Join World Map 

vdem_map6 <- world_map %>%
  dplyr::left_join(vdem, by = c("cown")) %>%
  dplyr::mutate(auto_period = ifelse(auto_decline_polity01_st005 ==0, NA, auto_decline_polity01_st005)) %>%
  dplyr::group_by(auto_period_polity_id01_st005) %>%
  dplyr::mutate(first_year = first(year), 
                last_year = last(year)) %>%
  ungroup() %>%
  select(geounit, auto_period, name, year, first_year, last_year, auto_decline_period, last_v2x_regime, lag_v2x_regime, 
         geometry) %>%
  mutate(Index = "Polity 0.1 (Start 0.05)")

vdem_map6$year <- as.integer(vdem_map6$year)
class(vdem_map6$year)


###############################################################
## Freedom House 0.1 ##

vdem <- vdem_org 

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

vdem$e_fh_com <- rescale(vdem$e_fh_com, to = c(0,1)) # rescaled Freedom House Variable to [0,1]


vdem <- vdem %>%
  group_by(country_name) %>%
  mutate(lag_e_fh_com = lag(e_fh_com, 1))

vdem <- vdem %>%
  dplyr::left_join(vdem_auto_fh, by = c("country_name", "year")) %>%
  select(country_name, year, e_fh_com, lag_e_fh_com, auto_period_fh_id, auto_decline_fh, auto_period_fh, last_v2x_regime, lag_v2x_regime) %>%
  filter(year >= 1990) %>%
  group_by(auto_period_fh_id) %>%
  mutate(auto_decline_period = first(auto_decline_fh)) %>%
  ungroup()

vdem$cown <- countrycode(vdem$country_name,"country.name", "cown", warn = TRUE )

# change cow codes for countries with warnings
vdem$cown[vdem$country_name == "Hong Kong"] <- 715
vdem$cown[vdem$country_name == "Palestine/British Mandate"] <- 1020
vdem$cown[vdem$country_name == "Palestine/Gaza"] <- 1020
vdem$cown[vdem$country_name == "Palestine/West Bank"] <- 1020
vdem$cown[vdem$country_name == "Republic of Vietnam"] <- 817
vdem$cown[vdem$country_name == "Serbia"] <- 345

## Left Join World Map 

vdem_map7 <- world_map %>%
  dplyr::left_join(vdem, by = c("cown")) %>%
  dplyr::mutate(auto_period = ifelse(auto_period_fh ==0, NA, auto_period_fh)) %>%
  dplyr::group_by(auto_period_fh_id) %>%
  dplyr::mutate(first_year = first(year), 
                last_year = last(year)) %>%
  ungroup() %>%
  select(geounit, auto_period, name, year, first_year, last_year, auto_decline_period, last_v2x_regime, lag_v2x_regime, 
         geometry) %>%
  mutate(Index = "Freedom House 0.1")

vdem_map7$year <- as.integer(vdem_map7$year)
class(vdem_map7$year)


##### Data Preparation #####

vdem_map_master <- rbind(vdem_map1, vdem_map2, vdem_map3, vdem_map4, vdem_map5, vdem_map6, vdem_map7)

# prepare Regime Types data 

vdem_map_master <- vdem_map_master %>%
  mutate(last_v2x_regime = ifelse(is.na(auto_period), " ", last_v2x_regime), 
         lag_v2x_regime = ifelse(is.na(auto_period), " ", lag_v2x_regime)) %>%
  mutate(last_v2x_regime = case_when(last_v2x_regime==0 ~ "Closed Autocracy",
                                    last_v2x_regime==1 ~ "Electoral Autocracy",
                                    last_v2x_regime==2 ~ "Electoral Democracy",
                                    last_v2x_regime==3 ~ "Liberal Democracy"), 
         lag_v2x_regime = case_when(lag_v2x_regime==0 ~ "Closed Autocracy",
                                    lag_v2x_regime==1 ~ "Electoral Autocracy",
                                    lag_v2x_regime==2 ~ "Electoral Democracy",
                                    lag_v2x_regime==3 ~ "Liberal Democracy"))

table(vdem_map_master$last_v2x_regime)
table(vdem_map_master$lag_v2x_regime)

# round decimal places #
vdem_map_master$auto_decline_period <- round(vdem_map_master$auto_decline_period, digits = 2)

summary(vdem_map_master$year)
table(vdem_map_master$Index)

vdem_map_master <- vdem_map_master[complete.cases(vdem_map_master$year), ]
names(st_geometry(vdem_map_master)) = NULL

saveRDS(vdem_map_master, "data/vdem_map_master10.rds")

#### Preparation for Regime Types that affected by Autocratization ####

vdem_map_master %>%
  group_by(lag_v2x_regime, last_v2x_regime)%>%
  count()

vdem_master <- vdem_map_master %>%
  mutate(auto_forms = case_when(lag_v2x_regime == "Liberal Democracy" & last_v2x_regime == "Electoral Democracy" ~ "LD to ED", 
                                lag_v2x_regime == "Liberal Democracy" & last_v2x_regime == "Electoral Autocracy" ~ "LD to EA",
                                lag_v2x_regime == "Liberal Democracy" & last_v2x_regime == "Closed Autocracy" ~ "LD to CA",
                                lag_v2x_regime == "Electoral Democracy" & last_v2x_regime == "Electoral Autocracy" ~ "ED to EA",
                                lag_v2x_regime == "Electoral Democracy" & last_v2x_regime == "Closed Autocracy" ~ "ED to CA",
                                lag_v2x_regime == "Electoral Autocracy" & last_v2x_regime == "Closed Autocracy" ~ "EA to CA",
                                lag_v2x_regime == "Liberal Democracy" & last_v2x_regime == "Liberal Democracy" ~ "no regime type change", 
                                lag_v2x_regime == "Electoral Democracy" & last_v2x_regime == "Electoral Democracy" ~ "no regime type change",
                                lag_v2x_regime == "Electoral Autocracy" & last_v2x_regime == "Electoral Autocracy" ~ "no regime type change",
                                lag_v2x_regime == "Closed Autocracy" & last_v2x_regime == "Closed Autocracy" ~ "no regime type change", 
                                lag_v2x_regime == "Closed Autocracy" & last_v2x_regime == "Electoral Autoracy" ~ "adverse change",
                                lag_v2x_regime == "Electoral Autocracy" & last_v2x_regime == "Electoral Democracy" ~ "adverse change",
                                lag_v2x_regime == "Electoral Democracy" & last_v2x_regime == "Liberal Democracy" ~ "adverse change"))

vdem_master <- vdem_master %>%
  mutate(auto_periods_id = str_c(name, first_year, last_year, sep = "_"))

vdem_master <- vdem_master %>%
  filter(auto_period ==1) %>%
  group_by(Index, auto_periods_id) %>%
  dplyr::summarize(country_name = first (name), 
                   start_year = first(year), 
                   end_year = last(year), 
                   auto_forms = first(auto_forms))

vdem_master <- vdem_master %>%
  group_by(Index, auto_forms) %>%
  dplyr::summarize(number_auto_forms = n()) %>%
  drop_na(auto_forms)

table(vdem_master$auto_forms)

  
vdem_master <- vdem_master %>%
  mutate(auto_forms = case_when(auto_forms == "EA to CA" ~ "Electoral Autocracy to Closed Autocracy", 
                                auto_forms == "ED to CA" ~ "Electoral Democracy to Closed Autocracy", 
                                auto_forms == "ED to EA" ~ "Electoral Democracy to Electoral Autocracy", 
                                auto_forms == "LD to ED" ~ "Liberal Democracy to Electoral Democracy", 
                                auto_forms == "LD to CA" ~ "Liberal Democracy to Closed Autocracy", 
                                auto_forms == "no regime type change" ~ "No regime type change", 
                                auto_forms == "adverse change" ~ "Adverse Change to more democratic quality")) %>%
  drop_na(auto_forms)

table(vdem_master$auto_forms)

#### Figure Supplementary Appendix ####

library(viridis)

auto_forms <- ggplot(vdem_master, aes(x = auto_forms, y = number_auto_forms, fill = Index)) +
  geom_col(position = "dodge", color = "black") +
  theme_bw() +
  scale_color_viridis(discrete = TRUE, option = "C")+
  scale_fill_viridis(discrete = TRUE) +
  labs(x =" Autocratization Forms", 
       y = "Number of Autocratization Periods") +
  coord_flip() +
  theme(legend.position="bottom")

ggplotly(auto_forms, tooltip =  c("Index", "number_auto_forms"))

saveRDS(vdem_master, "data/vdem_auto_forms10.rds")

##########################################################################################################
#########################################################################################################
#### Data Preparation Coungruence World Map ####

#### Load Map ####

world_map <- ne_countries(returnclass = "sf")
class(world_map)

world_map$cown <- countrycode(world_map$name_long,"country.name", "cown", warn = TRUE )

# change cow codes for countries with warnings
world_map$cown[world_map$name_long == "Dem. Rep. Korea"] <- 731
world_map$cown[world_map$name_long == "Palestine"] <- 1020
world_map$cown[world_map$name_long == "Serbia"] <- 345

# drop Antarctica 
world_map <- world_map %>%
  filter(subunit != "Antarctica") 

#### Prepare Country Convergence Data ####

CC_country <- readRDS("data/CC_country.rds")

CC_country$cown <- countrycode(CC_country$country,"country.name", "cown", warn = TRUE )

# change cow codes for countries with warnings
CC_country$cown[CC_country$country == "Hong Kong"] <- 715
CC_country$cown[CC_country$country == "Palestine/British Mandate"] <- 1020
CC_country$cown[CC_country$country == "Palestine/Gaza"] <- 1020
CC_country$cown[CC_country$country == "Palestine/West Bank"] <- 1020
CC_country$cown[CC_country$country == "Republic of Vietnam"] <- 817
CC_country$cown[CC_country$country == "Serbia"] <- 345
CC_country$cown[CC_country$country == "Eswatini"] <- 572 

summary(CC_country$cown)

## Left Join World Map 

CC_country_map <- world_map %>%
  dplyr::left_join(CC_country, by = c("cown")) 

CC_country_map$country_convergence <- round(CC_country_map$country_convergence, digits = 1)


CC_country_map <- CC_country_map %>%
  mutate(thresholds = "all")

#### Prepare Country Convergence Data 005 ####

CC_country005 <- readRDS("data/CC_auto_periods_df005.rds")

CC_country005$cown <- countrycode(CC_country005$country,"country.name", "cown", warn = TRUE )

# change cow codes for countries with warnings
CC_country005$cown[CC_country005$country == "Hong Kong"] <- 715
CC_country005$cown[CC_country005$country == "Palestine/British Mandate"] <- 1020
CC_country005$cown[CC_country005$country == "Palestine/Gaza"] <- 1020
CC_country005$cown[CC_country005$country == "Palestine/West Bank"] <- 1020
CC_country005$cown[CC_country005$country == "Republic of Vietnam"] <- 817
CC_country005$cown[CC_country005$country == "Serbia"] <- 345
CC_country005$cown[CC_country005$country == "Eswatini"] <- 572 

summary(CC_country005$cown)

## Left Join World Map 

CC_country_map005 <- world_map %>%
  dplyr::left_join(CC_country005, by = c("cown")) 

CC_country_map005$country_convergence <- round(CC_country_map005$country_convergence, digits = 1)

CC_country_map005 <- CC_country_map005 %>%
  mutate(thresholds = "0.05")

#### Prepare Country Convergence Data 01 ####

CC_country01 <- readRDS("data/CC_auto_periods_df01.rds")

CC_country01$cown <- countrycode(CC_country01$country,"country.name", "cown", warn = TRUE )

# change cow codes for countries with warnings
CC_country01$cown[CC_country01$country == "Hong Kong"] <- 715
CC_country01$cown[CC_country01$country == "Palestine/British Mandate"] <- 1020
CC_country01$cown[CC_country01$country == "Palestine/Gaza"] <- 1020
CC_country01$cown[CC_country01$country == "Palestine/West Bank"] <- 1020
CC_country01$cown[CC_country01$country == "Republic of Vietnam"] <- 817
CC_country01$cown[CC_country01$country == "Serbia"] <- 345
CC_country01$cown[CC_country01$country == "Eswatini"] <- 572 

summary(CC_country01$cown)

## Left Join World Map 

CC_country_map01 <- world_map %>%
  dplyr::left_join(CC_country01, by = c("cown")) 

CC_country_map01$country_convergence <- round(CC_country_map01$country_convergence, digits = 1)

CC_country_map01 <- CC_country_map01 %>%
  mutate(thresholds = "0.1")


#### Prepare Country Convergence Data 015 ####

CC_country015 <- readRDS("data/CC_auto_periods_df015.rds")

CC_country015$cown <- countrycode(CC_country015$country,"country.name", "cown", warn = TRUE )

# change cow codes for countries with warnings
CC_country015$cown[CC_country015$country == "Hong Kong"] <- 715
CC_country015$cown[CC_country015$country == "Palestine/British Mandate"] <- 1020
CC_country015$cown[CC_country015$country == "Palestine/Gaza"] <- 1020
CC_country015$cown[CC_country015$country == "Palestine/West Bank"] <- 1020
CC_country015$cown[CC_country015$country == "Republic of Vietnam"] <- 817
CC_country015$cown[CC_country015$country == "Serbia"] <- 345
CC_country015$cown[CC_country015$country == "Eswatini"] <- 572 

summary(CC_country015$cown)

## Left Join World Map 

CC_country_map015 <- world_map %>%
  dplyr::left_join(CC_country015, by = c("cown")) 

CC_country_map015$country_convergence <- round(CC_country_map015$country_convergence, digits = 1)

CC_country_map015 <- CC_country_map015 %>%
  mutate(thresholds = "0.15")


#### Prepare Country Convergence Data 015 ####

CC_country02 <- readRDS("data/CC_auto_periods_df02.rds")

CC_country02$cown <- countrycode(CC_country02$country,"country.name", "cown", warn = TRUE )

# change cow codes for countries with warnings
CC_country02$cown[CC_country02$country == "Hong Kong"] <- 715
CC_country02$cown[CC_country02$country == "Palestine/British Mandate"] <- 1020
CC_country02$cown[CC_country02$country == "Palestine/Gaza"] <- 1020
CC_country02$cown[CC_country02$country == "Palestine/West Bank"] <- 1020
CC_country02$cown[CC_country02$country == "Republic of Vietnam"] <- 817
CC_country02$cown[CC_country02$country == "Serbia"] <- 345
CC_country02$cown[CC_country02$country == "Eswatini"] <- 572 

summary(CC_country02$cown)

## Left Join World Map 

CC_country_map02 <- world_map %>%
  dplyr::left_join(CC_country02, by = c("cown")) 

CC_country_map02$country_convergence <- round(CC_country_map02$country_convergence, digits = 1)

CC_country_map02 <- CC_country_map02 %>%
  mutate(thresholds = "0.2")

#### prepare data for map ####

cc_country_map_all <- rbind(CC_country_map, CC_country_map005, CC_country_map01, CC_country_map015, CC_country_map02)

saveRDS(cc_country_map_all, "data/cc_country_map.rds")



