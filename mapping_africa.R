library(sf)
library(raster)
library(dplyr)
library(spData)
library(tmap)
library(leaflet)
library(cartogram)
library(tidyverse)

gain <- readr::read_csv("data/resources/gain/gain.csv")

gain_2017 <- gain %>% 
  filter(ISO3 %in% c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CV", "CMR", "CAF", "TCD", "COM", "COD",
                     "COG", "CIV", "DJI", "EGY", "ERI", "SWZ", "ETH", "GAB", "GMB", "GHA", "GIN", "GNB",
                     "KEN", "LSO", "LBR", "LBY", "MDG", "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM",
                     "NER", "NGA", "RWA", "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "TZA",
                     "TGO", "TUN", "UGA", "ZMB", "ZWE")) %>% 
  gather(year, gain, '1995':'2017') %>% 
  filter(year == 2017) %>% 
  rename(name = Name) %>% 
  distinct()

africa <-  world %>%
  filter(continent == "Africa", !is.na(iso_a2)) %>%
  left_join(worldbank_df, by = "iso_a2") %>% 
  left_join(gain_2017, by = "name") %>% 
  dplyr::select(name, subregion, gdpPercap, HDI, pop_growth, gain) %>% 
  st_transform("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25")

plot(africa["gain"])
tm_shape(africa) + tm_polygons("gain")

# NOTRE DAM GAIN INDEX MAP

ggplot(africa) +
  geom_sf(aes(geometry = geom, fill = gain)) +
  theme_minimal() + 
  scale_fill_viridis_c(option = "D") +
  labs(title = "ND-GAIN Country Index for Africa", caption = "Source: Notre Dame Global Adaptation Initiative 2020") +
  theme(text = element_text(family = "Georgia"), )


# Map of deforestation Africa

forest <- readxl::read_excel("data/FRA_africa.xlsx")

library(RColorBrewer)

forest <- forest %>% 
  gather(year_range, change_rate, `n1990_2015`:`n2000_2015`) %>%
  rename(name = country) %>% 
  mutate(change_rate = as.numeric(change_rate)) %>% 
  distinct()

forest$name <- iconv(forest$name, from = "UTF-8", to = "ASCII//TRANSLIT") # take accents off the country names

africa_f <-  world %>%
  filter(continent == "Africa", !is.na(iso_a2)) %>%
  left_join(worldbank_df, by = "iso_a2") %>% 
  left_join(gain_2017, by = "name") %>% 
  left_join(forest, by = "name") %>% 
  dplyr::select(name, subregion, gdpPercap, HDI, pop_growth, gain, year_range, change_rate) %>% 
  st_transform("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25")

# Mapping deforestation in Africa

africa_f %>% 
  filter(year_range == "n1990_2015") %>% 
ggplot() +
  geom_sf(aes(geometry = geom, fill = change_rate)) +
  scale_fill_viridis_d(option = "D", na.value = "#D5D5D3") +
  theme_minimal() +
  labs(title = "Rate of change in forest area coverage across Africa between 1990-2015") +
  theme(text = element_text(family = "Verdana Pro"))


