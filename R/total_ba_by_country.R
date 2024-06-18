# sum burned area by country
library(tidyverse)
library(vroom)
list.files("data/out_csvs_08mar22/", full.names = T) -> dd

result <- data.frame(country = NA, ba = NA)

for(i in 1:length(dd)){
  ddd <- read_csv(dd[i])
  result[i,2] <- ddd$all_total_ba
  result[i,1] <- ddd$aoi
}

result %>%
  arrange(desc(ba)) %>%
  mutate(percent_total = ba/ sum(ba) * 100,
         cumulative = cumsum(percent_total)) %>%
  filter(percent_total>1) %>%
  mutate(country = str_replace_all(country, "_", " ") %>% str_to_title()) %>%
  mutate_if(is.numeric, round, 1) %>%
  write_csv("countries_w_half_percent_of_worlds_ba.csv")

# side tasks for next world firedpy iteration ==================================

fpms <- read_csv("data/firedpy_master_spreadsheet - Sheet1.csv")
head(fpms)

world <- sf::st_read("data/world_boundaries") %>%
  dplyr::select(ne_name = NAME) %>%
  mutate(name_lwr = ne_name %>% str_to_lower() %>% str_replace_all(" ", "_"))

for_eric <- result %>%
  arrange(desc(ba)) %>%
  mutate(percent_total = ba/ sum(ba) * 100,
         cumulative = cumsum(percent_total)) %>%
  left_join(fpms, by= c("country" = "country_name")) %>%
  write_csv("countries_by_ba.csv")

world1 <- world %>%
  left_join(for_eric, by = c("name_lwr"="country")) %>%
  mutate(land_area_km2 = (sf::st_area(.) %>% as.numeric())/1e6) %>%
  sf::st_write("world_w_stats.gpkg")
