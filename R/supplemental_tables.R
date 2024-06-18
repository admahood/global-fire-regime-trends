# big table
library(tidyverse)
library(sf)
list_mf<-list.files("data/out_csvs_mar22_w_logs/", full.names = T) %>%
  lapply(read_csv)

# data inspection line
# list_mf%>%lapply(dplyr::select,aoi, fire_rotation, test_size) %>% bind_rows() %>% print(n=172)

tab <- list_mf %>%
  bind_rows() 


world_boundaries<- st_read("data/world_boundaries/") %>%
  mutate(name_lwr = NAME_EN %>% str_to_lower() %>% str_replace_all(" ", "_")) %>%
  mutate(name_lwr = ifelse(name_lwr == "the_gambia", "gambia", name_lwr),
         name_lwr = ifelse(name_lwr == "people's_republic_of_china", "china",name_lwr),
         name_lwr = ifelse(name_lwr == "united_states_of_america", "conus_ak", name_lwr)) %>%
  # filter(name_lwr != "australia") %>%
  dplyr::select(aoi=name_lwr, continent = CONTINENT) %>%
  # bind_rows(aus_boundaries) %>%
  # mutate(area_Mkm2 = (st_area(.) %>% units::drop_units())/1000000000000) %>%
  st_set_geometry(NULL) 

transmuted <- tab %>%
  transmute(aoi=aoi,
    peak_season = str_c(round(all_peak_season), " (", trend_peak_season, ")"),
    season_length = str_c(all_season_length, " (", trend_season_length, ")"),
    size = str_c(round(all_size,1), " (", trend_size, ")"),
    total_ba = str_c(prettyNum(all_total_ba, big.mark=","), " (", trend_total_ba, ")"),
    n_fires = str_c(prettyNum(all_n_fires, big.mark=","), " (", trend_n_fires, ")"),
    mx_grw = str_c(round(all_mx_grw,1), " (", trend_mx_grw, ")"),
    dur = str_c(round(all_dur,1), " (", trend_dur, ")"),
    fsr = str_c(round(all_fsr,1), " (", trend_fsr, ")")) %>%
  mutate_all(function(x) str_remove_all(x," \\(ns\\)")) %>%
  filter(!is.na(size))

for(i in unique(world_boundaries$continent)){
  transmuted %>% 
    left_join(world_boundaries) %>%
    filter(continent == i) %>%
    arrange(desc(total_ba)) %>%
    dplyr::select(Country = aoi, "Total Burned Area" = total_ba,
                  "N Fires" = n_fires, "Peak Season" = peak_season,
                  "Season Length" = season_length, "Size" = size,
                  "Fire Spread Rate" = fsr, "Max Growth" = mx_grw,
                  Duration = dur) %>%
    write_csv(str_c("tables/", str_replace_all(i, " ","_"), ".csv"))
}
