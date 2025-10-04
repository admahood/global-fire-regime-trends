# models

# setup ========================================================================
# library(ggpubr)
library(dplyr)
library(readr)
library(tidyr)
library(tibble)
library(stringr)
# library(ggtext)
# library(ggrepel)
library(sf)
# library(viridisLite)
library(RColorBrewer)
# library(lme4)
# library(lmerTest)
# library(brms)
# library(ggeffects)
# library(sjPlot)
# library(sjmisc)
# library(sjlabelled)
# library(broom)
# library(broom.mixed)
# library(mgcv)
# library(nlme)
# library(vegan)
cols = c(brewer.pal(3,"Dark2")[c(1,2)],"grey90")

# data =========================================================================

wcrs<-st_crs("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs")

list_mf<-list.files("data/out_csvs_mar22_w_logs/", full.names = T) %>%
  lapply(read_csv)

# data inspection line
# list_mf%>%lapply(dplyr::select,aoi, fire_rotation, test_size) %>% bind_rows() %>% print(n=172)
bind_rows(lapply(list_mf, rbind))

tab <- list_mf %>%
  bind_rows() 

# aus_boundaries <- st_read("data/aus_boundaries/") %>%
#   st_simplify()%>%
#   mutate(name_lwr = STE_NAME21 %>% str_to_lower() %>% str_replace_all(" ", "_"),
#          name_lwr = paste0("australia_", name_lwr)) %>%
#   dplyr::select(name_lwr)

world_boundaries<- st_read("data/world_boundaries/ne_50m_admin_0_countries.shp") %>%
  mutate(name_lwr = NAME_EN %>% str_to_lower() %>% str_replace_all(" ", "_")) %>%
  mutate(name_lwr = ifelse(name_lwr == "the_gambia", "gambia", name_lwr),
         name_lwr = ifelse(name_lwr == "people's_republic_of_china", "china",name_lwr),
         name_lwr = ifelse(name_lwr == "united_states_of_america", "conus_ak", name_lwr)) %>%
  # filter(name_lwr != "australia") %>%
  dplyr::select(name_lwr, CONTINENT, REGION_WB, INCOME_GRP, ECONOMY, SUBREGION) %>%
  # bind_rows(aus_boundaries) %>%
  mutate(area_Mkm2 = (st_area(.) %>% units::drop_units())/1e12)

res <- world_boundaries %>%
  dplyr::select(aoi=name_lwr, area_Mkm2, REGION_WB, INCOME_GRP, ECONOMY, SUBREGION) %>%
  left_join(tab)

res %>% filter(all_n_fires >1000) %>% nrow

alpha_pc <- 0.05
number_comparisons <- 20*8
alpha_fw <- (1-alpha_pc)^number_comparisons

clm <-"data/gee_outputs/terraclim/min_vpd_w_anomoly.csv" 

wrangle_climate2 <- function(clm, name_helper){
  read_csv(clm) %>%
    dplyr::select(-`system:index`, -end_mo, -peak_sn, -pk_sn_end, -pk_sn_strt, 
                  -sn_length, -start_mo, -`.geo`) %>%
    pivot_longer(cols = names(.)[1:36], names_to = "variable", values_to = "value") %>%
    mutate(year = str_extract(variable, "\\d+") %>% as.numeric() + 2003,
           anom = str_extract(variable, "anom"),
           variable = ifelse(is.na(anom), 
                             str_c(name_helper, "_vpd"), 
                             str_c(name_helper, "_vpd_anom"))) %>%
    pivot_wider(id_cols = c("aoi", "year"),
                names_from = "variable", values_from = "value")
}



# new gee outputs modeling July 2022 ===========================================

# reading in, joining

flammable_area <- read_csv(file = "data/gee_outputs/annual_flammable_area.csv") %>%
  mutate(year = lubridate::year(date)) %>%
  dplyr::select(-date)
lc_area <- read_csv(file = "data/gee_outputs/annual_landcover_area.csv") %>%
  mutate(year = lubridate::year(date)) %>%
  dplyr::select(-date)
pop <- read_csv(file = "data/gee_outputs/annual_population_count.csv")

mean_vpd <- wrangle_climate2("data/gee_outputs/terraclim/mean_vpd_w_anomoly.csv",
                             "mean")
min_vpd <- wrangle_climate2("data/gee_outputs/terraclim/min_vpd_w_anomoly.csv",
                            "min")
max_vpd <- wrangle_climate2("data/gee_outputs/terraclim/max_vpd_w_anomoly.csv",
                            "max")

# need to get australia in there NTS

aois <- list.files("data/out_ts_csvs_08mar22") %>%
  str_remove(".csv") %>%
  rep(each=18)

fire_stats<-list.files("data/out_ts_csvs_08mar22", full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  mutate(aoi = aois) %>%
  dplyr::rename(year = ig_year)

big_d_w_fire <- left_join(mean_vpd, lc_area) %>%
  left_join(min_vpd) %>%
  left_join(max_vpd)%>%
  left_join(pop) %>%
  left_join(fire_stats) %>%
  filter(!is.na(total_ba), !is.na(mean_vpd)) %>%
  na.omit() %>%
  group_by(aoi) %>%
  mutate(n=n()) %>%
  ungroup() %>%
  left_join(tab %>% dplyr::select(koppen_mode, aoi)) %>%
  left_join(res %>% st_set_geometry("NULL") %>% dplyr::select(aoi, REGION_WB, INCOME_GRP, ECONOMY, SUBREGION))

big_d <- left_join(mean_vpd, lc_area) %>%
  left_join(min_vpd) %>%
  left_join(max_vpd)%>%
  left_join(pop) %>%
  filter(!is.na(mean_vpd)) %>%
  na.omit() %>%
  group_by(aoi) %>%
  mutate(n=n()) %>%
  ungroup() 

bd <- filter(big_d_w_fire, n>15)  %>%
  dplyr::select(-`NULL`)

write_csv(bd, "data/bd.csv")
  
# doing the modelling

