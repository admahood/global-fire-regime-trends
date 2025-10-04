# models then figures
# land cover is in km2
# try an AIC model comparison approach, grouping, star thing
# load data
library(lme4)
library(broom)
library(broom.mixed)
library(tidyverse)
library(RColorBrewer)
library(performance)
library(datawizard)
library(sf)

country_x_kg <- read_csv("data/country_x_kg.csv")

# ancillary data ===============================================================
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
  dplyr::select(aoi=name_lwr, area_Mkm2, REGION_WB, INCOME_GRP, ECONOMY, SUBREGION) |>
  st_set_geometry(NULL)

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

# fire data ==============
flz <- list.files('data/out_ts_csvs_aug24', full.names = T, pattern = '.csv') 

rcs <- function(x){read_csv(x) |> mutate(filename = x)}


bdn <- flz %>%
  lapply(rcs) %>%
  bind_rows()

bdx <- bdn |>
  mutate(filename = str_sub(filename, 24, nchar(filename)) |> str_remove_all('\\.csv'),
         kg = case_when(str_detect(filename, "Arid") ~ "Arid",
                        str_detect(filename, 'Boreal') ~ 'Boreal',
                        str_detect(filename, "Polar") ~ "Polar",
                        str_detect(filename, 'Equatorial') ~ 'Equatorial',
                        str_detect(filename, 'Temperate') ~ 'Temperate'),
         aoi = str_remove_all(filename, "_Polar") |> str_remove_all("_Temperate") |>
           str_remove_all('_Boreal') |> str_remove_all('_Equatorial') |> str_remove_all('_Arid')) |> 
  dplyr::filter(kg != 'Polar') |>
  dplyr::rename(year = ig_year) |>
  na.omit() |>
  left_join(lc_area, by = c('aoi', 'year')) |>
  left_join(pop) |>
  left_join(mean_vpd) |>
  left_join(res) |> 
  na.omit()


library(glmmTMB)
# library(randomForest)
library(performance)
library(ggeffects)

a_aois <- bdx |> dplyr::filter(kg == 'Arid') |>
  group_by(aoi) |>
  summarise(n=n()) |>
  filter(n>15) |>
  pull(aoi)

m_sizea <- paste('log(size) ~ urban_built + mean_vpd_anom + forest + cropland + grassland + (1|aoi) + (1|SUBREGION)') |>
  as.formula() |>
  glmmTMB(data = bdx |> dplyr::filter(kg =="Arid", aoi %in% a_aois))
summary(m_sizea); performance::r2(m_sizea); car::Anova(m_sizea)

m_fsra <- paste('log(fsr) ~ mean_vpd_anom + urban_built + cropland + forest + grassland + (1|aoi) + (1|SUBREGION)') |>
  as.formula() |>
  glmmTMB(data = bdx |> dplyr::filter(kg =="Arid", aoi %in% a_aois))
summary(m_fsra); performance::r2(m_fsra); car::Anova(m_fsra)

m_mxga <- paste('log(mx_grw) ~ mean_vpd_anom + urban_built + cropland + forest + grassland + (1|aoi) + (1|SUBREGION)') |>
  as.formula() |>
  glmmTMB(data = bdx |> dplyr::filter(kg =="Arid", aoi %in% a_aois))
summary(m_mxga); performance::r2(m_mxga); car::Anova(m_mxga)

m_dura <- paste('log(dur) ~ mean_vpd_anom + urban_built + cropland + forest + grassland + (1|aoi) + (1|SUBREGION)') |>
  as.formula() |>
  glmmTMB(data = bdx |> dplyr::filter(kg =="Arid", aoi %in% a_aois))
summary(m_dura); performance::r2(m_dura); car::Anova(m_dura)

# boreal
bdx |> dplyr::filter(kg =="Boreal") |> pull(aoi) |> unique() |> length()
bdx |> dplyr::filter(kg =="Arid") |> pull(aoi) |> unique() |> length()

b_aois <- bdx |> dplyr::filter(kg == 'Boreal') |>
  group_by(aoi) |>
  summarise(n=n()) |>
  arrange(n) |>
  print(n=20) |>
  filter(n>15) |>
  pull(aoi)

length(b_aois); length(a_aois)

m_size <- paste('log(size) ~ mean_vpd_anom + grassland + cropland + forest + urban_built + (1|aoi) + (1|SUBREGION)') |>
  as.formula() |>
  glmmTMB(data = bdx |> dplyr::filter(kg =="Boreal", aoi %in% b_aois))
summary(m_size); performance::r2(m_size); AIC(m_size)

m_fsr <- paste('log(fsr) ~ mean_vpd_anom + cropland + forest + grassland +urban_built+ (1|aoi) + (1|SUBREGION)') |>
  as.formula() |>
  glmmTMB(data = bdx |> dplyr::filter(kg =="Boreal", aoi %in% b_aois))
summary(m_fsr); performance::r2(m_fsr)

m_mxg <- paste('log(mx_grw) ~ mean_vpd_anom + forest + urban_built + grassland + (1|aoi) + (1|SUBREGION)') |>
  as.formula() |>
  glmmTMB(data = bdx |> dplyr::filter(kg =="Boreal", aoi %in% b_aois))
summary(m_mxg); performance::r2(m_mxg); car::Anova(m_mxg)

m_durb <- paste('log(dur) ~ mean_vpd_anom + (1|aoi) + (1|SUBREGION)') |>
  as.formula() |>
  glmmTMB(data = bdx |> dplyr::filter(kg =="Boreal", aoi %in% a_aois), na.action = na.fail)
summary(m_durb); performance::r2(m_durb); car::Anova(m_durb); AIC(m_durb)

dummy <- data.frame(var = 'dur', kg = 'Boreal', estimate = 0, statistic = 0, p.value = 1,
                    term = c('urban_built', 'forest', 'grassland'))

list(m_durb, m_mxg, m_fsr, m_size,m_dura, m_mxga, m_fsra, m_sizea) |>
  lapply(model_performance) |>
  bind_rows() |>
  mutate(kg = c(rep('Boreal', 4), rep("Arid", 4)),
         term = rep(c("Duration", 'Max Growth', "Mean Growth", "Size"),2)) |>
  dplyr::select(kg, term, cr2 = 4, mr2 = 5) |>
  mutate(re_exp = (cr2) - mr2) |>
  mutate_if(is.numeric, round, 3) |>
  write_csv('tables/glmm_version2.csv')

library(ggsci)
bind_rows(
  broom.mixed::tidy(m_mxg) |> mutate(var = 'mxg', kg = 'Boreal'),
  broom.mixed::tidy(m_mxga) |> mutate(var = 'mxg', kg = 'Arid'),
  broom.mixed::tidy(m_size) |> mutate(var = 'size', kg = 'Boreal'),
  broom.mixed::tidy(m_sizea) |> mutate(var = 'size', kg = 'Arid'),
  broom.mixed::tidy(m_fsr) |> mutate(var = 'fsr', kg = 'Boreal'),
  broom.mixed::tidy(m_fsra) |> mutate(var = 'fsr', kg = 'Arid'),
  broom.mixed::tidy(m_durb) |> mutate(var = 'dur', kg = 'Boreal'),
  broom.mixed::tidy(m_dura) |> mutate(var = 'dur', kg = 'Arid')) |>
  filter(str_sub(term, 1,2) != 'sd', term != "(Intercept)", term != 'cropland') |>
  dplyr::select(term, var, kg, statistic, p.value, estimate) |>
  bind_rows(dummy) |>
  mutate(term = str_replace_all(term, 'forest_savannas', 'forest'),
         sig = ifelse(p.value < 0.05, "*", ''),
         term = str_replace_all(term, "mean_vpd_anom", "A. Mean VPD Anomaly") |>
           str_replace_all('grassland', "B. Grassland") |>
           str_replace_all('forest', "C. Forest") |> 
           str_replace_all('urban_built', "D. Urban/Built-Up") |>
           str_replace_all('cropland', "E. Cropland"),
         var  = str_replace_all(var, 'fsr', "Mean Growth") |>
           str_replace_all('mxg', "Max Growth") |>
           str_replace_all('dur', 'Duration') |>
           str_replace_all('size', 'Size')) |>
  ggplot(aes(x=kg, y=estimate, fill = var, alpha = sig, color = sig)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_wrap(~term, scales='free', nrow = 1) +
  scale_color_manual(values =c("white", 'black')) +
  # scale_y_continuous(breaks = c(0,1.96, -1.96), labels = c(0, 1.96, -1.96)) +
  scale_y_continuous(breaks = c(0), labels = c(0)) +
  scale_alpha_manual(values = c(0.25, 1)) +
  geom_hline(yintercept = 0, lty=3) +
  scale_fill_simpsons() +
  guides(color='none', alpha = 'none') +
  ggtitle("Effects of Annual Changes in VPD and Land Cover on Event-Based Characteristics") +
  ylab("Estiamte") +
  # ylab('Z-Statistic') +
  theme_bw() +
  theme(legend.title = element_blank(),
        # legend.position = c(1,0),
        # legend.justification = c(1,0),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank())

ggsave(filename = 'figures/figure_5_barplots.png', height = 3.5, width = 9.5, bg = 'white')

# library(pdp)
# pdf <- bind_rows(pdp::partial(mrfa, pred.var = "mean_vpd_anom") |> mutate(kg = "Arid", var = "Maximum Spread Rate"),
#           pdp::partial(mrfb, pred.var = "mean_vpd_anom") |> mutate(kg = "Boreal", var = "Maximum Spread Rate"),
#           pdp::partial(srfa, pred.var = "mean_vpd_anom") |> mutate(kg = "Arid", var = "Size"),
#           pdp::partial(srfb, pred.var = "mean_vpd_anom") |> mutate(kg = "Boreal", var = "Size"),
#           pdp::partial(drfa, pred.var = "mean_vpd_anom") |> mutate(kg = "Arid", var = "Duration"),
#           pdp::partial(drfb, pred.var = "mean_vpd_anom") |> mutate(kg = "Boreal", var = "Duration"),
#           pdp::partial(frfa, pred.var = "mean_vpd_anom") |> mutate(kg = "Arid", var = "Mean Spread Rate"),
#           pdp::partial(frfb, pred.var = "mean_vpd_anom") |> mutate(kg = "Boreal", var = "Mean Spread Rate"))
# 
# pdf |>
#   ggplot() +
#   geom_line(aes(x=mean_vpd_anom, y=yhat, color = kg)) +
#   facet_wrap(~var, scales = 'free', nrow = 1) +
#   xlab("Mean VPD Anomaly (Standardised)") + 
#   ylab("Partial Effect") +
#   theme_bw()
# 
# ggplot(bdx, aes(x=year, y=mean_vpd_anom)) +
#   geom_line(aes(group = aoi), alpha = 0.5) +
#   theme(legend.position = 'none') +
#   geom_smooth()
# 
# ggplot(bdx, aes(x=year, y=mean_vpd)) +
#   geom_line(aes(group = aoi), alpha = 0.5) +
#   theme(legend.position = 'none') +
#   geom_smooth()
# 
# pdfa <- bind_rows(pdp::partial(mrfa, pred.var = "grassland") |> mutate(kg = "Arid", var = "Maximum Spread Rate"),
#                  pdp::partial(mrfb, pred.var = "grassland") |> mutate(kg = "Boreal", var = "Maximum Spread Rate"),
#                  pdp::partial(srfa, pred.var = "grassland") |> mutate(kg = "Arid", var = "Size"),
#                  pdp::partial(srfb, pred.var = "grassland") |> mutate(kg = "Boreal", var = "Size"),
#                  pdp::partial(drfa, pred.var = "grassland") |> mutate(kg = "Arid", var = "Duration"),
#                  pdp::partial(drfb, pred.var = "grassland") |> mutate(kg = "Boreal", var = "Duration"),
#                  pdp::partial(frfa, pred.var = "grassland") |> mutate(kg = "Arid", var = "Mean Spread Rate"),
#                  pdp::partial(frfb, pred.var = "grassland") |> mutate(kg = "Boreal", var = "Mean Spread Rate"))
# 
# pdfa |>
#   ggplot() +
#   geom_line(aes(x=grassland, y=yhat, color = kg)) +
#   facet_wrap(~var, scales = 'free', nrow = 1) +
#   xlab("Grassland Area") + 
#   ylab("Partial Effect") +
#   theme_bw()
