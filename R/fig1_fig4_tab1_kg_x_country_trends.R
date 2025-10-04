# new results country x kg

library(tidyverse)
library(sf)
library(terra)
library(janitor)
library(lwgeom)
library(fasterize)
library(ggpubr)
sf::sf_use_s2(FALSE)

# look up tables for defining KG regions
lut_kop <- c("Af",  "Am", "Aw",   
             "BWh" ,"BWk","BSh" ,"BSk" ,
             "Csa", "Csb" ,"Csc" ,"Cwa", "Cwb", "Cwc","Cfa" , "Cfb", "Cfc" ,
             "Dsa" ,"Dsb" ,"Dsc" ,"Dsd","Dwa" ,"Dwb" ,"Dwc" ,"Dwd" ,
             "Dfa", "Dfb","Dfc", "Dfd" , 
             "ET",   "EF")

lut_kop <- c(rep("Equatorial",3), rep("Arid",4), rep("Temperate",9), rep("Boreal",12), rep("Polar",2))
names(lut_kop)<-c(1:30)


lut_frn <- c('Peak Season', "Season Length", "Size", "Burned Area", "N Fires", "Max Growth", "Duration", "Mean Growth")
names(lut_frn) <- c('peak_season',
             'season_length',
             'size',
             'total_ba',
             'n_fires',
             'mx_grw',
             'dur',
             'fsr')


# data ingest
if(!file.exists('data/country_x_kg.csv')){
filez <- list.files("data/out_csvs_aug24/", full.names = T, pattern = 'csv$')
d <- sapply(filez, read_csv) |> bind_rows()
write_csv(d, 'data/country_x_kg.csv')}else{d <- read_csv('data/country_x_kg.csv')}

# table o' trends ================
names(d)
d |>
  dplyr::select(starts_with('trend'), -starts_with('trend_log'), aoi, kg = koppen) |>
  pivot_longer(cols = c(trend_peak_season,
                        trend_season_length,
                        trend_size,
                        trend_total_ba,
                        trend_n_fires,
                        trend_mx_grw,
                        trend_dur,
                        trend_fsr)) |>
  mutate(name = str_remove_all(name, 'trend_')) |>
  na.omit() |>
  group_by(kg, name, value) |>
  summarise(n=n()) |>
  ungroup() |>
  pivot_wider(names_from = value, values_from = n) |>
  dplyr::rename(positive = '+', negative = "-") |>
  arrange(kg, name) |>
  tidyr::replace_na(list(positive = 0, negative = 0, ns = 0)) |> 
  dplyr::filter(kg != 'Polar') |>
  mutate(name = lut_frn[name],
         percent_positive = round(positive / (positive + negative + ns) * 100),
         percent_negative = round(negative / (positive + negative + ns) * 100)) |>
  write_csv("tables/new_trend_table1_by_kg.csv")

# not grouped by kg

d |>
  dplyr::select(starts_with('trend'), -starts_with('trend_log'), aoi, kg = koppen) |>
  pivot_longer(cols = c(trend_peak_season,
                        trend_season_length,
                        trend_size,
                        trend_total_ba,
                        trend_n_fires,
                        trend_mx_grw,
                        trend_dur,
                        trend_fsr)) |>
  mutate(name = str_remove_all(name, 'trend_')) |>
  na.omit() |>
    dplyr::filter(kg != 'Polar') |>
  group_by(name, value) |>
  summarise(n=n()) |>
  ungroup() |>
  pivot_wider(names_from = value, values_from = n) |>
  dplyr::rename(positive = '+', negative = "-") |>
  arrange(name) |>
  tidyr::replace_na(list(positive = 0, negative = 0, ns = 0)) |> 
  mutate(name = lut_frn[name],
         percent_positive = round(positive / (positive + negative + ns) * 100),
         percent_negative = round(negative / (positive + negative + ns) * 100),
         total_regions = positive + negative + ns) |>
  dplyr::select(-total_regions) |>
  write_csv("tables/new_trend_table2.csv")

# rasterize the country polygons

trends_d <- d |>
  dplyr::select(starts_with('trend'), -starts_with('trend_log'), aoi, kg = koppen) |>
  pivot_longer(cols = c(trend_peak_season,
                        trend_season_length,
                        trend_size,
                        trend_total_ba,
                        trend_n_fires,
                        trend_mx_grw,
                        trend_dur,
                        trend_fsr)) |>
  mutate(name = str_remove_all(name, 'trend_')) |>
  na.omit() |>
  mutate(name_lwr = str_remove_all(aoi, '_Temperate') |> 
           str_remove_all("_Arid") |> str_remove_all("_Equatorial") |> 
           str_remove_all('_Polar') |> str_remove_all("_Boreal")) |>
  dplyr::select(-aoi)

kg <- terra::rast('data/kg_1991_2020/koppen_geiger_0p5.tif') # final map should be one level finer in resolution
plot(kg)


world <- st_read("data/world_boundaries/ne_50m_admin_0_countries.shp") |>
  janitor::clean_names() %>%
  mutate(name_lwr = name_en %>% str_to_lower() %>% str_replace_all(" ", "_")) %>%
  mutate(name_lwr = ifelse(name_lwr == "the_gambia", "gambia", name_lwr),
         name_lwr = ifelse(name_lwr == "people's_republic_of_china", "china",name_lwr),
         name_lwr = ifelse(name_lwr == "united_states_of_america", "conus_ak", name_lwr)) %>%
  dplyr::mutate(name_num = 1:241) |>
  st_transform(crs = st_crs(kg))
library(raster)
world_rast <- fasterize::fasterize(world, as(kg, "Raster"), field = 'name_num')

plot(world_rast)

world_ter <- as(world_rast, "SpatRaster")

# Figure 1. KG plot ========================
spdf <- c(world_ter, kg) |> 
  as.data.frame(xy = TRUE) |>
  na.omit() |>
  left_join(world |> sf::st_set_geometry(NULL) |> dplyr::select(name_lwr, layer = name_num)) |>
  dplyr::mutate(kg = lut_kop[koppen_geiger_0p5])
  

pkg <- spdf |>
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%# st_transform('+proj=robin') |>
  ggplot() +
  # geom_raster(aes(x=x, y=y, fill = kg)) +
  geom_sf(aes(color = kg), size = .15, key_glyph = 'rect') +
  geom_sf(data = world, fill = NA, color = 'black') +
  scale_color_brewer(palette = "Accent", name = "Koppen-Geiger") +
  coord_sf(expand = F, crs = '+proj=robin', ylim =c(-6.5e6, 7.6e6)) +
  theme_bw() +
  # coord_sf(ylim = c(-60, 70), expand = F) +
  theme(axis.title = element_blank(),
        legend.position = "bottom");pkg
ggsave(plot = pkg, filename = 'figures/kg_map.png', bg = "white", width = 10, height = 5)

ddd <- read_csv('data/npix_plotdf.csv')

pline <- ggplot(ddd, aes(y=x*100, x=y*100)) +
  geom_line() +
  scale_y_continuous(limits = c(0,100)) +
  xlab('% of Countries') +
  ylab('% of Area Occupied by\nLargest KG class') +
  theme_bw() +
  geom_hline(yintercept = 75, lty=3)+
  geom_vline(xintercept = 72.5, lty=3) +
  theme(axis.title = element_text(size=8), 
        plot.background = element_rect(fill=NA))

cowplot::ggdraw(xlim = c(0,10), ylim = c(0,5)) +
  cowplot::draw_plot(pkg, 0,0,10,5) +
  cowplot::draw_plot(pline, x = .4, y=1.1, width=2.25, height = 1.7) +
  cowplot::draw_label(x=2.3, y=1.7, label = "(b)",fontface = 'bold')  +
  cowplot::draw_label(x=.5, y=4.7, label = "(a)", fontface = 'bold')
  
ggsave(filename = 'figures/kg_map_inset.png', bg = "white", width = 10, height = 5)

# Figure 4. Trends =============================================================

colz = RColorBrewer::brewer.pal(3, "Set1")
colz = c(colz[2], colz[1], 'beige', "grey")


plotz <- list()
for(i in 1:length(na.omit(unique(trends_d$name)))){

dpd <- spdf |>
  left_join(trends_d |> filter(name == na.omit(unique(trends_d$name)[i]))) |>
  dplyr::mutate(name = lut_frn[name]) |>
  tidyr::replace_na(list(value = 'Insufficient Data')) |>
  mutate(value = str_replace_all(value, "ns$", "Not Significant")) |>
  st_as_sf(coords = c("x", "y"), crs = 4326)

plotz[[i]] <- ggplot(dpd) +
  # geom_raster(aes(x=x, y=y, fill = value)) +
  geom_sf(aes(color = value), size=0.15) +
  geom_sf(data = world, fill = NA, color = 'black') +
  coord_sf(expand = F, crs = '+proj=robin', ylim =c(-6.5e6, 7.6e6)) +
  # coord_sf(expand = F, ylim = c(-60, 80)) +
  scale_color_manual(values = colz, name = 'Trend') +
  guides(color = guide_legend(
    override.aes = list(shape = 15, size = 10)))+
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size=24)) +
  ggtitle(unique(na.omit(dpd$name)))
}

ggarrange(plotz[[4]], plotz[[5]], plotz[[1]], plotz[[2]], 
          plotz[[3]], plotz[[6]], plotz[[8]], plotz[[7]],
          nrow = 4, ncol =2, common.legend = TRUE, legend = 'bottom')|>
  ggpubr::annotate_figure(left = text_grob('Event-Based                              Area-Based',
                                           size = 30, rot = 90)) |>
  ggsave(filename = 'figures/fig3_kg_country_trends_all.png', 
         width = 13.5, height =14.5, bg = 'white')
