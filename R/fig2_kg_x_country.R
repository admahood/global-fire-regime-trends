# bivariate map
library(raster)
library(terra)
library(tidyverse)
library(cowplot)
library(biscale)
library(sf)
library(ggpubr)
sf::sf_use_s2(FALSE)

# look up tables 
lut_kop <- c(rep("Equatorial",3), rep("Arid",4), rep("Temperate",9), rep("Boreal",12), rep("Polar",2))
names(lut_kop)<-c(1:30)

# polygon of KG
kg <- terra::rast('data/kg_1991_2020/koppen_geiger_0p5.tif') # final map should be one level finer in resolution

kgg <- kg * 1000


exx <- terra::ext(kg)
exx[3] <- -60
exx[4] <- 89

kgp <-  kg |>
  terra::crop(exx) |>
  terra::as.polygons() |> 
  sf::st_as_sf() |> 
  mutate(kg = lut_kop[koppen_geiger_0p5]) |>
  group_by(kg) |>
  summarise() |>
  ungroup()

# plot(kgp)

world <- st_read("data/world_boundaries/ne_50m_admin_0_countries.shp") |>
  janitor::clean_names() %>%
  mutate(name_lwr = name_en %>% str_to_lower() %>% str_replace_all(" ", "_")) %>%
  mutate(name_lwr = ifelse(name_lwr == "the_gambia", "gambia", name_lwr),
         name_lwr = ifelse(name_lwr == "people's_republic_of_china", "china",name_lwr),
         name_lwr = ifelse(name_lwr == "united_states_of_america", "conus_ak", name_lwr)) %>%
  dplyr::mutate(name_num = 1:241) |>
  st_transform(crs = st_crs(kg))

country_names <- world |> st_set_geometry(NULL) |> 
  dplyr::select(name_lwr, country_num = name_num)


world_rast <- fasterize::fasterize(world, as(kg, "Raster"), field = 'name_num')

# plot(world_rast)

world_ter <- as(world_rast, "SpatRaster")

kg_country_ter <- world_ter + kgg

kgcp <- kg_country_ter  |>
  terra::crop(exx) |>
  terra::as.polygons() |> 
  st_as_sf() |>
  mutate(kg_num = ifelse(layer > 9999, str_sub(layer,1,2), str_sub(layer,1,1)),
         country_num = ifelse(layer > 9999, str_sub(layer, 3,5),str_sub(layer, 2,4)) |> as.numeric(),
         kg = lut_kop[kg_num]) |>
  left_join(country_names) |>
  group_by(kg, name_lwr) %>%
  summarise() %>%
  ungroup() %>%
  mutate(uid = 1:nrow(.))
# plot(kgcp)
# 
# st_write(kgcp, 'data/kg_country_polygon.gpkg', delete_dsn = TRUE)


# spatial df kg country
spdf <- c(world_ter, kg) |> 
  as.data.frame(xy = TRUE) |>
  na.omit() |>
  left_join(world |> sf::st_set_geometry(NULL) |> dplyr::select(name_lwr, layer = name_num)) |>
  dplyr::mutate(kg = lut_kop[koppen_geiger_0p5])

# make a polygon

npix <- spdf |>
  group_by(name_lwr, kg) %>%
  summarise(n_pix = n()) |>
  mutate(total_pix = sum(n_pix),
         fra_pix = n_pix/total_pix,
         mx_fra = max(fra_pix)) |>
  ungroup()

# ggplot(npix) + 
# geom_histogram(aes(x=mx_fra))

# write_csv(npix, 'data/npix.csv')
  
# kg_country pattern map
d <- read_csv('data/country_x_kg.csv') |>
  dplyr::select(aoi, starts_with('all'), -starts_with('all_log'), kg = koppen) |>
  mutate(name_lwr = str_remove_all(aoi, '_Temperate') |> 
           str_remove_all("_Arid") |> str_remove_all("_Equatorial") |> 
           str_remove_all('_Polar') |> str_remove_all("_Boreal")) |>
  dplyr::select(-aoi) |>
  left_join(npix) |>
  dplyr::mutate(all_total_ba = all_total_ba/n_pix,
                all_n_fires = all_n_fires/n_pix)

bc <- bi_class(d, x=all_total_ba, y=all_season_length, "quantile") |>
  dplyr::select(kg, name_lwr, bi_class)

bc2  <- bi_class(d, x=all_size, y=all_dur, "quantile") |>
  dplyr::select(kg, name_lwr, bc2 = bi_class)

bpal <- "BlueOr"

# plotting figure 2 ===================
map <- spdf |>
  left_join(bc) |>
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%# st_transform('+proj=robin') |>
  ggplot() +
  # geom_tile(aes(x=x, y=y, fill = bi_class), show.legend = F) +
  geom_sf(aes(color = bi_class), size=0.15, show.legend = F) +
  geom_sf(data = world, fill = NA, color = 'grey20', lwd = .1) +
  coord_sf(expand = F, crs = '+proj=robin', ylim =c(-6.5e6, 7.6e6)) +
  # bi_scale_fill(pal = "GrPink", dim = 3) +
  bi_scale_color(pal = bpal, dim = 3) +
  bi_theme() +
  theme_bw() +
  ggtitle("(a) Area-Based Attributes") +
  theme(axis.title = element_blank(),
        plot.title = element_text(size = 16),
        legend.position = "bottom")#;map

legend <- bi_legend(pal = bpal,
                    dim = 3,
                    xlab = 'Burned Area',
                    ylab = "Season Length",
                    size = 8) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(plot.background = element_rect(fill=NA),
        text = element_text(size=12))

final_plot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0, 0.05, .4, .4)

map1 <- spdf |>
  left_join(bc2) |>
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%# st_transform('+proj=robin') |>
  ggplot() +
  # geom_tile(aes(x=x, y=y, fill = bi_class), show.legend = F) +
  geom_sf(aes(color = bc2), size=0.15, show.legend = F) +
  geom_sf(data = world, fill = NA, color = 'grey20', lwd = .1) +
  coord_sf(expand = F, crs = '+proj=robin', ylim =c(-6.5e6, 7.6e6)) +
  # bi_scale_fill(pal = "GrPink", dim = 3) +
  bi_scale_color(pal = bpal, dim = 3) +
  bi_theme() +
  theme_bw() +
  ggtitle("(b) Event-Based Attributes") +
  theme(axis.title = element_blank(),
        plot.title = element_text(size = 16),
        legend.position = "bottom");map1

legend1 <- bi_legend(pal = bpal,
                    dim = 3,
                    xlab = 'Size',
                    ylab = "Growth Rate",
                    size = 8) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_equal(expand = F) +
  theme(plot.background = element_rect(fill=NA),
        text = element_text(size=12))

final_plot1 <- ggdraw() +
  draw_plot(map1, 0, 0, 1, 1) +
  draw_plot(legend1, 0, 0.05, .4, .4)


ggarrange(final_plot, final_plot1, nrow = 2, ncol = 1) |>
  ggsave(width = 9.5, height = 8.9, filename = 'figures/new_BV_kg_x_country.png')


# turn into a polygon ===========
wc_filez <- list.files('data/too_big/wc2.1_2.5m_bio/', full.names = T)

wcr <- terra::rast(wc_filez)

spdf |> 
  st_as_sf(coords = c(1,2)) |>
  dplyr::select(name_lwr, kg) %>%
  mutate(terra::extract(wcr, ., ID = F)) |>
  st_set_geometry(NULL) %>%
  pivot_longer(names(.)[3:ncol(.)]) |>
  filter(value > -10000) |>
  group_by(name_lwr, kg, wcbio = name) |>
  summarise(value = mean(value, na.rm = T)) |>
  ungroup() |> 
  write_csv('data/wc_kg_country.csv')
