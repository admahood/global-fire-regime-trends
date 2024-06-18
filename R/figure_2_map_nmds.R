# setup ========================================================================
library(ggpubr)
library(dplyr)
library(readr)
library(GGally)
library(tidyr)
library(tibble)
library(stringr)
library(ggtext)
library(ggrepel)
library(sf)
library(viridisLite)
library(vegan)
library(biscale)
library(cowplot)
library(forcats)
cols = c(RColorBrewer::brewer.pal(3,"Dark2")[c(1,2)],"grey90")
# install.packages("biscale")

# data =========================================================================

wcrs<-st_crs("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs")

list_mf<-list.files("data/out_csvs_mar22_w_logs/", full.names = T) %>%
  lapply(read_csv)

# data inspection line
# list_mf%>%lapply(dplyr::select,aoi, fire_rotation, test_size) %>% bind_rows() %>% print(n=172)

tab <- list_mf %>%
  bind_rows() 

# aus_boundaries <- st_read("data/aus_boundaries/") %>%
#   st_simplify()%>%
#   mutate(name_lwr = STE_NAME21 %>% str_to_lower() %>% str_replace_all(" ", "_"),
#          name_lwr = paste0("australia_", name_lwr)) %>%
#   dplyr::select(name_lwr)

world_boundaries<- st_read("data/world_boundaries/") %>%
  mutate(name_lwr = NAME_EN %>% str_to_lower() %>% str_replace_all(" ", "_")) %>%
  mutate(name_lwr = ifelse(name_lwr == "the_gambia", "gambia", name_lwr),
         name_lwr = ifelse(name_lwr == "people's_republic_of_china", "china",name_lwr),
         name_lwr = ifelse(name_lwr == "united_states_of_america", "conus_ak", name_lwr)) %>%
  # filter(name_lwr != "australia") %>%
  dplyr::select(name_lwr, CONTINENT) %>%
  # bind_rows(aus_boundaries) %>%
  mutate(area_Mkm2 = (st_area(.) %>% units::drop_units())/1000000000000)

res <- world_boundaries %>%
  dplyr::select(aoi=name_lwr, area_Mkm2, continent =CONTINENT) %>%
  left_join(tab)

# figuring out which countries were not analyzed
res %>% filter(is.na(all_total_ba)) %>% 
  filter(continent == "Africa") %>%
  dplyr::pull(aoi) %>% paste(collapse = ", ") %>% str_replace_all("_", " ") %>% 
  str_to_title() %>%
  str_replace_all("And", "and") %>%
  str_replace_all("Of", "of")

# there are 116 countries with > 1000 events
# 151 countries with more than 50 fires
# 145 countries with trends calculated
# 168 countries w more than 1 fire
nrow(res)
res %>% filter((trend_total_ba) == "-") %>% nrow

# bonferonni adjustment
alpha_new <- (1-0.05)^168

# multicolor map ========

res1 <- dplyr::select(res, aoi,
                      fire_rotation,
                      area_Mkm2,
                      starts_with("all"), 
                      -starts_with("all_log")) %>%
  mutate(ba_per_area = all_total_ba/area_Mkm2,
         n_per_area = all_n_fires/area_Mkm2)

bc <- bi_class(res1, x=all_total_ba, y=all_season_length, "jenks")

gimme_bivariate_map <- function(bc, filename = "bivariate_cholorpleth.png",
                                x, title,
                                y){
  map <- ggplot(bc) +
    geom_sf(aes(fill=bi_class), show.legend = FALSE) +
    bi_scale_fill(pal = "GrPink", dim = 3) +
    coord_sf(crs = wcrs) +
    bi_theme() +
    ggtitle(title)
  
  legend <- bi_legend(pal = "GrPink",
                      dim = 3,
                      xlab = x,
                      ylab = y,
                      size = 8) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme(plot.background = element_rect(fill=NA))
  # combine map with legend
  final_plot <- ggdraw() +
    draw_plot(map, 0, 0, 1, 1) +
    draw_plot(legend, 0, .1, .4, .4)
  
  ggsave(filename = filename, width = 9.75, height=5)
  return(final_plot)
}
bc1 <- bi_class(res1, x=ba_per_area, y=all_season_length, style="quantile") |>
  filter(aoi != "antarctica")
bc2 <- bi_class(res1, y=all_dur, x=all_fsr, style="quantile") |>
  filter(aoi != "antarctica")
# bc3 <- bi_class(res1, x = all_dur, y = all_season_length, style = "quantile", dim = 3)
# bc4 <- bi_class(res1, x = all_dur, y = all_fsr, style = "quantile", dim = 3)

bv <- gimme_bivariate_map(bc1, x = "More Events and Burned Area", y="Longer Seasons",
                    filename = "figures/bivc_tba_sl.png", 
                    title = "Country-Level Attributes");bv

bv1 <- gimme_bivariate_map(bc2, y = "Longer Lasting", x="Larger and Faster Spreading",
                           filename = "figures/bivc_n_fsr.png", 
                           title = "Event Attributes")
# multipanel ==============

ggarrange(bv, bv1, nrow=2, ncol=1, labels = "auto", widths = c(2,1)) %>%
  ggsave(plot = ., filename = "figures/figure_1_multipanel_new.png", 
         bg="white",width=9, height=8.5)


# corrplots=====================

df<-res %>%
  st_set_geometry(NULL) %>%
  dplyr::select(starts_with("all"),
                -starts_with("all_log"),
                area_Mkm2, aoi,
                koppen_mode,
                isothermality,
                contains("precip"), 
                contains("temper"), 
                contains("range"), 
                starts_with("pct") ) %>% 
  column_to_rownames("aoi") %>%
  na.omit() %>%
  dplyr::select(starts_with("all"),area_Mkm2, koppen_mode) %>%
  mutate(tba = all_total_ba/area_Mkm2,
         n_fires = all_n_fires/area_Mkm2) %>%
  dplyr::select(-all_total_ba,-area_Mkm2) %>%
  filter(all_n_fires > 50) |>
  dplyr::select(-all_n_fires)

climdf <- res %>%
  st_set_geometry(NULL)  %>%
  filter(all_n_fires > 50) %>%
  dplyr::select(aoi, isothermality,
                contains("precip"), 
                contains("temperature"), 
                contains("range")) %>% 
  column_to_rownames("aoi")  %>%
  na.omit()|>
  as_tibble(rownames = "aoi") |>
  pivot_longer(-c(aoi)) |>
  mutate(name = str_replace_all(name, "_", " ") |>
           str_replace_all("temperature", "T") |>
           str_replace_all("precipitation", "PPT"))|>
  pivot_wider() |>
  dplyr::select(-aoi) |>
  janitor::clean_names()

cordf <- df |>
  as_tibble(rownames = "aoi") |>
  pivot_longer(-c(aoi, koppen_mode)) |>
  mutate(name = str_remove_all(name, "all_") |>
           str_replace_all("mx_grw", "max growth") |>
           str_replace_all("_", " ") |>
           str_replace_all("dur", "Duration") |>
           str_replace_all("fsr", "Mean Growth") |>
           str_to_title() |>
           str_replace_all("Tba", "Burned Area")) |>
  pivot_wider() |>
  dplyr::select(-aoi, -koppen_mode) 
cor_p <- ggcorrplot::cor_pmat(cordf)

corc <- cor(cordf, climdf)

resdf <- data.frame(fire = NA, clim = NA, Corr = NA, p = NA)
counter <- 1
for(i in 1:ncol(cordf)){
  for(j in 1:ncol(climdf)){
    c <- cor.test(cordf[,i] |> pull(), 
                  climdf[,j]|> pull())
    resdf[counter, 1] <- names(cordf)[i]
    resdf[counter, 2] <- names(climdf)[j]
    resdf[counter, 3] <- c$estimate
    resdf[counter, 4] <- c$p.value
    counter <- counter +1
  }
}

cc <- resdf |> 
  filter(p<0.01) |>
  mutate(fire = as.factor(fire) |> 
           fct_relevel("Burned Area", "N Fires", "Season Length", "Peak Season",
                       "Duration", "Size", "Mean Growth", "Max Growth"),
         clim = str_replace_all(clim, "_", " ") |> 
           str_to_title() |>
           str_replace_all("Ppt", "PPT") |>
           str_replace_all("ppt", "PPT") |>
           str_replace_all("Of", "of") |>
           str_replace_all(" t ", " T ") |>
           str_replace_all(" t", " T") |>
           as.factor() |>
           fct_relevel("PPT of Driest Quarter", 
                       "PPT of Driest Month",
                       "T Seasonality", 
                       "T Annual Range",
                       "Annual PPT", 
                       "PPT of Wettest Quarter",
                       "PPT of Wettest Month",
                       "PPT of Warmest Quarter", 
                       "Min T of Coldest Month",
                       "Mean T of Coldest Quarter", 
                       "Annual Mean T", 
                       "Mean T of Wettest Quarter",
                       "Isothermality", 
                       "Mean Diurnal Range", 
                       "PPT Seasonality", 
                       "Mean T of Driest Quarter", 
                       "Max T of Warmest Month", 
                       "Mean T of Warmest Quarter")) |>
ggplot(aes(x=fire, y=clim, fill=Corr)) +
  geom_raster() +
  theme_minimal() +
  scale_fill_gradient2(low = "blue", mid="white", high = "red") +
  coord_equal() +
  geom_text(aes(label = round(Corr,2)),size =3) +
  geom_vline(xintercept = 4.5, lty = 2) +
  xlab("Country-Level  |  Event-Level") +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(angle=45, hjust = 1))#,
        # legend.position = c(1,1),
        # legend.justification = c(1,1),
        # legend.background = element_rect(color = "black", fill = "white")) 
ggsave(plot = cc, filename = "figures/climate_correlations.png", width = 5, height = 8, bg="white")

# ggsave(filename = "figures/climate_correlations.png", width = 7, height = 8, bg="white")
corc[abs(corc) < 0.3] <- NA

ggcorrplot::ggcorrplot(corc, lab = T)

# df |>
#   as_tibble(rownames = "aoi") |>
#   dplyr::select(-aoi, -koppen_mode) |>
#   ggpairs()



cp <- cordf|>
  cor() |>
  round(2) |>
  ggcorrplot::ggcorrplot(type = "lower", hc.order = T, lab = T, lab_size = 3,
                         p.mat = cor_p, sig.level = .05, insig = 'blank') +
  theme(panel.grid.major.y = element_blank(),#linetype=3, color = "grey30"),
        panel.grid.major.x = element_blank());cp
ggsave("figures/fr_correlations.png",plot=cp, width=5.5, height = 4.5, bg="white")

cow<- cowplot::ggdraw(ylim=c(0,15), xlim = c(0,3)) +
  cowplot::draw_plot(cc,x = 0, y=5, height = 10, width=3) +
  cowplot::draw_plot(cp, x=.025, y=0, height=5, width =3) +
  cowplot::draw_text(x=2.75, y=4.9, "b")+
  cowplot::draw_text(x=2.75, y=14.9, "a")
ggsave(plot = cow, filename ="figures/fr_correlations_2pan.png", width=5.5, height = 10.5, bg="white")


# # nmds ==================================
# 
# ranki<-res %>%
#   st_set_geometry(NULL) %>%
#   filter(all_n_fires > 50)%>%
#   dplyr::select(aoi,contains("precip"), median_ai, mad_ai,
#                 contains("temper"), contains("range"), starts_with("pct") ) %>%
#   column_to_rownames("aoi") |>  na.omit() |>
#   rankindex(veg = df %>% dplyr::select(-koppen_mode) %>%
#             filter(all_n_fires > 50),
#           indices = c("euc", "man", "gow", "bra", "kul"))
# 
# vegan::metaMDS(df %>% dplyr::select(-koppen_mode),distance = "bra",
#                k=2# %>%
#                #wisconsin()
# ) -> nmds
# stressplot(nmds)
# 
# 
# 
# 
# env<-res %>%
#   st_set_geometry(NULL) %>%
#   filter(all_n_fires > 50)%>%
#   dplyr::select(starts_with("all"),area_Mkm2, aoi,contains("precip"), median_ai, mad_ai,
#                 contains("temper"), contains("range"), starts_with("pct") ) %>%
#   column_to_rownames("aoi")%>%
#   na.omit()%>%
#   dplyr::select(contains("precip"), contains("temper"), contains("range"), median_ai, mad_ai, starts_with("pct") )
# 
# fit <- vegan::envfit(nmds,env, perm=9999,p.max = 0.0001)
# fit1 <- vegan::envfit(nmds,df, perm=9999,p.max = 0.0001)
# # plot(nmds, type="t",display = "sites")
# # plot(fit1)
# 
# scores <- as.data.frame(scores(nmds)$sites) %>%
#   tibble::rownames_to_column("aoi") %>%
#   left_join(res %>% st_set_geometry(NULL) %>% dplyr::select(aoi, koppen_mode))
# 
# env <- as.data.frame(fit$vectors$arrows*sqrt(fit$vectors$r))  %>%
#   tibble::rownames_to_column("env") %>%
#   mutate(p = fit$vectors$pvals,
#          R2= fit$vectors$r,
#          env = str_replace_all(env,"quarter", "Q"),
#          env = str_replace_all(env,"temperature", "T"),
#          env = str_replace_all(env,"precipitation", "PPT"))%>%
#   filter(p<0.001, R2>0.21) %>%
#   filter(env != "pct_Equatorial") %>%
#   mutate(env = str_replace_all(env, "_", " ") %>% str_to_title() %>% 
#            str_remove_all("Of ") %>%
#            str_replace_all("Ppt", "PPT"))
# 
# fire <-as.data.frame(fit1$vectors$arrows*sqrt(fit1$vectors$r)) %>%
#   tibble::rownames_to_column("fire_characteristic") %>%
#   mutate(fire_characteristic = str_remove_all(fire_characteristic,"all_"),
#          fire_characteristic = str_replace(fire_characteristic,"size", "Fire Size"),
#          fire_characteristic = str_replace(fire_characteristic,"dur", "Event Duration"),
#          fire_characteristic = str_replace(fire_characteristic,"season_length", "Season Length"),
#          fire_characteristic = str_replace(fire_characteristic,"fsr", "Fire Spread Rate"),
#          fire_characteristic = str_replace(fire_characteristic,"n_fires", "Number of Fires"),
#          fire_characteristic = str_replace(fire_characteristic,"tba_per_area", "Total Burned Area"),
#          fire_characteristic = str_replace(fire_characteristic,"mx_grw", "Max Growth"),
#          p = fit1$vectors$pvals,
#          R2= fit1$vectors$r)%>%
#   filter(p<0.001)
# 
# # mutate(species = recode_factor(species,`ppt_2yr` = 'P[ant]', 
# #                                `sd_def` = 'sigma[CWD]',
# #                                `mean_aet` = 'AET',
# #                                `mean_tmin` = 'T[min]'))
# 
# pfire <- ggplot(scores, aes(x=NMDS1, y=NMDS2, color = koppen_mode)) +
#   geom_point(size= 2, stroke =1.5, alpha=0.5) +
#   theme_classic()+
#   geom_segment(data = fire,x=0,y=0,arrow = arrow(), color = "grey",
#                aes(yend = NMDS2/2, xend = NMDS1/2), lwd=1)+
#   geom_text_repel(data=fire, #parse = T,
#                   aes(x=NMDS1/2,y=NMDS2/2,label=fire_characteristic),color="black",
#                   size=3, fontface = "bold")+
#   # coord_fixed()+
#   scale_color_brewer(palette = "Set1")+
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
#         axis.title = element_text(size=10, face="bold"),
#         legend.position=c(0,1),
#         legend.title = element_blank(),
#         legend.justification=c(0,1),
#         legend.background = element_rect(fill = 'transparent'));pfire
# 
# penv<-ggplot(data=scores) +
#   geom_point(aes(x=NMDS1, y=NMDS2, color = koppen_mode),
#              size= 2, stroke =1.5, alpha = 0.5) +
#   geom_segment(data = env,x=0,y=0,arrow = arrow(), color = "grey",
#                aes(yend = NMDS2/2, xend = NMDS1/2), lwd=1)+
#   geom_text_repel(data=env, parse = F,
#                   aes(x=NMDS1/2,y=NMDS2/2,label=env),# hjust = "left",
#                   size=3,nudge_y = c(0.05,0,0,0), nudge_x = -.1,
#                   fontface = "bold")+
#   theme_classic()+
#   scale_color_brewer(palette = "Set1")+
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
#         axis.title = element_text(size=10, face="bold"),
#         legend.position="none",
#         legend.justification=c(0,1),
#         legend.background = element_rect(fill = 'transparent'));penv
# 
# # multipanel ======================
# ggarrange(ggarrange(pfire, penv, nrow=1, ncol=2, labels = "auto"), bv, 
#           ncol=1, nrow=2, heights = c(.75,1), labels = c("", "c")) %>%
#   ggsave(plot = ., filename = "figures/figure_1_multipanel.png", width=11, height=10)
