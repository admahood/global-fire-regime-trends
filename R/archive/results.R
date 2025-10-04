# setup ========================================================================
library(ggpubr)
library(dplyr)
library(readr)
library(tidyr)
library(tibble)
library(stringr)
library(ggtext)
library(ggrepel)
library(sf)
library(viridisLite)
library(vegan)
cols = c(RColorBrewer::brewer.pal(3,"Dark2")[c(1,2)],"grey90")

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
  dplyr::select(aoi=name_lwr, area_Mkm2) %>%
  left_join(tab)

res %>% filter(all_n_fires >1000) %>% nrow


# pyrodiversity <- res %>%
#   st_set_geometry(NULL) %>%
#   dplyr::select(starts_with("all"), "aoi",
#                 -starts_with("all_log")) %>%
#   filter(!is.na(all_peak_season)) %>%
#   tibble::column_to_rownames("aoi") %>%
#   vegan::diversity() %>%
#   as_tibble(rownames = "aoi") %>%
#   dplyr::rename(pyrodiversity =value)
# 
# res <- res %>%
#   left_join(pyrodiversity)
#   

# fire rotation ================================================================

p_frot<-ggplot(res) +
  geom_sf(aes(fill = fire_rotation), lwd=0.1) +
  scale_fill_steps(trans="log10",low = "firebrick", high = "beige", 
                   name = "Years",
                   breaks = c(5,10,100,500,1000),
                   labels = c("5", "10" , "100","500",   "100")) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  ggtitle("Fire Rotation")

ggsave(p_frot, filename="figures/fire_rotation.png", width = 7, height=4, bg="white")

# ggplot(res) +
#   geom_sf(aes(fill = pyrodiversity), lwd=0.1) +
#   scale_fill_gradient(low = "beige", high = cols[2]) +
#   coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
#   theme_minimal()+
#   ggtitle("Pyrodiversity")+
#   theme(legend.position = "bottom",
#         plot.title = element_text(hjust = 0.5),
#         legend.key.width = unit(2, "cm"),
#         legend.title = element_blank())

# multipanel patterns ==========================================================

pl1<-list()

pl1[[1]]<-ggplot(res) +
  geom_sf(aes(fill = (all_n_fires/area_Mkm2)/19), lwd=0.1) +
  scale_fill_steps2(trans="log",low = "beige", high = cols[2], 
                   breaks = c(300,1000,3000,10000, 30000, 100000, 300000, 1000000)) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  ggtitle("n Fires Mkm-2 year-1")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank())


pl1[[2]]<-ggplot(res) +
  geom_sf(aes(fill = 19/((all_total_ba/1000000)/area_Mkm2)), lwd=0.1) +
    scale_fill_steps2(trans="log10",low = cols[2], mid = "beige", high=cols[1],midpoint = log(10),
                     breaks= c(5,10,50,100,500,1000)) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  ggtitle("Fire Rotation (yrs)", "Time Period/(Burned Area/Land Area)")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank())

testvec<-1:12

lut_ig <- c( "1"="djf","mam" = 3,"mam" = 4,"mam" = 5,
            "djf" = 12,"jja" = 6,"jja" = 7,"jja" = 8,
            "djf" = 2, "son" = 9, "son" = 10, "son" = 11)

pl1[[3]]<-ggplot(res) +
    geom_sf(aes(fill = all_peak_season), lwd=0.1)+
      scale_fill_steps2(low = cols[1],
                        mid = "beige", high=cols[2],
                        midpoint = 180, breaks = c(30,90,150,180,210,270)) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  ggtitle("Peak Season (Day of Year)")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank())

pl1[[4]]<-ggplot(res) +
  geom_sf(aes(fill = all_season_length*2), lwd=0.1) +
    scale_fill_steps2(trans=,low = cols[1], mid= "beige", high = cols[2], 
                      midpoint = 180, breaks = c(30,90,150,180,210,270,330)) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  ggtitle("Season Length (Days)")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank())

pl1[[5]]<-ggplot(res%>% mutate(all_mx_grw = ifelse(all_n_fires < 100, NA, all_mx_grw))) +
  geom_sf(aes(fill = all_mx_grw * 100), lwd=0.1) +
    scale_fill_steps(trans="log10",low = "beige", high = cols[2]) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  ggtitle("Max Growth (ha)")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank())

pl1[[6]]<-ggplot(res) +
  geom_sf(aes(fill = all_fsr*100), lwd=0.1) +
    scale_fill_steps2(trans="log10",low = cols[1], mid = "beige", high = cols[2],
                      midpoint=1.75,
                     breaks = c(30,40,50,70,100,300)) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  ggtitle("Fire Spread Rate (ha/day)")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank())

pl1[[7]]<-ggplot(res) +
  geom_sf(aes(fill = all_dur), lwd=0.1) +
 scale_fill_steps2(low = cols[1], mid= "beige", high = cols[2], midpoint=3,
                   breaks = c(2,3,4)) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  ggtitle("Event Duration (days)")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank())

pl1[[8]]<-ggplot(res) +
  geom_sf(aes(fill = all_size*100), lwd=0.1) +
 scale_fill_steps2(trans="log10",low =cols[1],
                  midpoint = 2.1,mid= "beige", high = cols[2]) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  ggtitle("Fire size (ha)")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank())

ggarrange(plotlist=pl1, nrow=4, ncol=2) -> p_patterns

ggsave(p_patterns, filename = "figures/multipanel_patterns_map.png", 
       width=10, height=12, bg="white")

# multipanel trends ===========================================================

cols <- RColorBrewer::brewer.pal(3,"Set1")
cols <- c(cols[2], cols[1], "grey90")

trend_sl <- ggplot(res) +
  geom_sf(aes(fill = trend_season_length), lwd=0.1) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal() +
  scale_fill_manual(values = cols, name="Trend in Annual Means ", na.value = "white")+
  ggtitle("Season Length")


trend_tba<-ggplot(res) +
  geom_sf(aes(fill = trend_total_ba), lwd=0.1) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal() +
  scale_fill_manual(values = cols, name="Trend in Annual Means ", na.value = "white")+
  ggtitle("Total Burned Area")

trend_tba_l <-ggplot(res) +
  geom_sf(aes(fill = trend_log_tba), lwd=0.1) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal() +
  scale_fill_manual(values = cols, name="Trend in Annual Means ", na.value = "white")+
  ggtitle("Total Burned Area (log)")

trend_fsr <- ggplot(res) +
  geom_sf(aes(fill = trend_fsr), lwd=0.1)+
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  scale_fill_manual(values = cols, name="Trend in Annual Means ", na.value = "white")+
  ggtitle("Mean Growth Rate")

trend_fsr_l <- ggplot(res) +
  geom_sf(aes(fill = trend_log_fsr_km2_dy), lwd=0.1)+
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  scale_fill_manual(values = cols, name="Trend in Annual Means ", na.value = "white")+
  ggtitle("Fire Spread Rate (log)")

trend_mg <- ggplot(res) +
  geom_sf(aes(fill = trend_mx_grw), lwd=0.1) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  scale_fill_manual(values = cols, name="Trend in Annual Means ", na.value = "white")+
  ggtitle("Maximum Daily Growth Rate")

trend_mg_l <- ggplot(res) +
  geom_sf(aes(fill = trend_log_mx_grw_km2), lwd=0.1) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  scale_fill_manual(values = cols, name="Trend in Annual Means ", na.value = "white")+
  ggtitle("Max Growth (log)")


trend_size <- ggplot(res) +
  geom_sf(aes(fill = trend_size), lwd=0.1) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  scale_fill_manual(values = cols, name="Trend in Annual Means ", na.value = "white")+
  ggtitle("Event Size")

trend_size_l <- ggplot(res) +
  geom_sf(aes(fill = trend_log_size), lwd=0.1) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  scale_fill_manual(values = cols, name="Trend in Annual Means ", na.value = "white")+
  ggtitle("Fire Size (log)")

trend_pm <-ggplot(res) +
  geom_sf(aes(fill = trend_peak_season), lwd=0.1) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  scale_fill_manual(values = cols, name="Trend in Annual Means ", na.value = "white")+
  ggtitle("Peak Month")

trend_n <- ggplot(res) +
  geom_sf(aes(fill = trend_n_fires), lwd=0.1) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  scale_fill_manual(values = cols, name="Trend in Annual Means ", na.value = "white")+
  ggtitle("Number of Fires")

trend_n_l <- ggplot(res) +
  geom_sf(aes(fill = trend_log_n_fires), lwd=0.1) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  scale_fill_manual(values = cols, name="Trend in Annual Means ", na.value = "white")+
  ggtitle("n Fires (log)")

trend_dur <-ggplot(res) +
  geom_sf(aes(fill = trend_dur), lwd=0.1) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  scale_fill_manual(values = cols, name="Trend in Annual Means ", na.value = "white")+
  ggtitle("Event Duration") 

trend_dur_l <-ggplot(res) +
  geom_sf(aes(fill = trend_log_dur), lwd=0.1) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  scale_fill_manual(values = cols, name="Trend in Annual Means ", na.value = "white")+
  ggtitle("Event Duration (log)") 

multipanel<-ggarrange(plotlist = list(trend_tba,trend_n,
                                      trend_sl, trend_pm,
                                      trend_size, trend_dur,
                                      trend_mg, trend_fsr),
                      labels = "auto",label.x = 0.9,
                      ncol=2, nrow=4, common.legend = TRUE)

ggsave(multipanel, filename = "figures/multipanel_trend_map.png", width=10, height=12, bg="white")

# multipanel htests ============================================================


test_sl<-ggplot(res) +
  geom_sf(aes(fill = test_season_length), lwd=0.1) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal() +
  scale_fill_manual(values = cols, name="test", na.value = "white")+
  ggtitle("Season Length")


test_tba <-ggplot(res) +
  geom_sf(aes(fill = test_total_ba), lwd=0.1) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal() +
  scale_fill_manual(values = cols, name="test", na.value = "white")+
  ggtitle("Total Burned Area")

test_fsr <-ggplot(res) +
  geom_sf(aes(fill = test_fsr), lwd=0.1)+
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  scale_fill_manual(values = cols, name="test", na.value = "white")+
  ggtitle("Fire Spread Rate")

test_mg <-ggplot(res) +
  geom_sf(aes(fill = test_mx_grw), lwd=0.1) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  scale_fill_manual(values = cols, name="test", na.value = "white")+
  ggtitle("Max Growth")

test_size<-ggplot(res) +
  geom_sf(aes(fill = test_size), lwd=0.1) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  scale_fill_manual(values = cols, name="test", na.value = "white")+
  ggtitle("Fire Size")

test_pm<-ggplot(res) +
  geom_sf(aes(fill = test_peak_season), lwd=0.1) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  scale_fill_manual(values = cols, name="test", na.value = "white")+
  ggtitle("Peak Month")

test_n<-ggplot(res) +
  geom_sf(aes(fill = test_n_fires), lwd=0.1) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  scale_fill_manual(values = cols, name="test", na.value = "white")+
  ggtitle("n Fires")

test_dur <-ggplot(res) +
  geom_sf(aes(fill = test_dur), lwd=0.1) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  scale_fill_manual(values = cols, name="test", na.value = "white")+
  ggtitle("Event Duration") 

multipanel<-ggarrange(plotlist = list(test_sl, test_pm,
                                      test_tba, test_size,
                                      test_mg, test_fsr,
                                      test_n, test_dur),
                      ncol=2, nrow=4, common.legend = TRUE)
ggsave(multipanel, filename = "figures/multipanel_test_map.png", width=10, height=8, bg="white")




# NMDS ========================

df<-res %>%
  st_set_geometry(NULL) %>%
  dplyr::select(starts_with("all"),
                -starts_with("all_log"),
                area_Mkm2, aoi,median_ai, mad_ai,
                koppen_mode,
                contains("precip"), 
                contains("temper"), 
                contains("range"), 
                starts_with("pct") ) %>%
  column_to_rownames("aoi") %>%
  na.omit() %>%
  dplyr::select(starts_with("all"),area_Mkm2, koppen_mode) %>%
  mutate(tba_per_area = all_total_ba/area_Mkm2) %>%
  dplyr::select(-all_total_ba,-area_Mkm2) %>%
  filter(all_n_fires > 50)

vegan::metaMDS(df %>% dplyr::select(-koppen_mode)# %>%
                 #wisconsin()
               ) -> nmds


env<-res %>%
  st_set_geometry(NULL) %>%
  filter(all_n_fires > 50)%>%
  dplyr::select(starts_with("all"),area_Mkm2, aoi,contains("precip"), median_ai, mad_ai,
                contains("temper"), contains("range"), starts_with("pct") ) %>%
  column_to_rownames("aoi")%>%
  na.omit()%>%
  dplyr::select(contains("precip"), contains("temper"), contains("range"), median_ai, mad_ai, starts_with("pct") )
  
fit <- vegan::envfit(nmds,env, perm=9999,p.max = 0.0001)
fit1 <- vegan::envfit(nmds,df, perm=9999,p.max = 0.0001)
# plot(nmds, type="t",display = "sites")
# plot(fit1)

scores <- as.data.frame(scores(nmds)$sites) %>%
  tibble::rownames_to_column("aoi") %>%
  left_join(res %>% st_set_geometry(NULL) %>% dplyr::select(aoi, koppen_mode))

env <- as.data.frame(fit$vectors$arrows*sqrt(fit$vectors$r))  %>%
  tibble::rownames_to_column("env") %>%
  mutate(p = fit$vectors$pvals,
         R2= fit$vectors$r,
         env = str_replace_all(env,"quarter", "Q"),
         env = str_replace_all(env,"temperature", "T"),
         env = str_replace_all(env,"precipitation", "PPT")) %>%
  filter(R2>0.2)

fire <-as.data.frame(fit1$vectors$arrows*sqrt(fit1$vectors$r)) %>%
  tibble::rownames_to_column("fire_characteristic") %>%
  mutate(fire_characteristic = str_remove_all(fire_characteristic,"all_"),
         fire_characteristic = str_replace(fire_characteristic,"size", "Fire Size"),
         fire_characteristic = str_replace(fire_characteristic,"dur", "Event Duration"),
         fire_characteristic = str_replace(fire_characteristic,"season_length", "Season Length"),
         fire_characteristic = str_replace(fire_characteristic,"fsr", "Fire Spread Rate"),
         fire_characteristic = str_replace(fire_characteristic,"n_fires", "Number of Fires"),
         fire_characteristic = str_replace(fire_characteristic,"tba_per_area", "Total Burned Area"),
         fire_characteristic = str_replace(fire_characteristic,"mx_grw", "Max Growth"),
         p = fit1$vectors$pvals,
         R2= fit1$vectors$r)%>%
  filter(p<0.001)

# mutate(species = recode_factor(species,`ppt_2yr` = 'P[ant]', 
#                                `sd_def` = 'sigma[CWD]',
#                                `mean_aet` = 'AET',
#                                `mean_tmin` = 'T[min]'))

ptext <- ggplot(scores, aes(x=NMDS1, y=NMDS2, color = koppen_mode)) +
  geom_text(aes(label=aoi)) +
  theme_classic()+
  geom_segment(data = fire,x=0,y=0,arrow = arrow(), color = "grey",
               aes(yend = NMDS2/2, xend = NMDS1/2), lwd=1)+
  geom_text_repel(data=fire, #parse = T,
                  aes(x=NMDS1/2,y=NMDS2/2,label=fire_characteristic),color="black",
                  size=5, fontface = "bold")+
  # coord_fixed()+
  scale_color_brewer(palette = "Set1")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.title = element_text(size=17, face="bold"),
        legend.position=c(0,1),
        legend.title = element_blank(),
        legend.justification=c(0,1),
        legend.background = element_rect(fill = 'transparent'))
ggsave(ptext, filename = "figures/nmds_text.png", bg="white", width=10, height=10)

penv<-ggplot(data=scores) +
  geom_point(aes(x=NMDS1, y=NMDS2, color = koppen_mode),
             size= 4, stroke =1.5, alpha = 0.25) +
  geom_segment(data = env,x=0,y=0,arrow = arrow(), color = "grey",
               aes(yend = NMDS2, xend = NMDS1), lwd=1)+
  geom_text_repel(data=env, parse = T,
                  aes(x=NMDS1,y=NMDS2,label=env),# hjust = "left",
                  size=5,nudge_y = c(0.05,0,0,0), nudge_x = -.1,
                  fontface = "bold")+
  theme_classic()+
  scale_color_brewer(palette = "Set1")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.title = element_text(size=17, face="bold"),
        legend.position=c(0,1),
        legend.justification=c(0,1),
        legend.background = element_rect(fill = 'transparent'))

ggsave(penv, filename = "figures/nmds_env.png", bg="white", width=10, height=10)



# text_plots ===================

pba_s<- ggplot(res |> filter(!is.na(koppen_mode)),aes(y=all_total_ba/area_Mkm2, x=all_size)) +
  geom_point(aes(label = aoi, color = koppen_mode)) +
  scale_x_log10() +
  scale_y_log10() +
  ylab("Burned Area/Total Area (Mkm2)") +
  xlab("Event Size (Km2)") +
  scale_color_discrete(name = "Koppen-Geiger Climate Zone") +
  theme_classic()
ggsave(pba_s,filename = "figures/ba_size.png", bg="white", width=10, height=7)
  
pba_n<- ggplot(res|> filter(!is.na(koppen_mode)),aes(x=all_n_fires, y=all_total_ba/area_Mkm2)) +
  scale_x_log10() +
  scale_y_log10() +
  ylab("Burned Area/Total Area (Mkm2)") +
  xlab("Number of Fires") +
  scale_color_discrete(name = "Koppen-Geiger Climate Zone")+
  geom_point(aes(label = aoi, color = koppen_mode)) +
  theme_classic()
ggsave(pba_n,filename = "figures/ba_n.png", bg="white", width=10, height=7)

pba_fsr<- ggplot(res|> filter(!is.na(koppen_mode)),aes(y=all_fsr, x=all_total_ba/area_Mkm2)) +
  geom_point(aes(label = aoi, color = koppen_mode))  +
  scale_x_log10() +
  scale_y_log10()  +
  scale_color_discrete(name = "Koppen-Geiger Climate Zone")+
  ylab("Burned Area/Total Area (Mkm2)") +
  xlab("Mean Growth Rate")+
  theme_classic()
ggsave(pba_fsr,filename = "figures/ba_fsr.png", bg="white", width=10, height=7)

pmg_s<- ggplot(res|> filter(!is.na(koppen_mode)),aes(x=(all_mx_grw),y=(all_size)))  +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_discrete(name = "Koppen-Geiger Climate Zone") +
  ylab("Fire Event Size") +
  xlab("Max Growth Rate")+
  geom_point(aes(label = aoi, color = koppen_mode)) +
  theme_classic()
ggsave(pmg_s,filename = "figures/mxg_size.png", bg="white", width=10, height=7)

pp11<- ggplot(res|> filter(!is.na(koppen_mode)),aes(y=(all_fsr),x=(all_mx_grw))) +
  scale_x_log10() +
  scale_y_log10()  +
  scale_color_discrete(name = "Koppen-Geiger Climate Zone")+
  ylab("Mean Growth Rate") +
  xlab("Max Growth Rate")+
  geom_point(aes(label = aoi, color = koppen_mode)) +
  theme_classic()
ggsave(pp11,filename = "figures/fsr_mx_grw.png", bg="white", width=10, height=7)

ppdur<- ggplot(res|> filter(!is.na(koppen_mode)),aes(y=(all_n_fires),x=(all_dur))) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_discrete(name = "Koppen-Geiger Climate Zone") +
  ylab("Number of Fires") +
  xlab("Event Duration")+
  geom_point(aes(label = aoi, color = koppen_mode)) +
  theme_classic()
ggsave(ppdur,filename = "figures/fsr_mx_grw.png", bg="white", width=10, height=7)


ggarrange(pba_n, pba_s, pba_fsr, pp11, pmg_s, ppdur, common.legend = TRUE, 
          ncol = 3, nrow = 2) |>
  ggsave(filename = 'figures/multipanel_scatterplots.png', bg='white', width=10, height=7)

res %>%
  dplyr::select(aoi, temperature_seasonality,koppen_mode, starts_with("all"), -all_fsr) %>%
  mutate(all_size = log(all_size), all_total_ba = log(all_total_ba), 
         all_n_fires = log(all_n_fires), all_mx_grw = log(all_mx_grw))%>%
  st_set_geometry(NULL) %>%
  pivot_longer(cols = names(.)[4:ncol(.)],
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(x=temperature_seasonality, y=value)) +
  geom_point(aes(color = koppen_mode)) +
  geom_smooth(method = lm, se=T)+
  facet_wrap(~variable, scales = "free")

# pyrodiversity

pyrodiversity <- df %>% dplyr::select(-koppen_mode) %>%
  wisconsin() %>%
  vegan::diversity()

boxplots<- mutate(df, pyrodiversity = pyrodiversity) %>%
  tibble::rownames_to_column("aoi") %>%
  filter(koppen_mode != "Polar") %>%
  dplyr::select(aoi, koppen_mode, starts_with("all"), -all_fsr,
                tba_per_area,pyrodiversity) %>%
  mutate(all_size = log(all_size), tba_per_area = log(tba_per_area), 
         all_n_fires = log(all_n_fires), all_mx_grw = log(all_mx_grw))%>%
  pivot_longer(cols = names(.)[3:ncol(.)],
               names_to = "variable",
               values_to = "value") %>%
  ggplot(aes(x=koppen_mode, y=value, fill = koppen_mode)) +
  geom_boxplot()+
  facet_wrap(~variable, scales = "free")+
  theme_minimal() +
  theme(axis.text.x = element_blank())
ggsave(boxplots, filename = "figures/boxplots.png", bg = "white", width=10, height=10)


# climate maps =================================================================
library(purrr)
library(mblm)

get_means <- function(wrngld, var){
  wrngld %>%
    group_by(name_lwr) %>%
    summarise(mean = mean(value)) %>%
    ungroup()
}

wrangle_climate <- function(climate_df){
  read_csv(climate_df) %>%
    pivot_longer(cols = starts_with("2")) %>%
    mutate(year = str_sub(name, 1,4),
           variable = str_sub(name, 6,13)) %>%
    dplyr::select(-`system:index`, -`.geo`,-name) %>%
    filter(!is.na(value)) %>%
    mutate(name_lwr = NAME_EN %>% str_to_lower() %>%
             str_replace_all(" ", "_")) %>%
    mutate(name_lwr = ifelse(name_lwr == "the_gambia", "gambia", name_lwr),
           name_lwr = ifelse(name_lwr == "people's_republic_of_china",
                             "china",name_lwr),
           name_lwr = ifelse(name_lwr == "united_states_of_america",
                             "conus_ak", name_lwr),
           year=as.numeric(year)) 
  }
remove_int <- function(df){
  nms <- names(df)
  df %>%
    as.data.frame()%>%
    tibble::rownames_to_column("var") %>%
    filter(var == "year")
}

dfl <- list.files("data/terraclim_summaries/vpd", full.names = T,recursive = T)
plist <- list()
for(i in 1:length(dfl)){

  varname <- str_c(dfl[[i]] %>%  str_split(c("_")) %>%  unlist() %>%  pluck(4),
                   dfl[[i]] %>%  str_split(c("_")) %>%  unlist() %>%  pluck(6)) %>%
    str_remove_all(".csv")
  wrngld <- wrangle_climate(dfl[[i]])
  clim_trends <-wrngld %>%
    split(.$name_lwr) %>% # from base R
    map(~ mblm(value ~ year, data = ., repeated=TRUE)) %>%
    map(summary) %>%
    map_df("coefficients")
  nms <- names(clim_trends)
  clm_trnds <- lapply(clim_trends, remove_int) %>%
    bind_rows() %>%
    mutate(name_lwr = nms)

  plist[[i]] <- world_boundaries %>%
    dplyr::select(name_lwr, area_Mkm2) %>%
    left_join(clm_trnds)%>%
    mutate(Estimate = ifelse(`Pr(>|V|)` < 0.01, Estimate, NA))%>%
  ggplot() +
    geom_sf(aes(fill = Estimate), lwd=0.1) +
    coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
    theme_void()+
    scale_fill_gradient2(low=cols[1], high="chocolate4", na.value = "grey90"
                         ,name = "Theil-Sen Estimate",
                         breaks =c(round(min(clm_trnds$Estimate),1), 0, 
                                   round(quantile(clm_trnds$Estimate,
                                                  probs = 0.99)%>% as.numeric()
                                         ,1))
                         )+
    ggtitle(varname)+
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5))
}

ggarrange(plotlist = plist, nrow=2, ncol=2, common.legend = FALSE, legend = "bottom") %>%
  ggsave(plot = ., filename = "figures/climate_trends.png", bg="white", width =8, height = 5)


# new gee outputs initial wrangling ============================================

# 1 time thing - getting rid of the gee columns that take up 170 mb

# pop <- read_csv("data/too_big/annual_population_count.csv") %>%
#   dplyr::select(aoi, population = sum, year)
# 
# write_csv(pop, file = "data/gee_outputs/annual_population_count.csv")
# 
# lc_area <- read_csv("data/too_big/annual_landcover_area.csv") %>%
#   dplyr::select(aoi, cropland, cropland_mosaics, date, forest, forest_savannas, grassland, urban_built)
# 
# write_csv(lc_area, file = "data/gee_outputs/annual_landcover_area.csv")
# 
# flammable_area <- read_csv("data/too_big/annual_flammable_area.csv") %>%
#   dplyr::select(aoi, date, flammable_area = sum)
# 
# write_csv(flammable_area, file = "data/gee_outputs/annual_flammable_area.csv")

# new climate wrangling function

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
  left_join(tab %>% dplyr::select(koppen_mode, aoi))

big_d <- left_join(mean_vpd, lc_area) %>%
  left_join(min_vpd) %>%
  left_join(max_vpd)%>%
  left_join(pop) %>%
  filter(!is.na(mean_vpd)) %>%
  na.omit() %>%
  group_by(aoi) %>%
  mutate(n=n()) %>%
  ungroup() 

library(mgcv)
library(nlme)

aois <- big_d$aoi %>% unique()

clm_lc_trends <- tibble("aoi" = NA, "mean_vpd" = NA, "mean_vpd_anom" = NA, 
                        "cropland" = NA, "cropland_mosaics" = NA, "forest" = NA,
                        "forest_savannas" = NA, "grassland" = NA,
                        "urban_built" = NA, "min_vpd" = NA, "min_vpd_anom" = NA,
                        "max_vpd" = NA, "max_vpd_anom" = NA, "population" = NA)

for(i in 1:length(aois)){
  dd <- filter(big_d, aoi == aois[i]) 
  clm_lc_trends[i,1] <- aois[i]

  print(nrow(dd))
  if(nrow(dd) > 14){
    for(j in 2:ncol(clm_lc_trends)){
      f <- formula(paste(names(clm_lc_trends)[j], "~ year"))
      mod <- mblm::mblm(f, data = dd) %>%
        broom::tidy()
      if(!is.na(mod[2,5]) & mod[2,5] < 0.05) {clm_lc_trends[i,j] <- mod[2,2]}else{clm_lc_trends[i,j] <- 0}
    }
  }else{print("insufficient data")}
}

clm_lc_poly <- world_boundaries %>%
  dplyr::select(aoi=name_lwr) %>%
  left_join(clm_lc_trends)

clm_plotlist <- list()

# vpd plots ================================================================

v1 <- ggplot(clm_lc_poly) +
  geom_sf(aes(fill = mean_vpd)) +
  scale_fill_gradient2(high = "chocolate4", low="turquoise4") +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  ggtitle("mean vpd")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank())

va1 <- ggplot(clm_lc_poly) +
  geom_sf(aes(fill = mean_vpd_anom)) +
  scale_fill_gradient2(high = "chocolate4", low="turquoise4") +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  ggtitle("mean vpd anomaly")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank())

v2<-ggplot(clm_lc_poly) +
  geom_sf(aes(fill = min_vpd)) +
  scale_fill_gradient2(high = "chocolate4", low="turquoise4") +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  ggtitle("min vpd")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank())

va2<-ggplot(clm_lc_poly) +
  geom_sf(aes(fill = min_vpd_anom)) +
  scale_fill_gradient2(high = "chocolate4", low="turquoise4") +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  ggtitle("min vpd anomaly")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank())

v3<-ggplot(clm_lc_poly) +
  geom_sf(aes(fill = max_vpd)) +
  scale_fill_gradient2(high = "chocolate4", low="turquoise4") +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  ggtitle("max vpd")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank())

va3<-ggplot(clm_lc_poly) +
  geom_sf(aes(fill = max_vpd_anom)) +
  scale_fill_gradient2(high = "chocolate4", low="turquoise4") +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  ggtitle("max vpd anomaly")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank())

ggsave(filename = "figures/vpd_oct22_all.png",
       plot = ggarrange(plotlist = list(v1,va1,v2,va2,v3, va3), nrow=3, ncol=2),
       width=15, height=13, bg="white")

# landuselandcover ========
gr <- ggplot(clm_lc_poly) +
  geom_sf(aes(fill = grassland)) +
  scale_fill_gradient2() +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  ggtitle("grassland")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank())

cr <- ggplot(clm_lc_poly) +
  geom_sf(aes(fill =cropland)) +
  scale_fill_gradient2() +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  ggtitle("cropland")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank())


cm <- ggplot(clm_lc_poly) +
  geom_sf(aes(fill =cropland_mosaics)) +
  scale_fill_gradient2() +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  ggtitle("cropland mosaics")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank())


fo <- ggplot(clm_lc_poly) +
  geom_sf(aes(fill =forest)) +
  scale_fill_gradient2() +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  ggtitle("forest")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank())


fs <- ggplot(clm_lc_poly) +
  geom_sf(aes(fill =forest_savannas)) +
  scale_fill_gradient2() +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  ggtitle("forest_savannas")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank())

ub <- ggplot(clm_lc_poly) +
  geom_sf(aes(fill =urban_built)) +
  scale_fill_gradient2() +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  ggtitle("urban_built")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank())


pop<- ggplot(clm_lc_poly) +
  geom_sf(aes(fill =population)) +
  scale_fill_gradient2() +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_minimal()+
  ggtitle("population")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank())

ggsave(filename = "figures/lc_oct22_all.png",
       plot = ggarrange(plotlist = list(gr,cr, cm,fo, fs,ub, pop), nrow=4, ncol=2),
       width=15, height=17, bg="white")



# explaining things? =======================
library(lme4)
library(lmerTest)
library(brms)
library(ggeffects)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(broom)
library(broom.mixed)
bd <- filter(big_d_w_fire, n>15)


ba_mod <- lmerTest::lmer(log(total_ba+1) ~ 
                           (mean_vpd_anom + 
                              grassland + 
                              forest + cropland + cropland_mosaics + 
                              urban_built):koppen_mode + 
                           (1|aoi), data = bd)
performance::check_model(ba_mod)
car::vif(ba_mod)
summary(ba_mod)

broom.mixed::tidy(ba_mod) %>%
  separate(term,into = c("variable", "koppen"),sep = ":") %>%
  mutate(koppen = str_remove_all(koppen, "koppen_mode")) %>%
  mutate_if(is.numeric, round, 6) %>%
  write.csv("data/lmer_results.csv")

sjPlot::plot_model(ba_mod, type = "re")

p_re <- lme4::ranef(ba_mod) %>%
  as.data.frame() %>%
    ggplot(aes(y=grp,x=condval)) +
    geom_point() + 
    facet_wrap(~term,scales="free_x") +
    geom_errorbarh(aes(xmin=condval -2*condsd,
                       xmax=condval +2*condsd), height=0)
ggsave(p_re, filename = "figures/re.png", height=15, width=10)
ggResidpanel::resid_panel(ba_mod)


# bayesian stuff
# not converging :(
ba_brm <- brm(total_ba ~
                (mean_vpd_anom + grassland + cropland)*koppen_mode +
                (1|aoi),family = lognormal, data = bd, iter = 3000)

summary(ba_brm)

plot(ggeffects::ggeffect(ba_mod), facets=T, add.data=F)

lm_mod <- lme4::lmer(log(total_ba) ~ mean_vpd_anom + forest + grassland + forest_savannas +
                       cropland + urban_built + (1|aoi), data = bd)
summary(lm_mod)
# models ============
ba_trends <-big_d %>%
  split(.$aoi) %>% # from base R
  map(~ gls(total_ba ~ (mean_vpd_anom+min_vpd_anom+ population +
                         cropland+grassland + max_vpd_anom)*year,
             data = ., correlation = corAR1(form = ~ 1 | year))) %>%
  map(broom::tidy) %>%
  bind_rows(.id = "aoi")

ba_trends <-big_d %>%
  split(.$aoi) %>% # from base R
  map(~ lm(total_ba ~ (mean_vpd_anom+min_vpd_anom+ population +
                          cropland+grassland + max_vpd_anom)*year,
            data = .)) %>%
  map(broom::tidy) %>%
  bind_rows(.id = "aoi")

filter(ba_trends, p.value < 0.05)
filter(ba_trends, p.value < 0.05) %>% pull(aoi) %>% uniqu



# individual country deep dives
m1<- big_d %>%
  filter(aoi == "vietnam") %>%
  gls(total_ba ~ min_vpd_anom+population + cropland+grassland + max_vpd_anom+year,
   data = ., correlation = corAR1(form = ~ 1 | year)) 

summary(m1)
ggResidpanel::resid_panel(m1)

m2<-big_d %>%
  filter(aoi == "vietnam") %>%
  mgcv::gamm(total_ba ~ s(min_vpd_anom)+ population + s(cropland)+
               grassland + max_vpd_anom ,
      data = ., correlation = corAR1(form = ~ 1 | year))
plot(m2$gam, residuals = TRUE)
summary(m2$gam)
gam.check(m2$gam)

ggeffects::ggpredict(m2) %>% plot(facets=TRUE, add.data=TRUE)


big_d %>%
  filter(aoi == "bolivia") %>%
  ggplot(aes(x=population, y = total_ba)) +
  geom_point()
  
big_d %>%
  filter(aoi == "bolivia") %>%
  ggplot(aes(x=max_vpd_anom, y = total_ba)) +
  geom_point()

ggeffects::ggpredict(m2$gam) %>% plot(facets=TRUE, add.data=TRUE)

m2<-big_d %>%
  filter(aoi == "bolivia") %>%
  mgcv::gamm(total_ba ~ s(min_vpd_anom)+ s(population) + s(cropland)+
               s(grassland) + s(max_vpd_anom),
             data = ., correlation = corAR1(form = ~1| year))# %>%
  # pluck(2)%>%
  # broom::tidy()

library(brms)
m2b<-big_d %>%
  filter(aoi == "bolivia") %>%
  brms::brm(total_ba ~ s(min_vpd_anom)+ s(population) + s(cropland)+
               s(grassland) + s(max_vpd_anom) + ar(time = year),
             data = .) 


plist <- list()
for(i in 2:length(unique(clim_trends$term))){
plist[[i]] <- world_boundaries %>%
  dplyr::select(aoi = name_lwr, area_Mkm2) %>%
  left_join(ba_trends %>% dplyr::filter(term == unique(ba_trends$term)[i]))%>%
  mutate(estimate = ifelse(p.value < 0.1, estimate, NA))%>%
  ggplot() +
  geom_sf(aes(fill = estimate), lwd=0.1) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_void()+
  scale_fill_steps2(na.value = "grey90")+
  ggtitle(unique(ba_trends$term)[i])+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

print(plist[[i]])
}

main_var <- clim_trends %>%
  filter(term != "(Intercept)",
         term != "year") %>%
  group_by(aoi) %>%
  mutate(termnum = 1:n(),
         main_var = which.min(p.value)) %>%
  ungroup() %>%
  filter(termnum == main_var)

n_vars <- clim_trends %>%
  filter(term != "(Intercept)",
         term != "year") %>%
  group_by(aoi) %>%
  mutate(termnum = 1:n(),
         main_var = which.min(p.value)) %>%
  ungroup() %>%
  filter(termnum == main_var)

lulc <- RColorBrewer::brewer.pal(3, "Blues")
vpd <- RColorBrewer::brewer.pal(3, "YlOrRd")
cols <- c(lulc[1:2], vpd, lulc[3])
best_predictor <- world_boundaries %>%
  dplyr::select(aoi = name_lwr, area_Mkm2) %>%
  left_join(main_var)%>%
  mutate(sig = ifelse(p.value < 0.1, "is_significant", "not"),
         sign = ifelse(estimate <0, "negative", "positive"))%>%
  ggplot() +
  geom_sf(aes(fill = term, lty = sig, color = sign)) +
  coord_sf(crs = wcrs,ylim = c(-5500000,7200000)) +
  theme_void()+
  scale_fill_manual(values = cols, na.value = "grey90")+
  scale_color_manual(values = c("darkblue", "firebrick"), na.value = "grey")+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Best predictor (lowest p-value) of total burned area");best_predictor

ggsave("figures/best_predictor_total_ba.png")


wb_ts <- world_boundaries %>%
  dplyr::select(aoi = name_lwr, area_Mkm2, continent = CONTINENT) %>%
  left_join(big_d)

bigmod<-lm(total_ba ~ (min_vpd_anom+population + I(cropland+cropland_mosaics) +
                         grassland +forest+ forest_savannas+urban_built+
                         max_vpd_anom +year):aoi,
                       data = wb_ts %>% mutate_if(is.numeric, scale))
broom::tidy(bigmod) %>%
  filter(p.value < 0.05) %>%
  arrange(p.value) %>%
  print(n=50)
library(lme4); library(lmerTest)

bigmod<-lmer(total_ba ~ min_vpd_anom+population + I(cropland+cropland_mosaics) +
                         grassland +forest+ forest_savannas+urban_built+year+
                         max_vpd_anom + (1|aoi), 
           data = wb_ts %>% mutate_if(is.numeric, scale))
summary(bigmod)

africabigmod<-lmer(total_ba ~ min_vpd_anom+population + I(cropland+cropland_mosaics) +
               grassland +forest+ forest_savannas+urban_built+year+
               max_vpd_anom + (1|aoi), 
             data = wb_ts %>% mutate_if(is.numeric, scale) %>%
               filter(continent == "Africa"))
summary(africabigmod)

ggplot(wb_ts %>% mutate_if(is.numeric, scale) %>%
         filter(continent == "Africa"),
       aes(x=log(forest_savannas+1), y=total_ba, color= aoi)) +
  geom_point() +
  theme(legend.position = "none") 

na_bigmod<-lmer(total_ba ~ min_vpd_anom+population + I(cropland+cropland_mosaics) +
                     grassland +forest+ forest_savannas+urban_built+year+
                     max_vpd_anom + (1|aoi), 
                   data = wb_ts %>% mutate_if(is.numeric, scale) %>%
                     filter(continent == "North America"))
summary(na_bigmod)

sa_bigmod<-lmer(total_ba ~ min_vpd_anom+population + I(cropland+cropland_mosaics) +
                  grassland +forest+ forest_savannas+urban_built+year+
                  max_vpd_anom + (year|aoi), 
                data = wb_ts %>% mutate_if(is.numeric, scale) %>%
                  filter(continent == "South America"))
summary(sa_bigmod)


