# fig 2 correlations
# corrplots=====================

# a: top panel
df <- read_csv('data/country_x_kg.csv')  %>%
  dplyr::select(starts_with("all"), 'all_season_length', kg = koppen, aoi, 
                -starts_with("all_log"))|>
  mutate(name_lwr = str_remove_all(aoi, '_Temperate') |> 
           str_remove_all("_Arid") |> str_remove_all("_Equatorial") |> 
           str_remove_all('_Polar') |> str_remove_all("_Boreal"))  |>
  left_join(read_csv('data/npix.csv')) |>
  dplyr::mutate(all_total_ba = all_total_ba/n_pix,
                all_n_fires = all_n_fires/n_pix) |>
  dplyr::select(`Peak Season` = all_peak_season,
                `Season Length` = all_season_length,
                `Size` = all_size,
                `Burned Area` = all_total_ba,
                `N Fires` = all_n_fires,
                `Duration` = all_dur,
                `Mean Growth` = all_fsr,
                `Max Growth` = all_mx_grw
  )

cor_p <- ggcorrplot::cor_pmat(df)
top <- ggcorrplot::ggcorrplot(cor(df, use = 'pairwise.complete.obs', method = "pearson"), 
                       p.mat = cor_p, type = 'lower',
                       hc.method = 'single', hc.order = T, lab = T,
                       sig.level = 0.05, insig = 'blank') +
  ggtitle('(a) Fire Attributes') +
  geom_vline(xintercept = 3.5, lty = 2) +
  geom_hline(yintercept = 4.5, lty=2) +
  theme(legend.position = 'none', text = element_text()) +
  geom_text(aes(x=5, y=7, label = "Area\nBased"))+
  geom_text(aes(x=1, y=3, label = "Event\nBased"));top

# b: bottom panel

lut_wc <- c(
  
  'wc2.1_2.5m_bio_1' = 'Annual Mean Temperature',
  
  'wc2.1_2.5m_bio_2' = 'Mean Diurnal Range',
  
  'wc2.1_2.5m_bio_3' = 'Isothermality',
  
  'wc2.1_2.5m_bio_4' = 'Temperature Seasonality',
  
  'wc2.1_2.5m_bio_5' = 'Max Temperature of Warmest Month',
  
  'wc2.1_2.5m_bio_6' = 'Min Temperature of Coldest Month',
  
  'wc2.1_2.5m_bio_7' = 'Temperature Annual Range',
  
  'wc2.1_2.5m_bio_8' = 'Mean Temperature of Wettest Quarter',
  
  'wc2.1_2.5m_bio_9' = 'Mean Temperature of Driest Quarter',
  
  'wc2.1_2.5m_bio_10' = 'Mean Temperature of Warmest Quarter',
  
  'wc2.1_2.5m_bio_11' = 'Mean Temperature of Coldest Quarter',
  
  'wc2.1_2.5m_bio_12' = 'Annual Precipitation',
  
  'wc2.1_2.5m_bio_13' = 'Precipitation of Wettest Month',
  
  'wc2.1_2.5m_bio_14' = 'Precipitation of Driest Month',
  
  'wc2.1_2.5m_bio_15' = 'Precipitation Seasonality',
  
  'wc2.1_2.5m_bio_16' = 'Precipitation of Wettest Quarter',
  
  'wc2.1_2.5m_bio_17' = 'Precipitation of Driest Quarter',
  
  'wc2.1_2.5m_bio_18' = 'Precipitation of Warmest Quarter',
  
  'wc2.1_2.5m_bio_19' = 'Precipitation of Coldest Quarter'
) 


ccwc <- read_csv('data/wc_kg_country.csv') |>
  pivot_wider(names_from = 'wcbio') |>
  mutate(aoi = str_c(name_lwr, "_", kg)) |>
  dplyr::select(-kg, -name_lwr)
  
ccdf <- read_csv('data/country_x_kg.csv')  %>%
  dplyr::select(starts_with("all"), 'all_season_length', kg = koppen, aoi, 
                -starts_with("all_log"))|>
  mutate(name_lwr = str_remove_all(aoi, '_Temperate') |> 
           str_remove_all("_Arid") |> str_remove_all("_Equatorial") |> 
           str_remove_all('_Polar') |> str_remove_all("_Boreal"))  |>
  left_join(read_csv('data/npix.csv')) |>
  dplyr::mutate(all_total_ba = all_total_ba/n_pix,
                all_n_fires = all_n_fires/n_pix) |>
  dplyr::select(`Peak Season` = all_peak_season,
                `Season Length` = all_season_length,
                `Size` = all_size,
                `Burned Area` = all_total_ba,
                `N Fires` = all_n_fires,
                `Duration` = all_dur,
                `Mean Growth` = all_fsr,
                `Max Growth` = all_mx_grw,
                aoi
  ) |>
  filter(aoi %in% ccwc$aoi)

ccwc <- ccwc |>
  filter(aoi %in% ccdf$aoi) |>
  dplyr::select(-aoi)

ccdf <- ccdf |> dplyr::select(-aoi)

resdf <- data.frame(fire = NA, clim = NA, Corr = NA, p = NA)
counter <- 1
for(i in 1:ncol(ccdf)){
  for(j in 1:ncol(ccwc)){
    c <- cor.test(ccdf[,i] |> pull(), 
                  ccwc[,j]|> pull(),
                  method = "pearson")
    resdf[counter, 1] <- names(ccdf)[i]
    resdf[counter, 2] <- names(ccwc)[j]
    resdf[counter, 3] <- c$estimate
    resdf[counter, 4] <- c$p.value
    counter <- counter +1
  }
}


# resdf <- read_csv('data/resdf_fig2.csv')

cc <- resdf |> 
  filter(p<0.01) |>
  mutate(clim = lut_wc[clim]) |>
  mutate(fire = as.factor(fire) |> 
           fct_relevel("Burned Area", "N Fires", "Season Length", "Peak Season",
                       "Duration", "Size", "Mean Growth", "Max Growth"),
         clim = str_replace_all(clim, "_", " ") |> 
           str_to_title() |>
           str_replace_all("Temperature", "T") |>
           str_replace_all("Precipitation", "PPT") |>
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
  scale_fill_gradient2(low = "blue", mid="white", high = "red", limits = c(-1,1) ) +
  coord_equal() +
  geom_text(aes(label = round(Corr,2)),size =4) +
  geom_vline(xintercept = 4.5, lty = 2) +
  xlab("Area-Based  |  Event-Based") +
  ggtitle("(b) Climate Correlations") +
  theme(axis.title.y = element_blank(),
        legend.position = c(1,1),
        axis.text = element_text(size=11),
        legend.justification = c(1,1),
      axis.text.x = element_text(angle=45, hjust = 1))


# cow<- cowplot::ggdraw(ylim=c(0,15), xlim = c(0,3)) +
#   cowplot::draw_plot(cc,x = 0, y=0, height = 10, width=3) +
#   cowplot::draw_plot(top, x=.025, y=10, height=5, width =3)
# ggsave(plot = cow, filename ="figures/fr_correlations_2pan_new.png", width=6.5, height = 14, bg="white")


cow<- cowplot::ggdraw(ylim=c(0,10), xlim = c(0,10)) +
  cowplot::draw_plot(cc,x = 4, y=0, height = 10, width=6) +
  cowplot::draw_plot(top, x=0, y=6, height=4, width =4)
ggsave(plot = cow, filename ="figures/fr_correlations_2pan_v2.png", width=11, height = 11, bg="white")

