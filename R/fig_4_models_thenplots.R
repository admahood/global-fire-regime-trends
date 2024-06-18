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
bd <- read_csv("data/bd.csv")

ssa <- bd |> filter(REGION_WB == "Sub-Saharan Africa") |> pull(aoi) |> unique()

# functions ==================
tidy_model <- function(model, response) {
  broom.mixed::tidy(model) %>% 
    filter(is.na(group), term!= "(Intercept)") %>%
    separate(term, into = c("variable", "koppen"), sep = ":") %>% 
    mutate(koppen = sub("koppen_mode", "", koppen)) %>% 
    dplyr::select(-group, -effect) %>%
    mutate(response = response)
}

tidy_model_sr <- function(model, response) {
  broom.mixed::tidy(model) %>% 
    filter(is.na(group), term!= "(Intercept)") %>%
    separate(term, into = c("variable", "region_wb"), sep = ":") %>% 
    mutate(region_wb = sub("REGION_WB", "", region_wb)) %>% 
    dplyr::select(-group, -effect) %>%
    mutate(response = response, 
           response = str_remove_all(response, "log"),
           response = str_remove_all(response, "1"),
           response = str_remove_all(response,"\\+"),
           response = str_remove_all(response,"\\("),
           response = str_remove_all(response,"\\)"),
           response = trimws(response))
}
# variables =====
response_variables <- c(colnames(bd)[16:17], 
                        paste0("log(", colnames(bd)[18:23], " + 1)"))
landcover <- paste('(forest + urban_built + mean_vpd_anom + cropland + grassland)')
groups <- "koppen_mode"


funct <- function(rv, lc, gr, data, op = ":"){
  require(lme4)
  require(lmerTest)
  require(datawizard)
  require(stringr)
  rvars <- lc %>%
    str_remove_all("\\+") %>%
    str_remove_all("\\(") %>%
    str_remove_all("\\)") %>%
    str_split(pattern = "  ") %>%
    unlist() 
  
  data <- data %>%
    mutate_at(c(rvars), datawizard::standardize)
  
  f <- formula(paste0(rv,"~",lc,op,gr,"+ (1|aoi) + (1|SUBREGION)"))
  print(f)
  m <- lmerTest::lmer(f, data=data)
  return(m)
}

# look at the different groups
result <- list();counter <- 1;gr <- c();rv <- c()
for(i in response_variables){
  for(g in groups[1]){
    result[[counter]] <- funct(i, landcover, g, bd, op=":")
    gr[counter] <- g
    rv[counter] <- i
    counter <- counter + 1
    
  }
}
tab <- lapply(result, performance::model_performance) %>%
  bind_rows() %>%
  mutate(response = rv, 
         ba_trend = "all") %>%
  mutate_if(is.numeric, round,2)

write.csv(tab,"figures/lmm_metrics.csv")

fixed_effects <- list()
for(i in 1:length(response_variables)){
  fixed_effects[[i]] <- tidy_model(result[[i]],response_variables[i])
}

aov <- lapply(result, car::Anova, test.statistic = "F")

# plot the stuff =====================================
fe_df <- bind_rows(fixed_effects) %>%
  mutate(sign = ifelse(estimate < 0, "negative", "positive"),
         sig = ifelse(p.value < 0.05, "yes", "no"),
         response = str_remove_all(response, " \\+ 1\\)"),
         response = str_remove_all(response, "log\\("),
         response = str_replace_all(response, "dur", "duration"),
         response = factor(response, ordered = TRUE, 
                           levels = c("total_ba", "n_fires", "peak_season",
                                      "season_length", "size", 
                                      "duration", "fsr", "mx_grw"), 
                           labels = c("Total Burned Area", "N Fires", "Peak Season",
                                      "Season Length", "Size", 
                                      "Duration", "Mean Growth", "Max Growth")),
         variable = ifelse(variable == "mean_vpd_anom", variable,
                           paste0(variable, "_cover")))
#Kyle's idea
# bar plot with position dodge?
# error bar plot?
# fe_df |>
#   group_by(response) |>
#   mutate(estimate = scale(estimate))|>
# ggplot(aes(x=response, y=estimate, fill = variable)) +
#   geom_bar(stat = "identity", position = "dodge", color = "black", aes(alpha = sig)) +
#   facet_wrap(~koppen, ncol=1, scales = "free_y") +
#   geom_vline(xintercept = seq(1.5, 7.5, by =1, )) +
#   theme_bw()
# 
# fe_df |>
#   group_by(response) |>
#   mutate(estimate = scale(estimate))|>
#   ggplot(aes(x=response, y=estimate, fill = koppen)) +
#   geom_bar(stat = "identity", position = "dodge", color = "black", aes(alpha = sig)) +
#   facet_wrap(~variable, ncol=1, scales = "free_y") +
#   geom_vline(xintercept = seq(1.5, 7.5, by =1, )) +
#   theme_bw()
# 
# fe_df |>
#   # group_by(koppen) |>
#   # mutate(estimate = scale(estimate))|>
#   ggplot(aes(x=koppen, y=estimate, fill = variable)) +
#   geom_bar(stat = "identity", position = "dodge", color = "black", aes(alpha = sig)) +
#   facet_wrap(~response, ncol=2, scales = "free_y") +
#   geom_vline(xintercept = seq(1.5, 3.5, by =1, )) +
#   ylab("Standardized Estiamte") +
#   xlab("Koppen-Geiger Region") +
#   theme_bw()
# 
# fe_df |>
#   group_by(variable) |>
#   mutate(estimate = scale(estimate, center = F))|>
#   ggplot(aes(x=variable, y=estimate, fill = koppen)) +
#   geom_bar(stat = "identity", position = "dodge",
#            # color = "black", 
#            aes(alpha = sig, color = sig)) +
#   scale_color_manual(values = c("transparent", "black"))+
#   facet_wrap(~response, ncol=2, scales = "free_y") +
#   geom_vline(xintercept = seq(1.5, 4.5, by =1, )) +
#   ylab("Standardized Estiamte") +
#   xlab("Variable") +
#   # scale_fill_manual(values = c("gold","darkgreen", "olivedrab2", "chocolate4")) +
#   scale_fill_brewer(palette = "Set2") +
#   scale_alpha_manual(values = c(.45, 1)) +
#   theme_bw() +
#   ggtitle("Effect of Changes in Land Cover and VPD on Fire Regimes")

# fe_df |>
#   group_by(response) |>
#   mutate(estimate = scale(estimate, center = F),
#          variable = str_replace_all(variable,"_", " ") |> str_to_title() |>
#            str_replace_all("Vpd", "VPD"))|>
#   ggplot(aes(x=response, y=estimate, fill = koppen)) +
#   geom_bar(stat = "identity", position = "dodge",
#            # color = "black", 
#            aes(alpha = sig, color = sig)) +
#   scale_color_manual(values = c("transparent", "black"))+
#   facet_wrap(~variable, ncol=1, scales = "free_y") +
#   geom_vline(xintercept = seq(1.5, 7.5, by =1, )) +
#   ylab("Standardized Estiamte") +
#   xlab("Variable") +
#   scale_fill_manual(values = c("gold", "skyblue","darkgreen", "chocolate4")) +
#   # scale_fill_brewer(palette = "Accent") +
#   scale_alpha_manual(values = c(.45, 1)) +
#   theme_bw() +
#   ggtitle("Effect of Changes in Land Cover and VPD on Fire Regimes")
# ggsave("figures/model_coefficients_kr_special.png", bg="white", height=6.5, width=9)

fe_df |>
  group_by(variable) |>
  mutate(estimate = scale(estimate, center = F)) |>
  ungroup() |>
  mutate(Response = as.factor(response)) |>
  filter(!Response %in% c("Peak Season", "Season Length", "Duration"),
         koppen %in% c("Arid", "Boreal")) |>
  mutate(variable = str_replace_all(variable,"_", " ") |>
           str_to_title() |>
           str_replace_all("Vpd", "VPD") |>
           str_replace_all("Anom", "Anomaly") |>
           str_remove_all("Mean") |>
           str_remove_all("Cover"))|>
  ggplot(aes(x=variable, y=estimate, fill = Response, alpha = sig, color = sig)) +
  geom_col(position = position_dodge()) +
  guides(alpha = "none", color = "none") +
  scale_color_manual(values = c("transparent", "black")) +
  facet_wrap(~koppen, ncol=1, scales = "free_y") +
  geom_vline(xintercept = seq(1.5, 4.5, by =1), linewidth = 0.25) +
  ylab("Standardized Estiamtes") +
  xlab("Annual Changes in Land Cover and VPD") +
  labs(fill = "Fire\nRegime\nComponent") +
  # scale_fill_manual(values = c("gold", "skyblue","darkgreen", "chocolate4")) +
  scale_fill_brewer(palette = "Dark2") +
  scale_alpha_manual(values = c(.25, 1)) +
  theme_bw() +
  ggtitle("Effect of Annual Changes in VPD and Land Cover on Fire Regimes",
          "Statistically significant associations are outlined and opaque") +
  theme(panel.background = element_rect(fill = NA, color = "black"))
ggsave("figures/model_coefficients_kr_fill_aridboreal.png", bg="white", height=4.5, width=7.5)

ggsave("figures/model_coefficients_kr_fill_as_var.png", bg="white", height=7.5, width=9)
# 
# ggplot(fe_df, aes(x=response, 
#                   y=paste0(str_sub(koppen, 1,1) %>% str_to_lower(), ".",
#                                        str_replace_all(variable, "_", " ") %>%
#                                          str_to_title()) %>%
#                     str_remove_all(" Cover") %>% str_remove_all(" Anom") %>%
#                     str_replace_all("Vpd", "VPD"), 
#                   fill=sign, alpha=sig)) +
#   geom_tile(color="grey30") +
#   geom_hline(yintercept = seq(from = 5.5, to = 16.5,by=5)) +
#   scale_fill_manual(values = brewer.pal(2,"Set1")[c(2,1)],
#                     name = "Relationship") +
#   coord_cartesian(expand=0) +
#   guides(alpha = "none") +
#   theme_classic()+
#   theme(panel.grid = element_blank(),
#         panel.border = element_rect(color = "black", fill=NA),
#         legend.position = c(0.875,0.8),
#         legend.justification = c(1,1),
#         legend.background = element_rect(color = "black", fill = "white")) +
#   ylab("Koppen Region, Driver") +
#   annotate(x=c(9,9,9,9), y=c(3,8,13,18), label =c("Arid\n\n", "Boreal\n\n", "Equatorial\n\n", "Temperate\n\n"),geom="text",
#             angle = 90, size=5, color = "black")+
#   # scale_x_discrete(expand = c(0,.25))+
#   xlab(paste("Country-Wide Characteristics",
#              "                           ",
#              "Individual Fire Characteristics"))+
#   geom_vline(xintercept = c(4.5)) +
#   ggtitle("Effect of Changes in Land Cover Change and VPD on Fire Regimes")
# ggsave("figures/model_coefficients.png", bg="white", height=6.5, width=9)

## more in-depth look at all countries with negative BA trends =========================

tabs <- list.files("tables", full.names = TRUE)

aois <- lapply(tabs, function(x) read_csv(x, col_types = c("ccccccccc"))) %>%
  bind_rows() %>%
  dplyr::rename(tba =2) %>%
  filter(str_sub(tba, nchar(tba)-1, nchar(tba)-1) == "-") %>%
  pull(Country)
#looking at which countries we're looking at
bd %>% filter(aoi %in% aois, year == 2003) %>% 
  dplyr::select(koppen_mode, aoi) %>% print(n=50)

# look at the different groups
result <- list()
counter <- 1
gr <- c()
rv <- c()
for(i in response_variables){
  for(g in groups[1]){
    result[[counter]] <- funct(i, landcover, g, bd %>% filter(aoi %in% aois)) # doing it without the koppen
    gr[counter] <- g
    rv[counter] <- i
    counter <- counter + 1
    
  }
}
# lapply(result[4:5], summary)
# lapply(result[4:5], performance::check_model)
# broom.mixed::tidy(result[[4]]) %>% mutate(resp =response_variables[4]) %>% print(n=25)

tab1 <- lapply(result, performance::model_performance) %>%
  bind_rows() %>%
  mutate(response = rv,
         ba_trend = "negative") %>%
  mutate_if(is.numeric, round,2)


fixed_effects <- list()
for(i in 1:length(response_variables)){
  fixed_effects[[i]] <- tidy_model(result[[i]],response_variables[i])
}

# plot the stuff =====================================
fe_df <- bind_rows(fixed_effects) %>%
  mutate(sign = ifelse(estimate < 0, "negative", "positive"),
         sig = ifelse(p.value < 0.05, "yes", "no"),
         response = str_remove_all(response, " \\+ 1\\)"),
         response = str_remove_all(response, "log\\("),
         response = str_replace_all(response, "dur", "duration"),
         response = factor(response, ordered = TRUE, 
                           levels = c("total_ba", "n_fires", "peak_season",
                                      "season_length", "size", 
                                      "duration", "fsr", "mx_grw"), 
                           labels = c("Total Burned Area", "N Fires", "Peak Season",
                                      "Season Length", "Size", 
                                      "Duration", "Spread Rate", "Max Growth")),
         variable = ifelse(variable == "mean_vpd_anom", variable,
                           paste0(variable, "_cover")))
ggplot(fe_df, aes(x=response, 
                  y=paste0(str_sub(koppen, 1,1) %>% str_to_lower(), ".",
                           str_replace_all(variable, "_", " ") %>%
                             str_to_title()) %>%
                    str_remove_all(" Cover") %>% str_remove_all(" Anom") %>%
                    str_replace_all("Vpd", "VPD"), 
                  fill=sign, alpha=sig)) +
  geom_tile(color="grey30") +
  geom_hline(yintercept = seq(from = 5.5, to = 16.5,by=5)) +
  scale_fill_manual(values = brewer.pal(2,"Set1")[c(2,1)],
                    name = "Relationship") +
  coord_cartesian(expand=0) +
  guides(alpha = "none") +
  theme_classic()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill=NA),
        legend.position = c(0.875,0.8),
        legend.justification = c(1,1),
        legend.background = element_rect(color = "black", fill = "white")) +
  ylab("Koppen Region, Driver") +
  annotate(x=c(9,9,9,9), y=c(3,8,13,18), label =c("Arid\n\n", "Boreal\n\n", "Equatorial\n\n", "Temperate\n\n"),geom="text",
           angle = 90, size=5, color = "black")+
  # scale_x_discrete(expand = c(0,.25))+
  xlab(paste("Country-Wide Characteristics",
             "                           ",
             "Individual Fire Characteristics"))+
  geom_vline(xintercept = c(4.5)) +
  ggtitle("Effect of Changes in Land Cover Change and VPD on Fire Regimes",
          "Only countries with negative trends in burned area")
ggsave(filename = "figures/neg_ba_only_lc_trends.png", bg="white", height=6.5, width=9)



## more in-depth look at all countries with neutral BA trends =========================

tabs <- list.files("tables", full.names = TRUE)

aois <- lapply(tabs, function(x) read_csv(x, col_types = c("ccccccccc"))) %>%
  bind_rows() %>%
  dplyr::rename(tba =2) %>%
  filter(str_sub(tba, nchar(tba)-1, nchar(tba)-1) != "-") %>%
  filter(str_sub(tba, nchar(tba)-1, nchar(tba)-1) != "+") %>%
  pull(Country)
#looking at which countries we're looking at
bd %>% filter(aoi %in% aois, year == 2003) %>% 
  dplyr::select(koppen_mode, aoi) %>% print(n=50)

# look at the different groups
result <- list()
counter <- 1
gr <- c()
rv <- c()
for(i in response_variables){
  for(g in groups[1]){
    result[[counter]] <- funct(i, landcover, g, bd %>% filter(aoi %in% aois)) # doing it without the koppen
    gr[counter] <- g
    rv[counter] <- i
    counter <- counter + 1
    
  }
}
# lapply(result[4:5], summary)
# lapply(result[4:5], performance::check_model)
# broom.mixed::tidy(result[[4]]) %>% mutate(resp =response_variables[4]) %>% print(n=25)

tab2 <- lapply(result, performance::model_performance) %>%
  bind_rows() %>%
  mutate(response = rv,
         ba_trend = "neutral") %>%
  mutate_if(is.numeric, round,2)


fixed_effects <- list()
for(i in 1:length(response_variables)){
  fixed_effects[[i]] <- tidy_model(result[[i]],response_variables[i])
}

# plot the stuff =====================================
fe_df <- bind_rows(fixed_effects) %>%
  mutate(sign = ifelse(estimate < 0, "negative", "positive"),
         sig = ifelse(p.value < 0.05, "yes", "no"),
         response = str_remove_all(response, " \\+ 1\\)"),
         response = str_remove_all(response, "log\\("),
         response = str_replace_all(response, "dur", "duration"),
         response = factor(response, ordered = TRUE, 
                           levels = c("total_ba", "n_fires", "peak_season",
                                      "season_length", "size", 
                                      "duration", "fsr", "mx_grw"), 
                           labels = c("Total Burned Area", "N Fires", "Peak Season",
                                      "Season Length", "Size", 
                                      "Duration", "Spread Rate", "Max Growth")),
         variable = ifelse(variable == "mean_vpd_anom", variable,
                           paste0(variable, "_cover")))
ggplot(fe_df, aes(x=response, 
                  y=paste0(str_sub(koppen, 1,1) %>% str_to_lower(), ".",
                           str_replace_all(variable, "_", " ") %>%
                             str_to_title()) %>%
                    str_remove_all(" Cover") %>% str_remove_all(" Anom") %>%
                    str_replace_all("Vpd", "VPD"), 
                  fill=sign, alpha=sig)) +
  geom_tile(color="grey30") +
  geom_hline(yintercept = seq(from = 5.5, to = 16.5,by=5)) +
  scale_fill_manual(values = brewer.pal(2,"Set1")[c(2,1)],
                    name = "Relationship") +
  coord_cartesian(expand=0) +
  guides(alpha = "none") +
  theme_classic()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill=NA),
        legend.position = c(0.875,0.8),
        legend.justification = c(1,1),
        legend.background = element_rect(color = "black", fill = "white")) +
  ylab("Koppen Region, Driver") +
  annotate(x=c(9,9,9,9), y=c(3,8,13,18), label =c("Arid\n\n", "Boreal\n\n", "Equatorial\n\n", "Temperate\n\n"),geom="text",
           angle = 90, size=5, color = "black")+
  # scale_x_discrete(expand = c(0,.25))+
  xlab(paste("Country-Wide Characteristics",
             "                           ",
             "Individual Fire Characteristics"))+
  geom_vline(xintercept = c(4.5)) +
  ggtitle("Effect of Changes in Land Cover Change and VPD on Fire Regimes",
          "Only countries with neutral trends in burned area")
ggsave(filename = "figures/neutral_ba_only_lc_trends.png", bg="white", height=6.5, width=9)

## more in-depth look at all countries with neutral BA trends =========================

tabs <- list.files("tables", full.names = TRUE)

aois <- lapply(tabs, function(x) read_csv(x, col_types = c("ccccccccc"))) %>%
  bind_rows() %>%
  dplyr::rename(tba =2) %>%
  filter(str_sub(tba, nchar(tba)-1, nchar(tba)-1) == "+") %>%
  pull(Country)
#looking at which countries we're looking at
bd %>% filter(aoi %in% aois, year == 2003) %>% 
  dplyr::select(koppen_mode, aoi) %>% print(n=50)

# look at the different groups
result <- list()
counter <- 1
gr <- c()
rv <- c()
for(i in response_variables){
  for(g in groups[1]){
    result[[counter]] <- funct(i, landcover, g, bd %>% filter(aoi %in% aois)) # doing it without the koppen
    gr[counter] <- g
    rv[counter] <- i
    counter <- counter + 1
    
  }
}
# lapply(result[4:5], summary)
# lapply(result[4:5], performance::check_model)
# broom.mixed::tidy(result[[4]]) %>% mutate(resp =response_variables[4]) %>% print(n=25)

tab3 <- lapply(result, performance::model_performance) %>%
  bind_rows() %>%
  mutate(response = rv,
         ba_trend = "positive") %>%
  mutate_if(is.numeric, round,2)


fixed_effects <- list()
for(i in 1:length(response_variables)){
  fixed_effects[[i]] <- tidy_model(result[[i]],response_variables[i])
}

# plot the stuff =====================================
fe_df <- bind_rows(fixed_effects) %>%
  mutate(sign = ifelse(estimate < 0, "negative", "positive"),
         sig = ifelse(p.value < 0.05, "yes", "no"),
         response = str_remove_all(response, " \\+ 1\\)"),
         response = str_remove_all(response, "log\\("),
         response = str_replace_all(response, "dur", "duration"),
         response = factor(response, ordered = TRUE, 
                           levels = c("total_ba", "n_fires", "peak_season",
                                      "season_length", "size", 
                                      "duration", "fsr", "mx_grw"), 
                           labels = c("Total Burned Area", "N Fires", "Peak Season",
                                      "Season Length", "Size", 
                                      "Duration", "Spread Rate", "Max Growth")),
         variable = ifelse(variable == "mean_vpd_anom", variable,
                           paste0(variable, "_cover")))
ggplot(fe_df, aes(x=response, 
                  y=paste0(str_sub(koppen, 1,1) %>% str_to_lower(), ".",
                           str_replace_all(variable, "_", " ") %>%
                             str_to_title()) %>%
                    str_remove_all(" Cover") %>% str_remove_all(" Anom") %>%
                    str_replace_all("Vpd", "VPD"), 
                  fill=sign, alpha=sig)) +
  geom_tile(color="grey30") +
  geom_hline(yintercept = seq(from = 5.5, to = 16.5,by=5)) +
  scale_fill_manual(values = brewer.pal(2,"Set1")[c(2,1)],
                    name = "Relationship") +
  coord_cartesian(expand=0) +
  guides(alpha = "none") +
  theme_classic()+
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill=NA),
        legend.position = c(0.875,0.8),
        legend.justification = c(1,1),
        legend.background = element_rect(color = "black", fill = "white")) +
  ylab("Koppen Region, Driver") +
  annotate(x=c(9,9,9,9), y=c(3,8,13,18), label =c("Arid\n\n", "Boreal\n\n", "Equatorial\n\n", "Temperate\n\n"),geom="text",
           angle = 90, size=5, color = "black")+
  # scale_x_discrete(expand = c(0,.25))+
  xlab(paste("Country-Wide Characteristics",
             "                           ",
             "Individual Fire Characteristics"))+
  geom_vline(xintercept = c(4.5)) +
  ggtitle("Effect of Changes in Land Cover Change and VPD on Fire Regimes",
          "Only countries with positive trends in burned area")
ggsave(filename = "figures/pos_ba_only_lc_trends.png", bg="white", height=6.5, width=9)

# table with models with the ba splits

bind_rows(tab1, tab2, tab3) %>%
  write_csv("figures/lmm_metrics_ba_splits.csv")

# TABLE WITH athe number of countries for each trend and characteristic =======================

tabs <- list.files("tables", full.names = TRUE)

trends_table <- lapply(tabs, function(x) read_csv(x, col_types = c("ccccccccc"))) %>%
  bind_rows() %>%
  pivot_longer(-Country) %>%
  mutate(value = str_remove_all(value, "\\d"),
         value = str_remove_all(value, ","),
         value = str_remove_all(value, "\\."),
         value = trimws(value)) %>%
  group_by(name, value) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  mutate(value = case_match(value, 
                            "(+)" ~ "positive", 
                            "(-)" ~ "negative", 
                            "" ~ "neutral")) %>%
  pivot_wider(id_cols = name, names_from = value, values_from = n) %>%
  dplyr::select(name, positive, negative, neutral) %>%
  slice(c(8,4,6,5,7,1,2,3)); trends_table

write_csv(trends_table, "tables/temporal_trends.csv")


## more in-depth look at sub saharan africa =========================

regions <- list()
count = 1
for(i in unique(bd$REGION_WB)){
regions[[count]] <- bd |> filter(REGION_WB == i) |> pull(aoi) |> unique() |> sort()
names(regions)[count] <- i

count = count+1
}
ssa <- bd |> filter(REGION_WB == "Sub-Saharan Africa") |> pull(aoi) |> unique()

tabs <- list.files("tables", full.names = TRUE)

fixed <- c("~ (forest + urban_built + mean_vpd_anom + cropland + grassland) + koppen_mode",
           "~ (forest + urban_built + cropland + grassland) + mean_vpd_anom*koppen_mode",
           "~ forest + grassland + mean_vpd_anom + (mean_vpd_anom + cropland)*koppen_mode",
           "~ urban_built + (forest + mean_vpd_anom + cropland + grassland)*koppen_mode",
           "~ urban_built + (forest + mean_vpd_anom + cropland + grassland)*koppen_mode",
           "~ forest + grassland + (mean_vpd_anom + cropland)*koppen_mode",
           "~ (forest + urban_built + mean_vpd_anom + cropland + grassland)+koppen_mode",
           "~ forest + urban_built + grassland + (mean_vpd_anom + cropland)*koppen_mode")
  

# look at the different groups
result <- list();counter <- 1;gr <- c();rv <- c()
# for(region in regions){
for(i in response_variables){
  f <- formula(paste0(i,fixed[counter], "+ (1|aoi) + (1|REGION_WB) "))
  
    result[[counter]] <- lmerTest::lmer(f, data=bd, REML = T)#%>% filter(aoi %in% ssa) ) # change to F for model selection
    rv[counter] <- i
    counter <- counter + 1
}
# }
# lapply(result[4:5], summary)
# lapply(result[4:5], performance::check_model)
# broom.mixed::tidy(result[[4]]) %>% mutate(resp =response_variables[4]) %>% print(n=25)

tab2 <- lapply(result, performance::model_performance) %>%
  bind_rows() %>%
  mutate(response = rv,
         ba_trend = "ssa") %>%
  mutate_if(is.numeric, round,2);tab2


lapply(result[c(3,6,4,5)], tidy)
lapply(result[c(3,6,4,5)], summary)
aov<-lapply(result[c(3,6,4,5)], car::Anova, test.statistic = "F") # KR F-tests are time consuming

fixed_effects <- list()
for(i in 1:length(response_variables)){
  fixed_effects[[i]] <- tidy_model(result[[i]],response_variables[i])
}



# plot the stuff =====================================
fe_df <- bind_rows(fixed_effects) %>%
  mutate(sign = ifelse(estimate < 0, "negative", "positive"),
         sig = ifelse(p.value < 0.05, "yes", "no"),
         response = str_remove_all(response, " \\+ 1\\)"),
         response = str_remove_all(response, "log\\("),
         response = str_replace_all(response, "dur", "duration"),
         response = factor(response, ordered = TRUE, 
                           levels = c("total_ba", "n_fires", "peak_season",
                                      "season_length", "size", 
                                      "duration", "fsr", "mx_grw"), 
                           labels = c("Total Burned Area", "N Fires", "Peak Season",
                                      "Season Length", "Size", 
                                      "Duration", "Spread Rate", "Max Growth")),
         variable = ifelse(variable == "mean_vpd_anom", variable,
                           paste0(variable, "_cover")))
fe_df |>
  group_by(variable) |>
  mutate(estimate = scale(estimate)) |>
  ungroup() |>
  mutate(Response = as.factor(response)) |>
  mutate(variable = str_replace_all(variable,"_", " ") |>
           str_to_title() |>
           str_replace_all("Vpd", "VPD"))|>
  ggplot(aes(x=variable, y=estimate, fill = Response, alpha = sig, color = sig)) +
  geom_col(position = position_dodge()) +
  guides(alpha = "none", color = "none") +
  scale_color_manual(values = c("transparent", "black")) +
  ylab("Standardized Estiamte") +
  xlab("Fire Regime Component") +
  facet_wrap(~koppen_mode) +
  # scale_fill_manual(values = c("gold", "skyblue","darkgreen", "chocolate4")) +
  scale_fill_brewer(palette = "Dark2") +
  scale_alpha_manual(values = c(.25, 1)) +
  theme_bw() +
  labs(fill = "Fire\nRegime\nComponent")+
  ggtitle("Effect of Annual Changes in Land Cover and VPD on Fire Regimes in Sub-Saharan Africa",
          "Statistically significant associations are outlined and opaque") +
  theme(panel.background = element_rect(fill = NA, color = "black"))

ggsave("figures/model_coefficients_kr_fill_ssa.png", bg="white", height=4, width=8)


# sub saharan africa =============

ssa <- bd |> filter(REGION_WB == "Sub-Saharan Africa") |> pull(aoi) |> unique()

fixed <- c("~ (forest + urban_built + mean_vpd_anom + cropland + grassland)",
           "~ (forest + urban_built + mean_vpd_anom + cropland + grassland) ",
           "~ (forest + urban_built + mean_vpd_anom + cropland + grassland)",
           "~ (forest + urban_built + mean_vpd_anom + cropland)  + grassland",
           "~ (forest + urban_built + mean_vpd_anom + cropland ) + grassland",
           "~ (forest + urban_built + mean_vpd_anom + cropland + grassland)*koppen_mode",
           "~ (forest + urban_built + mean_vpd_anom + cropland + grassland)",
           "~ (forest + urban_built + mean_vpd_anom + cropland + grassland)*koppen_mode")


# look at the different groups
result <- list();counter <- 1;gr <- c();rv <- c()
# for(region in regions){
for(i in response_variables){
  f <- formula(paste0(i,"~ (forest + urban_built + mean_vpd_anom + cropland + grassland)", "+ (1|aoi)"))
  
  result[[counter]] <- lmerTest::lmer(f, data=bd %>% filter(aoi %in% ssa), REML = T)#) # change to F for model selection
  rv[counter] <- i
  counter <- counter + 1
}
# }
# lapply(result[4:5], summary)
# lapply(result[4:5], performance::check_model)
# broom.mixed::tidy(result[[4]]) %>% mutate(resp =response_variables[4]) %>% print(n=25)

tab2 <- lapply(result, performance::model_performance) %>%
  bind_rows() %>%
  mutate(response = rv,
         ba_trend = "ssa") %>%
  mutate_if(is.numeric, round,2);tab2


lapply(result[c(3,6,8,4,5,2)], tidy)
lapply(result[c(3,6,8,4,5,2)], summary)
aov<-lapply(result[c(3,6,8,4,5,2)], car::Anova)#, test.statistic = "F");aov # KR F-tests are time consuming

fixed_effects <- list()
for(i in 1:length(response_variables)){
  fixed_effects[[i]] <- tidy_model(result[[i]],response_variables[i])
}

fe_df <- bind_rows(fixed_effects) %>%
  mutate(sign = ifelse(estimate < 0, "negative", "positive"),
         sig = ifelse(p.value < 0.05, "yes", "no"),
         response = str_remove_all(response, " \\+ 1\\)"),
         response = str_remove_all(response, "log\\("),
         response = str_replace_all(response, "dur", "duration"),
         response = factor(response, ordered = TRUE, 
                           levels = c("total_ba", "n_fires", "peak_season",
                                      "season_length", "size", 
                                      "duration", "fsr", "mx_grw"), 
                           labels = c("Total Burned Area", "N Fires", "Peak Season",
                                      "Season Length", "Size", 
                                      "Duration", "Spread Rate", "Max Growth"))) |>
  mutate(variable = ifelse(!is.na(koppen), paste(variable, "x", koppen), variable)) |>
  dplyr::select(-koppen)

fe_df |>
  # group_by(variable) |>
  # mutate(estimate = scale(estimate, center=F)) |>
  # ungroup() |>
  mutate(Response = as.factor(response)) |>
  mutate(variable = str_replace_all(variable,"_", " ") |>
           str_to_title() |>
           str_replace_all("Vpd", "VPD"))|>
  filter(response %in% c("Size", "Max Growth", "N Fires", "Total Burned Area"),
         !variable %in% c("Grassland X Equatorial", "Grassland X Temperate", "Cropland")) |>
  ggplot(aes(y=variable, x=estimate, fill = Response, alpha = sig, color = sig)) +
  geom_col(position = position_dodge()) +
  guides(alpha = "none", color = "none") +
  scale_color_manual(values = c("transparent", "black")) +
  ylab("Standardized Estiamte") +
  xlab("Fire Regime Component") +
  # scale_fill_manual(values = c("gold", "skyblue","darkgreen", "chocolate4")) +
  scale_fill_brewer(palette = "Dark2") +
  scale_alpha_manual(values = c(.25, 1)) +
  theme_bw() +
  labs(fill = "Fire\nRegime\nComponent")+
  ggtitle("Effect of Annual Changes in Land Cover and VPD on Fire Regimes in Sub-Saharan Africa",
          "Statistically significant associations are outlined and opaque") +
  theme(panel.background = element_rect(fill = NA, color = "black"))


# boreal =============

ssa <- bd |> filter(koppen_mode == "Boreal") |> pull(aoi) |> unique()

fixed <- c("~ (forest + urban_built + mean_vpd_anom + cropland + grassland)",
           "~ (forest + urban_built + mean_vpd_anom + cropland + grassland)",
           "~ (forest +  mean_vpd_anom + cropland + grassland)",
           "~ (forest +  mean_vpd_anom + cropland)  + grassland",
           "~ (forest +  mean_vpd_anom + cropland ) + grassland",
           "~ (forest +  mean_vpd_anom + cropland + grassland)",
           "~ (forest + urban_built + mean_vpd_anom + cropland + grassland)",
           "~ (forest + urban_built + mean_vpd_anom + cropland + grassland)")


# look at the different groups
result <- list();counter <- 1;gr <- c();rv <- c()
# for(region in regions){
for(i in response_variables){
  f <- formula(paste0(i,fixed[counter], "+ (1|aoi) "))
  
  result[[counter]] <- lmerTest::lmer(f, data=bd %>% filter(aoi %in% ssa), REML = T)#) # change to F for model selection
  rv[counter] <- i
  counter <- counter + 1
}
# }
# lapply(result[4:5], summary)
# lapply(result[4:5], performance::check_model)
# broom.mixed::tidy(result[[4]]) %>% mutate(resp =response_variables[4]) %>% print(n=25)

tab2 <- lapply(result, performance::model_performance) %>%
  bind_rows() %>%
  mutate(response = rv,
         ba_trend = "ssa") %>%
  mutate_if(is.numeric, round,2);tab2


lapply(result[c(3,6,8,4,5,2)], tidy) |>
  bind_rows()
lapply(result[c(3,6,8,4,5,2)], summary)
aov<-lapply(result[c(3,6,8,4,5,2)], car::Anova)#, test.statistic = "F");aov # KR F-tests are time consuming

fixed_effects <- list()
for(i in 1:length(response_variables)){
  fixed_effects[[i]] <- tidy_model(result[[i]],response_variables[i])
}

fe_df <- bind_rows(fixed_effects) %>%
  mutate(sign = ifelse(estimate < 0, "negative", "positive"),
         sig = ifelse(p.value < 0.05, "yes", "no"),
         response = str_remove_all(response, " \\+ 1\\)"),
         response = str_remove_all(response, "log\\("),
         response = str_replace_all(response, "dur", "duration"),
         response = factor(response, ordered = TRUE, 
                           levels = c("total_ba", "n_fires", "peak_season",
                                      "season_length", "size", 
                                      "duration", "fsr", "mx_grw"), 
                           labels = c("Total Burned Area", "N Fires", "Peak Season",
                                      "Season Length", "Size", 
                                      "Duration", "Spread Rate", "Max Growth"))) |>
  mutate(variable = ifelse(!is.na(koppen), paste(variable, "x", koppen), variable)) |>
  dplyr::select(-koppen)
library(ggbreak) 
library(ggthemes)

fe_df |>
  # group_by(variable) |>
  # mutate(estimate = scale(estimate, center=F)) |>
  # ungroup() |>
  mutate(Response = as.factor(response)) |>
  mutate(variable = ifelse(variable == "mean_vpd_anom", "Mean VPD Anomaly", variable)) |>
  mutate(variable = str_replace_all(variable,"_", " ") |>
           str_to_title() |>
           str_replace_all("Vpd", "VPD"))|>
  filter(response %in% c("Size", "Max Growth", "N Fires", "Total Burned Area")) |>
  ggplot(aes(y=variable, x=estimate, fill = Response, alpha = sig, color = sig)) +
  geom_col(position = position_dodge()) +
  guides(alpha = "none", color = "none") +
  scale_color_manual(values = c("transparent", "black")) +
  xlab("Standardized Estiamtes") +
  ylab("Fire Regime Component") +
  # facet_wrap(~is_vpd, scales = "free", ) +
  # scale_fill_manual(values = c("gold", "skyblue","darkgreen", "chocolate4")) +
  scale_fill_brewer(palette = "Dark2") +
  scale_alpha_manual(values = c(.25, 1)) +
  scale_x_continuous(labels = c(0, 0.00001, 0.005, 0.010, 0.015, 0.020), 
                     breaks = c(0, 0.00001, 0.005, 0.010, 0.015, 0.020))+
  scale_x_break(breaks = c(0.000020, 0.0015), scales = 3) +
  theme_bw() +
  labs(fill = "Fire\nRegime\nComponent")+
  ggtitle("Effect of Annual Changes in Land Cover and VPD on Fire Regimes in Boreal Countries",
          "Statistically significant associations are outlined and opaque") +
  theme(panel.background = element_rect(fill = NA, color = "black"),
        legend.position = "right")
ggsave("figures/model_coefficients_kr_fill_boreal.png", bg="white", height=5.5, width=7.5)


# temperate =============

ssa <- bd |> filter(koppen_mode == "Temperate") |> pull(aoi) |> unique()

fixed <- c("~ (forest + urban_built + mean_vpd_anom + cropland + grassland)",
           "~ (forest + urban_built + mean_vpd_anom + cropland + grassland)",
           "~ (forest +  urban_built + mean_vpd_anom + cropland + grassland)",
           "~ (forest + urban_built +  mean_vpd_anom + cropland)  + grassland",
           "~ (forest +  urban_built + mean_vpd_anom + cropland ) + grassland",
           "~ (forest +  urban_built + mean_vpd_anom + cropland + grassland)",
           "~ (forest + urban_built + mean_vpd_anom + cropland + grassland)",
           "~ (forest + urban_built + mean_vpd_anom + cropland + grassland)")


# look at the different groups
result <- list();counter <- 1;gr <- c();rv <- c()
# for(region in regions){
for(i in response_variables){
  f <- formula(paste0(i,fixed[counter], "+ (1|aoi) "))
  
  result[[counter]] <- lmerTest::lmer(f, data=bd %>% filter(aoi %in% ssa), REML = T)#) # change to F for model selection
  rv[counter] <- i
  counter <- counter + 1
}
# }
# lapply(result[4:5], summary)
# lapply(result[4:5], performance::check_model)
# broom.mixed::tidy(result[[4]]) %>% mutate(resp =response_variables[4]) %>% print(n=25)

tab2 <- lapply(result, performance::model_performance) %>%
  bind_rows() %>%
  mutate(response = rv,
         ba_trend = "ssa") %>%
  mutate_if(is.numeric, round,2);tab2


lapply(result[c(3,6,8,4,5,2)], tidy) |>
  bind_rows()
lapply(result[c(3,6,8,4,5,2)], summary)
aov<-lapply(result[c(3,6,8,4,5,2)], car::Anova, test.statistic = "F");aov # KR F-tests are time consuming

fixed_effects <- list()
for(i in 1:length(response_variables)){
  fixed_effects[[i]] <- tidy_model(result[[i]],response_variables[i])
}

fe_df <- bind_rows(fixed_effects) %>%
  mutate(sign = ifelse(estimate < 0, "negative", "positive"),
         sig = ifelse(p.value < 0.05, "yes", "no"),
         response = str_remove_all(response, " \\+ 1\\)"),
         response = str_remove_all(response, "log\\("),
         response = str_replace_all(response, "dur", "duration"),
         response = factor(response, ordered = TRUE, 
                           levels = c("total_ba", "n_fires", "peak_season",
                                      "season_length", "size", 
                                      "duration", "fsr", "mx_grw"), 
                           labels = c("Total Burned Area", "N Fires", "Peak Season",
                                      "Season Length", "Size", 
                                      "Duration", "Spread Rate", "Max Growth"))) |>
  mutate(variable = ifelse(!is.na(koppen), paste(variable, "x", koppen), variable)) |>
  dplyr::select(-koppen)
library(ggbreak) 
library(ggthemes)

fe_df |>
  # group_by(variable) |>
  # mutate(estimate = scale(estimate, center=F)) |>
  # ungroup() |>
  mutate(Response = as.factor(response)) |>
  mutate(variable = ifelse(variable == "mean_vpd_anom", "Mean VPD Anomaly", variable)) |>
  mutate(variable = str_replace_all(variable,"_", " ") |>
           str_to_title() |>
           str_replace_all("Vpd", "VPD"))|>
  filter(response %in% c("Size", "Max Growth", "Spread Rate", "N Fires", "Total Burned Area")) |>
  ggplot(aes(y=variable, x=estimate, fill = Response, alpha = sig, color = sig)) +
  geom_col(position = position_dodge()) +
  guides(alpha = "none", color = "none") +
  scale_color_manual(values = c("transparent", "black")) +
  xlab("Standardized Estiamtes") +
  ylab("Fire Regime Component") +
  # facet_wrap(~is_vpd, scales = "free", ) +
  # scale_fill_manual(values = c("gold", "skyblue","darkgreen", "chocolate4")) +
  scale_fill_brewer(palette = "Dark2") +
  scale_alpha_manual(values = c(.25, 1)) +
  # scale_x_continuous(labels = c(0, 0.00001, 0.005, 0.010, 0.015, 0.020),
  #                    breaks = c(0, 0.00001, 0.005, 0.010, 0.015, 0.020))+
  scale_x_break(breaks = c(0.0025, 0.005), scales = 1) +
  theme_bw() +
  labs(fill = "Fire\nRegime\nComponent")+
  ggtitle("Effect of Annual Changes in Land Cover and VPD on Fire Regimes in Boreal Countries",
          "Statistically significant associations are outlined and opaque") +
  theme(panel.background = element_rect(fill = NA, color = "black"),
        legend.position = "right")
ggsave("figures/model_coefficients_kr_fill_temperate.png", bg="white", height=5.5, width=7.5)

# Arid =============

ssa <- bd |> filter(koppen_mode == "Arid") |> pull(aoi) |> unique()

fixed <- c("~ (forest + urban_built + max_vpd_anom + cropland + grassland)")


# look at the different groups
result <- list();counter <- 1;gr <- c();rv <- c()
# for(region in regions){
for(i in response_variables){
  f <- formula(paste0(i,fixed, "+ (1|aoi) + (1|SUBREGION) "))
  
  result[[counter]] <- lmerTest::lmer(f, data=bd %>% filter(aoi %in% ssa), REML = T)#) # change to F for model selection
  rv[counter] <- i
  counter <- counter + 1
}
# }
# lapply(result[4:5], summary)
# lapply(result[4:5], performance::check_model)
# broom.mixed::tidy(result[[4]]) %>% mutate(resp =response_variables[4]) %>% print(n=25)

tab2 <- lapply(result, performance::model_performance) %>%
  bind_rows() %>%
  mutate(response = rv,
         ba_trend = "ssa") %>%
  mutate_if(is.numeric, round,2);tab2


lapply(result[c(3,6,8,4,5,2)], tidy) |>
  bind_rows()
lapply(result[c(3,6,8,4,5,2)], summary)
aov<-lapply(result[c(3,6,8,4,5,2)], car::Anova, test.statistic = "F");aov # KR F-tests are time consuming

fixed_effects <- list()
for(i in 1:length(response_variables)){
  fixed_effects[[i]] <- tidy_model(result[[i]],response_variables[i])
}

fe_df <- bind_rows(fixed_effects) %>%
  mutate(sign = ifelse(estimate < 0, "negative", "positive"),
         sig = ifelse(p.value < 0.05, "yes", "no"),
         response = str_remove_all(response, " \\+ 1\\)"),
         response = str_remove_all(response, "log\\("),
         response = str_replace_all(response, "dur", "duration"),
         response = factor(response, ordered = TRUE, 
                           levels = c("total_ba", "n_fires", "peak_season",
                                      "season_length", "size", 
                                      "duration", "fsr", "mx_grw"), 
                           labels = c("Total Burned Area", "N Fires", "Peak Season",
                                      "Season Length", "Size", 
                                      "Duration", "Spread Rate", "Max Growth"))) |>
  mutate(variable = ifelse(!is.na(koppen), paste(variable, "x", koppen), variable)) |>
  dplyr::select(-koppen)
library(ggbreak) 
library(ggthemes)

fe_df |>
  # group_by(variable) |>
  # mutate(estimate = scale(estimate, center=F)) |>
  # ungroup() |>
  mutate(Response = as.factor(response)) |>
  mutate(variable = ifelse(variable == "mean_vpd_anom", "Mean VPD Anomaly", variable)) |>
  mutate(variable = str_replace_all(variable,"_", " ") |>
           str_to_title() |>
           str_replace_all("Vpd", "VPD"))|>
  filter(response %in% c("Size", "Max Growth", "Spread Rate", "N Fires", "Total Burned Area")) |>
  ggplot(aes(y=variable, x=estimate, fill = Response, alpha = sig, color = sig)) +
  geom_col(position = position_dodge()) +
  guides(alpha = "none", color = "none") +
  scale_color_manual(values = c("transparent", "black")) +
  xlab("Standardized Estiamtes") +
  ylab("Fire Regime Component") +
  # facet_wrap(~is_vpd, scales = "free", ) +
  # scale_fill_manual(values = c("gold", "skyblue","darkgreen", "chocolate4")) +
  scale_fill_brewer(palette = "Dark2") +
  scale_alpha_manual(values = c(.25, 1)) +
  # scale_x_continuous(labels = c(0, 0.00001, 0.005, 0.010, 0.015, 0.020),
  #                    breaks = c(0, 0.00001, 0.005, 0.010, 0.015, 0.020))+
  scale_x_break(breaks = c(-0.0005, -0.000045), scales = 1) +
  theme_bw() +
  labs(fill = "Fire\nRegime\nComponent")+
  ggtitle("Effect of Annual Changes in Land Cover and VPD on Fire Regimes in Boreal Countries",
          "Statistically significant associations are outlined and opaque") +
  theme(panel.background = element_rect(fill = NA, color = "black"),
        legend.position = "right")
ggsave("figures/model_coefficients_kr_fill_arid.png", bg="white", height=5.5, width=7.5)

# Equatorial =============

ssa <- bd |> filter(koppen_mode == "Equatorial") |> pull(aoi) |> unique()

fixed <- c("~ (forest + urban_built + mean_vpd_anom + cropland + grassland)")


# look at the different groups
result <- list();counter <- 1;gr <- c();rv <- c()
# for(region in regions){
for(i in response_variables){
  f <- formula(paste0(i,fixed, "+ (1|aoi) + (1|SUBREGION) "))
  
  result[[counter]] <- lmerTest::lmer(f, data=bd %>% filter(aoi %in% ssa), REML = T)#) # change to F for model selection
  rv[counter] <- i
  counter <- counter + 1
}
# }
# lapply(result[4:5], summary)
# lapply(result[4:5], performance::check_model)
# broom.mixed::tidy(result[[4]]) %>% mutate(resp =response_variables[4]) %>% print(n=25)

tab2 <- lapply(result, performance::model_performance) %>%
  bind_rows() %>%
  mutate(response = rv,
         ba_trend = "ssa") %>%
  mutate_if(is.numeric, round,2);tab2


lapply(result[c(3,6,8,4,5,2)], tidy) |>
  bind_rows()
lapply(result[c(3,6,8,4,5,2)], summary)
aov<-lapply(result[c(3,6,8,4,5,2)], car::Anova, test.statistic = "F");aov # KR F-tests are time consuming

fixed_effects <- list()
for(i in 1:length(response_variables)){
  fixed_effects[[i]] <- tidy_model(result[[i]],response_variables[i])
}

fe_df <- bind_rows(fixed_effects) %>%
  mutate(sign = ifelse(estimate < 0, "negative", "positive"),
         sig = ifelse(p.value < 0.05, "yes", "no"),
         response = str_remove_all(response, " \\+ 1\\)"),
         response = str_remove_all(response, "log\\("),
         response = str_replace_all(response, "dur", "duration"),
         response = factor(response, ordered = TRUE, 
                           levels = c("total_ba", "n_fires", "peak_season",
                                      "season_length", "size", 
                                      "duration", "fsr", "mx_grw"), 
                           labels = c("Total Burned Area", "N Fires", "Peak Season",
                                      "Season Length", "Size", 
                                      "Duration", "Spread Rate", "Max Growth"))) |>
  mutate(variable = ifelse(!is.na(koppen), paste(variable, "x", koppen), variable)) |>
  dplyr::select(-koppen)
library(ggbreak) 
library(ggthemes)

fe_df |>
  mutate(Response = as.factor(response)) |>
  mutate(variable = ifelse(variable == "mean_vpd_anom", "Mean VPD Anomaly", variable)) |>
  mutate(variable = str_replace_all(variable,"_", " ") |>
           str_to_title() |>
           str_replace_all("Vpd", "VPD"))|>
  filter(response %in% c("Size", "Max Growth", "Spread Rate", "N Fires", "Total Burned Area")) |>
  ggplot(aes(y=variable, x=estimate, fill = Response, alpha = sig, color = sig)) +
  geom_col(position = position_dodge()) +
  guides(alpha = "none", color = "none") +
  scale_color_manual(values = c("transparent", "black")) +
  xlab("Standardized Estiamtes") +
  ylab("Fire Regime Component") +
  # facet_wrap(~is_vpd, scales = "free", ) +
  # scale_fill_manual(values = c("gold", "skyblue","darkgreen", "chocolate4")) +
  scale_fill_brewer(palette = "Dark2") +
  scale_alpha_manual(values = c(.25, 1)) +
  # scale_x_continuous(labels = c(0, 0.00001, 0.005, 0.010, 0.015, 0.020),
  #                    breaks = c(0, 0.00001, 0.005, 0.010, 0.015, 0.020))+
  scale_x_break(breaks = c(0.0001, 0.0001), scales = .5) +
  theme_bw() +
  labs(fill = "Fire\nRegime\nComponent")+
  ggtitle("Effect of Annual Changes in Land Cover and VPD on Fire Regimes in Boreal Countries",
          "Statistically significant associations are outlined and opaque") +
  theme(panel.background = element_rect(fill = NA, color = "black"),
        legend.position = "right")
ggsave("figures/model_coefficients_kr_fill_equatorial.png", bg="white", height=5.5, width=7.5)
