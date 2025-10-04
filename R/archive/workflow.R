# setup ========================================================================
library(tidyverse)
library(sf)
library(Hmisc) # for weighted variance
library(units)
library(mblm) 
library(glue)
library(doParallel)
library(foreach)
library(parameters)
library(coin)
library(rstatix)
library(terra)
library(HDInterval)

dir.create("data/temp")
out_path <- "data/out_csvs_08mar22"
out_ts_path <- "data/out_ts_csvs_08mar22"
dir.create(out_path)
dir.create(out_ts_path)
options(timeout = max(2000, getOption("timeout")))

lut_kop <- c("Af",  "Am", "Aw",   
             "BWh" ,"BWk","BSh" ,"BSk" ,
             "Csa", "Csb" ,"Csc" ,"Cwa", "Cwb", "Cwc","Cfa" , "Cfb", "Cfc" ,
             "Dsa" ,"Dsb" ,"Dsc" ,"Dsd","Dwa" ,"Dwb" ,"Dwc" ,"Dwd" ,
             "Dfa", "Dfb","Dfc", "Dfd" , 
             "ET",   "EF")

lut_kop <- c(rep("Equatorial",3), rep("Arid",4), rep("Temperate",9), rep("Boreal",12), rep("Polar",2))
names(lut_kop)<-c(1:30)

# output full time series, so we can do an nmds on each year

# data ingest ==================================================================

# add in aridity index, MAT, MAP

world_boundaries<- st_read("data/world_boundaries/") %>%
  mutate(name_lwr = NAME_EN %>% str_to_lower() %>% str_replace_all(" ", "_"))%>%
  mutate(name_lwr = ifelse(name_lwr == "the_gambia", "gambia", name_lwr),
         name_lwr = ifelse(name_lwr == "people's_republic_of_china", "china",name_lwr),
         name_lwr = ifelse(name_lwr == "united_states_of_america", "conus_ak", name_lwr))

aus_boundaries <- st_read("data/aus_boundaries/") %>%
  st_simplify(dTolerance = 100, preserveTopology = TRUE)%>%
  mutate(name_lwr = STE_NAME21 %>% str_to_lower() %>% str_replace_all(" ", "_"),
         name_lwr = paste0("australia_", name_lwr)) %>%
  slice(1:8)

australia_states <-  read_csv("data/firedpy_master_spreadsheet - Sheet1.csv") %>%
  filter(region == "a")

individual_countries <- read_csv("data/firedpy_master_spreadsheet - Sheet1.csv") %>%
  filter(country_name != "hawaii") %>%
  filter(region == "i")

regions <- read_csv("data/firedpy_master_spreadsheet - Sheet1.csv") %>%
  filter(region == "r") %>%
  filter(region != "US_canada")

cna <- read_csv("data/firedpy_master_spreadsheet - Sheet1.csv") %>%
  filter(region == "cna")

country_names <- individual_countries %>% 
  filter(country_name != "hawaii") %>%
  pull(country_name)

# need to remove either us_canada or the individual countries 
region_names <- regions %>% 
  pull(country_name)

cna_names <- cna %>% pull(country_name)
aus_names <- australia_states %>% pull(country_name)


# ancillary data ===============================================================
# need global dataset of countries with pop, MAT/MAP etc
if(!file.exists("data/too_big/ai_et0/ai_et0.tif")){
  dir.create("data/too_big")
  download.file(url = "https://figshare.com/ndownloader/files/14118800",
                destfile = "data/too_big/aridity.zip")
  unzip(zipfile = "data/too_big/aridity.zip", exdir = "data/too_big")
}

# worldclim bioclimactic variables
# info at https://worldclim.org/data/bioclim.html

wc_names<-c("Annual Mean Temperature",
            "Mean Diurnal Range",
            "Isothermality",
            "Temperature Seasonality",
            "Max Temperature of Warmest Month",
            "Min Temperature of Coldest Month",
            "Temperature Annual Range",
            "Mean Temperature of Wettest Quarter",
            "Mean Temperature of Driest Quarter",
            "Mean Temperature of Warmest Quarter",
            "Mean Temperature of Coldest Quarter",
            "Annual Precipitation",
            "Precipitation of Wettest Month",
            "Precipitation of Driest Month",
            "Precipitation Seasonality",
            "Precipitation of Wettest Quarter",
            "Precipitation of Driest Quarter",
            "Precipitation of Warmest Quarter",
            "Precipitation of Coldest Quarter") %>%
  str_to_lower() %>%
  str_replace_all(" ", "_")

if(!file.exists("data/wc2.1_10m_bio.zip")){
  download.file("https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_10m_bio.zip",
                destfile = "data/wc2.1_10m_bio.zip")
  unzip(zipfile = "data/wc2.1_10m_bio.zip", exdir = "data/wc2.1_10m_bio")
  
}


# burnable
# proportion of landcovers
# population and all that kind of stuff -- MAT, MAP
# Built environment data
# Anything we can find on policy - 
# maybe dissolve all fires into one polygon, then extract to that.
get_ancillary_data<- function(country_boundary){
  
  lut_kop <- c(rep("Equatorial",3), rep("Arid",4), rep("Temperate",9), rep("Boreal",12), rep("Polar",2))
  names(lut_kop)<-c(1:30)
  
  cb <- st_transform(country_boundary, crs=4326)
  
  aridity <- "data/too_big/ai_et0/ai_et0.tif" %>%
    terra::rast() %>% 
    terra::extract(., as(cb, "SpatVector")) %>%
    as_tibble() %>%
    summarise(median_ai = median(ai_et0, na.rm=T),
              mad_ai = mad(ai_et0, na.rm=T))
  
  wc_bio<- list.files("data/wc2.1_10m_bio", full.names =TRUE)[c(1,12:19,2:11)] %>%
    terra::rast()
  names(wc_bio) <- wc_names
  
  wc <- wc_bio %>% 
    terra::extract(., as(cb, "SpatVector"), fun = "median") %>%
    as_tibble() %>%
    dplyr::select(-ID)
  
  kc <- "data/Beck_KG_V1_present_0p0083.tif" %>%
    terra::rast() %>% 
    terra::extract(., as(cb, "SpatVector")) %>%
    as_tibble() %>%
    filter(Beck_KG_V1_present_0p0083>0)%>%
    mutate(kc = lut_kop[Beck_KG_V1_present_0p0083])%>%
    group_by(kc) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(pct = n/sum(n) *100) %>%
    dplyr::select(kc,pct)  %>%
    bind_rows(tibble(kc = c("Arid", "Equatorial", "Boreal", "Polar", "Temperate"),
                     pct = 0)) %>%
    group_by(kc) %>%
    summarise(pct = max(pct)) %>%
    ungroup() %>%
    pivot_wider(names_from = kc, values_from = pct, names_glue = "pct_{kc}") %>%
    mutate(koppen_mode = which.max(.[1,]) %>% names() %>% str_replace_all("pct_", ""))
  
  return(bind_cols(kc, aridity, wc))
}
  
# big stats function ===========================================================

get_fire_stats <- function(country_fires, end_date, country_boundary, name){
  
  country_fires <- mutate(country_fires, ig_date = as.Date(ig_date))
  
  country_fires_00s <- country_fires %>%  dplyr::filter(ig_year<=2011) 
  country_fires_10s <- country_fires %>%  dplyr::filter(ig_year>2011) 
  
  
  time_period_days <- as.Date(end_date, "%Y%j") - as.Date("2000-11-06")
  time_period_years<- units::as_units(time_period_days,"years") %>%as.numeric()
  
  time_period_days_00s <- as.Date("20111231", "%Y%m%d") - as.Date("2003-01-01")
  time_period_years_00s<- units::as_units(time_period_days_00s,"years") %>%as.numeric()
  
  time_period_days_10s <- as.Date(end_date, "%Y%j") - as.Date("2012-01-01")
  time_period_years_10s<- units::as_units(time_period_days_10s,"years") %>%as.numeric()
  
  area <- country_boundary %>%
    st_area() %>%
    as.numeric()
  
  area_km2<- area / 1000000
  
  burned_area_all <- sum(country_fires$tot_ar_km2_adj)

  fire_rotation_all <- time_period_years/(burned_area_all/area_km2) 
  
  # calculate peak month by burned area in addition to number of events
  # try probability mode

  stats_all <- country_fires %>%
    # st_set_geometry(NULL) %>%
    summarise(size = mean(tot_ar_km2_adj),
              n_fires = n(),
              total_ba = sum(tot_ar_km2_adj),
              log_size = mean(log(tot_ar_km2_adj+1)),
              log_n_fires = (n()+1) %>% log(), 
              log_mx_grw_km2 = mean(log(mx_grw_km2+1)),
              log_tba = sum(log(tot_ar_km2_adj+1)),
              log_dur = mean(log(event_dur + 1)),
              log_fsr_km2_dy=mean(log(fsr_km2_dy+1)),
              dur = mean(event_dur),
              mx_grw = mean(mx_grw_km2),
              fsr = mean(fsr_km2_dy),
              peak_season = weighted.mean(x=ig_day, w=tot_ar_km2_adj), 
              season_length = Hmisc::wtd.var(ig_day,tot_ar_km2_adj) %>% sqrt
    )%>%
    mutate(var="all")
  stats_00s <- country_fires_00s %>%
    # st_set_geometry(NULL) %>%
    summarise(size = mean(tot_ar_km2_adj),
              n_fires = n(),
              total_ba = sum(tot_ar_km2_adj),
              log_size = mean(log(tot_ar_km2_adj+1)),
              log_n_fires = (n()+1) %>% log(), 
              log_mx_grw_km2 = mean(log(mx_grw_km2+1)),
              log_tba = sum(log(tot_ar_km2_adj+1)),
              log_dur = mean(log(event_dur + 1)),
              log_fsr_km2_dy=mean(log(fsr_km2_dy+1)), dur = mean(event_dur),
              mx_grw = mean(mx_grw_km2),
              fsr = mean(fsr_km2_dy),
              peak_season = weighted.mean(x=ig_day, w=tot_ar_km2_adj), 
              season_length = Hmisc::wtd.var(ig_day,tot_ar_km2_adj)%>% sqrt
    )%>%
    mutate(var="00s")
  stats_10s <- country_fires_10s %>%
    # st_set_geometry(NULL) %>%
    summarise(size = mean(tot_ar_km2_adj),
              n_fires = n(),
              total_ba = sum(tot_ar_km2_adj),
              log_size = mean(log(tot_ar_km2_adj+1)),
              log_n_fires = (n()+1) %>% log(), 
              log_mx_grw_km2 = mean(log(mx_grw_km2+1)),
              log_tba = sum(log(tot_ar_km2_adj+1)),
              log_dur = mean(log(event_dur + 1)),
              log_fsr_km2_dy=mean(log(fsr_km2_dy+1)), dur = mean(event_dur),
              mx_grw = mean(mx_grw_km2),
              fsr = mean(fsr_km2_dy),
              peak_season = weighted.mean(x=ig_day, w=tot_ar_km2_adj), 
              season_length = Hmisc::wtd.var(ig_day,tot_ar_km2_adj)%>% sqrt
    ) %>%
    mutate(var="10s")
  

  
 
  
  # data wrangling ===============
  cfa_blank  <- tibble(ig_year = 2003:2020,
                       size=0,
                       log_size=0,
                       n_fires=0,
                       log_n_fires = 0,
                       total_ba=0,
                       log_tba = 0,
                       dur=NA,
                       log_dur = NA,
                       mx_grw=NA,
                       fsr=NA,
                       log_mx_grw_km2=NA,
                       log_fsr_km2_dy=NA,
                       season_length=0,
                       peak_season = NA)%>%
    mutate(decade = ifelse(ig_year>2011, "10s", "00s"))
  
  country_fires_annual <- country_fires %>%
    dplyr::filter(ig_year>2002,
                  ig_year<2021) %>%
    group_by(ig_year) %>%
    summarise(peak_season = weighted.mean(x=ig_day, w=tot_ar_km2_adj), 
              season_length = Hmisc::wtd.var(ig_day,tot_ar_km2_adj)%>% sqrt,
              size = mean(tot_ar_km2_adj),
              total_ba = sum(tot_ar_km2_adj),
              n_fires = n(),
              mx_grw = mean(mx_grw_km2),
              dur=mean(event_dur),
              fsr=mean(fsr_km2_dy),
              log_size = mean(log(tot_ar_km2_adj+1)),
              log_n_fires = (n()+1) %>% log(), 
              log_mx_grw_km2 = mean(log(mx_grw_km2+1)),
              log_tba = sum(log(tot_ar_km2_adj+1)),
              log_dur = mean(log(event_dur + 1)),
              log_fsr_km2_dy=mean(log(fsr_km2_dy+1))) %>%
    ungroup() %>%
    replace_na(list(season_length = 0))%>%
    mutate(decade = ifelse(ig_year>2011, "10s", "00s")%>% as.factor)
  
  yrs <- country_fires_annual$ig_year
  
  country_fires_annual <- country_fires_annual %>%
    bind_rows(cfa_blank %>% filter(!ig_year %in% yrs))
  
  cf_ks <- country_fires%>%
    mutate(log_size = (log(tot_ar_km2_adj+1)),
           log_n_fires = (n()+1) %>% log(), 
           log_mx_grw_km2 = (log(mx_grw_km2+1)),
           log_dur = (log(event_dur + 1)),
           log_fsr_km2_dy=(log(fsr_km2_dy+1)))%>%
    dplyr::rename(size = tot_ar_km2_adj,
                  dur = event_dur,
                  fsr = fsr_km2_dy,
                  mx_grw = mx_grw_km2)%>%
    dplyr::filter(ig_year>2002,
                  ig_year<2021) %>%
    mutate(decade = ifelse(ig_year>2011, "10s", "00s")%>% as.factor)
  
  minimum_class <- table(cf_ks$decade) %>% min()
  
  write_csv(country_fires_annual, file = glue("{out_ts_path}/{country_names[i]}.csv"))
  
  if(nrow(country_fires)<40 | minimum_class < 30){
    
    res <- bind_rows(stats_all, stats_00s, stats_10s) %>%
      mutate_if(is.numeric,signif,4) %>%
      pivot_wider(names_from = var,
                  values_from = names(.)[1:(ncol(.)-1)],
                  names_glue = "{var}_{.value}") %>%
      mutate(aoi = name,
             fire_rotation = fire_rotation_all,
             test_n_fires = NA,
             test_total_ba = NA,
             test_size = NA,
             test_dur = NA,
             test_mx_grw = NA,
             test_fsr = NA,
             test_peak_season = NA,
             test_season_length = NA,
             
             eff_n_fires = NA,
             eff_total_ba = NA,
             eff_size = NA,
             eff_dur = NA,
             eff_mx_grw = NA,
             eff_fsr = NA,
             eff_peak_season = NA,
             eff_season_length = NA,
             
             trend_n_fires = NA,
             trend_total_ba = NA,
             trend_size = NA,
             trend_dur = NA,
             trend_mx_grw = NA,
             trend_fsr = NA,
             trend_peak_season = NA,
             trend_season_length = NA,
             
             coeficient_n_fires = NA,
             coeficient_total_ba = NA,
             coeficient_size = NA,
             coeficient_dur = NA,
             coeficient_mx_grw = NA,
             coeficient_fsr = NA,
             coeficient_peak_season = NA,
             coeficient_season_length = NA,
             
             intercept_n_fires = NA,
             intercept_total_ba = NA,
             intercept_size = NA,
             intercept_dur = NA,
             intercept_mx_grw = NA,
             intercept_fsr = NA,
             intercept_peak_season = NA,
             intercept_season_length = NA)
    return(res)
    
  }
  # wilcox or ks tests --------------------------------------------------------
  h_test <- function(variable,df){
    formula <- formula(paste(variable, "~ decade"))
    wilcox.test(formula, data=df) %>%
      parameters::model_parameters() %>%
      bind_cols(df %>%  wilcox_effsize(formula))
  }
  
  vars1 <- c("size","log_size","dur","log_dur","mx_grw",
             "log_mx_grw_km2","fsr","log_fsr_km2_dy")
  vars2 <- c("peak_season",  "season_length", "n_fires","log_n_fires", "log_tba", "total_ba")
  
  tests1 <- lapply(vars1, h_test, cf_ks)
  tests2 <- lapply(vars2, h_test, country_fires_annual)
  tests_raw <- bind_rows(tests1) %>%
    bind_rows(tests2) %>%
    mutate(Parameter1 = replace(Parameter1, Parameter1 == "tot_ar_km2_adj", "size"))
 
  tests <- tests_raw %>%
    dplyr::select(Parameter1,p)%>%
    mutate(var = "test") %>%
    pivot_wider(names_from = Parameter1, values_from = p)

  test_l <- tests %>%
    pivot_longer(names_to = "variable", values_to = "pval", cols = names(.)[2:ncol(.)]) 
  
  test_dir <- bind_rows(stats_00s, stats_10s) %>%
    pivot_longer(names_to = "variable", values_to = "value", cols = names(.)[1:ncol(.)-1]) %>%
    pivot_wider(names_from = c( "var"), values_from = "value") %>%
    mutate(dir = `10s` - `00s`) %>%
    left_join(test_l) %>%
    mutate(test_dir = ifelse(pval<0.05, case_when(dir > 0 ~ 1, dir < 0 ~ -1), 0)) %>%
    dplyr::select(variable, test_dir, var) %>%
    pivot_wider(names_from = "variable", values_from = "test_dir",id_cols = "var")
  
  eff_size<-
    tests_raw %>%
    dplyr::select(Parameter1,effsize)%>%
    mutate(var = "eff") %>%
    pivot_wider(names_from = Parameter1, values_from = effsize)
  # trends in annual means -----------------------------------------------------
  # might use quantiles for ig month and end month, for season start and end
  # season length could be calculated off quantiles too

  # maybe a bootstrap approach is warranted here, to overcome non-normality
  # explore temporal autocorrelation stuff
  # gamma glm for example 
  
  ts_trend <- function(variable, df) {
    formula <- formula(paste(variable,"~ig_year"))
    na_query <- df%>% pull(variable) %>% is.na() %>% any()
    if(na_query) df <- na.omit(df)
    mblm(formula, df, 
         repeated = TRUE) %>%
      broom::tidy()%>% 
      mutate(term = replace(term, term == "(Intercept)", "intercept"),
             term = replace(term, term == "ig_year", "time")) %>%
      dplyr::select(Coefficient=estimate, p=p.value, term) %>%
      pivot_wider(names_from = term, values_from = c("Coefficient", "p")) %>%
      mutate(var = variable)
  }
  vars1 <- country_fires_annual %>%
    dplyr::select(-ig_year, -decade) %>%
    names()
  
  bigtab <- lapply(vars1, ts_trend, country_fires_annual) %>%
    bind_rows()%>%
    mutate(Coefficient_time = ifelse(p_time < 0.05, Coefficient_time, NA),
           Coefficient_intercept = ifelse(p_time < 0.05, Coefficient_intercept, NA))
  
  trends <- bigtab %>%
    dplyr::select(vals=Coefficient_time, var) %>%
    pivot_wider(names_from = var, values_from = vals)%>%
    mutate(var = "trend")
  coefs <- trends%>%
    mutate(var = "coeficient")
  ints <- bigtab %>%
    dplyr::select(vals=Coefficient_intercept, var) %>%
    pivot_wider(names_from = var, values_from = vals) %>%
    mutate(var = "intercept")
  
# All together now =============
 res <- bind_rows(trends, coefs, ints, stats_all, stats_00s, stats_10s, test_dir, eff_size) %>%
    mutate_if(is.numeric,signif,4) %>%
    pivot_wider(names_from = var,
                values_from = names(.)[1:(ncol(.)-1)],
                names_glue = "{var}_{.value}") %>%
    mutate_at(vars(matches("test")), ~case_when(. == 1 ~ "+",
                                                . == 0 ~"ns",
                                                . == -1~"-")) %>%
    mutate_at(vars(matches("trend")), ~case_when(. > 0 ~ "+",
                                                 . < 0 ~ "-",
                                                 is.na(.)~ "ns")) %>%
    mutate(aoi = name, 
           fire_rotation = fire_rotation_all)
 return(res)
}

# doing the individual countries ===============================================

unlink("data/temp/*",recursive = TRUE)

for(i in 1:length(country_names)){
  print(country_names[i])
  download.file(individual_countries$direct_download[i], destfile = "data/temp/temp.zip", method="curl")
  unzip("data/temp/temp.zip", exdir="data/temp")
  gpkg_file <- list.files("data/temp", pattern = "events.gpkg$", full.names = T)
 
  if(country_names[i] == "conus_ak") gpkg_file <-  list.files("data/temp", pattern = "_events_", full.names = T)[1]
  if(country_names[i] == "bolivia") gpkg_file <- list.files("data/temp/outputs", pattern = "events.gpkg$", full.names = T)
   
  if(country_names[i] != "conus_ak" & str_split(gpkg_file, pattern = "_")[[1]][2] != str_split(country_names[i], pattern = "_")[[1]][1]){
    stop("The download link is pointing to the wrong file. Correct the spreadsheet and try again.")}else{
      print("The correct file appears to have been downloaded.")
    }
  country_polygons <- st_read(gpkg_file) 
  if(is.character(country_polygons$ig_date)) {
    country_polygons <- country_polygons %>%
      mutate(ig_date_num = as.numeric(as.Date(ig_date)))
  }else{
    country_polygons <- country_polygons %>%
      mutate(ig_date_num = as.numeric((ig_date)))
  }
  
  country_boundary <- world_boundaries %>%
    st_transform(crs=st_crs(country_polygons)) %>%
    dplyr::filter(name_lwr == country_names[i]) %>%
    dplyr::select(name_lwr)
  
  ancillary <- get_ancillary_data(country_boundary = country_boundary)
  
  rowset <- seq(1,nrow(country_polygons),10000)
  
  corz<-parallel::detectCores() -1
  registerDoParallel(corz)
  country_fires <- foreach(g = rowset, .combine = bind_rows) %do% {
    system(paste("echo", g, "/", last(rowset)))
    out<-country_polygons %>%
      slice(g:(g+9999))%>%
    st_intersection(country_boundary) %>%
    mutate(tot_ar_km2_adj = st_area(.) %>% 
             set_units("km^2") %>%
             as.numeric) %>%
    st_set_geometry(NULL)
    return(out)
  }
  
  country_fires <- mutate(country_fires, ig_date = as.Date(ig_date))
  
  
  if(country_names[i] != "conus_ak"){end_date <- str_c("2",str_split(gpkg_file, pattern = "_to2", simplify=T)[,2]) %>%
    str_replace_all("_events.gpkg", "")}else{
      end_date <- "2021121"
    }
  
  get_fire_stats(country_fires = country_fires,
                 end_date = end_date,
                 country_boundary = country_boundary,
                 name=country_names[i]) %>%
    bind_cols(ancillary) %>% 
    write_csv(file = glue("{out_path}/{country_names[i]}.csv"))
  
  rm(country_polygons, country_boundary, country_fires)
  unlink("data/temp/*",recursive = TRUE)
  gc()
  
  print(paste(country_names[i], "done"))
}

# regions ======================================================================

for(r in 1:length(region_names)){
  
  print(region_names[r])
  download.file(regions$direct_download[r], destfile = "data/temp/temp.zip", method="curl")
  unzip("data/temp/temp.zip", exdir="data/temp")
  gpkg_file <- list.files("data/temp", pattern = "events.gpkg$", full.names = T)
  
  country_polygons <- st_read(gpkg_file) 
  if(is.character(country_polygons$ig_date)) {
    country_polygons <- country_polygons %>%
      mutate(ig_date_num = as.numeric(as.Date(ig_date)))
  }else{
    country_polygons <- country_polygons %>%
      mutate(ig_date_num = as.numeric((ig_date)))
  }
  country_names <- regions %>%
    filter(country_name == region_names[r]) %>%
    pull(countries_within) %>%
    str_split(" ") %>%
    unlist()
  
  end_date <- str_split(gpkg_file, pattern = "_to", simplify=T)[,2] %>%
    str_replace_all("_events.gpkg", "")
  
  for(i in 1:length(country_names)){
    print(country_names[i])
    if(country_names[i] == "bahamas") country_names[i] <- "the_bahamas"
    if(country_names[i] == "saint_barthelemy") {
      world_boundaries <- world_boundaries %>%
        mutate(name_lwr = replace(name_lwr, name_lwr == "saint-barthélemy", "saint_barthelemy"))
    }
    
    if(country_names[i] == "guadeloupe" |country_names[i] == "french_guiana"){
      country_boundary <- world_boundaries %>%
        st_transform(crs=st_crs(country_polygons)) %>%
        dplyr::filter(name_lwr == "france")
    }else{
    country_boundary <- world_boundaries %>%
      st_transform(crs=st_crs(country_polygons)) %>%
      dplyr::filter(name_lwr == country_names[i])
    }
    
    # country_fires <- country_polygons %>%
    #   st_intersection(country_boundary) %>%
    #   mutate(tot_ar_km2_adj = st_area(.) %>% set_units("km^2") %>% as.numeric) %>%
    #   st_set_geometry(NULL)
    rowset <- seq(1,nrow(country_polygons),10000)
    
    corz<-parallel::detectCores() -1
    registerDoParallel(corz)
    country_fires <- foreach(g = rowset, .combine = bind_rows) %do% {
      system(paste("echo", g, "/", last(rowset)))
      out<-country_polygons %>%
        slice(g:(g+9999))%>%
        st_intersection(country_boundary) %>%
        mutate(tot_ar_km2_adj = st_area(.) %>% 
                 set_units("km^2") %>%
                 as.numeric) %>%
        st_set_geometry(NULL)
      return(out)
    }
    
    
    if(nrow(country_fires) > 0){ancillary <- get_ancillary_data(country_boundary = country_boundary)

    get_fire_stats(country_fires = country_fires, 
                   end_date = end_date, 
                   country_boundary = country_boundary,
                   name = country_names[i])%>%
      bind_cols(ancillary) %>%
      write_csv(file = glue("{out_path}/{country_names[i]}.csv"))
    }
    rm(country_boundary, country_fires)
    gc()
  }
  unlink("data/temp/*",recursive = TRUE)
}

# and then because I decided to do north africa different =====================

download.file("https://scholar.colorado.edu/downloads/2v23vv53x", destfile = "data/temp/temp.zip")
unzip("data/temp/temp.zip", exdir="data/temp/cna")

country_names <- cna %>%
  pull(countries_within) %>%
  str_split(" ") %>%
  unlist() %>%
  sort
zipfiles <- list.files("data/temp/cna", full.names = TRUE) %>% sort

for(i in 1:length(country_names)){
  print(country_names[i])
  unzip(zipfile = zipfiles[i], exdir=glue("data/temp/{country_names[i]}"))
  
  
  gpkg_file <- list.files(glue("data/temp/{country_names[i]}"), 
                          pattern = "events.gpkg$", full.names = T)

  country_polygons <- st_read(gpkg_file) 
  if(is.character(country_polygons$ig_date)) {
    country_polygons <- country_polygons %>%
      mutate(ig_date_num = as.numeric(as.Date(ig_date)))
  }else{
    country_polygons <- country_polygons %>%
      mutate(ig_date_num = as.numeric((ig_date)))
  }
  country_boundary <- world_boundaries %>%
    st_transform(crs=st_crs(country_polygons)) %>%
    dplyr::filter(name_lwr == country_names[i])
  country_fires <- country_polygons %>%
    st_intersection(country_boundary) %>%
    mutate(tot_ar_km2_adj = st_area(.) %>%
             set_units("km^2") %>% 
             as.numeric) %>%
    st_set_geometry(NULL)
  end_date <- str_split(gpkg_file, pattern = "_to", simplify=T)[,2] %>%
    str_replace_all("_events.gpkg", "")
    
  ancillary <- get_ancillary_data(country_boundary = country_boundary)

  get_fire_stats(country_fires = country_fires,
                 end_date = end_date, 
                 country_boundary = country_boundary, 
                 name = country_names[i]) %>%
    bind_cols(ancillary) %>%
    write_csv(file = glue("{out_path}/{country_names[i]}.csv"))
  
  gc()
}
unlink("data/temp/*",recursive = TRUE)



# Australia ====================================================================
dir.create("data/aus_files")

aus_names[1] <- "australia_new_south_wales"
aus_names[6] <- "australia_western_australia"
aus_names[7] <- "australia_northern_territory"
for(r in 1:length(aus_names)){
  
  print(aus_names[r])
  download.file(australia_states$direct_download[r], destfile = "data/temp/temp.zip", method="curl")
  unzip("data/temp/temp.zip", exdir="data/temp")
  gpkg_file <- list.files("data/temp", pattern = "events.gpkg$", full.names = T)
  
  country_polygons <- st_read(gpkg_file) 
  if(is.character(country_polygons$ig_date)) {
    country_polygons <- country_polygons %>%
      mutate(ig_date_num = as.numeric(as.Date(ig_date)))
  }else{
    country_polygons <- country_polygons %>%
      mutate(ig_date_num = as.numeric((ig_date)))
  }
    if(aus_names[r] == "australia_new_south_wales"){
      state_boundary <- aus_boundaries %>%
        st_transform(crs=st_crs(country_polygons)) %>%
        dplyr::filter(name_lwr %in% c("australia_australian_capital_territory", "australia_new_south_wales"))%>%
        st_simplify() %>%
        summarise() %>%
        nngeo::st_remove_holes()
    }else{
    state_boundary <- aus_boundaries %>%
      st_transform(crs=st_crs(country_polygons)) %>%
      dplyr::filter(name_lwr == aus_names[r])%>%
      st_simplify(dTolerance = 500)
    }
    
  rowset <- seq(1,nrow(country_polygons),10000)
  
  corz<-parallel::detectCores() -1
  registerDoParallel(corz)
  country_fires <- foreach(g = rowset, .combine = bind_rows) %do% {
    system(paste("echo", g, "/", last(rowset)))
    out<-country_polygons %>%
      slice(g:(g+9999))%>%
      st_intersection(state_boundary) %>%
      mutate(tot_ar_km2_adj = st_area(.) %>% 
               set_units("km^2") %>%
               as.numeric) %>%
      st_set_geometry(NULL)
    return(out)
  }
  
  country_fires <- mutate(country_fires, ig_date = as.Date(ig_date))
    
    write_csv(country_fires, paste0("data/aus_files/",aus_names[r], ".csv"))
    unlink("data/temp/*")
    
}   

# merge aus states together
filez <- list.files("data/aus_files", pattern="csv$", full.names = TRUE)
country_fires <- lapply(filez, read_csv) %>%
  bind_rows()

country_boundary <- world_boundaries %>%
  st_transform(crs=st_crs(country_polygons)) %>%
  dplyr::filter(name_lwr == "australia") %>%
  dplyr::select(name_lwr)

ancillary <- get_ancillary_data(country_boundary = country_boundary)

end_date <- str_split(gpkg_file, pattern = "_to", simplify=T)[,2] %>%
  str_replace_all("_events.gpkg", "")

get_fire_stats(country_fires = country_fires, 
               end_date = end_date,
               country_boundary = country_boundary,
               name = "australia")%>%
  bind_cols(ancillary) %>%
  write_csv(file = glue("{out_path}/australia.csv"))

gc()
unlink("data/temp/*",recursive = TRUE)

# git push =====================================================================

system(paste0("git add ", out_path, "/*"))
system(paste0("git add ", out_ts_path, "/*"))

system(paste0("git add R/workflow.R"))

system(paste0("git commit -m 'mar3 run'"))
system(paste0("git push"))
