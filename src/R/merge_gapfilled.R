library(tidyverse); library(lubridate); library(feather)
list.files('outputs/')

rh2qair <- function(rh, T, press = 101325) {
  stopifnot(T[!is.na(T)] >= 0)
  # Tc <- udunits2::ud.convert(T, "K", "degC")
  Tc <- T
  es <- 6.112 * exp((17.67 * Tc) / (Tc + 243.5))
  e <- rh * es
  p_mb <- press / 100
  qair <- (0.622 * e) / (p_mb - (0.378 * e))
  ## qair <- rh * 2.541e6 * exp(-5415.0 / T) * 18/29
  return(qair)
} # rh2qair


# tmp1 <- read_feather('outputs/cax_precip_gapfill-GPM.feather') %>% 
#   mutate(p_mm_gf = if_else(is.na(p_mm_obs)==T, p_gpm, p_mm_obs)) %>% 
#   select(date, p_gpm, p_mm_obs, p_mm_gf)
tmp1 <- read_feather('outputs/cax_precip_gapfill-GPM.feather') %>% 
  mutate(hour=hour(date), 
         date_ymd=date(date)) %>% 
  group_by(date_ymd, hour) %>% 
  summarize(p_gpm = sum(p_gpm,na.rm=TRUE)*0.5, # 0.5 because units are mm/hr and summarizing from 0.5 hr to 1hr
            p_mm_obs = sum(p_mm_obs,na.rm=F)) %>% 
  ungroup() %>% 
  mutate(date = ymd_h(paste(date_ymd,hour))) %>% 
  mutate(p_mm_gf = if_else(is.na(p_mm_obs)==T, p_gpm, p_mm_obs)) %>% 
  select(date, p_gpm, p_mm_obs, p_mm_gf)
tmp2 <- read_feather('outputs/cax_pred_t28m_ 2020-01-15 .feather')
tmp3 <- read_feather('outputs/cax_pred_rh28m_ 2020-01-15 .feather')
tmp4 <- read_feather('outputs/cax_pred_rad_global_ 2020-01-15 .feather') %>% 
  mutate(shortwave_pred = ifelse(shortwave_pred<0,0,shortwave_pred)) %>% 
  mutate(shortwave_gf = ifelse(is.na(shortwave_obs)==T, shortwave_pred, shortwave_obs))
tmp5 <- read_feather('outputs/cax_windspeed_biascorrected_ 2020-01-16 .feather')

# old surface pressure, but no longer have the files on disk 
# tmp6 <- file.path('/home/sami/Downloads/scratch/merged','merged_SP_.nc') %>% 
#   # stars::read_ncdf(.) %>% 
#   tidync::tidync(.) %>% tidync::hyper_tibble() %>% 
#   rename(lon=longitude,
#          lat=latitude) %>% 
#   mutate(lon=lon-360) %>% 
#   filter(lon==-51.5 & lat==-1.75) %>% 
#   mutate(date = ymd_h('1900-01-01 00', tz='UTC')+hours(time-3))
tmp6 <- read_feather("outputs/cax_gapfill_2001_2018_2020-01-16.feather", 
                     columns = c("date","sp"))


tmp7 <- inner_join(tmp2,tmp3,by=c('date'),suffix=c('_temp','_rh'))
tmp7 <- inner_join(tmp7, tmp6 %>% rename(sp=sp) %>% select(date,sp))
tmp7 <- tmp7 %>% 
  mutate(specific_humidity=rh2qair(rh=rh28m_gf/100, T=t28m_gf, press=sp))

tmp8 <- read_feather("data/era5_cax_strd_1980_2018.feather") %>% 
  mutate(longwave = strd/(60**2)) %>% 
  select(date, longwave)

tmp8 %>% mutate(hour=hour(date)) %>% sample_n(1000) %>% 
  ggplot(data=.,aes(hour, longwave))+geom_point()+
  geom_smooth()
tmp8 %>% mutate(hour=hour(date)) %>% 
  sample_n(1000) %>% 
  ggplot(data=.,aes(date, longwave))+geom_point()+
  geom_smooth(method='lm')



merged <- inner_join(tmp1, tmp7, by='date')
merged <- inner_join(merged, tmp4, by='date')
merged <- inner_join(merged, tmp5, by='date')

merged %>% names
merged %>% 
  select(p_mm_gf, t28m_gf, specific_humidity, shortwave_gf, ws_bc, sp) %>% 
  is.na() %>% table
merged <- merged %>% 
  rename(t28m_obs = t28m, 
         rh28m_obs = rh28m, 
         specific_humidity_gf = specific_humidity, 
         windspeed = ws, 
         windspeed_bc = ws_bc) %>% 
  select(date, p_mm_obs, p_gpm, p_mm_gf,
         t28m_obs, t28m_pred, t28m_gf, 
         rh28m_obs, rh28m_pred, rh28m_gf,
         specific_humidity_gf, 
         sp,
         shortwave_obs, shortwave_pred, shortwave_gf,
         windspeed, windspeed_bc)

merged <- inner_join(merged, tmp8, by='date')

merged %>% write_feather(., paste0("outputs/cax_gapfill_2001_2018_proc",Sys.Date(),".feather"))
merged %>% write_csv(., paste0("outputs/cax_gapfill_2001_2018_proc",Sys.Date(),".csv"))

merged$date %>% range
merged %>% summary
ymd_hms("2000-05-31 21:00:00 UTC")-ymd_hms("2018-12-31 20:00:00 UTC")
