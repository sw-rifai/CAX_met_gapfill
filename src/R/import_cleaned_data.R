# fit_met-scaling_models
# descrip: Fit models to observations from CAX tower with 
# covariates derived from ERA5-Land, GPM, CHIRPS
# author: Sami Rifai
# date: 2023-03 (ongoing)
# notes: 
# target variables: 
#     t28m
#     rh28m 
#     vpd28m (from t28m + rh28m)
#     rad_global (srad?)
#     precip
#     vv (wind speed)
#     vv_max (gust speed)

pacman::p_load(h2o, data.table, tidyverse, lubridate, feather, units,
               scico,bigleaf, mgcv, gratia, cols4all)

# Load ERA5 & GPM =================
e5 <- lapply(list.files("data/CAX_met_GPM/","ERA5-Land-hourly___", full.names = T), 
             fread) %>% 
  rbindlist()
e5 <- e5[,-c("system:index", ".geo")]
e5 <- e5[site=='cax_south'] %>% 
  rename(t2m = temperature_2m,
         d2m = dewpoint_temperature_2m,
         ps = surface_pressure,
         ssrd = surface_solar_radiation_downwards,
         strd = surface_thermal_radiation_downwards,
         tot_evap = total_evaporation_hourly,
         u = u_component_of_wind_10m,
         v = v_component_of_wind_10m) %>% 
  mutate(time = date - hours(3)) %>% # date is UTC, time is local time
  mutate(hour = hour(time),
         day = lubridate::day(time),
         month = month(time),
         year = year(time)) %>% 
  mutate(t2m = t2m-273.15,
         d2m = d2m-273.15, 
         ws = sqrt(u**2 + v**2)) %>% 
  mutate(esat = bigleaf::Esat.slope(t2m)$Esat) %>% 
  mutate(vpd = esat - bigleaf::Esat.slope(d2m)$Esat) %>% 
  .[order(time)] %>% 
  .[,`:=`(tot_p24h = frollapply(total_precipitation,n=24,FUN = max, align='right')*1000)]
tz(e5$time) <- "America/Belem" # fix timezone
e5[,`:=`(date = date(date))] # date (here) does not include time
vec_names <- names(e5)
off_limits <- c("date","site","time","hour","date","month","year")
vec_names2 <- vapply(vec_names, function(x) if(x%in%off_limits){return(x)}else{return(paste0('e5_',x))},FUN.VALUE = 'character') %>% 
  unname()
names(e5) <- vec_names2

# Global Precipitation Measurement Mission Data 
gpm <- fread("data/CAX_met_GPM/GPM_IMERGv6_precipitationCal_mmhr_2000-06-01_2022-12-31.csv")
gpm <- gpm[,.(date, precipitationCal,site)] %>% 
  .[site=='cax_south'] %>% 
  mutate(time = date-hours(3)) %>% 
  rename(precip = precipitationCal) %>% # mm/hr 
  mutate(hour = hour(time),
         day = lubridate::day(time),
         month = month(time),
         year = year(time))
tz(gpm$time) <- "America/Belem" # fix timezone
gpm <- gpm[,.(g_p = mean(precip)),
           by=.(year,month,day,hour)]
# time is local time
gpm[,`:=`(time = ymd_h(paste(year,month,day," ",hour),tz='UTC')-hours(3))]

gpm <- gpm[order(time)][,`:=`(
  g_p3h_sum = frollsum(g_p,3,align = 'right'),
  g_p6h_sum = frollsum(g_p,6,align = 'right'),
  g_p12h_sum = frollsum(g_p,12,align = 'right'),
  g_p24h_sum = frollsum(g_p,24,align = 'right'),
  g_p48h_sum = frollsum(g_p,48,align = 'right'),
  g_p120h_sum = frollsum(g_p,120,align = 'right')
)]

# CHIRPS daily data 
chirps <- fread("data/CAX_met_GPM/CHIRPSv2_Daily__precipitation_mmday_2000-06-01_2022-12-31.csv")
chirps <- chirps [,.(date, precipitation,site)] %>% 
  .[site=='cax_south'] %>% 
  rename(c_p24h = precipitation) %>% 
  select(-site)
chirps <- chirps[order(date)][,`:=`(
  c_p48h = frollsum(c_p24h,n=2,align='right'),
  c_p120h = frollsum(c_p24h,n=5,align='right')
)]  
chirps[,`:=`(date=date(date))]

# merge era5, gpm, chirps
e5 <- merge(e5 %>% select(-c("year","month","hour","e5_day")),
            gpm %>% select(-c("year","month","hour","day")),
            by=c("time"))
e5 <- merge(e5,chirps,by='date')

# e5[sample(.N,10000)] %>% mutate(hour=hour(time)) %>% ggplot(aes(hour,e5_vpd))+
#   geom_point()+geom_smooth()

# Load met observations ====================================================

## part 1: 2001 - early 2020
obs <- arrow::read_parquet("data/met_obs_filtered/cax_tower_met_obs_2023-03-08.parquet")
obs[,`:=`(minute = minute(date))]
obs[,`:=`(time = round_date(date,unit='30 minutes'))]
tz(obs) <- "America/Belem"
obs[,`:=`(minute = minute(time))]
obs <- obs[minute==0]
obs <- obs[order(time)]

# summarize to each hour! important!!!
## 2016 (randomly) has measurements every 30 min
obs <- obs[, lapply(.SD, mean, na.rm=T), 
           .SDcols=sapply(obs, is.numeric),
           by=time]
# obs[, lapply(.SD, mean, na.rm=T), by=time]
obs <- obs %>% select(-year,-month,-day,-minute)

## part 2: 2020 - early 2023
obs2 <- arrow::read_parquet("data/met_obs_filtered/cax_tower_met_obs_2021-2022_processed_2023-03-17.parquet")

obs <- rbindlist(list(obs,obs2), fill=T)
obs <- obs %>% select(-year,-month,-day, -date)

## correct precip. Was originally 30 min accum, so multiply by 2
obs[,`:=`(precip = precip*2)]


## calc VPD before models:

# this did not work well... 
# obs <- obs %>% 
#   mutate(esat28m = bigleaf::Esat.slope(t28m)$Esat) %>% 
#   mutate(vpd28m = esat28m - bigleaf::Esat.slope(d28m)$Esat)

# this must have forced 0s instead of negative VPDs
obs <- obs %>% 
  mutate(vpd28m = bigleaf::rH.to.VPD(rh28m/100,t28m))
### A lot of ~0 VPD observations. Some decisions need to be made on what
### to do with these. I am going to filter for VPD > 0.


## merge obs with era5/gpm/chirps
names(e5)
dat <- merge(e5,obs,by=c("time"),all.x=T,all.y=F)
dat[,`:=`(ddate = decimal_date(time),
          year=year(time),
          month=month(time),
          hour = hour(time))]
dat

dat <- merge(dat,
             dat[,.(e5_t2m_u = mean(e5_t2m)), by=.(month,hour)], 
             by=c("month","hour")) %>% 
  .[,`:=`(e5_t2m_anom = e5_t2m-e5_t2m_u)]
