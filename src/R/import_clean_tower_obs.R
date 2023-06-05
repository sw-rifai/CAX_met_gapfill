library(tidyverse); library(lubridate); library(feather); 
library(units)
# Required:                                    Source
# - Air pressure                             [Entirely reanalysis. No observations I'm aware of]
# - Specific humidity                      [observations and gapfill]
# - Air temperature                        [observations and gapfill]
# - Downward longwave radiation [not sure, probably reanalysis because I'm guessing the rad sensor is shortwave only?]
# - Downward shortwave radiation [observations and gapfill]
# - Precipitation                             [observations and satellite. Daily estimates are straightforward but getting the timing of hourly precipitation could could be very time consuming.]
# - Wind speed (or u and v components separately) [Really not sure. I think a lot of tests are needed between the limited observations and reanalysis estimates]
# 
# 

# --- Recent CAX tower observations abbreviations ---------------------------
# Bat (volts)	Bateria
# Temp_log (°C)	Temperatura do logger
# Tar_2m (°C)	Temperatura do ar a 2m
# Tar_16m (°C)	Temperatura do ar a 16m
# RH_16m (%)	Umidade relativa do ar a 16m
# Td_16m (°C)	Temperatura do ponto de orvalho a 16m
# Tw_16m (°C)	Temperatura do bulbo úmido a 16m
# PSV_ 16m (mbar)	Pressão de saturação do vapor a 16m
# Tar_28m (°C)	Temperatura do ar a 28m
# RH_28m (%)	Umidade relativa do ar a 28m
# Td_28m (°C)	Temperatura do ponto de orvalho a 28m
# Tw_28m (°C)	Temperatura do bulbo úmido a 28m
# PSV_ 28m (mbar)	Pressão de saturação do vapor a 28m
# Tar_42m (°C)	Temperatura do ar a 42m
# ETP_42m	Evapotranspiração a 42m
# Rso	Radiação solar
# RH_42m (%)	Umidade relativa do ar a 42m
# Td_42m (°C)	Temperatura do ponto de orvalho a 42m
# Tw_42m (°C)	Temperatura do bulbo úmido a 42m
# PSV_ 42m (mbar)	Pressão de saturação do vapor a 42m
# Chuva (mm)	Precipitação/Chuva
# RG (W/m²)	Radiação global
# RGT (MJ/m²)	Radiação global total
# NET (W/m²)	Saldo de radiação
# RH_2m (%)	Umidade relativa do ar a 2m
# DD (Graus)	Direção do vento
# RG (W/m²)	Radiação global
# RGT (MJ/m²)	Radiação global total
# NET (W/m²)	Saldo de radiação
# VV (m/s)	Velocidade do vento
# VV_max (m/s)	Velocidade máxima do vento

# Conversions: 



# Load CAX tower observations ---------------------------------------------
obs <- read_csv('data/met_obs_CAX_tower/cax_met_gapfill_2001-2016_notGF.csv')
obs <- obs %>%
  rename(date=Date,
         t28m = `Temp (deg C)`,
         rh28m = `Rh (%)`,
         rad_global = `RAD (W/m2)`,
         precip = `PPT (mm)`) %>% 
  mutate(year=year(date), month=month(date), hour=hour(date))
obs$hour %>% summary

# EDA - check temperature record
obs %>% 
  filter(hour==15) %>% 
  ggplot(data=.,aes(date, t28m))+
  geom_point()+
  geom_smooth(method='lm')


# 2016 and 2017 have the same columns so I'll join these first 
tmp2016 <- read_csv("data/met_obs_CAX_tower/Torre_A_2016.csv")
tmp2016 <- tmp2016 %>%   rename(day=Dia, month=Mês,year=Ano,hour=Hora) %>% 
  mutate(year = if_else(year< 2000, year+2000, year)) %>% 
  mutate(date = ymd_hms(paste(year,month,day," ",hour))) %>% 
  mutate(hour=hour(date))
tmp2016 %>% ggplot(data=., aes(date, Tar_28m))+geom_point()
  # select(year,month,day,hour)


tmp2017 <- read_csv("data/met_obs_CAX_tower/Torre_A_2017.csv")
tmp2017 <- tmp2017 %>%   rename(day=Dia, month=Mês,year=Ano,hour=Hora) %>% 
  mutate(year = if_else(year< 2000, year+2000, year)) %>% 
  mutate(date = ymd_hms(paste(year,month,day," ",hour))) %>% 
  mutate(hour=hour(date))
tmp2017 %>% ggplot(data=., aes(date, Tar_28m))+geom_point()



names(tmp2016) %in% names(tmp2017)
tmp_2016_2017 <- bind_rows(tmp2016, tmp2017)
names(tmp_2016_2017) %>% sort
tmp_2016_2017 <- tmp_2016_2017 %>% 
  rename(
         # day=Dia, month=Mês,year=Ano,hour=Hora,
         t2m=Tar_28m,
         t16m=Tar_16m,
         t28m=Tar_28m,
         t42m=Tar_42m, 
         rh2m=`UR_2m`, 
         rh16m=`UR_16m`, 
         rh28m=`UR_28m`, 
         rh42m=`UR_42m`, 
         precip=`Chuva`, 
         et42m = ETP_42, 
         rso=Rso, 
         vv = VV, 
         vv_max = VV_max, 
         direction=`DD`,
         rad_global = `RG`, 
         rad_glob_tot = `RGT`, 
         rad_net = `NET`)
tmp_2016_2017$hour %>% summary
tmp_2016_2017$day %>% summary
tmp_2016_2017$month %>% summary
tmp_2016_2017$year %>% summary

# distinct cols
tmp2018 <- read_csv("data/met_obs_CAX_tower/Torre_A_2018.csv")
tmp2018 %>%  # lots of missing data... 
  select(`Tar_2m (°C)`, `Tar_16m (°C)`, `Tar_28m (°C)`, `Tar_42m (°C)`) %>% 
  apply(., 2, FUN=function(x) sum(is.na(x)))

names(tmp2018)

# distinct cols
tmp2019 <- read_csv("data/met_obs_CAX_tower/Torre_A_2019.csv")
names(tmp2019)
glimpse(tmp2019)
tmp2019$`RH_2m (%)` %>% is.na %>% table
tmp2019$`RH_28m (%)` %>% is.na %>% table
tmp2019$`Td_28m (°C)` %>% is.na %>% table
tmp2019$`Tar_28m (°C)` %>% is.na %>% table

tmp2018 <- tmp2018 %>% rename(day=Dia, month=Mês,year=Ano,hour=Hora,
                   t2m=`Tar_2m (°C)`,t16m=`Tar_16m (°C)`,t28m=`Tar_28m (°C)`,t42m=`Tar_42m (°C)`, 
                   rh2m=`UR  2m (%)`, rh16m=`UR_16m (%)`, rh28m=`UR_28m (%)`, rh42m=`UR_42m (%)`, 
                   precip=`Chuva (mm)`, 
                   et42m = ETP_42, 
                   rso=`Rso (W/m²)`, 
                   vv = `VV (m/s)`, 
                   vv_max = `VV_max (m/s)`, 
                   direction=`DD (Graus)`,
                   rad_global = `RG (W/m²)`, 
                   rad_glob_tot = `RGT (MJ/m²)`, 
                   rad_net = `NET (W/m²)`) %>% 
  #convert units from MJ to Wm2
  mutate(rad_glob_tot = rad_glob_tot*1e6)

tmp2019 %>% names
tmp2019 <- tmp2019 %>% rename(day=Dia, month=Mês,year=Ano,hour=Hora, 
                   t2m=`Tar_2m (°C)`,t16m=`Tar_16m (°C)`,t28m=`Tar_28m (°C)`,t42m=`Tar_42m (°C)`) %>% 
  rename(rh2m=`RH_2m (%)`, rh16m=`RH_16m (%)`, rh28m=`RH_28m (%)`, rh42m=`RH_42m (%)`) %>% 
  rename(
    precip=`Chuva (mm)`, 
    et42m = ETP_42m, 
    rso=Rso, 
    rad_global = `RG (W/m²)`, 
    rad_glob_tot = `RGT (MJ/m²)`, 
    rad_net = `NET (W/m²)`
  )
tmp_2018_2019 <- bind_rows(tmp2018, tmp2019)
tmp_2018_2019 <- tmp_2018_2019 %>% 
  mutate(year = if_else(year< 2000, year+2000, year)) %>% 
  mutate(date = ymd_hms(paste(year,month,day," ",hour))) %>% 
  mutate(hour=hour(date))
tmp_2018_2019$hour %>% summary
tmp_2018_2019$day %>% summary
tmp_2018_2019$month %>% summary
tmp_2018_2019$year %>% summary


                   
tmp_2016_2019 <- bind_rows(tmp_2016_2017, 
          tmp_2018_2019)

obs <- bind_rows(obs, tmp_2016_2019) %>% 
  select(date,year,month,day,hour, 
         t28m, rh28m, rad_global, precip, 
         t2m, rh2m, t16m, rh16m, 
         t42m, rh42m, et42m,
         rso, rad_global, rad_glob_tot, rad_net,
         vv, vv_max, direction)



# Remove outliers ---------------------------------------------------------
obs <- obs %>%   # relative humidity --------------------------------------
  inner_join(., {
    obs %>% group_by(month, hour) %>% 
      summarize(sigma_rh28m = sd(rh28m,na.rm=T), 
                u_rh28m = mean(rh28m,na.rm=T)) %>% 
      ungroup()    
  }, by=c('month','hour')) %>% 
  ungroup() %>% 
  mutate(bad = case_when(rh28m < u_rh28m-3.5*sigma_rh28m ~ 'bad',
                         
                         rh28m > u_rh28m+3*sigma_rh28m ~ 'bad',
                         TRUE~'good')) %>% 
  mutate(rh28m = if_else(year==2011 & month>=4 & month<=6, NA_real_, rh28m)) %>%
  mutate(rh28m = if_else(year==2012 & month==6, NA_real_, rh28m)) %>%
  mutate(rh28m = if_else(year==2017 & month>=3, NA_real_, rh28m)) %>% 
  inner_join(., { # temperature --------------------------------------------
  obs %>% group_by(month, hour) %>% 
    summarize(sigma_t28m = sd(t28m,na.rm=T), 
              u_t28m = mean(t28m,na.rm=T)) %>% 
    ungroup()    
  }, by=c('month','hour')) %>% 
  ungroup() %>% 
  select(-bad) %>% 
  mutate(bad = case_when(t28m < u_t28m-3.5*sigma_t28m ~ 'bad',
                         t28m > u_t28m+3*sigma_t28m ~ 'bad',
                         TRUE~'good')) %>% 
  mutate(t28m = if_else(bad=='bad', NA_real_, t28m))
  # pull(t28m) %>% is.na %>% table
  
obs %>% 
  feather::write_feather(., path=paste0("data/cax_tower_met_obs_",Sys.Date(),".feather"))

#******************************************************************************
#******************************************************************************
#******************************************************************************
#******************************************************************************
#******************************************************************************
#******************************************************************************
#******************************************************************************
#******************************************************************************
#******************************************************************************
#******************************************************************************
#******************************************************************************
#******************************************************************************
#******************************************************************************
#******************************************************************************
#******************************************************************************
#******************************************************************************
#******************************************************************************
#******************************************************************************
#******************************************************************************
#******************************************************************************
#******************************************************************************
#******************************************************************************


obs %>% names
obs %>% 
  filter(is.na(t28m)==F) %>% 
  sample_frac(0.3) %>% 
  ggplot(data=.,aes(hour, rh28m, group=day, color=bad))+
  # geom_point()+
  geom_point()+
  # geom_smooth()+
  scale_color_viridis_d(end=0.8)+
  facet_grid(rows = vars(year), 
             cols = vars(month))



# outlier check: wind speed ------------------------------------------------
obs %>% 
  sample_n(50000) %>% 
  ggplot(data=.,aes(hour, vv, group=day, color=bad))+
  # geom_point()+
  geom_point()+
  scale_color_viridis_d(end=0.8)+
  facet_grid(rows = vars(year), 
             cols = vars(month))

# outlier check: radiation ------------------------------------------------
obs %>% 
  sample_n(50000) %>% 
  ggplot(data=.,aes(hour, rad_global, group=day, color=bad))+
  # geom_point()+
  geom_point()+
  scale_color_viridis_d(end=0.8)+
  facet_grid(rows = vars(year), 
             cols = vars(month))

obs %>% 
  pull(rad_glob_tot) %>% summary
  # mutate(lw = rad_glob_tot - rad_global) %>% 
  # pull(lw) %>% summary

library(ggpointdensity)
obs %>% 
  filter(is.na(rad_glob_tot)==F) %>% 
  filter(rad_glob_tot > 30) %>% 
  ggplot(data=., aes(rad_global, rad_glob_tot/3600))+
  geom_pointdensity()+
  geom_smooth(method='lm')+
  scale_color_viridis_c()

obs %>% 
  ggplot(data=., aes(hour, rad_glob_tot,color=as.factor(year)))+
  geom_point()+
  geom_smooth()+
  scale_color_viridis_d()

e_ssrd %>% 
  mutate(hour=hour(date), 
         year=year(date)) %>% 
  mutate(ssrd = ssrd/(60**2)) %>% 
  ggplot(data=., aes(hour, ssrd,color=as.factor(year)))+
  geom_point()+
  geom_smooth()+
  scale_color_viridis_d()

# outlier check: temperature ----------------------------------------------
obs %>% 
  inner_join(., {
    obs %>% group_by(month, hour) %>% 
      summarize(sigma_t28m = sd(t28m,na.rm=T), 
                u_t28m = mean(t28m,na.rm=T)) %>% 
      ungroup()    
  }, by=c('month','hour')) %>% 
  ungroup() %>% 
  mutate(bad = case_when(t28m < u_t28m-3.5*sigma_t28m ~ 'bad',
                         
                         t28m > u_t28m+3*sigma_t28m ~ 'bad',
                         TRUE~'good')) %>% 
  # pull(bad) %>% table
  # mutate(rh28m = if_else(year==2011 & month>=4 & month<=6, NA_real_, rh28m)) %>%
  # mutate(rh28m = if_else(year==2012 & month==6, NA_real_, rh28m)) %>%
  # mutate(rh28m = if_else(year==2017 & month>=3, NA_real_, rh28m)) %>%
  sample_n(50000) %>% 
  ggplot(data=.,aes(hour, t28m, group=day, color=bad))+
  # geom_point()+
  geom_point()+
  scale_color_viridis_d(end=0.8)+
  facet_grid(rows = vars(year), 
             cols = vars(month))



obs %>% 
  # sample_n(10000) %>% 
  ggplot(data=., aes(date, t28m))+
  geom_point()
  # geom_smooth()+
  # geom_smooth(method='lm',col='red')

obs %>% 
  filter(year<2005) %>% 
  ggplot(data=., aes(date, t28m))+
  geom_point()

obs %>% 
  # filter(year==2008) %>%
  # filter(month==12) %>% 
  mutate(day = day(date)) %>% 
  ggplot(data=.,aes(hour, t28m, group=day, color=month))+
  # geom_point()+
  geom_line()+
  scale_color_viridis_c()+
  facet_grid(rows = vars(year), 
             cols = vars(month))

obs %>% 
  # filter(year==2008) %>%
  # filter(month==12) %>% 
  mutate(day = day(date)) %>% 
  ggplot(data=.,aes(hour, rh28m, group=day, color=month))+
  # geom_point()+
  geom_line()+
  scale_color_viridis_c()+
  facet_grid(rows = vars(year), 
             cols = vars(month))

obs %>% group_by(month, hour) %>% 
  summarize(sigma_rh28m = sd(rh28m,na.rm=T), 
            u_rh28m = mean(rh28m,na.rm=T)) %>% 
  ungroup() %>% 
  ggplot(data=.,aes(hour,u_rh28m, color=as.factor(month)))+geom_line()+scale_color_viridis_d()

obs %>% 
  inner_join(., {
    obs %>% group_by(month, hour) %>% 
      summarize(sigma_rh28m = sd(rh28m,na.rm=T), 
                u_rh28m = mean(rh28m,na.rm=T)) %>% 
      ungroup()    
  }, by=c('month','hour')) %>% 
  ungroup() %>% 
  mutate(bad = case_when(rh28m < u_rh28m-3.5*sigma_rh28m ~ 'bad',
                         
                         rh28m > u_rh28m+3*sigma_rh28m ~ 'bad',
                        TRUE~'good')) %>% 
  # pull(bad) %>% table
  mutate(rh28m = if_else(year==2011 & month>=4 & month<=6, NA_real_, rh28m)) %>%
  mutate(rh28m = if_else(year==2012 & month==6, NA_real_, rh28m)) %>%
  mutate(rh28m = if_else(year==2017 & month>=3, NA_real_, rh28m)) %>%
  ggplot(data=.,aes(hour, rh28m, group=day, color=bad))+
  # geom_point()+
  geom_point()+
  scale_color_viridis_d(end=0.8)+
  facet_grid(rows = vars(year), 
             cols = vars(month))

obs %>% filter(year==2010 & month==12) %>%
  ggplot(data=., aes(date, rh28m))+
  geom_line()


obs %>% group_by(year) %>% summarize(val = max(rh28m,na.rm=T))

obs %>% 
  mutate(rh28m = if_else(year==2011 & month>=4 & month<=6, NA_real_, rh28m)) %>%
  mutate(rh28m = if_else(year==2012 & month==6, NA_real_, rh28m)) %>%
  mutate(rh28m = if_else(year==2017 & month>=3, NA_real_, rh28m)) %>%
  filter(year==2010) %>%
  mutate(day=day(date)) %>% 
  ggplot(data=., aes(hour, rh28m, color=as.factor(day)))+
  geom_line()+
  scale_color_viridis_d()+
  facet_wrap(~month)

  # mutate(bad = if_else(between(rh28m,
  #                      u_rh28m-3*sigma_rh28m,
  #                      u_rh28m-3*sigma_rh28m)==F,
  #                      "bad","ok"))
#


























obs %>% 
  mutate(year=year(date)) %>% 
  filter(year==2014) %>% 
  pull(t2m_obs) %>% hist

tmp2016 %>% 
  pull(t2m_obs) %>% hist
tmp2016 %>% 
  mutate(date = ymd_hms(paste(Ano, Mês, Dia, Hora))) %>% 
  select(date, Tar_2m, Tar_16m, Tar_28m, Tar_42m) %>% 
  gather(sensor, temp, -date) %>% 
  filter(date>= ymd('2016-07-01')) %>%
  filter(date< ymd('2016-08-01')) %>% 
  mutate(hour = hour(date)) %>% 
  filter(hour==15) %>% 
  # filter(between(date, ymd('2016-08-01'),ymd('2016-07-01'))) %>% 
  ggplot(data=., aes(date, temp, color=sensor))+
  geom_point(alpha=0.1)+
  geom_smooth()+
  scale_color_viridis_d()

obs %>%
  mutate(month=month(date), hour=hour(date)) %>% 
  filter(month==4 & hour==15) %>% 
  ggplot(data=.,aes(date, t2m_obs))+
  geom_point()+
  geom_smooth()

obs %>% 
  mutate(year=year(date), month=month(date), hour=hour(date)) %>%
  group_by(year, month, hour) %>% 
  summarize(val = mean(t2m_obs, na.rm=T), 
            nobs = sum(is.na(t2m_obs)==F)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(hour, val,  color=as.factor(year)))+
  geom_line()+
  scale_color_viridis_d()+
  facet_wrap(~month)

tmp2016 %>% 
  mutate(date = ymd_hms(paste(Ano, Mês, Dia, Hora))) %>% 
  select(date, Tar_2m, Tar_16m, Tar_28m, Tar_42m) %>% 
  gather(sensor, temp, -date) %>% 
  # filter(date>= ymd('2016-07-01')) %>%
  # filter(date< ymd('2016-08-01')) %>% 
  mutate(year=year(date), month=month(date), hour=hour(date)) %>%
  # filter(hour==15) %>% 
  # filter(between(date, ymd('2016-08-01'),ymd('2016-07-01'))) %>% 
  ggplot(data=., aes(hour, temp, color=sensor))+
  # geom_point(alpha=0.1)+
  geom_smooth()+
  scale_color_viridis_d(end=0.9)+
  facet_wrap(~month)

