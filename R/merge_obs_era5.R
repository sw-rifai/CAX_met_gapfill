library(tidyverse); library(lubridate); library(feather); 

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


# Load CAX tower observations ---------------------------------------------
obs <- read_csv('data/met_obs_CAX_tower/cax_met_gapfill_2001-2016_notGF.csv')
obs <- obs %>%
  rename(date=Date,
         t2m_obs = `Temp (deg C)`,
         rh_obs = `Rh (%)`,
         rad_obs = `RAD (W/m2)`,
         p_mm_obs = `PPT (mm)`) %>% 
  mutate(year=year(date), month=month(date), hour=hour(date))
  

tmp <- readxl::read_excel("data/met_obs_CAX_tower/Torre_A_2018.xls",sheet = 1)

# 2016 and 2017 have the same columns
names(tmp2016) %in% names(tmp2017)
tmp2016 <- read_csv("data/met_obs_CAX_tower/Torre_A_2016.csv")
tmp2017 <- read_csv("data/met_obs_CAX_tower/Torre_A_2017.csv")

# distinct cols
tmp2018 <- read_csv("data/met_obs_CAX_tower/Torre_A_2018.csv")

# distinct cols
tmp2019 <- read_csv("data/met_obs_CAX_tower/Torre_A_2019.csv")


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


# Load GPM (mm/hr) ----------------------------------------------------------------
flist <- list.files('data/gpm/', full.names = T, pattern='csv')
tmp <- foreach(i=1:length(flist), .combine = rbind) %do%{
  read_csv(flist[i])
}
gpm <- tmp %>% 
  filter(grid_cell=='cax_north') %>% 
  mutate(date=ymd_hms(substr(`system:index`,1,14))-hours(3)) %>% 
  select(date, precipitationCal) %>% 
  distinct() %>% 
  mutate(p_gpm = precipitationCal) %>% 
  select(-precipitationCal)

m <- left_join(gpm, obs %>% select(date, p_mm_obs), by='date')
m <- m %>% 
  mutate(precip = if_else(is.na(p_mm_obs)==T, p_gpm, p_mm_obs))
m %>% 
  feather::write_feather(., 
            path="outputs/cax_precip_gapfill-GPM.feather")



# ERA5 vars ---------------------------------------------------------------
e_d2m <- feather::read_feather("data/era5_cax_d2m_1980_2018.feather") %>% 
  mutate(date=date-hours(3))
e_t2m <- feather::read_feather("data/era5_cax_t2m_1980_2018.feather") %>% 
  mutate(date=date-hours(3))
e_ssrd <- feather::read_feather("data/era5_cax_ssrd_1980_2018.feather") %>% 
  mutate(date=date-hours(3))
e_strd <- feather::read_feather("data/era5_cax_strd_1980_2018.feather") %>% 
  mutate(date=date-hours(3))
e_sp <- feather::read_feather("data/era5_cax_SP_1980_2018.feather") %>% 
  mutate(date=date-hours(3))
e_swvl1 <- feather::read_feather("data/era5_cax_swvl1_1980_2018.feather") %>% 
  mutate(date=date-hours(3))
e_swvl2 <- feather::read_feather("data/era5_cax_swvl2_1980_2018.feather") %>% 
  mutate(date=date-hours(3))
e_swvl3 <- feather::read_feather("data/era5_cax_swvl3_1980_2018.feather") %>% 
  mutate(date=date-hours(3))
e_swvl4 <- feather::read_feather("data/era5_cax_swvl4_1980_2018.feather") %>% 
  mutate(date=date-hours(3))

m <- inner_join(e_d2m, e_t2m)
m <- left_join(m, e_sp)
e_swvl1 <- inner_join(e_swvl1, e_swvl2); e_swvl1$date %>% max
e_swvl1 <- inner_join(e_swvl1, e_swvl3)
e_swvl1 <- inner_join(e_swvl1, e_swvl4)
m <- inner_join(m, e_swvl1 %>% select(date, SWVL1, SWVL2, SWVL3, SWVL4))
# m <- left_join(m, e_ssrd)
m <- left_join(m, e_ssrd %>% select(date, ssrd))
m <- left_join(m, e_strd %>% select(date, strd))

# table(unique(m$date) %in% unique(e_swvl1$date))
# table(unique(m$date) %in% unique(e_ssrd$date))
m <- m %>% mutate(year=year(date), month=month(date), hour=hour(date))


# t2m ---------------------------------------------------------------------
m %>% 
  filter(between(date, ymd('1979-01-01',tz='UTC'), ymd('2018-12-31',tz='UTC'))) %>% 
  mutate(t2m_m = var_2t-273.15) %>% 
  lm(t2m_m ~ year, data=.)
m %>% 
  filter(between(date, ymd('1979-01-01',tz='UTC'), ymd('2018-12-31',tz='UTC'))) %>% 
  mutate(t2m_m = var_2t-273.15) %>% 
  bam(t2m_m ~ s(year), data=., discrete = T, select=T) %>% 
  plot()

f_expectation <- obs %>% 
  sample_n(10000) %>% 
  # filter(year>=2002 & year<=2009) %>% 
  gam(t2m_obs~s(hour, bs='cc')+s(month, bs='cc')+year, data=., 
      method='REML')
f_expectation %>% summary
getViz(f_expectation) %>% plot(allTerms=T)

obs %>% 
  sample_n(10000) %>% 
  ggplot(data=., aes(date, t2m_obs))+
  geom_point()+
  geom_smooth(span=0.1)

obs
m <- left_join(m, obs, by='date')
names(m) <- tolower(names(m))
m <- m %>% 
  mutate(month=month(date), 
         hour=hour(date))

train <- m %>% 
  filter(is.na(t2m_obs)==F) %>% 
  sample_frac(0.25)
test <- m %>% 
  filter(is.na(t2m_obs)==F) %>% 
  anti_join(., train) %>% 
  sample_frac(0.5)

library(mgcv); library(mgcViz)
m_t2m <- gam(t2m_obs ~ s(var_2t), 
             method='REML',
             select=T,
             data=train)
summary(m_t2m)
getViz(m_t2m) %>% plot
print(paste('RMSE: ',sqrt(mean(predict(m_t2m, newdata=test, type='response')-test$t2m_obs)**2)))
cor(test$t2m_obs,predict(m_t2m, newdata=test, type='response'))**2 # r2

m2_t2m <- bam(t2m_obs ~ te(var_2t, ssrd, hour), 
              family=Gamma(link='log'),
             method='REML',
             select=T,
             data=train)
summary(m2_t2m)
gam.check(m2_t2m); abline(0,1,col='red')
getViz(m2_t2m) %>% plot
qq.gam(m2_t2m)
print(paste('RMSE: ',sqrt(mean(predict(m2_t2m, newdata=test, type='response')-test$t2m_obs)**2)))
cor(test$t2m_obs,predict(m2_t2m, newdata=test, type='response'))**2 # r2

m3_t2m <- bam(t2m_obs ~ te(var_2t, ssrd, hour), 
              family=Gamma(link='log'),
              method='REML',
              select=T,
              data=train)


install.packages('ranger')
r_t2m <- ranger::ranger(t2m_obs ~ var_2t+ ssrd+ hour +
                          var_2d+
                          swvl1+swvl2+swvl3+swvl4, 
               data=train, importance = 'impurity_corrected')
print(paste('RMSE: ',sqrt(mean(predict(r_t2m, data=test, type='response')$predictions-test$t2m_obs)**2)))
r_t2m$variable.importance %>% plot

pred_t2m <- m %>% 
  mutate(t2m_pred = predict(m2_t2m, newdata=m, type='response')) %>% 
  select(date,t2m_obs, t2m_pred)
pred_t2m %>% 
  mutate(year=year(date), 
         month=month(date)) %>% 
  group_by(year,month) %>% 
  summarize(val_pred = mean(t2m_pred,na.rm=T), 
            val_obs = mean(t2m_obs,na.rm=T), 
            nobs = sum(is.na(t2m_obs)==F)) %>% 
  ungroup() %>% 
  mutate(date = ymd(paste(year,month,1))) %>% 
  ggplot(data=., aes(date, val_obs, col=nobs))+
  geom_point()+
  geom_line(aes(date, val_pred))+
  scale_color_viridis_c()



m %>% 
  filter(date >= ymd('2002-01-01')) %>% 
  select(t2m_obs, var_2t) %>% 
  na.omit() %>% 
  cor


# Relative Humidity -------------------------------------------------------
m1_d2m <- bam(rh_obs ~ te(var_2t, var_2d, ssrd, hour), 
              family=Gamma(link='log'),
              method='fREML',
              select=T,
              discrete = T,
              nthreads = 10,
              data=train)
summary(m1_d2m)
gam.check(m1_d2m); abline(0,1,col='red')
getViz(m1_d2m) %>% plot
qq.gam(m1_d2m)
print(paste('RMSE: ',sqrt(mean(predict(m1_d2m, newdata=test, type='response')-test$rh_obs)**2)))
cor(test$d2m_obs,predict(m1_d2m, newdata=test, type='response'))**2 # r2







# # Scratch -----------------------------------------------------------------
# 
# 
# # half hourly
# m %>% 
#   mutate(year=year(date), month=month(date), hour=hour(date)) %>% 
#   filter(year != 2001) %>% 
#   sample_n(10000) %>% 
#   select(p_gpm, p_mm_obs) %>% 
#   na.omit() %>% 
#   cor
# 
# # daily
# m %>% 
#   mutate(year=year(date), month=month(date), hour=hour(date), 
#          doy = yday(date)) %>% 
#   filter(year != 2001) %>%
#   group_by(doy,year) %>% 
#   summarize(p_gpm = sum(p_gpm,na.rm=T), 
#             p_obs = sum(p_mm_obs, na.rm=T)) %>% 
#   ungroup() %>% 
#   sample_n(1000) %>% 
#   select(p_gpm, p_obs) %>% 
#   na.omit() %>% 
#   cor
# 
# # daily
# m %>% 
#   mutate(year=year(date), month=month(date), hour=hour(date), 
#          doy = yday(date)) %>% 
#   filter(year != 2001) %>%
#   group_by(month,year) %>% 
#   summarize(p_gpm = sum(p_gpm,na.rm=T), 
#             p_obs = sum(p_mm_obs, na.rm=T), 
#             nobs = sum(is.na(p_mm_obs)==F)) %>% 
#   ungroup() %>% # pull(nobs) %>% max
#   # sample_n(1000) %>%
#   filter(nobs >= 1344) %>%
#   mutate(date = ymd(paste(year,month,1))) %>% 
#   select(date, p_gpm, p_obs) %>% 
#   ggplot(data=., aes(date, p_obs))+geom_point()+geom_line()+
#   geom_line(aes(date, p_gpm), col='blue')
#   # na.omit() %>% 
#   # cor
#   # ggplot(data=., aes(p_gpm, p_obs))+geom_point()+
#   # geom_smooth(method='lm')+
#   # geom_abline(aes(intercept=0,slope=1), col='red')
# 
# m %>% 
#   mutate(year=year(date), month=month(date), hour=hour(date)) %>% 
#   filter(year != 2001) %>% 
#   sample_n(10000) %>% 
#   ggplot(data=., aes(p_gpm, p_mm_obs))+
#   geom_point()+
#   geom_smooth(method='lm')+
#   geom_abline(aes(intercept=0,slope=1), col='red')
# 
# gpm %>% 
#   mutate(hour=hour(date)) %>% 
#   mutate(year=year(date)) %>% 
#   group_by(year) %>% 
#   summarize(val = sum(p_gpm)) %>% 
#   ungroup() %>% 
#   ggplot(data=., aes(year, val))+
#   geom_point()
# 
#     
# 
# gpm %>% 
#   mutate(hour=hour(date)) %>% 
#   mutate(year=year(date)) %>% 
#   group_by(hour, year) %>% 
#   summarize(val = mean(p_gpm)) %>% 
#   ungroup() %>% 
#   ggplot(data=., aes(hour, val))+
#   geom_point()+geom_line(col='blue')+
#   geom_line(data=
#  obs %>% 
#   mutate(hour=hour(date)) %>% 
#    mutate(year=year(date)) %>% 
#   group_by(hour, year) %>% 
#   summarize(val = mean(p_mm_obs, na.rm=T)) %>% 
#   ungroup(), 
#  aes(hour, val), col='black')+
#   facet_wrap(~year)
# 
# 
# 
# gpm %>% 
#   mutate(year=year(date)) %>% 
#   group_by(year) %>% 
#   summarize(val = sum(precipitationCal, na.rm=T)) %>% 
#   ungroup() %>% 
#   ggplot(data=., aes(year, val))+
#   geom_line()
# 
# obs %>% 
#   mutate(year=year(date)) %>% 
#   group_by(year) %>% 
#   summarize(val = sum(p_mm_obs, na.rm=T), 
#             nobs = n()) %>% 
#   ungroup() %>% 
#   ggplot(data=., aes(year, val, color=nobs))+
#   geom_point()+
#   scale_color_viridis_c()
