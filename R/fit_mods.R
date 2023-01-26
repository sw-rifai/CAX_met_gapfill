# Required:                                    Source
# - Air pressure                             [Entirely reanalysis. No observations I'm aware of]
# - Specific humidity                      [observations and gapfill]
# - Air temperature                        [observations and gapfill]
# - Downward longwave radiation [not sure, probably reanalysis because I'm guessing the rad sensor is shortwave only?]
# - Downward shortwave radiation [observations and gapfill]
# - Precipitation                             [observations and satellite. Daily estimates are straightforward but getting the timing of hourly precipitation could could be very time consuming.]
# - Wind speed (or u and v components separately) [Really not sure. I think a lot of tests are needed between the limited observations and reanalysis estimates]
library(tidyverse); library(lubridate); library(feather); library(mgcv); library(mgcViz); 


# Import observed met record ----------------------------------------------
obs <- read_feather("data/cax_tower_met_obs_2020-01-15.feather"); 
obs <- obs %>% 
  mutate(day = day(date)) %>% 
  group_by(year,month,day,hour) %>% 
  summarize(t28m=mean(t28m,na.rm=T), 
            rh28m=mean(rh28m,na.rm=T), 
            rad_global=mean(rad_global,na.rm=T), 
            vv=mean(vv,na.rm=T)) %>% 
  ungroup()

# ERA5 vars ---------------------------------------------------------------
e_d2m <- feather::read_feather("data/era5_cax_d2m_1980_2018.feather") %>% 
  mutate(date=date-hours(3)) %>% select(-idx) %>% distinct()
e_t2m <- feather::read_feather("data/era5_cax_t2m_1980_2018.feather") %>% 
  mutate(date=date-hours(3)) %>% select(-idx) %>% distinct()
e_ssrd <- feather::read_feather("data/era5_cax_ssrd_1980_2018.feather") %>% 
  mutate(date=date-hours(3)) %>% select(-idx) %>% distinct()
e_strd <- feather::read_feather("data/era5_cax_strd_1980_2018.feather") %>% 
  mutate(date=date-hours(3)) %>% select(-idx) %>% distinct()
e_sp <- feather::read_feather("data/era5_cax_SP_1980_2018.feather") %>% 
  mutate(date=date-hours(3)) %>% select(-idx) %>% distinct()
e_swvl1 <- feather::read_feather("data/era5_cax_swvl1_1980_2018.feather") %>% 
  mutate(date=date-hours(3)) %>% select(-idx) %>% distinct()
e_swvl2 <- feather::read_feather("data/era5_cax_swvl2_1980_2018.feather") %>% 
  mutate(date=date-hours(3)) %>% select(-idx) %>% distinct()
e_swvl3 <- feather::read_feather("data/era5_cax_swvl3_1980_2018.feather") %>% 
  mutate(date=date-hours(3)) %>% select(-idx) %>% distinct()
e_swvl4 <- feather::read_feather("data/era5_cax_swvl4_1980_2018.feather") %>% 
  mutate(date=date-hours(3)) %>% select(-idx) %>% distinct()

m <- inner_join(e_d2m, e_t2m) %>% unique
m <- left_join(m, e_sp, by='date')
unique(e_t2m$time) %in% unique(e_sp$time) %>% table

e_swvl1 <- inner_join(e_swvl1 %>% select(SWVL1, date), 
                      e_swvl2 %>% select(SWVL2, date));
e_swvl1 <- inner_join(e_swvl1, e_swvl3 %>% select(SWVL3, date))
e_swvl1 <- inner_join(e_swvl1, e_swvl4 %>% select(SWVL4, date))
m <- inner_join(m, e_swvl1 %>% select(date, SWVL1, SWVL2, SWVL3, SWVL4))
# m <- left_join(m, e_ssrd)
m <- left_join(m, e_ssrd %>% select(date, ssrd), by='date')
m <- left_join(m, e_strd %>% select(date, strd), by='date')
m <- m %>% mutate(year=year(date), month=month(date), day=day(date), hour=hour(date))
names(m) <- tolower(names(m))
m <- m %>% filter(is.na(ssrd)==F)

# m %>% apply(., 2, FUN=function(x) sum(is.na(x)))
# e_ssrd$ssrd %>% is.na %>% table
# unique(e_ssrd$date) %in% unique(e_t2m$date) %>% table
# unique(e_t2m$date) %in% unique(e_ssrd$date) %>% table
# m %>% filter(is.na(ssrd)==T) %>% pull(date) %>% year %>% table

# temperature mod ---------------------------------------------------------
j_t <- inner_join(obs %>% 
                    filter(is.na(t28m)==F), 
                  m, 
                  by=c('year','month','day','hour'))
j_t <- j_t %>% filter(is.na(ssrd)==F)
train <- j_t %>% sample_frac(0.3)
test <- j_t %>% anti_join(., train) %>% sample_n(dim(train)[1])

library(parallel)  
nc <- 10   ## cluster size, set for example portability
if (detectCores()>1) { ## no point otherwise
  cl <- makeCluster(nc) 
  ## could also use makeForkCluster, but read warnings first!
} else cl <- NULL

# models 
f_t_0 <- bam(t28m~s(hour, bs='cc')+s(month, bs='cc'), data=train)
f_t_1 <- bam(t28m~s(hour, bs='cc')+s(month, bs='cc')+
                  s(var_2t), data=train)
f_t_2 <- bam(t28m~s(hour, bs='cc')+s(month, bs='cc')+
               te(var_2t, var_2d, ssrd)+
               s(swvl1, swvl4),
             discrete=T, 
             method='fREML',
             select=T, 
             nthreads = 10, 
             data=train)
f_t_3 <- bam(t28m~s(hour, bs='cc')+s(month, bs='cc')+
               te(var_2t, var_2d, ssrd)+
               s(swvl1)+s(swvl4),
             discrete=T, 
             method='fREML',
             select=T, 
             nthreads = 10, 
             # cluster = cl, # not for use with bam
             data=train)
f_t_4 <- bam(t28m~s(hour,month)+
               te(var_2t, var_2d, ssrd)+
               s(swvl1)+s(swvl4),
             discrete=T, 
             method='fREML',
             select=T, 
             nthreads = 10, 
             # cluster = cl, # not for use with bam
             data=train)
f_t_5 <- bam(t28m~
               s(hour,month)+
               te(var_2t, var_2d)+
               s(ssrd)+
               s(swvl1)+
               s(swvl4),
             discrete=T,method='fREML',
             select=T,nthreads = 10, data=train)
f_t_6 <- bam(t28m~
               s(hour,month)+
               te(var_2t, var_2d, ssrd)+
               s(swvl4, swvl1),
             discrete=T,method='fREML',
             select=T,nthreads = 10, data=train)

# summaries
summary(f_t_0); 
summary(f_t_1)
summary(f_t_2)
summary(f_t_3)
summary(f_t_4)
summary(f_t_5)
summary(f_t_6)

# visuals
getViz(f_t_1) %>% plot
getViz(f_t_2) %>% plot
getViz(f_t_3) %>% plot
getViz(f_t_4) %>% plot
getViz(f_t_5) %>% plot
getViz(f_t_6) %>% plot

# gofs
sqrt(mean((predict(f_t_0, type='response', newdata=test)-test$t28m)**2)) # 1.4
sqrt(mean((predict(f_t_1, type='response', newdata=test)-test$t28m)**2)) # 1.32
sqrt(mean((predict(f_t_2, type='response', newdata=test)-test$t28m)**2)) # 1.27
sqrt(mean((predict(f_t_3, type='response', newdata=test)-test$t28m)**2)) # 1.27
sqrt(mean((predict(f_t_4, type='response', newdata=test)-test$t28m)**2)) # 1.1918
sqrt(mean((predict(f_t_5, type='response', newdata=test)-test$t28m)**2)) # 1.197
sqrt(mean((predict(f_t_6, type='response', newdata=test)-test$t28m)**2)) # 1.199

bbmle::AICtab(f_t_0, f_t_1, f_t_2, f_t_3, 
              f_t_4, f_t_5, f_t_6)
pred_t <- m %>% 
  mutate(t28m_pred = predict(f_t_6, newdata=., type='response')) %>% 
  select(date, t28m_pred, var_2t)

pred_t <- left_join(pred_t %>% select(t28m_pred, date), 
          obs %>% mutate(date = ymd_h(paste(year,month,day,hour))) %>% select(date, t28m), 
          by='date')

pred_t %>% 
  mutate(t28m_gf = if_else(is.na(t28m)==T, t28m_pred, t28m), 
         gapfill = if_else(is.na(t28m)==T, T, F)) %>% 
  write_feather(., path=paste("outputs/cax_pred_t28m_",Sys.Date(),".feather"))


pred_t %>% 
  filter(is.na(t28m)==F) %>% 
  sample_n(10000) %>% 
  ggplot(data=., aes(t28m_pred, t28m))+
  ggpointdensity::geom_pointdensity()+
  scale_color_viridis_c()+
  geom_smooth(method='lm',se=F,col='black')+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  scale_x_continuous(expand=c(0,0), limits=range(pred_t$t28m))+
  scale_y_continuous(expand=c(0,0), limits=range(pred_t$t28m))+
  labs(x=expression(paste(Predicted~Air~Temperature~at~28~m~(degree*C))), 
       y=expression(paste(Obs~Air~Temp~at~28~m~(degree*C))))+
  theme_linedraw()+
  theme(legend.position = 'none')


pred_t %>% 
  mutate(year=year(date),month=month(date),day=day(date)) %>% 
  filter(year == 2015) %>% 
  ggplot(data=., aes(date, t28m))+
  geom_line(aes(date, t28m_pred),col='blue')+
  geom_point(size=0.1,col='black')+
  facet_wrap(~month, scales = 'free_x', nrow = 4)

  


pred_t %>% 
  ggplot(data=., aes(date, t28m_pred))+
  geom_smooth(method='lm')+
  geom_smooth(inherit.aes = F, aes(date, var_2t-273.15), method='lm',col='red')

pred_t %>% 
  mutate(year=year(date),month=month(date),day=day(date)) %>% 
  group_by(year,month,day) %>% 
  summarize(nobs=n()) %>% 
  ungroup() %>% 
  pull(nobs) %>% table

pred_t %>% 
  mutate(year=year(date),month=month(date),day=day(date)) %>% 
  filter(year==1983 & month==3) %>% 
  arrange(date)

  
  
  
# relative humidity mod ---------------------------------------------------------
j_rh <- inner_join(obs %>% 
                    filter(is.na(rh28m)==F), 
                  m, 
                  by=c('year','month','day','hour'))
j_rh <- j_rh %>% filter(is.na(ssrd)==F)
train <- j_rh %>% sample_frac(0.3)
test <- j_rh %>% anti_join(., train) %>% sample_n(dim(train)[1])


# models 
f_rh_0 <- bam(rh28m~s(hour, bs='cc')+s(month, bs='cc'), data=train)
f_rh_1 <- bam(rh28m~s(hour, bs='cc')+s(month, bs='cc')+
               s(var_2t)+s(var_2d)+s(ssrd)+
                s(swvl1)+s(swvl4), 
              data=train, select=T, method='fREML',discrete=T)
f_rh_2 <- bam(rh28m~s(hour, bs='cc')+s(month, bs='cc')+
               te(var_2t, var_2d, ssrd)+
               s(swvl1, swvl4),
             discrete=T, 
             method='fREML',
             select=T, 
             nthreads = 10, 
             data=train)
f_rh_3 <- bam(rh28m~s(hour, bs='cc')+s(month, bs='cc')+
               te(var_2t, var_2d, ssrd)+
               s(swvl1)+s(swvl4),
             discrete=T, 
             method='fREML',
             select=T, 
             nthreads = 10, 
             # cluster = cl, # not for use with bam
             data=train)
f_rh_4 <- bam(rh28m~s(hour,month)+
               te(var_2t, var_2d, ssrd)+
               s(swvl1)+s(swvl4),
             discrete=T, 
             method='fREML',
             select=T, 
             nthreads = 10, 
             # cluster = cl, # not for use with bam
             data=train)
f_rh_5 <- bam(rh28m~
               s(hour,month)+
               te(var_2t, var_2d)+
               s(ssrd)+
               s(swvl1)+
               s(swvl4),
             discrete=T,method='fREML',
             select=T,nthreads = 10, data=train)
f_rh_6 <- bam(rh28m~
               s(hour,month)+
               te(var_2t, var_2d, ssrd)+
               s(swvl4, swvl1),
             discrete=T,method='fREML',
             select=T,nthreads = 10, data=train)

# summaries
summary(f_rh_0); 
summary(f_rh_1)
summary(f_rh_2)
summary(f_rh_3)
summary(f_rh_4)
summary(f_rh_5)
summary(f_rh_6)

# visuals
getViz(f_rh_1) %>% plot
getViz(f_rh_2) %>% plot
getViz(f_rh_3) %>% plot
getViz(f_rh_4) %>% plot
getViz(f_rh_5) %>% plot
getViz(f_rh_6) %>% plot

# gofs
sqrt(mean((predict(f_rh_0, type='response', newdata=test)-test$rh28m)**2)) # 1.4
sqrt(mean((predict(f_rh_1, type='response', newdata=test)-test$rh28m)**2)) # 1.32
sqrt(mean((predict(f_rh_2, type='response', newdata=test)-test$rh28m)**2)) # 1.27
sqrt(mean((predict(f_rh_3, type='response', newdata=test)-test$rh28m)**2)) # 1.27
sqrt(mean((predict(f_rh_4, type='response', newdata=test)-test$rh28m)**2)) # 1.1918
sqrt(mean((predict(f_rh_5, type='response', newdata=test)-test$rh28m)**2)) # 1.197
sqrt(mean((predict(f_rh_6, type='response', newdata=test)-test$rh28m)**2)) # 1.199

bbmle::AICtab(f_rh_0, f_rh_1, f_rh_2, f_rh_3, 
              f_rh_4, f_rh_5, f_rh_6)
pred_rh <- m %>% 
  mutate(rh28m_pred = predict(f_rh_6, newdata=., type='response')) %>% 
  select(date, rh28m_pred)

pred_rh <- left_join(pred_rh %>% select(rh28m_pred, date), 
                    obs %>% mutate(date = ymd_h(paste(year,month,day,hour))) %>% select(date, rh28m), 
                    by='date')


curve(humidity::SVP(x, isK=F),30,31)
tealeaves::convert_conductance


pred_rh %>% 
  mutate(rh28m_gf = if_else(is.na(rh28m)==T, rh28m_pred, rh28m), 
         gapfill = if_else(is.na(rh28m)==T, T, F)) %>% 
  write_feather(., path=paste("outputs/cax_pred_rh28m_",Sys.Date(),".feather"))


pred_rh %>% 
  filter(is.na(rh28m)==F) %>% 
  sample_n(10000) %>% 
  ggplot(data=., aes(rh28m_pred, rh28m))+
  ggpointdensity::geom_pointdensity()+
  scale_color_viridis_c()+
  geom_smooth(method='lm',se=F,col='black')+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  scale_x_continuous(expand=c(0,0), limits=c(50,100))+
  scale_y_continuous(expand=c(0,0), limits=c(50,100))+
  labs(x=expression(paste(RH~'%')), 
       y=expression(paste(RH~'%')))+
  theme_linedraw()+
  theme(legend.position = 'none')


pred_rh %>% 
  mutate(year=year(date),month=month(date),day=day(date)) %>% 
  filter(year == 2015) %>% 
  ggplot(data=., aes(date, rh28m))+
  geom_line(aes(date, rh28m_pred),col='blue')+
  geom_point(size=0.1,col='black')+
  facet_wrap(~month, scales = 'free_x', nrow = 4)


# shortwave radiation -----------------------------------------------------
j_sw <- inner_join(obs %>% 
                     filter(is.na(rad_global)==F), 
                   m, 
                   by=c('year','month','day','hour'))
j_sw <- j_sw %>% filter(is.na(ssrd)==F)
train <- j_sw %>% sample_frac(0.3)
test <- j_sw %>% anti_join(., train) %>% sample_n(dim(train)[1])




# models 
f_sw_0 <- bam(rad_global~s(hour, bs='cc')+s(month, bs='cc'), data=train)
f_sw_1 <- bam(rad_global~s(hour, bs='cc')+s(month, bs='cc')+
                s(var_2t)+s(var_2d)+s(ssrd)+
                s(swvl1)+s(swvl4), 
              data=train, select=T, method='fREML',discrete=T)
f_sw_2 <- bam(rad_global~s(hour, bs='cc')+s(month, bs='cc')+
                te(var_2t, var_2d, ssrd)+
                s(swvl1, swvl4),
              discrete=T, 
              method='fREML',
              select=T, 
              nthreads = 10, 
              data=train)
f_sw_3 <- bam(rad_global~s(hour, bs='cc')+s(month, bs='cc')+
                te(var_2t, var_2d, ssrd)+
                s(swvl1)+s(swvl4),
              discrete=T, 
              method='fREML',
              select=T, 
              nthreads = 10, 
              # cluster = cl, # not for use with bam
              data=train)
f_sw_4 <- bam(rad_global~s(hour,month)+
                te(var_2t, var_2d, ssrd)+
                s(swvl1)+s(swvl4),
              discrete=T, 
              method='fREML',
              select=T, 
              nthreads = 10, 
              # cluster = cl, # not for use with bam
              data=train)
f_sw_5 <- bam(rad_global~
                s(hour,month)+
                te(var_2t, var_2d)+
                s(ssrd)+
                s(swvl1)+
                s(swvl4),
              discrete=T,method='fREML',
              select=T,nthreads = 10, data=train)
f_sw_6 <- bam(rad_global~
                s(hour,month)+
                te(var_2t, var_2d, ssrd)+
                s(swvl4, swvl1),
              discrete=T,method='fREML',
              select=T,nthreads = 10, data=train)

f_sw_7 <- bam(rad_global~
                te(hour, swvl1)+
                te(var_2d, swvl4)+
                te(hour,ssrd,strd),
              discrete=T,method='fREML',
              select=T,nthreads = 10, data=train)

# summaries
summary(f_sw_0); 
summary(f_sw_1)
summary(f_sw_2)
summary(f_sw_3)
summary(f_sw_4)
summary(f_sw_5)
summary(f_sw_6)
summary(f_sw_7)

# visuals
getViz(f_sw_1) %>% plot
getViz(f_sw_2) %>% plot
getViz(f_sw_3) %>% plot
getViz(f_sw_4) %>% plot
getViz(f_sw_5) %>% plot
getViz(f_sw_6) %>% plot


bbmle::AICtab(f_sw_0, f_sw_1, f_sw_2, f_sw_3, 
              f_sw_4, f_sw_5, f_sw_6, 
              f_sw_7)

# gofs
sqrt(mean((predict(f_sw_0, type='response', newdata=test)-test$rad_global)**2)) # 112
sqrt(mean((predict(f_sw_1, type='response', newdata=test)-test$rad_global)**2)) # 105
sqrt(mean((predict(f_sw_2, type='response', newdata=test)-test$rad_global)**2)) # 105
sqrt(mean((predict(f_sw_3, type='response', newdata=test)-test$rad_global)**2)) # 105
sqrt(mean((predict(f_sw_4, type='response', newdata=test)-test$rad_global)**2)) # 104
sqrt(mean((predict(f_sw_5, type='response', newdata=test)-test$rad_global)**2)) # 104
sqrt(mean((predict(f_sw_6, type='response', newdata=test)-test$rad_global)**2)) # 104
sqrt(mean((predict(f_sw_7, type='response', newdata=test)-test$rad_global)**2)) # 106.19

bbmle::AICtab(f_sw_0, f_sw_1, f_sw_2, f_sw_3, 
              f_sw_4, f_sw_5, f_sw_6)
pred_sw <- m %>% 
  mutate(shortwave_pred = predict(f_sw_6, newdata=., type='response')) %>% 
  select(date, shortwave_pred)

pred_sw <- left_join(pred_sw %>% select(shortwave_pred, date), 
                     obs %>% mutate(date = ymd_h(paste(year,month,day,hour))) %>% select(date, rad_global) %>% 
                       rename(shortwave_obs = rad_global), 
                     by='date')
pred_sw %>% 
  mutate(shortwave_gf = if_else(is.na(shortwave_obs)==T, shortwave_pred, shortwave_obs), 
         gapfill = if_else(is.na(shortwave_obs)==T, T, F)) %>% 
  write_feather(., path=paste("outputs/cax_pred_rad_global_",Sys.Date(),".feather"))


pred_sw %>% 
  filter(is.na(rad_global)==F) %>% 
  sample_n(10000) %>% 
  ggplot(data=., aes(rad_global_pred, rad_global))+
  ggpointdensity::geom_pointdensity()+
  scale_color_viridis_c()+
  geom_smooth(method='lm',se=F,col='black')+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  scale_x_continuous(expand=c(0,0), limits=c(50,100))+
  scale_y_continuous(expand=c(0,0), limits=c(50,100))+
  labs(x=expression(paste(sw~'%')), 
       y=expression(paste(sw~'%')))+
  theme_linedraw()+
  theme(legend.position = 'none')


pred_sw %>% 
  mutate(year=year(date),month=month(date),day=day(date)) %>% 
  filter(year == 2015) %>% 
  ggplot(data=., aes(date, rad_global))+
  geom_line(aes(date, rad_global_pred),col='blue')+
  geom_point(size=0.1,col='black')+
  facet_wrap(~month, scales = 'free_x', nrow = 4)



# Wind speed --------------------------------------------------------------
e_u <- file.path('/home/sami/Downloads/scratch/merged','merged_VAR_10U_.nc') %>% 
  stars::read_ncdf(.) %>% 
  as_tibble() %>% 
  rename(lon=longitude,
         lat=latitude) %>% 
  mutate(lon=lon-360) %>% 
  filter(lon==-51.5 & lat==-1.75)
e_v <- file.path('/home/sami/Downloads/scratch/merged','merged_VAR_10V_.nc') %>% 
  stars::read_ncdf(.) %>% 
  as_tibble() %>% 
  rename(lon=longitude,
         lat=latitude) %>% 
  mutate(lon=lon-360) %>% 
  filter(lon==-51.5 & lat==-1.75)
e_ws <- inner_join(e_u, e_v) %>% distinct()
e_ws <- e_ws %>% mutate(date = time - hours(3)) %>%
  distinct() %>% 
  select(-time)
e_ws <- e_ws %>% 
  mutate(ws = sqrt(VAR_10U**2 + VAR_10V**2)) %>% 
  units::drop_units()
e_ws <- e_ws %>% mutate(year=year(date), month=month(date), day=day(date), hour=hour(date))
tmp <- inner_join(obs, 
                  e_ws %>% 
                    mutate(year=year(date), month=month(date), day=day(date), hour=hour(date))) %>% 
  filter(is.na(vv)==F)
tmp %>% select(vv, ws)

library(foreach)
e_ws$ws_bc <- NULL
e_ws_bc <- foreach(i=0:23, .combine = rbind) %do% {
  vec <- MBC::QDM(o.c = tmp %>% filter(hour==i) %>% pull(vv), # obs
           m.c = tmp %>% filter(hour==i) %>% pull(ws), # mod
           m.p = e_ws %>% filter(hour==i) %>% pull(ws))
  e_ws %>% filter(hour==i) %>% 
    mutate(ws_bc = vec$mhat.p)
}
e_ws_bc %>% 
  ggplot(data=., aes(ws_bc))+
  geom_density(fill='red', inherit.aes = F, aes(ws))+
  geom_density(fill='blue')+
  facet_wrap(~hour)
  
e_ws_bc %>% 
  select(date, ws, ws_bc) %>% 
  write_feather(., path=paste("outputs/cax_windspeed_biascorrected_",Sys.Date(),".feather"))

e_ws %>% 
  sample_n(10000) %>% 
  mutate(year=year(date), month=month(date), hour=hour(date)) %>% 
  ggplot(data=., aes(hour, ws))+
  # geom_point()+
  geom_smooth()+
  facet_wrap(~month)

obs$vv %>% summary
e_ws$ws %>% summary


tmp %>% 
  ggplot(data=., aes(ws, vv))+
  geom_point()+
  geom_smooth(method='lm')

tmp %>% 
  ggplot()+
  geom_density(aes(vv),fill='red')+
  geom_density(aes(ws),fill='blue')+
  facet_wrap(~hour)

fn <- function(psi, psi_max=-0.1){
  -0.5*((psi-psi_max)/(psi+psi_max))+0.5
}
curve(fn(x), -5,0)
