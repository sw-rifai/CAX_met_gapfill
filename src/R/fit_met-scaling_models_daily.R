# fit_met-scaling_models_daily
# descrip: Fit models to observations from CAX tower with 
# covariates derived from ERA5-Land, GPM, CHIRPS
# author: Sami Rifai
# date: 2023-06 (ongoing)
# notes: Daily min/mean/max of each met variable
# target variables: 
#     t28m  - prototyped
#     rh28m - prototyped
#     rad_global - prototyped
#     precip - prototyped
#     vv (wind speed) - prototyped
#     vv_max (gust speed) - prototyped
#     vpd28m (predicted directly) - prototyped

## to do:
#     vpd28m (from t28m + rh28m)

pacman::p_load(h2o, data.table, tidyverse, lubridate, feather, units,
               scico,bigleaf, mgcv, gratia, cols4all)

# User options =============================
frac_train <- 0.9 # fraction of data used for training 
# n_covars <- 20 # number of covariates used for training
n_models <- 10 # number models fit in autoML

# Import cleaned met obs and ERA5 & GPM data ===============================
source("src/R/import_cleaned_data.R")

## only working on CAX south for now
full_dat <- dat[site=="cax_south"] %>% 
       select(-site, -ddate)

## cdat: 24 obs per day
vec_cdat <- full_dat[,.(nobs = sum(is.na(t28m)==F)),by=date][nobs==24]$date
cdat <- full_dat[date %in% vec_cdat]


# Summarize to daily min/mean/max ==========================================
dmin <- cdat %>% select(-c("hour","year","time")) %>% 
  .[, lapply(.SD, min, na.rm=T), by=.(date,month)] %>% 
  .[,`:=`(stat = "min")]
dmean <- cdat %>% select(-c("hour","year","time")) %>% 
  .[, lapply(.SD, mean, na.rm=T), by=.(date,month)] %>% 
  .[,`:=`(stat = "mean")]
dmax <- cdat %>% select(-c("hour","year","time")) %>% 
  .[, lapply(.SD, max, na.rm=T), by=.(date,month)] %>% 
  .[,`:=`(stat = "max")]

## stack
ddat <- rbind(dmin,dmean,dmax)
ddat <- ddat[,`:=`(stat = factor(stat,
                 levels = c("min","mean","max"),
                 ordered = F))] # h2o thrown an error when ordered


# Splice data for mod fitting ==============================================
set.seed(3)
ddat[,`:=`(ym = paste0(year(date),"_",month(date)))] %>% 
  .[,`:=`(year_cat = year(date) %>% as.factor())] %>% 
  .[,`:=`(month = factor(month))]
vec_ym <- ddat$ym %>% unique %>% sort
vec_train <- sample(vec_ym, floor(length(vec_ym)*frac_train)) # high fraction for training
vec_test <- vec_ym[!vec_ym %in% vec_train]
train <- ddat[ym %in% vec_train][is.na(t28m)==F]
tmp <- fsetdiff(ddat, train)[is.na(t28m)==F]
test <- tmp[sample(.N, floor(.N*0.5))][is.na(t28m)==F]
valid <- fsetdiff(tmp,test)[is.na(t28m)==F] # questionable if validation frame is needed

rm(tmp); gc();


# SPINUP H2O CLUSTER ==============================================
h2o.init(
  max_mem_size = "128g" # change mem size as needed
) 

covar_names <- c(names(train)[str_detect(names(train),"e5")],
                 names(train)[str_detect(names(train),"g_")],
                 names(train)[str_detect(names(train),"c_")],
                 "month","stat")


tmp_dmin <- full_dat %>% select(-c("hour","year","time")) %>% 
  .[, lapply(.SD, min, na.rm=T), by=.(date,month)] %>% 
  .[,`:=`(stat = "min")]
tmp_dmean <- full_dat %>% select(-c("hour","year","time")) %>% 
  .[, lapply(.SD, mean, na.rm=T), by=.(date,month)] %>% 
  .[,`:=`(stat = "mean")]
tmp_dmax <- full_dat %>% select(-c("hour","year","time")) %>% 
  .[, lapply(.SD, max, na.rm=T), by=.(date,month)] %>% 
  .[,`:=`(stat = "max")]

# t28m =========================================================================

## check no infinite vals
testthat::expect_false(
  all(train$t28m %>% is.infinite())
)

m_t28m <- h2o.automl(x=covar_names,
                     y='t28m',
                     training_frame = as.h2o(train[is.na(t28m)==F]), 
                     # validation_frame = test_t28m,
                     include_algos = c("GBM"),
                     max_models = n_models,
                     # max_runtime_secs = 1500,
                     seed=3)
perf_t28m <- h2o.performance(m_t28m@leader,
                              newdata=as.h2o(test[is.na(t28m)==F]))
perf_t28m@metrics$r2


# Generate full predicted time series

pdat <- rbind(tmp_dmin,tmp_dmean,tmp_dmax) %>% 
  select(all_of(covar_names), t28m, date) %>% 
  .[,`:=`(month = factor(month))]

pred_t28m <- h2o.predict(m_t28m, 
                    newdata=as.h2o(pdat)) %>% 
  as.data.table() %>% 
  set_names("pred_t28m")

out <- cbind(pdat,pred_t28m) %>% 
  select(date,stat,pred_t28m,t28m)

## write to disc
out %>% 
  # fwrite(., file=paste0("outputs/cax_met-pred-obs_t28m_2001_2022_daily_proc",Sys.Date(),".csv"))
fwrite(., file=paste0("outputs/cax_met-pred-obs_t28m_2001_2022_daily_proc.csv"))


# Plot evaluation

## just from train and validation
pdat <- h2o.predict(m_t28m, newdata=as.h2o(rbind(train,valid))) %>% 
  as.data.table() %>% 
  set_names("pred_t28m")

cbind(rbind(train,valid),pdat) %>% 
  # filter(year_cat == 2011) %>% 
  ggplot(aes(pred_t28m,t28m, color=stat))+
  geom_point(color='black',size=1,shape=1)+
  geom_point(alpha=0.25,size=0.5)+
  geom_abline(col='grey30',lwd=1.1)+
  geom_smooth(se=F,
              aes(group=stat),
              method='lm',
              color='black',
              lwd=1.1)+
  geom_smooth(se=F,
              method='lm',
              lwd=0.5)+
  labs(x="predicted T28 m",
       y="tower T28 m",
       color="daily T28 m (°C)")+
  scale_color_viridis_d(option='H')+
  coord_equal()+
  facet_wrap(~ year_cat,ncol = 7)+
  theme_linedraw()
ggsave(filename = "figures/figure_pred_vs_obs_daily_t28m.png",
       width=25,
       height=12,
       units='cm',
       dpi=350, 
       scale = 1.25)

# Predicted time series plot
out %>% 
  ggplot(aes(date,pred_t28m,color=stat))+
  geom_line()+
  theme_linedraw()
ggsave(filename = "figures/time_series_pred_t28m.png")


## problematic monthly spike in t28m 
## Unclear. Something could be wrong with reducing the hourly to daily
## tmax_spike is low, and tmin_spike is high... 
out[date>=ymd("2012-01-01")][date<=ymd("2012-12-31")] %>% 
  ggplot(aes(date,pred_t28m,color=stat))+
  geom_line()+
  theme_linedraw()
ggsave(filename = "figures/monthlySpike_pred_t28m.png")


# rh28m ========================

## check no infinite vals
testthat::expect_false(
  all(train$rh28m %>% is.infinite())
)

m_rh28m <- h2o.automl(x=covar_names,
                     y='rh28m',
                     training_frame = as.h2o(train[is.na(rh28m)==F]), 
                     # validation_frame = test_rh28m,
                     include_algos = c("GBM"),
                     max_models = n_models,
                     # max_runtime_secs = 1500,
                     seed=3)
perf_rh28m <- h2o.performance(m_rh28m@leader,
                             newdata=as.h2o(test[is.na(rh28m)==F]))
perf_rh28m@metrics$r2


# Generate full predicted time series

pdat <- rbind(tmp_dmin,tmp_dmean,tmp_dmax) %>% 
  select(all_of(covar_names), rh28m, date) %>% 
  .[,`:=`(month = factor(month))]

pred_rh28m <- h2o.predict(m_rh28m, 
                         newdata=as.h2o(pdat)) %>% 
  as.data.table() %>% 
  set_names("pred_rh28m")

out <- cbind(pdat,pred_rh28m) %>% 
  select(date,stat,pred_rh28m,rh28m)

## write to disc
out %>% 
  # fwrite(., file=paste0("outputs/cax_met-pred-obs_rh28m_2001_2022_daily_proc",Sys.Date(),".csv"))
  fwrite(., file=paste0("outputs/cax_met-pred-obs_rh28m_2001_2022_daily_proc.csv"))


# Plot evaluation 

## just from train and validation
pdat <- h2o.predict(m_rh28m, newdata=as.h2o(rbind(train,valid))) %>% 
  as.data.table() %>% 
  set_names("pred_rh28m")

cbind(rbind(train,valid),pdat) %>% 
  # filter(year_cat == 2011) %>% 
  ggplot(aes(pred_rh28m,rh28m, color=stat))+
  geom_point(color='black',size=1,shape=1)+
  geom_point(alpha=0.25,size=0.5)+
  geom_abline(col='grey30',lwd=1.1)+
  geom_smooth(se=F,
              aes(group=stat),
              method='lm',
              color='black',
              lwd=1.1)+
  geom_smooth(se=F,
              method='lm',
              lwd=0.5)+
  labs(x="predicted RH 28 m",
       y="tower RH 28 m",
       color="daily RH 28 m (%)")+
  scale_color_viridis_d(option='H')+
  coord_equal()+
  facet_wrap(~ year_cat,ncol = 7)+
  theme_linedraw()
ggsave(filename = "figures/figure_pred_vs_obs_daily_rh28m.png",
       width=25,
       height=12,
       units='cm',
       dpi=350, 
       scale = 1.25)

# Predicted time series plot
out %>% 
  ggplot(aes(date,pred_rh28m,color=stat))+
  geom_line()+
  theme_linedraw()
ggsave(filename = "figures/time_series_pred_rh28m.png")

# vpd28m ========================

## check no infinite vals
testthat::expect_false(
  all(train$vpd28m %>% is.infinite())
)

m_vpd28m <- h2o.automl(x=covar_names,
                       y='vpd28m',
                       training_frame = as.h2o(train[is.na(vpd28m)==F]), 
                       # validation_frame = test_vpd28m,
                       include_algos = c("GBM"),
                       max_models = n_models,
                       # max_runtime_secs = 1500,
                       seed=3)
perf_vpd28m <- h2o.performance(m_vpd28m@leader,
                               newdata=as.h2o(test[is.na(vpd28m)==F]))
perf_vpd28m@metrics$r2


# Generate full predicted time series

pdat <- rbind(tmp_dmin,tmp_dmean,tmp_dmax) %>% 
  select(all_of(covar_names), vpd28m, date) %>% 
  .[,`:=`(month = factor(month))]

pred_vpd28m <- h2o.predict(m_vpd28m, 
                           newdata=as.h2o(pdat)) %>% 
  as.data.table() %>% 
  set_names("pred_vpd28m")

out <- cbind(pdat,pred_vpd28m) %>% 
  select(date,stat,pred_vpd28m,vpd28m)

## write to disc
out %>% 
  # fwrite(., file=paste0("outputs/cax_met-pred-obs_vpd28m_2001_2022_daily_proc",Sys.Date(),".csv"))
  fwrite(., file=paste0("outputs/cax_met-pred-obs_vpd28m_2001_2022_daily_proc.csv"))


# Plot evaluation 

## just from train and validation
pdat <- h2o.predict(m_vpd28m, newdata=as.h2o(rbind(train,valid))) %>% 
  as.data.table() %>% 
  set_names("pred_vpd28m")

cbind(rbind(train,valid),pdat) %>% 
  # filter(year_cat == 2011) %>% 
  ggplot(aes(pred_vpd28m,vpd28m, color=stat))+
  geom_point(color='black',size=1,shape=1)+
  geom_point(alpha=0.25,size=0.5)+
  geom_abline(col='grey30',lwd=1.1)+
  geom_smooth(se=F,
              aes(group=stat),
              method='lm',
              color='black',
              lwd=1.1)+
  geom_smooth(se=F,
              method='lm',
              lwd=0.5)+
  labs(x="predicted VPD 28 m",
       y="tower VPD 28 m",
       color="daily VPD 28 m")+
  scale_color_viridis_d(option='H')+
  coord_equal()+
  facet_wrap(~ year_cat,ncol = 7)+
  theme_linedraw()
ggsave(filename = "figures/figure_pred_vs_obs_daily_vpd28m.png",
       width=25,
       height=12,
       units='cm',
       dpi=350, 
       scale = 1.25)

# Predicted time series plot
out %>% 
  ggplot(aes(date,pred_vpd28m,color=stat))+
  geom_line()+
  theme_linedraw()
ggsave(filename = "figures/time_series_pred_vpd28m.png")


# rad_global ========================

## check no infinite vals
testthat::expect_false(
  all(train$rad_global %>% is.infinite())
)


m_rad_global <- h2o.automl(x=covar_names,
                      y='rad_global',
                      training_frame = as.h2o(train[is.na(rad_global)==F]), 
                      # validation_frame = test_rad_global,
                      include_algos = c("GBM"),
                      max_models = n_models,
                      # max_runtime_secs = 1500,
                      seed=3)
perf_rad_global <- h2o.performance(m_rad_global@leader,
                              newdata=as.h2o(test[is.na(rad_global)==F]))
perf_rad_global@metrics$r2


# Generate full predicted time series

pdat <- rbind(tmp_dmin,tmp_dmean,tmp_dmax) %>% 
  select(all_of(covar_names), rad_global, date) %>% 
  .[,`:=`(month = factor(month))]

pred_rad_global <- h2o.predict(m_rad_global, 
                          newdata=as.h2o(pdat)) %>% 
  as.data.table() %>% 
  set_names("pred_rad_global")

out <- cbind(pdat,pred_rad_global) %>% 
  select(date,stat,pred_rad_global,rad_global)

## write to disc
out %>% 
  # fwrite(., file=paste0("outputs/cax_met-pred-obs_rad_global_2001_2022_daily_proc",Sys.Date(),".csv"))
  fwrite(., file=paste0("outputs/cax_met-pred-obs_rad_global_2001_2022_daily_proc.csv"))


# Plot evaluation 

## just from train and validation
pdat <- h2o.predict(m_rad_global, newdata=as.h2o(rbind(train,valid))) %>% 
  as.data.table() %>% 
  set_names("pred_rad_global")

cbind(rbind(train,valid),pdat) %>% 
  # filter(year_cat == 2011) %>% 
  ggplot(aes(pred_rad_global,rad_global, color=stat))+
  geom_point(color='black',size=1,shape=1)+
  geom_point(alpha=0.25,size=0.5)+
  geom_abline(col='grey30',lwd=1.1)+
  geom_smooth(se=F,
              aes(group=stat),
              method='lm',
              color='black',
              lwd=1.1)+
  geom_smooth(se=F,
              method='lm',
              lwd=0.5)+
  labs(x="predicted global radiation",
       y="tower global radiation",
       color="daily global radiation (W/m2)")+
  scale_color_viridis_d(option='H')+
  coord_equal()+
  facet_wrap(~ year_cat,ncol = 7)+
  theme_linedraw()
ggsave(filename = "figures/figure_pred_vs_obs_daily_rad_global.png",
       width=25,
       height=12,
       units='cm',
       dpi=350, 
       scale = 1.25)

# Predicted time series plot
out %>% 
  ggplot(aes(date,pred_rad_global,color=stat))+
  geom_line()+
  theme_linedraw()
ggsave(filename = "figures/time_series_pred_rad_global.png")


# precip ========================

## check no infinite vals
testthat::expect_false(
  all(train$precip %>% is.infinite())
)

m_precip <- h2o.automl(x=covar_names,
                           y='precip',
                           training_frame = as.h2o(train[is.na(precip)==F]), 
                           # validation_frame = test_precip,
                           include_algos = c("GBM"),
                           max_models = n_models,
                           # max_runtime_secs = 1500,
                           seed=3)
perf_precip <- h2o.performance(m_precip@leader,
                                   newdata=as.h2o(test[is.na(precip)==F]))
perf_precip@metrics$r2


# Generate full predicted time series

pdat <- rbind(tmp_dmin,tmp_dmean,tmp_dmax) %>% 
  select(all_of(covar_names), precip, date) %>% 
  .[,`:=`(month = factor(month))]

pred_precip <- h2o.predict(m_precip, 
                               newdata=as.h2o(pdat)) %>% 
  as.data.table() %>% 
  set_names("pred_precip")

out <- cbind(pdat,pred_precip) %>% 
  select(date,stat,pred_precip,precip)


out %>% arrange(ymd(out$date))
tmp_dmin %>%arrange(ymd(tmp_dmin$date))
full_dat%>%arrange(ymd(full_dat$date))
cdat%>%arrange(ymd(cdat$date))
ddat%>%arrange(ymd(ddat$date))

## write to disc
out %>% 
  # fwrite(., file=paste0("outputs/cax_met-pred-obs_precip_2001_2022_daily_proc",Sys.Date(),".csv"))
  fwrite(., file=paste0("outputs/cax_met-pred-obs_precip_2001_2022_daily_proc.csv"))


# Plot evaluation 

## just from train and validation
pdat <- h2o.predict(m_precip, newdata=as.h2o(rbind(train,valid))) %>% 
  as.data.table() %>% 
  set_names("pred_precip")

cbind(rbind(train,valid),pdat) %>% 
  # filter(year_cat == 2011) %>% 
  ggplot(aes(pred_precip,precip, color=stat))+
  geom_point(color='black',size=1,shape=1)+
  geom_point(alpha=0.25,size=0.5)+
  geom_abline(col='grey30',lwd=1.1)+
  geom_smooth(se=F,
              aes(group=stat),
              method='lm',
              color='black',
              lwd=1.1)+
  geom_smooth(se=F,
              method='lm',
              lwd=0.5)+
  labs(x="predicted precipitation",
       y="tower T28 precipitation",
       color="daily precipitation (mm)")+
  scale_color_viridis_d(option='H')+
  coord_equal()+
  facet_wrap(~ year_cat,ncol = 7)+
  theme_linedraw()
ggsave(filename = "figures/figure_pred_vs_obs_daily_precip.png",
       width=25,
       height=12,
       units='cm',
       dpi=350, 
       scale = 1.25)

# Predicted time series plot
out %>% 
  ggplot(aes(date,pred_precip,color=stat))+
  geom_line()+
  theme_linedraw()
ggsave(filename = "figures/time_series_pred_precip.png")

# vv ========================

## check no infinite vals
testthat::expect_false(
  all(train$vv %>% is.infinite())
)

m_vv <- h2o.automl(x=covar_names,
                       y='vv',
                       training_frame = as.h2o(train[is.na(vv)==F]), 
                       # validation_frame = test_vv,
                       include_algos = c("GBM"),
                       max_models = n_models,
                       # max_runtime_secs = 1500,
                       seed=3)
perf_vv <- h2o.performance(m_vv@leader,
                               newdata=as.h2o(test[is.na(vv)==F]))
perf_vv@metrics$r2


# Generate full predicted time series

pdat <- rbind(tmp_dmin,tmp_dmean,tmp_dmax) %>% 
  select(all_of(covar_names), vv, date) %>% 
  .[,`:=`(month = factor(month))]

pred_vv <- h2o.predict(m_vv, 
                           newdata=as.h2o(pdat)) %>% 
  as.data.table() %>% 
  set_names("pred_vv")

out <- cbind(pdat,pred_vv) %>% 
  select(date,stat,pred_vv,vv)

## write to disc
out %>% 
  # fwrite(., file=paste0("outputs/cax_met-pred-obs_vv_2001_2022_daily_proc",Sys.Date(),".csv"))
  fwrite(., file=paste0("outputs/cax_met-pred-obs_vv_2001_2022_daily_proc.csv"))


# Plot evaluation 

## just from train and validation
pdat <- h2o.predict(m_vv, newdata=as.h2o(rbind(train,valid))) %>% 
  as.data.table() %>% 
  set_names("pred_vv")

cbind(rbind(train,valid),pdat) %>% 
  # filter(year_cat == 2011) %>% 
  ggplot(aes(pred_vv,vv, color=stat))+
  geom_point(color='black',size=1,shape=1)+
  geom_point(alpha=0.25,size=0.5)+
  geom_abline(col='grey30',lwd=1.1)+
  geom_smooth(se=F,
              aes(group=stat),
              method='lm',
              color='black',
              lwd=1.1)+
  geom_smooth(se=F,
              method='lm',
              lwd=0.5)+
  labs(x="predicted vv m",
       y="tower vv",
       color="daily vv")+
  scale_color_viridis_d(option='H')+
  coord_equal()+
  facet_wrap(~ year_cat,ncol = 7)+
  theme_linedraw()
ggsave(filename = "figures/figure_pred_vs_obs_daily_vv.png",
       width=25,
       height=12,
       units='cm',
       dpi=350, 
       scale = 1.25)

# Predicted time series plot
out %>% 
  ggplot(aes(date,pred_vv,color=stat))+
  geom_line()+
  theme_linedraw()
ggsave(filename = "figures/time_series_pred_vv.png")

# vv_max ========================

## check no infinite vals
testthat::expect_false(
  all(train$vv_max %>% is.infinite())
)

m_vv_max <- h2o.automl(x=covar_names,
                   y='vv_max',
                   training_frame = as.h2o(train[is.na(vv_max)==F]), 
                   # validation_frame = test_vv_max,
                   include_algos = c("GBM"),
                   max_models = n_models,
                   # max_runtime_secs = 1500,
                   seed=3)
perf_vv_max <- h2o.performance(m_vv_max@leader,
                           newdata=as.h2o(test[is.na(vv_max)==F]))
perf_vv_max@metrics$r2


# Generate full predicted time series

pdat <- rbind(tmp_dmin,tmp_dmean,tmp_dmax) %>% 
  select(all_of(covar_names), vv_max, date) %>% 
  .[,`:=`(month = factor(month))]

pred_vv_max <- h2o.predict(m_vv_max, 
                       newdata=as.h2o(pdat)) %>% 
  as.data.table() %>% 
  set_names("pred_vv_max")

out <- cbind(pdat,pred_vv_max) %>% 
  select(date,stat,pred_vv_max,vv_max)

## write to disc
out %>% 
  # fwrite(., file=paste0("outputs/cax_met-pred-obs_vv_max_2001_2022_daily_proc",Sys.Date(),".csv"))
  fwrite(., file=paste0("outputs/cax_met-pred-obs_vv_max_2001_2022_daily_proc.csv"))


# Plot evaluation 

## just from train and validation
pdat <- h2o.predict(m_vv_max, newdata=as.h2o(rbind(train,valid))) %>% 
  as.data.table() %>% 
  set_names("pred_vv_max")

cbind(rbind(train,valid),pdat) %>% 
  # filter(year_cat == 2011) %>% 
  ggplot(aes(pred_vv_max,vv_max, color=stat))+
  geom_point(color='black',size=1,shape=1)+
  geom_point(alpha=0.25,size=0.5)+
  geom_abline(col='grey30',lwd=1.1)+
  geom_smooth(se=F,
              aes(group=stat),
              method='lm',
              color='black',
              lwd=1.1)+
  geom_smooth(se=F,
              method='lm',
              lwd=0.5)+
  labs(x="predicted vv",
       y="tower vv",
       color="daily vv")+
  scale_color_viridis_d(option='H')+
  coord_equal()+
  facet_wrap(~ year_cat,ncol = 7)+
  theme_linedraw()
ggsave(filename = "figures/figure_pred_vs_obs_daily_vv_max.png",
       width=25,
       height=12,
       units='cm',
       dpi=350, 
       scale = 1.25)

# Predicted time series plot
out %>% 
  ggplot(aes(date,pred_vv_max,color=stat))+
  geom_line()+
  theme_linedraw()
ggsave(filename = "figures/time_series_pred_vv_max.png")