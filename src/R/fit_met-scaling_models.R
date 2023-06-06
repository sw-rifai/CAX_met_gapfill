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

# Import cleaned met obs and ERA5 & GPM data ===============================
source("src/R/import_cleaned_data.R")


# Splice data for mod fitting ==============================================
set.seed(3)
dat[,`:=`(ym = paste0(year(date),"_",month(date)))]
vec_ym <- dat$ym %>% unique %>% sort
vec_train <- sample(vec_ym, floor(length(vec_ym)*0.9)) # high fraction for training
vec_test <- vec_ym[!vec_ym %in% vec_train]
# train <- dat[sample(.N, floor(.N*0.75))][is.na(t28m)==F]
# test <- fsetdiff(dat, train)[is.na(t28m)==F]
train <- dat[ym %in% vec_train][is.na(t28m)==F]
tmp <- fsetdiff(dat, train)[is.na(t28m)==F]
test <- tmp[sample(.N, floor(.N*0.5))][is.na(t28m)==F]
valid <- fsetdiff(tmp,test)[is.na(t28m)==F]




# SPINUP H2O CLUSTER ==============================================
h2o.init(
  max_mem_size = "128g" # change mem size as needed
) 

covar_names <- c(names(train)[str_detect(names(train),"e5")],
                 names(train)[str_detect(names(train),"g_")],
                 names(train)[str_detect(names(train),"c_")],
                 "month","hour")


# t28m ========================
a_t28m <- h2o.automl(x=covar_names,
                 y='t28m',
                 training_frame = as.h2o(train[is.na(t28m)==F]), 
                 # validation_frame = test_t28m,
                 include_algos = c("GBM"),
                 max_models = 3,
                 # max_runtime_secs = 1500,
                 seed=3)
perf_t28m <- h2o.performance(a_t28m@leader,
                newdata=as.h2o(test[is.na(t28m)==F]))
a_t28m@leader %>% h2o.varimp() %>% as.data.table()

# vpd28m ======================
a_vpd28m <- h2o.automl(x=covar_names,
                     y='vpd28m',
                     training_frame = as.h2o(train[is.na(vpd28m)==F][vpd28m>0]), 
                     # validation_frame = test_t28m,
                     include_algos = c("GBM"),
                     max_models = 3,
                     # max_runtime_secs = 1500,
                     seed=3)
perf_vpd28m <- h2o.performance(a_vpd28m@leader,
                             newdata=as.h2o(test[is.na(vpd28m)==F][vpd28m>0]))
a_vpd28m@leader %>% h2o.varimp() %>% as.data.table()

# rh28m ======================
a_rh28m <- h2o.automl(x=covar_names,
                       y='rh28m',
                       training_frame = as.h2o(train[is.na(rh28m)==F][rh28m<100]), 
                       # validation_frame = test_t28m,
                       include_algos = c("GBM"),
                       max_models = 3,
                       # max_runtime_secs = 1500,
                       seed=3)
perf_rh28m <- h2o.performance(a_rh28m@leader,
                               newdata=as.h2o(test[is.na(rh28m)==F][rh28m>0]))
a_rh28m@leader %>% h2o.varimp() %>% as.data.table()


# rad_global ======================
a_rad_global <- h2o.automl(x=covar_names,
                       y='rad_global',
                       training_frame = as.h2o(train[is.na(rad_global)==F]), 
                       # validation_frame = test_t28m,
                       include_algos = c("GBM"),
                       max_models = 3,
                       # max_runtime_secs = 1500,
                       seed=3)
perf_rad_global <- h2o.performance(a_rad_global@leader,
                               newdata=as.h2o(test[is.na(rad_global)==F]))
a_rad_global@leader %>% h2o.varimp() %>% as.data.table()

# precip ======================
a_precip <- h2o.automl(x=covar_names,
                           y='precip',
                           training_frame = as.h2o(train[is.na(precip)==F]), 
                           # validation_frame = test_t28m,
                           include_algos = c("GBM"),
                           max_models = 3,
                           # max_runtime_secs = 1500,
                           seed=3)
perf_precip <- h2o.performance(a_precip@leader,
                                   newdata=as.h2o(test[is.na(precip)==F]))
a_precip@leader %>% h2o.varimp() %>% as.data.table()

# windspeed ======================
a_vv <- h2o.automl(x=covar_names,
                       y='vv', # velocidade do vento
                       training_frame = as.h2o(train[is.na(vv)==F]), 
                       # validation_frame = test_t28m,
                       include_algos = c("GBM"),
                       max_models = 3,
                       # max_runtime_secs = 1500,
                       seed=3)
perf_vv <- h2o.performance(a_vv@leader,
                          newdata=as.h2o(test[is.na(vv)==F]))
a_vv@leader %>% h2o.varimp() %>% as.data.table()



# Set up predictions ======================================
pdat <- as.h2o(dat)
pred_t28m <- h2o.predict(a_t28m@leader, newdata = pdat) %>% 
  as.data.table() %>% 
  set_names("pred_t28m")
pred_vpd28m <- h2o.predict(a_vpd28m@leader, newdata = pdat) %>% 
  as.data.table() %>% 
set_names("pred_vpd28m")
pred_rh28m <- h2o.predict(a_rh28m@leader, newdata = pdat) %>% 
  as.data.table() %>% 
set_names("pred_rh28m")
pred_rad_global <- h2o.predict(a_rad_global@leader, newdata = pdat) %>% 
  as.data.table() %>% 
  set_names("pred_rad_global")
pred_precip <- h2o.predict(a_precip@leader, newdata = pdat) %>% 
  as.data.table() %>% 
  set_names("pred_precip")
pred_vv <- h2o.predict(a_vv@leader, newdata = pdat) %>% 
  as.data.table() %>% 
  set_names("pred_windspeed")


pdat2 <- cbind(dat %>% select(time,date,year,month,hour,
                              t28m,vpd28m,rh28m,rad_global,
                              precip,vv),
               pred_t28m,pred_vpd28m,pred_rh28m,pred_rad_global,
               pred_precip, pred_vv)

pdat2 %>% 
  arrow::write_parquet(., 
                       sink = 
    paste0("outputs/cax_met-pred-obs_2001_2022_proc",Sys.Date(),".parquet"),
    compression = 'snappy')

pdat2 %>% 
  fwrite(., file=paste0("outputs/cax_met-pred-obs_2001_2022_proc",Sys.Date(),".csv"))


# SCRAP __________________________________________________________________
# pdat2$pred_windspeed %>% hist(1000)
# 
# 
# 
# 
# tmp1 <- cbind(dat %>% select(time,date,year,month,hour,precip,
#                              g_p),pred_precip)
# 
# 
# tmp1[year%in%c(2021,2022)][,.(val = sum(pred_precip)*2,
#                     p = mean(precip,na.rm=T)*2*744,
#                     p_nobs = sum(is.na(precip)==F),
#                     g_tot = sum(g_p,na.rm=T)),
#                    by=.(year,month)] %>% 
#   # pull(p_nobs) %>% 
#   ggplot(aes(month,val))+
#   geom_point()+
#   geom_point(aes(month,p),col='blue')+
#   geom_point(aes(month,g_tot),col='red')+
#   facet_wrap(~year)
# 
# 
# 
# obs2[time >= ymd("2020-01-01")][time <= ymd("2020-12-31")]
# 
# 
# 
# 
# 
# 
# 
# 
# 
# dat[date%in%(dat[t28m > 34.15]$date[1:20] %>% unique)] %>% 
#   ggplot(aes(hour, 
#              t28m))+
#   geom_point()+
#   facet_wrap(~date, scales = 'free_x')
# 
# # Fit models ===============================================================
# 
# # models
# f_t_0 <- gam(t28m  ~ 
#                # e5_t2m*e5_ssrd,
#              s(e5_t2m, bs='ad'),
#                # te(e5_t2m,e5_ssrd, bs='ts'),
#                # s(hour, bs='cc')+
#                # s(month, bs='cc'),
#              # family = Gamma(link='log'),
#              data=train[sample(.N,20000)])
# summary(f_t_0)
# draw(f_t_0)+geom_smooth(method='lm')
# predict(f_t_0)
# test %>% 
#   mutate(pred = predict(f_t_0,newdata=.,type='response')) %>% 
#   ggplot(data=.,aes(t28m,pred))+
#   geom_point(alpha=0.5,size=0.5)+
#   geom_abline(col='#cf0000')+
#   geom_smooth()+
#   theme_linedraw()
# test %>% 
#   mutate(pred = predict(f_t_0,newdata=.,type='response')) %>% 
#   ggplot(data=.,aes(pred,t28m))+
#   geom_point(alpha=0.5,size=0.5)+
#   geom_abline(col='#cf0000')+
#   geom_smooth()+
#   theme_linedraw()
# 
# # f_t_1 <- bam(t28m~s(hour, bs='cc')+
# #                s(month, bs='cc')+
# #                s(ddate, bs='ts') +
# #                s(e5_t2m), 
# #              data=train, 
# #              select=T)
# # f_t_2 <- bam(t28m~ #s(hour, bs='cc')+
# #                    #s(month, bs='cc')+
# #                # ti(e5_ssrd,hour,month)+
# #              # te(e5_t2m, e5_d2m)+
# #              #   te(e5_t2m,hour,month), 
# #                s(ddate, bs='ts') +
# #              te(e5_t2m, e5_ssrd,month, bs=c("ts","ts","cc")),
# #              data=train, 
# #            select=T,
# #            discrete=T)
# # summary(f_t_2)
# # 
# # summary(f_t_1); 
# # summary(f_t_2)
# # # appraise(f_t_2)
# # metrica::R2(obs = test$t28m, 
# #             pred = predict(f_t_2,newdata=test),
# #             na.rm=T)
# # 
# # quantile(test$t28m,na.rm=T, c(0.01,0.5,0.99))
# # quantile(predict(f_t_2,newdata=test),na.rm=T,c(0.01,0.5,0.99))
# 
# 
# 
# a1 <- h2o.automl(x=covar_names,
#                y='t28m',
#                training_frame = train_t28m, 
#                # validation_frame = test_t28m,
#                include_algos = c("GBM"),
#              max_models = 5,
#                # max_runtime_secs = 200,
#              seed=3)
# h2o.get_leaderboard(a1)
# a1@leaderboard
# best_aml <- a1@leader
# # best_aml <- h2o.getModel(gbm_gridperf1@model_ids[[1]])
# h2o.r2(best_aml)
# h2o.performance(best_aml, newdata=test_t28m)@metrics
# h2o.varimp(best_aml) %>% as.data.table()
# a1
# 
# 
# 
# test_t28m %>% 
#   as.data.table() %>% 
#   mutate(
#     pred = as.data.table(h2o.predict(best_aml,test_t28m)[,'predict'])$predict) %>% 
#   # lm(t28m ~ pred, data=.) %>% summary
#   ggplot(data=.,aes(t28m,pred))+
#   geom_point(alpha=0.5,size=0.5)+
#   geom_abline(col='#cf0000')+
#   geom_smooth()+
#   theme_linedraw()
# 
# test_t28m %>% 
#   as.data.table() %>% 
#   mutate(
#     pred = as.data.table(h2o.predict(best_aml,test_t28m)[,'predict'])$predict) %>% 
#   ggplot(data=.,aes(pred,t28m))+
#   geom_point(alpha=0.5,size=0.5)+
#   geom_abline(col='#cf0000')+
#   geom_smooth()+
#   theme_linedraw()
# 
# 
# g1 <- h2o.gbm(x=covar_names,
#                y='t28m',
#                training_frame = train_t28m, 
#                # validation_frame = test_t28m,
#   # calibrate_model = T,
#   # calibration_frame = test_t28m,
#                # max_runtime_secs = 200,
#   histogram_type = 'AUTO',
#              seed=3)
# summary(g1)
# h2o.performance(g1, newdata=test_t28m)@metrics
# test_t28m %>% 
#   as.data.table() %>% 
#   mutate(
#     pred = as.data.table(h2o.predict(g1,test_t28m)[,'predict'])$predict) %>% 
#   # lm(t28m ~ pred, data=.) %>% summary
#   ggplot(data=.,aes(t28m,pred))+
#   geom_point(alpha=0.5,size=0.5)+
#   geom_abline(col='#cf0000')+
#   geom_smooth()+
#   theme_linedraw()
# 
# test_t28m %>% 
#   as.data.table() %>% 
#   mutate(
#     pred = as.data.table(h2o.predict(g1,test_t28m)[,'predict'])$predict) %>% 
#   # lm(t28m ~ pred, data=.) %>% summary
#   ggplot(data=.,aes(pred,t28m))+
#   geom_point(alpha=0.5,size=0.5)+
#   geom_abline(col='#cf0000')+
#   geom_smooth()+
#   theme_linedraw()
# 
# 
# 
# pd <- as.h2o(dat)
# pdat <- dat
# pdat$pred_t28m <- h2o.predict(best_aml, newdata=pd) %>% 
#   as.data.table() %>% 
#   .$predict
# 
# 
# pdat <- pdat[order(time)][,`:=`(
#   pred_t28m_max_72h = frollapply(pred_t28m,n=72,FUN=max,align = 'right'),
#   pred_t28m_mean_72h = frollmean(pred_t28m,n=72,na.rm=F,align = 'right'),
# pred_t28m_min_72h = frollapply(pred_t28m,n=72,FUN=min,align = 'right'),
#   obs_t28m_max_72h = frollapply(t28m,n=72,FUN=max,align = 'right'),
#   obs_t28m_mean_72h = frollmean(t28m,n=72,align = 'right',na.rm = F),
#   obs_t28m_min_72h = frollapply(t28m,n=72,FUN=min,align = 'right')
# )]
# vec_time_train <- train$time
# pdat <- pdat[,`:=`(in_train = time%in%vec_time_train)][
#   ,`:=`(in_train = if_else(is.na(t28m)==F, in_train, F))
# ]
# 
# 
# library(cols4all)
# sel_year <- 2011:2013
# pdat[year%in%sel_year] %>% 
#   select(., time,in_train, starts_with(c("obs_t28m_","pred_t28m_"))) %>% 
#   pivot_longer(-c("time","in_train")) %>% 
#   mutate(stat =
#            case_when(str_detect(name,'max')~'max',
#                      str_detect(name,'mean')~'mean',
#                      str_detect(name,'min')~'min')) %>% 
#   mutate(obs = case_when(
#     str_detect(name,'obs_t28m_')~'observation',
#     str_detect(name,'pred_t28m_')~'prediction',
#   )) %>% 
#   ggplot(data=.,aes(time, value,
#                     color=obs,
#                     group=paste(stat,obs)))+
#   geom_rect(aes(xmin=time,xmax=time+hours(1),
#                 ymin=20,ymax=35,
#                 fill=in_train),
#             color='transparent')+
#   geom_line(color='black',
#             lwd=1)+
#   geom_line()+
#   labs(x=NULL,y='7-day min/max/mean Tair 28m (°C)',
#        fill='Period\nIn Training\nData',
#        color='Type')+
#   scale_color_discrete_c4a_cat(palette = 'okabe',reverse = T)+
#   scale_fill_manual(values=c('grey80','grey20'))+
#   coord_cartesian(expand=F)+
#   theme_linedraw()+
#   theme()
# ggsave(filename = "figures/compare_t28_drift.png",
#        width=25,
#        height=10,
#        units='cm',
#        dpi=300)
# 
# 
# sel_year <- 2019:2020
# pdat[year==sel_year] %>% 
#   select(., time,in_train, starts_with(c("obs_t28m_","pred_t28m_"))) %>% 
#   pivot_longer(-c("time","in_train")) %>% 
#   mutate(obs = str_detect(name, "obs_")) %>% 
#   ggplot(data=.,aes(time, value,
#                     color=obs,
#                     group=name))+
#   geom_rect(aes(xmin=time,xmax=time+hours(1),
#                 ymin=20,ymax=35,
#                 fill=in_train), 
#             color='transparent')+
#   geom_line()+
#   labs(x=NULL,y='Tair 28m (°C)')+
#   scale_color_discrete_c4a_cat(palette = 'okabe')+
#   scale_fill_manual(values=c('grey80','grey20'))+
#   coord_cartesian(expand=F)+
#   theme_linedraw()+
#   theme()
# 
# sel_year <- 2015:2016
# pdat[year%in%sel_year] %>% 
#   select(., time,in_train, starts_with(c("obs_t28m_","pred_t28m_"))) %>% 
#   pivot_longer(-c("time","in_train")) %>% 
#   mutate(obs = str_detect(name, "obs_")) %>% 
#   ggplot(data=.,aes(time, value,
#                     color=obs,
#                     group=name))+
#   geom_rect(aes(xmin=time,xmax=time+minutes(61),
#                 ymin=20,ymax=35,
#                 fill=in_train), 
#             color='transparent')+
#   geom_line()+
#   labs(x=NULL,y='Tair 28m (°C)')+
#   scale_color_discrete_c4a_cat(palette = 'okabe')+
#   scale_fill_manual(values=c('grey80','grey20'))+
#   coord_cartesian(expand=F)+
#   theme_linedraw()+
#   theme()
# 
# 
# dat[,.(nobs = sum(is.na(t28m)==F)),by=year][order(nobs)]
# dat[,.(val = unique(time) %>% length),by=year]
# 
# 
# tmp1 <- test[order(time)][,`:=`(
#   test_t28m_max_72h = frollapply(t28m,n=72,FUN=max,align = 'right'),
#   test_t28m_mean_72h = frollmean(t28m,n=72,align = 'right',na.rm = F),
#   test_t28m_min_72h = frollapply(t28m,n=72,FUN=min,align = 'right')
# )][year==sel_year] %>% 
#   select(., time, starts_with(c("test_t28m_"))) %>% 
#   pivot_longer(-time)
# tmp2 <- train[order(time)][,`:=`(
#   train_t28m_max_72h = frollapply(t28m,n=72,FUN=max,align = 'right'),
#   train_t28m_mean_72h = frollmean(t28m,n=72,align = 'right',na.rm = F),
#   train_t28m_min_72h = frollapply(t28m,n=72,FUN=min,align = 'right')
# )][year==sel_year] %>% 
#   select(., time, starts_with(c("train_t28m_"))) %>% 
#   pivot_longer(-time)
# 
# library(cols4all)
# pdat[year==sel_year] %>% 
#   select(time, starts_with(c("obs_"))) %>% 
#   pivot_longer(-time) %>% 
#   bind_rows(., tmp1,tmp2) %>% 
#   mutate(var_test = str_detect(name,'test')) %>% 
#   mutate(var_train = str_detect(name,'train')) %>% 
#   mutate(var_obs = str_detect(name,'obs')) %>% 
#   filter(str_detect(name,"mean")) %>% 
#   ggplot(data=.,aes(time, value,
#                     color=var_obs))+
#   # geom_point(col='black',size=0.9)+
#   # geom_point(size=0.7,alpha=0.5)+
#   geom_line()+
#   scale_color_discrete_c4a_cat(palette = 'okabe')
# 
# 
# 
# 
# sel_year <- 2008
# tmp1 <- test[order(time)][,`:=`(
#   test_t28m_max_72h = frollapply(t28m,n=72,FUN=max,align = 'right',fill=NA),
#   test_t28m_mean_72h = frollmean(t28m,n=72,align = 'right',na.rm = F,fill=NA),
#   test_t28m_min_72h = frollapply(t28m,n=72,FUN=min,align = 'right',fill=NA)
# )][year==sel_year]
# tmp2 <- train[order(time)][,`:=`(
#   train_t28m_max_72h = frollapply(t28m,n=72,FUN=max,align = 'right',fill=NA),
#   train_t28m_mean_72h = frollmean(t28m,n=72,align = 'right',na.rm = F,fill=NA),
#   train_t28m_min_72h = frollapply(t28m,n=72,FUN=min,align = 'right',fill=NA)
# )][year==sel_year]
# 
# pdat[year==sel_year] %>% 
#   ggplot(data=.,aes(time, obs_t28m_mean_72h))+
#   geom_point(size=2)+
#   geom_point(size=0.5,
#     data=tmp2, inherit.aes = F,
#             aes(time, train_t28m_mean_72h),col='blue')+
#   geom_point(size=0.5,
#     data=tmp1, inherit.aes = F,
#             aes(time, test_t28m_mean_72h),col='#cf0000')
# pdat[year==sel_year] %>% 
#   ggplot(data=.,aes(time, obs_t28m_max_72h))+
#   geom_point(size=2)+
#   geom_point(size=0.5,
#              data=tmp2, inherit.aes = F,
#              aes(time, train_t28m_max_72h),col='blue')+
#   geom_point(size=0.5,
#              data=tmp1, inherit.aes = F,
#              aes(time, test_t28m_max_72h),col='#cf0000')
# pdat[year==sel_year] %>% 
#   ggplot(data=.,aes(time, obs_t28m_min_72h))+
#   geom_point(size=2)+
#   geom_point(size=0.5,
#              data=tmp2, inherit.aes = F,
#              aes(time, train_t28m_min_72h),col='blue')+
#   geom_point(size=0.5,
#              data=tmp1, inherit.aes = F,
#              aes(time, test_t28m_min_72h),col='#cf0000')
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# h2o.explain(best_aml, test_t28m)
# 
# # GBM hyperparameters
# gbm_params1 <- list(learn_rate = c(0.01, 0.1),
#                     max_depth = c(3, 5, 9),
#                     sample_rate = c(0.8, 1.0),
#                     col_sample_rate = c(0.2, 0.5, 1.0))
# 
# # Train and validate a cartesian grid of GBMs
# gbm_grid1 <- h2o.grid("gbm", 
#   x=c(names(train)[str_detect(names(train),"e5")],"month","hour","ddate"),
#                   y='t28m',
#                       grid_id = "gbm_grid1",
#                       training_frame = train_t28m,
#                       validation_frame = test_t28m,
#                       ntrees = 100,
#                       seed = 1,
#                       hyper_params = gbm_params1)
# 
# # Get the grid results, sorted by validation AUC
# gbm_gridperf1 <- h2o.getGrid(grid_id = "gbm_grid1",
#                              # sort_by = "RMSE",
#                              decreasing = TRUE)
# print(gbm_gridperf1)
# best_gbm1 <- h2o.getModel(gbm_gridperf1@model_ids[[1]])
# 
# 
# h1 <- h2o.xgboost(x=c(names(train)[str_detect(names(train),"e5")],"month","hour","ddate"),
#                   y='t28m',
#                   training_frame = train_t28m, 
#                   seed=3)
# h1
# h2o.performance(h1,newdata = test_t28m)@metrics$r2
# h2o.varimp(h1)
# 
# 
# 
# h2 <- h2o.xgboost(x=c(names(train)[str_detect(names(train),"e5")],"month","hour"),
#                   y='t28m',
#                   training_frame = train_t28m, 
#                   seed=3)
# h2o.performance(h2,newdata = test_t28m)@metrics$r2
# h2o.varimp(h2)
# 
# 
# a1 <- h2o.automl(x=c(names(train)[str_detect(names(train),"e5")],"month","hour","ddate"),
#       y='t28m',
#       training_frame = train_t28m, 
#       include_algos = c("XGBoost"),
#       # max_models = 2,
#       max_runtime_secs = 200,
#       seed=3)
# h2o.get_leaderboard(a1)
# a1@leaderboard
# best_aml <- a1@leader
# # best_aml <- h2o.getModel(gbm_gridperf1@model_ids[[1]])
# h2o.performance(best_aml, newdata=test_t28m)@metrics
# 
# 
# # h2o.shap_summary_plot(h1, top_n_features = 10, 
# #                       newdata = test[is.na(t28m)==F][sample(.N,1000)] %>% as.h2o())
# 
# f_t_2 <- bam(t28m~s(hour, bs='cc')+s(month, bs='cc')+
#                te(var_2t, var_2d, ssrd)+
#                s(swvl1, swvl4),
#              discrete=T, 
#              method='fREML',
#              select=T, 
#              nthreads = 10, 
#              data=train)
# f_t_3 <- bam(t28m~s(hour, bs='cc')+s(month, bs='cc')+
#                te(var_2t, var_2d, ssrd)+
#                s(swvl1)+s(swvl4),
#              discrete=T, 
#              method='fREML',
#              select=T, 
#              nthreads = 10, 
#              # cluster = cl, # not for use with bam
#              data=train)
# f_t_4 <- bam(t28m~s(hour,month)+
#                te(var_2t, var_2d, ssrd)+
#                s(swvl1)+s(swvl4),
#              discrete=T, 
#              method='fREML',
#              select=T, 
#              nthreads = 10, 
#              # cluster = cl, # not for use with bam
#              data=train)
# f_t_5 <- bam(t28m~
#                s(hour,month)+
#                te(var_2t, var_2d)+
#                s(ssrd)+
#                s(swvl1)+
#                s(swvl4),
#              discrete=T,method='fREML',
#              select=T,nthreads = 10, data=train)
# f_t_6 <- bam(t28m~
#                s(hour,month)+
#                te(var_2t, var_2d, ssrd)+
#                s(swvl4, swvl1),
#              discrete=T,method='fREML',
#              select=T,nthreads = 10, data=train)
# 
# 
# 
# # SCRATCH  ----------------------------------------------------------------
# 
# 
# gpm$date %>% min
# e5$time %>% min
# 
# gpm$time[1:4]
# 
# e5[sample(.N,1000)] %>% 
#   mutate(hour = hour(date)-3) %>% 
#   ggplot(data=.,aes(hour, temperature_2m))+
#   geom_point()
# 
# gpm[sample(.N,1000)] %>% 
#   mutate(hour = hour(date)) %>% 
#   ggplot(data=.,aes(hour,precipitationCal,group=hour))+
#   geom_violin()
# 
# obs[sample(.N,1000)] %>% ggplot(aes(hour,rad_global))+
#   geom_point()+
#   geom_vline(aes(xintercept=12))
