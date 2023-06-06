# fit_met-scaling_models_daily
# descrip: Fit models to observations from CAX tower with 
# covariates derived from ERA5-Land, GPM, CHIRPS
# author: Sami Rifai
# date: 2023-06 (ongoing)
# notes: Daily min/mean/max of each met variable
# target variables: 
#     t28m  - prototyped
## to do:
#     rh28m 
#     vpd28m (from t28m + rh28m)
#     rad_global (srad?)
#     precip
#     vv (wind speed)
#     vv_max (gust speed)

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

## check no infinite vals
testthat::expect_false(
  all(train$t28m %>% is.infinite())
)




# SPINUP H2O CLUSTER ==============================================
h2o.init(
  max_mem_size = "128g" # change mem size as needed
) 

covar_names <- c(names(train)[str_detect(names(train),"e5")],
                 names(train)[str_detect(names(train),"g_")],
                 names(train)[str_detect(names(train),"c_")],
                 "month","stat")


# t28m ========================

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





# Generate full predicted time series ============================
tmp_dmin <- full_dat %>% select(-c("hour","year","time")) %>% 
  .[, lapply(.SD, min, na.rm=T), by=.(date,month)] %>% 
  .[,`:=`(stat = "min")]
tmp_dmean <- full_dat %>% select(-c("hour","year","time")) %>% 
  .[, lapply(.SD, mean, na.rm=T), by=.(date,month)] %>% 
  .[,`:=`(stat = "mean")]
tmp_dmax <- full_dat %>% select(-c("hour","year","time")) %>% 
  .[, lapply(.SD, max, na.rm=T), by=.(date,month)] %>% 
  .[,`:=`(stat = "max")]
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
  fwrite(., file=paste0("outputs/cax_met-pred-obs_t28m_2001_2022_proc",Sys.Date(),".csv"))


# Plot evaluation ================================
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


## problematic monthly spike in t28m 
## Unclear. Something could be wrong with reducing the hourly to daily
## tmax_spike is low, and tmin_spike is high... 
out[date>=ymd("2012-01-01")][date<=ymd("2012-12-31")] %>% 
  ggplot(aes(date,pred_t28m,color=stat))+
  geom_line()+
  theme_linedraw()
ggsave(filename = "figures/monthlySpike_pred_t28m.png")


