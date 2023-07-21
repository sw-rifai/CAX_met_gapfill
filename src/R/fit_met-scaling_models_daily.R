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

#     soil:
#     vwc
#     wp

## to do:
#     vpd28m (from t28m + rh28m)

remove(list = ls())

pacman::p_load(h2o, data.table, tidyverse, lubridate, feather, units,
               scico,bigleaf, mgcv, gratia, cols4all)

### Functions ####

# Function to perform and save predictions for a given variable present in the data
predictMet <- function(cdat = NULL,
                       variable = NULL, 
                       covar_names = NULL, 
                       frac_train = 0.9, # fraction of data used for training 
                       # n_covars <- 20, # number of covariates used for training
                       n_models = 10, # number models fit in autoML
                       outputFile = NULL,
                       performanceFile = NULL,
                       evaluationPlotFile = NULL, 
                       timeSeriesPlotFile = NULL){
  
  if(is.null(variable) | is.null(covar_names) | is.null(covar_names)){
    stop("assign variable (character 1)")
  }
  
  if(is.null(covar_names)){
    stop("assign covar_names (character n)")
  }
  
  if(is.null(cdat)){
    stop("assign ddat (data.table)")
  }
  
  # results list
  results <- list()
  
  # design name for predicted variable
  pred_variable <- paste0("pred_", variable)
  
  ## Data preparation ####

    # Summarize to daily min/mean/max
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
  
  # Recode Inf with NAs
  ddat[sapply(ddat, is.infinite)] <- NA
  
  # 
  tmp_dmin <- cdat %>% select(-c("hour","year","time")) %>% 
    .[, lapply(.SD, min, na.rm=T), by=.(date,month)] %>% 
    .[,`:=`(stat = "min")]
  tmp_dmean <- cdat %>% select(-c("hour","year","time")) %>% 
    .[, lapply(.SD, mean, na.rm=T), by=.(date,month)] %>% 
    .[,`:=`(stat = "mean")]
  tmp_dmax <- cdat %>% select(-c("hour","year","time")) %>% 
    .[, lapply(.SD, max, na.rm=T), by=.(date,month)] %>% 
    .[,`:=`(stat = "max")]
  
  # Splice data for mod fitting ###
  set.seed(3)
  ddat[,`:=`(ym = paste0(year(date),"_",month(date)))] %>% 
    .[,`:=`(year_cat = year(date) %>% as.factor())] %>% 
    .[,`:=`(month = factor(month))]
  vec_ym <- ddat$ym %>% unique %>% sort
  vec_train <- sample(vec_ym, floor(length(vec_ym)*frac_train)) # high fraction for training
  vec_test <- vec_ym[!vec_ym %in% vec_train]
  train <- ddat[ym %in% vec_train][is.na(variable)==F]
  tmp <- fsetdiff(ddat, train)[is.na(variable)==F]
  test <- tmp[sample(.N, floor(.N*0.5))][is.na(variable)==F]
  valid <- fsetdiff(tmp,test)[is.na(variable)==F] # questionable if validation frame is needed
  train <- ddat[ym %in% vec_train][is.na(variable)==F]
  tmp <- fsetdiff(ddat, train)[is.na(variable)==F]
  test <- tmp[sample(.N, floor(.N*0.5))][is.na(variable)==F]
  valid <- fsetdiff(tmp,test)[is.na(variable)==F] # questionable if validation frame is needed
  
  rm(tmp); gc();
  
  # check no infinite vals
  testthat::expect_false(
    any(as.vector(train[, ..variable])[[1]] %>% is.infinite())
  )
  
  # Model
  
  m <- h2o.automl(x=covar_names,
                       y=variable,
                       training_frame = as.h2o(train[is.na(variable)==F]), 
                       include_algos = c("GBM"),
                       max_models = n_models,
                       # max_runtime_secs = 1500,
                       seed=3)
  
  # Model performance
  
  performance <- h2o.performance(m@leader,
                               newdata=as.h2o(test[is.na(variable)==F]))
  
  results[[paste0("performance_", variable)]] <- data.frame("variable" = variable,
                                                            "predictors" = paste0(covar_names, collapse = ", "),
                                                            "Nobs" = performance@metrics[["nobs"]],
                                                            "R2" = performance@metrics[["r2"]],
                                                            "MSE" = performance@metrics[["MSE"]],
                                                            "RMSE" = performance@metrics[["RMSE"]],
                                                            "mean_residual_deviance" = performance@metrics[["mean_residual_deviance"]],
                                                            "mae" = performance@metrics[["mae"]],
                                                            "rmsle" = performance@metrics[["rmsle"]]
  )
  
  if(!is.null(performanceFile)){
    fwrite(results[[paste0("performance_", variable)]], file = performanceFile) 
  }
  
  # Generate full predicted time series
  
  pdat <- rbind(tmp_dmin,tmp_dmean,tmp_dmax) %>% 
    select(all_of(covar_names), all_of(variable), date) %>% 
    .[,`:=`(month = factor(month))]
  
  pred_t28m <- h2o.predict(m, 
                           newdata=as.h2o(pdat)) %>% 
    as.data.table() %>% 
    set_names(pred_variable)
  
  out <- cbind(pdat,pred_t28m) %>% 
    select(date, stat, all_of(c(pred_variable, variable)))
  
  results[[paste0("prediction_", variable)]] <- out
  
  ## write to disc
  if(!is.null(outputFile)){
    fwrite(out, file = outputFile)
  }
  
  # Plot evaluation
  
  ## just from train and validation
  pdat <- h2o.predict(m, newdata=as.h2o(rbind(train,valid))) %>% 
    as.data.table() %>% 
    set_names(pred_variable)
  
  plot.data <- as.data.frame(cbind(rbind(train,valid),pdat))
  
  results[[paste0("evaluation_plot_", variable)]] <-  plot.data %>%
    ggplot(aes(plot.data[, pred_variable], plot.data[, variable], color=stat))+
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
    labs(x= paste0("predicted ", variable),
         y= paste0("observed ", variable),
         color= variable) +
    scale_color_viridis_d(option='H')+
    coord_equal()+
    facet_wrap(~ year_cat,ncol = 7) +
    theme_linedraw()
  
  plot(results[[paste0("evaluation_plot_", variable)]] )
  
  if(!is.null(evaluationPlotFile)){
    ggsave(filename = evaluationPlotFile,
           width=25,
           height=12,
           units='cm',
           dpi=350, 
           scale = 1.25)
  }
  
  # Predicted time series plot
  
  tsPlot.data <- as.data.frame(out)
  results[[paste0("predicted_time_series_plot_", variable)]]  <- ggplot(tsPlot.data, aes(x = date, 
                                                                              y = tsPlot.data[, pred_variable], 
                                                                              color = stat)) +
    geom_line(alpha = 0.8) +
    labs(y= paste0("Predicted ", variable)) +
    theme_linedraw()
  
  plot(results[[paste0("evaluation_plot_", variable)]])
  
  if(!is.null(timeSeriesPlotFile)){
    ggsave(filename = timeSeriesPlotFile)
  }
  
  return(results)
}

# Function to perform gap filling
gapFillData <- function(data = st_pred.df, variablesToGapFill = "precip_mean"){
  
  data <- as.data.frame(data)

    for(var in variablesToGapFill){
    pred_var <- paste0("pred_", var)
    
    # imput missing values with predicted values
    data[is.na(data[, var]), var] <- data[is.na(data[, var]), pred_var]
    
    # Delete pred variable
    data[, pred_var] <- NULL
  }
  return(data)
}


## Zoom in if needed
# out[date>=ymd("2012-01-01")][date<=ymd("2012-12-31")] %>% 
#   ggplot(aes(date,pred_t28m,color=stat))+
#   geom_line()+
#   theme_linedraw()
# ggsave(filename = "figures/monthlySpike_pred_t28m.png")


# Import cleaned met obs and ERA5 & GPM data ===============================
source("src/R/import_cleaned_data.R")

## only working on CAX south for now
full_dat <- dat[site=="cax_south"] %>% 
       select(-site, -ddate) %>%
  mutate(month = month(date))    # seems like some months were not truly representing the date month, so I redefine month here

## cdat: 24 obs per day (I do not think this step is needed and it may give some problems as it constrain other variables to t28m data availability...)
# vec_cdat <- full_dat[,.(nobs = sum(is.na(t28m)==F)),by=date][nobs==24]$date
# cdat <- full_dat[date %in% vec_cdat]
cdat <- full_dat # (I do not think the previous two lines are needed and it may give some problems as it constrain other variables to t28m data availability...)

# SPINUP H2O CLUSTER ==============================================
h2o.init(
  max_mem_size = "128g" # change mem size as needed
) 

### variables to predict
names(cdat)
variables <- c("temperature_28m_C", "relative_humidity_28m_perc", "vapor_pressure_deficit_28m_mbar", "precipitation_mm", 
               "radiation_global_W_m2", "wind_speed_m_s", "wind_speed_maximum_m_s", 
               "soil_volumetric_water_content_250cm", "soil_water_potential_250cm_MPa")

# round 1 (using remote sensed variables to predict) =========================================================================

covar_names <- c(names(cdat)[str_detect(names(cdat),"e5")],
                 names(cdat)[str_detect(names(cdat),"g_")],
                 names(cdat)[str_detect(names(cdat),"c_p")],
                 "month","stat")

gapFilled.list <- list()

for(variable in variables){
  print(variable)
  ## Predictions
  
  variable_prediction <- predictMet(cdat = cdat,
                                variable = variable,
                                covar_names = covar_names,
                                performanceFile = paste0("outputs/performance_", variable, "_2001_2022_daily.csv"), 
                                outputFile = paste0("outputs/cax_met-pred-obs_", variable, "_2001_2022_daily_proc.csv"), 
                                evaluationPlotFile = paste0("figures/", variable, "_daily_pred_vs_obs.png"), 
                                timeSeriesPlotFile = paste0("figures/", variable, "_daily_pred_time_series.png"))

  pred.df <- variable_prediction[[paste0("prediction_", variable)]]
  
  ## Gap filling
  
  # from long to wide (stat)
  statistics <- unique(pred.df$stat)
  st_pred.list <- list()
  for(st in statistics){
    
    st_pred.df <- pred.df %>%
      filter(stat == st) %>%
      select(-stat)
    
    names(st_pred.df)[-1] <- paste0(names(st_pred.df)[-1], "_", st)
    
    st_pred.list[[st]] <- st_pred.df
  }
  
  st_pred.df <- st_pred.list %>%     
    reduce(merge, by = "date")
  

  met.variables <- paste0(variable, "_", statistics)
  
  # gap fill and store
  gapFilled.list[[variable]] <- gapFillData(data = st_pred.df, variablesToGapFill = met.variables)
}

gapFilled.df <- gapFilled.list %>%     
  reduce(merge, by = "date")
fwrite(gapFilled.df, file= "products/cax_met-gap_filled_2001_2022_daily.csv")

# round 2 (using previously predicted variables) ===============================

gapFilled.df <- read_csv("products/cax_met-gap_filled_2001_2022_daily.csv")

gapFilled2.list <- list()
for(variable in variables){
  print(variable)
  
  covar_names <- variables[!variables %in% variable]
  covar_names_st <- c(paste0(covar_names, "_min"),
                      paste0(covar_names, "_mean"),
                      paste0(covar_names, "_max"))

  cdat2 <- cdat %>%
    select(-all_of(covar_names))

  cdat2 <- merge(cdat2,
                 gapFilled.df[, c("date", covar_names_st)],
                 by = "date",
                 all.x = T)

  covar_names_st <- c(covar_names_st, "month","stat")
  
  ## Predictions
  
  variable_prediction <- predictMet(cdat = cdat2,
                                    variable = variable,
                                    covar_names = covar_names_st,
                                    performanceFile = paste0("outputs/performance_", variable, "_2001_2022_daily_round2.csv"), 
                                    outputFile = paste0("outputs/cax_met-pred-obs_", variable, "_2001_2022_daily_proc_round2.csv"), 
                                    evaluationPlotFile = paste0("figures/", variable, "_daily_pred_vs_obs_round2.png"), 
                                    timeSeriesPlotFile = paste0("figures/", variable, "_daily_pred_time_series_round2.png"))
  
  pred.df <- variable_prediction[[paste0("prediction_", variable)]]
  
  ## Gap filling
  
  # from long to wide (stat)
  statistics <- unique(pred.df$stat)
  st_pred.list <- list()
  for(st in statistics){
    
    st_pred.df <- pred.df %>%
      filter(stat == st) %>%
      select(-stat)
    
    names(st_pred.df)[-1] <- paste0(names(st_pred.df)[-1], "_", st)
    
    st_pred.list[[st]] <- st_pred.df
  }
  
  st_pred.df <- st_pred.list %>%     
    reduce(merge, by = "date")
  
  
  met.variables <- paste0(variable, "_", statistics)
  
  # gap fill and store
  gapFilled2.list[[variable]] <- gapFillData(data = st_pred.df, variablesToGapFill = met.variables)
}

gapFilled2.df <- gapFilled2.list %>%     
  reduce(merge, by = "date")
fwrite(gapFilled2.df, file=paste0("products/cax_met-gap_filled_2001_2022_daily_round2.csv"))


# Predictive pperformance ===============================

performance.files <- list.files("outputs/", 
                                pattern = c("performance", ".csv"), 
                                full.names = T)



performances.df <- lapply(performance.files, 
                           read.csv) %>% 
  list_rbind()
fwrite(performances.df, file=paste0("products/predictive_performances.csv"))
