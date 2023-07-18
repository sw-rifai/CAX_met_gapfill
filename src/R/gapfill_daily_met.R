# gapfill_daily_met
# descrip: gap fill daily meteorological data 
# author: Pablo Sanchez-Martinez
# date: 2023-07 (ongoing)
# notes: Daily min/mean/max of each met variable
# target variables: 
#     t28m  - prototyped
#     rh28m - prototyped
#     rad_global - prototyped
#     precip - prototyped
#     vv (wind speed) - prototyped
#     vv_max (gust speed) - prototyped
#     vpd28m (predicted directly) - prototyped

remove(list = ls())

# Packages =============================

library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)

# Functions =============================

gapFillData <- function(data = st_pred.df, variablesToGapFill = "precip_mean"){
  
  for(var in variablesToGapFill){
    pred_var <- paste0("pred_", var)
    
    # imput missing values with predicted values
    data[is.na(data[, var]), var] <- data[is.na(data[, var]), pred_var]
    
    # Delete pred variable
    data[, pred_var] <- NULL
  }
  return(data)
}

# Load and unify daily predicted met data =============================

pred.files <- list.files("outputs/", pattern = c("daily", ".csv"), full.names = T)
pred.list <- lapply(pred.files, 
                        read.csv)

pred.df <- pred.list %>% 
  reduce(inner_join, by = c("date", "stat"))
pred.df

# Recode Inf with NAs
pred.df[sapply(pred.df, is.infinite)] <- NA

# include daily statistics (mean, min and max) as columns

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

# Write unified obs-pred data as a product

fwrite(st_pred.df, file=paste0("products/cax_met-pred-obs_2001_2022_daily.csv"))


# Gapfill missing data of variables with predicted values =============================

mean_met.variables <- paste0(c("precip", "rad_global", "rh28m", "t28m", "vpd28m", "vv", "vv_max"), c("_mean"))
min_met.variables <- paste0(c("precip", "rad_global", "rh28m", "t28m", "vpd28m", "vv", "vv_max"), c("_min"))
max_met.variables <- paste0(c("precip", "rad_global", "rh28m", "t28m", "vpd28m", "vv", "vv_max"), c("_max"))

met.variables <- c(mean_met.variables, max_met.variables, min_met.variables)

gapFilled.df <- gapFillData(data = st_pred.df, variablesToGapFill = met.variables)

fwrite(gapFilled.df, file=paste0("products/cax_met-gap_filled_2001_2022_daily.csv"))
