library(tidyverse); library(lubridate); library(stringr)
library(stars)

to_cat <- list.files(path="/home/sami/Downloads/scratch_v2/", 
                     pattern='ssrd', full.names = T)
library(foreach); library(doParallel)
library(doMC)
parallel::detectCores()
# cl <- makeCluster(10)
# registerDoParallel(cl, cores = 10)
# getDoParWorkers()

cl <- makePSOCKcluster(10); 
registerDoParallel(cl,cores = 10) # Register parallel backend for foreach
numCores <- getDoParWorkers(); print(numCores)
getDoParRegistered()
doMC::registerDoMC(cores = 10)
system.time(
  o <- foreach(i = 1:length(to_cat), .combine = rbind) %dopar% {
    tidync::tidync(to_cat[i]) %>% 
      tidync::hyper_tibble() %>% 
      rename(lon=longitude, 
             lat = latitude) %>% 
      mutate(lon=lon-360) %>% 
      filter(near(lon,-51.5, tol = 0.1) & near(lat,-1.75, tol = 0.1)) %>% 
      mutate(idx = row_number()) %>% 
      mutate(date = ymd_hm('1900-01-01 00:00')+hours(forecast_initial_time)+hours(forecast_hour))
  }
) # do: 76 seconds # dopar:17
doParallel::stopImplicitCluster(cl)

o %>% 
  rename(ssrd = SSRD) %>% 
  mutate(date = date-hours(3)) %>% 
  feather::write_feather(x = ., path="data/era5_cax_ssrd_1980_2018.feather")



# Longwave radiation ------------------------------------------------------
to_cat <- list.files(path="/home/sami/Downloads/scratch_v2/", 
                     full.names = F)
str_extract(to_cat, "(?<=\\.)[^.]+") %>% unique
to_cat <- list.files(path="/home/sami/Downloads/scratch_v2/",
                     pattern='STRD',
                     full.names = T)
to_cat[1]
library(foreach); library(doParallel)
library(doMC)
parallel::detectCores()
# cl <- makeCluster(10)
# registerDoParallel(cl, cores = 10)
# getDoParWorkers()

cl <- makePSOCKcluster(10); 
registerDoParallel(cl,cores = 10) # Register parallel backend for foreach
numCores <- getDoParWorkers(); print(numCores)
getDoParRegistered()
doMC::registerDoMC(cores = 10)
system.time(
  o <- foreach(i = 1:length(to_cat), .combine = rbind) %dopar% {
    tidync::tidync(to_cat[i]) %>% 
      tidync::hyper_tibble() %>% 
      rename(lon=longitude, 
             lat = latitude) %>% 
      mutate(lon=lon-360) %>% 
      filter(near(lon,-51.5, tol = 0.1) & near(lat,-1.75, tol = 0.1)) %>% 
      mutate(idx = row_number()) %>% 
      mutate(date = ymd_hm('1900-01-01 00:00')+hours(forecast_initial_time)+hours(forecast_hour))
  }
) # do: 76 seconds # dopar:17
doParallel::stopImplicitCluster()

o %>% 
  rename(strd = STRD) %>% 
  mutate(date = date-hours(3)) %>% 
  feather::write_feather(x = ., path="data/era5_cax_strd_1980_2018.feather")



o %>% 
  rename(ssrd = SSRD) %>% 
  mutate(date = date-hours(3)) %>% 
  mutate(year=year(date)) %>% 
  filter(year==2015) %>% 
  mutate(hour = hour(date)) %>% 
  ggplot(data=., aes(hour, ssrd/60**2, group=hour))+
  geom_point(alpha=0.1)+
  geom_smooth(inherit.aes = F, aes(hour, ssrd/60**2))

o %>% 
  rename(ssrd = SSRD) %>% 
  mutate(date = date-hours(3)) %>% 
  mutate(year=year(date), 
         month=month(date)) %>% 
  mutate(hour = hour(date)) %>% 
  group_by(year,month) %>% 
  summarize(val = mean(ssrd)/60**2) %>% 
  ungroup() %>% 
  mutate(date = ymd(paste(year,month,1))) %>% 
  filter(year>=1980 & year<= 2018) %>% 
  ggplot(data=., aes(date, val))+
  geom_line()+
  geom_smooth(method='loess', span=0.1)

# 
# 
# 
# 
# scratch_dir <- '/home/sami/Downloads/scratch_v2'
# merged_d <- "/home/sami/Downloads/scratch_v2/merged/"
# tmp_list <- list.files(scratch_dir)
# tmp_list <- tmp_list[str_detect(tmp_list, 'SSRD')]
# 
# x1 <- stars::read_ncdf(file.path(scratch_dir, tmp_list[1]))
# x2 <- stars::read_ncdf(file.path(scratch_dir, tmp_list[2]))
# x3 <- stars::read_ncdf(file.path(scratch_dir, tmp_list[3]))
# x4 <- stars::read_ncdf(file.path(scratch_dir, tmp_list[4]))
# 
# dim(x1); 
# dim(x2)
# dim(x3)
# dim(x4)
# c(x1,x2, along="forecast_initial_time")
# 
# 
# 
# 
# junk <- stars::read_ncdf("/home/sami/Downloads/scratch_v2/merged/merged_CP_.nc") %>% 
#   as_tibble()
# junk$forecast_initial_time %>% last
# 
# 
# 
# for(i in 1:length(var_list)){
#   to_cat <- tmp_list[str_detect(tmp_list, var_list[i])]
#   to_cat <- to_cat[str_detect(to_cat, ".tar")==F]
#   to_cat <- file.path(scratch_dir, to_cat)  
#   to_cat <- sort(to_cat)
#   system(paste('cdo cat ',
#                paste(to_cat, collapse = ' '),
#                file.path(merged_d, paste0("merged_",var_list[i],"_.nc"))))
#   print(paste(i, var_list[i]))
# }
# list.files(merged_d)
# 
# to_cat[1:2]
# 
# system(paste('ncdump -h',to_cat[1]))
# system(paste('cdo sinfo',to_cat[1]))
# 
# paste0('cdo cat ',paste(to_cat[1:2], collapse = ' '),
#        ' ',
#        file.path(merged_d, paste0("merged_",var_list[i],"_.nc")))
# 
# system(paste0('cdo -O mergetime ',paste(to_cat[1:2], collapse = ' '),
#               ' ',
#               file.path(merged_d, paste0("merged_",var_list[i],"_.nc"))))
# 
