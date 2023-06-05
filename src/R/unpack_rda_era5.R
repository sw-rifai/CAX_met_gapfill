library(tidyverse); library(lubridate); library(stringr)
library(stars)
# part 1 (instantaenous vars) -----------------------------------------------------------
scratch_dir <- "/home/sami/Downloads/scratch/"
file_list <- list.files("/home/sami/Downloads/", pattern = 'sfc', 
                        full.names = T)
file_list <- file_list[str_detect(file_list, '.tar')]


for(i in 1:length(file_list)){
  untar(file_list[i], exdir = scratch_dir)
}
name_list <- list.files(scratch_dir)

# Extract met var names from file name
var_list <- str_extract(name_list, "(?<=\\.)[^.]+") %>% unique
var_list <- var_list[is.na(var_list)!=T]


merged_d <- "/home/sami/Downloads/scratch/merged/"

tmp_list <- list.files(scratch_dir, pattern = '.nc')

library(foreach); library(doParallel)
library(doMC)
parallel::detectCores()

cl <- makePSOCKcluster(10); 
registerDoParallel(cl,cores = 10) # Register parallel backend for foreach
numCores <- getDoParWorkers(); print(numCores)
getDoParRegistered()
doMC::registerDoMC(cores = 10)

foreach(i=1:length(var_list)) %dopar% {
  to_cat <- tmp_list[str_detect(tmp_list, var_list[i])]
  to_cat <- file.path(scratch_dir, to_cat)  
  system(paste('cdo cat ',
               paste(to_cat, collapse = ' '),
               file.path(merged_d, paste0("merged_",var_list[i],"_.nc"))))
  print(paste(i, var_list[i]))
}


# air temperature
o <- foreach(i=1:length(list.files(merged_d, pattern = 'VAR_2T', full.names = T)), 
        .combine = bind_rows) %dopar% 
  {
      tidync::tidync(list.files(merged_d, pattern = 'VAR_2T', full.names = T)) %>% 
        tidync::hyper_tibble() %>% 
        rename(lon=longitude, 
               lat = latitude) %>% 
        mutate(lon=lon-360) %>% 
        filter(near(lon,-51.5, tol = 0.1) & near(lat,-1.75, tol = 0.1)) %>% 
        mutate(idx = row_number()) %>% 
        mutate(date = ymd_hm('1900-01-01 00:00')+hours(time)-hours(3))  
  }
o %>% 
  feather::write_feather(., path='era5_cax_t2m_1980_2018.feather')

# dew point temperature
list.files(merged_d)
o <- foreach(i=1:length(list.files(merged_d, pattern = 'VAR_2D', full.names = T)), 
             .combine = bind_rows) %dopar% 
  {
    tidync::tidync(list.files(merged_d, pattern = 'VAR_2D', full.names = T)) %>% 
      tidync::hyper_tibble() %>% 
      rename(lon=longitude, 
             lat = latitude) %>% 
      mutate(lon=lon-360) %>% 
      filter(near(lon,-51.5, tol = 0.1) & near(lat,-1.75, tol = 0.1)) %>% 
      mutate(idx = row_number()) %>% 
      mutate(date = ymd_hm('1900-01-01 00:00')+hours(time)-hours(3))  
  }
o %>% 
  feather::write_feather(., path='era5_cax_d2m_1980_2018.feather')

# wind speed
source_dir <- "/home/sami/Downloads/scratch/"
list.files(source_dir, pattern = 'VAR_10U')
list.files(source_dir, pattern = 'VAR_10V')

list.files('/home/sami/Downloads/scratch/merged/')
file.path('/home/sami/Downloads/scratch/merged','merged_VAR_10U_.nc') %>% 
  stars::read_ncdf(.)

cl <- makePSOCKcluster(10); registerDoParallel(cl,cores = 10) # Register parallel backend for foreach
numCores <- getDoParWorkers(); print(numCores); getDoParRegistered(); doMC::registerDoMC(cores = 10)
o <- foreach(i=1:length(list.files(source_dir, pattern = 'VAR_10U', full.names = T)), 
             .combine = bind_rows) %dopar% 
  {
    tidync::tidync(list.files(source_dir, pattern = 'VAR_10U', full.names = T)) %>% 
      tidync::hyper_tibble() %>% 
      rename(lon=longitude, 
             lat = latitude) %>% 
      mutate(lon=lon-360) %>% 
      filter(near(lon,-51.5, tol = 0.1) & near(lat,-1.75, tol = 0.1)) %>% 
      mutate(idx = row_number()) %>% 
      mutate(date = ymd_hm('1900-01-01 00:00')+hours(time)-hours(3))  
  }
o %>% 
  feather::write_feather(., path='era5_cax_u10_1980_2018.feather')

o <- foreach(i=1:length(list.files(source_dir, pattern = 'VAR_10V', full.names = T)), 
             .combine = bind_rows) %dopar% 
  {
    tidync::tidync(list.files(source_dir, pattern = 'VAR_10V', full.names = T)) %>% 
      tidync::hyper_tibble() %>% 
      rename(lon=longitude, 
             lat = latitude) %>% 
      mutate(lon=lon-360) %>% 
      filter(near(lon,-51.5, tol = 0.1) & near(lat,-1.75, tol = 0.1)) %>% 
      mutate(idx = row_number()) %>% 
      mutate(date = ymd_hm('1900-01-01 00:00')+hours(time)-hours(3))  
  }
o %>% 
  feather::write_feather(., path='era5_cax_v10_1980_2018.feather')

# surface pressure --------------------------------------------------------------
source_dir <- "/home/sami/Downloads/scratch/"
name_list <- list.files(source_dir)
name_list
# Extract met var names from file name
var_list <- str_extract(name_list, "(?<=\\.)[^.]+") %>% unique
var_list <- var_list[is.na(var_list)!=T]
list.files(source_dir, pattern='_sp')

list.files(merged_d)
cl <- makePSOCKcluster(10); registerDoParallel(cl,cores = 10) # Register parallel backend for foreach
numCores <- getDoParWorkers(); print(numCores); getDoParRegistered(); doMC::registerDoMC(cores = 10)
o <- foreach(i=1:length(list.files(source_dir, pattern = '_sp', full.names = T)), 
             .combine = bind_rows) %dopar% 
  {
    tidync::tidync(list.files(source_dir, pattern = '_sp', full.names = T)[i]) %>% 
      tidync::hyper_tibble() %>% 
      rename(lon=longitude, 
             lat = latitude) %>% 
      mutate(lon=lon-360) %>% 
      filter(near(lon,-51.5, tol = 0.1) & near(lat,-1.75, tol = 0.1)) %>% 
      mutate(idx = row_number()) %>% 
      mutate(date = ymd_hm('1900-01-01 00:00')+hours(time)-hours(3))  
  }
o %>% 
  feather::write_feather(., path='data/era5_cax_SP_1980_2018.feather')

# swvl1 --------------------------------------------------------------
source_dir <- "/home/sami/Downloads/scratch/"
name_list <- list.files(source_dir)
name_list
# Extract met var names from file name
var_list <- str_extract(name_list, "(?<=\\.)[^.]+") %>% unique
var_list <- var_list[is.na(var_list)!=T]
list.files(source_dir, pattern='_swvl1')

list.files(merged_d)
cl <- makePSOCKcluster(10); registerDoParallel(cl,cores = 10) # Register parallel backend for foreach
numCores <- getDoParWorkers(); print(numCores); getDoParRegistered(); doMC::registerDoMC(cores = 10)
o <- foreach(i=1:length(list.files(source_dir, pattern = '_swvl1', full.names = T)), 
             .combine = bind_rows) %dopar% 
  {
    tidync::tidync(list.files(source_dir, pattern = '_swvl1', full.names = T)[i]) %>% 
      tidync::hyper_tibble() %>% 
      rename(lon=longitude, 
             lat = latitude) %>% 
      mutate(lon=lon-360) %>% 
      filter(near(lon,-51.5, tol = 0.1) & near(lat,-1.75, tol = 0.1)) %>% 
      mutate(idx = row_number()) %>% 
      mutate(date = ymd_hms('1900-01-01 00:00:00')+hours(time)-hours(3))  
  }
o$date %>% summary
o %>% 
  feather::write_feather(., path='data/era5_cax_swvl1_1980_2018.feather')

# swvl2 --------------------------------------------------------------
source_dir <- "/home/sami/Downloads/scratch/"
name_list <- list.files(source_dir)
name_list
# Extract met var names from file name
var_list <- str_extract(name_list, "(?<=\\.)[^.]+") %>% unique
var_list <- var_list[is.na(var_list)!=T]
list.files(source_dir, pattern='_swvl2')

list.files(merged_d)
cl <- makePSOCKcluster(10); registerDoParallel(cl,cores = 10) # Register parallel backend for foreach
numCores <- getDoParWorkers(); print(numCores); getDoParRegistered(); doMC::registerDoMC(cores = 10)
o <- foreach(i=1:length(list.files(source_dir, pattern = '_swvl2', full.names = T)), 
             .combine = bind_rows) %dopar% 
  {
    tidync::tidync(list.files(source_dir, pattern = '_swvl2', full.names = T)[i]) %>% 
      tidync::hyper_tibble() %>% 
      rename(lon=longitude, 
             lat = latitude) %>% 
      mutate(lon=lon-360) %>% 
      filter(near(lon,-51.5, tol = 0.1) & near(lat,-1.75, tol = 0.1)) %>% 
      mutate(idx = row_number()) %>% 
      mutate(date = ymd_hm('1900-01-01 00:00')+hours(time)-hours(3))  
  }
o %>% 
  feather::write_feather(., path='data/era5_cax_swvl2_1980_2018.feather')

# swvl3 --------------------------------------------------------------
source_dir <- "/home/sami/Downloads/scratch/"
name_list <- list.files(source_dir)
name_list
# Extract met var names from file name
var_list <- str_extract(name_list, "(?<=\\.)[^.]+") %>% unique
var_list <- var_list[is.na(var_list)!=T]
list.files(source_dir, pattern='_swvl3')

list.files(merged_d)
cl <- makePSOCKcluster(10); registerDoParallel(cl,cores = 10) # Register parallel backend for foreach
numCores <- getDoParWorkers(); print(numCores); getDoParRegistered(); doMC::registerDoMC(cores = 10)
o <- foreach(i=1:length(list.files(source_dir, pattern = '_swvl3', full.names = T)), 
             .combine = bind_rows) %dopar% 
  {
    tidync::tidync(list.files(source_dir, pattern = '_swvl3', full.names = T)[i]) %>% 
      tidync::hyper_tibble() %>% 
      rename(lon=longitude, 
             lat = latitude) %>% 
      mutate(lon=lon-360) %>% 
      filter(near(lon,-51.5, tol = 0.1) & near(lat,-1.75, tol = 0.1)) %>% 
      mutate(idx = row_number()) %>% 
      mutate(date = ymd_hm('1900-01-01 00:00')+hours(time)-hours(3))  
  }
o %>% 
  feather::write_feather(., path='data/era5_cax_swvl3_1980_2018.feather')

# swvl4 --------------------------------------------------------------
source_dir <- "/home/sami/Downloads/scratch/"
name_list <- list.files(source_dir)
name_list
# Extract met var names from file name
var_list <- str_extract(name_list, "(?<=\\.)[^.]+") %>% unique
var_list <- var_list[is.na(var_list)!=T]
list.files(source_dir, pattern='_swvl4')

list.files(merged_d)
cl <- makePSOCKcluster(10); registerDoParallel(cl,cores = 10) # Register parallel backend for foreach
numCores <- getDoParWorkers(); print(numCores); getDoParRegistered(); doMC::registerDoMC(cores = 10)
o <- foreach(i=1:length(list.files(source_dir, pattern = '_swvl4', full.names = T)), 
             .combine = bind_rows) %dopar% 
  {
    tidync::tidync(list.files(source_dir, pattern = '_swvl4', full.names = T)[i]) %>% 
      tidync::hyper_tibble() %>% 
      rename(lon=longitude, 
             lat = latitude) %>% 
      mutate(lon=lon-360) %>% 
      filter(near(lon,-51.5, tol = 0.1) & near(lat,-1.75, tol = 0.1)) %>% 
      mutate(idx = row_number()) %>% 
      mutate(date = ymd_hm('1900-01-01 00:00')+hours(time)-hours(3))  
  }
o %>% 
  feather::write_feather(., path='data/era5_cax_swvl4_1980_2018.feather')








list.files(merged_d, pattern = 'VAR_2T', full.names = T)[i] %>% 
  read_ncdf() %>% 
  as_tibble() %>% 
  units::drop_units() %>% 
  pull(VAR_2T) %>% 
  hist

stars::read_ncdf('/home/sami/Downloads/scratch/merged//merged_ALNID_.nc') %>% 
  as_tibble() %>% 
  pull(time) %>% 
  summary

to_cat <- list.files(merged_d, full.names = T)
o <- tidync::tidync(to_cat[1]) %>% 
  tidync::hyper_tibble() %>% 
  rename(lon=longitude, 
         lat = latitude) %>% 
  mutate(lon=lon-360) %>% 
  filter(near(lon,-51.5, tol = 0.1) & near(lat,-1.75, tol = 0.1)) %>% 
  mutate(idx = row_number()) %>% 
  mutate(date = ymd_hm('1900-01-01 00:00')+hours(time)-hours(3))



library(foreach); library(doParallel)
library(doMC)
parallel::detectCores()

cl <- makePSOCKcluster(10); 
registerDoParallel(cl,cores = 10) # Register parallel backend for foreach
numCores <- getDoParWorkers(); print(numCores)
getDoParRegistered()
doMC::registerDoMC(cores = 10)
system.time(
  o <- foreach(i = 1:length(to_cat), .combine = bind_cols) %dopar% {
    tidync::tidync(to_cat[i]) %>% 
      tidync::hyper_tibble() %>% 
      rename(lon=longitude, 
             lat = latitude) %>% 
      mutate(lon=lon-360) %>% 
      filter(near(lon,-51.5, tol = 0.1) & near(lat,-1.75, tol = 0.1)) %>% 
      mutate(idx = row_number()) %>% 
      mutate(date = ymd_hm('1900-01-01 00:00')+hours(time)-hours(3))
  }
) # do: 76 seconds # dopar:17
doParallel::stopImplicitCluster(cl)


# part 2 accumulated vars -------------------------------------------------
scratch_dir <- "/home/sami/Downloads/scratch_v2/"
file_list <- list.files("/home/sami/Downloads/scratch_v2/", pattern = 'sfc', 
                        full.names = T)
file_list <- file_list[str_detect(file_list, '.tar')]


for(i in 1:length(file_list)){
  untar(file_list[i], exdir = scratch_dir)
}
file.remove(file_list)
name_list <- list.files(scratch_dir)

# Extract met var names from file name
name_list <- name_list[str_detect(name_list, '.nc')]
var_list <- str_extract(name_list, "(?<=\\.)[^.]+") %>% unique
var_list <- var_list[is.na(var_list)!=T]


merged_d <- "/home/sami/Downloads/scratch_v2/merged/"

tmp_list <- list.files(scratch_dir)

for(i in 1:length(var_list)){
  to_cat <- tmp_list[str_detect(tmp_list, var_list[i])]
  to_cat <- to_cat[str_detect(to_cat, ".tar")==F]
  to_cat <- file.path(scratch_dir, to_cat)  
  system(paste('cdo cat ',
               paste(to_cat[1:2], collapse = ' '),
               file.path(merged_d, paste0("merged_",var_list[i],"_.nc"))))
  print(paste(i, var_list[i]))
}
list.files(merged_d)

# scratch 
paste('cdo cat ',
             paste(to_cat[1:2], collapse = ' '),
             file.path(merged_d, paste0("merged_",var_list[i],"_.nc")))



junk <- stars::read_ncdf("/home/sami/Downloads/scratch/merged/all_vars.nc") %>% 
  as_tibble()
junk <- junk %>% mutate(lon=-360-longitude) %>% 
  rename(lat = latitude, 
         date = time)
junk <- junk %>% select(-longitude)
names(junk) <- tolower(names(junk))
feather::write_feather(junk, path = "/home/sami/Downloads/cax_era5.feather")

junk %>% 
  filter(near(lon, -51.45, tol=0.1) & near(lat, -1.71, tol=0.1)) %>% 
  feather::write_feather(., path='/home/sami/Downloads/cax_tower_era5.feather')

# keep: swvl1-4, cape, sp, tcc, val_10u, val_10v, val_2t, val_2d, 
# stl2, stl3, val_100u, val_100v


library(tidyverse); library(lubridate); library(stringr)
library(stars)

# part 3 (mean rates of ssrd) -----------------------------------------------------------
scratch_dir <- "/home/sami/Downloads/scratch_v3/"
file_list <- list.files(scratch_dir, pattern='.tar', full.names = T)
ex_dir <- "/home/sami/Downloads/scratch_v3/unzipped"
for(i in 1:length(file_list)){
  untar(file_list[i], exdir = ex_dir)
}
name_list <- list.files(ex_dir)
name_list

# Extract met var names from file name
var_list <- str_extract(name_list, "(?<=\\.)[^.]+") %>% unique
var_list <- var_list[is.na(var_list)!=T]

merged_d <- "/home/sami/Downloads/scratch_v3/merged"

tmp_list <- list.files(ex_dir)

for(i in 1:length(var_list)){
  to_cat <- tmp_list[str_detect(tmp_list, var_list[i])]
  to_cat <- file.path(ex_dir,to_cat)  
  system(paste('cdo cat ',
               paste(to_cat, collapse = ' '),
               file.path(merged_d, paste0("merged_",var_list[i],"_.nc"))))
  print(paste(i, var_list[i]))
}
list.files(merged_d)

# part 4 (mean rates of ssrd) -----------------------------------------------------------
scratch_dir <- "/home/sami/Downloads/scratch_v4/"
file_list <- list.files(scratch_dir, pattern='.tar', full.names = T)
ex_dir <- "/home/sami/Downloads/scratch_v4/unzipped"
for(i in 1:length(file_list)){
  untar(file_list[i], exdir = ex_dir)
}
name_list <- list.files(ex_dir)
name_list

# Extract met var names from file name
var_list <- str_extract(name_list, "(?<=\\.)[^.]+") %>% unique
var_list <- var_list[is.na(var_list)!=T]

merged_d <- "/home/sami/Downloads/scratch_v4/merged"

tmp_list <- list.files(ex_dir)

for(i in 1:length(var_list)){
  to_cat <- tmp_list[str_detect(tmp_list, var_list[i])]
  to_cat <- file.path(ex_dir,to_cat)  
  system(paste('cdo cat ',
               paste(to_cat, collapse = ' '),
               file.path(merged_d, paste0("merged_",var_list[i],"_.nc"))))
  print(paste(i, var_list[i]))
}
list.files(merged_d)

# Scratch -----------------------------------------------------------------
# MSNSWRF: Mean surface net short-wave radiation flux

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

o %>% dim
o %>%   ggplot(data=., aes(date, SSRD))+
  geom_line()


tidync::tidync(to_cat[2]) %>% 
  tidync::hyper_tibble() %>% 
  rename(lon=longitude, 
         lat = latitude) %>% 
  mutate(lon=lon-360) %>% 
  filter(near(lon,-51.5, tol = 0.1) & near(lat,-1.75, tol = 0.1)) %>% 
  mutate(idx = row_number()) %>% 
  mutate(date = ymd_hm('1900-01-01 00:00')+hours(forecast_initial_time)+hours(forecast_hour)) %>% 
  ggplot(data=., aes(date, SSRD))+
  geom_line()

to_cat <- list.files('/home/sami/Downloads/scratch_v2/', pattern='SSRD',full.names = T)
stars::read_ncdf(to_cat[200], 
                 make_time = F) %>% 
  as_tibble()


o <- stars:: read_ncdf(to_cat[300]) %>% 
  as_tibble() %>% 
  rename(lon=longitude, lat=latitude) %>% 
  mutate(lon=lon-360) %>% 
  filter(near(lon,-51.5, tol = 0.1) & near(lat,-1.75, tol = 0.1)) %>% 
  mutate(date = forecast_initial_time+minutes(forecast_hour*60)+hours(3)) %>% 
  units::drop_units()
names(o) <- colnames(o) %>% tolower()


o %>% 
  arrange(date) %>% 
  # filter(hour(forecast_initial_time)==12) %>%
  # filter(ssrd>0) %>% 
  ggplot(data=., aes(date, cumsum(ssrd)))+
  geom_point()+
  geom_smooth()

o %>% 
  group_by(forecast_initial_time, forecast_hour) %>% 
  summarize(val = sum(ssrd)) %>% 
  filter(val>0) %>% 
  ungroup() %>% 
  mutate(date = forecast_initial_time+forecast_hour) %>%
  
  ggplot(data=., aes(date, val))+
  geom_point()


o %>% group_by(forecast_initial_time) %>% 
  summarize(val = mean(ssrd), 
            val2 = mean(forecast_hour)) %>% 
  ggplot(data=., aes(forecast_initial_time, val, color=val2))+
  geom_point()+
  scale_color_viridis_c()

o %>% 
  # filter(ssrd > 0) %>% 
  group_by(date) %>% 
  summarize(val = mean(ssrd), 
            val2 = mean(forecast_hour)) %>% 
  ggplot(data=., aes(date, val2))+
  geom_point()

o %>% 
  ggplot(data=., aes(forecast_initial_time, forecast_hour, color=ssrd))+
  geom_point()+
  scale_color_viridis_c()

o %>% 
  filter(date <= ymd('1979-02-05')) %>% 
  # filter(ssrd > 0) %>% 
  # filter(forecast_hour >= 12.5) %>% 
  # mutate(ssrd_l1 = lag(ssrd)) %>% 
  # mutate(delta_ssrd = ssrd - ssrd_l1) %>% 
  ggplot(data=., aes(date, ssrd))+
  geom_line()+
  geom_vline(aes(xintercept=forecast_initial_time),col='red')


alt <- stars::read_ncdf('data/era5_ssrd_2018_2019.nc') %>% 
  as_tibble() %>% 
  units::drop_units()
alt %>% 
  mutate(date = time-hours(3)) %>% 
  mutate(hour = hour(date)) %>% 
  ggplot(data=., aes(hour, p0001))+
  geom_point(alpha = 0.25)+
  geom_smooth()

# %>% 
#   filter(forecast_hour <= 12 & forecast_hour>0)

o %>% 
  ggplot(data=., aes(forecast_initial_time, SSRD))+
  geom_point(alpha=0.25)

o %>% 
  mutate(hour=hour(date)) %>% 
  pull(hour) %>% table()
max(o$date)-min(o$date)

o %>% 
  group_by(forecast_hour) %>% 
  summarize(val = mean(MSNSWRF,na.rm=T)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(forecast_hour, val))+
  geom_line()

o %>% 
  mutate(hour=hour(date)) %>% 
  ggplot(data=., aes(hour, MSNSWRF, color=as.factor(forecast_initial_time)))+
  geom_point()+
  geom_line()+
  scale_color_viridis_d()

o %>% 
  mutate(doy = yday(date)) %>% 
  ggplot(data=., aes(date, as.numeric(MSNSWRF), color=forecast_hour))+
  geom_point()+
  scale_color_viridis_c()+
  facet_wrap(~doy, scales = 'free')

o %>% group_by(lon,lat) %>% 
  units::drop_units() %>% 
  summarize(val = mean(MCPR)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(lon,lat,fill=val))+
  geom_tile()+
  geom_label(aes(label=paste(lon,lat)))+
  # control plot
  geom_point(data=tibble(lon=-51.45701, lat=-1.71595),col='red',inherit.aes = F, 
             aes(lon,lat))+
  # drought plot? 
  geom_point(data=tibble(lon=-51.46203, lat=-1.73572),col='black',inherit.aes = F, 
             aes(lon,lat))+
  scale_fill_viridis_c()

test <- stars::read_ncdf("/home/sami/Downloads/scratch_v2/401905.SSRD.e5.oper.fc.sfc.accumu.128_169_ssrd.ll025sc.1979010106_1979011606.nc") %>% 
  as_tibble()
test <- test %>% 
  mutate(date = forecast_initial_time + minutes(forecast_hour*60))
test$forecast_hour %>% unique %>% length()
test$forecast_initial_time %>% unique 
list.files(merged_d, pattern = 'merged')

test <- stars::read_ncdf("/home/sami/Downloads/scratch/merged/merged_LMLD_.nc") %>% 
  as_tibble()
test %>% pull(time) %>% summary

junk %>% 
  filter(date >= ymd('1980-01-01')) %>% 
  filter(date <= ymd('1981-01-01')) %>% 
  group_by(lon,lat) %>% 
  summarize(val = mean(tcc)) %>% 
  ungroup() %>% 
  ggplot(data=.,aes(lon,lat,fill=val))+
  geom_tile()+
  geom_point(data=tibble(lon=-51.45704, lat=-1.71603),
             aes(lon,lat), inherit.aes = F, col='red')+
  coord_equal()+
  scale_fill_viridis_c(direction = -1)




junk <- feather::read_feather(path = "/home/sami/Downloads/cax_era5.feather")

names(junk)
junk <- stars::read_ncdf("/home/sami/Downloads/scratch/merged/merged_VAR_2T_.nc") %>% 
  as_tibble()

junk %>% 
  mutate(year=year(time)) %>% 
  group_by(year) %>% 
  summarize(val = mean(as.numeric(VAR_2T))) %>% 
  ungroup() %>% 
  ggplot(data=.,aes(year, val-273.15))+
  geom_point()+
  geom_smooth(method='lm')

junk %>% 
  group_by(time) %>% 
  summarize(val = mean(VAR_2T)) %>% 
  ungroup() %>% 
  ggplot(data.,aes(time, val))+
  geom_smooth(method='lm')


substr(list.files(scratch_dir),8,12) %>% unique

str_extract(list.files(scratch_dir)[1],"[0-9]")
str_split_fixed(list.files(scratch_dir)[1], pattern = '[.]',n = 2, simplify = T)

tmp <- list.files(scratch_dir, pattern = 'ALNID', full.names = T)
stars::read_ncdf(tmp[1]) %>% as_tibble() %>% 
  pull(time) %>% summary
stars::read_ncdf(tmp[2]) %>% as_tibble() %>% 
  pull(time) %>% summary


tmp[1]; 
tmp[2]

file_list[1]
gsub("^.*:","", string)
"'(.*?)'"
gsub("()", "", name_list[1])

str_extract(name_list[1], "401801.(.*?).e5")
str_extract(name_list[1], "[.](.*?)[.]")
str_extract(name_list[1], "\\.(.*?)\\.")
str_extract(name_list[1], pattern = "[.].(.*?)[.]")
str_extract(name_list[1], "\\.(.*?)\\.")
str_extract(name_list[1], "(?<=\\.)[^.]+")


library(tidyverse); library(lubridate); library(stringr)
library(stars)
# part 1 (instantaenous vars) -----------------------------------------------------------
scratch_dir <- "/home/sami/Downloads/scratch/"
file_list <- list.files("/home/sami/Downloads/", pattern = 'sfc', 
                        full.names = T)
file_list <- file_list[str_detect(file_list, '.tar')]


for(i in 1:length(file_list)){
  untar(file_list[i], exdir = scratch_dir)
}
name_list <- list.files(scratch_dir)

# Extract met var names from file name
var_list <- str_extract(name_list, "(?<=\\.)[^.]+") %>% unique
var_list <- var_list[is.na(var_list)!=T]


merged_d <- "/home/sami/Downloads/scratch/merged/"

tmp_list <- list.files(scratch_dir)

for(i in 1:length(var_list)){
  to_cat <- tmp_list[str_detect(tmp_list, var_list[i])]
  to_cat <- file.path(scratch_dir, to_cat)  
  system(paste('cdo cat ',
               paste(to_cat, collapse = ' '),
               file.path(merged_d, paste0("merged_",var_list[i],"_.nc"))))
  print(paste(i, var_list[i]))
}
list.files(merged_d)


# Part 2 accumulated vars -------------------------------------------------
scratch_dir <- "/home/sami/Downloads/scratch_v2/"
file_list <- list.files("/home/sami/Downloads/scratch_v2/", pattern = 'sfc', 
                        full.names = T)
file_list <- file_list[str_detect(file_list, '.tar')]


for(i in 1:length(file_list)){
  untar(file_list[i], exdir = scratch_dir)
}
file.remove(file_list)
name_list <- list.files(scratch_dir)

# Extract met var names from file name
name_list <- name_list[str_detect(name_list, '.nc')]
var_list <- str_extract(name_list, "(?<=\\.)[^.]+") %>% unique
var_list <- var_list[is.na(var_list)!=T]


merged_d <- "/home/sami/Downloads/scratch_v2/merged/"

tmp_list <- list.files(scratch_dir)

for(i in 1:length(var_list)){
  to_cat <- tmp_list[str_detect(tmp_list, var_list[i])]
  to_cat <- to_cat[str_detect(to_cat, ".tar")==F]
  to_cat <- file.path(scratch_dir, to_cat)  
  to_cat <- sort(to_cat)
  system(paste('cdo cat ',
               paste(to_cat, collapse = ' '),
               file.path(merged_d, paste0("merged_",var_list[i],"_.nc"))))
  print(paste(i, var_list[i]))
}
list.files(merged_d)

# scratch 
system(paste('cdo cat ',
      paste(to_cat[1:2], collapse = ' '),
      file.path(merged_d, paste0("merged_",var_list[i],"_.nc")))
)


junk <- stars::read_ncdf("/home/sami/Downloads/scratch/merged/all_vars.nc") %>% 
  as_tibble()
junk <- junk %>% mutate(lon=-360-longitude) %>% 
  rename(lat = latitude, 
         date = time)
junk <- junk %>% select(-longitude)
names(junk) <- tolower(names(junk))
feather::write_feather(junk, path = "/home/sami/Downloads/cax_era5.feather")

junk %>% 
  filter(near(lon, -51.45, tol=0.1) & near(lat, -1.71, tol=0.1)) %>% 
  feather::write_feather(., path='/home/sami/Downloads/cax_tower_era5.feather')

# keep: swvl1-4, cape, sp, tcc, val_10u, val_10v, val_2t, val_2d, 
# stl2, stl3, val_100u, val_100v

# Scratch -----------------------------------------------------------------
test <- stars::read_ncdf("/home/sami/Downloads/scratch_v2/401905.SSRD.e5.oper.fc.sfc.accumu.128_169_ssrd.ll025sc.1979010106_1979011606.nc") %>% 
  as_tibble()
test <- test %>% 
  mutate(date = forecast_initial_time + minutes(forecast_hour*60))
test$forecast_hour %>% unique %>% length()
test$forecast_initial_time %>% unique 
list.files(merged_d, pattern = 'merged')

test <- stars::read_ncdf("/home/sami/Downloads/scratch/merged/merged_LMLD_.nc") %>% 
  as_tibble()
test %>% pull(time) %>% summary

junk %>% 
  filter(date >= ymd('1980-01-01')) %>% 
  filter(date <= ymd('1981-01-01')) %>% 
  group_by(lon,lat) %>% 
  summarize(val = mean(tcc)) %>% 
  ungroup() %>% 
  ggplot(data=.,aes(lon,lat,fill=val))+
  geom_tile()+
  geom_point(data=tibble(lon=-51.45704, lat=-1.71603),
             aes(lon,lat), inherit.aes = F, col='red')+
  coord_equal()+
  scale_fill_viridis_c(direction = -1)




junk <- feather::read_feather(path = "/home/sami/Downloads/cax_era5.feather")

names(junk)
junk <- stars::read_ncdf("/home/sami/Downloads/scratch/merged/merged_VAR_2T_.nc") %>% 
  as_tibble()

junk %>% 
  mutate(year=year(time)) %>% 
  group_by(year) %>% 
  summarize(val = mean(as.numeric(VAR_2T))) %>% 
  ungroup() %>% 
  ggplot(data=.,aes(year, val-273.15))+
  geom_point()+
  geom_smooth(method='lm')

junk %>% 
  group_by(time) %>% 
  summarize(val = mean(VAR_2T)) %>% 
  ungroup() %>% 
  ggplot(data.,aes(time, val))+
  geom_smooth(method='lm')


substr(list.files(scratch_dir),8,12) %>% unique

str_extract(list.files(scratch_dir)[1],"[0-9]")
str_split_fixed(list.files(scratch_dir)[1], pattern = '[.]',n = 2, simplify = T)

tmp <- list.files(scratch_dir, pattern = 'ALNID', full.names = T)
stars::read_ncdf(tmp[1]) %>% as_tibble() %>% 
  pull(time) %>% summary
stars::read_ncdf(tmp[2]) %>% as_tibble() %>% 
  pull(time) %>% summary


tmp[1]; 
tmp[2]

file_list[1]
gsub("^.*:","", string)
"'(.*?)'"
gsub("()", "", name_list[1])

str_extract(name_list[1], "401801.(.*?).e5")
str_extract(name_list[1], "[.](.*?)[.]")
str_extract(name_list[1], "\\.(.*?)\\.")
str_extract(name_list[1], pattern = "[.].(.*?)[.]")
str_extract(name_list[1], "\\.(.*?)\\.")
str_extract(name_list[1], "(?<=\\.)[^.]+")

str_extract(name_list, "(?<=\\.)[^.]+") %>% unique

# CP: Convective precip
# LSP: Large scale precip 
# E: ? 
# PEV: ? 
# SSRD: shortwave 
# STRD: ? 



junk <- stars::read_ncdf("/home/sami/Downloads/scratch_v2/merged/merged_CP_.nc") %>% 
  as_tibble()
junk$forecast_initial_time %>% last


p <- stars::read_ncdf('data/era5_surface_solar_radiation_downwards_2000_2010.nc') %>% 
  as_tibble()
p %>% 
 filter(near(lon,-51.5, tol = 0.1) & near(lat,-1.75, tol = 0.1)) %>% 
  units::drop_units() %>% 
  mutate(day = date(time)) %>% 
  filter(day==date(x='2000-03-01')) %>% 
  ggplot(data=., aes(time-hours(3), rsds))+
  geom_point()
  
        