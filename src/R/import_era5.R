library(tidyverse); library(lubridate); library(ncdf4);

d2m <- ncdf4::nc_open("data/era5_land/era5_land_d2m_2010_2018.nc")
ncdf4::ncvar_get(d2m, varid='d2m') %>% dim
nc_close(d2m)


library(stars); 
d2m <- read_ncdf("data/era5_land/era5_land_d2m_2010_2018.nc") %>% 
  as_tibble()
p2 <- read_ncdf("data/era5_land/part_d.nc") %>% 
  as_tibble()

read_ncdf("data/era5_land/era5_land_strd_2001_2009.nc")


d2m %>% 
  mutate(d2m=as.numeric(d2m)) %>% 
  mutate(date=as.POSIXct(time, tz='UTC')) %>% 
  mutate(month=month(date), year=year(date)) %>% 
  # filter(time>=ymd('2009-01-01')) %>%
  filter(near(latitude,-1.7,tol=0.01)) %>% 
  ggplot(data=.,aes(year, d2m))+
  geom_smooth(method='lm')+
  facet_wrap(~month)
