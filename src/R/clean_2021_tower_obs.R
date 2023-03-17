pb <- fread("data/met_obs_CAX_tower/TORRE_PB_Drought_2021-22-23.csv")
setDT(pb)

pa <- fread("data/met_obs_CAX_tower/TORRE_Control_PA_2021-22-23.csv")
setDT(pa)

pa <- pa %>%
  rename(day=Dia, month=Mês,year=Ano,hour=Hora) %>%
  mutate(jnk1 = hm(hour) %>% as.character(),
         jnk2 = hms(hour) %>% as.character()) %>% 
  mutate(jnk3 = if_else(is.na(jnk1)==T, jnk2, jnk1)) %>% 
  mutate(year = if_else(year< 2000, year+2000, year)) %>% 
  mutate(date = ymd(paste(year,month,day))) %>% 
  mutate(time = parse_date_time(paste(date, jnk3), 
                  orders = c("ymd HMS","ymd HM", "ymd MS", "ymd S"),
                  tz="America/Belem")
         ) %>% 
  # select(-c("jnk1","jnk2","jnk3")) %>% 
  mutate(hour=hour(time)) %>% 
  rename(t2m = `Tar_2m (°C)`,
         t16m = `Tar_16m (°C)`,
         t28m = `Tar_28m (°C)`,
         t42m = `Tar_42m (°C)`,
         
         d2m = `Td_2m (°C)`,
         d16m = `Td_16m (°C)`,
         d28m = `Td_28m (°C)`,
         d42m = `Td_42m (°C)`,
         
         rh2m = `RH_2m (%)`,
         rh16m = `RH_16m (%)`,
         rh28m = `RH_28m (%)`,
         rh42m = `RH_42m (%)`,
         
         vp2m = `PSV_ 2m (mbar)`,
         vp16m = `PSV_ 16m (mbar)`,
         vp28m = `PSV_ 28m (mbar)`,
         vp42m = `PSV_ 42m (mbar)`,
         
         rad_global = `RG (W/m²)`,
         rad_net = `NET (W/m²)`,
         precip = `Chuva (mm)`) %>% 
  mutate(year=year(time), month=month(time), hour=hour(time)) %>% 
  .[order(time)] %>% 
  .[,`:=`(delta_t28 = c(0, diff(.$t28m)))] %>% # 
  .[,`:=`(t_l1 = lag(t28m,n=1))] %>% 
  .[is.na(date)==F] %>% 
  .[,`:=`(time = round_date(time,unit='60 minutes'))] %>% 
  .[, lapply(.SD, mean, na.rm=T), 
   .SDcols=sapply(., is.numeric),
   by=.(time)] %>% 
  .[,`:=`(year=year(time),month=month(time),day=day(time),hour=hour(time))]
pa[,`:=`(date = date(time))]

# attempting to pair columns with previous "cleaned" met observations
pa <- pa %>% 
  select(c(# "Bat (volts)", 
           "d16m", "d28m", "d2m", "d42m", "date", "day", 
           "delta_t28", 
           # "Dias Juliano", 
           "hour", "month", "precip", "rad_global", 
           "rad_net", 
           # "RGT (MJ/m²)", 
           "rh16m", "rh28m", "rh2m", "rh42m", 
           "t_l1", "t16m", "t28m", "t2m", "t42m",
           # "Temp_log (°C)",
           "time", 
           # "Tw_16m (°C)", "Tw_28m (°C)", "Tw_2m (°C)", "Tw_42m (°C)", 
           "vp16m", "vp28m", "vp2m", "vp42m", "year"))

pa %>%
  arrow::write_parquet(., 
         sink=paste0("data/met_obs_filtered/cax_tower_met_obs_2021-2022_processed_",Sys.Date(),".parquet"),
              compression = 'snappy')



# SCRAP -----------------------------
# pa
# 
# 
# pb
# pb[is.na(AirTC_Avg)==F]
# 
# 
# pa %>% 
#   ggplot(data=., aes(hour,t28m,group=date))+
#   geom_line(alpha=0.25,lwd=0.25)+
#   theme_linedraw()
#   facet_wrap(~date,scales = 'free_x',ncol = 1)
# 
# 
# pa[date%in%ymd("2022-05-03")]
# 
# 
# 
# 
# pa$time %>% diff %>% unique
# 
# 
# pa$time[1:5] %>% 
#   round_date(., unit='1 hour')
# 
# pa[is.na(hour)==T]
# 
# pa$time %>% diff %>% table
# 
# pa$delta_t28 %>% hist(100)
# pa[is.na(delta_t28)==F][abs(delta_t28 > 3)]
# 
# pa %>%
#   select(t28m,t_l1,delta_t28)
# 
# pa$t28m %>% length
# pa$t28m %>% diff %>% length
# 
# 
# pa %>% ggplot(aes(time,t28m))+
#   geom_line()
# pa[date%in%pa[t28m>29]$date] %>% 
#   ggplot(aes(time,t28m))+
#   geom_point()
# 
# 
# 
# tmp <- pa$Hora %>% unique
# hm(tmp[139],roll=T)
# 
# na.omit((c(NA,hm(tmp[139]) %>% as.character())))[1]
# pa$jnk3 %>% is.na %>% table
# pa$jnk3 %>% unique
# 
# pa[is.na(time)==T]
# pa %>% 
#   select(Dia,Mês,Ano,Hora) %>% 
#   drop_na()
# 
# pa$Dia %>% unique
# pa$Ano %>% unique
# pa$Mês %>% unique
# 
# pa$date %>% is.na %>% table
# pa[is.na(date)==T]
# pa[month==2][day>28]
# 
# pa[1,] %>% 
#   mutate(time = ymd_hms(paste(year,month,day," ",jnk3))) 
# 
# pa[1,]$date
# pa[1,]$jnk3 %>% parse_time()
# 
# parse_date_time(paste("2021-01-01","30M 0S"), 
#                 orders = c("ymd HMS","ymd HM", "ymd MS"))
# 
# 
# pa$time %>% is.na %>% table
# 
# 
# pa[is.na(time)]
# 
# names(pa)
# pa %>% 
#   ggplot(aes(hour,rh28m,group=month,color=month))+
#   # geom_point()
#   geom_smooth(formula = y~s(x,bs='cc',k=24),
#               method = 'gam',se=F)+
#   scale_color_viridis_c(option='H')
# 
