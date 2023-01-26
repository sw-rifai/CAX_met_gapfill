# CAX control plot coord is approx: -51.45701, -1.71595

file_list <- list.files(pattern='.tar', full.names = T)

for(i in 1:length(file_list)){
  untar(file_list[i], exdir = "tmp_data/")
}


file.copy(list.files("tmp_data/", pattern = "STRD",full.names = T), 
          to="tmp_data/strd/")

list.files('tmp_data/strd/')
sys.call()

# -------------------------------------------------------------------------
# PROBLEM WITH MERGING ACCUMULATED VARIABLE FILES 

merged_d <- "/home/sami/Downloads/scratch_v2/merged/"
tmp_list <- list.files(scratch_dir)
tmp_list <- tmp_list[str_detect(tmp_list, 'SSRD')]



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

to_cat[1:2]

system(paste('ncdump -h',to_cat[1]))
system(paste('cdo sinfo',to_cat[1]))

paste0('cdo cat ',paste(to_cat[1:2], collapse = ' '),
       ' ',
       file.path(merged_d, paste0("merged_",var_list[i],"_.nc")))

system(paste0('cdo -O mergetime ',paste(to_cat[1:2], collapse = ' '),
       ' ',
       file.path(merged_d, paste0("merged_",var_list[i],"_.nc"))))

          


file.path('/home/sami/Downloads/scratch_v3/merged','merged_CP_.nc') %>% 
  stars::read_ncdf(.) %>% 
  as_tibble() %>% 
  rename(lon=longitude,
         lat=latitude) %>% 
  mutate(lon=lon-360) %>% 
  filter(lon==-51.5 & lat==-1.75)



o <- read_csv("outputs/cax_gapfill_2001_2018_2020-01-16.csv")
o %>% 
  mutate(year=year(date)) %>%
  group_by(year) %>% 
  summarize(tot = sum(p_mm_gf,na.rm=TRUE)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(year,tot))+geom_line()


merged %>% 
  mutate(year=year(date)) %>% 
  group_by(year) %>% 
  summarize(val = sum(p_mm_gf,na.rm=TRUE), 
            val2 = sum(p_gpm,na.rm=TRUE)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(year, val))+geom_line()+
  geom_line(aes(year,val2),color='blue')

gpm$date[1:3]


merged$date[1]


oo <- read_feather("outputs/cax_precip_gapfill-GPM.feather")
oo %>% mutate(year=year(date)) %>% 
  group_by(year) %>% 
  summarize(p_gpm = sum(precip,na.rm=TRUE)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(year, p_gpm))+geom_line()


tmp1 <- read_feather('outputs/cax_precip_gapfill-GPM.feather') %>% 
  mutate(hour=hour(date), 
         date_ymd=date(date)) %>% 
  group_by(date_ymd, hour) %>% 
  summarize(p_gpm = sum(p_gpm,na.rm=TRUE), 
            p_mm_obs = sum(p_mm_obs,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(date = ymd_h(paste(date_ymd,hour))) %>% 
  mutate(p_mm_gf = if_else(is.na(p_mm_obs)==T, p_gpm, p_mm_obs)) %>% 
  select(date, p_gpm, p_mm_obs, p_mm_gf)


tmp1 %>% 
  mutate(year=year(date)) %>% 
  group_by(year) %>% 
  summarize(val = sum(p_mm_gf,na.rm=TRUE)) %>% 
  ungroup() %>% 
  ggplot(data=., aes(year, val))+geom_line()

merged$shortwave_pred %>% hist
merged %>% select(shortwave_obs, shortwave_pred, shortwave_gf)
merged %>% mutate(hour=hour(date)) %>% 
  group_by(hour) %>% 
  summarize(val = mean(shortwave_pred,na.rm=TRUE)) %>% 
  ungroup()
tmp5$ws_bc %>% hist
tmp5 <- tmp5 %>% mutate(ws_bc = ifelse(ws_bc < 0, 0, ws_bc))

merged %>% summary
