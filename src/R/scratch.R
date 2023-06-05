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



test_t28m %>% 
  as.data.table() %>% 
  mutate(
    pred = as.data.table(h2o.predict(a0@leader,test_t28m)[,'predict'])$predict) %>% 
  # lm(t28m ~ pred, data=.) %>% summary
  ggplot(data=.,aes(t28m,pred))+
  geom_point(alpha=0.5,size=0.5)+
  geom_abline(col='#cf0000')+
  geom_smooth()+
  theme_linedraw()

test_t28m %>% 
  as.data.table() %>% 
  mutate(
    pred = as.data.table(h2o.predict(a0@leader,test_t28m)[,'predict'])$predict) %>% 
  ggplot(data=.,aes(pred,t28m))+
  geom_point(alpha=0.5,size=0.5)+
  geom_abline(col='#cf0000')+
  geom_smooth()+
  theme_linedraw()














pd <- as.h2o(dat)
pdat <- dat
pdat$pred_t28m <- predict(f_t_0, newdata=pdat, type='response')


pdat <- pdat[order(time)][,`:=`(
  pred_t28m_max_72h = frollapply(pred_t28m,n=72,FUN=max,align = 'right'),
  pred_t28m_mean_72h = frollmean(pred_t28m,n=72,na.rm=F,align = 'right'),
  pred_t28m_min_72h = frollapply(pred_t28m,n=72,FUN=min,align = 'right'),
  obs_t28m_max_72h = frollapply(t28m,n=72,FUN=max,align = 'right'),
  obs_t28m_mean_72h = frollmean(t28m,n=72,align = 'right',na.rm = F),
  obs_t28m_min_72h = frollapply(t28m,n=72,FUN=min,align = 'right')
)]
vec_time_train <- train$time
pdat <- pdat[,`:=`(in_train = time%in%vec_time_train)][
  ,`:=`(in_train = if_else(is.na(t28m)==F, in_train, F))
]


sel_year <- 2011:2013
pdat[year%in%sel_year] %>% 
  select(., time,in_train, starts_with(c("obs_t28m_","pred_t28m_"))) %>% 
  pivot_longer(-c("time","in_train")) %>% 
  mutate(stat =
           case_when(str_detect(name,'max')~'max',
                     str_detect(name,'mean')~'mean',
                     str_detect(name,'min')~'min')) %>% 
  mutate(obs = case_when(
    str_detect(name,'obs_t28m_')~'observation',
    str_detect(name,'pred_t28m_')~'prediction',
  )) %>% 
  ggplot(data=.,aes(time, value,
                    color=obs,
                    group=paste(stat,obs)))+
  geom_rect(aes(xmin=time,xmax=time+hours(1),
                ymin=20,ymax=35,
                fill=in_train),
            color='transparent')+
  geom_line(color='black',
            lwd=1)+
  geom_line()+
  labs(x=NULL,y='7-day min/max/mean Tair 28m (°C)',
       fill='Period\nIn Training\nData',
       color='Type')+
  scale_color_discrete_c4a_cat(palette = 'okabe',reverse = T)+
  scale_fill_manual(values=c('grey80','grey20'))+
  coord_cartesian(expand=F)+
  theme_linedraw()+
  theme()



pdat[is.na(obs_t28m_mean_72h)==F][,.(
  val_hi = mean(
  sqrt((obs_t28m_max_72h - pred_t28m_max_72h)**2) ,na.rm=T),
  val_mean = mean(
    sqrt((obs_t28m_mean_72h - pred_t28m_mean_72h)**2) ,na.rm=T),
  val_lo = mean(
    sqrt((obs_t28m_min_72h - pred_t28m_min_72h)**2) ,na.rm=T)
),
                           by=in_train]




jnk <- h2o.automl(x=covar_names,
                 y='t28m',
                 training_frame = train_t28m, 
                 # validation_frame = test_t28m,
                 include_algos = c("GLM"),
                 max_models = 50,
                 # max_runtime_secs = 200,
                 seed=3)
jnk
h2o.performance(jnk@leader, newdata=test_t28m)@metrics
summary(jnk)
h2o.varimp(jnk)


cat(c(paste(covar_names,"+"),"1"))
jnk <- gam(reformulate(covar_names,response = 't28m'), 
    data=train,
    method='REML')
summary(jnk)

jnk <- bam(t28m ~ 
             s(ddate, bs='ad')+
             s(e5_t2m, bs='ad'), 
           data=train,
           discrete = T,
           method='fREML')
summary(jnk)
plot(jnk,scheme=2,pages=1)

pdat <- dat
pdat$pred_t28m <- predict(jnk, newdata=pdat, type='response')


pdat <- pdat[order(time)][,`:=`(
  pred_t28m_max_72h = frollapply(pred_t28m,n=72,FUN=max,align = 'right'),
  pred_t28m_mean_72h = frollmean(pred_t28m,n=72,na.rm=F,align = 'right'),
  pred_t28m_min_72h = frollapply(pred_t28m,n=72,FUN=min,align = 'right'),
  obs_t28m_max_72h = frollapply(t28m,n=72,FUN=max,align = 'right'),
  obs_t28m_mean_72h = frollmean(t28m,n=72,align = 'right',na.rm = F),
  obs_t28m_min_72h = frollapply(t28m,n=72,FUN=min,align = 'right')
)]
vec_time_train <- train$time
pdat <- pdat[,`:=`(in_train = time%in%vec_time_train)][
  ,`:=`(in_train = if_else(is.na(t28m)==F, in_train, F))
]

sel_year <- 2011:2013
pdat[year%in%sel_year] %>% 
  select(., time,in_train,year, starts_with(c("obs_t28m_","pred_t28m_"))) %>% 
  pivot_longer(-c("time","in_train","year")) %>% 
  mutate(stat =
           case_when(str_detect(name,'max')~'max',
                     str_detect(name,'mean')~'mean',
                     str_detect(name,'min')~'min')) %>% 
  mutate(obs = case_when(
    str_detect(name,'obs_t28m_')~'observation',
    str_detect(name,'pred_t28m_')~'prediction',
  )) %>% 
  ggplot(data=.,aes(time, value,
                    color=obs,
                    group=paste(stat,obs)))+
  geom_rect(aes(xmin=time,xmax=time+hours(1),
                ymin=20,ymax=35,
                fill=in_train),
            color='transparent')+
  geom_line(color='black',
            lwd=1)+
  geom_line()+
  labs(x=NULL,y='7-day min/max/mean Tair 28m (°C)',
       fill='Period\nIn Training\nData',
       color='Type')+
  scale_color_discrete_c4a_cat(palette = 'okabe',reverse = T)+
  scale_fill_manual(values=c('grey80','grey20'))+
  coord_cartesian(expand=F)+
  facet_wrap(~year,ncol = 1,scales = 'free_x')+
  theme_linedraw()+
  theme()

pdat[is.na(obs_t28m_mean_72h)==F][,.(
  val_hi = mean(
    sqrt((obs_t28m_max_72h - pred_t28m_max_72h)**2) ,na.rm=T),
  val_mean = mean(
    sqrt((obs_t28m_mean_72h - pred_t28m_mean_72h)**2) ,na.rm=T),
  val_lo = mean(
    sqrt((obs_t28m_min_72h - pred_t28m_min_72h)**2) ,na.rm=T)
),
by=in_train]


pdat[,`:=`(t28_mean_72h_coarse = round(obs_t28m_mean_72h*5)/5)] %>% 
  .[,.(val = mean(abs(pred_t28m_mean_72h-obs_t28m_mean_72h),na.rm=T)),
  by=.(t28_mean_72h_coarse)] %>% 
  ggplot(data=.,aes(t28_mean_72h_coarse,val))+
  geom_point()





names(dat)
dat[sample(.N,1000)] %>% 
  ggplot(aes(hour, rad_net))+
  geom_point()


names(pa)
pa[sample(.N,1000)] %>% 
  ggplot(aes(hour, `NET (W/m²)`))+
  geom_point()

pa[sample(.N,1000)] %>% 
  ggplot(aes(hour, rad_global))+
  geom_point()


pa[sample(.N,1000)] %>% 
  ggplot(aes(hour, d42m))+
  geom_point()


dat %>% select(year, e5_t2m, t28m) %>% 
  drop_na() %>% 
  .[,.(rho = cor(e5_t2m,t28m)),by=year] %>% 
  .[order(year)] %>% 
  ggplot(aes(year,rho))+
  geom_point()


pdat2 %>% 
  group_by(year,month) %>% 
  summarize_if(., is.numeric, mean, na.rm=T) %>% 
  ungroup() %>% 
  mutate(date = ymd(paste(year,month,1))) %>% 
  ggplot(data=.,aes(month, pred_precip*729,color=year))+
  geom_point()+
  scale_color_viridis_c()

30.4*24

pdat2[, lapply(.SD, mean, na.rm=T), 
    .SDcols=sapply(obs, is.numeric),
    by=.(year,month)] %>%

  # mutate(date = ymd(paste(year,month,1))) %>% 
  ggplot(data=.,aes(date, pred_vpd28m))+
  geom_point()

library(patchwork)

p1 <- pdat2[sample(.N,30000)] %>% 
  ggplot(data=.,aes(t28m,pred_t28m))+
  geom_point(alpha=0.25,size=0.5)+
  geom_abline(col='#cf0000',lwd=1)+
  geom_smooth(method='gam', se=F)+
  coord_equal()+
  theme_linedraw()+
  theme(panel.grid = element_blank())

p2 <- pdat2[sample(.N,30000)] %>% 
  ggplot(data=.,aes(pred_t28m, t28m))+
  geom_point(alpha=0.25,size=0.5)+
  geom_abline(col='#cf0000',lwd=1)+
  geom_smooth(method='gam', se=F)+
  coord_equal()+
  theme_linedraw()+
  theme(panel.grid = element_blank())

ggsave(p1|p2, filename="figures/figure_pred_vs_obs_t28m.png",
       width=20,
       height=12.,
       units='cm')
