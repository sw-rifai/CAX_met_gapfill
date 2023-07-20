#### CLEAN SOIL OBS ############################################################

## Pablo Sanchez-Martinez
## Descr: unify water content and water potential measured in the soil and prepare
## datatable for gap filling

library(tidyverse); library(lubridate); library(feather); library(units); library(data.table)

#### SOIL VOLUMETRIC WATER CONTENT OBS ----------------------------------------- ####

## Plot A

vwc_a_2009_2023 <- read.csv("data/soil_obs/VWCControl_callibrated_2009.2022.csv", 
                            header = T, 
                            sep = ";") %>%
  # separate day, moth and year because some months had no 0 before and gives problems
  mutate(day = str_split(TIMESTAMP, "/", simplify = T)[, 1],
         month = str_split(TIMESTAMP, "/", simplify = T)[, 2],
         month = ifelse(as.numeric(month) < 10, paste0("0", as.numeric(month)), month),
         year = str_split(TIMESTAMP, "/", simplify = T)[, 3]) %>%
# Now we can transform to date
  mutate(date = as_date(paste0(year, "/", month, "/", day)),
         date = as_datetime(paste0(date, " ", hour)),
         soil_volumetric_water_content_sup = rowMeans(select(., callSup_E_P1, callSup_D_P1, callSUP_P2), na.rm = T),
         soil_volumetric_water_content_50cm = rowMeans(select(., callX50cm_E_P1, callX50cm_D_P1, calx50cm_P2), na.rm = T), 
         soil_volumetric_water_content_100cm = rowMeans(select(., callX100cm_E_P1, callx100cm_P2), na.rm = T),
         soil_volumetric_water_content_250cm = rowMeans(select(., callX250cm_E_P1, callx250cm_P2), na.rm = T),
         soil_volumetric_water_content_400cm = rowMeans(select(., callX400cm_E_P1, callx400cm_P2), na.rm = T)) %>%
  select(date, soil_volumetric_water_content_sup, soil_volumetric_water_content_50cm, soil_volumetric_water_content_100cm, soil_volumetric_water_content_250cm, soil_volumetric_water_content_400cm) %>%
  filter(!is.na(date))

## Plot B

vwc_b_2009_2023 <- read.csv("data/soil_obs/VWCtfe_callibrated_2009.2022.csv", 
                            header = T, 
                            sep = ";") %>%
  filter(!is.na(TIMESTAMP)) %>%
  # separate day, moth and year because some months had no 0 before and gives problems
  mutate(day = str_split(TIMESTAMP, "/", simplify = T)[, 1],
         month = str_split(TIMESTAMP, "/", simplify = T)[, 2],
         month = ifelse(as.numeric(month) < 10, paste0("0", as.numeric(month)), month),
         year = str_split(TIMESTAMP, "/", simplify = T)[, 3]) %>%
  # Now we can transform to date
  mutate(date = as_date(paste0(year, "/", month, "/", day)),
         date = as_datetime(paste0(date, " ", hour)),
         soil_volumetric_water_content_sup = rowMeans(select(., callSup_E_P1, callSup_D_P1, callSup_D_P2, callSup_E_P2), na.rm = T),
         soil_volumetric_water_content_50cm = rowMeans(select(., callX50cm_E_P1, callX50cm_D_P1, callX50cm_D_P2, callX50cm_E_P2), na.rm = T), 
         soil_volumetric_water_content_100cm = rowMeans(select(., callX100cm_D_P1, callX100cm_D_P1_new, callX100cm_E_P1, callX100cm_E_P1_new, callX100cm_D_P2, callX100cm_D_P2_new, callX100cm_E_P2, callX100cm_E_P2_new), na.rm = T) * 100,
         soil_volumetric_water_content_250cm = rowMeans(select(., callX250cm_E_P1, callX250cm_E_P2), na.rm = T),
         soil_volumetric_water_content_400cm = rowMeans(select(., callX400cm_E_P1, callX400cm_E_P2), na.rm = T)) %>%
  select(date, soil_volumetric_water_content_sup, soil_volumetric_water_content_50cm, soil_volumetric_water_content_100cm, soil_volumetric_water_content_250cm, soil_volumetric_water_content_400cm) %>%
  filter(!is.na(date)) # in this case, some dates may be wrong, and it causes some warnings (31/04/2020 doesn't exist), I'm filtering out these days


#### SOIL WATER POTENTIAL ------------------------------------------------------ ####

## Plot A

wp_a_2019_2023 <- read.csv("data/soil_obs/WP_A_2019_2023.csv", 
                            header = T, 
                            sep = ";") %>%
  filter(!is.na(day), !is.na(month), !is.na(year)) %>%
  mutate(month = ifelse(as.numeric(month) < 10, paste0("0", as.numeric(month)), month),
         date = as_date(paste0(year, "/", month, "/", day)),
         date = as_datetime(paste0(date, " ", hour)),
         date = round_date(date, unit="hours"),
         soil_water_potential_SUP_MPa = SWP_SUP_Avg/1000,
         soil_water_potential_50cm_MPa = SWP_50cm_Avg/1000,
         soil_water_potential_100cm_MPa = SWP_100cm_Avg/1000,
         soil_water_potential_250cm_MPa = SWP_250cm_Avg/1000,
         soil_water_potential_400cm_MPa = SWP_400cm_Avg/1000,
         soil_water_potential_600cm_MPa = SWP_600cm_Avg/1000,
         soil_water_potential_800cm_MPa = SWP_800cm_Avg/1000) %>%
  select(-day, -month, -hour, -year, -SWP_SUP_Avg, -SWP_50cm_Avg, -SWP_100cm_Avg, -SWP_250cm_Avg, -SWP_400cm_Avg, -SWP_600cm_Avg, -SWP_800cm_Avg) %>%
  filter(!is.na(date))

wp_a_20019_2023_hourly <- aggregate(wp_a_2019_2023[, -which(names(wp_a_2019_2023) == "date")], 
                                    by = list(wp_a_2019_2023$date), 
                                    FUN = mean, 
                                    na.rm = T)  %>%
  select(date = Group.1, everything())

## Plot B

wp_b_2019_2023 <- read.csv("data/soil_obs/WP_B_2019_2023.csv", 
                            header = T, 
                            sep = ";") %>%
  filter(!is.na(day), !is.na(month), !is.na(year)) %>%
  mutate(month = ifelse(as.numeric(month) < 10, paste0("0", as.numeric(month)), month),
         date = as_date(paste0(year, "/", month, "/", day)),
         date = as_datetime(paste0(date, " ", hour)),
         date = round_date(date, unit="hours"),
         soil_water_potential_SUP_MPa = SWP_SUP_Avg/1000,
         soil_water_potential_50cm_MPa = SWP_50cm_Avg/1000,
         soil_water_potential_100cm_MPa = SWP_100cm_Avg/1000,
         soil_water_potential_250cm_MPa = SWP_250cm_Avg/1000,
         soil_water_potential_400cm_MPa = SWP_400cm_Avg/1000,
         soil_water_potential_600cm_MPa = SWP_600cm_Avg/1000,
         soil_water_potential_800cm_MPa = SWP_800cm_Avg/1000) %>%
  select(-day, -month, -hour, -year, -SWP_SUP_Avg, -SWP_50cm_Avg, -SWP_100cm_Avg, -SWP_250cm_Avg, -SWP_400cm_Avg, -SWP_600cm_Avg, -SWP_800cm_Avg) %>%
  filter(!is.na(date))

wp_b_2019_2023_hourly <- aggregate(wp_b_2019_2023[, -which(names(wp_b_2019_2023) == "date")], 
                                    by = list(wp_b_2019_2023$date), 
                                    FUN = mean, 
                                    na.rm = T) %>%
  select(date = Group.1, everything()) 

### Merge WP and VWC and save ####

## Plot A

vwc_wp_a_2009_2023 <- merge(vwc_a_2009_2023, wp_a_2019_2023, by = "date", all = T)

fwrite(vwc_wp_a_2009_2023, file=paste0("data/soil_obs/cax_a_soil_obs_2009_2023_hourly.csv"))

## Plot B

vwc_wp_b_2009_2023 <- merge(vwc_b_2009_2023, wp_b_2019_2023, by = "date", all = T)

fwrite(vwc_wp_b_2009_2023, file=paste0("data/soil_obs/cax_b_soil_obs_2009_2023_hourly.csv"))
