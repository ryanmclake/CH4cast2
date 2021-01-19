#################################################################
# CH4cast version 2                                             #
# Ryan McClure                                                  #
# 3 December 2020                                               #
# Data Wrangling Script                                         #
#################################################################

if (!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(tidyverse,rjags,runjags,MCMCvis,lubridate,tidybayes,R2jags,ncdf4,reshape2)

### Pull together all of the observed ebullition, hobo temperature, and catwalk temperature data from 2019

# pull in catwalk data
#########################################################################
# catwalk_main <- read_csv("./observed/Catwalk.csv", skip = 1)%>% 
#   dplyr::filter(TIMESTAMP >= "2018-12-31 12:00:00") %>%
#   dplyr::select(TIMESTAMP, wtr_2, wtr_3) %>%
#   dplyr::filter(wtr_2 != "NAN") %>%
#   dplyr::filter(wtr_3 != "NAN") %>%
#   dplyr::filter(TIMESTAMP != "NAN") %>%
#   dplyr::filter(TIMESTAMP != "YYYY_MM_DD_HH_MM_SS")%>%
#   dplyr::mutate(mean_ws_temp = (as.numeric(wtr_2)+as.numeric(wtr_3))/2) %>%
#   dplyr::select(TIMESTAMP, mean_ws_temp)%>%
#   dplyr::mutate(time = as_date(TIMESTAMP)) %>%
#   dplyr::group_by(time) %>%
#   dplyr::summarize(temperature = mean(mean_ws_temp, na.rm = TRUE),
#                    temperature_sd = sd(mean_ws_temp))
#########################################################################


# pull in meteorological data
met <- read_csv("./observed/Met_final_2015_2019.csv")

# organize  the met data that is used to train the model from Air Temp to SWI temperatures
met_sum <- met %>%
  select(DateTime, AirTemp_Average_C)%>%
  mutate(time = as_date(DateTime))%>%
  group_by(time) %>%
  summarize(temperature = mean(AirTemp_Average_C, na.rm = TRUE),
                   temperature_sd = sd(AirTemp_Average_C))

# Pull in hobo data 
# Hobo data for Trap 1
hobo_t1 <- read_csv("./observed/EDI_DATA_HOBO_TEMPS_2017_2019.csv")%>%
  filter(Site == "T1e1") %>%
  dplyr::rename(time = DateTime)%>%
  dplyr::mutate(time = as_date(time))%>%
  dplyr::group_by(time) %>%
  dplyr::summarize(temperature = mean(Sed_temp, na.rm = TRUE),
                   temperature_sd = sd(Sed_temp, na.rm = TRUE))

temp_model_t1 <- left_join(met_sum, hobo_t1, by = "time")%>%
  rename(air_temp = temperature.x, air_temp_sd = temperature_sd.x, hobo_temp = temperature.y, hobo_temp_sd = temperature_sd.y)%>%
  mutate(trap_id = "T1e1") %>% select(time, trap_id, air_temp, air_temp_sd, hobo_temp, hobo_temp_sd)

# Hobo data for Trap 2
hobo_t2 <- read_csv("./observed/EDI_DATA_HOBO_TEMPS_2017_2019.csv")%>%
  filter(Site == "T1e2") %>%
  dplyr::rename(time = DateTime)%>%
  dplyr::mutate(time = as_date(time))%>%
  dplyr::group_by(time) %>%
  dplyr::summarize(temperature = mean(Sed_temp, na.rm = TRUE),
                   temperature_sd = sd(Sed_temp, na.rm = TRUE))

temp_model_t2 <- left_join(met_sum, hobo_t2, by = "time")%>%
  rename(air_temp = temperature.x, air_temp_sd = temperature_sd.x, hobo_temp = temperature.y, hobo_temp_sd = temperature_sd.y)%>%
  mutate(trap_id = "T1e2") %>% select(time, trap_id, air_temp, air_temp_sd, hobo_temp, hobo_temp_sd)


# Hobo data for Trap 3
hobo_t3 <- read_csv("./observed/EDI_DATA_HOBO_TEMPS_2017_2019.csv")%>%
  filter(Site == "T1e3") %>%
  dplyr::rename(time = DateTime)%>%
  dplyr::mutate(time = as_date(time))%>%
  dplyr::group_by(time) %>%
  dplyr::summarize(temperature = mean(Sed_temp, na.rm = TRUE),
                   temperature_sd = sd(Sed_temp, na.rm = TRUE))

temp_model_t3 <- left_join(met_sum, hobo_t3, by = "time")%>%
  rename(air_temp = temperature.x, air_temp_sd = temperature_sd.x, hobo_temp = temperature.y, hobo_temp_sd = temperature_sd.y)%>%
  mutate(trap_id = "T1e3") %>% select(time, trap_id, air_temp, air_temp_sd, hobo_temp, hobo_temp_sd)

# Hobo data for Trap 4
hobo_t4 <- read_csv("./observed/EDI_DATA_HOBO_TEMPS_2017_2019.csv")%>%
  filter(Site == "T1e4") %>%
  dplyr::rename(time = DateTime)%>%
  dplyr::mutate(time = as_date(time))%>%
  dplyr::group_by(time) %>%
  dplyr::summarize(temperature = mean(Sed_temp, na.rm = TRUE),
                   temperature_sd = sd(Sed_temp, na.rm = TRUE))

temp_model_t4 <- left_join(met_sum, hobo_t4, by = "time")%>%
  rename(air_temp = temperature.x, air_temp_sd = temperature_sd.x, hobo_temp = temperature.y, hobo_temp_sd = temperature_sd.y)%>%
  mutate(trap_id = "T1e4") %>% select(time, trap_id, air_temp, air_temp_sd, hobo_temp, hobo_temp_sd)

temp_all <- rbind(temp_model_t1, temp_model_t2, temp_model_t3, temp_model_t4)


# Read in the observed ebullition throughout the 2019
ebu <- read_csv("./observed/EDI_DATA_EBU_DIFF_DEPTH_2015_2019.csv") %>%
  rename(time = DateTime) %>%
  filter(Transect == "T1")%>%
  select(time, Site, Ebu_rate)%>%
  group_by(time, Site) %>%
  summarize(log_ebu_rate = mean(Ebu_rate, na.rm = TRUE),
            log_ebu_rate_sd = sd(Ebu_rate, na.rm = TRUE))%>%
  rename(trap_id = Site)

full_ebullition_model <- full_join(temp_all, ebu, by = c("trap_id", "time")) %>%
  filter(time >= "2017-04-30") 
  
full_ebullition_model$log_ebu_rate[is.nan(full_ebullition_model$log_ebu_rate)] <- NA











### Pull together all of the forecast water temperature data from FLARE runs
# These are the individual FLARE forecasts from the day we sampled ebullition out to the next day we would be sampling. 
# Sadly - this is repeated because each .nc forecast file is its own separate inetity. 
nc <- nc_open("./forecasted/forecast_H_2019_06_07_2019_06_10_F_10_552020_19_34.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_prediction_wk1 <- temp %>% 
  filter(full_time_day >= "2019-06-10") %>% 
  filter(full_time_day <= "2019-06-18")

nc <- nc_open("./forecasted/forecast_H_2019_06_14_2019_06_17_F_10_552020_20_49.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_prediction_wk2 <- temp %>% 
  filter(full_time_day >= "2019-06-18") %>% 
  filter(full_time_day <= "2019-06-25")

nc <- nc_open("./forecasted/forecast_H_2019_06_21_2019_06_24_F_10_552020_22_47.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_prediction_wk3 <- temp %>% 
  filter(full_time_day >= "2019-06-25") %>% 
  filter(full_time_day <= "2019-07-02")

nc <- nc_open("./forecasted/forecast_H_2019_06_28_2019_07_01_F_10_552020_23_44.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_prediction_wk4 <- temp %>% 
  filter(full_time_day >= "2019-07-02") %>% 
  filter(full_time_day <= "2019-07-09")

nc <- nc_open("./forecasted/forecast_H_2019_07_05_2019_07_08_F_10_562020_0_12.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_prediction_wk5 <- temp %>% 
  filter(full_time_day >= "2019-07-09") %>% 
  filter(full_time_day <= "2019-07-16")

nc <- nc_open("./forecasted/forecast_H_2019_07_12_2019_07_15_F_10_562020_0_33.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_prediction_wk6 <- temp %>% 
  filter(full_time_day >= "2019-07-16") %>% 
  filter(full_time_day <= "2019-07-23")

nc <- nc_open("./forecasted/forecast_H_2019_07_19_2019_07_22_F_10_562020_0_47.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_prediction_wk7 <- temp %>% 
  filter(full_time_day >= "2019-07-23") %>% 
  filter(full_time_day <= "2019-07-30")

nc <- nc_open("./forecasted/forecast_H_2019_07_26_2019_07_29_F_10_562020_1_6.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_prediction_wk8 <- temp %>% 
  filter(full_time_day >= "2019-07-30") %>% 
  filter(full_time_day <= "2019-08-06")

nc <- nc_open("./forecasted/forecast_H_2019_08_02_2019_08_05_F_10_562020_1_20.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_prediction_wk9 <- temp %>% 
  filter(full_time_day >= "2019-08-06") %>% 
  filter(full_time_day <= "2019-08-13")

nc <- nc_open("./forecasted/forecast_H_2019_08_09_2019_08_12_F_10_562020_1_31.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_prediction_wk10 <- temp %>% 
  filter(full_time_day >= "2019-08-13") %>% 
  filter(full_time_day <= "2019-08-20")

nc <- nc_open("./forecasted/forecast_H_2019_08_16_2019_08_19_F_10_562020_1_52.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_prediction_wk11 <- temp %>% 
  filter(full_time_day >= "2019-08-20") %>% 
  filter(full_time_day <= "2019-08-29")

nc <- nc_open("./forecasted/forecast_H_2019_08_25_2019_08_28_F_10_562020_2_5.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_prediction_wk12 <- temp %>% 
  filter(full_time_day >= "2019-08-29") %>% 
  filter(full_time_day <= "2019-09-03")

nc <- nc_open("./forecasted/forecast_H_2019_08_30_2019_09_02_F_10_562020_2_22.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_prediction_wk13 <- temp %>% 
  filter(full_time_day >= "2019-09-03") %>% 
  filter(full_time_day <= "2019-09-12")

nc <- nc_open("./forecasted/forecast_H_2019_09_08_2019_09_11_F_10_562020_10_15.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_prediction_wk14 <- temp %>% 
  filter(full_time_day >= "2019-09-12") %>% 
  filter(full_time_day <= "2019-09-21")


nc <- nc_open("./forecasted/forecast_H_2019_09_17_2019_09_20_F_10_562020_10_27.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_prediction_wk15 <- temp %>% 
  filter(full_time_day >= "2019-09-21") %>% 
  filter(full_time_day <= "2019-09-28")

nc <- nc_open("./forecasted/forecast_H_2019_09_24_2019_09_27_F_10_562020_10_37.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_prediction_wk16 <- temp %>% 
  filter(full_time_day >= "2019-09-28") %>% 
  filter(full_time_day <= "2019-10-03")

nc <- nc_open("./forecasted/forecast_H_2019_09_29_2019_10_02_F_10_562020_10_53.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_prediction_wk17 <- temp %>% 
  filter(full_time_day >= "2019-10-03") %>% 
  filter(full_time_day <= "2019-10-12")

nc <- nc_open("./forecasted/forecast_H_2019_10_08_2019_10_11_F_10_562020_11_25.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_prediction_wk18 <- temp %>% 
  filter(full_time_day >= "2019-10-12") %>% 
  filter(full_time_day <= "2019-10-17")

nc <- nc_open("./forecasted/forecast_H_2019_10_13_2019_10_16_F_10_562020_11_38.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_prediction_wk19 <- temp %>% 
  filter(full_time_day >= "2019-10-17") %>% 
  filter(full_time_day <= "2019-10-24")

nc <- nc_open("./forecasted/forecast_H_2019_10_20_2019_10_23_F_10_562020_11_53.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_prediction_wk20 <- temp %>% 
  filter(full_time_day >= "2019-10-24") %>% 
  filter(full_time_day <= "2019-10-31")

nc <- nc_open("./forecasted/forecast_H_2019_10_27_2019_10_30_F_10_562020_12_6.nc")
t <- ncvar_get(nc,'time')
full_time <- as.POSIXct(t, origin = '1970-01-01 00:00.00 UTC', tz = "EST")
full_time_day <- strftime(full_time, format="%Y-%m-%d")
temp <- ncvar_get(nc,'temp')
nc_close(nc)
time_day <- as.data.frame(full_time_day)
temp1 <- as.data.frame(cbind(time_day, temp[1:14,1:210,8]))
temp2 <- as.data.frame(cbind(time_day, temp[1:14,1:210,9]))
temp3 <- as.data.frame(cbind(time_day, temp[1:14,1:210,10]))
temp <- rbind(temp1, temp2, temp3, deparse.level = 1)
temp <- temp %>% group_by(full_time_day) %>% summarise_all(funs(mean)) %>% arrange(full_time_day)
temp$full_time_day <- as.POSIXct(strptime(temp$full_time_day, '%Y-%m-%d', tz = 'EST'))
temp_prediction_wk21 <- temp %>% 
  filter(full_time_day >= "2019-10-31") %>% 
  filter(full_time_day <= "2019-11-07")

FLARE <- rbind(temp_prediction_wk1, 
               temp_prediction_wk2,
               temp_prediction_wk3,
               temp_prediction_wk4,
               temp_prediction_wk5,
               temp_prediction_wk6,
               temp_prediction_wk7,
               temp_prediction_wk8,
               temp_prediction_wk9,
               temp_prediction_wk10,
               temp_prediction_wk11,
               temp_prediction_wk12,
               temp_prediction_wk13,
               temp_prediction_wk14,
               temp_prediction_wk15,
               temp_prediction_wk16,
               temp_prediction_wk17,
               temp_prediction_wk18,
               temp_prediction_wk19,
               temp_prediction_wk20,
               temp_prediction_wk21)
colnames(FLARE)[-1] = paste0('ens_',colnames(FLARE)[-1])

FLARE_long <- melt(FLARE, id=c("full_time_day"))
FLARE_long$full_time_day <- as.POSIXct(strptime(FLARE_long$full_time_day, '%Y-%m-%d', tz = 'EST'))
full_ebullition_model$time <- as.POSIXct(strptime(full_ebullition_model$time, '%Y-%m-%d', tz = 'EST'))


# PLOT THE TEMPERATURE FORECASTS AGAINST THE OBSERVED CATWALK DATA --> This is basically FLARE proper with the ENKF
ggplot()+
  geom_line(data = FLARE_long, aes(full_time_day, value, group = variable), color = "grey30")+
  geom_errorbar(data = full_ebullition_model, aes(x=time, ymin = cat_temp - cat_temp_sd, ymax = cat_temp + cat_temp_sd, position = "dodge"), color = "red")+
  geom_point(data = full_ebullition_model, aes(time, cat_temp), color = "red")+
  theme_classic()

