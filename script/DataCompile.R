#################################################################
# CH4cast version 2                                             #
# Ryan McClure                                                  #
# 3 December 2020                                               #
# Data Wrangling Script                                         #
#################################################################

if (!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(tidyverse,rjags,runjags,MCMCvis,lubridate,tidybayes,R2jags,ncdf4)

### Pull together all of the observed ebullition, hobo temperature, and catwalk temperature data from 2019

catwalk_main <- read_csv("./observed/Catwalk.csv", skip = 1)%>% 
  dplyr::filter(TIMESTAMP >= "2018-12-31 12:00:00") %>%
  dplyr::select(TIMESTAMP, wtr_2, wtr_3) %>%
  dplyr::filter(wtr_2 != "NAN") %>%
  dplyr::filter(wtr_3 != "NAN") %>%
  dplyr::filter(TIMESTAMP != "NAN") %>%
  dplyr::filter(TIMESTAMP != "YYYY_MM_DD_HH_MM_SS")%>%
  dplyr::mutate(mean_ws_temp = (as.numeric(wtr_2)+as.numeric(wtr_3))/2) %>%
  dplyr::select(TIMESTAMP, mean_ws_temp)%>%
  dplyr::mutate(time = as_date(TIMESTAMP)) %>%
  dplyr::group_by(time) %>%
  dplyr::summarize(temperature = mean(mean_ws_temp, na.rm = TRUE),
                   temperature_sd = sd(mean_ws_temp))

hobo <- read_csv("./observed/Hobo.csv")%>% 
  dplyr::filter(Date >= "2018-12-31 12:00:00") %>%
  dplyr::rename(time = Date)%>%
  dplyr::mutate(time = as_date(time))%>%
  dplyr::group_by(time) %>%
  dplyr::summarize(temperature = mean(Temp_C, na.rm = TRUE),
                   temperature_sd = sd(Temp_C, na.rm = TRUE))
  
hobo_t1 <- read_csv("./observed/t1eb1_sed_temp.csv")%>% 
  dplyr::mutate(time = as_date(time))%>%
  dplyr::group_by(time) %>%
  dplyr::summarize(temperature = mean(temp_C, na.rm = TRUE),
                   temperature_sd = sd(temp_C, na.rm = TRUE))%>%
  dplyr::mutate(trap_id = "t1eb1")%>%
  dplyr::full_join(., hobo, by = "time")%>%
  dplyr:: mutate(temperature.x = ifelse(is.na(temperature.x), temperature.y, temperature.x))%>%
  dplyr::mutate(temperature_sd.x = ifelse(is.na(temperature_sd.x), temperature_sd.y, temperature_sd.x))%>%
  dplyr::select(time, trap_id, temperature.x, temperature_sd.x) %>% rename(temperature = temperature.x, temperature_sd = temperature_sd.x)

hobo_t2 <- read_csv("./observed/t1eb2_sed_temp.csv")%>% 
  dplyr::mutate(time = as_date(time))%>%
  dplyr::group_by(time) %>%
  dplyr::summarize(temperature = mean(temp_C, na.rm = TRUE),
                   temperature_sd = sd(temp_C, na.rm = TRUE))%>%
  mutate(trap_id = "t1eb2")%>%
  dplyr::full_join(., hobo, by = "time")%>%
  dplyr:: mutate(temperature.x = ifelse(is.na(temperature.x), temperature.y, temperature.x))%>%
  dplyr::mutate(temperature_sd.x = ifelse(is.na(temperature_sd.x), temperature_sd.y, temperature_sd.x))%>%
  dplyr::select(time, trap_id, temperature.x, temperature_sd.x) %>% rename(temperature = temperature.x, temperature_sd = temperature_sd.x)

hobo_t3 <- read_csv("./observed/t1eb3_sed_temp.csv")%>% 
  dplyr::mutate(time = as_date(time))%>%
  dplyr::group_by(time) %>%
  dplyr::summarize(temperature = mean(temp_C, na.rm = TRUE),
                   temperature_sd = sd(temp_C, na.rm = TRUE))%>%
  mutate(trap_id = "t1eb3")%>%
  dplyr::full_join(., hobo, by = "time")%>%
  dplyr:: mutate(temperature.x = ifelse(is.na(temperature.x), temperature.y, temperature.x))%>%
  dplyr::mutate(temperature_sd.x = ifelse(is.na(temperature_sd.x), temperature_sd.y, temperature_sd.x))%>%
  dplyr::select(time, trap_id, temperature.x, temperature_sd.x) %>% rename(temperature = temperature.x, temperature_sd = temperature_sd.x)

hobo_t4 <- read_csv("./observed/t1eb4_sed_temp.csv")%>% 
  dplyr::mutate(time = as_date(time))%>%
  dplyr::group_by(time) %>%
  dplyr::summarize(temperature = mean(temp_C, na.rm = TRUE),
                   temperature_sd = sd(temp_C, na.rm = TRUE))%>%
  mutate(trap_id = "t1eb4")%>%
  dplyr::full_join(., hobo, by = "time")%>%
  dplyr:: mutate(temperature.x = ifelse(is.na(temperature.x), temperature.y, temperature.x))%>%
  dplyr::mutate(temperature_sd.x = ifelse(is.na(temperature_sd.x), temperature_sd.y, temperature_sd.x))%>%
  dplyr::select(time, trap_id, temperature.x, temperature_sd.x) %>% rename(temperature = temperature.x, temperature_sd = temperature_sd.x)

temp_model_t1 <- left_join(catwalk_main, hobo_t1, by = "time") %>% filter(time >= "2019-05-27") %>% filter(time <= "2019-11-07")%>%
  rename(cat_temp = temperature.x, cat_temp_sd = temperature_sd.x, hobo_temp = temperature.y, hobo_temp_sd = temperature_sd.y)%>%
  mutate(trap_id = "t1eb1") %>% select(time, trap_id, cat_temp, cat_temp_sd, hobo_temp, hobo_temp_sd)

temp_model_t2 <- left_join(catwalk_main, hobo_t2, by = "time") %>% filter(time >= "2019-05-27") %>% filter(time <= "2019-11-07")%>%
  rename(cat_temp = temperature.x, cat_temp_sd = temperature_sd.x, hobo_temp = temperature.y, hobo_temp_sd = temperature_sd.y)%>%
  mutate(trap_id = "t1eb2") %>% select(time, trap_id, cat_temp, cat_temp_sd, hobo_temp, hobo_temp_sd)

temp_model_t3 <- left_join(catwalk_main, hobo_t3, by = "time") %>% filter(time >= "2019-05-27") %>% filter(time <= "2019-11-07")%>%
  rename(cat_temp = temperature.x, cat_temp_sd = temperature_sd.x, hobo_temp = temperature.y, hobo_temp_sd = temperature_sd.y)%>%
  mutate(trap_id = "t1eb3") %>% select(time, trap_id, cat_temp, cat_temp_sd, hobo_temp, hobo_temp_sd)

temp_model_t4 <- left_join(catwalk_main, hobo_t4, by = "time") %>% filter(time >= "2019-05-27") %>% filter(time <= "2019-11-07")%>%
  rename(cat_temp = temperature.x, cat_temp_sd = temperature_sd.x, hobo_temp = temperature.y, hobo_temp_sd = temperature_sd.y)%>%
  mutate(trap_id = "t1eb4") %>% select(time, trap_id, cat_temp, cat_temp_sd, hobo_temp, hobo_temp_sd)

temp_all <- rbind(temp_model_t1, temp_model_t2, temp_model_t3, temp_model_t4)

# Read in the observed ebullition throughout the 2019
ebu <- read_csv("./observed/observed_ebu_rates.csv") %>%
  rename(time = date) %>%
  group_by(time, trap_id) %>%
  summarize(log_ebu_rate = mean(log_ebu_rate_mg_m2_d, na.rm = TRUE),
            log_ebu_rate_sd = sd(log_ebu_rate_mg_m2_d, na.rm = TRUE)) %>%
  group_by(trap_id)%>%
  mutate(log_ebu_rate_lag = lag(log_ebu_rate)) %>%
  mutate(log_ebu_rate_lag_sd = lag(log_ebu_rate_sd))

full_ebullition_model <- full_join(temp_all, ebu, by = c("trap_id", "time"))

### Pull together all of the forecast water temperature data from FLARE runs
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

require(reshape2)
FLARE_long <- melt(FLARE, id=c("full_time_day"))
FLARE_long$full_time_day <- as.POSIXct(strptime(FLARE_long$full_time_day, '%Y-%m-%d', tz = 'EST'))
full_ebullition_model$time <- as.POSIXct(strptime(full_ebullition_model$time, '%Y-%m-%d', tz = 'EST'))

ggplot()+
  geom_line(data = FLARE_long, aes(full_time_day, value, group = variable), color = "grey30")+
  geom_errorbar(data = full_ebullition_model, aes(x=time, ymin = cat_temp - cat_temp_sd, ymax = cat_temp + cat_temp_sd, position = "dodge"), color = "red")+
  geom_point(data = full_ebullition_model, aes(time, cat_temp), color = "red")+
  theme_classic()

