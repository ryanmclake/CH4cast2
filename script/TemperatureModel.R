#################################################################
# CH4cast version 2                                             #
# Ryan McClure                                                  #
# 3 December 2020                                               #
# Data Wrangling Script                                         #
#################################################################

if (!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(tidyverse,rjags,runjags,MCMCvis,lubridate,tidybayes,R2jags)

### RJAGS temperature model using hobo and catwalk data going back to deployment of the catwalk of FLARE in 2019

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

# Develop the Temperature JAGS model
temp_jags <- function(){
  for (i in 1:N){
    hobo_temp[i] ~ dnorm(mu[i], tau) # tau is precision (1 / variance)
    mu[i] <- beta1 + beta2 * cat_temp[i]
  }
  # Priors:
  beta1 ~ dnorm(0, 0.001) # intercept
  beta2 ~ dnorm(0, 0.001) # parameter estimate around the catwalk
  sigma ~ dunif(0, 100) # standard deviation
  tau <- 1 / (sigma * sigma) # sigma^2 doesn't work in JAGS
}
init_values <- function(){
  list(beta1 = rnorm(1), beta2 = rnorm(1), sigma = runif(1))
}
output <- c("beta1", "beta2", "sigma")


# Develop the Ebullition Jags Model
ebu_jags <- function(){
  for (i in 2:N){
    ebu.obs[i] ~ dnorm(ebu.latent[i], tau_obs)
    ebu.latent[i] ~ dnorm(ebu.hat[i], tau_add)
    ebu.hat[i] <- beta1 + beta2*ebu.latent[i-1] + beta3*hobo_temp[i]
  }
  
    ebu.latent[1] ~ dnorm(ebu.obs[1], tau_obs)
  
  # Priors:
  beta1 ~ dnorm(0,1/10000000) # intercept
  beta2 ~ dnorm(0,1/10000000) # parameter estimate around the lagged ebu
  beta3 ~ dnorm(0,1/10000000) # paramter estimate around the hobo temp
  sigma ~ dunif(0, 100) # standard deviation
  tau_obs <- 1 / (sigma * sigma) # sigma^2 doesn't work in JAGS
  tau_add ~ dgamma(0.1,0.1)
}

init_values <- function(){
  list(beta1 = rnorm(1), beta2 = rnorm(1), beta3 = rnorm(1), sigma = runif(1))
}
output <- c("beta1", "beta2", "beta3", "sigma")

# Run Jags Daily for each ebullition trap between 10Jun20 and 07Nov20
# Ebullition trap #1
full_ebullition_model_short_eb1 <- full_ebullition_model %>% filter(trap_id == "t1eb1") %>% filter(time >= "2019-06-17")
fit_ebu_eb1 <- list(matrix(NA, ncol = 4))

for(s in 1:length(unique(full_ebullition_model_short_eb1$time))){
ebu_model_jags <- with(full_ebullition_model_short_eb1, list(hobo_temp = hobo_temp, ebu.obs = log_ebu_rate, N = length(unique(full_ebullition_model_short_eb1$time))))
fit_ebu_eb1[s] <- as.mcmc(jags(data = ebu_model_jags, inits = init_values, parameters.to.save = output, model.file = ebu_jags,
                            n.chains = 3, n.iter = 100000, n.burnin = 20000, n.thin = 10, DIC = F))
}



# Ebullition trap #2
full_ebullition_model_short_eb2 <- full_ebullition_model %>% filter(trap_id == "t1eb2") %>% filter(time >= "2019-06-10")
fit_ebu_eb2 <- list(matrix(NA, ncol = 4))

for(s in 1:length(unique(full_ebullition_model_short_eb2$time))){
ebu_model_jags <- with(full_ebullition_model_short_eb2, list(hobo_temp = hobo_temp, ebu.obs = log_ebu_rate, N = length(unique(full_ebullition_model_short_eb2$time))))
fit_ebu_eb2[s] <- as.mcmc(jags(data = ebu_model_jags, inits = init_values, parameters.to.save = output, model.file = ebu_jags,
                            n.chains = 3, n.iter = 100000, n.burnin = 20000, n.thin = 10, DIC = F))
}


# Ebullition trap #3
full_ebullition_model_short_eb3 <- full_ebullition_model %>% filter(trap_id == "t1eb3") %>% filter(time >= "2019-06-24")
fit_ebu_eb3 <- list(matrix(NA, ncol = 4))

for(s in 1:length(unique(full_ebullition_model_short_eb3$time))){
  ebu_model_jags <- with(full_ebullition_model_short_eb3, list(hobo_temp = hobo_temp, ebu.obs = log_ebu_rate, N = length(unique(full_ebullition_model_short_eb3$time))))
  fit_ebu_eb3[s] <- as.mcmc(jags(data = ebu_model_jags, inits = init_values, parameters.to.save = output, model.file = ebu_jags,
                                 n.chains = 3, n.iter = 100000, n.burnin = 20000, n.thin = 10, DIC = F))
}

# Ebullition trap #4
full_ebullition_model_short_eb4 <- full_ebullition_model %>% filter(trap_id == "t1eb4") %>% filter(time >= "2019-06-10")
fit_ebu_eb4 <- list(matrix(NA, ncol = 4))

for(s in 1:length(unique(full_ebullition_model_short_eb4$time))){
  ebu_model_jags <- with(full_ebullition_model_short_eb4, list(hobo_temp = hobo_temp, ebu.obs = log_ebu_rate, N = length(unique(full_ebullition_model_short_eb4$time))))
  fit_ebu_eb4[s] <- as.mcmc(jags(data = ebu_model_jags, inits = init_values, parameters.to.save = output, model.file = ebu_jags,
                                 n.chains = 3, n.iter = 100000, n.burnin = 20000, n.thin = 10, DIC = F))
}

#Temp Jags Model for Ebullition trap #1
full_model_model_short_eb1 <- full_ebullition_model %>% filter(trap_id == "t1eb1") %>% filter(time >= "2019-06-10")
fit_temp_eb1 <- list(matrix(NA, ncol = 4))

for(s in 1:length(full_model_model_short_eb1$time))){

  temp_model_jags <- with(full_model_model_short_eb1, list(hobo_temp = hobo_temp, cat_temp = cat_temp, N = length(unique(full_model_model_short_eb1$time))))

  fit_temp_eb1[s] <- as.mcmc(jags(data = temp_model_jags, inits = init_values, parameters.to.save = output, model.file = temp_jags,
                n.chains = 3, n.iter = 12000, n.burnin = 2000, n.thin = 10, DIC = F))
}



