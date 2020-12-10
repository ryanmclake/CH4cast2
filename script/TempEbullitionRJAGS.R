if (!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(tidyverse,rjags,runjags,MCMCvis,lubridate,tidybayes,R2jags)

# Develop the Temperature JAGS model
#############################################################################################################################
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
init_values_temp <- function(){
  list(beta1 = rnorm(1), beta2 = rnorm(1), sigma = runif(1))
}
output_temp <- c("beta1", "beta2", "sigma")

#Trap #1 temperature model
full_model_short_eb1 <- full_ebullition_model %>% filter(trap_id == "t1eb1") %>% filter(time >= "2019-06-10")
subsetdate_eb1 <- Reduce(rbind, split(full_model_short_eb1, seq(as.factor(full_model_short_eb1$time))), accumulate = T)
output_model_eb1 <- lapply(subsetdate_eb1, function(x) {
  N <- nrow(x)
  hobo_temp <- x$hobo_temp
  cat_temp <- x$cat_temp
  data <- list("N", "hobo_temp", "cat_temp")
  myinits <- init_values_temp
  parameters <- c("beta1", "beta2", "sigma")
  samples <- combine.mcmc(as.mcmc(jags(data, inits=myinits, parameters,
                                       model.file = temp_jags, 
                                       n.chains=3, n.iter=20000, 
                                       n.burnin=5000, n.thin=10, DIC=F)))
  return(samples)
})

trap1_tempmodel_out <- map_df(output_model_eb1, ~as.data.frame(t(.)))

# beta1 term
beta1_t1 <- trap1_tempmodel_out[grep("beta1", rownames(trap1_tempmodel_out)),]
beta1_t1 <- cbind(full_model_short_eb1$time, beta1_t1, deparse.level = 1)
beta1_t1 <- melt(beta1_t1, id = "full_model_short_eb1$time")

beta1_t1 <- beta1_t1 %>% rename(time = `full_model_short_eb1$time`) %>%
  group_by(time)%>%
  summarize(beta1_t1_mean = mean(value),
            beta1_t1_sd = sd(value))

# beta2 term
beta2_t1 <- trap1_tempmodel_out[grep("beta2", rownames(trap1_tempmodel_out)),]
beta2_t1 <- cbind(full_model_short_eb1$time, beta2_t1, deparse.level = 1)
beta2_t1 <- melt(beta2_t1, id = "full_model_short_eb1$time")

beta2_t1 <- beta2_t1 %>% rename(time = `full_model_short_eb1$time`) %>%
  group_by(time)%>%
  summarize(beta2_t1_mean = mean(value),
            beta2_t1_sd = sd(value))
# sigma term

sigma_t1 <- trap1_tempmodel_out[grep("sigma", rownames(trap1_tempmodel_out)),]
sigma_t1 <- cbind(full_model_short_eb1$time, sigma_t1, deparse.level = 1)
sigma_t1 <- melt(sigma_t1, id = "full_model_short_eb1$time")

sigma_t1 <- sigma_t1 %>% rename(time = `full_model_short_eb1$time`) %>%
  group_by(time)%>%
  summarize(sigma_t1_mean = mean(value),
            sigma_t1_sd = sd(value))

#Trap #2 temperature model
full_model_short_eb2 <- full_ebullition_model %>% filter(trap_id == "t1eb2") %>% filter(time >= "2019-06-10")
subsetdate_eb2 <- Reduce(rbind, split(full_model_short_eb2, seq(as.factor(full_model_short_eb2$time))), accumulate = T)
output_model_eb2 <- lapply(subsetdate_eb2, function(x) {
  N <- nrow(x)
  hobo_temp <- x$hobo_temp
  cat_temp <- x$cat_temp
  data <- list("N", "hobo_temp", "cat_temp")
  myinits <- init_values_temp
  parameters <- c("beta1", "beta2", "sigma")
  samples <- combine.mcmc(as.mcmc(jags(data, inits=myinits, parameters,
                                       model.file = temp_jags, 
                                       n.chains=3, n.iter=20000, 
                                       n.burnin=5000, n.thin=10, DIC=F)))
  return(samples)
})

trap2_tempmodel_out <- map_df(output_model_eb2, ~as.data.frame(t(.)))

# beta1 term
beta1_t2 <- trap2_tempmodel_out[grep("beta1", rownames(trap2_tempmodel_out)),]
beta1_t2 <- cbind(full_model_short_eb2$time, beta1_t2, deparse.level = 1)
beta1_t2 <- melt(beta1_t2, id = "full_model_short_eb2$time")

beta1_t2 <- beta1_t2 %>% rename(time = `full_model_short_eb2$time`) %>%
  group_by(time)%>%
  summarize(beta1_t2_mean = mean(value),
            beta1_t2_sd = sd(value))

# beta2 term
beta2_t2 <- trap2_tempmodel_out[grep("beta2", rownames(trap2_tempmodel_out)),]
beta2_t2 <- cbind(full_model_short_eb2$time, beta2_t2, deparse.level = 1)
beta2_t2 <- melt(beta2_t2, id = "full_model_short_eb2$time")

beta2_t2 <- beta2_t2 %>% rename(time = `full_model_short_eb2$time`) %>%
  group_by(time)%>%
  summarize(beta2_t2_mean = mean(value),
            beta2_t2_sd = sd(value))
# sigma term
sigma_t2 <- trap2_tempmodel_out[grep("sigma", rownames(trap2_tempmodel_out)),]
sigma_t2 <- cbind(full_model_short_eb2$time, sigma_t2, deparse.level = 1)
sigma_t2 <- melt(sigma_t2, id = "full_model_short_eb2$time")

sigma_t2 <- sigma_t2 %>% rename(time = `full_model_short_eb2$time`) %>%
  group_by(time)%>%
  summarize(sigma_t2_mean = mean(value),
            sigma_t2_sd = sd(value))

#Trap #3 temperature model
full_model_short_eb3 <- full_ebullition_model %>% filter(trap_id == "t1eb3") %>% filter(time >= "2019-06-10")
subsetdate_eb3 <- Reduce(rbind, split(full_model_short_eb3, seq(as.factor(full_model_short_eb3$time))), accumulate = T)
output_model_eb3 <- lapply(subsetdate_eb3, function(x) {
  N <- nrow(x)
  hobo_temp <- x$hobo_temp
  cat_temp <- x$cat_temp
  data <- list("N", "hobo_temp", "cat_temp")
  myinits <- init_values_temp
  parameters <- c("beta1", "beta2", "sigma")
  samples <- combine.mcmc(as.mcmc(jags(data, inits=myinits, parameters,
                                       model.file = temp_jags, 
                                       n.chains=3, n.iter=20000, 
                                       n.burnin=5000, n.thin=10, DIC=F)))
  return(samples)
})

trap3_tempmodel_out <- map_df(output_model_eb3, ~as.data.frame(t(.)))

# beta1 term
beta1_t3 <- trap3_tempmodel_out[grep("beta1", rownames(trap3_tempmodel_out)),]
beta1_t3 <- cbind(full_model_short_eb3$time, beta1_t3, deparse.level = 1)
beta1_t3 <- melt(beta1_t3, id = "full_model_short_eb3$time")

beta1_t3 <- beta1_t3 %>% rename(time = `full_model_short_eb3$time`) %>%
  group_by(time)%>%
  summarize(beta1_t3_mean = mean(value),
            beta1_t3_sd = sd(value))

# beta2 term
beta2_t3 <- trap3_tempmodel_out[grep("beta2", rownames(trap3_tempmodel_out)),]
beta2_t3 <- cbind(full_model_short_eb3$time, beta2_t3, deparse.level = 1)
beta2_t3 <- melt(beta2_t3, id = "full_model_short_eb3$time")

beta2_t3 <- beta2_t3 %>% rename(time = `full_model_short_eb3$time`) %>%
  group_by(time)%>%
  summarize(beta2_t3_mean = mean(value),
            beta2_t3_sd = sd(value))
# sigma term
sigma_t3 <- trap3_tempmodel_out[grep("sigma", rownames(trap3_tempmodel_out)),]
sigma_t3 <- cbind(full_model_short_eb3$time, sigma_t3, deparse.level = 1)
sigma_t3 <- melt(sigma_t3, id = "full_model_short_eb3$time")

sigma_t3 <- sigma_t3 %>% rename(time = `full_model_short_eb3$time`) %>%
  group_by(time)%>%
  summarize(sigma_t3_mean = mean(value),
            sigma_t3_sd = sd(value))

#Trap #4 temperature model
full_model_short_eb4 <- full_ebullition_model %>% filter(trap_id == "t1eb4") %>% filter(time >= "2019-06-10")
subsetdate_eb4 <- Reduce(rbind, split(full_model_short_eb4, seq(as.factor(full_model_short_eb4$time))), accumulate = T)
output_model_eb4 <- lapply(subsetdate_eb4, function(x) {
  N <- nrow(x)
  hobo_temp <- x$hobo_temp
  cat_temp <- x$cat_temp
  data <- list("N", "hobo_temp", "cat_temp")
  myinits <- init_values_temp
  parameters <- c("beta1", "beta2", "sigma")
  samples <- combine.mcmc(as.mcmc(jags(data, inits=myinits, parameters,
                                       model.file = temp_jags, 
                                       n.chains=3, n.iter=20000, 
                                       n.burnin=5000, n.thin=10, DIC=F)))
  return(samples)
})

trap4_tempmodel_out <- map_df(output_model_eb4, ~as.data.frame(t(.)))

# beta1 term
beta1_t4 <- trap4_tempmodel_out[grep("beta1", rownames(trap4_tempmodel_out)),]
beta1_t4 <- cbind(full_model_short_eb4$time, beta1_t4, deparse.level = 1)
beta1_t4 <- melt(beta1_t4, id = "full_model_short_eb4$time")

beta1_t4 <- beta1_t4 %>% rename(time = `full_model_short_eb4$time`) %>%
  group_by(time)%>%
  summarize(beta1_t4_mean = mean(value),
            beta1_t4_sd = sd(value))

# beta2 term
beta2_t4 <- trap4_tempmodel_out[grep("beta2", rownames(trap4_tempmodel_out)),]
beta2_t4 <- cbind(full_model_short_eb4$time, beta2_t4, deparse.level = 1)
beta2_t4 <- melt(beta2_t4, id = "full_model_short_eb4$time")

beta2_t4 <- beta2_t4 %>% rename(time = `full_model_short_eb4$time`) %>%
  group_by(time)%>%
  summarize(beta2_t4_mean = mean(value),
            beta2_t4_sd = sd(value))
# sigma term

sigma_t4 <- trap4_tempmodel_out[grep("sigma", rownames(trap4_tempmodel_out)),]
sigma_t4 <- cbind(full_model_short_eb4$time, sigma_t4, deparse.level = 1)
sigma_t4 <- melt(sigma_t4, id = "full_model_short_eb4$time")

sigma_t4 <- sigma_t4 %>% rename(time = `full_model_short_eb4$time`) %>%
  group_by(time)%>%
  summarize(sigma_t4_mean = mean(value),
            sigma_t4_sd = sd(value))

# Combine paramter esitmates
# beta 1
temp_beta1_all <- left_join(beta1_t1, beta1_t2, by = "time") %>%
  left_join(., beta1_t3, by = "time") %>%
  left_join(., beta1_t4, by = "time")

temp_beta1_all_short <- temp_beta1_all %>% filter(time>="2019-07-01")

# beta 2
temp_beta2_all <- left_join(beta2_t1, beta2_t2, by = "time") %>%
  left_join(., beta2_t3, by = "time") %>%
  left_join(., beta2_t4, by = "time")

temp_beta2_all_short <- temp_beta2_all %>% filter(time>="2019-07-01")

# sigma
temp_sigma_all <- left_join(sigma_t1, sigma_t2, by = "time") %>%
  left_join(., sigma_t3, by = "time") %>%
  left_join(., sigma_t4, by = "time")

temp_sigma_all_short <- temp_sigma_all %>% filter(time>="2019-07-01")
#############################################################################################################################


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
  beta3 ~ dnorm(0,1/10000000) # parameter estimate around the hobo temp
  sigma ~ dunif(0, 100) # standard deviation
  tau_obs <- 1 / (sigma * sigma) # sigma^2 doesn't work in JAGS
  tau_add ~ dgamma(0.1,0.1)
}
init_values_ebu <- function(){
  list(beta1 = rnorm(1), beta2 = rnorm(1), beta3 = rnorm(1), sigma = runif(1))
}
output_ebu <- c("beta1", "beta2", "beta3", "sigma")

#Trap #1 ebullition model
full_ebullition_model_short_eb1 <- full_ebullition_model %>% filter(trap_id == "t1eb1") %>% filter(time >= "2019-06-17")
subsetdate_ebu_model_eb1 <- Reduce(rbind, split(full_ebullition_model_short_eb1, seq(as.factor(full_ebullition_model_short_eb1$time))), accumulate = T)
output_ebu_model_eb1 <- lapply(subsetdate_ebu_model_eb1, function(x) {
  N <- nrow(x)
  hobo_temp <- x$hobo_temp
  ebu.latent = x$log_ebu_rate_lag
  ebu.obs = x$log_ebu_rate
  data <- list("N", "hobo_temp", "ebu.latent", "ebu.obs")
  myinits <- init_values_ebu
  parameters <- c("beta1", "beta2", "beta3", "sigma")
  samples <- combine.mcmc(as.mcmc(jags(data, inits=myinits, parameters,
                                       model.file = ebu_jags, 
                                       n.chains=3, n.iter=20000, 
                                       n.burnin=5000, n.thin=10, DIC=F)))
  return(samples)
})

trap1_ebumodel_out <- map_df(output_ebu_model_eb1, ~as.data.frame(t(.)))

# beta1 term
ebu_beta1_t1 <- trap1_ebumodel_out[grep("beta1", rownames(trap1_ebumodel_out)),]
ebu_beta1_t1 <- cbind(full_ebullition_model_short_eb1$time, ebu_beta1_t1, deparse.level = 1)
ebu_beta1_t1 <- melt(ebu_beta1_t1, id = "full_ebullition_model_short_eb1$time")

ebu_beta1_t1 <- ebu_beta1_t1 %>% rename(time = `full_ebullition_model_short_eb1$time`) %>%
  group_by(time)%>%
  summarize(ebu_beta1_t1_mean = mean(value),
            ebu_beta1_t1_sd = sd(value))

# beta2 term
ebu_beta2_t1 <- trap1_ebumodel_out[grep("beta2", rownames(trap1_ebumodel_out)),]
ebu_beta2_t1 <- cbind(full_ebullition_model_short_eb1$time, ebu_beta2_t1, deparse.level = 1)
ebu_beta2_t1 <- melt(ebu_beta2_t1, id = "full_ebullition_model_short_eb1$time")

ebu_beta2_t1 <- ebu_beta2_t1 %>% rename(time = `full_ebullition_model_short_eb1$time`) %>%
  group_by(time)%>%
  summarize(ebu_beta2_t1_mean = mean(value),
            ebu_beta2_t1_sd = sd(value))

# beta3 term
ebu_beta3_t1 <- trap1_ebumodel_out[grep("beta3", rownames(trap1_ebumodel_out)),]
ebu_beta3_t1 <- cbind(full_ebullition_model_short_eb1$time, ebu_beta3_t1, deparse.level = 1)
ebu_beta3_t1 <- melt(ebu_beta3_t1, id = "full_ebullition_model_short_eb1$time")

ebu_beta3_t1 <- ebu_beta3_t1 %>% rename(time = `full_ebullition_model_short_eb1$time`) %>%
  group_by(time)%>%
  summarize(ebu_beta3_t1_mean = mean(value),
            ebu_beta3_t1_sd = sd(value))

# sigma term
ebu_sigma_t1 <- trap1_ebumodel_out[grep("sigma", rownames(trap1_ebumodel_out)),]
ebu_sigma_t1 <- cbind(full_ebullition_model_short_eb1$time, ebu_sigma_t1, deparse.level = 1)
ebu_sigma_t1 <- melt(ebu_sigma_t1, id = "full_ebullition_model_short_eb1$time")

ebu_sigma_t1 <- ebu_sigma_t1 %>% rename(time = `full_ebullition_model_short_eb1$time`) %>%
  group_by(time)%>%
  summarize(ebu_sigma_t1_mean = mean(value),
            ebu_sigma_t1_sd = sd(value))



#Trap #2 ebullition model
full_ebullition_model_short_eb2 <- full_ebullition_model %>% filter(trap_id == "t1eb2") %>% filter(time >= "2019-06-10")
subsetdate_ebu_model_eb2 <- Reduce(rbind, split(full_ebullition_model_short_eb2, seq(as.factor(full_ebullition_model_short_eb2$time))), accumulate = T)
output_ebu_model_eb2 <- lapply(subsetdate_ebu_model_eb2, function(x) {
  N <- nrow(x)
  hobo_temp <- x$hobo_temp
  ebu.latent = x$log_ebu_rate_lag
  ebu.obs = x$log_ebu_rate
  data <- list("N", "hobo_temp", "ebu.latent", "ebu.obs")
  myinits <- init_values_ebu
  parameters <- c("beta1", "beta2", "beta3", "sigma")
  samples <- combine.mcmc(as.mcmc(jags(data, inits=myinits, parameters,
                                       model.file = ebu_jags, 
                                       n.chains=3, n.iter=20000, 
                                       n.burnin=5000, n.thin=10, DIC=F)))
  return(samples)
})

trap2_ebumodel_out <- map_df(output_ebu_model_eb2, ~as.data.frame(t(.)))

# beta1 term
ebu_beta1_t2 <- trap2_ebumodel_out[grep("beta1", rownames(trap2_ebumodel_out)),]
ebu_beta1_t2 <- cbind(full_ebullition_model_short_eb2$time, ebu_beta1_t2, deparse.level = 1)
ebu_beta1_t2 <- melt(ebu_beta1_t2, id = "full_ebullition_model_short_eb2$time")

ebu_beta1_t2 <- ebu_beta1_t2 %>% rename(time = `full_ebullition_model_short_eb2$time`) %>%
  group_by(time)%>%
  summarize(ebu_beta1_t2_mean = mean(value),
            ebu_beta1_t2_sd = sd(value))

# beta2 term
ebu_beta2_t2 <- trap2_ebumodel_out[grep("beta2", rownames(trap2_ebumodel_out)),]
ebu_beta2_t2 <- cbind(full_ebullition_model_short_eb2$time, ebu_beta2_t2, deparse.level = 1)
ebu_beta2_t2 <- melt(ebu_beta2_t2, id = "full_ebullition_model_short_eb2$time")

ebu_beta2_t2 <- ebu_beta2_t2 %>% rename(time = `full_ebullition_model_short_eb2$time`) %>%
  group_by(time)%>%
  summarize(ebu_beta2_t2_mean = mean(value),
            ebu_beta2_t2_sd = sd(value))

# beta3 term
ebu_beta3_t2 <- trap2_ebumodel_out[grep("beta3", rownames(trap2_ebumodel_out)),]
ebu_beta3_t2 <- cbind(full_ebullition_model_short_eb2$time, ebu_beta3_t2, deparse.level = 1)
ebu_beta3_t2 <- melt(ebu_beta3_t2, id = "full_ebullition_model_short_eb2$time")

ebu_beta3_t2 <- ebu_beta3_t2 %>% rename(time = `full_ebullition_model_short_eb2$time`) %>%
  group_by(time)%>%
  summarize(ebu_beta3_t2_mean = mean(value),
            ebu_beta3_t2_sd = sd(value))

# sigma term
ebu_sigma_t2 <- trap2_ebumodel_out[grep("sigma", rownames(trap2_ebumodel_out)),]
ebu_sigma_t2 <- cbind(full_ebullition_model_short_eb2$time, ebu_sigma_t2, deparse.level = 1)
ebu_sigma_t2 <- melt(ebu_sigma_t2, id = "full_ebullition_model_short_eb2$time")

ebu_sigma_t2 <- ebu_sigma_t2 %>% rename(time = `full_ebullition_model_short_eb2$time`) %>%
  group_by(time)%>%
  summarize(ebu_sigma_t2_mean = mean(value),
            ebu_sigma_t2_sd = sd(value))

#Trap #3 ebullition model
full_ebullition_model_short_eb3 <- full_ebullition_model %>% filter(trap_id == "t1eb3") %>% filter(time >= "2019-07-01")
subsetdate_ebu_model_eb3 <- Reduce(rbind, split(full_ebullition_model_short_eb3, seq(as.factor(full_ebullition_model_short_eb3$time))), accumulate = T)
output_ebu_model_eb3 <- lapply(subsetdate_ebu_model_eb3, function(x) {
  N <- nrow(x)
  hobo_temp <- x$hobo_temp
  ebu.latent = x$log_ebu_rate_lag
  ebu.obs = x$log_ebu_rate
  data <- list("N", "hobo_temp", "ebu.latent", "ebu.obs")
  myinits <- init_values_ebu
  parameters <- c("beta1", "beta2", "beta3", "sigma")
  samples <- combine.mcmc(as.mcmc(jags(data, inits=myinits, parameters,
                                       model.file = ebu_jags, 
                                       n.chains=3, n.iter=20000, 
                                       n.burnin=5000, n.thin=10, DIC=F)))
  return(samples)
})

trap3_ebumodel_out <- map_df(output_ebu_model_eb3, ~as.data.frame(t(.)))

# beta1 term
ebu_beta1_t3 <- trap3_ebumodel_out[grep("beta1", rownames(trap3_ebumodel_out)),]
ebu_beta1_t3 <- cbind(full_ebullition_model_short_eb3$time, ebu_beta1_t3, deparse.level = 1)
ebu_beta1_t3 <- melt(ebu_beta1_t3, id = "full_ebullition_model_short_eb3$time")

ebu_beta1_t3 <- ebu_beta1_t3 %>% rename(time = `full_ebullition_model_short_eb3$time`) %>%
  group_by(time)%>%
  summarize(ebu_beta1_t3_mean = mean(value),
            ebu_beta1_t3_sd = sd(value))

# beta2 term
ebu_beta2_t3 <- trap3_ebumodel_out[grep("beta2", rownames(trap3_ebumodel_out)),]
ebu_beta2_t3 <- cbind(full_ebullition_model_short_eb3$time, ebu_beta2_t3, deparse.level = 1)
ebu_beta2_t3 <- melt(ebu_beta2_t3, id = "full_ebullition_model_short_eb3$time")

ebu_beta2_t3 <- ebu_beta2_t3 %>% rename(time = `full_ebullition_model_short_eb3$time`) %>%
  group_by(time)%>%
  summarize(ebu_beta2_t3_mean = mean(value),
            ebu_beta2_t3_sd = sd(value))

# beta3 term
ebu_beta3_t3 <- trap3_ebumodel_out[grep("beta3", rownames(trap3_ebumodel_out)),]
ebu_beta3_t3 <- cbind(full_ebullition_model_short_eb3$time, ebu_beta3_t3, deparse.level = 1)
ebu_beta3_t3 <- melt(ebu_beta3_t3, id = "full_ebullition_model_short_eb3$time")

ebu_beta3_t3 <- ebu_beta3_t3 %>% rename(time = `full_ebullition_model_short_eb3$time`) %>%
  group_by(time)%>%
  summarize(ebu_beta3_t3_mean = mean(value),
            ebu_beta3_t3_sd = sd(value))

# sigma term
ebu_sigma_t3 <- trap3_ebumodel_out[grep("sigma", rownames(trap3_ebumodel_out)),]
ebu_sigma_t3 <- cbind(full_ebullition_model_short_eb3$time, ebu_sigma_t3, deparse.level = 1)
ebu_sigma_t3 <- melt(ebu_sigma_t3, id = "full_ebullition_model_short_eb3$time")

ebu_sigma_t3 <- ebu_sigma_t3 %>% rename(time = `full_ebullition_model_short_eb3$time`) %>%
  group_by(time)%>%
  summarize(ebu_sigma_t3_mean = mean(value),
            ebu_sigma_t3_sd = sd(value))

#Trap #4 ebullition model
full_ebullition_model_short_eb4 <- full_ebullition_model %>% filter(trap_id == "t1eb4") %>% filter(time >= "2019-06-10")
subsetdate_ebu_model_eb4 <- Reduce(rbind, split(full_ebullition_model_short_eb4, seq(as.factor(full_ebullition_model_short_eb4$time))), accumulate = T)
output_ebu_model_eb4 <- lapply(subsetdate_ebu_model_eb4, function(x) {
  N <- nrow(x)
  hobo_temp <- x$hobo_temp
  ebu.latent = x$log_ebu_rate_lag
  ebu.obs = x$log_ebu_rate
  data <- list("N", "hobo_temp", "ebu.latent", "ebu.obs")
  myinits <- init_values_ebu
  parameters <- c("beta1", "beta2", "beta3", "sigma")
  samples <- combine.mcmc(as.mcmc(jags(data, inits=myinits, parameters,
                                       model.file = ebu_jags, 
                                       n.chains=3, n.iter=20000, 
                                       n.burnin=5000, n.thin=10, DIC=F)))
  return(samples)
})

trap4_ebumodel_out <- map_df(output_ebu_model_eb4, ~as.data.frame(t(.)))

# beta1 term
ebu_beta1_t4 <- trap4_ebumodel_out[grep("beta1", rownames(trap4_ebumodel_out)),]
ebu_beta1_t4 <- cbind(full_ebullition_model_short_eb4$time, ebu_beta1_t4, deparse.level = 1)
ebu_beta1_t4 <- melt(ebu_beta1_t4, id = "full_ebullition_model_short_eb4$time")

ebu_beta1_t4 <- ebu_beta1_t4 %>% rename(time = `full_ebullition_model_short_eb4$time`) %>%
  group_by(time)%>%
  summarize(ebu_beta1_t4_mean = mean(value),
            ebu_beta1_t4_sd = sd(value))

# beta2 term
ebu_beta2_t4 <- trap4_ebumodel_out[grep("beta2", rownames(trap4_ebumodel_out)),]
ebu_beta2_t4 <- cbind(full_ebullition_model_short_eb4$time, ebu_beta2_t4, deparse.level = 1)
ebu_beta2_t4 <- melt(ebu_beta2_t4, id = "full_ebullition_model_short_eb4$time")

ebu_beta2_t4 <- ebu_beta2_t4 %>% rename(time = `full_ebullition_model_short_eb4$time`) %>%
  group_by(time)%>%
  summarize(ebu_beta2_t4_mean = mean(value),
            ebu_beta2_t4_sd = sd(value))

# beta3 term
ebu_beta3_t4 <- trap4_ebumodel_out[grep("beta3", rownames(trap4_ebumodel_out)),]
ebu_beta3_t4 <- cbind(full_ebullition_model_short_eb4$time, ebu_beta3_t4, deparse.level = 1)
ebu_beta3_t4 <- melt(ebu_beta3_t4, id = "full_ebullition_model_short_eb4$time")

ebu_beta3_t4 <- ebu_beta3_t4 %>% rename(time = `full_ebullition_model_short_eb4$time`) %>%
  group_by(time)%>%
  summarize(ebu_beta3_t4_mean = mean(value),
            ebu_beta3_t4_sd = sd(value))

# sigma term
ebu_sigma_t4 <- trap4_ebumodel_out[grep("sigma", rownames(trap4_ebumodel_out)),]
ebu_sigma_t4 <- cbind(full_ebullition_model_short_eb4$time, ebu_sigma_t4, deparse.level = 1)
ebu_sigma_t4 <- melt(ebu_sigma_t4, id = "full_ebullition_model_short_eb4$time")

ebu_sigma_t4 <- ebu_sigma_t4 %>% rename(time = `full_ebullition_model_short_eb4$time`) %>%
  group_by(time)%>%
  summarize(ebu_sigma_t4_mean = mean(value),
            ebu_sigma_t4_sd = sd(value))


# Combine paramter esitmates
# beta 1
ebu_beta1_all <- left_join(ebu_beta1_t1, ebu_beta1_t2, by = "time") %>%
  left_join(., ebu_beta1_t3, by = "time") %>%
  left_join(., ebu_beta1_t4, by = "time")

ebu_beta1_all_short <- ebu_beta1_all %>% filter(time>="2019-07-01")

# beta 2
ebu_beta2_all <- left_join(ebu_beta2_t1, ebu_beta2_t2, by = "time") %>%
  left_join(., ebu_beta2_t3, by = "time") %>%
  left_join(., ebu_beta2_t4, by = "time")

ebu_beta2_all_short <- ebu_beta2_all %>% filter(time>="2019-07-01")

# beta 3
ebu_beta3_all <- left_join(ebu_beta3_t1, ebu_beta3_t2, by = "time") %>%
  left_join(., ebu_beta3_t3, by = "time") %>%
  left_join(., ebu_beta3_t4, by = "time")

ebu_beta3_all_short <- ebu_beta3_all %>% filter(time>="2019-07-01")

# sigma
ebu_sigma_all <- left_join(ebu_sigma_t1, ebu_sigma_t2, by = "time") %>%
  left_join(., ebu_sigma_t3, by = "time") %>%
  left_join(., ebu_sigma_t4, by = "time")

ebu_sigma_all_short <- ebu_sigma_all %>% filter(time>="2019-07-01")
