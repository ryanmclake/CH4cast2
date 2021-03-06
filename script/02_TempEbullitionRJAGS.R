if (!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(tidyverse,rjags,runjags,MCMCvis,lubridate,tidybayes,R2jags, ggforce, reshape2)

#All trap averaged ebullition model
full_ebullition_model_short_all <- full_ebullition_model %>% group_by(time) %>% summarize_all(funs(mean), na.rm = T)
full_ebullition_model_short_all$log_ebu_rate[sapply(full_ebullition_model_short_all$log_ebu_rate, is.nan)] <- NA
full_ebullition_model_short_all$log_ebu_rate_sd[sapply(full_ebullition_model_short_all$log_ebu_rate_sd, is.nan)] <- NA


# Develop the Temperature JAGS model
# This really takes a while!
#############################################################################################################################
temp_jags <- function(){
  for (i in 1:N){
    air_temp[i] ~ dnorm(mu[i], tau) # tau is precision (1 / variance)
    mu[i] <- beta1 + beta2 * air_temp[i]
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


# Run the temperature scaling model as a 
#Trap #1 temperature model for 2017 and 2018
full_model_short_eb1_17_18 <- full_ebullition_model %>% filter(trap_id == "T1e1") %>% filter(time <= "2018-12-31")

N <- nrow(full_model_short_eb1_17_18)
hobo_temp <- full_model_short_eb1_17_18$hobo_temp
air_temp <- full_model_short_eb1_17_18$air_temp
data <- list("N", "hobo_temp", "air_temp")
myinits <- init_values_temp
parameters <- c("beta1", "beta2", "sigma")
samples <- as.mcmc(jags(data, inits=myinits, parameters,
                                     model.file = temp_jags, 
                                     n.chains=3, n.iter=20000, 
                                     n.burnin=5000, n.thin=10, DIC=F))
gelman.plot(samples)
gelman.diag(samples)
plot(samples)


#Trap #2 temperature model for 2017 and 2018
full_model_short_eb2_17_18 <- full_ebullition_model %>% filter(trap_id == "T1e2") %>% filter(time <= "2018-12-31")

N <- nrow(full_model_short_eb2_17_18)
hobo_temp <- full_model_short_eb2_17_18$hobo_temp
air_temp <- full_model_short_eb2_17_18$air_temp
data <- list("N", "hobo_temp", "air_temp")
myinits <- init_values_temp
parameters <- c("beta1", "beta2", "sigma")
samples <- as.mcmc(jags(data, inits=myinits, parameters,
                        model.file = temp_jags, 
                        n.chains=3, n.iter=20000, 
                        n.burnin=5000, n.thin=10, DIC=F))
gelman.plot(samples)
gelman.diag(samples)
plot(samples)


#Trap #3 temperature model for 2017 and 2018
full_model_short_eb3_17_18 <- full_ebullition_model %>% filter(trap_id == "T1e3") %>% filter(time <= "2018-12-31")

N <- nrow(full_model_short_eb3_17_18)
hobo_temp <- full_model_short_eb3_17_18$hobo_temp
air_temp <- full_model_short_eb3_17_18$air_temp
data <- list("N", "hobo_temp", "air_temp")
myinits <- init_values_temp
parameters <- c("beta1", "beta2", "sigma")
samples <- as.mcmc(jags(data, inits=myinits, parameters,
                        model.file = temp_jags, 
                        n.chains=3, n.iter=20000, 
                        n.burnin=5000, n.thin=10, DIC=F))
gelman.plot(samples)
gelman.diag(samples)
plot(samples)


#Trap #4 temperature model for 2017 and 2018
full_model_short_eb4_17_18 <- full_ebullition_model %>% filter(trap_id == "T1e4") %>% filter(time <= "2018-12-31")

N <- nrow(full_model_short_eb4_17_18)
hobo_temp <- full_model_short_eb4_17_18$hobo_temp
air_temp <- full_model_short_eb4_17_18$air_temp
data <- list("N", "hobo_temp", "air_temp")
myinits <- init_values_temp
parameters <- c("beta1", "beta2", "sigma")
samples <- as.mcmc(jags(data, inits=myinits, parameters,
                        model.file = temp_jags, 
                        n.chains=3, n.iter=20000, 
                        n.burnin=5000, n.thin=10, DIC=F))
gelman.plot(samples)
gelman.diag(samples)
plot(samples)


### Run through the temperature model training daily for '17, '18, and '19. 
full_temp_model_eb1<- full_ebullition_model %>% filter(trap_id == "T1e1")
subsetdate_eb1 <- Reduce(rbind, split(full_temp_model_eb1, seq(as.factor(full_temp_model_eb1$time))), accumulate = T)
output_model_eb1 <- lapply(subsetdate_eb1, function(x) {
  N <- nrow(x)
  hobo_temp <- x$hobo_temp
  air_temp <- x$air_temp
  data <- list("N", "hobo_temp", "air_temp")
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
beta1_t1 <- cbind(full_temp_model_eb1$time, beta1_t1, deparse.level = 1)
beta1_t1 <- melt(beta1_t1, id = "full_temp_model_eb1$time")

beta1_t1 <- beta1_t1 %>% rename(time = `full_temp_model_eb1$time`) %>%
  group_by(time)%>%
  summarize(beta1_t1_mean = mean(value),
            beta1_t1_sd = sd(value))

# beta2 term
beta2_t1 <- trap1_tempmodel_out[grep("beta2", rownames(trap1_tempmodel_out)),]
beta2_t1 <- cbind(full_temp_model_eb1$time, beta2_t1, deparse.level = 1)
beta2_t1 <- melt(beta2_t1, id = "full_temp_model_eb1$time")

beta2_t1 <- beta2_t1 %>% rename(time = `full_temp_model_eb1$time`) %>%
  group_by(time)%>%
  summarize(beta2_t1_mean = mean(value),
            beta2_t1_sd = sd(value))
# sigma term

sigma_t1 <- trap1_tempmodel_out[grep("sigma", rownames(trap1_tempmodel_out)),]
sigma_t1 <- cbind(full_temp_model_eb1$time, sigma_t1, deparse.level = 1)
sigma_t1 <- melt(sigma_t1, id = "full_temp_model_eb1$time")

sigma_t1 <- sigma_t1 %>% rename(time = `full_temp_model_eb1$time`) %>%
  group_by(time)%>%
  summarize(sigma_t1_mean = mean(value),
            sigma_t1_sd = sd(value))




#Trap #2 temperature model
full_model_short_eb2 <- full_ebullition_model %>% filter(trap_id == "T1e2")
subsetdate_eb2 <- Reduce(rbind, split(full_model_short_eb2, seq(as.factor(full_model_short_eb2$time))), accumulate = T)
output_model_eb2 <- lapply(subsetdate_eb2, function(x) {
  N <- nrow(x)
  hobo_temp <- x$hobo_temp
  air_temp <- x$air_temp
  data <- list("N", "hobo_temp", "air_temp")
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
full_model_short_eb3 <- full_ebullition_model %>% filter(trap_id == "T1e3")
subsetdate_eb3 <- Reduce(rbind, split(full_model_short_eb3, seq(as.factor(full_model_short_eb3$time))), accumulate = T)
output_model_eb3 <- lapply(subsetdate_eb3, function(x) {
  N <- nrow(x)
  hobo_temp <- x$hobo_temp
  air_temp <- x$air_temp
  data <- list("N", "hobo_temp", "air_temp")
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
full_model_short_eb4 <- full_ebullition_model %>% filter(trap_id == "T1e4")
subsetdate_eb4 <- Reduce(rbind, split(full_model_short_eb4, seq(as.factor(full_model_short_eb4$time))), accumulate = T)
output_model_eb4 <- lapply(subsetdate_eb4, function(x) {
  N <- nrow(x)
  hobo_temp <- x$hobo_temp
  air_temp <- x$air_temp
  data <- list("N", "hobo_temp", "air_temp")
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

#All Trap temperature model
subsetdate_trapall <- Reduce(rbind, split(full_ebullition_model_short_all, seq(as.factor(full_ebullition_model_short_all$time))), accumulate = T)
output_model_trapall <- lapply(subsetdate_trapall, function(x) {
  N <- nrow(x)
  hobo_temp <- x$hobo_temp
  air_temp <- x$air_temp
  data <- list("N", "hobo_temp", "air_temp")
  myinits <- init_values_temp
  parameters <- c("beta1", "beta2", "sigma")
  samples <- combine.mcmc(as.mcmc(jags(data, inits=myinits, parameters,
                                       model.file = temp_jags, 
                                       n.chains=3, n.iter=20000, 
                                       n.burnin=5000, n.thin=10, DIC=F)))
  return(samples)
})

all_tempmodel_out <- map_df(output_model_trapall, ~as.data.frame(t(.)))

# beta1 term
beta1_alltrap <- all_tempmodel_out[grep("beta1", rownames(all_tempmodel_out)),]
beta1_alltrap <- cbind(full_ebullition_model_short_all$time, beta1_alltrap, deparse.level = 1)
beta1_alltrap <- melt(beta1_alltrap, id = "full_ebullition_model_short_all$time")

beta1_alltrap <- beta1_alltrap %>% rename(time = `full_ebullition_model_short_all$time`) %>%
  group_by(time)%>%
  summarize(beta1_alltrap_mean = mean(value),
            beta1_alltrap_sd = sd(value))

# beta2 term
beta2_alltrap <- all_tempmodel_out[grep("beta2", rownames(all_tempmodel_out)),]
beta2_alltrap <- cbind(full_ebullition_model_short_all$time, beta2_alltrap, deparse.level = 1)
beta2_alltrap <- melt(beta2_alltrap, id = "full_ebullition_model_short_all$time")

beta2_alltrap <- beta2_alltrap %>% rename(time = `full_ebullition_model_short_all$time`) %>%
  group_by(time)%>%
  summarize(beta2_alltrap_mean = mean(value),
            beta2_alltrap_sd = sd(value))
# sigma term

sigma_alltrap <- all_tempmodel_out[grep("sigma", rownames(all_tempmodel_out)),]
sigma_alltrap <- cbind(full_ebullition_model_short_all$time, sigma_alltrap, deparse.level = 1)
sigma_alltrap <- melt(sigma_alltrap, id = "full_ebullition_model_short_all$time")

sigma_alltrap <- sigma_alltrap %>% rename(time = `full_ebullition_model_short_all$time`) %>%
  group_by(time)%>%
  summarize(sigma_alltrap_mean = mean(value),
            sigma_alltrap_sd = sd(value))


# Combine paramter esitmates
# beta 1
temp_beta1_all <- left_join(beta1_t1, beta1_t2, by = "time") %>%
  left_join(., beta1_t3, by = "time") %>%
  left_join(., beta1_t4, by = "time") %>%
  left_join(., beta1_alltrap, by = "time")

temp_beta1_all_short <- temp_beta1_all %>% filter(time>="2019-07-01")

# beta 2
temp_beta2_all <- left_join(beta2_t1, beta2_t2, by = "time") %>%
  left_join(., beta2_t3, by = "time") %>%
  left_join(., beta2_t4, by = "time")%>%
  left_join(., beta2_alltrap, by = "time")

temp_beta2_all_short <- temp_beta2_all %>% filter(time>="2019-07-01")

# sigma
temp_sigma_all <- left_join(sigma_t1, sigma_t2, by = "time") %>%
  left_join(., sigma_t3, by = "time") %>%
  left_join(., sigma_t4, by = "time")%>%
  left_join(., sigma_alltrap, by = "time")

temp_sigma_all_short <- temp_sigma_all %>% filter(time>="2019-07-01")
#############################################################################################################################

# Develop the Ebullition Jags Model and extract parameters for each trap
# This also really takes a while - if you dont want to run it you can just review. 
#############################################################################################################################
ebu_jags <- function(){
  for (i in 2:N){
    ebu.obs[i] ~ dnorm(ebu.latent[i], tau_obs)
    ebu.latent[i] ~ dnorm(ebu.hat[i], tau_add)
    ebu.hat[i] <- beta1 + beta2*ebu.latent[i-1] + beta3*hobo_temp[i]
  }
  
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
output_ebu <- c("beta1", "beta2", "beta3", "sigma", "tau_obs", "ebu.latent")



#Trap #1 ebullition model
full_ebullition_model_short_eb1 <- full_ebullition_model %>% filter(trap_id == "t1eb1") %>% filter(time >= "2019-06-17")
subsetdate_ebu_model_eb1 <- Reduce(rbind, split(full_ebullition_model_short_eb1, seq(as.factor(full_ebullition_model_short_eb1$time))), accumulate = T)
output_ebu_model_eb1 <- lapply(subsetdate_ebu_model_eb1, function(x) {
  N <- nrow(x)
  hobo_temp <- x$hobo_temp
  ebu.latent = x$log_ebu_rate
  ebu.obs = x$log_ebu_rate
  data <- list("N", "hobo_temp", "ebu.latent", "ebu.obs")
  myinits <- init_values_ebu
  parameters <- output_ebu
  samples <- combine.mcmc(as.mcmc(jags(data, inits=myinits, parameters,
                                       model.file = ebu_jags, 
                                       n.chains=3, n.iter=50000, 
                                       n.burnin=10000, n.thin=10, DIC=F)))
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

#latent ebullition term
ebu_latent_t1 <- trap1_ebumodel_out[grep("ebu.latent", rownames(trap1_ebumodel_out)),]
ebu_latent_t1 <- ebu_latent_t1[10297:10440,]
rownames(ebu_latent_t1) <- sub("ebu.latent", "", rownames(ebu_latent_t1))
rownames(ebu_latent_t1) <- gsub("\\[|\\]", "", rownames(ebu_latent_t1))
rownames(ebu_latent_t1) <- gsub("[[:punct:]]+", '', rownames(ebu_latent_t1))

ebu_latent_t1_ordered <- ebu_latent_t1[ order(as.numeric(row.names(ebu_latent_t1))), ]
ebu_latent_t1_ordered <- ebu_latent_t1_ordered[c(2:144,1),]


ebu_latent_t1 <- cbind(full_ebullition_model_short_eb1$time, ebu_latent_t1_ordered, deparse.level = 1)
ebu_latent_t1 <- melt(ebu_latent_t1, id = "full_ebullition_model_short_eb1$time")

ebu_latent_t1 <- ebu_latent_t1 %>% rename(time = `full_ebullition_model_short_eb1$time`) %>%
  group_by(time)%>%
  summarize(ebu_latent_t1_mean = mean(value),
            ebu_latent_t1_sd = sd(value))
ebu_latent_t1$time <- as.Date(ebu_latent_t1$time) - 7


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
  parameters <- output_ebu
  samples <- combine.mcmc(as.mcmc(jags(data, inits=myinits, parameters,
                                       model.file = ebu_jags, 
                                       n.chains=3, n.iter=50000, 
                                       n.burnin=10000, n.thin=10, DIC=F)))
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

#latent ebullition term
ebu_latent_t2 <- trap2_ebumodel_out[grep("ebu.latent", rownames(trap2_ebumodel_out)),]
ebu_latent_t2 <- ebu_latent_t2[11326:11476,]
rownames(ebu_latent_t2) <- sub("ebu.latent", "", rownames(ebu_latent_t2))
rownames(ebu_latent_t2) <- gsub("\\[|\\]", "", rownames(ebu_latent_t2))
rownames(ebu_latent_t2) <- gsub("[[:punct:]]+", '', rownames(ebu_latent_t2))

ebu_latent_t2_ordered <- ebu_latent_t2[ order(as.numeric(row.names(ebu_latent_t2))), ]
ebu_latent_t2_ordered <- ebu_latent_t2_ordered[c(2:151,1),]


ebu_latent_t2 <- cbind(full_ebullition_model_short_eb2$time, ebu_latent_t2_ordered, deparse.level = 1)
ebu_latent_t2 <- melt(ebu_latent_t2, id = "full_ebullition_model_short_eb2$time")

ebu_latent_t2 <- ebu_latent_t2 %>% rename(time = `full_ebullition_model_short_eb2$time`) %>%
  group_by(time)%>%
  summarize(ebu_latent_t2_mean = mean(value),
            ebu_latent_t2_sd = sd(value))
ebu_latent_t2$time <- as.Date(ebu_latent_t2$time) - 7



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
  parameters <- output_ebu
  samples <- combine.mcmc(as.mcmc(jags(data, inits=myinits, parameters,
                                       model.file = ebu_jags, 
                                       n.chains=3, n.iter=50000, 
                                       n.burnin=10000, n.thin=10, DIC=F)))
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

#latent ebullition term
ebu_latent_t3 <- trap3_ebumodel_out[grep("ebu.latent", rownames(trap3_ebumodel_out)),]
ebu_latent_t3 <- ebu_latent_t3[8386:8515,]
rownames(ebu_latent_t3) <- sub("ebu.latent", "", rownames(ebu_latent_t3))
rownames(ebu_latent_t3) <- gsub("\\[|\\]", "", rownames(ebu_latent_t3))
rownames(ebu_latent_t3) <- gsub("[[:punct:]]+", '', rownames(ebu_latent_t3))

ebu_latent_t3_ordered <- ebu_latent_t3[ order(as.numeric(row.names(ebu_latent_t3))), ]
ebu_latent_t3_ordered <- ebu_latent_t3_ordered[c(2:130,1),]


ebu_latent_t3 <- cbind(full_ebullition_model_short_eb3$time, ebu_latent_t3_ordered, deparse.level = 1)
ebu_latent_t3 <- melt(ebu_latent_t3, id = "full_ebullition_model_short_eb3$time")

ebu_latent_t3 <- ebu_latent_t3 %>% rename(time = `full_ebullition_model_short_eb3$time`) %>%
  group_by(time)%>%
  summarize(ebu_latent_t3_mean = mean(value),
            ebu_latent_t3_sd = sd(value))
ebu_latent_t3$time <- as.Date(ebu_latent_t3$time) - 7




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
  parameters <- output_ebu
  samples <- combine.mcmc(as.mcmc(jags(data, inits=myinits, parameters,
                                       model.file = ebu_jags, 
                                       n.chains=3, n.iter=50000, 
                                       n.burnin=10000, n.thin=10, DIC=F)))
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

#latent ebullition term
ebu_latent_t4 <- trap4_ebumodel_out[grep("ebu.latent", rownames(trap4_ebumodel_out)),]
ebu_latent_t4 <- ebu_latent_t4[11326:11476,]
rownames(ebu_latent_t4) <- sub("ebu.latent", "", rownames(ebu_latent_t4))
rownames(ebu_latent_t4) <- gsub("\\[|\\]", "", rownames(ebu_latent_t4))
rownames(ebu_latent_t4) <- gsub("[[:punct:]]+", '', rownames(ebu_latent_t4))

ebu_latent_t4_ordered <- ebu_latent_t4[ order(as.numeric(row.names(ebu_latent_t4))), ]
ebu_latent_t4_ordered <- ebu_latent_t4_ordered[c(2:151,1),]


ebu_latent_t4 <- cbind(full_ebullition_model_short_eb4$time, ebu_latent_t4_ordered, deparse.level = 1)
ebu_latent_t4 <- melt(ebu_latent_t4, id = "full_ebullition_model_short_eb4$time")

ebu_latent_t4 <- ebu_latent_t4 %>% rename(time = `full_ebullition_model_short_eb4$time`) %>%
  group_by(time)%>%
  summarize(ebu_latent_t4_mean = mean(value),
            ebu_latent_t4_sd = sd(value))
ebu_latent_t4$time <- as.Date(ebu_latent_t4$time) - 7




subsetdate_ebu_model_all <- Reduce(rbind, split(full_ebullition_model_short_all, seq(as.factor(full_ebullition_model_short_all$time))), accumulate = T)
output_ebu_model_all <- lapply(subsetdate_ebu_model_all, function(x) {
  N <- nrow(x)
  hobo_temp <- x$hobo_temp
  ebu.latent = x$log_ebu_rate_lag
  ebu.obs = x$log_ebu_rate
  data <- list("N", "hobo_temp", "ebu.latent", "ebu.obs")
  myinits <- init_values_ebu
  parameters <- output_ebu
  samples <- combine.mcmc(as.mcmc(jags(data, inits=myinits, parameters,
                                       model.file = ebu_jags, 
                                       n.chains=3, n.iter=50000, 
                                       n.burnin=10000, n.thin=10, DIC=F)))
  return(samples)
})

gc()
all_ebumodel_out <- map_df(output_ebu_model_all, ~as.data.frame(t(.)))

# beta1 term
ebu_beta1_alltrap <- all_ebumodel_out[grep("beta1", rownames(all_ebumodel_out)),]
ebu_beta1_alltrap <- cbind(full_ebullition_model_short_all$time, ebu_beta1_alltrap, deparse.level = 1)
ebu_beta1_alltrap <- melt(ebu_beta1_alltrap, id = "full_ebullition_model_short_all$time")

ebu_beta1_alltrap <- ebu_beta1_alltrap %>% rename(time = `full_ebullition_model_short_all$time`) %>%
  group_by(time)%>%
  summarize(ebu_beta1_alltrap_mean = mean(value),
            ebu_beta1_alltrap_sd = sd(value))

# beta2 term
ebu_beta2_alltrap <- all_ebumodel_out[grep("beta2", rownames(all_ebumodel_out)),]
ebu_beta2_alltrap <- cbind(full_ebullition_model_short_all$time, ebu_beta2_alltrap, deparse.level = 1)
ebu_beta2_alltrap <- melt(ebu_beta2_alltrap, id = "full_ebullition_model_short_all$time")

ebu_beta2_alltrap <- ebu_beta2_alltrap %>% rename(time = `full_ebullition_model_short_all$time`) %>%
  group_by(time)%>%
  summarize(ebu_beta2_alltrap_mean = mean(value),
            ebu_beta2_alltrap_sd = sd(value))

# beta3 term
ebu_beta3_alltrap <- all_ebumodel_out[grep("beta3", rownames(all_ebumodel_out)),]
ebu_beta3_alltrap <- cbind(full_ebullition_model_short_all$time, ebu_beta3_alltrap, deparse.level = 1)
ebu_beta3_alltrap <- melt(ebu_beta3_alltrap, id = "full_ebullition_model_short_all$time")

ebu_beta3_alltrap <- ebu_beta3_alltrap %>% rename(time = `full_ebullition_model_short_all$time`) %>%
  group_by(time)%>%
  summarize(ebu_beta3_alltrap_mean = mean(value),
            ebu_beta3_alltrap_sd = sd(value))

# sigma term
ebu_sigma_alltrap <- all_ebumodel_out[grep("sigma", rownames(all_ebumodel_out)),]
ebu_sigma_alltrap <- cbind(full_ebullition_model_short_all$time, ebu_sigma_alltrap, deparse.level = 1)
ebu_sigma_alltrap <- melt(ebu_sigma_alltrap, id = "full_ebullition_model_short_all$time")

ebu_sigma_alltrap <- ebu_sigma_alltrap %>% rename(time = `full_ebullition_model_short_all$time`) %>%
  group_by(time)%>%
  summarize(ebu_sigma_alltrap_mean = mean(value),
            ebu_sigma_alltrap_sd = sd(value))


#latent ebullition term
ebu_latent_alltrap <- all_ebumodel_out[grep("ebu.latent", rownames(all_ebumodel_out)),]
ebu_latent_alltrap <- ebu_latent_alltrap[11326:11476,]
rownames(ebu_latent_alltrap) <- sub("ebu.latent", "", rownames(ebu_latent_alltrap))
rownames(ebu_latent_alltrap) <- gsub("\\[|\\]", "", rownames(ebu_latent_alltrap))
rownames(ebu_latent_alltrap) <- gsub("[[:punct:]]+", '', rownames(ebu_latent_alltrap))

ebu_latent_alltrap_ordered <- ebu_latent_alltrap[ order(as.numeric(row.names(ebu_latent_alltrap))), ]
ebu_latent_alltrap_ordered <- ebu_latent_alltrap_ordered[c(2:151,1),]


ebu_latent_alltrap <- cbind(full_ebullition_model_short_all$time, ebu_latent_alltrap_ordered, deparse.level = 1)
ebu_latent_alltrap <- melt(ebu_latent_alltrap, id = "full_ebullition_model_short_all$time")

ebu_latent_alltrap <- ebu_latent_alltrap %>% rename(time = `full_ebullition_model_short_all$time`) %>%
  group_by(time)%>%
  summarize(ebu_latent_alltrap_mean = mean(value),
            ebu_latent_alltrap_sd = sd(value))
ebu_latent_alltrap$time <- as.Date(ebu_latent_alltrap$time) - 7





# Combine paramter esitmates
# beta 1
ebu_beta1_all <- left_join(ebu_beta1_t1, ebu_beta1_t2, by = "time") %>%
  left_join(., ebu_beta1_t3, by = "time") %>%
  left_join(., ebu_beta1_t4, by = "time") %>%
  left_join(., ebu_beta1_alltrap, by = "time")

ebu_beta1_all_short <- ebu_beta1_all %>% filter(time>="2019-07-01")

# beta 2
ebu_beta2_all <- left_join(ebu_beta2_t1, ebu_beta2_t2, by = "time") %>%
  left_join(., ebu_beta2_t3, by = "time") %>%
  left_join(., ebu_beta2_t4, by = "time") %>%
  left_join(., ebu_beta2_alltrap, by = "time")

ebu_beta2_all_short <- ebu_beta2_all %>% filter(time>="2019-07-01")

# beta 3
ebu_beta3_all <- left_join(ebu_beta3_t1, ebu_beta3_t2, by = "time") %>%
  left_join(., ebu_beta3_t3, by = "time") %>%
  left_join(., ebu_beta3_t4, by = "time") %>%
  left_join(., ebu_beta3_alltrap, by = "time")

ebu_beta3_all_short <- ebu_beta3_all %>% filter(time>="2019-07-01")

# sigma
ebu_sigma_all <- left_join(ebu_sigma_t1, ebu_sigma_t2, by = "time") %>%
  left_join(., ebu_sigma_t3, by = "time") %>%
  left_join(., ebu_sigma_t4, by = "time")%>%
  left_join(., ebu_sigma_alltrap, by = "time")

ebu_sigma_all_short <- ebu_sigma_all %>% filter(time>="2019-07-01")

# latent ebullition flux
ebu_latent_all <- left_join(ebu_latent_t1, ebu_latent_t2, by = "time") %>%
  left_join(., ebu_latent_t3, by = "time") %>%
  left_join(., ebu_latent_t4, by = "time")%>%
  left_join(., ebu_latent_alltrap, by = "time")

ebu_latent_all_short <- ebu_latent_all %>% filter(time>="2019-07-01")
#############################################################################################################################




# Develop the Persistence Null Model and extract parameters for each trap
# Not finished yet!!!!! 
#############################################################################################################################
# 
# null <- function(){
#   for (i in 2:N){
#     ebu.latent[i] ~ dnorm(ebu.hat[i], tau_add)
#     ebu.hat[i] <- dnorm(ebu.latent[i])
#   }
#   
#   # Priors:
#   sigma ~ dunif(0, 100) # standard deviation
#   tau_obs <- 1 / (sigma * sigma) # sigma^2 doesn't work in JAGS
#   tau_add ~ dgamma(0.1,0.1)
# }
# 
# init_values_ebu <- function(){
#   list(sigma = runif(1))
# }
# output_null <- c("sigma", "ebu.latent")
# 
# #Trap #1 ebullition model
# full_ebullition_model_short_eb1 <- full_ebullition_model %>% filter(trap_id == "t1eb1") %>% filter(time >= "2019-06-17")
# subsetdate_null_model_eb1 <- Reduce(rbind, split(full_ebullition_model_short_eb1, seq(as.factor(full_ebullition_model_short_eb1$time))), accumulate = T)
# output_ebu_model_eb1 <- lapply(subsetdate_ebu_model_eb1, function(x) {
#   N <- nrow(x)
#   ebu.latent = x$log_ebu_rate_lag
#   ebu.obs = x$log_ebu_rate
#   data <- list("N", "ebu.latent", "ebu.obs")
#   myinits <- init_values_ebu
#   parameters <- output_ebu
#   samples <- combine.mcmc(as.mcmc(jags(data, inits=myinits, parameters,
#                                        model.file = ebu_jags, 
#                                        n.chains=3, n.iter=50000, 
#                                        n.burnin=10000, n.thin=10, DIC=F)))
#   return(samples)
# })
