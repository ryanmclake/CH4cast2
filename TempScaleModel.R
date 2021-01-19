setwd('C:/Users/Owner/Desktop/CH4cast2/observed/fcre/')

# Load packages
pacman::p_load(tidyverse,rjags,runjags,MCMCvis,lubridate,tidybayes,R2jags,ncdf4,modelr,aws.s3,prov,EML,jsonlite)

set.seed(329)

#'Generate plot to visualized forecast
generate_plots <- TRUE

# define the forecast horizon
f_days = 16

#'Generic temperature scaling modelto convert the forecasted Air temperature to represent the 
#'SWI temperature below the ebullition traps

TempScale = "
model{
  # Priors:
  beta1 ~ dnorm(0, 0.001) # intercept
  beta2 ~ dnorm(0, 0.001) # parameter estimate around the catwalk
  sigma ~ dunif(0, 100) # standard deviation
  tau <- 1 / (sigma * sigma) # sigma^2 doesn't work in JAGS

  # Process Model
  for(t in 2:nobs){
    air_temp[t] ~ dnorm(hobo_temp[t], tau) # tau is precision (1 / variance)
    hobo_temp[t] <- beta1 + beta2 * air_temp[t]
  }
  
    # Data Model
  for(i in 1:nobs){
    air_temp[i] ~ dnorm(x[air_temp_wgaps_index[i]], tau_obs[air_temp_wgaps_index[i]])
    hobo_temp[i] ~ dnorm(x[hobo_temp_wgaps_index[i]], tau_obs[hobo_temp_wgaps_index[i]])
  }
}
"

init_values_temp <- function(){
  list(beta1 = rnorm(1), beta2 = rnorm(1), sigma = runif(1))
}
output_temp <- c("beta1", "beta2", "sigma")

#'## NEE Model

#'Create variable for combined forecasts across sites
forecast_saved_tempscale <- NULL
ebullition_figures <- list()

targets <- full_ebullition_model %>% filter(time <= "2019-05-27")

site_names <- c("T1e1", "T1e2", "T1e3", "T1e4")


#' Loop through individual ebullition traps
#for(s in 1:length(site_names)){
  
  # Select site
  site_data_var <- targets %>%
    filter(trap_id == site_names[s])
  
  # Select site
  site_data_var <- targets %>%
    filter(trap_id == "T1e1")
  
  # Find the last day in the observed data and add one day for the start of the 
  # forecast. Then, read in the forecast file from the fcre file
  
  files <- list.files()
  start_forecast <- max(site_data_var$time)
  start <- paste(gsub("-", "", start_forecast))
  start <-paste0(start,"_gep_all_12z")
  
  forecast_file <- grep(start, files, value=TRUE)
  
  forecast_air <- read_csv(forecast_file)%>%
    rename(time = forecast.date)%>%
    mutate(time = as_date(time))%>%
    select(time, ensembles, tmp2m)%>%
    group_by(time) %>%
    summarize(air_temp = mean(tmp2m, na.rm = TRUE),
              air_temp_sd = sd(tmp2m, na.rm = TRUE))%>%
    mutate(air_temp = air_temp-273.15)%>%
    arrange(time)%>%
    mutate(hobo_temp = NA)%>%
    mutate(hobo_temp_sd = NA)%>%
    mutate(log_ebu_rate = NA)%>%
    mutate(log_ebu_rate_sd = NA)
  
  # This is key here - I added the forecast horizon on the end of the data for the forecast period
  # Join the full time with the site_data_var so there aren't gaps in the time column
  site_data_var <- bind_rows(site_data_var, forecast_air)
  
  N <- nrow(site_data_var)
 
  hobo_temp <- site_data_var$hobo_temp
  hobo_temp_nogaps <- hobo_temp[!is.na(hobo_temp)]
  hobo_temp_wgaps_index <- 1:length(hobo_temp)
  hobo_temp_wgaps_index <- hobo_temp_wgaps_index[!is.na(hobo_temp)]
  
  air_temp <- site_data_var$air_temp
  air_temp_nogaps <- air_temp[!is.na(air_temp)]
  air_temp_wgaps_index <- 1:length(air_temp)
  air_temp_wgaps_index <- air_temp_wgaps_index[!is.na(air_temp)]
  

  
  data <- list(hobo_temp = hobo_temp_nogaps,
               hobo_temp_wgaps_index = hobo_temp_wgaps_index,
               air_temp = air_temp_nogaps,
               air_temp_wgaps_index = air_temp_wgaps_index,
               nobs = length(air_temp_wgaps_index),
               tau_obs = 1/(sd_wgaps ^ 2),
               n = length(y_wgaps),
               x_ic = 0.0)
  
  #Initialize parameters 
  nchain = 3
  chain_seeds <- c(200,800,1400)
  init <- list()
  for(i in 1:nchain){
    init[[i]] <- list(tau_add = 1/var(diff(air_temp_nogaps)),
                      tau_init = mean( 1/var(diff(air_temp_nogaps)), na.rm = TRUE),
                      .RNG.name = "base::Wichmann-Hill",
                      .RNG.seed = chain_seeds[i],
                      x = myinits)
  }
  
  #Initialize JAGS model
  j.model   <- jags.model (file = textConnection(TempScale),
                           data = data,
                           inits = init,
                           n.chains = 3)
  
  
  #Run JAGS model as the burn-in
  jags.out   <- coda.samples(model = j.model,variable.names = c("tau_add","tau_init"), n.iter = 10000)
  
  #Run JAGS model again and sample from the posteriors
  m   <- coda.samples(model = j.model,
                      variable.names = c("x","tau_add","tau_init", "x_obs"),
                      n.iter = 10000,
                      thin = 5)
  
  #Use TidyBayes package to clean up the JAGS output
  model_output <- m %>%
    spread_draws(x_obs[day]) %>%
    filter(.chain == 1) %>%
    rename(ensemble = .iteration) %>%
    mutate(time = full_time$time[day]) %>%
    ungroup() %>%
    select(time, x_obs, ensemble)
  
  if(generate_plots){
    #Pull in the observed data for plotting
    obs <- tibble(time = full_time$time,
                  obs = y_wgaps)
    
    
    #Post past and future
    model_output %>% 
      group_by(time) %>% 
      summarise(mean = mean(x_obs),
                upper = quantile(x_obs, 0.95),
                lower = quantile(x_obs, 0.05),.groups = "drop") %>% 
      ggplot(aes(x = time, y = mean)) +
      geom_line() +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = "lightblue", fill = "lightblue") +
      geom_point(data = obs, aes(x = time, y = obs), color = "red") +
      labs(x = "Date", y = "ebullition rate")
    
    ggsave(paste0("ebullition_",site_names[s],"_figure.pdf"), device = "pdf")
  }
  
  #Filter only the forecasted dates and add columns for required variable
  forecast_saved_tmp <- model_output %>%
    filter(time > start_forecast) %>%
    rename(oxygen = x_obs) %>% 
    mutate(data_assimilation = 0,
           forecast = 1,
           obs_flag = 2,
           siteID = site_names[s]) %>%
    mutate(forecast_iteration_id = start_forecast) %>%
    mutate(forecast_project_id = "NULL")
  
  # Combined with the previous sites
  forecast_saved_ebullition <- rbind(forecast_saved_ebullition, forecast_saved_tmp)
  
#}
