# Forecast Evaluation pages
library(hydroGOF)

### Temperature Forecast Evaluation ###
#Temperature Forecast Evaluation Trap 1
temp_observe_t1 <- full_ebullition_model%>%
  filter(trap_id == "t1eb1")%>%
  select(time, trap_id, hobo_temp)%>%
  filter(time>="2019-07-15")

observe_model_temp_t1 <- left_join(temp_observe_t1, temp_forecast_t1_summarize, by = "time")
rmse(observe_model_temp_t1$mean, observe_model_temp_t1$hobo_temp, na.rm = T)
NSE(observe_model_temp_t1$mean, observe_model_temp_t1$hobo_temp, na.rm = T)

#Temperature Forecast Evaluation Trap 2
temp_observe_t2 <- full_ebullition_model%>%
  filter(trap_id == "t1eb2")%>%
  select(time, trap_id, hobo_temp)%>%
  filter(time>="2019-07-15")

observe_model_temp_t2 <- left_join(temp_observe_t2, temp_forecast_t2_summarize, by = "time")
rmse(observe_model_temp_t2$mean, observe_model_temp_t2$hobo_temp, na.rm = T)
NSE(observe_model_temp_t2$mean, observe_model_temp_t2$hobo_temp, na.rm = T)

#Temperature Forecast Evaluation Trap 3
temp_observe_t3 <- full_ebullition_model%>%
  filter(trap_id == "t1eb2")%>%
  select(time, trap_id, hobo_temp)%>%
  filter(time>="2019-07-15")

observe_model_temp_t3 <- left_join(temp_observe_t3, temp_forecast_t3_summarize, by = "time")
rmse(observe_model_temp_t3$mean, observe_model_temp_t3$hobo_temp, na.rm = T)
NSE(observe_model_temp_t3$mean, observe_model_temp_t3$hobo_temp, na.rm = T)

#Temperature Forecast Evaluation Trap 4
temp_observe_t4 <- alltrap%>%
  select(time,hobo_temp)%>%
  filter(time>="2019-07-15")

observe_model_temp_t4 <- left_join(temp_observe_t4, temp_forecast_t4_summarize, by = "time")
rmse(observe_model_temp_t4$mean, observe_model_temp_t4$hobo_temp, na.rm = T)
NSE(observe_model_temp_t4$mean, observe_model_temp_t4$hobo_temp, na.rm = T)

#Temperature Forecast Evaluation All traps
temp_observe_all <- full_ebullition_model%>%
  filter(trap_id == "t1eb2")%>%
  select(time, trap_id, hobo_temp)%>%
  filter(time>="2019-07-15")

observe_model_temp_all <- left_join(temp_observe_all, temp_forecast_alltraps_summarize, by = "time")
rmse(observe_model_temp_all$mean, observe_model_temp_all$hobo_temp, na.rm = T)
NSE(observe_model_temp_all$mean, observe_model_temp_all$hobo_temp, na.rm = T)


### Ebullition Forecast Evaluation ###
#Ebullition Forecast Evaluation Trap 1
ebullition_observe_t1 <- full_ebullition_model_short%>%
  filter(trap_id == "t1eb1")%>%
  select(time, trap_id, log_ebu_rate)%>%
  na.omit(.) %>%
  filter(time>="2019-07-15")

observe_model_compare_t1 <- left_join(ebullition_observe_t1, ebu_rate_forecast_t1_summarize, by = "time")
rmse(observe_model_compare_t1$mean, observe_model_compare_t1$log_ebu_rate, na.rm = T)
NSE(observe_model_compare_t1$mean, observe_model_compare_t1$log_ebu_rate, na.rm = T)

#Ebullition Forecast Evaluation Trap 2
ebullition_observe_t2 <- full_ebullition_model%>%
  filter(trap_id == "t1eb2")%>%
  select(time, trap_id, log_ebu_rate)%>%
  na.omit(.) %>%
  filter(time>="2019-07-15")

observe_model_compare_t2 <- left_join(ebullition_observe_t2, ebu_rate_forecast_t2_summarize, by = "time")
rmse(observe_model_compare_t2$mean, observe_model_compare_t2$log_ebu_rate)
NSE(observe_model_compare_t2$mean, observe_model_compare_t2$log_ebu_rate)

#Ebullition Forecast Evaluation Trap 3
ebullition_observe_t3 <- full_ebullition_model%>%
  filter(trap_id == "t1eb3")%>%
  select(time, trap_id, log_ebu_rate)%>%
  na.omit(.) %>%
  filter(time>="2019-07-15")

observe_model_compare_t3 <- left_join(ebullition_observe_t3, ebu_rate_forecast_t3_summarize, by = "time")
rmse(observe_model_compare_t3$mean, observe_model_compare_t3$log_ebu_rate)
NSE(observe_model_compare_t3$mean, observe_model_compare_t3$log_ebu_rate)

#Ebullition Forecast Evaluation Trap 4
ebullition_observe_t4 <- full_ebullition_model%>%
  filter(trap_id == "t1eb4")%>%
  select(time, trap_id, log_ebu_rate)%>%
  na.omit(.) %>%
  filter(time>="2019-07-15")

observe_model_compare_t4 <- left_join(ebullition_observe_t4, ebu_rate_forecast_t4_summarize, by = "time")
rmse(observe_model_compare_t4$mean, observe_model_compare_t4$log_ebu_rate)
NSE(observe_model_compare_t4$mean, observe_model_compare_t4$log_ebu_rate)

#Ebullition Forecast Evaluation all traps
ebullition_observe_all <- alltrap %>%
  select(time, log_ebu_rate)%>%
  na.omit(.) %>%
  filter(time>="2019-07-15")

observe_model_compare_alltrap <- left_join(ebullition_observe_all, ebu_rate_forecast_alltraps_summarize, by = "time")
rmse(observe_model_compare_alltrap$mean, observe_model_compare_alltrap$log_ebu_rate)
NSE(observe_model_compare_alltrap$mean, observe_model_compare_alltrap$log_ebu_rate)
