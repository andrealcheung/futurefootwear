####Averages####Mean Mass Loss Rate#######

mean(tread_joined_noout$tread_mass_lost_per_mile)
#-0.05899063 grams/mile

mean(mass_joined_noout$measuremass_lost_per_mile)
#-0.03245783 grams/mile

median(tread_joined_noout$tread_mass_lost_per_mile)
#-0.02262389 grams/mile

median(mass_joined_noout$measuremass_lost_per_mile)
#-0.01439732 grams/mile


tread_depth_measureerror <- tread_depth_raw_initial %>% 
  mutate("measurement_difference"= final_mm-initial_mm) %>% 
  group_by(shoe_ID,model,side,location) %>% 
  summarize(
    average_measurement_difference=mean(measurement_difference)
  ) %>% 
  drop_na()

abs(tread_depth_measureerror$average_measurement_difference) #abslute value of the differences 
#the mean of these abolute values is 0.238645833