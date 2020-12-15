######################################################################################
# Analysis for Tread Depth#
# Andrea Cheung, Started 3/4/20 #
######################################################################################

#### This data is still missing the surface area data for all Reebok Shoes###

library(tidyverse) # hello tidyverse
library(ggthemes)
library(janitor)
library(estimatr)
library(olsrr)

#find the average of the tread mass lost extrapolations from different parts of the foot. 
tread_mass_average <- tread_joined %>% 
  group_by(shoe_ID) %>% 
  summarize(
    average_tread_mass=mean(tread_mass_loss)
  ) 

#join step data with average tread mas lost
full_data_joined_tread <- full_join(full_data_joined, tread_joined_shoeID) 

#create a data frame that includes normalized mass loss, average tread mass divided by steps to miles, filtered out ALL CONTROLLS, and participants with not data
full_data_joined_tread_norm <- full_data_joined_tread %>% 
  mutate("tread_mass_lost_per_mile"= average_tread_mass/`steps to miles`) %>% 
  filter(name != "CONTROL") %>% 
  filter(name != "BRI WINKLER") %>% #DID NOT TRACK STEPS
  filter(name != "THOMAS BUTERA") %>% #DID NOT TRACK STEPS 
  filter(name != "TIMMY HUYNH") %>% #DID NOT RETURN
  filter(name != "CURTIS BAUMANN") %>% #DID NOT RETURN
  filter(name != "SHIVA HASSON") %>% #DID NOT RETURN
  filter(name != "GARY FOX") %>% #DID NOT TRACK STEPS 
  filter(name != "LINDA HUYNH") %>% #DID NOT RETURN
  filter(name != "BRIDGET GIBBONS") %>% #DID NOT WEAR
  filter(name != "ANDREW PATERSON (REEBOK RAINBOW)") #DID NOT WEAR
  drop_na(average_tread_mass)
#these individuals are being excluded because they did not return their shoes or did not return any step data. 
full_data_joined_tread_norm


#run a regression on average_tread_mass lost against distanced travelled
treadmass_miles_lm <- lm(formula = average_tread_mass ~ `steps to miles`, data = full_data_joined_tread_norm)
summary(treadmass_miles_lm) 
#Multiple R-squared:  0.1567,	Adjusted R-squared:  0.1484 
#F-statistic: 18.95 on 1 and 102 DF,  p-value: 3.197e-05

#add trendline to
treadmass_miles_lm_graph<- ggplot(full_data_joined_tread, aes(x=`steps to miles`, y= average_tread_mass))+
  geom_point()+
  ylim(-8,4)+
  xlim(NA, 600)+
  theme_bw()+
  labs(x = "Distance Travelled (Miles)", y = "Mass Change (g)")+
  geom_smooth(method = "lm", color = "indianred1")+ # add se = FALSE to remove error bar
  geom_hline(yintercept=c(0), color="blue")
treadmass_miles_lm_graph




######################## Updating mass change by tread NORMALIZED vs variables ############################

# Wrangling to remove high outliers, -Inf values, NA shoe types

# tread mass loss vs. rubber

treadmass_vs_rubber <- ggplot(full_data_joined_tread_norm, aes(x=rubber_type, y=tread_mass_lost_per_mile))+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust=1))+
  labs(title = "Mass Lost Calc by tread", x = "Rubber Type", y = "Mass Lost per Mile (g)")

treadmass_vs_rubber

# tread mass loss vs. hardness

treadmass_vs_hardness <- ggplot(full_data_joined_tread_norm, aes(x=hardness, y=tread_mass_lost_per_mile))+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust=1))+
  labs(title = "Mass Lost Calc by tread, hardness", x = "Hardness", y = "Mass Lost per Mile (g)")

treadmass_vs_hardness

# tread mass loss vs. geometry

treadmass_vs_geometry <- ggplot(full_data_joined_tread_norm, aes(x=geometry, y=tread_mass_lost_per_mile))+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust=1))+
  labs(title = "Mass Lost Calc by tread, geometry", x = "Geometry", y = "Mass Lost per Mile (g)")

treadmass_vs_geometry


#tread mass normalized lost based on shoe model
treadmasslost_model <- ggplot(full_data_joined_tread_norm, aes(x = model, y = tread_mass_lost_per_mile)) +
  geom_boxplot() +
  theme_bw() +
  ylim(-0.51,.4)+
  theme(axis.text.x = element_text(angle = 45, hjust=1, size=12))+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.title.y = element_text(size = 12))+
  labs(x = "", y = "Mass Change per Mile (g)")


treadmasslost_model




##################Calculating some averages for the slides, this is with all outliers########################


#mean average tread loss (mg)
mean_average_tread_mass_change <- mean(full_data_joined_tread_norm$average_tread_mass, na.rm = TRUE)
mean_average_tread_mass_change
#-2.313494

#mean average tread loss per mile (mg)
mean_average_tread_mass_changenorm <- mean(full_data_joined_tread_norm$tread_mass_lost_per_mile, na.rm = TRUE)
mean_average_tread_mass_changenorm
#-0.04747255


#############Calculating measurement error for tread depth measurements#############
tread_depth_measureerror <- tread_depth_raw_initial %>% 
  mutate("measurement_difference"= final_mm-initial_mm) %>% 
  group_by(shoe_ID,model,side,location) %>% 
  summarize(
    average_measurement_difference=mean(measurement_difference)
  ) %>% 
  drop_na()

abs(tread_depth_measureerror$average_measurement_difference) #abslute value of the differences 
#the mean of these abolute values is 0.238645833



##############Boxplot of mass_changed per mile###################

tread_mass_norm_boxplot <- ggplot(full_data_joined_tread_norm, aes(y=tread_mass_lost_per_mile))+
  geom_boxplot()+
  ylim(-0.8,.4)+
  theme_bw()

tread_mass_norm_boxplot

measured_mass_norm_boxplot <- ggplot(full_data_joined, aes(y=measuremass_lost_per_mile))+
  geom_boxplot()+ 
  ylim(-0.8,.4)+
  theme_bw()

measured_mass_norm_boxplot



###### find the median and mean of average tread mass lost #######
median(full_data_joined_tread_norm$tread_mass_lost_per_mile, na.rm = TRUE)
mean(full_data_joined_tread_norm$tread_mass_lost_per_mile, na.rm = TRUE)
#median: -0.01970962
#mean: -0.04747255



#### Analysis started 3/4/20, looking at the data again using Allison's suggestions###########

###### multivariate linear model using weight ###

# First we need to join the data set with presurvey data that has the weight

pre_survey_data <- read_csv("Pre Survey Data.csv") #read in survey data

pre_survey_data_weight <- pre_survey_data %>% 
  clean_names(.) %>% 
  select(weight, name) %>%
  mutate_if(is.character, str_to_upper) #make everything upper case

  name_ID_weight <- full_join(pre_survey_data_weight,clean_shoe_ID) %>% #join together name ID and weight in one data frame
  drop_na(weight)
  name_ID_weight

tread_joined_weight <- full_join(name_ID_weight,full_data_joined_tread_norm) %>% 
  filter(name != "CONTROL") %>% 
  filter(name != "BRI WINKLER") %>% 
  filter(name != "THOMAS BUTERA") %>% 
  filter(name != "TIMMY HUYNH") %>% 
  filter(name != "CURTIS BAUMANN") %>%
  filter(name != "SHIVA HASSON") %>%
  filter(name != "GARY FOX") %>% 
  filter(name != "LINDA HUYNH") 

#run the multivariate model using lm and lmrobust

weight_steps_lm1 <- lm(formula = average_tread_mass ~ 0 + weight + `steps to miles`, data = tread_joined_weight) #linear model with weight as a contributing factor

summary(weight_steps_lm1)
#pvalue<0.01
#weight -0.009944   
#steps to miles -0.005622 

lm1equation = function(x){coef(weight_steps_lm1)[2]*x+coef(weight_steps_lm1)[1]} #multivariate equation above to be plotted in gplot, ask Allison if I am using the right coefficients

weight_steps_lm1_plot <- ggplot(tread_joined_weight,aes(x=`steps to miles`,y=average_tread_mass,color=weight))+
  geom_point() +
  stat_function(fun=lm1equation,geom="line")+
  geom_hline(yintercept=c(0), color="dark blue")+
  theme_bw() +
  scale_y_continuous(limits = c(-7, 2)) + 
  scale_x_continuous(limits = c(0, 600)) +
  labs(title = "All observations, forced to zero", y = "Tread Derived Mass Loss", x = "Distance Travelled (Steps to Miles")


weight_steps_lm1_plot


######Do the same thing after REMOVING ALL POSITIVE VALUES#####
zero = 0

tread_joined_weight_nopos <- tread_joined_weight %>% 
  filter(average_tread_mass < zero)

weight_steps_lm2 <- lm(formula = average_tread_mass ~ 0 + weight + `steps to miles`, data = tread_joined_weight_nopos) #linear model with weight as a contributing factor

summary(weight_steps_lm2)
#weight -0.011737
#steps to miles -0.004862
#pvalue<0.01

lm2equation = function(x){coef(weight_steps_lm2)[2]*x+coef(weight_steps_lm2)[1]} #multivariate equation above to be plotted in gplot, ask Allison if I am using the right coefficients

weight_steps_lm2_plot <- ggplot(tread_joined_weight_nopos,aes(x=`steps to miles`,y=average_tread_mass,color=weight))+
  geom_point() +
  stat_function(fun=lm2equation,geom="line")+
  geom_hline(yintercept=c(0), color="dark blue")+
  theme_bw() +
  scale_y_continuous(limits = c(-7, 2)) + 
  scale_x_continuous(limits = c(0, 600)) +
  labs(title = "Forced to Zer, NO positive Values", y = "Tread Derived Mass Loss", x = "Distance Travelled (Steps to Miles")

weight_steps_lm2_plot

####################################################################################
#####Find the outliers using Cook's D!#####
#graphs outliers but I can't figure out which observation is which
cooksdplot1 <- ols_plot_cooksd_chart(weight_steps_lm1)
cooksdplot2 <- ols_plot_cooksd_chart(weight_steps_lm2)

#find outlier values, this just prints the outliers from a boxplot
average_tread_outliers <- boxplot(tread_joined_weight_nopos$average_tread_mass)
outvals = boxplot(tread_joined_weight_nopos$average_tread_mass)$out
outvals # the outliers in nopos is -8.43, -10.372, this corresponds to L50 and R50

#make a data frame with no outliers OR positive values and run the model again and graph
tread_joined_noout <- tread_joined_weight_nopos %>%
  filter(shoe_ID != "L50") %>% 
  filter(shoe_ID != "R50") #this removed the outliers from data from no positive tread dataframe


weight_steps_lm3 <- lm(formula = average_tread_mass ~ 0 + weight + `steps to miles`, data = tread_joined_noout) #linear model with weight as a contributing factor

summary(weight_steps_lm3)
#pvalue<0.01
#weight -0.009258
#steps to miles -0.004774

lm3equation = function(x){coef(weight_steps_lm3)[2]*x+coef(weight_steps_lm3)[1]}

weight_steps_lm3_plot <- ggplot(tread_joined_noout,aes(x=`steps to miles`,y=average_tread_mass,color=weight))+
  geom_point() +
  stat_function(fun=lm3equation,geom="line")+
  geom_hline(yintercept=c(0), color="dark blue")+
  theme_bw() +
  scale_y_continuous(limits = c(-8, 2)) + 
  scale_x_continuous(limits = c(0, 600)) +
  labs(title = "Forced to zero, No Outliers OR Positive Values", y = "Tread Derived Mass Loss", x = "Distance Travelled (Steps to Miles")

weight_steps_lm3_plot

####Count the sample sizes that we have for each group

tread_sample_size <- tread_joined_weight %>% 
  drop_na(average_tread_mass) %>% 
  group_by(model) %>% 
  summarize(
    sample_size = length(model)
  )
tread_sample_size

#ADIDAS ASTRARUN (60A)	8
#ADIDAS F/22 PRIME KNIT	5
#ADIDAS NIZZA(50A SYN) WHITE	6
#ADIDAS NIZZA(60A) BLACK	14
#ADIDAS TERREX BOAT	8
#ADIDAS ULTRABOOST PARLEY BLACK	9
#ADIDAS ULTRABOOST PARLEY WHITE	9
#ADIDAS ULTRATECH	5
#PARLEY PRIME LTD ADIZERO BLACK	10
#PARLEY PRIME LTD ADIZERO WHITE	8
#ULTRA BOOST STELLA	6

#Sample size no positives
tread_sample_size_nopos <- tread_joined_weight_nopos %>% 
  group_by(model) %>% 
  summarize(
    sample_size = length(model)
  )
tread_sample_size_nopos
#ADIDAS ASTRARUN (60A)	8
#ADIDAS F/22 PRIME KNIT	5
#ADIDAS NIZZA(50A SYN) WHITE	6
#ADIDAS NIZZA(60A) BLACK	9
#ADIDAS TERREX BOAT	8
#ADIDAS ULTRABOOST PARLEY BLACK	9
#ADIDAS ULTRABOOST PARLEY WHITE	9
#ADIDAS ULTRATECH	5
#ADIDAS PRIME LTD ADIZERO BLACK	10
#ADIDAS PRIME LTD ADIZERO WHITE	8
#ADIDAS	ULTRA BOOST STELLA	6

#Table for sample sizes for the remaining sample numbers after no positives and 
tread_sample_size_noout_nopos <- tread_joined_noout %>% 
  group_by(model) %>% 
  summarize(
    sample_size = length(model)
  )
tread_sample_size_noout_nopos
#ADIDAS ASTRARUN (60A)	6
#ADIDAS F/22 PRIME KNIT	5
#ADIDAS NIZZA(50A SYN) WHITE	6
#ADIDAS NIZZA(60A) BLACK	9
#ADIDAS TERREX BOAT	8
#ADIDAS ULTRABOOST PARLEY BLACK	9
#ADIDAS ULTRABOOST PARLEY WHITE	9
#ADIDAS ULTRATECH	5
#PARLEY PRIME LTD ADIZERO BLACK	10
#PARLEY PRIME LTD ADIZERO WHITE	7
#ULTRA BOOST STELLA	4
#NA is for filtered out individuals

####Averages####

mean(tread_joined_noout$tread_mass_lost_per_mile)
#-0.05899063 grams/mile

mean(mass_joined_noout$measuremass_lost_per_mile)
#-0.03245783


###############Quartiles for Tread Derived Mass loss#################

summary(tread_joined_noout$`steps to miles`)
# First quartile is 32.37 , mean is 135.42, 3rd quartile is 212.17, max is 602.50


steps_1Q_tread <- full_data_joined_tread_norm %>% 
  filter(`steps to miles`<= 29.47) 

steps_2Q_tread <- full_data_joined_tread_norm %>% 
  filter(`steps to miles` > 29.47) %>% 
  filter(`steps to miles` <= 221.35) 

steps_3Q_tread <- full_data_joined_tread_norm %>% 
  filter(`steps to miles` > 221.35) %>% 
  filter(`steps to miles` <= 602.50) 

steps_4Q_tread <- full_data_joined_tread_norm %>% 
  filter(`steps to miles` == 602.50) 




mean(steps_1Q_tread$tread_mass_lost_per_mile, na.rm = TRUE) #-0.1035245
mean(steps_2Q_tread$tread_mass_lost_per_mile, na.rm = TRUE) #-0.03629332
mean(steps_3Q_tread$tread_mass_lost_per_mile, na.rm = TRUE) #-0.01084244
mean(steps_4Q_tread$tread_mass_lost_per_mile, na.rm = TRUE) #-0.007556803

