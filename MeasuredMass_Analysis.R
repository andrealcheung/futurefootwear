######################################################################################
# Round 2 of Making Graphs for our Final Report #
# Andrew Paterson, Started 3/4/20 #
######################################################################################

### Packages ###

library(tidyverse) # hello tidyverse
library(janitor) # load janitor to clean up dataframe
library(lubridate) # load lubridate to work with dates and times
library(ggthemes) # ggplot themes
library(plm)# regression analysis package

################we want to do the same thing as tread depth and create an average that has no outliers and no postive values######

zero = 0

mass_joined_weight_nopos <- full_data_joined %>% #we need to use this dataframe because the tread data frame has values removed due to no Surface Area Data
  filter(measured_mass_change < zero) 

mass_multi_lm2_new <- lm(formula = measured_mass_change ~ 0 + weight + `steps to miles`, data = mass_joined_weight_nopos) #linear model with weight as a contributing factor

summary(mass_multi_lm2_new)

# p-value << 0.001. R-squared now .5369

lmeq2_new = function(x){coef(mass_multi_lm2)[2]*x+coef(mass_multi_lm2_new)[1]} 

mass_multi_lm2_plot_new <- ggplot(mass_joined_weight_nopos,aes(x=`steps to miles`,y=measured_mass_change,color=weight))+
  geom_point() +
  stat_function(fun=lmeq2_new ,geom="line")+
  geom_hline(yintercept=c(0), color="dark blue")+
  theme_bw() +
  scale_y_continuous(limits = c(-7, 2)) + 
  scale_x_continuous(limits = c(0, 600)) +
  labs(title = "Forced to Zero, NO positive Values", y = "Mass Change (g)", x = "Distance Travelled (Miles)")

mass_multi_lm2_plot_new

########################################################################################

### Outliers using Cook's D ###

# Question for the team: Should this be based on rate of mass loss instead of total mass lost?

#####Find the outliers using Cook's D!#####
#graphs outliers but I can't figure out which observation is which
cooksdplot2 <- ols_plot_cooksd_chart(mass_multi_lm2_new) #nopos

#find outlier values, this just prints the outliers from a boxplot
average_mass_outliers2 <- boxplot(mass_joined_weight_nopos$measuremass_lost_per_mile)
outvals2 = boxplot(mass_joined_weight_nopos$measuremass_lost_per_mile)$out
outvals2 # the outliers in nopos are -0.1838642 -0.1874984 -0.7719500 -0.7860500 -0.2286885 -0.4416800 -0.1462051 -0.1722194 -0.1694932, the numbers that correspond to that re below 

### Make data frame with all Cook's D outliers removed

mass_joined_noout <- mass_joined_weight_nopos %>%
  filter(name != "CONTROL") %>% 
  filter(shoe_ID != "R18") %>% 
  filter(shoe_ID != "L18") %>% 
  filter(shoe_ID != "R24") %>% 
  filter(shoe_ID != "L24") %>% 
  filter(shoe_ID != "R31") %>% 
  filter(shoe_ID != "R25") %>% 
  filter(shoe_ID != "L70") %>% 
  filter(shoe_ID != "R70") 



### Fitting a model with outliers removed

mass_multi_lm3_new <- lm(formula = measured_mass_change ~ 0 + weight + `steps to miles`, data = mass_joined_noout) #linear model with weight as a contributing factor

summary(mass_multi_lm3_new)
# p << .001, R-squared = 0.5988

lmeq3_new = function(x){coef(mass_multi_lm3)[2]*x+coef(mass_multi_lm3_new)[1]}

mass_multi_lm3_plot_new <- ggplot(mass_joined_noout,aes(x=`steps to miles`,y=measured_mass_change,color=weight))+
  geom_point() +
  stat_function(fun=lmeq3_new,geom="line")+
  geom_hline(yintercept=c(0), color="dark blue")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 75, hjust=1), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20))+
  scale_y_continuous(limits = c(-5, 2)) + 
  scale_x_continuous(limits = c(0, 600)) +
  labs(y = "Mass Change (g)", x = "Distance Travelled (Miles)", color='Weight (lbs)')

mass_multi_lm3_plot_new




####Averages####Mean Mass Loss Rate#######

mean(tread_joined_noout$tread_mass_lost_per_mile)
#-0.05899063 grams/mile

mean(mass_joined_noout$measuremass_lost_per_mile)
#-0.03245783 grams/mile