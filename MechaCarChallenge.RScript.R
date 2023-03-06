#part 1
library(dplyr)

mecha_car<-read.csv('MechaCar_mpg.csv')
mecha_car
mecha_df <-data.frame(mecha_car)
mecha_df

mecha_model<-lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD, data=mecha_car)
mecha_model

summary(mecha_model)


#part 2
install.packages("data.table")
library(data.table)
library(dplyr)

suspension_frame<-read.csv('Suspension_Coil.csv')
class(suspension_frame)
suspension_frame

table_sum <-suspension_frame %>% 
  summarise(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI)) 
table_sum


table2 <-suspension_frame %>% 
  group_by(Manufacturing_Lot) %>% 
  summarise(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))
table2


#part 3 T-test on suspension coils

t.test(suspension_frame$PSI, MU=1500)

t.test(subset(suspension_frame, Manufacturing_Lot=='Lot1')$PSI, mu=1500)
t.test(subset(suspension_frame, Manufacturing_Lot=='Lot2')$PSI, mu=1500)
t.test(subset(suspension_frame, Manufacturing_Lot=='Lot3')$PSI, mu=1500)













            