library(dplyr)
MechaCar <- read.csv('MechaCar_mpg.csv',check.names = F,stringsAsFactors = F)
head(MechaCar)
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar)
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar))



suspension_coil <- read.csv('Suspension_Coil.csv', check.names = F)
total_summary <- suspension_coil %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))
lot_summary <- suspension_coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))                                              



population_table <- read.csv('Suspension_Coil.csv', check.names = F)
sample_table <- population_table %>% sample_n(50)
t.test((sample_table$PSI),mu=mean(population_table$PSI))

t.test((sample_table$PSI),mu=mean(1500))

lot1 <- population_table %>% subset(Manufacturing_Lot == "Lot1")
lot2 <- population_table %>% subset(Manufacturing_Lot == "Lot2")
lot3 <- population_table %>% subset(Manufacturing_Lot == "Lot3")

t.test((lot1$PSI),mu=mean(population_table$PSI))
t.test((lot2$PSI),mu=mean(population_table$PSI))
t.test((lot3$PSI),mu=mean(population_table$PSI))
head(population_table)
