#Script: Abhinand S.
#Covid-19 patient clinical data analysis

data <- as.data.frame(covid_data)
firstcols <- data[,1:16]
lastcols <- data[,130:135]
#combine cols
final_data <- cbind(firstcols, lastcols)
#age histogram
library(ggplot2)
ggplot(final_data,aes(demo_age))+
  geom_histogram(binwidth = 2, color = "black",fill="lightblue") 
+ xlim(0,100) + labs(title="Age distribution",x = "Age")
age_hist <- ggplot(final_data,aes(demo_age))+
  geom_histogram(binwidth = 2, color = "black",fill="lightblue") 
+ xlim(0,100) + labs(title="Age distribution",x = "Age")
age_hist + geom_vline(aes(xintercept=mean(demo_age)),
                      color="red",linetype="dashed",size=1.5)
age_hist_2 <- age_hist + geom_vline(aes(xintercept=mean(demo_age))
                                    ,color="red",linetype="dashed",size=1.5)
#bmi histogram
ggplot(final_data,aes(x=demo_BMI))+
  geom_histogram(color="black",fill="lightgreen",binwidth = 2)
+xlim(0,70) + labs(title="BMI distribution",x="BMI")
bmi_hist <- ggplot(final_data,aes(x=demo_BMI))+
  geom_histogram(color="black",fill="lightgreen",binwidth = 2)+
  xlim(0,70) + labs(title="BMI distribution",x="BMI")
#bmi density plot
bmi_dens_hist <- ggplot(final_data,aes(x=demo_BMI))+
  geom_histogram(aes(y= ..density..),binwidth = 3,
                 color="black",fill="lightblue")+ 
  stat_function(fun=dnorm,lwd=1,col='red',
                args = list(mean = mean(final_data$demo_BMI),
                            sd=sd(final_data$demo_BMI))) + xlim(0,100)
#age density plot
age_dens_hist <- ggplot(final_data,aes(x=demo_age))+
  geom_histogram(aes(y= ..density..),binwidth = 3,
                 color="black",fill="lightpink")+ 
  stat_function(fun=dnorm,lwd=1,col='purple',
                args = list(mean = mean(final_data$demo_age),
                            sd=sd(final_data$demo_age))) + xlim(0,100)
#qqplots
qqplot_bmi <- qqnorm(final_data$demo_BMI, pch=1)
qqplot_age <- qqnorm(final_data$demo_age, pch=1)
#test for normal distribution
shapiro.test(sample(final_data$demo_age,5000))
shapiro.test(sample(final_data$demo_BMI,5000))
#extract missing data rows
missing_data <- final_data[!complete.cases(final_data),]





