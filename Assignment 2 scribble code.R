#Scribble Code
install.packages('kableExtra')
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)
library(knitr)
library(kableExtra)

crime <- read_csv("SF_Crime.csv")
colnames(crime)
crimeDay <- data.frame(table(crime$DayOfWeek))
colnames(crimeDay)<- c("Day","Observed")
levels(crimeDay$Day)
crimeDay$Day <- factor(crimeDay$Day, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday",
                                      "Sunday"),ordered = TRUE)
levels(crimeDay$Day)
crimeDay %>%
  kbl() %>%
  kable_styling()
p <- ggplot(data = day, aes(x=Day,y=Count)) +
  geom_bar(stat="identity",width=0.7,fill="steelblue")
p

Obs_Proportion <- round(prop.table(crimeDay$Observed),3)
Expected <- rep(150500/7,7)
Exp_Proportion <- rep(1/7,7)
Day <- c("Friday","Monday","Saturday","Sunday","Thursday","Tuesday","Wednesday")
Observed <- c(crimeDay$Observed)

OvsE_Table <- data.frame(Day,Observed,Expected,Obs_Proportion,Exp_Proportion)
OvsE_Table <- t(OvsE_Table)
OvsE_Table %>% kbl() %>% kable_styling()

qchisq(p = 0.05, df = 6, lower.tail = FALSE)
pchisq(q = 290.66, df = 6,lower.tail = FALSE)

crimeChi<-chisq.test(table(crime$DayOfWeek), p = Exp_Proportion)
crimeChi
crimeChi$observed
crimeChi$expected



table(crime$DayOfWeek)









































































































