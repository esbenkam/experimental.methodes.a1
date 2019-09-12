pacman::p_load(ggplot2, tidyr, tidyverse)

?p_load

cogsci <- read.csv("data/NEW_CogSciPersonalityTest2019.csv", header = TRUE, sep=",")

cogsci <- NEW_CogSciPersonalityTest2019


#Her laver jeg fødselsdagen om til en dato som R så forstår
cogsci$birth_day <- as.Date(cogsci$birth_day, format = c("%Y-%m-%d"))

#Det er ikke alle som har skrevet den rigtige dage 
mean(cogsci$birth_day, na.rm = TRUE)

#Så jeg sortere alle dem fra, som har skrevet at deres fødselsdag er efter den 1. januar 2019
cogsci <- cogsci %>% mutate(birth_day = replace(birth_day, birth_day > "2019-01-01", NA))

mean(cogsci$birth_day, na.rm = TRUE)

count(cogsci$birth_day > "2019-01-01")

cogsci.query1 <- filter(cogsci, birth_day < "2019-01-01")
summary(cogsci.query1)

ggplot(cogsci, aes(x = shoesize, fill = gender)) + 
  geom_bar()














































































