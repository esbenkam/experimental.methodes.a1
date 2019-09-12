pacman::p_load(ggplot2, tidyr, tidyverse, data.table, lme4)

#Først importere jeg datasættet
cogsci <- read.csv("data/NEW_CogSciPersonalityTest2019.csv", header = TRUE, sep=",")

View(cogsci)


#Her laver jeg fødselsdagen om til en dato som R så forstår
cogsci$birth_day <- as.Date(cogsci$birth_day, format = c("%Y-%m-%d"))

#Det er ikke alle som har skrevet den rigtige dage 
mean(cogsci$birth_day, na.rm = TRUE)

#Så jeg sortere alle dem fra, som har skrevet at deres fødselsdag er efter den 1. januar 2019
cogsci <- cogsci %>% mutate(birth_day = replace(birth_day, birth_day > "2019-01-01", NA))

#Nu kan man se, at det passer meget bedre
mean(cogsci$birth_day, na.rm = TRUE)

#Her kan man se fordelingne af skostørelser ift. køn
ggplot(cogsci, aes(x = shoesize, fill = gender)) + 
  geom_bar()

#Her laver jeg en ny frame, hvor det kun er folk med skostørrelse 40
shoes_40 <- filter(cogsci, shoesize == 40)
view(shoes_40)

#Og en med folk med skostørrelse over 45
shoes_bigfoot <- filter(cogsci, shoesize > 45)
view(shoes_bigfoot)

#Her får jeg en frame med kun dem som både er mænd og venstrehåndet
male_lefthand <- filter(cogsci, gender == "male" & handedness == "Left-handed")
view(male_lefthand)

pernille_gustav <- filter(cogsci, name == "Pernille" | name == "Gustav")
view(pernille_gustav)

shoes_39 <- filter(cogsci, shoesize > 38)
view(shoes_39)

levels(cogsci$touch_floor)

touch_floor <- filter(cogsci, touch_floor == "Yes, of course!!" | touch_floor == "Yes")
view(touch_floor)

mean(cogsci$breath_hold)

breath_mean <- filter(cogsci, breath_hold > 55.77112)
view(breath_mean)

ballon_1360 <- filter(cogsci, balloon_balance > 13 & balloon_balance < 60)
view(ballon_1360)






















































