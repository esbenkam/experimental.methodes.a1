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

#Frane kun med folk der hedder Pernille eller Gustav
pernille_gustav <- filter(cogsci, name == "Pernille" | name == "Gustav")
view(pernille_gustav)

#Frame med folk med skostørrelse over 38. (Starter altså på 39)
shoes_39 <- filter(cogsci, shoesize > 38)
view(shoes_39)

#Her finder jeg de måder man kan have svaret på i dette spørgsmål
levels(cogsci$touch_floor)

#Et frame med kun folk som kan finde ud af at nå jorden
touch_floor <- filter(cogsci, touch_floor == "Yes, of course!!" | touch_floor == "Yes")
view(touch_floor)

#Her finder jeg gennemsnittet af, hvor langt tid folk kan holde vejret
mean(cogsci$breath_hold)

#Og laver en frame med dem som kan holde over
breath_mean <- filter(cogsci, breath_hold > 55.77113)
view(breath_mean)

#Her laver jeg en frame med dem som kan balancere ballonen mellem 13 og 60 sekunder.  
ballon_1360 <- filter(cogsci, balloon_balance > 13 & balloon_balance < 60)
view(ballon_1360)

#Her har jeg så lavet en frame med alle de tidliger kriteriere.
det_hele <- filter(cogsci, balloon_balance > 13 & balloon_balance < 60 & shoesize > 38 & touch_floor == "Yes, of course!!" | touch_floor == "Yes" & breath_hold > 55.77113 & balloon_balance > 13 & balloon_balance < 60)
view(det_hele)

#Her har jeg lavet en frame med dem der var langsomt til tongue twister øverst
slow_tongue <- arrange(cogsci, desc(tongue_twist))
view(slow_tongue)

#Her skulle man finde den der var bedst til Romberg forsøget, men man skulle selv definere hvad bedst var. Så jeg tog og lavede en ny kolone hvor jeg havde summen af de to værdier. Så tog jeg og sortere den, så den der samlet havde stået kortest tid, var den bedste.
cogsci$romber_total <- cogsci$romberg_open + cogsci$romberg_closed
best_romberg <- arrange(cogsci, romber_total)
view(best_romberg)

#Her skulle man prøve, at se, hvis man tager og vælger samme kolone to gange, og der sker ikke rigtig noget.
select(cogsci, name, name)

#Når man laver den første vektor og sætter den i select, tager den de værdier som passer med overskrifterne, lidt lige som hvis man selv bare havde skrevet det ind.
vars <- c("name", "shoesize", "touch_floor")
select(cogsci, vars)

#Nu skal jeg omrokere min frame, så gender og shoesize kommer først og derefter alt det andet. Her kan jeg udnyttet, at man kun kan vælge en kolone én gang. Jeg vælger først gender, shoesize og derefter alt andet.
vars_2 <- select(cogsci, "gender", "shoesize", everything())


cogsci$words_per_sec <- 99 %/% cogsci$tongue_twist


























