## Do you have your smartphone with you? Behavioral barriers for measuring everyday activities with smartphone sensors
## Florian Keusch, Alexander Wenz, & Frederick G. Conrad
## https://doi.org/10.1016/j.chb.2021.107054
## Data preparation: Sample 2

## set working directory
setwd("C:/Users/Alexander/Documents/1 Research/3 Published/19 Behavioral barriers/Data/v2")

## load packages
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(hms)

## sample 2 data stored in GESIS Data Archive for the Social Sciences: https://doi.org/10.7802/2331
## import data
data <- read.csv("Smartphone-Usage-Behavior_Data_v2.csv", stringsAsFactors=FALSE)

## recode sociodemographics
data <- data %>%
  mutate(age=2019-v_2,
         agegroup=ifelse(age>=18 & age<=30, 1,
                         ifelse(age>=31 & age<=40, 2,
                                ifelse(age>=41 & age<=50, 3,
                                       ifelse(age>=51 & age<=60, 4,
                                              ifelse(age>=61, 5, NA))))),
         agegroup=factor(agegroup, levels=c(1, 2, 3, 4, 5), labels=c("18-30", "31-40", "41-50", "51-60", "61+")),
         agegroup1=ifelse(age>=18 & age<=24, 1,
                          ifelse(age>=25 & age<=30, 2,
                                 ifelse(age>=31 & age<=40, 3,
                                        ifelse(age>=41 & age<=50, 4,
                                               ifelse(age>=51 & age<=60, 5,
                                                      ifelse(age>=61, 6, NA)))))),
         agegroup1=factor(agegroup1, levels=c(1, 2, 3, 4, 5, 6), labels=c("18-24", "25-30", "31-40", "41-50", "51-60", "61+")),
         gender=ifelse(v_1=="weiblich", 1,
                       ifelse(v_1=="männlich", 0, NA)),
         gender=factor(gender, levels=c(0, 1), labels=c("Male", "Female")),
         edu=ifelse(is.na(v_6)==FALSE & v_6=="Bachelor an (Fach-)Hochschule abgeschlossen", 3,
                    ifelse(is.na(v_6)==FALSE & v_6=="Fachhochschulabschluss (z. B. Diplom, Master)", 3,
                           ifelse(is.na(v_6)==FALSE & v_6=="Universitätsabschluss (z. B. Diplom, Magister, Staatsexamen, Master)", 3,
                                  ifelse(is.na(v_6)==FALSE & v_6=="Promotion", 3,
                                         ifelse(is.na(v_4)==FALSE & v_4=="Abitur bzw. Erweiterte Oberschule mit Abschluss 12./13. Klasse (Hochschulreife)", 2,
                                                ifelse(is.na(v_4)==FALSE &v_4=="Fachhochschulreife (Abschluss einer Fachoberschule etc.)", 2, 1)))))),
         edu=factor(edu, levels=c(1, 2, 3), labels=c("No high school degree", "High school degree", "College degree")))

## recode smartphone-related characteristics
data <- data %>%
  mutate(os=ifelse(v_17=="iPhone", 1,
                   ifelse(v_17=="Android Phone", 2,
                          ifelse(v_17=="Blackberry", 3,
                                 ifelse(v_17=="Windows Phone", 3,
                                        ifelse(v_17=="Ein anderes Smartphone", 3, NA))))),
         os=factor(os, levels=c(1, 2, 3), labels=c("iPhone", "Android", "Other OS")),
         frequse=ifelse(v_26=="Mehrmals täglich", 3,
                        ifelse(v_26=="Täglich", 2,
                               ifelse(v_26=="Mehrmals in der Woche", 1,
                                      ifelse(v_26=="Mehrmals im Monat", 1,
                                             ifelse(v_26=="Einmal im Monat oder seltener", 1, NA))))),
         frequse=factor(frequse, levels=c(1, 2, 3), labels=c("Several times a week or less", "Every day", "Several times a day")),
         frequse1=ifelse(v_26=="Mehrmals täglich", 5,
                         ifelse(v_26=="Täglich", 4,
                                ifelse(v_26=="Mehrmals in der Woche", 3,
                                       ifelse(v_26=="Mehrmals im Monat", 2,
                                              ifelse(v_26=="Einmal im Monat oder seltener", 1, NA))))),
         frequse1=factor(frequse1, levels=c(1, 2, 3, 4, 5), labels=c("Once a month or less", "Several times a month", "Several times a week", "Every day", "Several times a day")),
         skills=ifelse(v_27=="1 Anfänger/in", 1,
                       ifelse(v_27=="2", 1,
                              ifelse(v_27=="3", 1,
                                     ifelse(v_27=="4", 2,
                                            ifelse(v_27=="5 Fortgeschrittene/r", 3, NA))))),
         skills=factor(skills, levels=c(1, 2, 3), labels=c("Beginner", "Intermediate", "Advanced")),
         skills1=ifelse(v_27=="1 Anfänger/in", 1,
                        ifelse(v_27=="2", 2,
                               ifelse(v_27=="3", 3,
                                      ifelse(v_27=="4", 4,
                                             ifelse(v_27=="5 Fortgeschrittene/r", 5, NA))))),
         skills1=factor(skills1, levels=c(1, 2, 3, 4, 5), labels=c("Beginner (1)", "(2)", "(3)", "(4)", "Advanced (5)")),
         hasdesktop=ifelse(v_12=="ja", 1,
                           ifelse(v_12=="nein", 0, NA)),
         hasdesktop=factor(hasdesktop, levels=c(1, 0), labels=c("Yes", "No")),
         hastablet=ifelse(v_13=="ja", 1,
                          ifelse(v_13=="nein", 0, NA)),
         hastablet=factor(hastablet, levels=c(1, 0), labels=c("Yes", "No")),
         phonedependent=ifelse(v_12=="nein" & v_13=="nein", 1, 
                               ifelse(v_12=="ja" | v_13=="ja", 0, NA)),
         phonedependent=factor(phonedependent, levels=c(0, 1), labels=c("No", "Yes")))

## recode behavioral barriers to sensor measurement
data <- data %>%
  mutate(sharing=ifelse(v_18=="Ich teile das Smartphone mit jemand anderem.", 1,
                        ifelse(v_18=="Das Smartphone wird in der Regel nur von mir genutzt.", 0, NA)),
         sharing=factor(sharing, levels=c(1, 0), labels=c("Smartphone shared with another person", "Not shared with another person")),
         hrsturnedon=ifelse(v_51>=0, v_51, NA),
         turnedoff=ifelse(hrsturnedon!=24, 1, 0),
         turnedoff=factor(turnedoff, levels=c(1, 0), labels=c("Smartphone not always on", "Always on")),
         outhomeuse=ifelse(v_54=="ja, (fast) immer", 0,
                           ifelse(v_54=="das kommt darauf an, wo ich hingehe", 1,
                                  ifelse(v_54=="nein, (fast) nie", 1, NA))),
         outhomeuse=factor(outhomeuse, levels=c(1, 0), labels=c("Smartphone left at home", "Not left at home")),
         outhomeuse1=ifelse(v_54=="ja, (fast) immer", 1,
                            ifelse(v_54=="das kommt darauf an, wo ich hingehe", 2,
                                   ifelse(v_54=="nein, (fast) nie", 3, NA))),
         outhomeuse1=factor(outhomeuse1, levels=c(1, 2, 3), labels=c("Yes, (almost) always", "It depends on where I go", "No, (almost) never")),
         outhomelocation=ifelse(v_65=="in der Hosentasche oder am Gürtel/in einem Halter am Köper", 0,
                                ifelse(v_65=="in der Hand", 0,
                                       ifelse(v_65=="in der Jacken- oder Westentasche", 0,
                                              ifelse(v_65=="in einer Handtasche, einem Rucksack oder einer anderen Tasche", 1, NA)))),
         outhomelocation=factor(outhomelocation, levels=c(1, 0), labels=c("In purse/backpack/bag", "Close to body")),
         outhomelocation1=ifelse(v_65=="in der Hosentasche oder am Gürtel/in einem Halter am Köper", 1,
                                 ifelse(v_65=="in der Hand", 4,
                                        ifelse(v_65=="in der Jacken- oder Westentasche", 2,
                                               ifelse(v_65=="in einer Handtasche, einem Rucksack oder einer anderen Tasche", 3, NA)))),
         outhomelocation1=factor(outhomelocation1, levels=c(1, 2, 3, 4), labels=c("In the pocket of my pants or on the belt/in a holster close to the body", "In a pocket of my jacket or vest", "In a purse, back pack, or other bag", "In my hand")),
         athomelocation=ifelse(v_69=="in der Hosentasche oder am Gürtel/in einem Halter am Köper", 0,
                               ifelse(v_69=="nicht am Körper aber in Reichweite und ich nehme es mit, wenn ich in einen anderen Raum gehe", 0,
                                      ifelse(v_69=="an einem fixen Platz in der Wohnung/im Haus (z.B. auf einem Tisch oder einem Kästchen)", 1,
                                             ifelse(v_69=="An einem anderen Ort und zwar", 1, NA)))),
         athomelocation=factor(athomelocation, levels=c(1, 0), labels=c("Smartphone left stationary when at home", "Not left stationary when at home")),
         athomelocation1=ifelse(v_69=="in der Hosentasche oder am Gürtel/in einem Halter am Köper", 1,
                                ifelse(v_69=="nicht am Körper aber in Reichweite und ich nehme es mit, wenn ich in einen anderen Raum gehe", 2,
                                       ifelse(v_69=="an einem fixen Platz in der Wohnung/im Haus (z.B. auf einem Tisch oder einem Kästchen)", 3,
                                              ifelse(v_69=="An einem anderen Ort und zwar", 4, NA)))),
         athomelocation1=factor(athomelocation1, levels=c(1, 2, 3, 4), labels=c("In the pocket of my pants or on the belt/in a holster close to the body", "Not on my body but somewhere within reach and I usually take the phone with me when I leave the room", "On a specific place somewhere in my home (e.g., on the table or the dresser)", "Somewhere else")),
         nightstatus=ifelse(v_66=="Das Smartphone bleibt eingeschaltet", 0,
                            ifelse(v_66=="Ich stelle das Smartphone auf lautlos/stumm/^Bitte nicht stören/Flugmodus", 0,
                                   ifelse(v_66=="Ich schalte das Smartphone komplett aus", 1, NA))),
         nightstatus=factor(nightstatus, levels=c(1, 0), labels=c("Turned off", "Turned on, Silent, do not disturb, or flight mode")),
         nightstatus1=ifelse(v_66=="Das Smartphone bleibt eingeschaltet", 3,
                             ifelse(v_66=="Ich stelle das Smartphone auf lautlos/stumm/^Bitte nicht stören/Flugmodus", 2,
                                    ifelse(v_66=="Ich schalte das Smartphone komplett aus", 1, NA))),
         nightstatus1=factor(nightstatus1, levels=c(1, 2, 3), labels=c("I completely turn my smartphone off", "I put my smartphone on silent/Do not disturb/flight mode", "I keep my smartphone turned on")),
         nightlocation=ifelse(v_67=="Es liegt bei mir im Bett", 0,
                              ifelse(v_67=="Es liegt in Reichweite neben dem Bett (z.B. auf dem Nachttisch)", 0,
                                     ifelse(v_67=="Es liegt im selben Raum wie ich, allerdings nicht in direkter Reichweite", 0,
                                            ifelse(v_67=="Es liegt in einem anderen Raum, nicht dort wo ich schlafe", 1, NA)))),
         nightlocation=factor(nightlocation, levels=c(1, 0), labels=c("In other room", "In the same room")),
         nightlocation1=ifelse(v_67=="Es liegt bei mir im Bett", 1,
                               ifelse(v_67=="Es liegt in Reichweite neben dem Bett (z.B. auf dem Nachttisch)", 2,
                                      ifelse(v_67=="Es liegt im selben Raum wie ich, allerdings nicht in direkter Reichweite", 3,
                                             ifelse(v_67=="Es liegt in einem anderen Raum, nicht dort wo ich schlafe", 4, NA)))),
         nightlocation1=factor(nightlocation1, levels=c(1, 2, 3, 4), labels=c("It is in my bed", "It is within reach close to my bed (e.g., the night stand)", "It is somewhere in the room I sleep but not within my reach", "It is in another room")),
         nightstatusandlocation=ifelse(nightlocation=="In the same room" & nightstatus=="Turned on, Silent, do not disturb, or flight mode", 0,
                                       ifelse(nightlocation=="In other room" & nightstatus=="Turned off" | nightlocation=="In other room" & nightstatus=="Turned on, Silent, do not disturb, or flight mode" | nightlocation=="In the same room" & nightstatus=="Turned off", 1, NA)),
         nightstatusandlocation=factor(nightstatusandlocation, levels=c(1, 0), labels=c("Smartphone turned off or in other room at night", "Turned on and in same room at night")))

## recode mobility
data <- data %>%
  mutate(kmtravelwd=ifelse(v_71>=0, v_71, NA),
         kmtravelwe=ifelse(v_72>=0, v_72, NA))

kmtravelwd_p97 <- quantile(data$kmtravelwd, probs=0.97, na.rm=TRUE)
kmtravelwe_p97 <- quantile(data$kmtravelwe, probs=0.97, na.rm=TRUE)

data <- data %>%
  mutate(kmtravelwd_adj1=ifelse(kmtravelwd>kmtravelwd_p97, kmtravelwd_p97, kmtravelwd),
         kmtravelwe_adj1=ifelse(kmtravelwe>kmtravelwe_p97, kmtravelwe_p97, kmtravelwe),
         kmtravelwdlog=log(kmtravelwd_adj1+0.001),
         kmtravelwelog=log(kmtravelwe_adj1+0.001))

## recode physical activity
data <- data %>%
  mutate(vpa=ifelse(v_73=="Mehr als einmal in der Woche", 1,
                    ifelse(v_73=="Einmal in der Woche", 1,
                           ifelse(v_73=="Ein- bis dreimal im Monat", 0,
                                  ifelse(v_73=="Fast nie oder nie", 0, NA)))),
         vpa=factor(vpa, levels=c(1, 0), labels=c("At least once a week", "Less than once a week")),
         vpa1=ifelse(v_73=="Mehr als einmal in der Woche", 1,
                    ifelse(v_73=="Einmal in der Woche", 0,
                           ifelse(v_73=="Ein- bis dreimal im Monat", 0,
                                  ifelse(v_73=="Fast nie oder nie", 0, NA)))),
         vpa1=factor(vpa1, levels=c(1, 0), labels=c("More than once a week", "Once a week or less")),
         vpaordinal=ifelse(v_73=="Mehr als einmal in der Woche", 4,
                           ifelse(v_73=="Einmal in der Woche", 3,
                                  ifelse(v_73=="Ein- bis dreimal im Monat", 2,
                                         ifelse(v_73=="Fast nie oder nie", 1, NA)))),
         vpaordinal=factor(vpaordinal, levels=c(1, 2, 3, 4), labels=c("Hardly ever, or never", "One to three times a month", "Once a week", "More than once a week")),
         mpa=ifelse(v_74=="Mehr als einmal in der Woche", 1,
                    ifelse(v_74=="Einmal in der Woche", 1,
                           ifelse(v_74=="Ein- bis dreimal im Monat", 0,
                                  ifelse(v_74=="Fast nie oder nie", 0, NA)))),
         mpa=factor(mpa, levels=c(1, 0), labels=c("At least once a week", "Less than once a week")),
         mpa1=ifelse(v_74=="Mehr als einmal in der Woche", 1,
                    ifelse(v_74=="Einmal in der Woche", 0,
                           ifelse(v_74=="Ein- bis dreimal im Monat", 0,
                                  ifelse(v_74=="Fast nie oder nie", 0, NA)))),
         mpa1=factor(mpa1, levels=c(1, 0), labels=c("More than once a week", "Once a week or less")),
         mpaordinal=ifelse(v_74=="Mehr als einmal in der Woche", 4,
                           ifelse(v_74=="Einmal in der Woche", 3,
                                  ifelse(v_74=="Ein- bis dreimal im Monat", 2,
                                         ifelse(v_74=="Fast nie oder nie", 1, NA)))),
         mpaordinal=factor(mpaordinal, levels=c(1, 2, 3, 4), labels=c("Hardly ever, or never", "One to three times a month", "Once a week", "More than once a week")),
         hrssitwd=ifelse(v_75>=0, v_75, NA),
         hrssitwe=ifelse(v_76>=0, v_76, NA))

hrssitwd_p97 <- quantile(data$hrssitwd, probs=0.97, na.rm=TRUE)
hrssitwe_p97 <- quantile(data$hrssitwe, probs=0.97, na.rm=TRUE)

data <- data %>%
  mutate(hrssitwd_adj1=ifelse(hrssitwd>hrssitwd_p97, hrssitwd_p97, hrssitwd),
         hrssitwe_adj1=ifelse(hrssitwe>hrssitwe_p97, hrssitwe_p97, hrssitwe))

## recode sleep
## -- recode in time format
data <- data %>%
  mutate(sleepstartwd=v_77,
         sleepstartwd=str_replace(sleepstartwd, " Uhr", ""),
         sleepstartwd=str_replace(sleepstartwd, "^(\\d{1,2})$", "\\1:00"),
         sleepstartwd=str_replace(sleepstartwd, "^(\\d{1,2}).3$", "\\1:30"),
         sleepstartwd=str_replace(sleepstartwd, "2200", "22:00"),
         sleepstartwd=str_replace(sleepstartwd, "2300", "23:00"),
         sleepstartwd=str_replace(sleepstartwd, "24:00", "0:00"),
         sleepstartwd=str_replace(sleepstartwd, "24:00:00", "0:00"),
         sleepstartwd=str_replace(sleepstartwd, "0:00:00", "0:00"),
         sleepstartwd=str_replace(sleepstartwd, "2:30 - 10:30", "2:30"),
         sleepstartwd=str_replace(sleepstartwd, "2:30 - 10:30", "2:30"),
         sleepstartwd=str_replace(sleepstartwd, "1.00", "1:00"),
         sleepstartwd_hm=hm(sleepstartwd, quiet=TRUE),
         sleepstartwd_pm=pm(sleepstartwd_hm),
         
         sleependwd=v_78,
         sleependwd=str_replace(sleependwd, " Uhr", ""),
         sleependwd=str_replace(sleependwd, "^(\\d{1,2})$", "\\1:00"),
         sleependwd=str_replace(sleependwd, "^(\\d{1,2}).3$", "\\1:30"),
         sleependwd=str_replace(sleependwd, "Zwischen 6 und 8Uhr", "7:00"),
         sleependwd=str_replace(sleependwd, "5,30", "5:30"),
         sleependwd=str_replace(sleependwd, "600", "6:00"),
         sleependwd=str_replace(sleependwd, "630", "6:30"),
         sleependwd=str_replace(sleependwd, "7.00", "7:00"),
         sleependwd_hm=hm(sleependwd, quiet=TRUE),
         sleependwd_pm=pm(sleependwd_hm),
         
         sleepstartwe=v_79,
         sleepstartwe=str_replace(sleepstartwe, " Uhr", ""),
         sleepstartwe=str_replace(sleepstartwe, "Uhr", ""),
         sleepstartwe=str_replace(sleepstartwe, "^(\\d{1,2})$", "\\1:00"),
         sleepstartwe=str_replace(sleepstartwe, "^(\\d{1,2}).3$", "\\1:30"),
         sleepstartwe=str_replace(sleepstartwe, "2200", "22:00"),
         sleepstartwe=str_replace(sleepstartwe, "2300", "23:00"),
         sleepstartwe=str_replace(sleepstartwe, "2400", "0:00"),
         sleepstartwe=str_replace(sleepstartwe, "1.00", "1:00"),
         sleepstartwe=str_replace(sleepstartwe, "2-3 uhr", "2:30"),
         sleepstartwe=str_replace(sleepstartwe, "24:00:00", "0:00"),
         sleepstartwe=str_replace(sleepstartwe, "24:00", "0:00"),
         sleepstartwe=str_replace(sleepstartwe, "3 - 11:00", "3:00"),
         sleepstartwe_hm=hm(sleepstartwe, quiet=TRUE),
         sleepstartwe_pm=pm(sleepstartwe_hm),
         
         sleependwe=v_80,
         sleependwe=str_replace(sleependwe, " Uhr", ""),
         sleependwe=str_replace(sleependwe, "^(\\d{1,2})$", "\\1:00"),
         sleependwe=str_replace(sleependwe, "^(\\d{1,2}).3$", "\\1:30"),
         sleependwe=str_replace(sleependwe, "9-10.00", "09:30"),
         sleependwe_hm=hm(sleependwe, quiet=TRUE),
         sleependwe_pm=pm(sleependwe_hm))

## -- recode in datetime format
data <- data %>%
  mutate(sleeptypewd=ifelse(sleepstartwd_pm==TRUE & sleependwd_pm==FALSE, 1, # PM-AM: Day 1-Day 2
                            ifelse(sleepstartwd_pm==FALSE & sleependwd_pm==FALSE, 2, # AM-AM: Day 1-Day 1
                                   ifelse(sleepstartwd_pm==FALSE & sleependwd_pm==TRUE, 3, # AM-PM: Day 1-Day 1
                                          ifelse(sleepstartwd_pm==TRUE & sleependwd_pm==TRUE, 4, NA)))), # PM-PM: Day 1-Day 1
         sleepstartwd_d=ifelse(sleeptypewd>0, "2018-11-18", NA),
         sleependwd_d=ifelse(sleeptypewd==1, "2018-11-19", "2018-11-18"),
         sleepstartwd_dhm=paste0(sleepstartwd_d, " ", sleepstartwd),
         sleepstartwd_dhm=ymd_hm(sleepstartwd_dhm, quiet=TRUE),
         sleependwd_dhm=paste0(sleependwd_d, " ", sleependwd),
         sleependwd_dhm=ymd_hm(sleependwd_dhm, quiet=TRUE),
         
         sleeptypewe=ifelse(sleepstartwe_pm==TRUE & sleependwe_pm==FALSE, 1, # PM-AM: Day 1-Day 2
                            ifelse(sleepstartwe_pm==FALSE & sleependwe_pm==FALSE, 2, # AM-AM: Day 1-Day 1
                                   ifelse(sleepstartwe_pm==FALSE & sleependwe_pm==TRUE, 3, # AM-PM: Day 1-Day 1
                                          ifelse(sleepstartwe_pm==TRUE & sleependwe_pm==TRUE, 4, NA)))), # PM-PM: Day 1-Day 1
         sleepstartwe_d=ifelse(sleeptypewe>0, "2018-11-18", NA),
         sleependwe_d=ifelse(sleeptypewe==1, "2018-11-19", "2018-11-18"),
         sleepstartwe_dhm=paste0(sleepstartwe_d, " ", sleepstartwe),
         sleepstartwe_dhm=ymd_hm(sleepstartwe_dhm, quiet=TRUE),
         sleependwe_dhm=paste0(sleependwe_d, " ", sleependwe),
         sleependwe_dhm=ymd_hm(sleependwe_dhm, quiet=TRUE))

## -- compute sleep duration
data <- data %>%
  mutate(sleephrswd=difftime(sleependwd_dhm, sleepstartwd_dhm, units=c("hours")),
         sleephrswd=as.numeric(sleephrswd),
         
         sleephrswe=difftime(sleependwe_dhm, sleepstartwe_dhm, units=c("hours")),
         sleephrswe=as.numeric(sleephrswe))

## -- correct negative sleep duration
## RULE 1: IF TIME GOING TO BED=05:00-11:59 ON DAY 1 AND TIME GETTING UP=02:00-11:59 ON DAY 1 RECODE AS TIME GOING TO BED=17:00-23:59 ON DAY 1 AND TIME GETTING UP=02:00-11:59 ON DAY 2
## RULE 2: RECODE REMAINING CASES AS MISSING
data <- data %>%
  mutate(flag1=ifelse(between(sleepstartwd_dhm, ymd_hm("2018-11-18 05:00"), ymd_hm("2018-11-18 11:59")) & between(sleependwd_dhm, ymd_hm("2018-11-18 02:00"), ymd_hm("2018-11-18 11:59")) & sleephrswd<0, 1, 0),
         sleepstartwd_dhm=ifelse(flag1==1 & hour(sleepstartwd_dhm)==5, update(sleepstartwd_dhm, hours=17), sleepstartwd_dhm),
         sleepstartwd_dhm=as_datetime(sleepstartwd_dhm),
         sleepstartwd_dhm=ifelse(flag1==1 & hour(sleepstartwd_dhm)==6, update(sleepstartwd_dhm, hours=18), sleepstartwd_dhm),
         sleepstartwd_dhm=as_datetime(sleepstartwd_dhm),
         sleepstartwd_dhm=ifelse(flag1==1 & hour(sleepstartwd_dhm)==7, update(sleepstartwd_dhm, hours=19), sleepstartwd_dhm),
         sleepstartwd_dhm=as_datetime(sleepstartwd_dhm),
         sleepstartwd_dhm=ifelse(flag1==1 & hour(sleepstartwd_dhm)==8, update(sleepstartwd_dhm, hours=20), sleepstartwd_dhm),
         sleepstartwd_dhm=as_datetime(sleepstartwd_dhm),
         sleepstartwd_dhm=ifelse(flag1==1 & hour(sleepstartwd_dhm)==9, update(sleepstartwd_dhm, hours=21), sleepstartwd_dhm),
         sleepstartwd_dhm=as_datetime(sleepstartwd_dhm),
         sleepstartwd_dhm=ifelse(flag1==1 & hour(sleepstartwd_dhm)==10, update(sleepstartwd_dhm, hours=22), sleepstartwd_dhm),
         sleepstartwd_dhm=as_datetime(sleepstartwd_dhm),
         sleepstartwd_dhm=ifelse(flag1==1 & hour(sleepstartwd_dhm)==11, update(sleepstartwd_dhm, hours=23), sleepstartwd_dhm),
         sleepstartwd_dhm=as_datetime(sleepstartwd_dhm),
         sleependwd_dhm=ifelse(flag1==1, update(sleependwd_dhm, day=19), sleependwd_dhm),
         sleependwd_dhm=as_datetime(sleependwd_dhm),
         sleephrswd_flag1=difftime(sleependwd_dhm, sleepstartwd_dhm, units=c("hours")),
         sleephrswd=ifelse(flag1==1, sleephrswd_flag1, sleephrswd),
         flag2=ifelse(sleephrswd<0 & flag1==0, 1, 0),
         sleephrswd=ifelse(flag2==1, NA, sleephrswd),
         flag10=ifelse(between(sleepstartwe_dhm, ymd_hm("2018-11-18 05:00"), ymd_hm("2018-11-18 11:59")) & between(sleependwe_dhm, ymd_hm("2018-11-18 02:00"), ymd_hm("2018-11-18 11:59")) & sleephrswe<0, 1, 0),
         sleepstartwe_dhm=ifelse(flag10==1 & hour(sleepstartwe_dhm)==5, update(sleepstartwe_dhm, hours=17), sleepstartwe_dhm),
         sleepstartwe_dhm=as_datetime(sleepstartwe_dhm),
         sleepstartwe_dhm=ifelse(flag10==1 & hour(sleepstartwe_dhm)==6, update(sleepstartwe_dhm, hours=18), sleepstartwe_dhm),
         sleepstartwe_dhm=as_datetime(sleepstartwe_dhm),
         sleepstartwe_dhm=ifelse(flag10==1 & hour(sleepstartwe_dhm)==7, update(sleepstartwe_dhm, hours=19), sleepstartwe_dhm),
         sleepstartwe_dhm=as_datetime(sleepstartwe_dhm),
         sleepstartwe_dhm=ifelse(flag10==1 & hour(sleepstartwe_dhm)==8, update(sleepstartwe_dhm, hours=20), sleepstartwe_dhm),
         sleepstartwe_dhm=as_datetime(sleepstartwe_dhm),
         sleepstartwe_dhm=ifelse(flag10==1 & hour(sleepstartwe_dhm)==9, update(sleepstartwe_dhm, hours=21), sleepstartwe_dhm),
         sleepstartwe_dhm=as_datetime(sleepstartwe_dhm),
         sleepstartwe_dhm=ifelse(flag10==1 & hour(sleepstartwe_dhm)==10, update(sleepstartwe_dhm, hours=22), sleepstartwe_dhm),
         sleepstartwe_dhm=as_datetime(sleepstartwe_dhm),
         sleepstartwe_dhm=ifelse(flag10==1 & hour(sleepstartwe_dhm)==11, update(sleepstartwe_dhm, hours=23), sleepstartwe_dhm),
         sleepstartwe_dhm=as_datetime(sleepstartwe_dhm),
         sleependwe_dhm=ifelse(flag10==1, update(sleependwe_dhm, day=19), sleependwe_dhm),
         sleependwe_dhm=as_datetime(sleependwe_dhm),
         sleephrswe_flag10=difftime(sleependwe_dhm, sleepstartwe_dhm, units=c("hours")),
         sleephrswe=ifelse(flag10==1, sleephrswe_flag10, sleephrswe),
         flag20=ifelse(sleephrswe<0 & flag10==0, 1, 0),
         sleephrswe=ifelse(flag20==1, NA, sleephrswe))

## -- correct zero sleep duration
## RULE 3: RECODE CASES AS MISSING
data <- data %>%
  mutate(flag3=ifelse(sleephrswd==0, 1, 0),
         flag30=ifelse(sleephrswe==0, 1, 0),
         sleephrswd=ifelse(sleephrswd==0, NA, sleephrswd),
         sleephrswe=ifelse(sleephrswe==0, NA, sleephrswe))

## -- correct sleep duration>=11 hours
## RULE 4: IF TIME GOING TO BED=12:00-12:59 ON DAY 1 AND TIME GETTING UP=05:00-10:30 ON DAY 2 RECODE AS TIME GOING TO BED=0:00-0:59 ON DAY 2 AND TIME GETTING UP=05:00-10:30 ON DAY 2
## RULE 5: DO NOT RECODE REMAINING CASES
data <- data %>%
  mutate(flag4=ifelse(between(sleepstartwd_dhm, ymd_hm("2018-11-18 12:00"), ymd_hm("2018-11-18 12:59")) & between(sleependwd_dhm, ymd_hm("2018-11-19 05:00"), ymd_hm("2018-11-19 10:30")) & sleephrswd>=11, 1, 0),
         sleepstartwd_dhm=ifelse(flag4==1, update(sleepstartwd_dhm, day=19, hours=0), sleepstartwd_dhm),
         sleepstartwd_dhm=as_datetime(sleepstartwd_dhm),
         sleephrswd_flag4=difftime(sleependwd_dhm, sleepstartwd_dhm, units=c("hours")),
         sleephrswd=ifelse(flag4==1, sleephrswd_flag4, sleephrswd),
         flag40=ifelse(between(sleepstartwe_dhm, ymd_hm("2018-11-18 12:00"), ymd_hm("2018-11-18 12:59")) & between(sleependwe_dhm, ymd_hm("2018-11-19 05:00"), ymd_hm("2018-11-19 10:30")) & sleephrswe>=11, 1, 0),
         sleepstartwe_dhm=ifelse(flag4==1, update(sleepstartwe_dhm, day=19, hours=0), sleepstartwe_dhm),
         sleepstartwe_dhm=as_datetime(sleepstartwe_dhm),
         sleephrswe_flag40=difftime(sleependwe_dhm, sleepstartwe_dhm, units=c("hours")),
         sleephrswe=ifelse(flag40==1, sleephrswe_flag40, sleephrswe))

## -- adjust for outliers
sleephrswd_p3 <- quantile(data$sleephrswd, probs=0.03, na.rm=TRUE)
sleephrswd_p97 <- quantile(data$sleephrswd, probs=0.97, na.rm=TRUE)

sleephrswe_p3 <- quantile(data$sleephrswe, probs=0.03, na.rm=TRUE)
sleephrswe_p97 <- quantile(data$sleephrswe, probs=0.97, na.rm=TRUE)

data <- data %>%
  mutate(sleephrswd_adj1=ifelse(sleephrswd<sleephrswd_p3, sleephrswd_p3,
                                ifelse(sleephrswd>sleephrswd_p97, sleephrswd_p97, sleephrswd)),
         sleephrswe_adj1=ifelse(sleephrswe<sleephrswe_p3, sleephrswe_p3,
                                ifelse(sleephrswe>sleephrswe_p97, sleephrswe_p97, sleephrswe)))

# save data
save(data, file="Behavioral barriers_Sample2.RData")