# -----------------------------------------------------------------
# Analysis code for:
# Keusch, F., Wenz, A., & Conrad, F. (in press). Do you have your smartphone with you?
# Behavioral barriers for measuring everyday activities with smartphone sensors.
# Computers in Human Behavior. https://doi.org/10.1016/j.chb.2021.107054
# Data preparation: Sample 1
# -----------------------------------------------------------------
# Set working directory
setwd("U:\\Projekte\\GIP\\W48")

# -----------------------------------------------------------------
# Load packages
library(tidyverse)
library(dplyr)
library(naniar)
library(gmodels)
library(pscl)
library(margins)
library(MASS)
library(DescTools)
library(vcd)

# -----------------------------------------------------------------
# Sample a data stored in GESIS Data Archive for the Social Sciences: https://doi.org/10.7802/2331
# Load data set
load("GIP_W48_V1.Rdata")

# -----------------------------------------------------------------
# Inspect dataset
dim(GIP_W48_V1)
head(GIP_W48_V1)
tail(GIP_W48_V1)
str(GIP_W48_V1)
View(GIP_W48_V1)

# -----------------------------------------------------------------
# Drop Break-offs
gipw48 <- GIP_W48_V1 %>%
  drop_na(AJ48030)

# -----------------------------------------------------------------
# Select relevant variables
gipw48 <- gipw48 %>%
  dplyr::select(id_g, gender_19, year_of_birth_cat_19, educ_school_19,
         educ_job_19, AJ48030, AJ48031, AJ48032, AJ48004, AJ48005, AJ48101,
         AJ48102_a, AJ48102_b, AJ48102_c, AJ48102_d, AJ48102_e,
         AJ48102_f, AM48001, AM48003, AJ48103, AJ48104, AJ48105,
         AJ48106_a, AJ48106_b, AJ48106_c, AJ48106_d, AJ48106_e,
         AJ48106_f, AJ48106_g, AJ48106_h, AJ48106_i, AJ48107,
         AJ48108, AJ48109)

# -----------------------------------------------------------------
# Convert all NAs
summary(gipw48$id_g)
summary(gipw48$gender_19)
summary(gipw48$year_of_birth_cat_19)
summary(gipw48$educ_school_19)
summary(gipw48$educ_job_19)
summary(gipw48$AJ48030)
summary(gipw48$AJ48031)
summary(gipw48$AJ48032)
summary(gipw48$AJ48004)
summary(gipw48$AJ48005)
summary(gipw48$AJ48101)
summary(gipw48$AJ48102_a)
summary(gipw48$AJ48102_b)
summary(gipw48$AJ48102_c)
summary(gipw48$AJ48102_d)
summary(gipw48$AJ48102_e)
summary(gipw48$AJ48102_f)
summary(gipw48$AM48001)
summary(gipw48$AM48003)
summary(gipw48$AJ48103)
summary(gipw48$AJ48104)
summary(gipw48$AJ48105)
summary(gipw48$AJ48106_a)
summary(gipw48$AJ48106_b)
summary(gipw48$AJ48106_c)
summary(gipw48$AJ48106_d)
summary(gipw48$AJ48106_e)
summary(gipw48$AJ48106_f)
summary(gipw48$AJ48106_g)
summary(gipw48$AJ48106_h)
summary(gipw48$AJ48106_i)
summary(gipw48$AJ48107)
summary(gipw48$AJ48108)
summary(gipw48$AJ48109)

na_strings <- c("-97. trifft nicht zu",
                "-91. 'bitte wählen'-Platzhalter",
                "-90. item nonresponse",
                "-80. Wert nicht plausibel",
                "NA's"
                )

na_var <- c("id_g", "gender_19", "year_of_birth_cat_19", "educ_school_19",
            "educ_job_19", "AJ48030", "AJ48031", "AJ48032","AJ48004", "AJ48005",
            "AJ48101", "AJ48102_a", "AJ48102_b", "AJ48102_c", "AJ48102_d",
            "AJ48102_e", "AJ48102_f", "AM48001", "AM48003", "AJ48103",
            "AJ48104", "AJ48105", "AJ48106_a", "AJ48106_b", "AJ48106_c",
            "AJ48106_d", "AJ48106_e", "AJ48106_f", "AJ48106_g", "AJ48106_h",
            "AJ48106_i", "AJ48107", "AJ48108", "AJ48109")

gipw48 <- gipw48 %>%
  mutate_if(is.factor, as.character) %>%
  replace_with_na_all(condition = ~.x %in% na_strings) %>%
  mutate_if(is.character, as.factor) %>%
  replace_with_na(replace = list(AJ48103 = -90))

# -----------------------------------------------------------------
# Subset to smartphone owners only
prop.table(table(gipw48$AJ48030))
prop.table(table(gipw48$AJ48004))

gipw48 <- gipw48 %>%
  mutate(smartphone = ifelse(AJ48030 == "1. ja, besitze ich" & 
                               AJ48004 == "1. ja, ein Smartphone", 1,
                         ifelse(AJ48030 == "1. ja, besitze ich" & 
                                  AJ48004 == "2. nein, kein Smartphone", 2,
                                ifelse(AJ48030 == "1. ja, besitze ich" & 
                                         AJ48004 == "-99. bin nicht sicher/wei�Y nicht", 2, NA))),
         smartphone = factor(smartphone, levels = c(1, 2), labels = c("owns smartphone", "does not own smartphopne")))
prop.table(table(gipw48$smartphone))

gipw48_sp <- gipw48 %>%
  filter(smartphone == "owns smartphone")

# -----------------------------------------------------------------
# Recode variables
# Gender
summary(gipw48_sp$gender_19)
gipw48_sp <- gipw48_sp %>%
  mutate(gender = ifelse(gender_19 == "2. weiblich", 1,
              ifelse(gender_19 == "1. männlich", 0, NA)),
         gender = factor(gender, levels = c(0, 1), labels = c("Male", "Female")))
summary(gipw48_sp$gender)
prop.table(table(gipw48_sp$gender))

# Age
summary(gipw48_sp$year_of_birth_cat_19)
gipw48_sp <- gipw48_sp %>%
  mutate(agegroup = ifelse(year_of_birth_cat_19 == "14. 2000 und später" |
                             year_of_birth_cat_19 == "13. 1995-1999" |
                             year_of_birth_cat_19 == "12. 1990-1994", 1,
                         ifelse(year_of_birth_cat_19 == "10. 1980-1984" | 
                                  year_of_birth_cat_19 == "11. 1985-1989", 2,
                                ifelse(year_of_birth_cat_19 == "8. 1970-1974" |
                                         year_of_birth_cat_19 == "9. 1975-1979", 3,
                                       ifelse(year_of_birth_cat_19 == "6. 1960-1964" |
                                                year_of_birth_cat_19 == "7. 1965-1969", 4,
                                              ifelse(year_of_birth_cat_19 == "1. 1935-1939" | 
                                                       year_of_birth_cat_19 == "2. 1940-1944" |
                                                       year_of_birth_cat_19 == "3. 1945-1949" |
                                                       year_of_birth_cat_19 == "4. 1950-1954" | 
                                                       year_of_birth_cat_19 == "5. 1955-1959", 5, NA))))),
         agegroup = factor(agegroup, levels = c(1, 2, 3, 4, 5), labels = c("18-30", "31-40", "41-50", "51-60", "61+")))
summary(gipw48_sp$agegroup)
prop.table(table(gipw48_sp$agegroup))
plot(gipw48_sp$agegroup)

# Age alternate 1
summary(gipw48_sp$year_of_birth_cat_19)
gipw48_sp <- gipw48_sp %>%
  mutate(agegroup1 = ifelse(year_of_birth_cat_19 == "14. 2000 und später" |
                             year_of_birth_cat_19 == "13. 1995-1999", 1,
                           ifelse(year_of_birth_cat_19 == "12. 1990-1994", 2,
                                  ifelse(year_of_birth_cat_19 == "10. 1980-1984" |
                                           year_of_birth_cat_19 == "11. 1985-1989", 3,
                                         ifelse(year_of_birth_cat_19 == "8. 1970-1974" |
                                                  year_of_birth_cat_19 == "9. 1975-1979", 4,
                                                ifelse(year_of_birth_cat_19 == "6. 1960-1964" |
                                                         year_of_birth_cat_19 == "7. 1965-1969", 5,
                                                       ifelse(year_of_birth_cat_19 == "1. 1935-1939" |
                                                                year_of_birth_cat_19 == "2. 1940-1944" |
                                                                year_of_birth_cat_19 == "3. 1945-1949" |
                                                                year_of_birth_cat_19 == "4. 1950-1954" |
                                                                year_of_birth_cat_19 == "5. 1955-1959", 6, NA)))))),
         agegroup1 = factor(agegroup1, levels = c(1, 2, 3, 4, 5, 6), labels = c("18-24", "25-30", "31-40", "41-50", "51-60", "61+")))
summary(gipw48_sp$agegroup1)
prop.table(table(gipw48_sp$agegroup1))
plot(gipw48_sp$agegroup1)

# Education
summary(gipw48_sp$educ_school_19)
summary(gipw48_sp$educ_job_19)
gipw48_sp <- gipw48_sp %>%
  mutate(edu = ifelse(educ_job_19 == "8. Bachelor an (Fach-)Hochschule abgeschlossen" |
                        educ_job_19 == "9. Fachhochschulabschluss (z. B. Diplom, Master)" |
                        educ_job_19 == "10. Universitätsabschluss (z. B. Diplom, Magister, Staatsexamen, Master)" |
                        educ_job_19 == "11. Promotion", 3,
                      ifelse(educ_school_19 == "5. Fachhochschulreife (Abschluss einer Fachoberschule etc.)" |
                        educ_school_19 == "6. Abitur bzw. Erweiterte Oberschule mit Abschluss 12. Klasse (Hochschulreife)", 2, 1)),
         edu = factor(edu, levels = c(1, 2, 3), labels = c("no high school degree", "High school degree", "College degree")))
summary(gipw48_sp$edu)
prop.table(table(gipw48_sp$edu))
plot(gipw48_sp$edu)

# OS
summary(gipw48_sp$AJ48005)
gipw48_sp <- gipw48_sp %>%
  mutate(os = ifelse(AJ48005 == "1. iPhone", 1,
          ifelse(AJ48005 == "2. Android Phone", 2,
                 ifelse(AJ48005 == "3. Windows Phone" |
                          AJ48005 == "3. Windows Phone" |
                          AJ48005 == "5. etwas anderes" |
                          AJ48005 == "-99. bin nicht sicher/wei�Y nicht", 3, NA))),
         os = factor(os, levels = c(1, 2, 3), labels = c("iPhone", "Android", "Other OS")))
summary(gipw48_sp$os)
prop.table(table(gipw48_sp$os))

# Frequency of smartphone use
summary(gipw48_sp$AM48001)
prop.table(table(gipw48_sp$AM48001))
gipw48_sp <- gipw48_sp %>%
  mutate(frequse = ifelse(AM48001 == "1. mehrmals täglich", 3,
                          ifelse(AM48001 == "2. täglich", 2,
                                 ifelse(AM48001 == "3. mehrmals in der Woche" |
                                          AM48001 == "4. mehrmals im Monat" |
                                          AM48001 == "5. einmal im Monat oder seltener", 1, NA))),
         frequse = factor(frequse, levels = c(1, 2, 3),
                          labels = c("Several times a week or less", "Every day", "Several times a day")))
summary(gipw48_sp$frequse)
prop.table(table(gipw48_sp$frequse))

# Smartphone skills
summary(gipw48_sp$AM48003)
prop.table(table(gipw48_sp$AM48003))
gipw48_sp <- gipw48_sp %>%
  mutate(skills = ifelse(AM48003 == "5. 5 Fortgeschrittene/-r", 3,
                         ifelse(AM48003 == "4. 4", 2,
                                ifelse(AM48003 == "3. 3" |
                                         AM48003 == "2. 2" |
                                         AM48003 == "1. 1 Anfänger/-in", 1, NA))),
         skills = factor(skills, levels = c(1, 2, 3),
                         labels = c("Beginner", "Intermediate", "Advanced")))

summary(gipw48_sp$skills)
prop.table(table(gipw48_sp$skills))

# Smartphone dependency
summary(gipw48_sp$AJ48030)
prop.table(table(gipw48_sp$AJ48030))
summary(gipw48_sp$AJ48031)
prop.table(table(gipw48_sp$AJ48031))
summary(gipw48_sp$AJ48032)
prop.table(table(gipw48_sp$AJ48032))
gipw48_sp <- gipw48_sp %>%
  mutate(phonedependent = ifelse(AJ48031 == "1. ja, besitze ich" |
                                  AJ48032 == "1. ja, besitze ich", 1,
                         ifelse(AJ48031 == "2. nein, besitze ich nicht" &
                                  AJ48032 == "2. nein, besitze ich nicht", 2, NA)),
         phonedependent = factor(phonedependent, levels = c(1, 2),
                          labels = c("No", "Yes")))
summary(gipw48_sp$phonedependent)
prop.table(table(gipw48_sp$phonedependent))

# Smartphone barriers
# Smartphone sharing
summary(gipw48_sp$AJ48101)
gipw48_sp <- gipw48_sp %>%
  mutate(sharing = ifelse(AJ48101 == "1. Das Smartphone wird in der Regel nur von mir genutzt", 0,
                     ifelse(AJ48101 == "2. Ich teile das Smartphone mit jemand anderem", 1, NA)),
      sharing = factor(sharing, levels = c(0, 1), labels = c("No", "Yes")))
summary(gipw48_sp$sharing)
prop.table(table(gipw48_sp$sharing))

# Smartphone time turned off
summary(gipw48_sp$AJ48103)
sd(gipw48_sp$AJ48103, na.rm = T)
hist(gipw48_sp$AJ48103)

gipw48_sp <- gipw48_sp %>%
  mutate(h_turnedoff = (AJ48103-24)*-1)
summary(gipw48_sp$h_turnedoff)
hist(gipw48_sp$h_turnedoff)

summary(gipw48_sp$h_turnedoff[gipw48_sp$agegroup == "18-30"])
sd(gipw48_sp$h_turnedoff[gipw48_sp$agegroup == "18-30"], na.rm = T)
summary(gipw48_sp$h_turnedoff[gipw48_sp$agegroup == "31-40"])
sd(gipw48_sp$h_turnedoff[gipw48_sp$agegroup == "31-40"], na.rm = T)
summary(gipw48_sp$h_turnedoff[gipw48_sp$agegroup == "41-50"])
sd(gipw48_sp$h_turnedoff[gipw48_sp$agegroup == "41-50"], na.rm = T)
summary(gipw48_sp$h_turnedoff[gipw48_sp$agegroup == "51-60"])
sd(gipw48_sp$h_turnedoff[gipw48_sp$agegroup == "51-60"], na.rm = T)
summary(gipw48_sp$h_turnedoff[gipw48_sp$agegroup == "61+"])
sd(gipw48_sp$h_turnedoff[gipw48_sp$agegroup == "61+"], na.rm = T)

ggplot(gipw48_sp, aes(x = agegroup, y = h_turnedoff)) + 
  geom_boxplot()

# Smartphone not always on
gipw48_sp <- gipw48_sp %>%
  mutate(turnedoff = ifelse(h_turnedoff == 0, 0,
                            ifelse(h_turnedoff > 0 & h_turnedoff < 25, 1, NA)),
         turnedoff = factor(turnedoff, levels = c(0, 1), labels = c("No", "Yes")))
summary(gipw48_sp$turnedoff)
prop.table(table(gipw48_sp$turnedoff))

# Smartphone not always out-of-home use
summary(gipw48_sp$AJ48104)
prop.table(table(gipw48_sp$AJ48104))
gipw48_sp <- gipw48_sp %>%
  mutate(outhomeuse = ifelse(AJ48104 == "1. ja, (fast) immer", 0,
                             ifelse(AJ48104 == "2. das kommt darauf an, wo ich hingehe" |
                                      AJ48104 == "3. nein, (fast) nie", 1, NA)),
         outhomeuse = factor(outhomeuse, levels = c(0, 1), labels=c("No", "Yes")))
summary(gipw48_sp$outhomeuse)
prop.table(table(gipw48_sp$outhomeuse))

# Smartphone not carried on body while out-of-home
summary(gipw48_sp$AJ48105)
prop.table(table(gipw48_sp$AJ48105))
gipw48_sp <- gipw48_sp %>%
  mutate(outhomelocation = ifelse(AJ48105 == "1. in der Hosentasche oder am Gürtel/in einem Halter am Körper" |
                                    AJ48105 == "2. in der Jacken- oder Westentasche" |
                                    AJ48105 == "4. in der Hand", 0,
                                  ifelse(AJ48105 == "3. in einer Handtasche, einem Rucksack oder einer anderen Tasche", 1, NA)),
         outhomelocation = factor(outhomelocation, levels = c(0, 1), labels = c("No", "Yes")))
summary(gipw48_sp$outhomelocation)
prop.table(table(gipw48_sp$outhomelocation))

# Smartphone stationary at home
summary(gipw48_sp$AJ48109)
prop.table(table(gipw48_sp$AJ48109))
gipw48_sp <- gipw48_sp %>%
  mutate(athomelocation = ifelse(AJ48109 == "1. in meiner Hosentasche oder am Gürtel/in einem Halter am Körper" |
                                 AJ48109 == "2. nicht am Körper, aber in Reichweite und ich nehme es mit, wenn ich in einen and", 0,
                               ifelse(AJ48109 == "3. an einem fixen Platz in der Wohnung/im Haus (z. B. auf einem Tisch)" |
                                        AJ48109 == "4. an einem anderen Ort, und zwar: [answer field]", 1, NA)),
         athomelocation = factor(athomelocation, levels = c(0, 1), labels = c("No", "Yes")))
summary(gipw48_sp$athomelocation)
prop.table(table(gipw48_sp$athomelocation))

# Night turned off
summary(gipw48_sp$AJ48107)
prop.table(table(gipw48_sp$AJ48107))
gipw48_sp <- gipw48_sp %>%
  mutate(nightstatus = ifelse(AJ48107 == "1. Ich schalte das Smartphone komplett aus", 1,
                              ifelse(AJ48107 == "2. Ich stelle das Smartphone auf lautlos/stumm/Bitte nicht stören?/Flugmodus" |
                                       AJ48107 == "3. Das Smartphone bleibt eingeschaltet", 0, NA)),
         nightstatus = factor(nightstatus, levels = c(0, 1), labels = c("Turned on", "Turned off")))
summary(gipw48_sp$nightstatus)
prop.table(table(gipw48_sp$nightstatus))

# Night in other room location
summary(gipw48_sp$AJ48108)
prop.table(table(gipw48_sp$AJ48108))
gipw48_sp <- gipw48_sp %>%
  mutate(nightlocation = ifelse(AJ48108 == "4. Es liegt in einem anderen Raum, nicht dort wo ich schlafe", 1,
                              ifelse(AJ48108 == "1. Es liegt bei mir im Bett" |
                                       AJ48108 == "2. Es liegt in Reichweite neben dem Bett (z. B. auf dem Nachttisch)" |
                                       AJ48108 == "3. Es liegt im selben Raum wie ich, allerdings nicht in direkter Reichweite", 0, NA)),
         nightlocation = factor(nightlocation, levels = c(0, 1), labels = c("No", "Yes")))
summary(gipw48_sp$nightlocation)
prop.table(table(gipw48_sp$nightlocation))

# Combined night status and night location
table(gipw48_sp$nightstatus, gipw48_sp$nightlocation)
prop.table(table(gipw48_sp$nightstatus, gipw48_sp$nightlocation))
chisq.test(table(gipw48_sp$nightstatus, gipw48_sp$nightlocation))

gipw48_sp <- gipw48_sp %>%
  mutate(nightoff_location = ifelse(nightstatus == "Turned off" |
                                      nightlocation == "Yes", 1,
                                ifelse(nightstatus == "Turned on" &
                                         nightlocation == "No", 0, NA)),
         nightoff_location = factor(nightoff_location, levels = c(0, 1), labels = c("No", "Yes")))
summary(gipw48_sp$nightoff_location)
prop.table(table(gipw48_sp$nightoff_location))

# save data
save(gipw48_sp, file="Behavioral barriers_Sample1.RData")
