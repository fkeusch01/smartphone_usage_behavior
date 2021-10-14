# -----------------------------------------------------------------
# Analysis code for:
# Keusch, F., Wenz, A., & Conrad, F. (in press). Do you have your smartphone with you?
# Behavioral barriers for measuring everyday activities with smartphone sensors.
# Computers in Human Behavior. https://doi.org/10.1016/j.chb.2021.107054
# Plots
# -----------------------------------------------------------------
# Set working directory
setwd("U:\\Projekte\\GIP\\W48")

# -----------------------------------------------------------------
# Load data set
load("Behavioral barriers_Sample1.RData")

# -----------------------------------------------------------------
# Table 2. Descriptive statistics.
# Gender
summary(gipw48_sp$gender)
prop.table(table(gipw48_sp$gender))

# Age
summary(gipw48_sp$agegroup1)
prop.table(table(gipw48_sp$agegroup1))

# Educational attainment
summary(gipw48_sp$edu)
prop.table(table(gipw48_sp$edu))

# OS
summary(gipw48_sp$os)
prop.table(table(gipw48_sp$os))

# Frequency of smartphone use
summary(gipw48_sp$frequse)
prop.table(table(gipw48_sp$frequse))

# Smartphone skills
summary(gipw48_sp$skills)
prop.table(table(gipw48_sp$skills))

# Smartphone dependency
summary(gipw48_sp$phonedependent)
prop.table(table(gipw48_sp$phonedependent))

# -----------------------------------------------------------------
# Table 3. Prevalence of behavioral barriers to measuring mobility, physical activity, and sleep using smartphone sensors.
# Smartphone shared with another person
summary(gipw48_sp$sharing)
prop.table(table(gipw48_sp$sharing))

# Smartphone not always on
summary(gipw48_sp$turnedoff)
prop.table(table(gipw48_sp$turnedoff))

# Smartphone left at home
summary(gipw48_sp$outhomeuse)
prop.table(table(gipw48_sp$outhomeuse))

# Smartphone carried in purse/backpack/bag when outside of home
summary(gipw48_sp$outhomelocation)
prop.table(table(gipw48_sp$outhomelocation))

# Smartphone left stationary when at home and not asleep
summary(gipw48_sp$athomelocation)
prop.table(table(gipw48_sp$athomelocation))

# Smartphone turned off or in other room at night
summary(gipw48_sp$nightoff_location)
prop.table(table(gipw48_sp$nightoff_location))

# -----------------------------------------------------------------
# Table A.3. Descriptive statistics of measures before recoding.
# Frequency of smartphone use
summary(gipw48_sp$AM48001)
prop.table(table(gipw48_sp$AM48001))

# Smartphone skills
summary(gipw48_sp$AM48003)
prop.table(table(gipw48_sp$AM48003))

# Device ownership - PC
summary(gipw48_sp$AJ48031)
prop.table(table(gipw48_sp$AJ48031))

# Device ownership - Tablet
summary(gipw48_sp$AJ48032)
prop.table(table(gipw48_sp$AJ48032))

# Smartphone out-of-home-use
summary(gipw48_sp$AJ48104)
prop.table(table(gipw48_sp$AJ48104))

# Smartphone location out-of-home
summary(gipw48_sp$AJ48105)
prop.table(table(gipw48_sp$AJ48105))

# Smartphone status at night
summary(gipw48_sp$AJ48107)
prop.table(table(gipw48_sp$AJ48107))

# Smartphone location at night
summary(gipw48_sp$AJ48108)
prop.table(table(gipw48_sp$AJ48108))

# Smartphone location at home
summary(gipw48_sp$AJ48109)
prop.table(table(gipw48_sp$AJ48109))

# No. hours smartphone turned on typical day
summary(gipw48_sp$AJ48103)
sd(gipw48_sp$AJ48103, na.rm = T)
hist(gipw48_sp$AJ48103)