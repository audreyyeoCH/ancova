library(readxl)
library(foreign)
library(readxl)
library(readstata13)
library(car) # for levene's test
library(compute.es)
library(effects)# for adjusted means
library(ggplot2) # for graphs
library(multcomp) #for post hoc test
library(pastecs)# for descriptive statistics
library(WRS) # for robust test

############## PloS and Scientific advances #################
#18
db_18 <- read_excel("07082019 plos one and scientific advances/18.xls")
str(db_18)
View(db_18)
leveneTest( db_18$`Body size`, as.factor(db_18$Temperature), center = mean) 
# can we assume homoscedatic variance?

# as per their Table 5
#1 Body Size
db_18aov1 <- aov(`Body size` ~ as.factor(Temperature) + as.factor(Ph), data = db_18)
summary(db_18aov1)
plot(db_18aov1)

leveneTest( db_18$`Body size`, as.factor(db_18$Ph), center = mean) 

tt <- lm(`Body size` ~Temperature + Ph, data= db_18)
summary(tt)
class(db_18$Temperature)

db_18aov1b <- aov(`Body size` ~ Ph, data = db_18)
summary(db_18aov1b)
plot(db_18aov1b)



#2 Egg production
db_18aov2 <- aov(EP ~ Temperature, data = db_18)
leveneTest( db_18$EP, as.factor(db_18$Temperature), center = mean) 
summary(db_18aov2)
plot(db_18aov2)

db_18aov2b <- aov(EP ~ Oxygen, data = db_18)
leveneTest( db_18$EP, as.factor(db_18$Oxygen), center = mean) 
summary(db_18aov2b)
plot(db_18aov2b)

db_18aov2c <- aov(EP ~ Ph, data = db_18)
leveneTest( db_18$EP, as.factor(db_18$Ph), center = mean) 
summary(db_18aov2c)
plot(db_18_aov2c)

#3 Egg Size
db_18aov3 <- aov(`Egg size` ~ Ph, data = db_18)
summary(db_18aov3)
plot(db_18_aov3)


db_18aov3b <- aov(`Egg size` ~ Temperature, data = db_18)
summary(db_18aov3b)
plot(db_18aov3b)

#4 Growth
db_18aov4 <- aov(Growth ~ Ph, data = db_18)
summary(db_18aov4)
plot(db_18aov4)
db_18aov4b <- aov(Growth ~ Temperature, data = db_18)
summary(db_18aov4b)
plot(db_18aov4b)

#51 the text files available here are many

#53 Y is not defined
db_53 <- read_excel("07082019 plos one and scientific advances/53.xlsx")
str(db_53)
View(db_53)
# Y is HRQL, which ones in particular
# X are sex, age and recruitment centre
# assessed on four levels of MedDiet adherence
var <- db_53[1, 1:28]
var
for (i in 1:28) { 
  aov_mod[i] <- aov(var(i) ~ MedDiet_17item, data = db_53)
  return()}
 


db_53aov1 <- aov(BMI ~ MedDiet_17item, data = db_53)
summary(db_53aov1)


db_53_one <- subset(db_53, MedDiet_17item, data = db_53)
# lm_db_53_one <- 
summary(db_53$center)
str(db_53$center) # there are 6'874 centers which was controlled for in this study



# 62 ¥ is not defined

# 80
db_80 <- read_excel("07082019 plos one and scientific advances/80.xlsx")
str(db_80)
View(db_80)



# 80 ... Y = RCS_Tot X = Age, 

db_80_aov <- aov(RCS_Tot ~ Nos_Cat + Age, data = db_80)
db_80_aov <- aov(RCS_Tot ~ as.factor(Nos_Cat), data = db_80) # this didn't work
str(db_80)

class(db_80$Nos_Cat)
lm_80 <- lm(db_80$RCS_Tot ~ Age + as.factor(Nos_Cat==1), data = db_80)
summary(lm_80)
summary(db_80$Nos_Cat)


# 82 data read fail
db_82 <- file.choose("07082019 plos one and scientific advances/82.sav")
dataset_82 <- read.spss(db_82, to.data.frame = TRUE)
View(dataset_82)

# db176_hit <- subset(db176, `Group (1 - HIT, 2 - SIT)`== 1)
# db176_sit <- subset(db176, `Group (1 - HIT, 2 - SIT)`== 2)

# 86

# 98

# 108
db_108 <- read_excel("07082019 plos one and scientific advances/108.xlsx")
str(db_108)
View(db_108)


# 115

# 122

# 124

# 141 #no covariance 
db_141 <- read_excel("07082019 plos one and scientific advances/141.xlsx")
str(db_141)
View(db_141)

# 147

# 151

# 159

# 160

#169
db_169 <- file.choose("07082019 plos one and scientific advances/169.SAV")
dataset_169 <- read.spss(db_169, to.data.frame = TRUE)
View(dataset_169)
 
# 174
db_174 <- read.dta13("07082019 plos one and scientific advances/174.dta")
str(db_174) # 60 variables


# 192

# 203

# 206

#211 idk where this is from
db_211 <- read_excel("07082019 plos one and scientific advances/211.xlsx")
str(db_211)
View(db_211)

# lm_211 <- lm(~) # could not do this aov because family number as covariate not defined
















########### misc ###########
biochem_gen_molec <- seq(1:329)
agri_bio <- seq(1:321)
multi <- seq(1:54)
phy_astro <- seq(1:8)

class(phy_astro)
set.seed(100)
sample(biochem_gen_molec, 1, replace = TRUE)
sample(agri_bio, 1, replace = TRUE)
sample(multi, 1, replace = TRUE)
sample(phy_astro, 1, replace = TRUE)


set.seed(100)
the317_r <- sample(1:317, 32, replace = FALSE)
the317_r
sort(the317_r)

# [1]  18  51  53  62  80  82  86  98 108 115 122 124 141 147
# [15] 151 159 160 169 174 192 203 206 211 221 225 232 253 256
# [29] 266 270 295 300

the317_t72 <- sample(1:317, 72, replace = FALSE)
the317_t72
sort(the317_t72) # different from the 317_t72

# we now have 763 search results
# sampling 200 with same seed(100)

the763_r <- sample(1:763, 100, replace = FALSE)
sort(the763_r)

the763_r200 <- sample(1:763, 200, replace = FALSE)
sort(the763_r200)

as.list(the763_r200)
View(sort(the763_r200))
