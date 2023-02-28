#Assignment 5
#Author: Humayun Kabir
Ibuprofen_data <- read_excel("C:/Users/humay/Dropbox/HRM/HRM 753/Assignments/Assignment 5/bernard Ibuprofen RCT dataset.xlsx")
options (scipen=999)
save.image("C:/Users/humay/Dropbox/HRM/HRM 753/Assignments/Assignment 5/Assignment 5 data.RData")

## Install tidyverse packages
#  install.packages("pacman")
pacman::p_load(
  tidyverse,    # data management + ggplot2 graphics
  ggplot2,      # ggplot2 graphics
  skimr,        # get overview of data
  tidymodels,   # for tidy modelling
  survey,       # for survey functions
  srvyr,        # for tidy survey work
  lubridate,    # for converting date character to date format
  tidyquant,    # for tidy time series functions
  patchwork,    # easily combine ggplot rasters
  plyr,         # for seeing the frequency
  freqtables,   # for frequency table
  glue,
  ggpubr,
  caret,        # for easy machine learning workflow
  gmodels       # for cross tab percentage
)

###
rm(Ibuprofen_data.temp24.cat.ifelse)
rm (x)
###

Ibuprofen_data$temp24
typeof (Ibuprofen_data$temp24)
Ibuprofen_data$temp24.int = as.integer(Ibuprofen_data$temp24)

Ibuprofen_data$temp24.cat= cut(Ibuprofen_data$temp24.int, breaks = c(0, 98.60, 99.40, 100.8, Inf),) 
Ibuprofen_data$temp24.cat
levels (Ibuprofen_data$temp24.cat)

Ibuprofen_data %>% 
  freq_table(temp24.cat)

###
sum (is.na(Ibuprofen_data$temp24))

Ibuprofen_data$temp24

Ibuprofen_data$temp24.cat.ifelse= as.factor(ifelse(Ibuprofen_data$temp24 <98.7, "normal", 
                             ifelse(Ibuprofen_data$temp24 <99.5 & Ibuprofen_data$temp24>98.6,"slightly elevated",
                                    ifelse(Ibuprofen_data$temp24 <100.9 & Ibuprofen_data$temp24>99.4, "fever", 
                                   ifelse(Ibuprofen_data$temp24>100.8, "high fever", NA
                                           )))))

Ibuprofen_data$temp24.cat.ifelse.1= as.factor(ifelse(Ibuprofen_data$temp24 > 100.8, "high fever", 
                                                   ifelse(Ibuprofen_data$temp24 > 99.4,"fever",
                                                          ifelse(Ibuprofen_data$temp24 > 98.6, "slightly elevated", 
                                                                 ifelse(Ibuprofen_data$temp24 <= 98.6, "normal", NA
                                                                 )))))

levels (Ibuprofen_data$temp24.cat.ifelse)
sum (is.na(Ibuprofen_data$temp24.cat.ifelse))

Ibuprofen_data %>% 
  freq_table(temp24.cat.ifelse)

Ibuprofen_data %>% 
  freq_table(temp24.cat.ifelse.1)

####
Ibuprofen_data$temp24.cat.binary= ifelse (Ibuprofen_data$temp24.cat.ifelse.1=="normal"|Ibuprofen_data$temp24.cat.ifelse.1=="slightly elevated",1,0)
Ibuprofen_data$temp24.cat.binary

###

#Q1
#re-level in the model
q1.glm.model <- glm(temp24.cat.binary ~ relevel(treat, ref = "Placebo"),
                       data = Ibuprofen_data,
                       family = "binomial")
#
Ibuprofen_data$treat = as.factor (Ibuprofen_data$treat)
Ibuprofen_data$treat <- relevel(Ibuprofen_data$treat, ref = "Placebo") 

Ibuprofen_data %>% 
  freq_table(temp24.cat.binary)

Ibuprofen_data %>% 
  freq_table(treat)

q1.glm.model= glm (Ibuprofen_data$temp24.cat.binary~Ibuprofen_data$treat, family = "binomial")

q1.glm.model
summary (q1.glm.model)

or.q1.glm.model= exp (cbind (coef (q1.glm.model), confint(q1.glm.model, level=0.95)))
or.q1.glm.model

#Q2
Ibuprofen_data_na.omit=Ibuprofen_data
Ibuprofen_data_na.omit <- na.omit(Ibuprofen_data_na.omit)

Ibuprofen_data %>% 
  freq_table(temp24.cat.binary)
Ibuprofen_data %>% 
  freq_table(treat)
Ibuprofen_data %>% 
  freq_table(race)
Ibuprofen_data %>% 
  freq_table(fate)
summary (Ibuprofen_data$apache) 
summary (Ibuprofen_data$o2del) 


Ibuprofen_data$temp24.cat.binary= 
  ifelse (Ibuprofen_data_na.omit$temp24.cat.ifelse.1=="normal"|Ibuprofen_data_na.omit$temp24.cat.ifelse.1=="slightly elevated",1,0)

Ibuprofen_data$temp24.cat.binary


Ibuprofen_data_na.omit

Ibuprofen_data_na.omit$temp24.cat.binary

sum (is.na (Ibuprofen_data$o2del))

#check for interaction 
levels (Ibuprofen_data$treat)

Ibuprofen_data$treat.recode= as.integer(recode (Ibuprofen_data$treat,
                                     "Placebo" = "0",
                                     "Ibuprofen" = "1"))
levels (Ibuprofen_data$race)

Ibuprofen_data$race= as.factor (Ibuprofen_data$race)

Ibuprofen_data$race.recode= as.integer( recode (Ibuprofen_data$race,
                                     "Black" = "2",
                                     "White" = "1",
                                     "Other"= "0"))

Ibuprofen_data$race.recode

Ibuprofen_data$treat.race.int= Ibuprofen_data$race.recode*Ibuprofen_data$treat.recode

Ibuprofen_data$treat.apache.int= Ibuprofen_data$apache*Ibuprofen_data$treat.recode

Ibuprofen_data$treat.o2del.int= Ibuprofen_data$o2del*Ibuprofen_data$treat.recode


#Test interaction for treat.race
q2.glm.model.treat.race.int= glm (temp24.cat.binary~treat+race+treat.race.int,data=Ibuprofen_data, family = "binomial")
q2.glm.model.treat.race.int
summary (q2.glm.model.treat.race.int)

q2.glm.model.treat.apache.int= glm (temp24.cat.binary~treat+treat.apache.int,data=Ibuprofen_data, family = "binomial")
q2.glm.model.treat.apache.int
summary (q2.glm.model.treat.apache.int)


q2.glm.model.treat.o2del.int= glm (temp24.cat.binary~treat+treat.o2del.int,data=Ibuprofen_data, family = "binomial")
q2.glm.model.treat.o2del.int
summary (q2.glm.model.treat.o2del.int)

q2.glm.model
summary (q2.glm.model)

or.q2.glm.model= exp (cbind (coef (q2.glm.model), confint(q2.glm.model, level=0.95)))

or.q2.glm.model

#Test confounding 
q2.glm.model.treat= glm (temp24.cat.binary~treat.recode,data=Ibuprofen_data, family = "binomial")
summary (q2.glm.model.treat)

q2.glm.model.treat.race.cof= glm (temp24.cat.binary~treat.recode+race.recode,data=Ibuprofen_data, family = "binomial")
summary (q2.glm.model.treat.race.cof)

#The change of beta is 0.00365 <10%, So, we are not taking this varible in the model 

q2.glm.model.treat= glm (temp24.cat.binary~treat.recode,data=Ibuprofen_data, family = "binomial")
summary (q2.glm.model.treat)

q2.glm.model.treat.apache.cof= glm (temp24.cat.binary~treat.recode+apache,data=Ibuprofen_data, family = "binomial")
summary (q2.glm.model.treat.apache.cof)

#The change of beta is 0.015719 <10%, So, we are not taking this varible in the model 

q2.glm.model.treat= glm (temp24.cat.binary~treat.recode,data=Ibuprofen_data, family = "binomial")
summary (q2.glm.model.treat)

q2.glm.model.treat.o2del.cof= glm (temp24.cat.binary~treat.recode+o2del,data=Ibuprofen_data, family = "binomial")
summary (q2.glm.model.treat.o2del.cof)

#The change of beta is 0.120451 >10%, So, we will taking this varible in the model 

#Final Model 
typeof (Ibuprofen_data$o2del)
Ibuprofen_data$o2del.100=Ibuprofen_data$o2del*1000

q2.glm.model= glm (temp24.cat.binary~treat.recode
                   +o2del.100, 
                   data=Ibuprofen_data, 
                   family = "binomial")
summary (q2.glm.model)

or.q2.glm.model= 
  exp (cbind (coef (q2.glm.model), 
              confint(q2.glm.model, 
                      level=0.95)))
or.q2.glm.model


#ROC curb
library(pROC)

Ibuprofen_data_na.omit = na.omit (Ibuprofen_data)



q2.glm.model= glm (temp24.cat.binary~treat.recode
                   +o2del.100, 
                   data=Ibuprofen_data_na.omit, 
                   family = "binomial")
summary (q2.glm.model)

#Probability
q2.prob=predict(q2.glm.model,type=c("response"))
#Prediction 
q2.pred <- prediction(  q2.prob, Ibuprofen_data_na.omit$temp24.cat.binary) 

#roc object
roc_object <- roc( Ibuprofen_data_na.omit$temp24.cat.binary,  q2.prob)

auc( roc_object )

#roc plot 
plot (roc_object, col=rainbow(7), main="ROC curve for temperature at hours 24", print.auc=TRUE)
 

  
#Extra 
perf <- performance(q2.pred, measure = "tpr", x.measure = "fpr")    

plot(perf, col=rainbow(7), main="ROC curve for temperature at hours 24", xlab="Specificity", 
     ylab="Sensitivity")  
abline(0, 1)







#Q3
#Q3
#Q3
#Q3





#Q3
require(Hmisc)
require(MASS)

#by plolr
q3.model= polr(temp24.cat.ifelse.1~treat, data = Ibuprofen_data_na.omit, Hess = TRUE)
q3.model
summary(q3.model)

ctable <- coef(summary(q3.model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
ci <- confint(q3.model, level=0.95)
ci


exp (1.7463518)

#
#install if not already installed and load package 
if (!require('ordinal')) install.packages('ordinal'); require('ordinal')
if (!require('carData')) install.packages('carData'); require('carData')

#by clm
Ibuprofen_data$temp24.cat.ifelse.1 <- relevel(Ibuprofen_data$temp24.cat.ifelse.1.cat, ref = "0"  ) 
levels (Ibuprofen_data$temp24.cat.ifelse.1)


Ibuprofen_data$temp24.cat.ifelse.1.cat= recode (Ibuprofen_data$temp24.cat.ifelse.1,
                                                "normal" = "0",
                                                "slightly elevated" = "1",
                                                "fever" = "2",
                                                "high fever"  = "3")

#run the analysis 
q3.model.1=clm(temp24.cat.ifelse.1.cat~treat, data = Ibuprofen_data)
summary(q3.model.1)

or.q3.model.1= exp (cbind (coef (q3.model.1), confint(q3.model.1, level=0.95)))
or.q3.model.1

nominal_test(q3.model.1)
exp (1.1827)

normal vs. slightly elevated *
slightly elevated vs. fever
fever vs. high fever 


#Test interaction for treat.race
q3.model.1=clm(temp24.cat.ifelse.1~treat, data = Ibuprofen_data)
q3.model.treat.race.int= clm(temp24.cat.ifelse.1~treat+treat.race.int, data = Ibuprofen_data)
summary (q3.model.treat.race.int)

q3.model.1=clm(temp24.cat.ifelse.1~treat, data = Ibuprofen_data)
q3.model.treat.apache.int= clm(temp24.cat.ifelse.1~treat+treat.apache.int, data = Ibuprofen_data)
summary (q3.model.treat.apache.int)


q3.model.1=clm(temp24.cat.ifelse.1~treat, data = Ibuprofen_data)
q3.model.treat.o2del.int= clm(temp24.cat.ifelse.1~treat+treat.o2del.int, data = Ibuprofen_data)
summary (q3.model.treat.o2del.int)

#No interaction was found in the model. 

#Test confounding 
q3.model.1=clm(temp24.cat.ifelse.1~treat.recode, data = Ibuprofen_data)
summary (q3.model.1)

q3.model.treat.race=clm(temp24.cat.ifelse.1~treat.recode+race.recode, data = Ibuprofen_data)
summary (q3.model.treat.race)
#The change of beta is 0.00147 <10%, So, we are not taking this variable in the model

q3.model.1=clm(temp24.cat.ifelse.1~treat.recode, data = Ibuprofen_data)
summary (q3.model.1)

q3.model.treat.apache=clm(temp24.cat.ifelse.1~treat.recode+apache, data = Ibuprofen_data)
summary (q3.model.treat.apache)
#The change of beta is 0.007 <10%, So, we are not taking this variable in the model

q3.model.1=clm(temp24.cat.ifelse.1~treat.recode, data = Ibuprofen_data)
summary (q3.model.1)

q3.model.treat.race=clm(temp24.cat.ifelse.1~treat.recode+race.recode, data = Ibuprofen_data)
summary (q3.model.treat.race)
#The change of beta is .00147 <10%, So, we are not taking this variable in the model


