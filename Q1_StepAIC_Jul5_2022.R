#################
#Data Loading and Transformation
#################

#Loading needed libraries
library(mice)  
library(dplyr)
library(tidyverse)
library(ISLR)
library(MASS)
library(car) 
library(lme4)
library(ggstatsplot)
library(broom)

#reading in project-file and assigning as dat project
dat <- read.csv("project_data.csv")

#selecting relevant columns with sleep-questionnaires and predictor-variables from dat-object and assigning to dat2  
dat2 <- dat[, c("Epworth.Sleepiness.Scale", "Athens.Insomnia.Scale","Berlin.Sleepiness.Scale", "Age",
                'BMI', 'Time.from.transplant', 'Gender', 'Rejection.graft.dysfunction',
                  'Any.fibrosis', 'Corticoid', 'Liver.Diagnosis', 'Recurrence.of.disease', 'Renal.Failure', 
                  'Depression')]

#missing value imputation performed through stochastic regression on dat2 dataset, assigned to imp object
imp <- mice(dat2, method = "norm.nob", seed = 11, m = 1, print = FALSE)

#imputation plot compared across 3 sleep-scales, to visually inspect placement of missing values 
xyplot(imp, Epworth.Sleepiness.Scale ~ Age + BMI + Time.from.transplant + Gender + Rejection.graft.dysfunction +
         Any.fibrosis + Corticoid + Liver.Diagnosis + Recurrence.of.disease + Renal.Failure+
         Depression)
xyplot(imp, Athens.Insomnia.Scale ~ Age + BMI + Time.from.transplant + Gender + Rejection.graft.dysfunction +
         Any.fibrosis + Corticoid + Liver.Diagnosis + Recurrence.of.disease + Renal.Failure+
         Depression)
xyplot(imp, Berlin.Sleepiness.Scale ~ Age + BMI + Time.from.transplant + Gender + Rejection.graft.dysfunction +
         Any.fibrosis + Corticoid + Liver.Diagnosis + Recurrence.of.disease + Renal.Failure+
         Depression)

#Imputation function used to impute NA's into dat-2 object, assigned to imputed.data.frame object
imputed.data.frame <- complete(imp) 

#Sleep-response questionnaires for ESS, AIS and BSS formatted into  binary 
#ESS Questionnaire named as ESSBinary
#BSS Questionnaire named as Berlin.Sleepiness.Scale 
#AIS Questionnaire named as AISBinary
dat3 <- imputed.data.frame %>%
  mutate(ESSBinary = ifelse(Epworth.Sleepiness.Scale > 10, 1, 0)) %>%
  mutate(AISBinary = ifelse(Athens.Insomnia.Scale > 5, 1, 0)) %>% 
  mutate(Berlin.Sleepiness.Scale = ifelse(Berlin.Sleepiness.Scale > 0.5, 1, 0))

# Categorical factor converted from 1-2 encoding into  0, 1 coding for interpretation 
dat3 <- dat3 %>% 
  mutate(Gender = ifelse(Gender >= 2, 1, 0))

#Other categorical variables converted into factors 
dat3$Liver.Diagnosis <- as.factor(dat3$Liver.Diagnosis)
dat3$Recurrence.of.disease <- as.factor(dat3$Recurrence.of.disease)
dat3$Rejection.graft.dysfunction <- as.factor(dat3$Rejection.graft.dysfunction)
dat3$Any.fibrosis <- as.factor(dat3$Any.fibrosis)
dat3$Renal.Failure <- as.factor(dat3$Renal.Failure)
dat3$Depression <- as.factor(dat3$Depression)
dat3$Corticoid <- as.factor(dat3$Corticoid)
dat3$Gender <- as.factor(dat3$Gender)
dat3$ESSBinary <- as.factor(dat3$ESSBinary)
dat3$AISBinary <- as.factor(dat3$AISBinary)
dat3$Berlin.Sleepiness.Scale <- as.factor(dat3$Berlin.Sleepiness.Scale)

#remove these columns after:
dat3$Epworth.Sleepiness.Scale <- as.factor(dat3$Epworth.Sleepiness.Scale)
dat3$Athens.Insomnia.Scale <- as.factor(dat3$Athens.Insomnia.Scale)

#################
#Determination of Prevalence 
#################

#Prevalence for ESS Questionnaire 
dat3$ESSBinary %>% table() %>% prop.table()

#Prevalence for BSS Questionnaire
dat3$Berlin.Sleepiness.Scale %>% table() %>% prop.table()

#Prevalence for AIS Questionnaire 
dat3$AISBinary %>% table() %>% prop.table()

######################################################
#ESS Questionnaire - Determination of Sleep Predictors 
#######################################################

#ESS Questionnaire used for logistic regression model  
mymodel_essbinary_all <- glm(ESSBinary ~ 
                               Age + BMI + Time.from.transplant + Gender + Rejection.graft.dysfunction +
                               Any.fibrosis + Corticoid + Liver.Diagnosis + Recurrence.of.disease + Renal.Failure+
                               Depression, data = dat3, family = binomial) 

#Assumptions for linearity
probabilities <- predict(mymodel_essbinary_all, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
mydata <- dat3 %>% dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")


#AIC of full model 
AIC(mymodel_essbinary_all)

#VIF of model to check for collinearity 
vif(mymodel_essbinary_all)

#STEPAIC function applied to the model 
ess_final <- stepAIC(mymodel_essbinary_all,trace = F)

#Summary of ESS model 
summary(ess_final)

#Changing names for improved interpretation

#Plot of ESS model 
ggcoefstats(ess_final, xlab="Coefficient Estimate", ylab = "Variable", main = "Variable Coefficients for ESS")

#Odds Ratio and CI for model being retrieved 
exp(ess_final$coefficients)
round(exp(confint(ess_final)),2)

##################################
#AIS Questionnaire - Sleep Predictors 
##################################

#AIS questionnaire used for logistic regression model  
mymodel_aisbinary_all <- glm(AISBinary ~ 
                               Age + BMI + Time.from.transplant + Gender + Rejection.graft.dysfunction +
                               Any.fibrosis + Corticoid + Liver.Diagnosis + Recurrence.of.disease + Renal.Failure+
                               Depression, data = dat3, family = binomial) 

#Assumptions for linearity
probabilities <- predict(mymodel_aisbinary_all, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
mydata <- dat3 %>% dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")


#VIF being conducted for AIS model
vif(mymodel_aisbinary_all)

#AIC being conducted for AIS model
AIC(mymodel_aisbinary_all)

#AIS model being used with the stepAIC model
ais_final <- stepAIC(mymodel_aisbinary_all,trace = F)

#AIS model being used with the stepAIC model
summary(ais_final)

#Odds Ratio and CI for model 
exp(ais_final$coefficients)
round(exp(confint(ais_final)),2)

#AIS model box-and-whiskers plot
ggcoefstats(ais_final, xlab="Coefficient Estimate", 
            ylab = "Variable", 
            main = "Variable Coefficients for ESS")

##################################
#BERLIN Questionnaire - Sleep Predictors 
##################################

#Berlin Sleepiness Scale used for logistic regression model  
mymodel_berlinbinary_all <- glm(Berlin.Sleepiness.Scale ~ 
                               Age + BMI + Time.from.transplant + Gender + Rejection.graft.dysfunction +
                               Any.fibrosis + Corticoid + Liver.Diagnosis + Recurrence.of.disease + Renal.Failure+
                               Depression, data = dat3, family = binomial) 

#Assumptions for linearity
probabilities <- predict(mymodel_berlinbinary_all, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
mydata <- dat3 %>% dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")


#AIC being conducted for Berlin model
AIC(mymodel_berlinbinary_all)

#VIF conducted for AIS model
vif(mymodel_berlinbinary_all)

#STEPAIC function used for developing logistic model with least AIC 
berlin_final <- stepAIC(mymodel_berlinbinary_all,trace = F)

#summary function for lowest-AIC model
summary(berlin_final)

#box and whiskers plot for final logistic mdoel
ggcoefstats(berlin_final,  xlab="Coefficient Estimate", ylab = "Variable")

#Odds Ratio and CI for model 
exp(berlin_final$coefficients)
round(exp(confint(berlin_final)),2)

