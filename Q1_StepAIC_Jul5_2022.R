library(mice)
library(dplyr)
library(ISLR)
library(MASS)
library(car)
library(lme4)
library(ggstatsplot)
 
dat <- read.csv("project_data.csv")
#
# more conveniently using mice
# stochastic regression -> improvement from the above method "norm.nob" -> added some error +noise
dat2 <- dat[, c("Epworth.Sleepiness.Scale", "Athens.Insomnia.Scale","Berlin.Sleepiness.Scale", "Age",
                'BMI', 'Time.from.transplant', 'Gender', 'Rejection.graft.dysfunction',
                  'Any.fibrosis', 'Corticoid', 'Liver.Diagnosis', 'Recurrence.of.disease', 'Renal.Failure', 
                  'Depression')]
imp <- mice(dat2, method = "norm.nob", seed = 11, m = 1, print = FALSE)
xyplot(imp, Epworth.Sleepiness.Scale ~ Age + BMI + Time.from.transplant + Gender + Rejection.graft.dysfunction +
         Any.fibrosis + Corticoid + Liver.Diagnosis + Recurrence.of.disease + Renal.Failure+
         Depression)
#xyplot(imp, SF36.PCS ~ Epworth.Sleepiness.Scale + Athens.Insomnia.Scale + Berlin.Sleepiness.Scale) # Imputed values for ozone are not always the same, it depends on solar.

#fit.stoch <- with(imp, lm(Epworth.Sleepiness.Scale + Athens.Insomnia.Scale + Berlin.Sleepiness.Scale))
#summary(pool(fit.stoch))

# this is how we can extract the actual imputed dataset
imputed.data.frame <- complete(imp) #


# Making variables binary
dat3 <- imputed.data.frame %>%
  mutate(ESSBinary = ifelse(Epworth.Sleepiness.Scale > 10, 1, 0)) %>%
  mutate(AISBinary = ifelse(Athens.Insomnia.Scale > 5, 1, 0)) %>% 
  mutate(Berlin.Sleepiness.Scale = ifelse(Berlin.Sleepiness.Scale > 0.5, 1, 0))

# Converting 1-2 encoding as 0, 1 coding for improved interpretation 
dat3 <- dat3 %>% 
  mutate(Gender = ifelse(Gender >= 2, 1, 0))

#Converting categorical data into factors 
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

mymodel_essbinary_all <- glm(ESSBinary ~ 
                               Age + BMI + Time.from.transplant + Gender + Rejection.graft.dysfunction +
                               Any.fibrosis + Corticoid + Liver.Diagnosis + Recurrence.of.disease + Renal.Failure+
                               Depression, data = dat3, family = binomial) 
AIC(mymodel_essbinary_all)
vif(mymodel_essbinary_all)
ess_final <- stepAIC(mymodel_essbinary_all,trace = F)
summary(ess_final)
ggcoefstats(ess_final)

#Odds Ratio and CI for model 
exp(ess_final$coefficients)
round(exp(confint(ess_final)),2)


##AIS 
mymodel_aisbinary_all <- glm(AISBinary ~ 
                               Age + BMI + Time.from.transplant + Gender + Rejection.graft.dysfunction +
                               Any.fibrosis + Corticoid + Liver.Diagnosis + Recurrence.of.disease + Renal.Failure+
                               Depression, data = dat3, family = binomial) 

vif(mymodel_aisbinary_all)
AIC(mymodel_aisbinary_all)
ais_final <- stepAIC(mymodel_aisbinary_all,trace = F)
summary(ais_final)
ggcoefstats(ais_final)

#Odds Ratio and CI for model 
exp(ais_final$coefficients)
round(exp(confint(ais_final)),2)


##BERLIN 
mymodel_berlinbinary_all <- glm(Berlin.Sleepiness.Scale ~ 
                               Age + BMI + Time.from.transplant + Gender + Rejection.graft.dysfunction +
                               Any.fibrosis + Corticoid + Liver.Diagnosis + Recurrence.of.disease + Renal.Failure+
                               Depression, data = dat3, family = binomial) 

vif(mymodel_berlinbinary_all)
berlin_final <- stepAIC(mymodel_berlinbinary_all,trace = F)
summary(berlin_final)

#Prevalence for ESS 
dat3$ESSBinary %>% table() %>% prop.table()

#Prevalence for Berlin Sleepiness Scale 
dat3$Berlin.Sleepiness.Scale %>% table() %>% prop.table()
attach(dat3)

#Prevalence for AISBinary
dat3$AISBinary %>% table() %>% prop.table()



