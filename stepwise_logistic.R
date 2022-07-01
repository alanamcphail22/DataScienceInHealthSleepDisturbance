
### Data cleaning
library(dplyr)
dat <- read.csv("project_data.csv")


# Making variables binary
dat2 <- dat %>%
  mutate(ESSBinary = ifelse(Epworth.Sleepiness.Scale > 10, 1, 0)) %>%
  mutate(PSGQBinary = ifelse(Pittsburgh.Sleep.Quality.Index.Score > 5, 1, 0)) %>%
  mutate(AISBinary = ifelse(Athens.Insomnia.Scale > 5, 1, 0))

# Removing all NAs in those columns
dat3 <- dat2 %>%
  filter(
    !is.na(ESSBinary),
    !is.na(PSGQBinary),
    !is.na(AISBinary),
    !is.na(Berlin.Sleepiness.Scale)
  )

dat3 <- dat3 %>%
  filter(
    !is.na(Age),
    !is.na(Gender),
    !is.na(BMI),
    !is.na(Time.from.transplant),
    !is.na(Liver.Diagnosis),!is.na(Recurrence.of.disease),
    !is.na(Rejection.graft.dysfunction),
    !is.na(Any.fibrosis),!is.na(Renal.Failure),
    !is.na(Depression),
    !is.na(Corticoid)
  )


#data into factors 
dat3$Gender <- as.factor(dat3$Gender)
dat3$Liver.Diagnosis <- as.factor(dat3$Liver.Diagnosis)
dat3$Recurrence.of.disease <- as.factor(dat3$Recurrence.of.disease)
dat3$Rejection.graft.dysfunction <- as.factor(dat3$Rejection.graft.dysfunction)
dat3$Any.fibrosis <- as.factor(dat3$Any.fibrosis)
dat3$Renal.Failure <- as.factor(dat3$Renal.Failure)
dat3$Depression <- as.factor(dat3$Depression)
dat3$Corticoid <- as.factor(dat3$Corticoid)

####ESS SLEEP SCALE####
#stepwise approach - take out predictor corresponding to largest p-value, refit model, and then repeat process
library(ggplot2)
mymodel_essbinary_all <- glm(ESSBinary ~ 
                               Age + BMI + Time.from.transplant+ Gender + Rejection.graft.dysfunction +
                               Any.fibrosis + Corticoid + Liver.Diagnosis + Recurrence.of.disease + Renal.Failure+
                               Depression, data = dat3, family = binomial) 
summary(mymodel_essbinary_all)
#without renal failure1
mymodel_essbinary_1 <- glm(ESSBinary ~ 
                               Age + BMI + Time.from.transplant+ Gender + Rejection.graft.dysfunction +
                               Any.fibrosis+Corticoid + Liver.Diagnosis + Recurrence.of.disease+
                               Depression, data = dat3, family = binomial) 
summary(mymodel_essbinary_1)

#without depression
mymodel_essbinary_2 <- glm(ESSBinary ~ 
                             Age + BMI + Time.from.transplant+ Gender + Rejection.graft.dysfunction +
                             Any.fibrosis+Corticoid + Liver.Diagnosis + Recurrence.of.disease
                             , data = dat3, family = binomial) 
summary(mymodel_essbinary_2)

#without age
mymodel_essbinary_3 <- glm(ESSBinary ~ 
                             BMI + Time.from.transplant+ Gender + Rejection.graft.dysfunction +
                             Any.fibrosis+Corticoid + Liver.Diagnosis + Recurrence.of.disease
                           , data = dat3, family = binomial) 
summary(mymodel_essbinary_3)


#without fibrosis
mymodel_essbinary_4 <- glm(ESSBinary ~ 
                             BMI + Time.from.transplant+ Gender + Rejection.graft.dysfunction +
                             Corticoid + Liver.Diagnosis + Recurrence.of.disease
                           , data = dat3, family = binomial) 
summary(mymodel_essbinary_4)

#without rejection
mymodel_essbinary_5 <- glm(ESSBinary ~ 
                             BMI + Time.from.transplant+ Gender +
                             Corticoid + Liver.Diagnosis + Recurrence.of.disease
                           , data = dat3, family = binomial) 
summary(mymodel_essbinary_5)

#without recurrence
mymodel_essbinary_6 <- glm(ESSBinary ~ 
                             BMI + Time.from.transplant+ Gender +
                             Corticoid + Liver.Diagnosis
                           , data = dat3, family = binomial) 
summary(mymodel_essbinary_6)

#without time from transplant
mymodel_essbinary_7 <- glm(ESSBinary ~ 
                             BMI + Gender +
                             Corticoid + Liver.Diagnosis
                           , data = dat3, family = binomial) 
summary(mymodel_essbinary_7)

#without gender
mymodel_essbinary_8 <- glm(ESSBinary ~ 
                             BMI  +
                             Corticoid + Liver.Diagnosis
                           , data = dat3, family = binomial) 
summary(mymodel_essbinary_8)

#without liver
mymodel_essbinary_9 <- glm(ESSBinary ~ 
                             BMI  +
                             Corticoid
                           , data = dat3, family = binomial) 
summary(mymodel_essbinary_9)

#without bmi
mymodel_essbinary_10 <- glm(ESSBinary ~ Corticoid, data = dat3, family = binomial) 
summary(mymodel_essbinary_10)

#final tests show that model with corticoid is significant
summary(mymodel_essbinary_all)
summary(mymodel_essbinary_final)
AIC(mymodel_essbinary_all)
AIC(mymodel_essbinary_10)

#plotting for ESS 
## Lastly, let's  see what this logistic regression predicts, given
## that a patient is either female or male (and no other data about them).
install.packages("effects")
library(effects)
plot(allEffects(mymodel_essbinary_10)) #shows those without corticoid use have 
#lower sleep-scale measures vs those with use

#plot this way doesn't work:
#corticoid_vals <- seq(min(dat3$Corticoid), max(dat3$Corticoid), 0.01)
#mynewdata2 <- data.frame(Corticoid=corticoid_vals)
#my_preds <- predict(mymodel_essbinary_10, list(Corticoid=mynewdata2), type="response")
#plot(corticoid_vals,my_preds,type = "l",xlab = "Corticoid",ylab = "Predicted probabilities")










#######################
####BERLIN SLEEP SCALE####
#stepwise approach - take out predictor corresponding to largest p-value, refit model, and then repeat process
mymodel_bssbinary_all <- glm(Berlin.Sleepiness.Scale ~ 
                             Age + BMI + Time.from.transplant+ Gender + Rejection.graft.dysfunction +
                               Any.fibrosis+Corticoid + Liver.Diagnosis + Recurrence.of.disease + Renal.Failure+
                               Depression, data = dat3, family = binomial) 

summary(mymodel_bssbinary_all)


#without renal failure
mymodel_bssbinary_1 <- glm(Berlin.Sleepiness.Scale ~ 
                             Age + BMI + Time.from.transplant+ Gender + Rejection.graft.dysfunction +
                             Any.fibrosis+Corticoid + Liver.Diagnosis + Recurrence.of.disease+
                             Depression, data = dat3, family = binomial) 
summary(mymodel_bssbinary_1)

#without age
mymodel_bssbinary_2 <- glm(Berlin.Sleepiness.Scale ~ 
                             BMI + Time.from.transplant+ Gender + Rejection.graft.dysfunction +
                             Any.fibrosis+Corticoid + Liver.Diagnosis + Recurrence.of.disease+
                             Depression, data = dat3, family = binomial) 

summary(mymodel_bssbinary_2)


#without depression
mymodel_bssbinary_3 <- glm(Berlin.Sleepiness.Scale ~ BMI + Time.from.transplant+ Gender + Rejection.graft.dysfunction + Any.fibrosis+Corticoid + Liver.Diagnosis + Recurrence.of.disease
                             ,data = dat3, family = binomial) 

summary(mymodel_bssbinary_3)

#without liver diagnosis
mymodel_bssbinary_4 <- glm(Berlin.Sleepiness.Scale ~ BMI + Time.from.transplant+ Gender + Rejection.graft.dysfunction + Any.fibrosis+Corticoid + Recurrence.of.disease
                           ,data = dat3, family = binomial) 

summary(mymodel_bssbinary_4)


#without gender 
mymodel_bssbinary_5 <- glm(Berlin.Sleepiness.Scale ~ BMI + Time.from.transplant + Rejection.graft.dysfunction + Any.fibrosis+Corticoid + Recurrence.of.disease
                           ,data = dat3, family = binomial) 

summary(mymodel_bssbinary_5)

#without time 
mymodel_bssbinary_6 <- glm(Berlin.Sleepiness.Scale ~ BMI + Rejection.graft.dysfunction + Any.fibrosis+Corticoid + Recurrence.of.disease
                           ,data = dat3, family = binomial) 

summary(mymodel_bssbinary_6)


#without fibrosis 
mymodel_bssbinary_7 <- glm(Berlin.Sleepiness.Scale ~ BMI + Rejection.graft.dysfunction +Corticoid + Recurrence.of.disease
                           ,data = dat3, family = binomial) 

summary(mymodel_bssbinary_7)


#without corticoid 
mymodel_bssbinary_8 <- glm(Berlin.Sleepiness.Scale ~ BMI + Rejection.graft.dysfunction + Recurrence.of.disease
                           ,data = dat3, family = binomial) 

summary(mymodel_bssbinary_8)

#without rejection 
mymodel_bssbinary_9 <- glm(Berlin.Sleepiness.Scale ~ BMI + Recurrence.of.disease
                           ,data = dat3, family = binomial) 

summary(mymodel_bssbinary_9)

#without recurrence - final model just has BMI information
mymodel_bssbinary_10 <- glm(Berlin.Sleepiness.Scale ~ BMI,data = dat3, family = binomial) 

summary(mymodel_bssbinary_10)

AIC(mymodel_bssbinary_10)
AIC(mymodel_bssbinary_all)

#plotting for ESS 
## Lastly, let's  see what this logistic regression predicts, given
## that a patient's recurrence of disease is provided, and no other information

bmi_vals <- seq(min(dat3$BMI), max(dat3$BMI), 0.1)
mynewdata_3 <- data.frame(BMI=bmi_vals)
mypreds <- predict(mymodel_bssbinary_10, mynewdata_3, type="response")
plot(bmi_vals,mypreds,type = "l",xlab = "BMI",ylab = "Predicted probabilities")
##graph shows higher BMI associated with greater probability of greater sleep-score i.e. more difficulty











####PSGQ Binary SLEEP####
#stepwise approach - take out predictor corresponding to largest p-value, refit model, and then repeat process
mymodel_PSGQBinary_all <- glm(PSGQBinary ~ 
                               Age + BMI + Time.from.transplant+ Gender + Rejection.graft.dysfunction +
                               Any.fibrosis+Corticoid + Liver.Diagnosis + Recurrence.of.disease + Renal.Failure+
                               Depression, data = dat3, family = binomial) 
summary(mymodel_PSGQBinary_all)


#without BMI
mymodel_PSGQbinary_1 <- glm(PSGQBinary ~ 
                             Age + Time.from.transplant+ Gender + Rejection.graft.dysfunction +
                             Any.fibrosis+Corticoid + Liver.Diagnosis + Recurrence.of.disease + Renal.Failure+
                             Depression, data = dat3, family = binomial) 
summary(mymodel_PSGQbinary_1)

#without liver diagnosis
mymodel_PSGQbinary_2 <- glm(PSGQBinary ~ 
                              Age + Time.from.transplant+ Gender + Rejection.graft.dysfunction +
                              Any.fibrosis+Corticoid + Recurrence.of.disease + Renal.Failure+
                              Depression, data = dat3, family = binomial) 
summary(mymodel_PSGQbinary_2)

#without renal failure 
mymodel_PSGQbinary_3 <- glm(PSGQBinary ~ 
                              Age + Time.from.transplant+ Gender + Rejection.graft.dysfunction +
                              Any.fibrosis+Corticoid + Recurrence.of.disease +
                              Depression, data = dat3, family = binomial) 
summary(mymodel_PSGQbinary_3)

#without time from transplant 
mymodel_PSGQbinary_4 <- glm(PSGQBinary ~ 
                              Age + Gender + Rejection.graft.dysfunction +
                              Any.fibrosis+Corticoid + Recurrence.of.disease +
                              Depression, data = dat3, family = binomial) 
summary(mymodel_PSGQbinary_4)

#without recurrence
mymodel_PSGQbinary_5 <- glm(PSGQBinary ~ 
                              Age + Gender + Rejection.graft.dysfunction +
                              Any.fibrosis+Corticoid +
                              Depression, data = dat3, family = binomial) 
summary(mymodel_PSGQbinary_5)

#without corticoid
mymodel_PSGQbinary_6 <- glm(PSGQBinary ~ 
                              Age + Gender + Rejection.graft.dysfunction +
                              Any.fibrosis +
                              Depression, data = dat3, family = binomial) 
summary(mymodel_PSGQbinary_6)

#without rejection
mymodel_PSGQbinary_7 <- glm(PSGQBinary ~ 
                              Age + Gender +
                              Any.fibrosis +
                              Depression, data = dat3, family = binomial) 
summary(mymodel_PSGQbinary_7)

#without fibrosis
mymodel_PSGQbinary_8 <- glm(PSGQBinary ~ 
                              Age + Gender + Depression, data = dat3, family = binomial) 
summary(mymodel_PSGQbinary_8)

#without age - both depression and gender are important predictors for this model
mymodel_PSGQbinary_9 <- glm(PSGQBinary ~ Gender + Depression, data = dat3, family = binomial) 
summary(mymodel_PSGQbinary_9)


#final tests show that model with Recurrence.of.disease is possible, but since p-value > 0.05, not ideal model.
#should just report that there are shortcomings in this model 
AIC(mymodel_PSGQBinary_all)
AIC(mymodel_PSGQbinary_9)

#plotting - how to do just 2 categorical in one plot ? is there way?  have separate graphs for now 
plot(allEffects(mymodel_PSGQbinary_9))



















############################3
####AIS Binary SLEEP SCALE####
#stepwise approach - take out predictor corresponding to largest p-value, refit model, and then repeat process
mymodel_AISBinary_all <- glm(AISBinary ~ 
                                Age + BMI + Time.from.transplant+ Gender + Rejection.graft.dysfunction +
                                Any.fibrosis+Corticoid + Liver.Diagnosis + Recurrence.of.disease + Renal.Failure+
                                Depression, data = dat3, family = binomial) 
summary(mymodel_AISBinary_all)


#without time from transplant
mymodel_AISBinary_1 <- glm(AISBinary ~ 
                               Age + BMI + Gender + Rejection.graft.dysfunction +
                               Any.fibrosis+Corticoid + Liver.Diagnosis + Recurrence.of.disease + Renal.Failure+
                               Depression, data = dat3, family = binomial) 
summary(mymodel_AISBinary_1)

#without liver diagnosis
mymodel_AISBinary_2 <- glm(AISBinary ~ 
                             Age + BMI + Gender + Rejection.graft.dysfunction +
                             Any.fibrosis+Corticoid + Recurrence.of.disease + Renal.Failure+
                             Depression, data = dat3, family = binomial) 
summary(mymodel_AISBinary_2)

#without BMI
mymodel_AISBinary_3 <- glm(AISBinary ~ 
                             Age + Gender + Rejection.graft.dysfunction +
                             Any.fibrosis+Corticoid + Recurrence.of.disease + Renal.Failure+
                             Depression, data = dat3, family = binomial) 
summary(mymodel_AISBinary_3)

#without renal failure 
mymodel_AISBinary_4 <- glm(AISBinary ~ 
                             Age + Gender + Rejection.graft.dysfunction +
                             Any.fibrosis+Corticoid + Recurrence.of.disease+
                             Depression, data = dat3, family = binomial) 
summary(mymodel_AISBinary_4)

#without recurrence
mymodel_AISBinary_5 <- glm(AISBinary ~ 
                             Age + Gender + Rejection.graft.dysfunction +
                             Any.fibrosis+Corticoid +
                             Depression, data = dat3, family = binomial) 
summary(mymodel_AISBinary_5)

#without fibrosis
mymodel_AISBinary_6 <- glm(AISBinary ~ 
                             Age + Gender + Rejection.graft.dysfunction + Corticoid +
                             Depression, data = dat3, family = binomial) 
summary(mymodel_AISBinary_6)


#without rejection
mymodel_AISBinary_7 <- glm(AISBinary ~ 
                             Age + Gender + Corticoid +
                             Depression, data = dat3, family = binomial) 
summary(mymodel_AISBinary_7)

#without cortiocid
mymodel_AISBinary_8 <- glm(AISBinary ~ 
                             Age + Gender  +
                             Depression, data = dat3, family = binomial) 
summary(mymodel_AISBinary_8)

#without gender - final predictors are age and depression
mymodel_AISBinary_9 <- glm(AISBinary ~ 
                             Age   +
                             Depression, data = dat3, family = binomial) 
summary(mymodel_AISBinary_9)

#final tests show that model with Recurrence.of.disease is possible, but since p-value > 0.05, not ideal model.
#should just report that there are shortcomings in this model 
AIC(mymodel_AISBinary_9)
AIC(mymodel_AISBinary_all)


age.vals <- seq(from=min(dat3$Age), to=max(dat3$Age),by = 0.01)

depression.mode <- names(which.max(table(dat3$Depression)))  #finding mode 

mynewdata2 <- data.frame(Age=age.vals, Depression = rep(depression.mode, length(age.vals)))
# let's take a look
mynewdata2
mypreds <- predict(mymodel_AISBinary_9, newdata = mynewdata2, type = "response")

plot(age.vals,mypreds,type = "l",xlab = "Age (yrs)",ylab = "Predicted probabilities")

plot(allEffects(mymodel_AISBinary_9)) #lower sleep scores with greater age, higher sleep scores with depression-state
