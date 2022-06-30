
####ESS SLEEP SCALE####
#stepwise approach - take out predictor corresponding to largest p-value, refit model, and then repeat process
library(ggplot2)
mymodel_essbinary_all <- glm(ESSBinary ~ 
                               Age + BMI + Time.from.transplant+ Gender + Rejection.graft.dysfunction +
                               Any.fibrosis+Corticoid + Liver.Diagnosis + Recurrence.of.disease + Renal.Failure+
                               Depression, data = dat3, family = binomial) 

#without renal failure
mymodel_essbinary_1 <- glm(ESSBinary ~ 
                               Age + BMI + Time.from.transplant+ Gender + Rejection.graft.dysfunction +
                               Any.fibrosis+Corticoid + Liver.Diagnosis + Recurrence.of.disease+
                               Depression, data = dat3, family = binomial) 

#without age
mymodel_essbinary_2 <- glm(ESSBinary ~ 
                               BMI + Time.from.transplant+ Gender + Rejection.graft.dysfunction +
                               Any.fibrosis+Corticoid + Liver.Diagnosis + Recurrence.of.disease+
                               Depression, data = dat3, family = binomial) 


#without rejection graft dysfunction
mymodel_essbinary_3 <- glm(ESSBinary ~ 
                               BMI + Time.from.transplant+ Gender +
                               Any.fibrosis+Corticoid + Liver.Diagnosis + Recurrence.of.disease+
                               Depression, data = dat3, family = binomial) 


#without fibrosis
mymodel_essbinary_4 <- glm(ESSBinary ~ 
                               BMI + Time.from.transplant+ Gender +
                               Corticoid + Liver.Diagnosis + Recurrence.of.disease+
                               Depression, data = dat3, family = binomial) 


#without time from transplant
mymodel_essbinary_5 <- glm(ESSBinary ~ 
                               BMI + Gender +
                               Corticoid + Liver.Diagnosis + Recurrence.of.disease+
                               Depression, data = dat3, family = binomial) 


#without depression
mymodel_essbinary_6 <- glm(ESSBinary ~ 
                               BMI + Gender +
                               Corticoid + Liver.Diagnosis + Recurrence.of.disease
                             , data = dat3, family = binomial) 

#without liver diagnosis
mymodel_essbinary_7 <- glm(ESSBinary ~ 
                               BMI + Gender +
                               Corticoid  + Recurrence.of.disease
                             , data = dat3, family = binomial) 

#without BMI
mymodel_essbinary_8 <- glm(ESSBinary ~ 
                               Gender +
                               Corticoid  + Recurrence.of.disease
                             , data = dat3, family = binomial) 


#without gender
mymodel_essbinary_9 <- glm(ESSBinary ~ Corticoid  + Recurrence.of.disease
                             , data = dat3, family = binomial)


#without recurrence of disease - final model with significant predictors 
mymodel_essbinary_final <- glm(ESSBinary ~ Corticoid, data = dat3, family = binomial)

#final tests show that model with corticoid is significant
summary(mymodel_essbinary_all)
summary(mymodel_essbinary_final)
AIC(mymodel_essbinary_all)
AIC(mymodel_essbinary_final)

#plotting for ESS 
## Lastly, let's  see what this logistic regression predicts, given
## that a patient is either female or male (and no other data about them).

new_trial_data <- seq(min(dat3$Corticoid), max(dat3$Corticoid), 0.01)
yv <- predict(mymodel_essbinary_final, list(Corticoid=new_trial_data), type="response")
plot(xv,yv)














###
dat3$Berlin.Sleepiness.Scale
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

#without gender
mymodel_bssbinary_4 <- glm(Berlin.Sleepiness.Scale ~ 
                             BMI + Time.from.transplant + Rejection.graft.dysfunction +
                             Any.fibrosis+ Corticoid + Liver.Diagnosis + Recurrence.of.disease
                           , data = dat3, family = binomial) 

summary(mymodel_bssbinary_4)

#without liver diagnosis
mymodel_bssbinary_5 <- glm(Berlin.Sleepiness.Scale ~ 
                             BMI + Time.from.transplant + Rejection.graft.dysfunction +
                             Any.fibrosis+Corticoid + Recurrence.of.disease
                           , data = dat3, family = binomial) 

summary(mymodel_bssbinary_5)

#without time from transplant
mymodel_bssbinary_6 <- glm(Berlin.Sleepiness.Scale ~ 
                             BMI + Rejection.graft.dysfunction +
                             Any.fibrosis+Corticoid + Recurrence.of.disease
                           , data = dat3, family = binomial) 

summary(mymodel_bssbinary_6)

#without any fibrosis
mymodel_bssbinary_7 <- glm(Berlin.Sleepiness.Scale ~ 
                             BMI + Rejection.graft.dysfunction +
                             +Corticoid + Recurrence.of.disease
                           , data = dat3, family = binomial) 

summary(mymodel_bssbinary_7)

#without BMI
mymodel_bssbinary_8 <- glm(Berlin.Sleepiness.Scale ~ 
                              Rejection.graft.dysfunction +
                             Corticoid + Recurrence.of.disease
                           , data = dat3, family = binomial) 

summary(mymodel_bssbinary_8)

#without corticoid
mymodel_bssbinary_9 <- glm(Berlin.Sleepiness.Scale ~ 
                             Rejection.graft.dysfunction 
                            + Recurrence.of.disease
                           , data = dat3, family = binomial) 

summary(mymodel_bssbinary_9)


#without rejection - has only recurrence of disease 
mymodel_bssbinary_10 <- glm(Berlin.Sleepiness.Scale ~ 
                             Recurrence.of.disease
                           , data = dat3, family = binomial) 

summary(mymodel_bssbinary_10)

#final tests show that model with Recurrence.of.disease is possible, but since p-value > 0.05, not ideal model.
#should just report that there are shortcomings in this model 
AIC(mymodel_bssbinary_10)
AIC(mymodel_bssbinary_all)

#plotting for ESS 
## Lastly, let's  see what this logistic regression predicts, given
## that a patient's recurrence of disease is provided, and no other information

new_trial_data <- seq(min(dat3$Recurrence.of.disease), max(dat3$Recurrence.of.disease), 0.01)
yv <- predict(mymodel_bssbinary_10, list(Recurrence.of.disease=new_trial_data), type="response")
plot(xv,yv)





