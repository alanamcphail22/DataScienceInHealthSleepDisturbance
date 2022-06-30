#stepwise approach - take out predictor corresponding to largest p-value, refit model, and then repeat process
mymodel_essbinary_all <- glm(ESSBinary ~ 
                               Age + BMI + Time.from.transplant+ Gender + Rejection.graft.dysfunction +
                               Any.fibrosis+Corticoid + Liver.Diagnosis + Recurrence.of.disease + Renal.Failure+
                               Depression, data = dat3, family = binomial) 

#without renal failure
mymodel_essbinary_all <- glm(ESSBinary ~ 
                               Age + BMI + Time.from.transplant+ Gender + Rejection.graft.dysfunction +
                               Any.fibrosis+Corticoid + Liver.Diagnosis + Recurrence.of.disease+
                               Depression, data = dat3, family = binomial) 

#without age
mymodel_essbinary_all <- glm(ESSBinary ~ 
                               BMI + Time.from.transplant+ Gender + Rejection.graft.dysfunction +
                               Any.fibrosis+Corticoid + Liver.Diagnosis + Recurrence.of.disease+
                               Depression, data = dat3, family = binomial) 


#without rejection graft dysfunction
mymodel_essbinary_all <- glm(ESSBinary ~ 
                               BMI + Time.from.transplant+ Gender +
                               Any.fibrosis+Corticoid + Liver.Diagnosis + Recurrence.of.disease+
                               Depression, data = dat3, family = binomial) 


#without fibrosis
mymodel_essbinary_all <- glm(ESSBinary ~ 
                               BMI + Time.from.transplant+ Gender +
                               Corticoid + Liver.Diagnosis + Recurrence.of.disease+
                               Depression, data = dat3, family = binomial) 


#without time from transplant
mymodel_essbinary_all <- glm(ESSBinary ~ 
                               BMI + Gender +
                               Corticoid + Liver.Diagnosis + Recurrence.of.disease+
                               Depression, data = dat3, family = binomial) 


#without depression
mymodel_essbinary_all <- glm(ESSBinary ~ 
                               BMI + Gender +
                               Corticoid + Liver.Diagnosis + Recurrence.of.disease
                             , data = dat3, family = binomial) 

#without liver diagnosis
mymodel_essbinary_all <- glm(ESSBinary ~ 
                               BMI + Gender +
                               Corticoid  + Recurrence.of.disease
                             , data = dat3, family = binomial) 

#without BMI
mymodel_essbinary_all <- glm(ESSBinary ~ 
                               Gender +
                               Corticoid  + Recurrence.of.disease
                             , data = dat3, family = binomial) 


#without gender
mymodel_essbinary_all <- glm(ESSBinary ~ Corticoid  + Recurrence.of.disease
                             , data = dat3, family = binomial)


#without recurrence of disease 
mymodel_essbinary_all <- glm(ESSBinary ~ Corticoid
                             , data = dat3, family = binomial)
summary(mymodel_essbinary_all)

