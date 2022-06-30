
##### ESSBinary #####

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

##univariate and then multivariate test
#bmi 
bmi_model <- glm(ESSBinary ~ BMI, data = dat3, family=binomial)
summary(bmi_model) #

#age 
age_model <- glm(ESSBinary ~ Age, data = dat3, family=binomial)
summary(age_model) #KEEP

#time since 
time_since_transplant_model <- glm(ESSBinary ~ Time.from.transplant, data = dat3, family=binomial)
summary(time_since_transplant_model)

#gender
attach(dat3)
gender <- glm(ESSBinary ~ Gender, data = dat3, family=binomial)
summary(gender) #KEEP

# liver diagnosis 
liver_diagnosis  <- glm(ESSBinary ~ Liver.Diagnosis, data = dat3, family=binomial)
summary(liver_diagnosis)

# recurrence of disease 
RecurrenceOfDisease <- glm(ESSBinary ~ Recurrence.of.disease, data = dat3, family=binomial)
summary(RecurrenceOfDisease)

# rejection graft dysfunction 
rejection_graft_dysfunction  <- glm(ESSBinary ~ Rejection.graft.dysfunction, data = dat3, family=binomial)
summary(rejection_graft_dysfunction) #KEEP

#fibrosis 
fibro <- glm(ESSBinary ~ Any.fibrosis, data = dat3, family=binomial)
summary(fibro)#KEEP

# renal failure 
renal_failure <- glm(ESSBinary ~ Renal.Failure, data = dat3, family=binomial)
summary(renal_failure)

# depression 
Depression <- glm(ESSBinary ~ Depression, data = dat3, family=binomial)
summary(Depression)

# corticoid 
Corticoid <- glm(ESSBinary ~ Corticoid, data = dat3, family=binomial)
summary(Corticoid) #KEEP


mymodel_essbinary_all <- glm(ESSBinary ~ 
                             Age + BMI + Time.from.transplant+ Gender + Rejection.graft.dysfunction +
                             Any.fibrosis+Corticoid + Liver.Diagnosis + Recurrence.of.disease + Renal.Failure+
                             Depression, data = dat3, family = binomial) 

mymodel_essbinary_without_corticoid <- glm(ESSBinary ~ 
                               Age + BMI + Time.from.transplant+ Gender + Rejection.graft.dysfunction +
                               Any.fibrosis + Liver.Diagnosis + Recurrence.of.disease + Renal.Failure+
                               Depression, data = dat3, family = binomial) 


model_all <- glm(ESSBinary ~ Age + BMI + Gender + Time.from.transplant + Liver.Diagnosis + Recurrence.of.disease + 
            Rejection.graft.dysfunction + Any.fibrosis + Renal.Failure + 
            Depression + Corticoid, data = dat3)
model_without_corticoid <- glm(ESSBinary ~ Age + BMI + Gender + Time.from.transplant + Liver.Diagnosis + Recurrence.of.disease + 
                   Rejection.graft.dysfunction + Any.fibrosis + Renal.Failure + 
                   Depression, data = dat3)
models_all_sig_wogender <- glm(ESSBinary ~Age + Rejection.graft.dysfunction + Any.fibrosis + Corticoid, data = dat3, family = binomial)
models_all_sig_wo_corticoid <- glm(ESSBinary ~ 
                                     Age + Gender + Rejection.graft.dysfunction + Any.fibrosis, data = dat3, family = binomial)
AIC(model_all)
AIC(model_without_corticoid)
AIC(models_all_sig_wogender)
AIC(models_all_sig_wo_corticoid)
AIC(models_all_sig)

summary(model_all)
anova(model_all,model_without_corticoid, test="Chisq")


models_all_sig <- glm(ESSBinary ~ 
                        Age + Gender + Rejection.graft.dysfunction + Any.fibrosis + Corticoid, data = dat3, family = binomial) 

models_all_sig_wo_corticoid <- glm(ESSBinary ~ 
                        Age + Gender + Rejection.graft.dysfunction + Any.fibrosis, data = dat3, family = binomial)

models_all_sig_wogender <- glm(ESSBinary ~ 
                                  Age + Rejection.graft.dysfunction + Any.fibrosis + Corticoid, data = dat3, family = binomial)
#anova shows sig 
anova(mymodel_essbinary_all,mymodel_essbinary_without_corticoid,test="Chisq")
anova(models_all_sig,models_all_sig_wo_corticoid,test="Chisq")

---

