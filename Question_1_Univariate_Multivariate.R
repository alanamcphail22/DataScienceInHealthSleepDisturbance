
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
















##### PSGQBinary ###### 
# regression model with all predictors
all2 <- lm(PSGQBinary ~ Gender + Liver.Diagnosis + Recurrence.of.disease + 
             Rejection.graft.dysfunction + Any.fibrosis + Renal.Failure + 
             Depression + Corticoid, data = dat3)


anova(all2)

# no gender 
noGender2 <- lm(PSGQBinary ~ Liver.Diagnosis + Recurrence.of.disease + 
                  Rejection.graft.dysfunction + Any.fibrosis + Renal.Failure + 
                  Depression + Corticoid, data = dat3)

summary(anova(all2, noGender2))

# no liver diagnosis 
noLiverDiagnosis2 <- lm(PSGQBinary ~ Gender + Recurrence.of.disease + 
                          Rejection.graft.dysfunction + Any.fibrosis + 
                          Renal.Failure + Depression + Corticoid, data = dat3)

summary(anova(all2, noLiverDiagnosis2))

# no recurrence of disease 
noRecurrenceOfDisease2 <- lm(PSGQBinary ~ Gender + Liver.Diagnosis + 
                               Rejection.graft.dysfunction + Any.fibrosis + 
                               Renal.Failure + Depression + Corticoid, data = dat3)

summary(anova(all2, noRecurrenceOfDisease2))

# no rejection graft dysfunction 

noRejectGraftDys2 <- lm(PSGQBinary ~ Gender + Liver.Diagnosis + Recurrence.of.disease + 
                          Any.fibrosis + Renal.Failure + 
                          Depression + Corticoid, data = dat3)

summary(anova(all2, noRejectGraftDys2))

# No any fibrosis 

noAnyFIbro2 <- lm(PSGQBinary ~ Gender + Liver.Diagnosis + Recurrence.of.disease + 
                    Rejection.graft.dysfunction + Renal.Failure + 
                    Depression + Corticoid, data = dat3)

summary(anova(all2, noAnyFIbro2))

# no renal failure 

noRenalFail2 <- lm(PSGQBinary ~ Gender + Liver.Diagnosis + Recurrence.of.disease + 
                     Rejection.graft.dysfunction + Any.fibrosis + 
                     Depression + Corticoid, data = dat3)

summary(anova(all2, noRenalFail2))

# no depression 

noDepression2 <- lm(PSGQBinary ~ Gender + Liver.Diagnosis + Recurrence.of.disease + 
                      Rejection.graft.dysfunction + Any.fibrosis + Renal.Failure + 
                      Corticoid, data = dat3)

summary(anova(all2, noDepression2))

# no corticoid 

noCorticoid2 <- lm(PSGQBinary ~ Gender + Liver.Diagnosis + Recurrence.of.disease + 
                     Rejection.graft.dysfunction + Any.fibrosis + Renal.Failure + 
                     Depression, data = dat3)

summary(anova(all2, noCorticoid2))










##### AISBinary #####


# regression model with all predictors
all3 <- lm(AISBinary ~ Gender + Liver.Diagnosis + Recurrence.of.disease + 
             Rejection.graft.dysfunction + Any.fibrosis + Renal.Failure + 
             Depression + Corticoid, data = dat3)


anova(all3)

# no gender 
noGender3 <- lm(AISBinary ~ Liver.Diagnosis + Recurrence.of.disease + 
                  Rejection.graft.dysfunction + Any.fibrosis + Renal.Failure + 
                  Depression + Corticoid, data = dat3)

summary(anova(all3, noGender3))

# no liver diagnosis 
noLiverDiagnosis3 <- lm(AISBinary ~ Gender + Recurrence.of.disease + 
                          Rejection.graft.dysfunction + Any.fibrosis + 
                          Renal.Failure + Depression + Corticoid, data = dat3)

summary(anova(all3, noLiverDiagnosis3))

# no recurrence of disease 
noRecurrenceOfDisease3 <- lm(AISBinary ~ Gender + Liver.Diagnosis + 
                               Rejection.graft.dysfunction + Any.fibrosis + 
                               Renal.Failure + Depression + Corticoid, data = dat3)

summary(anova(all3, noRecurrenceOfDisease3))

# no rejection graft dysfunction 

noRejectGraftDys3 <- lm(AISBinary ~ Gender + Liver.Diagnosis + Recurrence.of.disease + 
                          Any.fibrosis + Renal.Failure + 
                          Depression + Corticoid, data = dat3)

summary(anova(all3, noRejectGraftDys3))

# No any fibrosis 

noAnyFIbro3 <- lm(AISBinary ~ Gender + Liver.Diagnosis + Recurrence.of.disease + 
                    Rejection.graft.dysfunction + Renal.Failure + 
                    Depression + Corticoid, data = dat3)

summary(anova(all3, noAnyFIbro3))

# no renal failure 

noRenalFail3 <- lm(AISBinary ~ Gender + Liver.Diagnosis + Recurrence.of.disease + 
                     Rejection.graft.dysfunction + Any.fibrosis + 
                     Depression + Corticoid, data = dat3)

summary(anova(all3, noRenalFail3))

# no depression 

noDepression3 <- lm(AISBinary ~ Gender + Liver.Diagnosis + Recurrence.of.disease + 
                      Rejection.graft.dysfunction + Any.fibrosis + Renal.Failure + 
                      Corticoid, data = dat3)

summary(anova(all3, noDepression3))

# no corticoid 

noCorticoid3 <- lm(AISBinary ~ Gender + Liver.Diagnosis + Recurrence.of.disease + 
                     Rejection.graft.dysfunction + Any.fibrosis + Renal.Failure + 
                     Depression, data = dat3)

summary(anova(all3, noCorticoid3))













##### Berlin Sleepines Scale ##### 


# regression model with all predictors
all4 <- lm(Berlin.Sleepiness.Scale ~ Gender + Liver.Diagnosis + Recurrence.of.disease + 
             Rejection.graft.dysfunction + Any.fibrosis + Renal.Failure + 
             Depression + Corticoid, data = dat3)


anova(all4)

# no gender 
noGender4 <- lm(Berlin.Sleepiness.Scale ~ Liver.Diagnosis + Recurrence.of.disease + 
                  Rejection.graft.dysfunction + Any.fibrosis + Renal.Failure + 
                  Depression + Corticoid, data = dat3)

summary(anova(all4, noGender4))

# no liver diagnosis 
noLiverDiagnosis4 <- lm(Berlin.Sleepiness.Scale ~ Gender + Recurrence.of.disease + 
                          Rejection.graft.dysfunction + Any.fibrosis + 
                          Renal.Failure + Depression + Corticoid, data = dat3)

summary(anova(all4, noLiverDiagnosis4))

# no recurrence of disease 
noRecurrenceOfDisease4 <- lm(Berlin.Sleepiness.Scale ~ Gender + Liver.Diagnosis + 
                               Rejection.graft.dysfunction + Any.fibrosis + 
                               Renal.Failure + Depression + Corticoid, data = dat3)

summary(anova(all4, noRecurrenceOfDisease4))

# no rejection graft dysfunction 

noRejectGraftDys4 <- lm(Berlin.Sleepiness.Scale ~ Gender + Liver.Diagnosis + Recurrence.of.disease + 
                          Any.fibrosis + Renal.Failure + 
                          Depression + Corticoid, data = dat3)

summary(anova(all4, noRejectGraftDys4))

# No any fibrosis 

noAnyFIbro4 <- lm(Berlin.Sleepiness.Scale ~ Gender + Liver.Diagnosis + Recurrence.of.disease + 
                    Rejection.graft.dysfunction + Renal.Failure + 
                    Depression + Corticoid, data = dat3)

summary(anova(all4, noAnyFIbro4))

# no renal failure 

noRenalFail4 <- lm(Berlin.Sleepiness.Scale ~ Gender + Liver.Diagnosis + Recurrence.of.disease + 
                     Rejection.graft.dysfunction + Any.fibrosis + 
                     Depression + Corticoid, data = dat3)

summary(anova(all4, noRenalFail4))

# no depression 

noDepression4 <- lm(Berlin.Sleepiness.Scale ~ Gender + Liver.Diagnosis + Recurrence.of.disease + 
                      Rejection.graft.dysfunction + Any.fibrosis + Renal.Failure + 
                      Corticoid, data = dat3)

summary(anova(all4, noDepression4))

# no corticoid 

noCorticoid4 <- lm(Berlin.Sleepiness.Scale ~ Gender + Liver.Diagnosis + Recurrence.of.disease + 
                     Rejection.graft.dysfunction + Any.fibrosis + Renal.Failure + 
                     Depression, data = dat3)

summary(anova(all4, noCorticoid4))






