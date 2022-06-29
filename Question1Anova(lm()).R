
##### ESSBinary #####

# regression model with all predictors
all <- lm(ESSBinary ~ Gender + Liver.Diagnosis + Recurrence.of.disease + 
            Rejection.graft.dysfunction + Any.fibrosis + Renal.Failure + 
            Depression + Corticoid, data = dat3)


anova(all)

# no gender 
noGender <- lm(ESSBinary ~ Liver.Diagnosis + Recurrence.of.disease + 
                 Rejection.graft.dysfunction + Any.fibrosis + Renal.Failure + 
                 Depression + Corticoid, data = dat3)

summary(anova(all, noGender))

# no liver diagnosis 
noLiverDiagnosis <- lm(ESSBinary ~ Gender + Recurrence.of.disease + 
                         Rejection.graft.dysfunction + Any.fibrosis + 
                         Renal.Failure + Depression + Corticoid, data = dat3)

summary(anova(all, noLiverDiagnosis))

# no recurrence of disease 
noRecurrenceOfDisease <- lm(ESSBinary ~ Gender + Liver.Diagnosis + 
                              Rejection.graft.dysfunction + Any.fibrosis + 
                              Renal.Failure + Depression + Corticoid, data = dat3)

summary(anova(all, noRecurrenceOfDisease))

# no rejection graft dysfunction 

noRejectGraftDys <- lm(ESSBinary ~ Gender + Liver.Diagnosis + Recurrence.of.disease + 
                         Any.fibrosis + Renal.Failure + 
                         Depression + Corticoid, data = dat3)

summary(anova(all, noRejectGraftDys))

# No any fibrosis 

noAnyFIbro <- lm(ESSBinary ~ Gender + Liver.Diagnosis + Recurrence.of.disease + 
                   Rejection.graft.dysfunction + Renal.Failure + 
                   Depression + Corticoid, data = dat3)

summary(anova(all, noAnyFIbro))

# no renal failure 

noRenalFail <- lm(ESSBinary ~ Gender + Liver.Diagnosis + Recurrence.of.disease + 
                    Rejection.graft.dysfunction + Any.fibrosis + 
                    Depression + Corticoid, data = dat3)

summary(anova(all, noRenalFail))

# no depression 

noDepression <- lm(ESSBinary ~ Gender + Liver.Diagnosis + Recurrence.of.disease + 
                     Rejection.graft.dysfunction + Any.fibrosis + Renal.Failure + 
                     Corticoid, data = dat3)

summary(anova(all, noDepression))

# no corticoid 

noCorticoid <- lm(ESSBinary ~ Gender + Liver.Diagnosis + Recurrence.of.disease + 
                    Rejection.graft.dysfunction + Any.fibrosis + Renal.Failure + 
                    Depression, data = dat3)

summary(anova(all, noCorticoid))
















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






