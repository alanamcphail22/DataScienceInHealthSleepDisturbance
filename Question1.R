#Question1


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


mymodel <-
  glm(
    ESSBinary ~ Age + Gender + BMI + Time.from.transplant + Liver.Diagnosis +
      Recurrence.of.disease + Rejection.graft.dysfunction + Any.fibrosis + Renal.Failure +
      Depression + Corticoid ,
    data = dat3,
    family = binomial
  )

# let's take a look
mynewdata2
mypreds <-
  predict(mymodel2, newdata = mynewdata2, type = "response")

plot(age.vals,
     mypreds,
     type = "l",
     xlab = "Age (yrs)",
     ylab = "Predicted probabilities")


mymodel_2 <-
  glm(
    PSGQBinary ~ Age + Gender + BMI + Time.from.transplant + Liver.Diagnosis +
      Recurrence.of.disease +
      Rejection.graft.dysfunction + Any.fibrosis + Renal.Failure +
      Depression + Corticoid ,
    data = dat3,
    family = binomial
  )

anova(mymodel, mymodel_2)


#Prevalence for ESSBinary
dat3$ESSBinary %>% table() %>% prop.table()

#Prevalence for PSGQBinary
dat3$PSGQBinary %>% table() %>% prop.table()

#Prevalence for AISBinary
dat3$AISBinary %>% table() %>% prop.table()

#Prevalence for AISBinary
dat3$Berlin.Sleepiness.Scale %>% table() %>% prop.table()
attach(dat3)


#BMI, Age, Time.Elapsed
age_vals <- seq(from = min(dat3$Age),
                to = max(dat3$Age),
                by = 1)

gender.mode <- names(which.max(table(dat3$Gender))) #finding mode
Liver.Diagnosis.mode <-
  names(which.max(table(dat3$Liver.Diagnosis)))
Recurrence.of.disease.mode <-
  names(which.max(table(dat3$Recurrence.of.disease)))
Rejection.graft.dysfunction.mode <-
  names(which.max(table(dat3$Rejection.graft.dysfunction)))
Any.fibrosis.mode <- names(which.max(table(dat3$Any.fibrosis.mode)))
Renal.Failure.mode <- names(which.max(table(dat3$Renal.Failure)))
Depression.mode <- names(which.max(table(dat3$Depression)))
Corticoid.mode <- names(which.max(table(dat3$Corticoid)))



trial_data <- data.frame(
  age = age_vals,
  gender_mode = rep(dat3$gender.mode, length(age_vals)),
  Liver.Diagnosis_mode = rep(dat3$Liver.Diagnosis.mode, length(age_vals)),
  Recurrence.of.disease_mode = rep(dat3$Recurrence.of.disease.mode, length(age_vals)),
  Rejection.graft.dysfunction_mode = rep(dat3$Rejection.graft.dysfunction.mode, length(age_vals)),
  Any.fibrosis_mode = rep(dat3$Any.fibrosis.mode, length(age_vals)),
  Renal.Failure_mode = rep(dat3$Renal.Failure.mode, length(age_vals)),
  Depression_mode = rep(dat3$Depression.mode, length(age_vals)),
  Corticoid_mode = rep(dat3$Corticoid.mode, length(age_vals))
)
