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


#Prevalence for ESSBinary
dat3$ESSBinary %>% table() %>% prop.table()

#Prevalence for PSGQBinary
dat3$PSGQBinary %>% table() %>% prop.table()

#Prevalence for AISBinary
dat3$AISBinary %>% table() %>% prop.table()

#Prevalence for AISBinary
dat3$Berlin.Sleepiness.Scale %>% table() %>% prop.table()

attach(dat3)









#
attach(dat3)

age_vals <- seq(from = min(dat3$Age),
                to = max(dat3$Age),
                by = 1)

gender.mode <- as.numeric(names(which.max(table(dat3$Gender)))) #finding mode
Liver.Diagnosis.mode <- as.numeric(names(which.max(table(dat3$Liver.Diagnosis))))
Recurrence.of.disease.mode <- as.numeric(names(which.max(table(dat3$Recurrence.of.disease))))
Rejection.graft.dysfunction.mode <- as.numeric(names(which.max(table(dat3$Rejection.graft.dysfunction))))
Any.fibrosis.mode <- as.numeric(names(which.max(table(dat3$Any.fibrosis))))
Renal.Failure.mode <- as.numeric(names(which.max(table(dat3$Renal.Failure))))
Depression.mode <- as.numeric(names(which.max(table(dat3$Depression))))
Corticoid.mode <- as.numeric(names(which.max(table(dat3$Corticoid))))

trial_data <- data.frame(
  age = age_vals,
  gender_mode = rep(gender.mode, length(age_vals)),
  Liver.Diagnosis_mode = rep(Liver.Diagnosis.mode, length(age_vals)),
  Recurrence.of.disease_mode = rep(Recurrence.of.disease.mode, length(age_vals)),
  Rejection.graft.dysfunction_mode = rep(Rejection.graft.dysfunction.mode, length(age_vals)),
  Any.fibrosis_mode = rep(Any.fibrosis.mode, length(age_vals)),
  Renal.Failure_mode = rep(Renal.Failure.mode, length(age_vals)),
  Depression_mode = rep(Depression.mode, length(age_vals)),
  Corticoid_mode = rep(Corticoid.mode, length(age_vals))
)
#model has only age as numeric, removed other numeric variables: BMI + Time.from.transplant
mymodel_essbinary_age <- glm(ESSBinary ~ Age+Gender+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, 
               data = dat3, family = binomial)

my_preds <- predict(mymodel_essbinary_age, newdata = trial_data, type = "response")

plot(age_vals,my_preds,type = "l",xlab = "Age (yrs)",ylab = "Predicted probabilities")
