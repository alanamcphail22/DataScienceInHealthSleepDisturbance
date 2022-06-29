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


#Plotting probabilities with new data 
attach(dat3)
#creating ages vector
age_vals <- seq(from = min(dat3$Age),
                to = max(dat3$Age),
                by = 1)

#retrieving mode for relevant features 
gender.mode <- as.numeric(names(which.max(table(dat3$Gender)))) #finding mode
Liver.Diagnosis.mode <- as.numeric(names(which.max(table(dat3$Liver.Diagnosis))))
Recurrence.of.disease.mode <- as.numeric(names(which.max(table(dat3$Recurrence.of.disease))))
Rejection.graft.dysfunction.mode <- as.numeric(names(which.max(table(dat3$Rejection.graft.dysfunction))))
Any.fibrosis.mode <- as.numeric(names(which.max(table(dat3$Any.fibrosis))))
Renal.Failure.mode <- as.numeric(names(which.max(table(dat3$Renal.Failure))))
Depression.mode <- as.numeric(names(which.max(table(dat3$Depression))))
Corticoid.mode <- as.numeric(names(which.max(table(dat3$Corticoid))))

#generating trial dataframe of new data with ages and features 
trial_data <- data.frame(
  Age = age_vals,
  Gender = rep(gender.mode, length(age_vals)),
  Liver.Diagnosis = rep(Liver.Diagnosis.mode, length(age_vals)),
  Recurrence.of.disease = rep(Recurrence.of.disease.mode, length(age_vals)),
  Rejection.graft.dysfunction = rep(Rejection.graft.dysfunction.mode, length(age_vals)),
  Any.fibrosis = rep(Any.fibrosis.mode, length(age_vals)),
  Renal.Failure = rep(Renal.Failure.mode, length(age_vals)),
  Depression = rep(Depression.mode, length(age_vals)),
  Corticoid = rep(Corticoid.mode, length(age_vals))
)

#experimenting with lgm models 
#currently: model has only age as numeric, removed other numeric variables: BMI + Time.from.transplant
mymodel_essbinary_age <- glm(ESSBinary ~ Age+Gender+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, 
               data = dat3, family = binomial)

#can make another model by subtracting/adding variables 
mymodel_essbinary_age_2 <- glm(ESSBinary ~ Age+Renal.Failure+Depression+Corticoid, 
                             data = dat3, family = binomial)

#ran cho-sq test to compare the two models generated 
anova(mymodel_essbinary_age, mymodel_essbinary_age_2, test="Chisq")

#this vector will have predictions from the provided trial data for ESSBinary scale 
my_preds <- predict(mymodel_essbinary_age, newdata = trial_data, type = "response")

#plotting graph 
plot(age_vals,my_preds,type = "l",xlab = "Age (yrs)",ylab = "Predicted probabilities")





#Alternative way of doing this for ESSBinary scale: -- I think this can be our main strategy? different ANOVAs?
attach(dat3)

#Generating Boxplots for ESSBinary to see which categorical predictors are informative, then will use these predictors in modelling
boxplot(formula = Gender ~ ESSBinary, data=dat3) 
boxplot(formula = Liver.Diagnosis~ ESSBinary, data=dat3)
boxplot(formula = Recurrence.of.disease ~ESSBinary, data=dat3)
boxplot(formula = Rejection.graft.dysfunction~ ESSBinary, data=dat3)
boxplot(formula = Any.fibrosis~ ESSBinarys, data=dat3)
boxplot(formula = Renal.Failure~ ESSBinary, data=dat3)
boxplot(formula = Depression ~ESSBinary, data=dat3)
boxplot(formula = Corticoid ~ESSBinary, data=dat3)

dat3 <- na.omit(dat3)

#Corticoid is significant for ESSBinary
mymodel_essbinary_1 <- glm(ESSBinary ~ Age+BMI+Time.from.transplant+Gender+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, 
                             data = dat3, family = binomial) 
summary(mymodel_essbinary_1)

#Gender and Depression for PSGQBinary
mymodel_PSGQBinary <- glm(PSGQBinary ~ Age+BMI+Time.from.transplant+Gender+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, 
                           data = dat3, family = binomial) 
summary(mymodel_PSGQBinary)

# AISBinary - Age and Depression and Corticoid
mymodel_AISBinary <- glm(AISBinary ~ Age+BMI+Time.from.transplant+Gender+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, 
                          data = dat3, family = binomial) 
summary(mymodel_AISBinary)

#Berlin.Sleepiness.Scale - BMI 
mymodel_Berlin.Sleepiness.Scale <- glm(Berlin.Sleepiness.Scale ~ Age+BMI+Time.from.transplant+Gender+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, 
                         data = dat3, family = binomial) 
summary(mymodel_Berlin.Sleepiness.Scale)


anova(mymodel_essbinary_1, mymodel_essbinary_2, test="Chisq")
#removed 
mymodel_essbinary_2 <- glm(ESSBinary ~ Age+BMI+Time.from.transplant+Gender+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, 
                           data = dat3, family = binomial)

