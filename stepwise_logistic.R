###Data Transformation 

# Data cleaning
library(dplyr)
library(car)
dat <- read.csv("project_data.csv")

# Making variables binary
dat2 <- dat %>% 
  mutate(ESSBinary = ifelse(Epworth.Sleepiness.Scale > 10, 1, 0)) %>%
  mutate(PSGQBinary = ifelse(Pittsburgh.Sleep.Quality.Index.Score > 5, 1, 0)) %>%
  mutate(AISBinary = ifelse(Athens.Insomnia.Scale > 5, 1, 0))

# Removing NAs in relevant columns
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


#Converting categorical data into factors 
dat3$Liver.Diagnosis <- as.factor(dat3$Liver.Diagnosis)
dat3$Recurrence.of.disease <- as.factor(dat3$Recurrence.of.disease)
dat3$Rejection.graft.dysfunction <- as.factor(dat3$Rejection.graft.dysfunction)
dat3$Any.fibrosis <- as.factor(dat3$Any.fibrosis)
dat3$Renal.Failure <- as.factor(dat3$Renal.Failure)
dat3$Depression <- as.factor(dat3$Depression)
dat3$Corticoid <- as.factor(dat3$Corticoid)


# Converting gender to be coded as 0, 1 
dat3 <- dat3 %>% 
  mutate(Gender = ifelse(Gender >= 2, 1, 0))

#Converting remaining categorical data into factors 
dat3$Gender <- as.factor(dat3$Gender)
dat3$ESSBinary <- as.factor(dat3$ESSBinary)
dat3$PSGQBinary <- as.factor(dat3$PSGQBinary)
dat3$AISBinary <- as.factor(dat3$AISBinary)
dat3$AISBinary <- as.factor(dat3$Berlin.Sleepiness.Scale)
str(dat3)

####ESS SLEEP SCALE MODEL DEVELOPEMENT####
#Step wise approach, in full model, predictor corresponding to largest p-value is removed, model is refitted and process is repeated
mymodel_essbinary_all <- glm(ESSBinary ~ 
                               Age + BMI + Time.from.transplant + Gender + Rejection.graft.dysfunction +
                               Any.fibrosis + Corticoid + Liver.Diagnosis + Recurrence.of.disease + Renal.Failure+
                               Depression, data = dat3, family = binomial) 
summary(mymodel_essbinary_all)

#Renal Failure is identified as predictor with largest p-value, and is removed
mymodel_essbinary_1 <- glm(ESSBinary ~ 
                               Age + BMI + Time.from.transplant+ Gender + Rejection.graft.dysfunction +
                               Any.fibrosis+Corticoid + Liver.Diagnosis + Recurrence.of.disease+
                               Depression, data = dat3, family = binomial) 
summary(mymodel_essbinary_1)

#Depression is identified as predictor with largest p-value, and is removed
mymodel_essbinary_2 <- glm(ESSBinary ~ 
                             Age + BMI + Time.from.transplant+ Gender + Rejection.graft.dysfunction +
                             Any.fibrosis+Corticoid + Liver.Diagnosis + Recurrence.of.disease
                             , data = dat3, family = binomial) 
summary(mymodel_essbinary_2)

#Age is identified as predictor with largest p-value, and is removed
mymodel_essbinary_3 <- glm(ESSBinary ~ 
                             BMI + Time.from.transplant+ Gender + Rejection.graft.dysfunction +
                             Any.fibrosis+Corticoid + Liver.Diagnosis + Recurrence.of.disease
                           , data = dat3, family = binomial) 
summary(mymodel_essbinary_3)

#Recurrence is identified as predictor with largest p-value, and is removed
mymodel_essbinary_4 <- glm(ESSBinary ~ 
                             BMI + Time.from.transplant+ Gender + Rejection.graft.dysfunction +
                             Any.fibrosis+Corticoid + Liver.Diagnosis
                           , data = dat3, family = binomial) 
summary(mymodel_essbinary_4)


#Fibrosis is identified as predictor with largest p-value, and is removed
mymodel_essbinary_5 <- glm(ESSBinary ~ 
                             BMI + Time.from.transplant+ Gender + Rejection.graft.dysfunction +
                             Corticoid + Liver.Diagnosis
                           , data = dat3, family = binomial) 
summary(mymodel_essbinary_5)



#Rejection graft is identified as predictor with largest p-value, and is removed
mymodel_essbinary_6 <- glm(ESSBinary ~ 
                             BMI + Time.from.transplant+ Gender +
                             Corticoid + Liver.Diagnosis
                           , data = dat3, family = binomial) 
summary(mymodel_essbinary_6)

#Time from transplant is identified as predictor with largest p-value, and is removed
mymodel_essbinary_7 <- glm(ESSBinary ~ 
                             BMI + Gender +
                             Corticoid + Liver.Diagnosis
                           , data = dat3, family = binomial) 
summary(mymodel_essbinary_7)

#Liver diagnosis is identified as predictor with largest p-value, and is removed
mymodel_essbinary_8 <- glm(ESSBinary ~ 
                             BMI + Gender +
                             Corticoid
                           , data = dat3, family = binomial) 
summary(mymodel_essbinary_8)

#BMI is identified as predictor with largest p-value, and is removed
mymodel_essbinary_9 <- glm(ESSBinary ~ 
                            Gender +
                             Corticoid
                           , data = dat3, family = binomial) 
summary(mymodel_essbinary_9)


#Gender is identified as predictor with largest p-value, and is removed
#Corticosteroid Presence is identified as remaining predictor with p-value < 0.05, and retained in model 
mymodel_essbinary_10 <- glm(ESSBinary ~ Corticoid 
                           , data = dat3, family = binomial) 
summary(mymodel_essbinary_10)

#Comparison of AIC for original-full model and model with Corticosteroid
AIC(mymodel_essbinary_all)
AIC(mymodel_essbinary_10)


vif(mymodel_essbinary_all)
#plotting for ESS 
install.packages("effects")
library(effects)
plot(allEffects(mymodel_essbinary_10), main="Predicted Probabilities for Epworth Sleepiness Scale", xlab="Corticosteroid Use", ylab="Predicted Probabilities") 
#shows those without corticoid use have 
#lower sleep-scale measures vs those with use

#plot this way doesn't work:
#var1 = ESSBinary
#var2 = Corticoid
#Data frame with hp in ascending order
# Predicted_data <- data.frame(Corticoid=seq(min(dat3$Corticoid), max(dat3$Corticoid),len=100))
# 
# # Fill predicted values using regression model
# Predicted_data$ESSBinary <- predict(mymodel_essbinary_10, Predicted_data, type="response")
# 
# # Plot Predicted data and original data points
# plot(ESSBinary ~ Corticoid, data=dat3)
# lines(ESSBinary ~ Corticoid, Predicted_data, lwd=2, col="green")










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
vif(mymodel_bssbinary_all)
#plotting for ESS 

bmi_vals <- seq(min(dat3$BMI), max(dat3$BMI), 0.1)
mynewdata_3 <- data.frame(BMI=bmi_vals)
mypreds <- predict(mymodel_bssbinary_10, mynewdata_3, type="response")
plot(bmi_vals,mypreds,type = "l",xlab = "BMI",ylab = "Predicted probabilities", main="Predicted Probabilities for Berlin Sleepiness Scale")
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

AIC(mymodel_PSGQBinary_all)
AIC(mymodel_PSGQbinary_9)
vif(mymodel_PSGQBinary_all)
#plotting - how to do just 2 categorical in one plot ? is there way?  have separate graphs for now 
plot(allEffects(mymodel_PSGQbinary_9), main="Predicted Probabilities", ylab="PSQI Predicted Probabilities")


















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


##final tests
AIC(mymodel_AISBinary_9)
AIC(mymodel_AISBinary_all)

vif(mymodel_AISBinary_all)
age.vals <- seq(from=min(dat3$Age), to=max(dat3$Age),by = 0.01)

depression.mode <- names(which.max(table(dat3$Depression)))  #finding mode 

mynewdata2 <- data.frame(Age=age.vals, Depression = rep(depression.mode, length(age.vals)))
# let's take a look
mynewdata2
mypreds <- predict(mymodel_AISBinary_9, newdata = mynewdata2, type = "response")

plot(age.vals,mypreds,type = "l",xlab = "Age (yrs)",ylab = "Predicted probabilities")

plot(allEffects(mymodel_AISBinary_9), main="Predicted Probabilities for AIS", ylab="Predicted Probabilities") #lower sleep scores with greater age, higher sleep scores with depression-state
