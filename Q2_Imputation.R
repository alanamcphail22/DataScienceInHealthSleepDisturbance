### Data cleaning
library(tidyr)
library(dplyr)
library(car)
library(MASS)
library(mice)


# Loading dataset
dat <- read.csv("project_data.csv")
dim(dat) #268 rows

#Sub-setting data frame for the variables we are looking into
dat <- dat[c("Age", "Gender", "BMI","Time.from.transplant","Liver.Diagnosis", "Recurrence.of.disease",
             "Rejection.graft.dysfunction", "Any.fibrosis", "Renal.Failure", "Depression","Corticoid",
             "Epworth.Sleepiness.Scale", "Athens.Insomnia.Scale", "Berlin.Sleepiness.Scale",
             "Pittsburgh.Sleep.Quality.Index.Score", "SF36.PCS", "SF36.MCS")]

# Renaming variables and counting the NA values 
# PSGQ NA: 85 <- more than 30% of data is missing (NA > 80), can remove entire variable
# AIS NA: 6
# ESS NA: 17
# BSS NA: 6
apply(is.na(dat),2,sum)

# Removing Pittsburgh.Sleep.Quality.Index.Score due to <30% NA values
dat2 <- dat[c("Age", "Gender", "BMI","Time.from.transplant","Liver.Diagnosis", "Recurrence.of.disease",
              "Rejection.graft.dysfunction", "Any.fibrosis", "Renal.Failure", "Depression","Corticoid",
              "Epworth.Sleepiness.Scale", "Athens.Insomnia.Scale", "Berlin.Sleepiness.Scale",
              "SF36.PCS", "SF36.MCS")]

# Renaming predictors
dat2 <- dat2 %>% 
  rename(ESS = Epworth.Sleepiness.Scale, 
         AIS = Athens.Insomnia.Scale, BSS = Berlin.Sleepiness.Scale) 




# Changing BSS to a logical factor.
dat2$BSS <- as.logical(dat2$BSS)
dat2$SF36.PCS <- as.numeric(dat2$SF36.PCS)
dat2$SF36.MCS <- as.numeric(dat2$SF36.MCS)
dat2$ESS <- as.numeric(dat2$ESS)
dat2$AIS <- as.numeric(dat2$AIS)


#Checking the how the missing values relate to the other variables - all cases are MAR
data_missing_ESS <- dat2 %>%
  filter(is.na(ESS))
View(data_missing_ESS)

data_missing_AIS <- dat2 %>%
  filter(is.na(AIS))
View(data_missing_AIS)

data_missing_BSS <- dat2 %>%
  filter(is.na(BSS))
View(data_missing_BSS)


########################################################

# Q2: Building a multiple linear regression
# Predictor: Sleep quality (all 4 measurements)
# Response: MCS and PCS.


# IMPUTATIONS FOR MISSING VALUES in PCS 

# stochastic regression -> improvement from the above method "norm.nob" -> added some error +noise
data.lim.PCS <- dat2[, c("SF36.PCS", "ESS", "AIS","BSS")]
imp <- mice(data.lim.PCS, method = "norm.nob", seed = 11,
            m = 1, print = FALSE)
xyplot(imp, SF36.PCS ~ ESS + AIS + BSS) # Imputed values for ozone are not always the same, it depends on solar.

fit.stoch <- with(imp, lm(SF36.PCS ~ ESS + AIS + BSS))
summary(pool(fit.stoch))

#Complete function extracts the imputed data 
imputed.dataframe <- complete(imp)
imputed.dataframe <- as.data.frame(imputed.dataframe)
dim(imputed.dataframe)
dim(dat2)


# Running the model with imputed dataframe
PCS <- lm(SF36.PCS ~ ESS + AIS + BSS, data = imputed.dataframe)
summary(PCS)

# Is there a way to specifiy that there should be atleast 1 predictor but not specifying which one
PCS_null <- lm(SF36.PCS ~ 1, data = imputed.dataframe)


# STEPWISE BACKWARD AIC for PCS
PCS.step.back <- stepAIC(PCS,trace = T, direction = "backward", scope = list(upper=PCS, lower=PCS_null))
summary(PCS.step.back) # Full model is the best fit

# VIF to check for colinearity
vif(PCS.step.back)

#Graphs for linear regression
hist(resid(PCS.step.back))
plot(fitted(PCS.step.back),resid(PCS.step.back))

plot(imputed.dataframe$SF36.PCS,fitted(PCS.step.back))

qqnorm(resid(PCS.step.back))
qqline(resid(PCS.step.back), col=2)
#######


####### Mental health ########

# stochastic regression -> improvement from the above method "norm.nob" -> added some error +noise
data.lim.MCS <- dat2[, c("SF36.MCS", "ESS", "AIS","BSS")]
imp_MCS <- mice(data.lim.MCS, method = "norm.nob", seed = 11,
            m = 1, print = FALSE)
xyplot(imp_MCS, SF36.MCS ~ ESS + AIS + BSS) # Imputed values for ozone are not always the same, it depends on solar.

#Complete function extracts the imputed data 
imputed.dataframe.MCS <- complete(imp_MCS)
imputed.dataframe.MCS <- as.data.frame(imputed.dataframe.MCS)
dim(imputed.dataframe.MCS)
dim(dat2)


# Running the model with imputed dataframe
MCS <- lm(SF36.MCS ~ ESS + AIS + BSS, data = imputed.dataframe.MCS)
summary(MCS)

# Is there a way to specifiy that there should be atleast 1 predictor but not specifying which one
MCS_null <- lm(SF36.MCS ~ 1, data = imputed.dataframe.MCS)


# STEPWISE BACKWARD AIC for PCS
MCS.step.back <- stepAIC(MCS,trace = T, direction = "backward", scope = list(upper=MCS, lower=MCS_null))
summary(MCS.step.back) #  model without BSS is the best fit

# VIF to check for colinearity
vif(MCS.step.back)

#Graphs for linear regression
hist(resid(MCS.step.back))
plot(fitted(MCS.step.back),resid(MCS.step.back))

plot(imputed.dataframe.MCS$SF36.MCS,fitted(MCS.step.back))

qqnorm(resid(MCS.step.back))
qqline(resid(MCS.step.back), col=2)



