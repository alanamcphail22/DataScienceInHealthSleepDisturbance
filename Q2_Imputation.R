### Data cleaning
library(tidyr)
library(dplyr)
library(car)
library(MASS)
library(mice)


# Loading data set
dat <- read.csv("project_data.csv")
summary(dat)
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




# Changing Variables to a appropriate type - BSS is logical
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
# Tentative Predictors: Sleep quality (all 4 measurements)
# Response: MCS and PCS.


# IMPUTATIONS FOR MISSING VALUES in PCS 

# stochastic regression -> added some noise
data.lim.PCS <- dat2[, c("SF36.PCS", "ESS", "AIS","BSS")]
imp <- mice(data.lim.PCS, method = "norm.nob", seed = 11,
            m = 1, print = FALSE)
xyplot(imp, SF36.PCS ~ ESS + AIS + BSS) 

#Complete function extracts the imputed data 
imputed.dataframe <- complete(imp)
imputed.dataframe <- as.data.frame(imputed.dataframe)
dim(imputed.dataframe)
dim(dat2)

# Making sure imputed points of BSS are either 1 or 0 and AIS and ESS dont have negative inputs
imputed.dataframe <- imputed.dataframe %>%
  mutate(BSS = ifelse(BSS > 1, 1, 0)) 

imputed.dataframe$AIS[imputed.dataframe$AIS < 0] <- 0 
imputed.dataframe$ESS[imputed.dataframe$ESS < 0] <-0




# Changing Variables to a appropriate type - BSS is logical
imputed.dataframe$BSS <- as.logical(imputed.dataframe$BSS)
imputed.dataframe$SF36.PCS <- as.numeric(imputed.dataframe$SF36.PCS)
imputed.dataframe$ESS <- as.numeric(imputed.dataframe$ESS)
imputed.dataframe$AIS <- as.numeric(imputed.dataframe$AIS)



# Running the model with imputed dataframe (full model)
PCS <- lm(SF36.PCS ~ ESS + AIS + BSS, data = imputed.dataframe)
summary(PCS)

# VIF to check for colinearity - no colinearity of concern, all below 5
vif(PCS)

# Null model with 0 predictors
PCS_null <- lm(SF36.PCS ~ 1, data = imputed.dataframe)


# STEPWISE BACKWARD AIC for PCS
PCS.step.back <- stepAIC(PCS,trace = T, direction = "backward", scope = list(upper=PCS, lower=PCS_null))
summary(PCS.step.back) #  model without BSS is the best fit

#Confidence interval for PCS
round(confint(PCS.step.back), 2)


# VIF to check for co linearity again
vif(PCS.step.back)

#Graphs for linear regression
hist(resid(PCS.step.back))

plot(fitted(PCS.step.back),resid(PCS.step.back))

qqnorm(resid(PCS.step.back))
qqline(resid(PCS.step.back), col=2)

cor(imputed.dataframe)

#######


####### Mental health ########

# stochastic regression -> added some noise
data.lim.MCS <- dat2[, c("SF36.MCS", "ESS", "AIS","BSS")]
imp_MCS <- mice(data.lim.MCS, method = "norm.nob", seed = 11,
            m = 1, print = FALSE)
xyplot(imp_MCS, SF36.MCS ~ ESS + AIS + BSS) 

#Complete function extracts the imputed data 
imputed.dataframe.MCS <- complete(imp_MCS)
imputed.dataframe.MCS <- as.data.frame(imputed.dataframe.MCS)
dim(imputed.dataframe.MCS)
dim(dat2)

# Making sure imputed points of BSS are either 1 or 0 and AIS and ESS dont have negative inputs
imputed.dataframe <- imputed.dataframe %>%
  mutate(BSS = ifelse(BSS > 1, 1, 0)) 

imputed.dataframe$AIS[imputed.dataframe$AIS < 0] <- 0 
imputed.dataframe$ESS[imputed.dataframe$ESS < 0] <-0


# Changing Variables to a appropriate type - BSS is logical
imputed.dataframe.MCS$BSS <- as.logical(imputed.dataframe.MCS$BSS)
imputed.dataframe.MCS$SF36.MCS <- as.numeric(imputed.dataframe.MCS$SF36.MCS)
imputed.dataframe.MCS$ESS <- as.numeric(imputed.dataframe.MCS$ESS)
imputed.dataframe.MCS$AIS <- as.numeric(imputed.dataframe.MCS$AIS)


# Running the model with imputed dataframe
MCS <- lm(SF36.MCS ~ ESS + AIS + BSS, data = imputed.dataframe.MCS)
summary(MCS)

# VIF to check for colinearity - all values below 5
vif(MCS)

# Null model with 0 predictors
MCS_null <- lm(SF36.MCS ~ 1, data = imputed.dataframe.MCS)


# STEPWISE BACKWARD AIC for MCS
MCS.step.back <- stepAIC(MCS,trace = T, direction = "backward", scope = list(upper=MCS, lower=MCS_null))
summary(MCS.step.back) #  model without BSS is the best fit

# Including confidence intervals 
round(confint(MCS.step.back), 2)

# VIF to check for colinearity
vif(MCS.step.back)

#Graphs for linear regression
hist(resid(MCS.step.back))
plot(fitted(MCS.step.back),resid(MCS.step.back))

qqnorm(resid(MCS.step.back))
qqline(resid(MCS.step.back), col=2)

cor(imputed.dataframe.MCS)

####################################################
# Added variable plots for models SF36.PCS and SF36.MCS

#PCS plots
avPlots(PCS.step.back, layout = c(1,2), main = "Relationship of Physical health and Sleep disturbance"
        , id = FALSE, xlim=c(0,26), xaxs = "i", )



#MCS plots
avPlots(MCS.step.back, main = "Relationship of Mental health and Sleep disturbance", 
        id = FALSE, xlim=c(0,26), xaxs = "i")


