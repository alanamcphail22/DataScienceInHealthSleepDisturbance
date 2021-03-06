### Data cleaning
library(tidyr)
library(dplyr)
library(car)
library(MASS)
library(mice)


# Loading dataset
dat <- read.csv("project_data.csv")
dim(dat) #268 rows

# Finding a pattern in missingnes


dat <- dat[c("Age", "Gender", "BMI","Time.from.transplant","Liver.Diagnosis", "Recurrence.of.disease",
             "Rejection.graft.dysfunction", "Any.fibrosis", "Renal.Failure", "Depression","Corticoid",
             "Epworth.Sleepiness.Scale", "Athens.Insomnia.Scale", "Berlin.Sleepiness.Scale", "SF36.PCS", "SF36.MCS")]

dat$Berlin.Sleepiness.Scale <- as.logical(dat$Berlin.Sleepiness.Scale)

data_missing_ESS <- dat %>%
  filter(is.na(Epworth.Sleepiness.Scale))

data_missing_AIS <- dat %>%
  filter(is.na(Athens.Insomnia.Scale))

data_missing_BSS <- dat %>%
  filter(is.na(Berlin.Sleepiness.Scale))

# IMPUTATIONS FOR MISSING VALUES

# more conveniently using mice
# stochastic regression -> improvement from the above method "norm.nob" -> added some error +noise
data.lim.PCS <- dat[, c("SF36.PCS", "Epworth.Sleepiness.Scale", "Athens.Insomnia.Scale","Berlin.Sleepiness.Scale")]
imp <- mice(data.lim.PCS, method = "norm.nob", seed = 11,
            m = 1, print = FALSE)
xyplot(imp, SF36.PCS ~ Epworth.Sleepiness.Scale + Athens.Insomnia.Scale + Berlin.Sleepiness.Scale) # Imputed values for ozone are not always the same, it depends on solar.

fit.stoch <- with(imp, lm(SF36.PCS ~ Epworth.Sleepiness.Scale + Athens.Insomnia.Scale + Berlin.Sleepiness.Scale))
summary(pool(fit.stoch))

# this is how we can extract the actual imputed dataset
imputed.data.frame <- complete(imp, action = 1)



# Renaming variables and removing NA's 
# PSGQ NA: 85 <- more than 30% of data is missing (NA > 80)
# AIS: 6
# ESS: 17
# BSS: 6
apply(is.na(dat),2,sum)
dat2 <- dat %>% 
  rename(ESS = Epworth.Sleepiness.Scale, 
         AIS = Athens.Insomnia.Scale, BSS = Berlin.Sleepiness.Scale) 

# Filtering for the response and predictors we need (plus unique identifier)
data_Q2 <- dat2[c("Subject","ESS", "AIS", "BSS", "SF36.PCS", "SF36.MCS")]

# Changing BSS to a logical factor.
data_Q2$BSS <- as.logical(data_Q2$BSS)

########################################################

# Q2: Building a multiple linear regression

# Predictor: Sleep quality (all 4 measurements)
# Response: MCS and PCS.

# Check if they are very colinear (predictors)
# Even if they are separate instruments: each measures different aspect with sleep problems
# Practical reasons as to why you might wanna remove some of them 
# Identify the relationship between sleep disturbance and quality of life (physical and mental).
# Multiple ways of analyzing this?

####### Physical health ########

# Complex model with all 4 sleep predictors
PCS <- lm(SF36.PCS ~ ESS + AIS + BSS, data = data_Q2)

summary(PCS)
hist(resid(PCS))
plot(fitted(PCS),resid(PCS))

plot(data_Q2$SF36.PCS,fitted(PCS))

qqnorm(resid(PCS))
qqline(resid(PCS), col=2)


PCS.step.back <- stepAIC(PCS,trace = F)
summary(PCS.step.back)
# Testing for collinearity using VIF: Guidance on reference levels:
# 1 = not correlated.
# Between 1 and 5 = moderately correlated.
# Greater than 5 = highly correlated.

# PCS: PSGQ and AIS are moderately correlated. We can remove them if u want. B
# Future Note: down below when we do VIF testing of AIS and ESS they are not correlated. 
# So I think we should keep the significant predictors and remove the insignificant ones.
vif(PCS)



# Simpler model with only ESS and AIS -- as BSS and PSGQ were not siginificant in complex model.
PCS_simple <- lm(SF36.PCS ~ ESS + AIS, data = data_Q2)

summary(PCS_simple)
round(confint(PCS_simple), 2)

hist(resid(PCS_simple))
plot(fitted(PCS_simple),resid(PCS_simple))

plot(data_Q2$SF36.PCS,fitted(PCS_simple))

qqnorm(resid(PCS_simple))
qqline(resid(PCS_simple), col=2)

# Testing fro collinearity - VIF
# AIS and ESS are pretty much not correlated - this model looks better.
vif(PCS_simple)

# ANOVA of Chisq goodness of fit. 
anova(PCS, PCS_simple, test = "Chisq") # p > 0.05, null hypothesis cannot be rejected, can use simpler model.

# Estimates of the predictors and their 95% CI


####### Mental health ########

# Complex model with all 4 sleep predictors
MCS <- lm(SF36.MCS ~ ESS + AIS + BSS, data = data_Q2)

summary(MCS)
hist(resid(MCS))
plot(fitted(MCS),resid(MCS))

plot(data_Q2$SF36.MCS,fitted(MCS))

qqnorm(resid(MCS))
qqline(resid(MCS), col=2)


MCS.step.back <- stepAIC(MCS,trace = F)
summary(MCS.step.back)
# Testing for co-linearity using VIF - PSGQ and AIS are moderately correlated.
vif(MCS)


#Future note: For the simpler model, I dont think it is wise to use PSGQ and AIS as the predictors
# Since they are moderately correlated.

# Simpler model with only PSGQ  as BSS, ESS and AIS were not significant in complex model.
MCS_simple <- lm(SF36.MCS ~ AIS, data = data_Q2)
summary(MCS_simple)
round(confint(MCS_simple), 2)

summary(MCS_simple)
hist(resid(MCS_simple))
plot(fitted(MCS_simple),resid(MCS_simple))

plot(data_Q2$SF36.MCS,fitted(MCS_simple))

qqnorm(resid(MCS_simple))
qqline(resid(MCS_simple), col=2)
ggeffects::ggeffect(MCS_simple)


# Testing for co-linearity using VIF - w/ PSGQ (cant do it cuz theres only 1 predictor lol) .
vif(MCS_simple)


# ANOVA of Chisq goodness of fit. 
anova(MCS, MCS_simple, test = "Chisq") # p > 0.05, null hypothesis cannot be rejected, can use simpler model.
###########

MCS_1 <- lm(SF36.MCS ~ ESS + PSGQ + AIS, data = data_Q2)
summary(MCS_1)
anova(MCS, MCS_1)

MCS_2 <- lm(SF36.MCS ~ PSGQ + AIS, data = data_Q2)
summary(MCS_2)
cor(data_Q2$AIS, data_Q2$PSGQ)


######################################
# DOING ANALYSIS WHILE KEEPING MISSINGNESS IN MIND.

