library(mice)
library(dplyr)
library(ISLR)
library(MASS)
library(car)

dat <- read.csv("project_data.csv")

# more conveniently using mice
# stochastic regression -> improvement from the above method "norm.nob" -> added some error +noise
dat2 <- dat[, c("Epworth.Sleepiness.Scale", "Athens.Insomnia.Scale","Berlin.Sleepiness.Scale", "Age",
                'BMI', 'Time.from.transplant', 'Gender', 'Rejection.graft.dysfunction',
                  'Any.fibrosis', 'Corticoid', 'Liver.Diagnosis', 'Recurrence.of.disease', 'Renal.Failure', 
                  'Depression')]
imp <- mice(dat2, method = "norm.nob", seed = 11, m = 1, print = FALSE)
xyplot(imp, Epworth.Sleepiness.Scale ~ Age + BMI + Time.from.transplant + Gender + Rejection.graft.dysfunction +
         Any.fibrosis + Corticoid + Liver.Diagnosis + Recurrence.of.disease + Renal.Failure+
         Depression)
#xyplot(imp, SF36.PCS ~ Epworth.Sleepiness.Scale + Athens.Insomnia.Scale + Berlin.Sleepiness.Scale) # Imputed values for ozone are not always the same, it depends on solar.

fit.stoch <- with(imp, lm(Epworth.Sleepiness.Scale + Athens.Insomnia.Scale + Berlin.Sleepiness.Scale))
summary(pool(fit.stoch))

# this is how we can extract the actual imputed dataset
imputed.data.frame <- complete(imp, action = 1)

imputed.data.frame
