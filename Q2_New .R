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


