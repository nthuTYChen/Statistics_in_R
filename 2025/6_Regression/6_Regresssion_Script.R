# Revisit the durationsOnt data set
library(languageR)
head(durationsOnt)

# Review the results of the Pearson's correlation test for the correlation
# between log-Frequency and the duration of the ont- prefix.
# There's a negative r, but it is not significantly different from 0.
cor.test(durationsOnt$Frequency, durationsOnt$DurationOfPrefix)

# Focus on the Pearson's correlation test between log-frequency and the
# duration of [n] in the ont- prefix.
# There's a "trending" negative correlation (p = .11), which matches Figure 1
# on p.2 of the Unit 6 handout.
cor.test(durationsOnt$Frequency, durationsOnt$DurationPrefixNasal)

# Pearson's correlation tests do not account for cause-effect relations and 
# cannot make predictions, so let's move on the simple linear regression.
dur.lm = lm(formula = DurationPrefixNasal ~ Frequency, data = durationsOnt)
# Get the output of the report and focus on the intercept and the main effect.
summary(dur.lm)

# The same model that explictly states that the intercept (1) is included.
dur.lm2 = lm(formula = DurationPrefixNasal ~ 1 + Frequency, data = durationsOnt)
summary(dur.lm2)

# Validate the t value of the intercept by dividing the beta coefficient
# by the SE
0.055275 / 0.004042

# Use predict() to calculate the predicted values with a data frame including
# the same predictors (the column names must be the same as the predictor names
# in the model) and the predictors' values.

# Create a sequence from 0 to 2 with an interval of 0.1 between every two values
# for the Frequency variable.
freq.seq = seq(from = 0, to = 2, by = 0.1)
head(freq.seq)

# Create the data frame with the same variable name "Frequency"
new.df = data.frame(Frequency = freq.seq)
head(new.df)

# Use the data frame with the new Frequency values to get the corresponding
# values from the linear regression model dur.lm.
predict(dur.lm, newdata = new.df)

# Convert the table output of predict() into a vector, and store the vector
# to the "Predict" variable of the data frame so it is easier to compare
# each "x" value with the predicted value.
new.df$Predict = as.vector(predict(dur.lm, newdata = new.df))
head(new.df)

# Build an intercept-only model, which serves as the baseline for comparison.
# See the Unit 6 handout (pp.4-7) for detailed explanations.
dur.lm.int = lm(DurationPrefixNasal ~ 1, data = durationsOnt)
summary(dur.lm.int)

# The intercept in the intercept-only model is the mean of the dependent 
# variable y: The squared differences between the mean and the individual
# data points also represent the random variance (SSE) when no explanatory
# factors (independent variables) are taken into consideration.
mean(durationsOnt$DurationPrefixNasal)

# Validate r2 (the proportion of SSE in the intercept-only (null) model explained
# in the full model taking some explanatory factors into consideration)

# Get the residuals in the intercept-only model (the difference between 
# observed values and their mean)
null.res = dur.lm.int$residuals
# Calculate SSE (sum of squared errors)
null.sse = sum(null.res ^ 2)
null.sse

# Get the residuals in the full model (the differences between observed values 
# and their predicted values in the model)
model.res = dur.lm$residuals
# Calculate SSE (sum of squared errors)
model.sse = sum(model.res ^ 2)
model.sse

# r2; only 2.4% of the random variance is explained in the model taking
# word frequency into consideration
1 - model.sse / null.sse

# Check if the residuals of the full model is normally distributed, which is the
# fundamental assumption in regression.
qqnorm(model.res, main = "Q-Q Plot of dur.lm Residuals")
qqline(model.res, col = "red", lwd = 2)

# Linear regression with standardized coefficients; see pp.6-7 for detailed
# explanation.

# Convert both dependent and independent variables into z-scores
nas.dur = durationsOnt$DurationPrefixNasal
durationsOnt$DurPrefixN.z = (nas.dur - mean(nas.dur)) / sd(nas.dur)
freq = durationsOnt$Frequency
durationsOnt$Freq.z = (freq - mean(freq)) / sd(freq)

# The model with standard coefficient has the same explanatory power (check r2
# and the t/p-value of the predictor).
dur.lm.z = lm(formula = DurPrefixN.z ~ Freq.z, data = durationsOnt)
summary(dur.lm.z)

# Check the number of observations with and without [t] in the ont- prefix
xtabs(~ PlosivePresent, data = durationsOnt)

# Convert PlosivePresent into a factor and use the default dummy coding system
# to code 'no' as 0 and 'yes' as 1. See pp.7-11 in the Unit 6 handout for the 
# most detailed explanation of why we do everything below.
durationsOnt$Pl.Pr.Fac = as.factor(durationsOnt$PlosivePresent)
# Show the contrast table
contrasts(durationsOnt$Pl.Pr.Fac)
# Build the model
dur.lm.plo = lm(DurationPrefixNasal ~ Pl.Pr.Fac, data = durationsOnt)
summary(dur.lm.plo)
# Calculate the means of DurationPrefixNasal in the two subsets divided by
# DurationPrefixNasal. The means are identical to the predicted values in
# the linear regression model.
mean(durationsOnt[durationsOnt$PlosivePresent == "yes",]$DurationPrefixNasal)
mean(durationsOnt[durationsOnt$PlosivePresent == "no",]$DurationPrefixNasal)

durationsOnt$Pl.Pr.Fac.sum = as.factor(durationsOnt$PlosivePresent)
contrasts(durationsOnt$Pl.Pr.Fac.sum) = contr.sum(2)
contrasts(durationsOnt$Pl.Pr.Fac.sum)

contrasts(durationsOnt$Pl.Pr.Fac)

dur.lm.plo.sum = lm(formula = DurationPrefixNasal ~ Pl.Pr.Fac.sum, 
                    data = durationsOnt)
summary(dur.lm.plo.sum)

t.test(DurationPrefixNasal ~ PlosivePresent, var = TRUE, data = durationsOnt)

dur.lm.aov = aov(DurationPrefixNasal ~ PlosivePresent, data = durationsOnt)
summary(dur.lm.aov)

head(english)
english.sub = subset(english, WordCategory == "N")
nrow(english.sub)

english.sub = english.sub[c("RTlexdec", "NounFrequency", "Familiarity", "Word")]
head(english.sub)

range(english.sub$NounFrequency)

english.sub$NounFreq.log = log(english.sub$NounFrequency)
head(english.sub)

freq.mean = mean(english.sub$NounFreq.log)
freq.sd = sd(english.sub$NounFreq.log)
fam.mean = mean(english.sub$Familiarity)
fam.sd = sd(english.sub$Familiarity)

tail(subset(english.sub, NounFreq.log < freq.mean - freq.sd &
              Familiarity > fam.mean + fam.sd))

english.lm = lm(RTlexdec ~ NounFreq.log + Familiarity, data = english.sub)
summary(english.lm)

english.lm.freq = lm(RTlexdec ~ NounFreq.log, data = english.sub)
summary(english.lm.freq)

cor(english.sub$NounFreq.log, english.sub$Familiarity)

set.seed(seed = 100)
x = rnorm(n = 50)
y = 10 + 3 * x + rnorm(n = 50)
summary(lm(y ~ x))

set.seed(seed = 200)
x2 = x - rnorm(50) * 0.1
cor(x, x2)

summary(lm(y ~ x2))

coll.lm = lm(y ~ x + x2)
summary(coll.lm)

install.packages("car", ask = F, dependencies = T)
library(car)

vif(coll.lm)

vif(english.lm)
nrow(english.sub)

english.lm.int = lm(RTlexdec ~ NounFreq.log * Familiarity, data = english.sub)
summary(english.lm.int)