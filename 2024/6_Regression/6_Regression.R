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

# Follow the multinominal equation in (1) on p.2 to understand how simple
# regression work.

# Intercept is the predicted value of y (DurationPrefixNasal) when x (Frequency)
# is zero.
0.055275 + -0.002052 * 0

# The coeffecient of Frequency stands for the amount of change in y when Frequency
# increases by 1.
0.055275 + -0.002052 * 1

# Create a number sequence from 1 to 5
log.freq.seq = 1:5

# Calculate the predicted duration of the prefix nasal with the number sequence.
# Now you can see where the regression line in Figure 1 comes from.
0.055275 + -0.002052 * log.freq.seq

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

# Get the residuals in the full model (the differences between observed values 
# and their predicted values in the model)
model.res = dur.lm$residuals
# Calculate SSE (sum of squared errors)
model.sse = sum(model.res ^ 2)
model.sse

# Get the residuals in the intercept-only model (the difference between 
# observed values and their mean)
null.res = dur.lm.int$residuals
# Calculate SSE
null.sse = sum(null.res ^ 2)
null.sse

# r2; only 2.4% of the random variance is explained in the model taking
# word frequency into consideration
1 - model.sse / null.sse

# Check if the residuals of a model is normally distributed, which is the
# fundamental assumption in regression.
dur.res = as.vector(dur.lm$residuals)
qqnorm(dur.res, main = "Q-Q Plot of the residuals of dur.lm")
qqline(dur.res, col = "red", lwd = 2)

# The p-value of each coefficient in linear regression modeling is essentially a 
# one-sample t-test assuming an equal variance and a population mean of 0.

# Validate the p-value of the Frequency effect
(-0.002052 - 0) / 0.001298

pt(q = -1.581, df = 100) * 2

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

# This part is not covered in the Unit 6 handout. Use SpeechRate to predict
# the change in the prefix nasal duration; the faster the speech rate, the shorter
# the prefix nasal duration.

# z-scoring speech rates for a standardized coefficient because on the original
# scale, x = 0 does not make sense for speech rate (0 speech rate = total silence?)
sr = durationsOnt$SpeechRate
durationsOnt$sr.z = (sr - mean(sr)) / sd(sr)

# The effect of speech rate is indeed significant; faster = shorter.
dur.sr.lm.z = lm(formula = DurPrefixN.z ~ sr.z, data = durationsOnt)
summary(dur.sr.lm.z)

# Moving to linear regression with a categorical predictor (i.e., an independent
# variable with a fixed number of levels)

# PlosivePresent: Whether [t] is produced in the -ont prefix. The prediction
# is that when [t] is not produced, there is more room for the production of [n]
# in the prefix, so [n] would be longer.

# About one-fourth of the ont- words do not have [t] in their production.
table(durationsOnt$PlosivePresent)

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
mean(durationsOnt[durationsOnt$Pl.Pr.Fac == "no",]$DurationPrefixNasal)
mean(durationsOnt[durationsOnt$Pl.Pr.Fac == "yes",]$DurationPrefixNasal)

# The difference between the two means is also identical to the coefficient
# of the linear regression model.
0.04285096 - 0.06822025

# The "linear regression" including a categorical predictor with two levels
# is conceptually and mathematically associated to an unpaired two-sample
# t-test assuming an equal variance. The latter is in fact the special form of
# linear regression. Pay attention to the means and the t/p-value.
t.test(formula = DurationPrefixNasal ~ PlosivePresent, var.equal = T,
       data = durationsOnt)

# Replace the default dummy coding with sum coding using the contr.sum()
# function. Check the Unit 6 handout on pp.10-11 for detailed explanations.
durationsOnt$Pl.Pr.Fac.Sum = as.factor(durationsOnt$PlosivePresent)
contrasts(durationsOnt$Pl.Pr.Fac.Sum) = contr.sum(2)
contrasts(durationsOnt$Pl.Pr.Fac.Sum)

# Create another model using the sum-coded predictor; the explanatory power
# remains the same, but the intercept no longer represents either level of the
# predictor.
dur.lm.plo.sum = lm(formula = DurationPrefixNasal ~ Pl.Pr.Fac.Sum,
                    data = durationsOnt)
summary(dur.lm.plo.sum)

# Run an one-way independent-measures ANOVA to show that ANOVA is a special
# form of linear regression, too. Pay attention to the F-value and the p-value.
dur.pl.aov = aov(formula = DurationPrefixNasal ~ PlosivePresent,
                 data = durationsOnt)
summary(dur.pl.aov)
