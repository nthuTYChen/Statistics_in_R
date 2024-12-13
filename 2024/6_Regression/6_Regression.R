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

# Multiple Regression: Test the effects of noun frequency and familiarity
# rating on the reaction times in the lexical decision task in the "english"
# data set. Check the Unit 6 handout for why these two factors are potentially
# important in a lexical decision task.
library(languageR)
head(english)

# We only focus on test items that are categorized as a noun in this data set,
# so we can test the genuine effect of noun frequency
english.sub = subset(english, WordCategory == "N")
nrow(english.sub)

# Include only columns with the crucial information we need here.
english.sub = english.sub[c("RTlexdec", "NounFrequency", "Familiarity", "Word")]
head(english.sub)

# The natural log of 0 would be negative infinity, which would cause problems
# when doing mathemetical calculations. In some occassions, a lexical entry
# might have a frequency of zero because it is not documented in a corpus.
log(0)

# Since raw frequencies would potentially have zeros, we can add a small number
# to all frequencies before log-transformation. Here we add 1, which is thus called
# "Add-1 smoothing".
english.sub$NounFreq.log = log(english.sub$NounFrequency + 1)

# Although frequency is highly correlated with familiarity (if a word is used
# more frequently, it is supposed to be a more familiar word), there are still
# some exceptions. Let's try to find the nouns that have a relatively lower
# frequency but a relatively higher familiarity.
freq.mean = mean(english.sub$NounFreq.log)
freq.sd = sd(english.sub$NounFreq.log)
fam.mean = mean(english.sub$Familiarity)
fam.sd = sd(english.sub$Familiarity)
# frequency < mean - 1SD & familiarity > mean + 1SD
english.sub.exc = subset(english.sub, NounFreq.log < freq.mean - freq.sd &
                           Familiarity > fam.mean + fam.sd)
head(english.sub.exc)

# Build a multiple regression with the predictors WITHOUT their interaction.
english.lm = lm(RTlexdec ~ NounFreq.log + Familiarity, data = english.sub)
# See the Unit 6 handout for the explanations of all statistics in the model.
summary(english.lm)

# Calculate the predict value of RTlexdec when NounFreq.log and Familiarity are
# both 1 using the linear equation.
6.809384 + -0.020492 * 1 + -0.040432 * 1

# To show the difference between the intercept and the above predicted RT in
# raw RT, you must convert the log RTs back to raw RTs before calculating the
# difference.
exp(6.809384) - exp(6.809384 + -0.020492 * 1 + -0.040432 * 1)

# Use predict() to calculate the predicted values with a data frame including
# the same predictors (the column names must be the same as the predictor names
# in the model) and the predictors' values.
english.new = data.frame(NounFreq.log = seq(1, 5), Familiarity = seq(3, 7))
english.new

# Calculate the predicted values of the dependent variable (RTlexdec) with 
# the regression model and the data framing containing the new values of the
# two predictors.
predict(object = english.lm, newdata = english.new)

# Just merge the predicted values with the data frame including the new values
# of the two predictors to make it easier to see everything together.
english.pred = predict(object = english.lm, newdata = english.new)
english.new$RT.pred = as.vector(english.pred)
english.new
head(english.sub)

# Compare the multiple regression model with the simple linear regression model
# including either predictor; the multiple regression model can explain 2-4%
# more variance in RTlexdec than the simple linear regression model, which is
# a significant improvement.
english.lm.freq = lm(RTlexdec ~ NounFreq.log, data = english.sub)
summary(english.lm.freq)
english.lm.fam = lm(RTlexdec ~ Familiarity, data = english.sub)
summary(english.lm.fam)

# There's a strong correlation between noun frequency and familiarity as expected,
# which leads to a potential linearity problem.
cor(english.sub$NounFreq.log, english.sub$Familiarity)

# Similulate the collinearity problem. Check the collinearity section of the
# Unit 6 handout for explanations.
set.seed(100)
x = rnorm(n = 50)
y = 10 + 3 * x + rnorm(n = 50)
summary(lm(y ~ x))

set.seed(200)
x2 = x - rnorm(50) * 0.1
cor(x, x2)

summary(lm(y ~ x2))

coll.lm = lm(y ~ x + x2)
summary(coll.lm)

# Use the vif() function of the "car" package to estimate the variance 
# inflation factor and the seriousness of the collinearity problem.
library(car)
# VIF is above 110 for the model simulating the collinearity problem, which
# is well above the more lenient threshold (i.e., 10) in the literature.
vif(coll.lm)

# VIF is below 3 for the real-world model, so the strong correlation between
# noun frequency and familiarity is not a real issue.
vif(english.lm)

# Multiple regression with interactions; see Unit 6 handout for why we expect
# an interaction between noun frequency and familiarity

# Build the multiple regression model with the two-way interaction
# As in ANOVA, NounFreq.log * Familiarity =  
#                     NounFreq.log + Familiarity + NounFreq.log:Familiarity
english.lm.int = lm(RTlexdec ~ NounFreq.log * Familiarity, data = english.sub)
# Check the Unit 6 handout for detailed explanations of the model summary.
summary(english.lm.int)

# Calculate the predicted RTlexdec when NounFreq.log is 2 and Familiarity is 3;
# the interaction term literally means to multiply the two predictors.
6.977674 + -0.057196 * 2 + -0.08433 * 3 + 0.00894 * 2 * 3

# Get the predicted values of RTlexdec with this new model with the interaction
# between two predictors.
predict(object = english.lm.int, newdata = english.new)

# Multiple linear regression when both predictors are categorical with two levels
# See the Unit 6 handout for an explanation of why we expect the duration of [n]
# in the ont- prefix is influenced simultaneously by the presence of the stop [t]
# and the gender of speakers.
head(durationsOnt)

# Convert the two categorical predictors into factors so they can serve as 
# predictors in our regression modeling. They are dummy-coded by default.
durationsOnt$Sex.Fac = as.factor(durationsOnt$Sex)
durationsOnt$Pl.Pr.Fac = as.factor(durationsOnt$PlosivePresent)

# Check the contrast table for the coding of the two predictors; in Sex.Fac,
# female = 0 = the reference level and in Pl.Pr.Fac, no = 0 = the reference
# level
contrasts(durationsOnt$Sex.Fac)
contrasts(durationsOnt$Pl.Pr.Fac)

# Build the multiple regression model. See the Unit 6 handout for the explanation
# of the statistics in the model.
dur.cat.lm = lm(DurationPrefixNasal ~ Pl.Pr.Fac * Sex.Fac, data = durationsOnt)
summary(dur.cat.lm)

dur.cat.lm2 = lm(DurationPrefixNasal ~ Sex.Fac * Pl.Pr.Fac, data = durationsOnt)
summary(dur.cat.lm2)

dur.cat.aov = aov(DurationPrefixNasal ~ Pl.Pr.Fac * Sex.Fac, data = durationsOnt)
dur.cat.aov2 = aov(DurationPrefixNasal ~ Sex.Fac * Pl.Pr.Fac, data = durationsOnt)
summary(dur.cat.aov)
summary(dur.cat.aov2)

anova(dur.cat.lm)

# 10 questions with two choices, 6 correct, 4 incorrect
binom.test(x = 6, n = 10, p = .5)

# 100 questions with two choices, 60 correct, 40 incorrect
binom.test(x = 60, n = 100, p = .5)

hits = 60
sample.n = 100
p1 = 60 / 100
odds.ratio = p1 / (1 - p1)
logit.p1 = log(odds.ratio)

odds.ratio.raw = exp(logit.p1)
p1.raw = odds.ratio.raw / (1 + odds.ratio.raw)
0.6 * sample.n

source("https://raw.githubusercontent.com/nthuTYChen/Statistics_in_R/main/courseUtil.R")
chen.sample = loadCourseCSV(2024, "6_Regression", "Chen2020Sample.csv")

chen.sample$InitialTone_Fac = as.factor(chen.sample$InitialTone)
chen.sample$Group_Fac = as.factor(chen.sample$Group)
contrasts(chen.sample$InitialTone_Fac) = contr.sum(2)
contrasts(chen.sample$Group_Fac) = contr.sum(2)

chen.glm = glm(Accept ~ Group_Fac * InitialTone_Fac, family = "binomial",
               data = chen.sample)
summary(chen.glm)

chen.p1.logit = 0.04788
chen.p1.odds.ratio = exp(chen.p1.logit)
chen.p1 = chen.p1.odds.ratio / (1 + chen.p1.odds.ratio)

mean(chen.sample$Accept)

mean(chen.sample[chen.sample$Group == "NonFinalH",]$Accept)
mean(chen.sample[chen.sample$Group == "NonFinalR",]$Accept)

aggregate(Accept ~ Group + InitialTone, FUN = mean, data = chen.sample)

exp(0.04788 + -1 * -0.06881 + 1 * 0.10035 + -0.23860 * 1 * -1)

1.577182 / (1 + 1.577182)

chen.glm2 = glm(Accept ~ Group_Fac + InitialTone_Fac, family = "binomial",
               data = chen.sample)
summary(chen.glm2)

TwSyllables = loadCourseCSV("2024", "7_Nonparametric", "TwSyllables.csv")
head(TwSyllables)

TwSyllables.xtab = xtabs(~ Tone, data = subset(TwSyllables, Tone != "T0"))
TwSyllables.xtab
sum(TwSyllables.xtab)

chisq.test(TwSyllables.xtab)

chisq.test(c(60, 40))