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

# Replace the default dummy coding with sum coding using the contr.sum()
# function and created a new variable Pl.Pr.Fac.sum. 
# Check the Unit 6 handout on pp.10-11 for detailed explanations.
durationsOnt$Pl.Pr.Fac.sum = as.factor(durationsOnt$PlosivePresent)
contrasts(durationsOnt$Pl.Pr.Fac.sum) = contr.sum(2)
contrasts(durationsOnt$Pl.Pr.Fac.sum)
# Compare the sum-coding system with the dummy coding system
contrasts(durationsOnt$Pl.Pr.Fac)

# Create another model using the sum-coded predictor; the explanatory power
# remains the same, but the intercept no longer represents either level of the
# predictor.
dur.lm.plo.sum = lm(formula = DurationPrefixNasal ~ Pl.Pr.Fac.sum, 
                    data = durationsOnt)
summary(dur.lm.plo.sum)

# The "linear regression" including a categorical predictor with two levels
# is conceptually and mathematically associated to an unpaired two-sample
# t-test assuming an equal variance. The latter is in fact the special form of
# linear regression. Pay attention to the means and the t/p-value.
t.test(DurationPrefixNasal ~ PlosivePresent, var = TRUE, data = durationsOnt)

# Run an one-way independent-measures ANOVA to show that ANOVA is a special
# form of linear regression, too. Pay attention to the F-value and the p-value.
dur.lm.aov = aov(DurationPrefixNasal ~ PlosivePresent, data = durationsOnt)
summary(dur.lm.aov)

# Multiple Regression: Test the effects of noun frequency and familiarity
# rating on the reaction times in the lexical decision task in the "english"
# data set. Check the Unit 6 handout for why these two factors are potentially
library(languageR)
head(english)

# We only focus on test items that are categorized as a noun in this data set,
# so we can test the genuine effect of noun frequency
english.sub = subset(english, WordCategory == "N")
nrow(english.sub)

# Include only columns with the crucial information we need here.
english.sub = english.sub[c("RTlexdec", "NounFrequency", "Familiarity", "Word")]
head(english.sub)

# Check the range of our raw noun frequency to make sure that the minimal value
# is not zero before log transformation, because log(0) = -infinity, which
# causes some technical problems.
range(english.sub$NounFrequency)

# There is no zero in the raw noun frequencies, so just do log transformation
# directly.
english.sub$NounFreq.log = log(english.sub$NounFrequency)
head(english.sub)

# Although frequency is highly correlated with familiarity (if a word is used
# more frequently, it is supposed to be a more familiar word), there are still
# some exceptions. Let's try to find the nouns that have a relatively lower
# frequency but a relatively higher familiarity.
freq.mean = mean(english.sub$NounFreq.log)
freq.sd = sd(english.sub$NounFreq.log)
fam.mean = mean(english.sub$Familiarity)
fam.sd = sd(english.sub$Familiarity)
# frequency < mean - 1SD & familiarity > mean + 1SD
tail(subset(english.sub, NounFreq.log < freq.mean - freq.sd &
              Familiarity > fam.mean + fam.sd))

# Build a multiple regression with the predictors WITHOUT their interaction.
english.lm = lm(RTlexdec ~ NounFreq.log + Familiarity, data = english.sub)
summary(english.lm)

# Build a multiple regression with only log noun frequency as the only predictor.
# This model only explains 18.7% of the variance in the dependent variable,
# which is about 4% less than the model with both predictors.
english.lm.freq = lm(RTlexdec ~ NounFreq.log, data = english.sub)
summary(english.lm.freq)

# There's a strong correlation between noun frequency and familiarity as expected,
# which leads to a potential linearity problem.
cor(english.sub$NounFreq.log, english.sub$Familiarity)

# Similulate the collinearity problem. Check the collinearity section of the
# Unit 6 handout for explanations.
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
# Use the vif() function of the "car" package to estimate the variance 
# inflation factor and the seriousness of the collinearity problem.
library(car)
# VIF is above 110 for the model simulating the collinearity problem, which
# is well above the more lenient threshold (i.e., 10) in the literature.
vif(coll.lm)

# VIF is below 3 for the real-world model, so the strong correlation between
# noun frequency and familiarity is not a real issue.
vif(english.lm)
# The low VIF despite a high correlation between frequency and familiarity is
# in part due to a large sample size. Regression modeling is powerful enough
# to tease apart the independent effect of highly correlated predictors.
nrow(english.sub)

# Multiple regression with interactions; see Unit 6 handout for why we expect
# an interaction between noun frequency and familiarity

# Build the multiple regression model with the two-way interaction
# As in ANOVA, NounFreq.log * Familiarity =  
#                     NounFreq.log + Familiarity + NounFreq.log:Familiarity
english.lm.int = lm(RTlexdec ~ NounFreq.log * Familiarity, data = english.sub)
# Check the Unit 6 handout for detailed explanations of the model summary.
summary(english.lm.int)

# Multiple linear regression when both predictors are categorical with two levels
# See the Unit 6 handout for an explanation of why we expect the duration of [n]
# in the ont- prefix is influenced simultaneously by the presence of the stop [t]
# and the gender of speakers.
library(languageR)
head(durationsOnt)

# Convert the two categorical predictors into factors so they can serve as 
# predictors in our regression modeling. They are dummy-coded by default.
durationsOnt$Sex.Fac = as.factor(durationsOnt$Sex)
durationsOnt$Pl.Pr.Fac = as.factor(durationsOnt$PlosivePresent)
# Check the contrast table for the coding of the two predictors; in Sex.Fac,
# female = 0 = the reference level and in Pl.Pr.Fac, no = 0 = the reference
# levelcontrasts(durationsOnt$Sex.Fac)
contrasts(durationsOnt$Pl.Pr.Fac)
# Build the multiple regression model. See the Unit 6 handout for the explanation
# of the statistics in the model.
dur.cat.lm = lm(DurationPrefixNasal ~ Pl.Pr.Fac * Sex.Fac, data = durationsOnt)
summary(dur.cat.lm)
# The same regression modeling, but with the sum-coding system.
contrasts(durationsOnt$Sex.Fac) = contr.sum(2)
contrasts(durationsOnt$Pl.Pr.Fac) = contr.sum(2)
contrasts(durationsOnt$Sex.Fac)
contrasts(durationsOnt$Pl.Pr.Fac)

dur.cat.sum = lm(DurationPrefixNasal ~ Pl.Pr.Fac * Sex.Fac, data = durationsOnt)
summary(dur.cat.sum)

# Multiple regression with categorical predictors is just like to compare 
# different sample means...
aggregate(DurationPrefixNasal ~ Pl.Pr.Fac * Sex.Fac, 
          FUN = mean, data = durationsOnt)

# The distribution of data points is slightly unbalanced.
xtabs(~ Pl.Pr.Fac + Sex.Fac, data = durationsOnt)
# Try to demonstrate that the order of predictors does not matter in regression
# modeling, unlike in ANOVA. The estimates in regression are calculated all at 
# once, but the variances are partitioned following the order of independent
# variables in Type I ANOVA.
dur.cat.lm.revord = lm(DurationPrefixNasal ~ Sex.Fac * Pl.Pr.Fac, 
                       data = durationsOnt)
summary(dur.cat.lm.revord)

# Revisit the sample data set of Chen's (2020) artificial grammar learning
# experiment to demonstrate logistic regression modeling on the binary
# acceptance rate of test items by learners in different learning conditions.
source("https://raw.githubusercontent.com/nthuTYChen/Statistics_in_R/refs/heads/main/courseUtil.R")
chen.sample = loadCourseCSV(2025, "6_Regression", "Chen2020Sample.csv")
head(chen.sample)

# In this script, we simply use the dummy-coding system. For the results
# of logistic regression modeling using the sum-coding system, see the Unit 6
# handout. There's no critical differences, except that the interpretation of
# the intercept and the slopes is a bit different.
chen.glm = glm(Accept ~ InitialTone * Group, 
               family = "binomial", data = chen.sample)
summary(chen.glm)

# logit = ln(p1 / (1 - p1)), and p1 = exp(logit / (1 + logit))
exp(-0.15919)
exp(-0.15919) / (1 + exp(-0.15919))

# Again, multiple regression with categorical predictors is just like to compare 
# different sample mean probabilities, if you try to convert all the logits
# back to p1...
aggregate(Accept ~ InitialTone + Group, FUN = mean, chen.sample)