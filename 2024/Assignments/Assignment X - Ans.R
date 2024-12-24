# Preparation work
library(languageR)
head(english)

# Task 1
# Add-1 smoothing
english$VerbFrequency = english$VerbFrequency + 1

# Task 2
# Verb frequency is the raw frequency, so log-transformation is necessary
# to make its distribution normal-like.
english$VFreq.log = log(english$VerbFrequency)

# RTnaming and FrequencyInitialDiphoneWord are already log-transformed according 
# to the official document, so no log-transformation is necessary.

# Convert all three variables into z-scores, so you can remove observations
# with a z-score beyond mean+-2SD or mean+-2SD for each variable one by one

# z-scoring each of the three variable
english$RTnaming.z = scale(english$RTnaming)
english$VFreq.log.z = scale(english$VerbFrequency)
english$FreqDiWord.z = scale(english$FrequencyInitialDiphoneWord)

# Include observations with a z-score within the range -2 >= z <= 2 in each
# of the three variables
english.sub = subset(english, RTnaming.z >= -2 & RTnaming.z <= 2)
english.sub = subset(english.sub, VFreq.log.z >= -2 & VFreq.log.z <= 2)
english.sub = subset(english.sub, FreqDiWord.z >= -2 & FreqDiWord.z <= 2)

# Check if all three raw log-transformed variables are normally distributed using
# the QQ-Plot
par(mfrow = c(1, 3))

qqnorm(english.sub$RTnaming)
qqline(english.sub$RTnaming, col = "red")

qqnorm(english.sub$VFreq.log)
qqline(english.sub$VFreq.log, col = "red")

qqnorm(english.sub$FrequencyInitialDiphoneWord)
qqline(english.sub$FrequencyInitialDiphoneWord, col = "red")

par(mfrow = c(1, 1))
# RTnaming is still not that normally distributed. A different transformation
# might be required; see footnote 7 on p.17 of the Unit 3 handout.
# But that's the best you can get with log-transformation.

# Task 3
# Multiple regression without interaction; note that you can build your model
# using z-scored or log-transformed variables, but the interpretation of the
# stats in the model would be different. Here I only demonstrate the one
# with the log-transformed variables.
english.lm = lm(RTlexdec ~ VFreq.log + FrequencyInitialDiphoneWord, data = english.sub)
summary(english.lm)

# The verb frequency has a significant negative effect on naming latency 
# (p < .001); when verb frequency increases by 1 on the log-scale, the log 
# naming latency decreases by 0.0135.

# The diphone frequency does NOT have a significant negative effect on naming latency 
# (p = .099); when diphone frequency increases by 1 on the log-scale, the log 
# naming latency only increases by 0.003.

# Only the verb frequency effect is consistent with the hypothesis, as a higher
# log verb frequency predicts a shorter naming latency.

# The performance of the model is poor, since it only explains about 5.2% of the
# variance in the naming latency.

# Task 4
# A model with the two-way interaction
english.lm2 = lm(RTlexdec ~ VFreq.log * FrequencyInitialDiphoneWord, data = english.sub)
summary(english.lm2)

# By incorporating the two-way interaction, nothing is significant. Moreover, 
# the model with the interaction explains a similar proportion of random
# variance in the naming latency (i.e., 5.2%).

# Task 5
# Interactions in ANOVA and regression are mathematically based on the product
# of the variables in the interaction, so let's create a variable by multiplying
# verb frequency and diphone frequency. Note that if your model is built using
# the z-scored variables, you should examine the collinearity issues of these
# variables, too.
english.sub$VFreq.DiFreq = english.sub$VFreq.log * english.sub$FrequencyInitialDiphoneWord

# Use the cor() for the subset including only the three crucial variables to show
# their correlations all at once.
cor(english.sub[c("VFreq.log", "FrequencyInitialDiphoneWord", "VFreq.DiFreq")])

# The two-way interaction is almost perfectly correlated with verb frequency,
# which causes the collinearity problem.

# Calculate VIF using vif() of the car package
library(car)
vif(english.lm2)

# The VIF is much higher than the less stringent threshold (i.e., 10) for the
# main effect of verb frequency and the two-way interaction, which coincides
# with the collinearity problem indicated above.

# Task 6
# Create a data frame with five sequential values of verb frequency and diphone
# frequency to generate the predicted naming latencies using the model without
# the two-way interaction.

english.new = data.frame(VFreq.log = 1:5, FrequencyInitialDiphoneWord = 4:8)
# The predicted naming latencies are also log-transformed, so you need to reverse
# the log scale back to the raw scale using exp()
exp(predict(english.lm, english.new))

# Task 7
# One simple hypothesis can be made with AgeSubject, as young speakers should react
# faster than old speakers.
english.lm3 = lm(RTlexdec ~ VFreq.log + FrequencyInitialDiphoneWord + AgeSubject, 
                 data = english.sub)
summary(english.lm3)

# The effect of AgeSubject is highly significant, and it predicts a much shorter
# naming latency for young speakers. Note that the effect of diphone frequency
# also becomes significant after taking into account of speakers' age! It means
# the after sorting out the variance explained by age, the model is more certain
# about the effect of diphone frequency. The stronger explanatory power can also
# be seen with a substantial increase in the proportion of explained variance in
# naming latency this new model (55.7%).