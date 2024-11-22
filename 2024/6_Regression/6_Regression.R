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