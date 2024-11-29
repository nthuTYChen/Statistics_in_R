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

dur.lm.int = lm(DurationPrefixNasal ~ 1, data = durationsOnt)
summary(dur.lm.int)

mean(durationsOnt$DurationPrefixNasal)

model.res = dur.lm$residuals
model.sse = sum(model.res ^ 2)
model.sse

null.res = dur.lm.int$residuals
null.sse = sum(null.res ^ 2)
null.sse

1 - model.sse / null.sse

dur.res = as.vector(dur.lm$residuals)
qqnorm(dur.res, main = "Q-Q Plot of the residuals of dur.lm")
qqline(dur.res, col = "red", lwd = 2)

(-0.002052 - 0) / 0.001298

pt(q = -1.581, df = 100) * 2

nas.dur = durationsOnt$DurationPrefixNasal
durationsOnt$DurPrefixN.z = (nas.dur - mean(nas.dur)) / sd(nas.dur)
freq = durationsOnt$Frequency
durationsOnt$Freq.z = (freq - mean(freq)) / sd(freq)

dur.lm.z = lm(formula = DurPrefixN.z ~ Freq.z, data = durationsOnt)
summary(dur.lm.z)

sr = durationsOnt$SpeechRate
durationsOnt$sr.z = (sr - mean(sr)) / sd(sr)

dur.sr.lm.z = lm(formula = DurPrefixN.z ~ sr.z, data = durationsOnt)
summary(dur.sr.lm.z)

table(durationsOnt$PlosivePresent)

durationsOnt$Pl.Pr.Fac = as.factor(durationsOnt$PlosivePresent)
contrasts(durationsOnt$Pl.Pr.Fac)

dur.lm.plo = lm(DurationPrefixNasal ~ Pl.Pr.Fac, data = durationsOnt)
summary(dur.lm.plo)

mean(durationsOnt[durationsOnt$Pl.Pr.Fac == "no",]$DurationPrefixNasal)
mean(durationsOnt[durationsOnt$Pl.Pr.Fac == "yes",]$DurationPrefixNasal)

0.04285096 - 0.06822025

t.test(formula = DurationPrefixNasal ~ PlosivePresent, var.equal = T,
       data = durationsOnt)

durationsOnt$Pl.Pr.Fac.Sum = as.factor(durationsOnt$PlosivePresent)
contrasts(durationsOnt$Pl.Pr.Fac.Sum) = contr.sum(2)
contrasts(durationsOnt$Pl.Pr.Fac.Sum)

dur.lm.plo.sum = lm(formula = DurationPrefixNasal ~ Pl.Pr.Fac.Sum,
                    data = durationsOnt)
summary(dur.lm.plo.sum)

dur.pl.aov = aov(formula = DurationPrefixNasal ~ PlosivePresent,
                 data = durationsOnt)
summary(dur.pl.aov)
