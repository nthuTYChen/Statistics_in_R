library(languageR)
head(durationsOnt)

cor.test(durationsOnt$Frequency, durationsOnt$DurationOfPrefix)

cor.test(durationsOnt$Frequency, durationsOnt$DurationPrefixNasal)

dur.lm = lm(formula = DurationPrefixNasal ~ Frequency, data = durationsOnt)
summary(dur.lm)

0.055275 + -0.002052 * 0

0.055275 + -0.002052 * 1

log.freq.seq = 1:5

0.055275 + -0.002052 * log.freq.seq