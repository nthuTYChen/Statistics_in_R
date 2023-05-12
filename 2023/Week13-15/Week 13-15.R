library(languageR)
head(durationsOnt)

range(durationsOnt$Frequency)

cor.test(durationsOnt$Frequency, durationsOnt$DurationOfPrefix)

dur.lm = lm(formula = DurationPrefixNasal ~ Frequency, data = durationsOnt)
summary(dur.lm)

0.055275 + -0.002052 * 4

freq.val = data.frame(Frequency = seq(from = 0, to = 2, by = 0.1))
dur.pred = predict(object = dur.lm, newdata = freq.val)

dur.pred.df = as.data.frame(dur.pred)
