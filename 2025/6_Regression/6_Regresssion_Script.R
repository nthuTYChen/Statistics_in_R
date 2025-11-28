library(languageR)
head(durationsOnt)

cor.test(durationsOnt$Frequency, durationsOnt$DurationOfPrefix)

dur.lm = lm(formula = DurationPrefixNasal ~ Frequency, data = durationsOnt)
summary(dur.lm)

dur.lm2 = lm(formula = DurationPrefixNasal ~ 1 + Frequency, data = durationsOnt)
summary(dur.lm2)

0.055275 / 0.004042

freq.seq = seq(from = 0, to = 2, by = 0.1)
head(freq.seq)

new.df = data.frame(Frequency = freq.seq)
head(new.df)

predict(dur.lm, newdata = new.df)

new.df$Predict = as.vector(predict(dur.lm, newdata = new.df))
head(new.df)

dur.lm.int = lm(DurationPrefixNasal ~ 1, data = durationsOnt)
summary(dur.lm.int)

mean(durationsOnt$DurationPrefixNasal)

null.res = dur.lm.int$residuals
null.sse = sum(null.res ^ 2)
null.sse

model.res = dur.lm$residuals
model.sse = sum(model.res ^ 2)
model.sse

1 - model.sse / null.sse

qqnorm(model.res, main = "Q-Q Plot of dur.lm Residuals")
qqline(model.res, col = "red", lwd = 2)

nas.dur = durationsOnt$DurationPrefixNasal
durationsOnt$DurPrefixN.z = (nas.dur - mean(nas.dur)) / sd(nas.dur)
freq = durationsOnt$Frequency
durationsOnt$Freq.z = (freq - mean(freq)) / sd(freq)

dur.lm.z = lm(formula = DurPrefixN.z ~ Freq.z, data = durationsOnt)
summary(dur.lm.z)

xtabs(~ PlosivePresent, data = durationsOnt)

durationsOnt$Pl.Pr.Fac = as.factor(durationsOnt$PlosivePresent)
contrasts(durationsOnt$Pl.Pr.Fac)

dur.lm.plo = lm(DurationPrefixNasal ~ Pl.Pr.Fac, data = durationsOnt)
summary(dur.lm.plo)

mean(durationsOnt[durationsOnt$PlosivePresent == "yes",]$DurationPrefixNasal)
mean(durationsOnt[durationsOnt$PlosivePresent == "no",]$DurationPrefixNasal)