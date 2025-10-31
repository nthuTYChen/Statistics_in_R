mean.pop = 300
sd.pop = 40
mean.sample = 320
n = 10

se = sd.pop / sqrt(n)

voc.z = (mean.sample - mean.pop) / se
voc.z

pnorm(voc.z, lower.tail = F)

pnorm(voc.z, lower.tail = F) * 2

for(n in 1:10) {
  randSample = rnorm(n = 10, mean = 300, sd = 40)
  M = mean(randSample)
  diff = abs(M - 300)
  print(diff >= 20)
}

mean.sample = 325
sd.sample = 40
n = 10

t.upper = (mean.sample - mean.pop) / (sd.sample / sqrt(n))
t.upper

p.upper = pt(t.upper, df = n - 1, lower.tail = F)
p.upper

p.upper * 2

set.seed(5)
group1.sim = rnorm(n = 10, mean = 325, sd = 40)
mean(group1.sim)
sd(group1.sim)

t.test(x = group1.sim, mu = 300)

set.seed(9)
group2.sim = rnorm(n = 40, mean = 325, sd = 40)
mean(group2.sim)
sd(group2.sim)

t.test(x = group2.sim, mu = 300)

library(languageR)
head(durationsOnt)
cor(durationsOnt$Frequency, durationsOnt$DurationOfPrefix)

freq.mean = mean(durationsOnt$Frequency)
dur.mean = mean(durationsOnt$DurationOfPrefix)

df = nrow(durationsOnt) - 1

sum((durationsOnt$Frequency - freq.mean) * 
      (durationsOnt$DurationOfPrefix - dur.mean)) / df
cov(durationsOnt$Frequency, durationsOnt$DurationOfPrefix)

durationsOnt$Freq.10x = durationsOnt$Frequency * 10

cov(durationsOnt$Freq.10x, durationsOnt$DurationOfPrefix)

freq.z = scale(durationsOnt$Frequency)
freq.10x.z = scale(durationsOnt$Freq.10x)
dur.z = scale(durationsOnt$DurationOfPrefix)

sum(freq.z * dur.z) / (nrow(durationsOnt) - 1)
sum(freq.10x.z * dur.z) / (nrow(durationsOnt) - 1)

r = -0.04927405
n = nrow(durationsOnt)
t.lower = r / (sqrt(1 - r^2) / sqrt(n - 2))
t.lower

p.lower = pt(t.lower, df = n - 2)
p.lower
p.lower * 2

cor.test(x = freq.z, y = dur.z)