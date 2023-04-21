# Assume a population of three-year-olds with an average 
# vocabulary size of 300.
mean.pop = 300
# Assume that the average distance between every single
# vocabulary size and the mean vocabulary size is 40 in
# the population.
sd.pop = 40

# Convert a numeric value in the population into a standardized
# z-score, that is, the distance between the value and the mean
# in SD
# 360 is 1.5 SD above the mean
(360 - mean.pop) / sd.pop # z-score for population
# 275 is -0.625 SD below the mean
(275 - mean.pop) / sd.pop

# With a z-score (SD), we can get the probability of retrieving
# the z-score and beyond in a normal distribution.

# The chance of getting an SD of 1.5 above the mean and beyond
# is 6.68% ("above the mean" = "check the probability in the upper
# tail" = "lower.tail = FALSE")
pnorm(q = 1.5, lower.tail = FALSE)
# The chance of getting an SD of -0.625 below the mean and beyond
# is 26.6% ("below the mean" = "check the probability in the lower
# tail" = "lower.tail = TRUE")
pnorm(q = -0.625, lower.tail = TRUE)

set.seed(100)
# Try to sample 100 data points from a population with
# a mean of 300 and an SD of 10.
sample = rnorm(n = 100, mean = 300, sd = 10)

# Manually convert the first numeric value in our sample
# into a sample z-score. Review the difference between
# population SD and sample SD in the handout.
(sample[1] - mean(sample)) / sd(sample)

# Use scale() to convert the entire vector of numeric values
# into z-scores based on the sample mean and the sample SD.
sample.z = scale(sample)

# Manually calculate the probability of observing the difference
# between a sample mean and a population mean in a z-test.
# Note that in a z-test, sample SD is further replaced by
# Standard Error, which is the population SD dividied by the
# square root of the size of the sample.

# First, assume that we have a sample of 10 three-year-olds
# with a mean vocabulary size of 325, so the difference between
# the sample mean and the population mean is 325 - 300 = 25.
# Question: How likely is this difference observed due to randomness?

# z = 1.967
z.10 = (325 - mean.pop) / (40 / sqrt(10))
# The probability of observing a z of 1.967 in the upper tail is only
# 0.024 (2.4%), so this difference is unlikely to be observed due to
# randomness. In other words, the difference is likely to be true.
pnorm(q = z.10, lower.tail = FALSE)
# We usually report a two-tailed p-value, since our assumption about
# a group of children with a different mean vocabulary size is not
# "directional" - the sample mean could be either higher or lower
# than the population mean. So here we choose to report a probability
# for a difference of 25 or beyond to be observed in the upper tail
# and the lower tail, which is 0.048 (4.8%).
pnorm(q = z.10, lower.tail = FALSE) * 2

# The probability is below our threshold of alpha = .05, so it is very
# unlikely that the difference is observed due to randomness. Put
# differently, the mean difference is statistically significant.

# Now let's calculate the probability of observing a difference of 5
# (305 - 300) with a group of 40 three-year-olds.
z.50 = (305 - mean.pop) / (40 / sqrt(50))
# One-tailed p = 0.3953 (34.6%)
pnorm(q = z.50, lower.tail = FALSE)
# Two-tailed p = 0.6926 (69.3%)
pnorm(q = z.50, lower.tail = FALSE) * 2

# The probability of 0.6926 is considerably higher than our threshold
# alpha = .05, so it is very likely that this difference of 5 is observed
# with the random sampling process. That is, the difference is not
# statistically significant.

# The problem with a z-test is that we don't really know the population
# SD, so we have to turn to a t-test, in which the shape of the theoretical
# distribution depends on the degree of freedom and the calculation of SE
# depends in part on the sample SD (as opposed to the population SD). See
# handout for the review.

set.seed(5)
# Simulate a real sample with 10 three-year-olds
group1 = rnorm(n = 10, mean = 325, sd = 40)
mean(group1) # 321.8459
sd(group1) #38.09322

# Manually calculate the t-value, which is a more "realistic" version
# of the z-score. This is a "one-sample" t-test since we're comparing
# the mean of ONE sample to a population mean.
t = (321.8459 - mean.pop) / (40 / sqrt(10)) # t = 1.72707
# Get the probability from a t-distribution using pt(), in which the
# degree of freedom (df) is the sample size minus 1.
# One-tailed (upper) p = 0.0591 (5.9%)
p.upper = pt(q = t, df = 10 - 1, lower.tail = FALSE)
# Two-tailed p = 0.1182 (11.8%)
p.upper * 2

# Based on the result of the one-sample t-test, there's a probability
# of 0.1182 for observing the difference between our sample mean
# and the population mean in the specific t-distribution. Since it is 
# higher than the alpha of .05, the difference is not statistically 
# significant. Check the handout to see how this result is reported in 
# the APA format.

# Run the same one-sample t-test using t.test(), and you will get the
# same t-value and p-value. (mu (how the Greek symbol for the 
# population mean is pronunced) = population mean)
t.test(x = group1, mu = 300)

set.seed(9)
# Simulate a different group of 40 three-year-olds
group2 = rnorm(n = 40, mean = 325, sd = 40)
mean(group2) # 325.8303
sd(group2) # 38.08742

# Compare the sample mean to the same population mean
t.test(x = group2, mu = 300)
# t = 4.2892, df = 39, p = 0.0001142
# The probabilty of observing the difference between
# 325.8303 and 300 with our sample is almost 1 in
# 10,000 times, so the difference is very unlikely
# due to randomness. That is, the difference is
# statistically significant.

library(languageR)
cor(durationsOnt$Frequency, 
    durationsOnt$DurationOfPrefix)

freq.mean = mean(durationsOnt$Frequency)
dur.mean = mean(durationsOnt$DurationOfPrefix)

sum.prod = sum((durationsOnt$Frequency - freq.mean) *
        (durationsOnt$DurationOfPrefix - dur.mean))

freq.dur.cov = sum.prod / (nrow(durationsOnt) - 1)

cov(durationsOnt$Frequency, 
    durationsOnt$DurationOfPrefix)

durationsOnt$Freq.10x = durationsOnt$Frequency * 10

cov(durationsOnt$Freq.10x, 
    durationsOnt$DurationOfPrefix)

scale(c(5, 10))
scale(c(50, 100))

cov(scale(durationsOnt$Freq.10x), 
    scale(durationsOnt$DurationOfPrefix))

freq.dur.cor.2 = cor(durationsOnt$Frequency, 
    durationsOnt$DurationOfPrefix) ^ 2

r = cor(durationsOnt$Frequency, 
        durationsOnt$DurationOfPrefix)
n = nrow(durationsOnt)
t = r / (sqrt(1 - freq.dur.cor.2) / sqrt(n - 2))

p = pt(t, df = n - 2, lower.tail = TRUE)
p * 2

cor.test(x = scale(durationsOnt$Freq.10x), 
         y = scale(durationsOnt$DurationOfPrefix))

source("https://lngproc.hss.nthu.edu.tw/statisticsR/courseUtil.R")
Myers.clean = loadCourseCSV("Week7-9", "MyersClean.csv")

table(Myers.clean$Session)

s1.rt = Myers.clean[Myers.clean$Session == 1,]$logRT
s2.rt = Myers.clean[Myers.clean$Session == 2,]$logRT

t.test(x = s1.rt, y = s2.rt, var = TRUE)

var.test.res = var.test(x = s1.rt, y = s2.rt)
var.test.res$p.value

t.test(x = s1.rt, y = s2.rt)