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

# Load the library for the "durationsOnt" dataset, which is used to explore
# the correlation between (log-transformed) word frequency and the duration
# of the ont- prefix (in seconds)
library(languageR)

# Get Pearson's correlation coefficient for word frequency and prefix duration
# r = -0.049; a very weak negative correlation.
cor(durationsOnt$Frequency, 
    durationsOnt$DurationOfPrefix)

# Explore if the very small negative r is significantly different from zero -
# a question that can be answered in a Pearson's correlation test

# Try to manually calculate the statistics based on the formula in the Week 7-9
# handout.

# Starting with covariance.

# Get the mean for each of the two correlated variables.
freq.mean = mean(durationsOnt$Frequency)
dur.mean = mean(durationsOnt$DurationOfPrefix)

# Get the differences between every individual data point and the mean for
# the two correlated variables
freq.diffs = durationsOnt$Frequency - freq.mean
dur.diffs = durationsOnt$DurationOfPrefix - dur.mean

# Get the "sum of products"
sum.prod = sum(freq.diffs * dur.diffs)

# Get the covariance of the two variables by dividing the sum of products
# by the number of observations (i.e., the number of words) in durationsOnt.
freq.dur.cov = sum.prod / (nrow(durationsOnt) - 1) # -0.003129659

# You can also get the same covariance of two correlated variables using cov()
cov(durationsOnt$Frequency, 
    durationsOnt$DurationOfPrefix)

# A potential problem with the manual calculation is that a covariance might
# be different depending on the scale of a variable. See the Week 7-9 handout
# for a more detailed explanation.

# Multiply word frequencies by 10 and store it into a variable called Freq.10x
# in the dataset.
durationsOnt$Freq.10x = durationsOnt$Frequency * 10

# Try to get the covariance between Freq.10x and DurationOfPrefix.
# The number is -0.03129659; it's also 10 times larger!
cov(durationsOnt$Freq.10x, 
    durationsOnt$DurationOfPrefix)

# The solution is to convert raw numbers into z-scores (i.e., SDs in a 
# distribution where the original mean is converted into 0), so the difference 
# in scale doesn't matter.

# The difference between 5 and 10 is 5, and the z-scores are -0.7071068
# and 0.7071068
scale(c(5, 10))
# The difference between 50 and 100 is 50 (ten times larger), but the z-scores
# are still -0.7071068 and 0.7071068, because the distance between the two
# values and the mean remains identical when the distance is converted into
# z-scores.
scale(c(50, 100))

# Thus, Pearson's correlation coefficient (r) is in fact a "standardized 
# coefficient" which is calculated by z-scoring the correlated variables
freq.dur.r = cov(scale(durationsOnt$Freq.10x), 
    scale(durationsOnt$DurationOfPrefix))
# -0.04927405; same as the one we've got from the statement below, in which
# the variables are NOT z-scored beforehand
# cor(durationsOnt$Frequency, durationsOnt$DurationOfPrefix)

# Moving on to calculating the t statistics of the Pearson's correlation
# coefficient.

# Get r-squared first, which means the proportion of variance explained
# by the correlation; # 0.0024 = 0.2%, a very small proportion.
freq.dur.r2 = freq.dur.r ^ 2

# Get the t-value based on all the information we have following the formula
# in the Week 7-9 handout.
n = nrow(durationsOnt)
t = freq.dur.r / (sqrt(1 - freq.dur.r2) / sqrt(n - 2)) # -0.493

# Because t is negative, get one-tailed p-value from the lower tail of a 
# t distribution for the probability of observing the negative correlation
# and beyond. p = .311
p = pt(t, df = n - 2, lower.tail = TRUE)
# Get a two-tailed p-value; p = .623; the chance of observing the weak 
# correlation due to total randomness is 62.3%, so the weak correlation is not
# significantly different from 0 (i.e., no correlation).
p * 2

# You will get the same t statistics with cor.test(). Note that as in cor(),
# the correlated variations do not have be z-scored first; the conversion is
# done automatically.
cor.test(x = durationsOnt$Frequency, 
         y = durationsOnt$DurationOfPrefix)

# Check Week 4-5 handout for the quick explanation of the experiment in
# Myers (2015). In this part, we will explore if the judgment of 3000+ 
# nonwords eventually became very tedious for the participants, so 
# reaction times had become much faster or slower in the second half of the
# experiment (the participants probably just wanted to end the experiment
# as quickly as possible or just got tired and responded more slowly)

source("https://lngproc.hss.nthu.edu.tw/statisticsR/courseUtil.R")
Myers.clean = loadCourseCSV("Week7-9", "MyersClean.csv")

# We have different number of data points in both sessions, so unpaired two
# sample t-test is the only choice.
table(Myers.clean$Session)

# Get RTs from each session as two separate vectors.
s1.rt = Myers.clean[Myers.clean$Session == 1,]$logRT
s2.rt = Myers.clean[Myers.clean$Session == 2,]$logRT

# Run an unpaired two-sample t-test assuming equal variance for both samples
# (i.e., var = TRUE). We get two means 6.981007 and 7.038837, and the t 
# statistics suggests that with a very small p-value (< .001), the 
# distance between the two means, which is not zero, is very unlikely to
# be observed due to total randomness. Thus, the distance is significantly
# different from zero, and the two means are significantly different from
# each other.
t.test(x = s1.rt, y = s2.rt, var = TRUE)

# However, do the two samples really have an equal variance? This question
# could be answered using the var.test() function (i.e., variance test).
# The results are stored to var.test.res
var.test.res = var.test(x = s1.rt, y = s2.rt)
# We only care about the p-value for now, which is also very small. That is,
# the variances in the two samples are significantly different!
var.test.res$p.value

# Re-run the t-test by leaving out "var=TRUE" (it is by default FALSE).
# We still get a p-value suggesting a significant difference between the two
# sample means. But check the Week 7-9 handout for the explanations of the
# differences between the outputs of the two statistical analyses as well as
# their connection to Type I and Type II Errors.
t.test(x = s1.rt, y = s2.rt)

overgen = loadCourseCSV(week = "Week7-9", file = "overgen.csv")

overgen$Error.diff = overgen$Error_2 - overgen$Error_3

diff.mean = mean(overgen$Error.diff)
diff.sd = sd(overgen$Error.diff)
diff.se = diff.sd / sqrt(nrow(overgen))

overgen.t = diff.mean / diff.se

overgen.p = pt(q = overgen.t, df = nrow(overgen) - 1, lower.tail = FALSE)
overgen.p * 2

t.test(x = overgen$Error_2, y = overgen$Error_3, paired = TRUE)

t.test(x = overgen$Error_2, y = overgen$Error_3)