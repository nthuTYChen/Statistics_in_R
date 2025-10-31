# Introduction to parametric tests that deal with CONTINUOUS data

# Let's being with a z-test
# Research question: Assuming that the population of 3-year-olds has an average
# vocabulary size of 300 words that vary by 40 words on average, do the children of
# the same age from a family with a higher socio-economic status have a significantly 
# larger vocabulary size?

# Critical "parameters" in a parametric tests
mean.pop = 300    # Assumed population mean
sd.pop = 40       # Assumed population variance (SD)
mean.sample = 320 # The sample mean of vocabulary size
n = 10            # The sample size; pnly ten 3-year-old children

# Standard Error
se = 40 / sqrt(n)
# z-value (different from a z-score; see the Unit 4 handout)
voc.z = (mean.sample - mean.pop) / se
voc.z

# Our sample mean is higher than the assumed population mean, so we calculate
# the upper tail p value in a normal distribution of differences based on
# the z-value obtained above.
pnorm(q = voc.z, lower.tail = F)

# The probability is 0.0569 > the alpha level .05 = non-significant difference.
# That is, there is a 5.69% of chance that we can obtain a sample mean that is
# 20 higher than the population mean because of pure luck. Thus, we cannot
# say that the null hypothesis should be rejected.

# If our assumption is that children from a family with a high socio-econimic
# status has a "significantly different" vocabulary size compared to the 
# population mean, then the hypothesis is "non-directional". In this case, we
# need to calculate a two-tailed p-value: That is, whether it is unlikely to
# get the difference of 20 from either side of the normal distribution of
# differences. You multiply the upper tail p-value by 2 to get the two-tailed
# p-value, which is 0.1138463. Of course, it is still higher than .05.

pnorm(q = voc.z, lower.tail = F) * 2

# The two-tailed p-value suggests that if we randomly sample 10 data points
# from the assumed normal distribution, there is a 11.4% chance that we will
# get a difference larger than 20. To validate this chance, we can run a 
# simulation. In the simulation, we randomly sample 10 data points from
# the normal distribution 10 times, and calculate the sample mean and its
# difference from the population mean. If the chance is correct, we should
# obtain at least one difference >= 20 (1 / 10 = 10% < 11.4%).

set.seed(500)

for(n in 1:10) {
  randSamp = rnorm(n = 10, mean = 300, sd = 40)  # Random sampling
  M = mean(randSamp)                             # Calculate the sample mean
  # Calculate the absolute difference between the sample mean and the population
  # mean (because it's a two-tailed p-value).
  diff = abs(M - 300) 
  # Print out the difference; indeed, there is only one difference higher than 20
  print(diff)
}

# One-sample t-test: Replacing the population SD, which we usually don't know,
# with the sample SD to test the difference between the sample mean and the
# population mean.

# Let's still work on the vocabulary example.
mean.sample = 325 # Our sample average vocabulary size
mean.pop = 300 # The assumed population mean of vocabulary size
n = 10 # The sample size
sd.sample = 40 # The sample SD

# Note that the calculation of the standard error has the sample SD, instead
# of the population SD, in the numerator.
t.upper = (mean.sample - mean.pop) / (sd.sample / sqrt(n))
t.upper

# The shape of a t-distribution and how close it is to a normal distribution
# is determined by the sample size and the degree of freedom, which is
# sample size minus one in one-sample t-tests.
t.df = n - 1

# Get the upper-tail probability (because of a positive difference/t-value) from
# a corresponding t distribution using pt(). Note that the additional df parameter
# you need in this function.
p.upper = pt(q = t.upper, df = t.df, lower.tail = FALSE)
# Calculate the two-tailed p-value by default, assuming that our research
# hypothesis is never directional.
p.upper * 2

# We can also use t.test() to run a one-sample t-test for sure, but it needs
# an actual sample, so we need to simulate a sample of 10 three-year-old 
# children from a family of a high socio-economic status and their vocabulary
# size.

# Set a random seed so we get the same sample.
set.seed(5)
# Sample these 10 children from a normal distribution with a mean of 325 and
# an SD of 40.
group.sim = rnorm(n = 10, mean = 325, sd = 40)
# Check the mean and the SD of the simulated sample; both numbers are close
# to 325 and 40.
mean(group.sim)
sd(group.sim)

# Test the sample against a normal distribution with a mean (mu) of 300.
# The result suggests a non-significant difference. Check the Unit 4
# handout to see how to report the result of a one-sample t-test.
t.test(x = group.sim, mu = 300)

# Run the same test with another sample of 40 children to show the importance
# of sample size.
set.seed(9)
group2.sim = rnorm(n = 40, mean = 325, sd = 40)
mean(group2.sim)
sd(group2.sim)

# The sample and SD are similar but not the same as in the smaller sample, which 
# makes sense because it's random sampling. The difference from the population
# mean is similar, but with more data points, the result of the test suggests
# a significant difference.
t.test(x = group2.sim, mu = 300)

# Pearson's correlation test: Revisiting the correlation between word frequency
# and ont- prefix duration
library(languageR)
head(durationsOnt)

# Calculate the Pearson's correlation coefficient (r); -0.049...
cor(durationsOnt$Frequency, durationsOnt$DurationOfPrefix)

# The mean of each smaple
freq.mean = mean(durationsOnt$Frequency)
dur.mean = mean(durationsOnt$DurationOfPrefix)

# Calculate the Sum of Products (SP) in the numerator, and divide SP by 
# the sample size minus 1 to get the covariance. Since the two samples are
# paired, the size is the same for both samples, and you can obtain the 
# sample size (i.e., the number of value pairs) by calculated the number of
# observations in the entire data set using nrow().
df = nrow(durationsOnt) - 1

sum((durationsOnt$Frequency - freq.mean) * 
      (durationsOnt$DurationOfPrefix - dur.mean)) / df

# Use cov() and the raw sample data to validate the covariance
cov(durationsOnt$Frequency, durationsOnt$DurationOfPrefix)

# Demonstrating the change in covariance due to a change in the scale of a
# sample. 
durationsOnt$Freq.10x = durationsOnt$Frequency * 10

# Covariance is ten times higher, which does not make sense.
cov(durationsOnt$Freq.10x, durationsOnt$DurationOfPrefix)

# The calculation of r actually use z-scores of the two paired samples; after
# standardizing everything, the original scale does not matter.

# Convert raw values into z-scores in each sample.
freq.z = scale(durationsOnt$Frequency)
freq.10x.z = scale(durationsOnt$Freq.10x)
dur.z = scale(durationsOnt$DurationOfPrefix)

# Calculate the covariance with z-scored samples
sum(freq.z * dur.z) / (nrow(durationsOnt) - 1)
# The orignal scale does not matter because all raw values are standardized as z
sum(freq.10x.z * dur.z) / (nrow(durationsOnt) - 1)

# Calculate the t statistics for the Pearson's correlation test
r = -0.04927405
n = nrow(durationsOnt)
# We have a weak correlation and a negative r, so we get a negative t as well.
# Calculate the lower-tail p using pt(). Df is n - 2 in a Pearson's correlation
# test because we have two samples.
t.lower = r / (sqrt(1 - r^2) / sqrt(n - 2))
t.lower

# Not significant
p.lower = pt(t.lower, df = n - 2)
p.lower
p.lower * 2

# Validate the r statistics using cor.test() with raw sample data.
cor.test(x = freq.z, y = dur.z)