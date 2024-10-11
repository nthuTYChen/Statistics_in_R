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
n = 10            # The sample sizeL Only ten 3-year-old children

# Standard Error
se = 40 / sqrt(n)
# z-value (different from a z-score; see the Unit 4 handout)
z = (mean.sample - mean.pop) / se

# Our sample mean is higher than the assumed population mean, so we calculate
# the upper tail p value in a normal distribution of differences based on
# the z-value obtained above.
pnorm(q = z, lower.tail = F)

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

pnorm(q = z, lower.tail = F) * 2

# The two-tailed p-value suggests that if we randomly sample 10 data points
# from the assumed normal distribution, there is a 11.4% chance that we will
# get a difference larger than 20. To validate this chance, we can run a 
# simulation. In the simulation, we randomly sample 10 data points from
# the normal distribution 10 times, and calculate the sample mean and its
# difference from the population mean. If the chance is correct, we should
# obtain at least one difference >= 20 (1 / 10 = 10% < 11.4%).

set.seed(500)

for(n in 1:10) {
  sam = rnorm(n = 10, mean = 300, sd = 40)  # Random sampling
  M = mean(sam)                             # Calculate the sample mean
  # Calculate the absolute difference between the sample mean and the population
  # mean (because it's a two-tailed p-value).
  diff = abs(M - 300) 
  # Print out the difference; indeed, there is only one difference higher than 20
  print(diff)
}

# Redo everything, but assuming that we have 40 data points in our sample
n = 40
se = 40 / sqrt(n)
z = (320 - 300) / se

# Now the one-tailed p-value is much lower than .05, so the positive difference
# between the sample mean and the population mean is very unlikely due to
# random sampling. If we say that the null hypothesis should be rejected,
# it is very unlikely that we are wrong.
pnorm(q = z, lower.tail = F)

# The two-tailed p-value is still smaller than .05, so the chance of getting
# an absolute difference of 20 with 40 data points is still very low in this case.
pnorm(q = z, lower.tail = F) * 2

# Because the two-tailed p-value suggests that there is only 1 in 1000 times
# that we can obtain an absolute difference of 20 or higher by randomly sampling, 
# we do not expect to obtain the difference in just 10 attempts, which is correct.

set.seed(800)

for(n in 1:10) {
  sam = rnorm(n = 40, mean = 300, sd = 40)
  M = mean(sam)
  diff = abs(M - 300)
  print(diff)
}