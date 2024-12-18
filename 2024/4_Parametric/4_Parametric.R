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

# One-sample t-test: Replacing the population SD, which we usually don't know,
# with the sample SD to test the difference between the sample mean and the
# population mean.

# Let's still work on the vocabulary example.
M = 325 # Our sample average vocabulary size
pop.mu = 300 # The assumed population mean of vocabulary size
n = 10 # The sample size
sam.sd = 40 # The sample SD

# Note that the calculation of the standard error has the sample SD, instead
# of the population SD, in the numerator.
t.upper = (325 - 300) / (sam.sd / sqrt(n))

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

# Store the two paired samples separately as a factor to save some typing below
dur.logFreq = durationsOnt$Frequency
dur.prefDur = durationsOnt$DurationOfPrefix

# Calculate the Pearson's correlation coefficient (r); -0.049...
cor(dur.logFreq, dur.prefDur)

# Calculate r manually; check p.10-11 of the Unit 4 handouts for the most
# detailed explanation

# The mean of each smaple
dur.logFreq.m = mean(dur.logFreq)
dur.prefDur.m = mean(dur.prefDur)

# Calculate the Sum of Products (SP) in the numerator, and divide SP by 
# the sample size minus 1 to get the covariance. Since the two samples are
# paired, the size is the same for both samples, and you can obtain the 
# sample size with one of the two samples in length().

# In my lecture, the object is named dur.sp, which is not appropriate, because
# SP is represented only by the numerator. The output of the entire calculation
# should be covariance.
dur.cov = sum((dur.logFreq - dur.logFreq.m) * 
               (dur.prefDur - dur.prefDur.m)) / (length(dur.logFreq) - 1)

# Use cov() and the raw sample data to validate the covariance
cov(dur.logFreq, dur.prefDur)

# Demonstrating the change in covariance due to a change in the scale of a
# sample. 
cov(dur.logFreq, dur.prefDur * 10)

# The calculation of r actually use z-scores of the two paired samples; after
# standardizing everything, the original scale does not matter.

# Convert raw values into z-scores in each sample.
dur.logFreq.z = (dur.logFreq - dur.logFreq.m) / sd(dur.logFreq)
dur.prefDur.z = (dur.prefDur - dur.prefDur.m) / sd(dur.prefDur)

# Calculate the covariance with z-scored samples

# Again, I shouldn't have named the output object as dur.sp.z in my lecture.
dur.cov.z = sum(dur.logFreq.z * dur.prefDur.z)

# Calculate r
r = dur.cov.z / (length(dur.logFreq.z) - 1) # = r = the output of cor()

# Calculate r-squared (which I forgot to do in my lecture).
r ^ 2
# 0.002427932; only 0.2% of the variance in the data is explained by the correlation.

# Store the sample size separately
n = length(dur.logFreq.z)

# Calculate the t value using a formula similar to that for an one-sample t-test
# The numerator is in fact (r - 0).
t = r / (sqrt(1 - r ^ 2) / sqrt(n - 2))
# We have a weak correlation and a negative r, so we get a negative t as well.
# Calculate the lower-tail p using pt(). Df is n - 2 in a Pearson's correlation
# test because we have two samples.
p = pt(t, df = n - 2)
p * 2 # Not significant

# Validate the r statistics using cor.test() with raw sample data.
cor.test(x = dur.logFreq, y = dur.prefDur)

# Explore the correlation between word frequency and the vowel duration in
# the ont- prefix; still not significant.
cor.test(x = durationsOnt$Frequency, y = durationsOnt$DurationPrefixVowel)

# Use a scatter plot to check if our paired samples have a good data distribution
# for running a Pearson's correlation test. See p.12-14 in the Unit 4 handout
plot(x = dur.logFreq, y = dur.prefDur)

# Use the Myers (2015) data to demonstrat two-sample t-tests
# Load the course script first
source("https://raw.githubusercontent.com/nthuTYChen/Statistics_in_R/main/courseUtil.R")

# Load the "clean" data set without RT outliers and zero RTs
Myers.clean = loadCourseCSV(2024, "4_Parametric", "MyersClean.csv")

# The working hypothesis is that due to the satiation effect, the nonword judgments
# become significantly slower (due to hesitation) or faster (due to boredom) in
# Session 2 than in Session 1.

# Extract the two RT subsets based on the two experiment sessions
s1.rt = Myers.clean[Myers.clean$Session == 1,]$logRT
s2.rt = Myers.clean[Myers.clean$Session == 2,]$logRT

# Run a two-sample t-test assuming an equal variance of the two samples
# The report suggests a significant difference in logRT across the two
# experiment sessions; crucially S2 logRT > S1RT
t.test(x = s1.rt, y = s2.rt, var = TRUE)

# Check the entire sample size to validate that df in a two-sample t-test
# assuming an equal variance is n - 2
nrow(Myers.clean)

# However, using a two-sample t-test assuming an equal variance has to be 
# justified with a test of equal variance, which can be done using var.test().
# The test report could be stored separately in another object.
var.test.res = var.test(x = s1.rt, y = s2.rt)
# What really matters here is the p-value of the report, which is lower than
# .05 and suggests an UNEQUAL variance between the two samples.
var.test.res$p.value

# Thus, what we really need to run to compare the two samples is in fact
# a two-sample t-test assuming an UNEQUAL variance, aka Welch's Two-Sample
# t-test. The var parameter is by default set to FALSE, so we don't have to
# to specify it in t.test() to run the t-test. The test result still suggests
# a significant difference in logRT between the two sessions. However, due to
# an unequal variance, we obtain a smaller t-value, a smaller df, and a slightly
# higher p-value to reflect our uncertainly when the variance is unequal.
t.test(x = s1.rt, y = s2.rt)

# Even if we just randomly sample the same number of data points from the same
# normal distribution, two samples can differ in their variance to some extend.
# This is probably why t.test() has var set to FALSE by default.
set.seed(10)
norm.x = rnorm(n = 100, mean = 0, sd = 1)
set.seed(49)
norm.y = rnorm(n = 100, mean = 0, sd = 1)
var.test.norm = var.test(x = norm.x, y = norm.y)
var.test.norm$p.value

# Paired t-test. Here we use overgeneralization in child language acquisition
# as example. See pp.18-19 in the Unit 4 handout for detailed explanation.

# Get the data set first.
overgen = loadCourseCSV(2024, "4_Parametric", "overgen.csv")
# Check the structure. The subject IDs and their ages are not properly ordered.
head(overgen)

# To run a PAIRED t-test, we need to order our data frame properly, so our
# data points are properly PAIRED.

# Use the order() function to order the row numbers firstly by Subject ID
# and then by Age in overgen.
newRowNums = order(overgen$Subj, overgen$Age)
# Use the reordered row numbers to sort the original data frame and store the
# output as overgen.ord. Now, the new data frame is ordered by Subject ID, and
# within each subject, the error numbers are also ordered by Age.
overgen.ord = overgen[newRowNums,]
head(overgen.ord)

# After pairing the data points, extract the two paired samples based on
# participants' age.
overgen.2 = subset(overgen.ord, Age == "Two")
overgen.3 = subset(overgen.ord, Age == "Three")
# Calculate the difference in error number for each pair of data points.
error.diff = overgen.3$ErrorN - overgen.2$ErrorN

# Calculate the mean/sd of the paired differences
diff.mean = mean(error.diff)
diff.sd = sd(error.diff)
# Get the SE based on the two statistics above
diff.se = diff.sd / sqrt(length(error.diff))
# Get the t-value using a formula that is logically similar to a one-sample
# t-test assuming zero as the population mean.
overgen.t = diff.mean / diff.se
# We've got a negative t, so compute the lower-tail p first in a t-distribution
# (df = the number of paired differences minus 1)
overgen.p = pt(q = overgen.t, df = length(error.diff) - 1)
# Compute the two-tail p.
overgen.p * 2

# Run a paired t-test using t.test() by specifying two samples and set
# paired as TRUE.
t.test(x = overgen.3$ErrorN, y = overgen.2$ErrorN, paired = T)

# Calculate Cohen's d to quantify the effect size, which is basically the absolute 
# difference between two samples divided by the SD of the two samples altogether. 
# The logic is straightforward, so please just refer to pp.23-24 in the Unit 4 handout
# for the explanations of the demonstration below.

# One-sample t-test that was run earlier in this unit
set.seed(9)
group2.sim = rnorm(n = 40, mean = 325, sd = 40)
mean(group2.sim)
sd(group2.sim)

t.test(x = group2.sim, mu = 300)

# Cohen's d = 0.678; medium effect size (i.e., 0.2 < 0.678 < 0.8)
abs(mean(group2.sim) - 300) / sd(group2.sim)

# Two-sample t-test based on the Myers (2015) study
abs(mean(s1.rt) - mean(s2.rt)) / sd(Myers.clean$logRT)

# Paired t-test based on the overgeneralization example
abs(diff.mean) / diff.sd