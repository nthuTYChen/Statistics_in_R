library(languageR)
head(durationsOnt)

# Task I
# z-score is a process to convert raw numeric values in a sample 
# into a relative distance from the mean (i.e., a standard deviation)
durationsOnt$DurV.z = 
  (durationsOnt$DurationPrefixVowel - mean(durationsOnt$DurationPrefixVowel)) / 
  sd(durationsOnt$DurationPrefixVowel)
durationsOnt$Freq.z = 
  (durationsOnt$Frequency - mean(durationsOnt$Frequency)) / 
     sd(durationsOnt$Frequency)

# Task II
# A sample sum of products is the sum of Zx multiples by Zy following
# the formula
DurV.Freq.sp = sum(durationsOnt$DurV.z * durationsOnt$Freq.z) # 3.57984...
# A higher sum of products represents a strong positive correlation,
# and a lower sum of products close to zero represents a weak or zero 
# correlation. The reason is that if a value in x is higher than the mean of x
# and its paired value in y is also higher than the mean of y, you get a 
# positive product by multiplying the two positive differences together. 
# Likewise, if a value in x is lower than the mean of x and its paired value in 
# y is also lower than the mean of a y, you also get a positive product by
# multiplying the two negative differences together. Then, when you sum all
# these products, you get a large sum of products, which represents a positive
# correlation. However, when x and y are not correlated, it means that an x 
# value and its paired value in y are sometimes both higher than the mean of x 
# and y respectively. so you get a positive product by multiplying the two
# differences, but sometimes an x value is higher than the mean of x while its 
# paired y value is lower than the mean of y, so you get a negative product by
# multiplying the two differences. Then, when you sum both positive and negative
# products, you won't get a high sum of products, which suggests a weak or no
# correlation between x and y.

# Task III
# Get the number of data points in the sample
sample.n = nrow(durationsOnt) # 102
# Get the covariance, which is the same as the one you get from
# cor(durationsOnt$DurationPrefixVowel, durationsOnt$Frequency)
DurV.Freq.r = DurV.Freq.sp / (sample.n - 1) # 0.03544397...

# Task IV
# Get the t-value following the formula in the handout
DurV.Freq.t = DurV.Freq.r / 
  sqrt((1 - DurV.Freq.r ^ 2) / (sample.n - 2)) # 0.35466...

# The t-value is positive, so get the one-tail p-value in the upper tail first
DurV.Freq.p = pt(q = DurV.Freq.t, df = sample.n - 2, lower.tail = F)
# one-tailed p = 0.362
# Get two-tailed p = 0.724
DurV.Freq.p * 2

# The r here, which represents a weak positive correlation between prefix
# vowel duration and word frequency is not significantly different from zero.
# That is, there is a 72% of chance to observe the weak positive correlation
# due to total randomness.

# The APA format:
# “To test if the variation in prefix vowel duration is explained by word 
# frequency, we correlated the two variables for Pearson’s r, which suggests a 
# weak, nonsignificant positive correlation (r = 0.035, t(100) = 0.355, 
# two-tailed p = .724)."

# Task V
# a. Set the random seed to 100
set.seed(100)

# b. Sample 100,000 data points from a t-distribution based on df = n - 2
t.rand = rt(n = 100000, df = sample.n - 2)

# c. Generate the density plot with t.rand to visualize the t-distribution
# note the informative title for the plot
plot(density(t.rand), main = "t-distribution, n = 100,000, df = 2")

# d. Mark two red lines based on the positive and negative t-value
abline(v = DurV.Freq.t, col = "red", lwd = 1.5)
abline(v = DurV.Freq.t * -1, col = "red", lwd = 1.5)

# Task VI: Correlation between Prefix Nasal Duration and Word Frequency
# z-scoring DurationPrefixNasal
durationsOnt$DurN.z = 
  (durationsOnt$DurationPrefixNasal - mean(durationsOnt$DurationPrefixNasal)) / 
  sd(durationsOnt$DurationPrefixNasal)

# Sum of product
DurN.Freq.ss = sum(durationsOnt$DurN.z * durationsOnt$Freq.z)

# Sample covariance
# Same as the output of 
# cor(durationsOnt$DurationPrefixNasal, durationsOnt$Frequency)
DurN.Freq.r = DurN.Freq.ss / (sample.n - 1) # -0.157764...

# Get the t-value: -1.581434...
DurN.Freq.t = DurN.Freq.r / 
  sqrt((1 - DurN.Freq.r ^ 2) / (sample.n - 2))

# Get the one-tailed p-value: 0.0584....
DurN.Freq.p = pt(q = DurN.Freq.t, df = sample.n - 2)

# Get the two-tailed p: 0.1169...
DurN.Freq.p * 2

# The APA format:
# “To test if the variation in prefix nasal duration is explained by word 
# frequency, we correlated the two variables for Pearson’s r, which suggests a 
# medium but nonsignificant negative correlation (r = -0.158, t(100) = -1.581, 
# two-tailed p = .117)."

# Task VII
# Based on the two analyses above, I would conclude that Dutch and French are
# similar in a way. The main reason is that the weak positive correlation
# between prefix vowel duration and word frequency is negligible in Dutch,
# whereas the negative correlation between prefix nasal duration and word
# frequency is line with our hypothesis that high word frequency leads to 
# shorter duration. Put differently, when Dutch speakers produce a low-frequency
# word, they try to preserve as much information of a nasal in the prefix as
# possible, probably because the nasal carries more information than the vowel
# in the prefix. Of course, you could also argue that since the correlation
# between prefix nasal duration and word frequency is not significant, there's
# really no evidence showing that nasals contain more information than vowels
# in Dutch, so Dutch is not like French at all.

# Bonus
# Task VIII
# The same old trick with plot(), but note that y-axis should be duration,
# which is explained by word frequency. Also, you should plot with z-scores,
# which are your statistical analyses based on.
plot(DurV.z ~ Freq.z, data = durationsOnt, main = "Correlation between
     Prefix Vowel Duration and Word Frequency (r = 0.035)", 
     xlab = "log-Frequency (z-score)", ylab = "ont- Vowel Duration (z-score)")

plot(DurN.z ~ Freq.z, data = durationsOnt, main = "Correlation between
     Prefix Nasal Duration and Word Frequency (r = -0.156)", 
     xlab = "log-Frequency (z-score)", ylab = "ont- Nasal Duration (z-score)")

# Task IX
# The point is to use facet_grid() to specify that the plot will be divided by
# two columns based on the Sex variable in the data frame.
library(ggplot2)
ggplot(mapping = aes(x = Freq.z, y = DurN.z), data = durationsOnt) +
  geom_point(size = 3, alpha = 0.5, color = "darkgrey") +
  facet_grid(~ Sex) + 
  labs(title = "Correlation between Prefix Nasal Duration and Word Frequency",
       subtitle = "r = -0.156",
       x = "log-Frequency (z-score)", y = "ont- Nasal Duration") +
  theme_bw()