# Create a vector with numbers from 1 to 100
nums = 1:100

# Try to randomly sample three numbers from "nums"
sample(x = nums, size = 3)

# Introduction to "for loops", which repeatedly run all the codes 
# inside the {} block
# "n" starts with 1, which increases by 1 after the end of a loop.
# The loop stops when "n" becomes 10, which means the loop repeats 10 times.
for(n in 1:10) {
  # In each loop, show the value of "n", which increases in every loop.
  print(n)
}

# With a for loop, we can repeat the sampling process more efficiently
# Sample three numbers from "nums" randomly in each loop
for(n in 1:10) {
  # Store the sampled numbers as a vector "res"
  res = sample(x = nums, size = 3)
  # Show the content of "res"
  print(res)
}
# In the 10 loops, check if there's any result showing three "more extreme values"
# (> 80) sampled from "nums". Very unlikely, which shows it's not easy to have a
# biased sample unless you are biased in the first place.

# Increase the number of loops to 50, and see if you can get any result
# with three numbers higher than 80 - still very unlikely.
for(n in 1:50) {
  res = sample(x = nums, size = 3)
  print(res)
}

# Load a script I created for you, which generated three data distributions.
source("https://raw.githubusercontent.com/nthuTYChen/Statistics_in_R/main/2024/2_Concept/distExamples.R")

# Calculate the average value, or the mean, of each data distribution
mean(dist.norm)
mean(dist.skewR)
mean(dist.skewL)

# Calculate the median value, of each data distribution
median(dist.norm)
median(dist.skewR)
median(dist.skewL)

# Try to generate the density plot for "dist.norm"
# Get the density information for the distribution, which represent the (predicted) 
# frequency of a specific value in the data distribution
dist.norm.den = density(dist.norm)
# Generate a density plot with the density information using plot(), and 
# set the "main" parameter to show a proper title for the plot.
plot(dist.norm.den, main = "(Near-)Normal Distribution")
# Use abline() to add a line to an existing plot. The parameter "v" means
# to add a "vertical" line based on a specific value on the x-axis. In the next
# line, this value is the mean of the distribution. You can also specify "col"
# to set the color of the line, and "lwd" to set the line width.
abline(v = mean(dist.norm), col = "red", lwd = 1.5)
# Ditto, except that the point on the x-axis where the vertical line falls is
# based on the median of the distribution.
abline(v = median(dist.norm), col = "blue", lwd = 1.5)

# Get the range of a data distribution and compare it to the median and mean
# to have a general idea of the variance in the distribution
range(dist.norm)
median(dist.norm)
mean(dist.norm)

# Use summary() to show the core distributional properties all at once, including
# the median, mean, range, and quartile boundaries.
summary(dist.norm)

# Use IQR() to get another information about the variance of a distribution,
# that is, the inter-quartile range (the range representing the 25%-75% of
# a data distribution).
IQR(dist.norm)

# Standard Deviation
# Calculate the difference between every single value in dist.norm and the mean
# of the distribution.
dist.norm.diff = dist.norm - mean(dist.norm)

# If you sum every difference up, you get a number close to zero for two reasons:
# First, some values are higher than the mean and some lower, so some differences
# are positive and some are negative. Second, since we are dealing with a normal-like
# distribution, positive and negative differences will likely be equal, and they
# even out when you sum all the numbers.
sum(dist.norm.diff)

# So, you square the individual differences to make all the differences POSITIVE.
# Then, you can add these squared differences into a positive number, too.
dist.norm.diff.sq = dist.norm.diff ^ 2

# Then, you will get the Sum of Squares (SS)
ss.norm = sum(dist.norm.diff.sq)

# When you divide SS by the number of data points in the dist.norm vector obtained
# using length(), you get the average squared difference between individual 
# data points and the mean, which is how "variance" is quantified.
ss.norm / length(dist.norm)

# You can also use the var() function to obtain the variance of a data distribution.
# But wait! The number is different from the number above!
var(dist.norm)

# This is because var() calculates SAMPLE VARIANCE, so you need to subtract 1 
# from the number of data points in the denominator
ss.norm / (length(dist.norm) - 1)

# Since variance is squared, to get a true average distance between individual
# data points and the mean, we need to calculate the square root of the variance,
# which gives us SAMPLE SD.
sd.norm = sqrt(ss.norm / (length(dist.norm) - 1))

# You can also obtain SAMPLE SD using sd()
sd(dist.norm)

# You can calculate "the number that represents xSD above/below the mean".
# The number that represents 1SD above the mean
mean(dist.norm) + sd(dist.norm)
# The number that represents 1SD below the mean.
mean(dist.norm) - sd(dist.norm)

# Generate a density plot of a data distribution with vertical lines that
# represent the mean, median, and +-1SD from the mean.
dist.norm.den = density(dist.norm)
plot(dist.norm.den, main = "(Near-)Normal Distribution")
abline(v = mean(dist.norm), col = "red", lwd = 1.5)
abline(v = median(dist.norm), col = "blue", lwd = 1.5)
abline(v = mean(dist.norm) + sd(dist.norm), col = "green", lwd = 1.5, lty = 2)
abline(v = mean(dist.norm) - sd(dist.norm), col = "green", lwd = 1.5, lty = 2)

# Compare a sample distribution to a theoretical distribution. The Unit 2
# handout has the most detailed explanation, which I don't repeat here.
set.seed(1)
nums = rnorm(n = 100, mean = 10, sd = 2)
noise = runif(n = 100)
nums.noi = nums * noise
range(nums.noi)
nums.mean = mean(nums.noi)
nums.sd = sd(nums.noi)

nums.d = density(nums.noi)
plot(nums.d, main = "Sample vs. Theoretical Distribution")

range.seq = seq(from = -5, to = 15, by = 0.01)
range.seq.d = dnorm(x = range.seq, mean = nums.mean, sd = nums.sd)
lines(x = range.seq, y = range.seq.d, col = "purple", lwd = 2)

# Generate the Q-Q Plot to make a quick comparison between the sample and the
# theoretical distribution. Please also refer to the Unit 2 handout for a
# detailed explanation of how to interpret a Q-Q Plot
qqnorm(dist.norm)
qqline(dist.norm, col = "red")

# Use pnorm() to obtain the proportion of the LOWER-TAIL area below a specific
# SD
# The proportion of the lower-tail area below -1.96SD from the mean
pnorm(q = -1.96) # 0.024...
# The proportion of the lower-tail area below 1.96SD from the mean
pnorm(q = 1.96) # 0.975...
# The proportion of the UPPER-TAIL area above 1.96SD from the mean
1 - pnorm(q = 1.96) # 0.024...
# Set the parameter lower.tail to FALSE (or F) so pnorm() also gives you
# the proportion of the UPPER-TAIL area above 1.96SD from the mean
pnorm(q = 1.96, lower.tail = FALSE)