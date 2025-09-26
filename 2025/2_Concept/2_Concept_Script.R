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

dist.norm.diff = dist.norm - mean(dist.norm)

sum(dist.norm.diff)

# Sum of squares
ss.norm = sum(dist.norm.diff ^ 2)

# Mean variance (population)
ss.norm / length(dist.norm.diff)

# Mean variance (sample)
var(dist.norm)

# Mean variance (sample)
ss.norm / (length(dist.norm.diff) - 1)

# Standard deviation (sample)
sd.norm = sqrt(ss.norm / (length(dist.norm.diff) - 1))
sd(dist.norm)

# Try to generate the density plot for "dist.norm"
dist.norm.den = density(dist.norm)
plot(dist.norm.den, main = "(Near-)Normal Distribution")
abline(v = mean(dist.norm), col = "red", lwd = 1.5)
abline(v = median(dist.norm), col = "blue", lwd = 1.5)
abline(v = mean(dist.norm) + sd.norm, col = "green", lwd = 1.5, lty = 2)
abline(v = mean(dist.norm) - sd.norm, col = "green", lwd = 1.5, lty = 2)

# Compare an actual sample distribution to its corresponding theoretical 
# normal distribution

# Set a random seed, so a random process always generates the same results
# with a specific algorithm. We set the same random seed together so we
# will get the same results from a random process.
set.seed(1)

# Randomly select 100 values from a normal distribution with a mean of 10
# and an SD of 2
nums = rnorm(n = 100, mean = 10, sd = 2)

# Randomly select another 100 values between 0 and 0.999999... from a uniform 
# distribution as "noise" (i.e., random variation).
noise = runif(n = 100)

# Multiply the 100 data points with the noise values so our data distribution
# becomes a bit "messy", just like a data set we would get in the real world.
# Without noise, our data distribution will be just a normal distribution, and
# it won't be meaningful to make the comparison.
nums.noi = nums * noise

# Get the density info for each value in the "noisy" data distribution.
nums.noi.den = density(nums.noi)

# Get the density plot for the "noisy" distribution. The range of x-axis and
# y-axis is arbitrary - they simply cover the minimum and maximum values of
# the distribution (-5, 15) and the minimum and maximum density values (0, 0.15).
plot(nums.noi.den, xlim = c(-5, 15), ylim = c(0, 0.15),
     main = "Sample vs. Theoretical Distribution")

# For each sample, there's a theoretical normal distribution with the same mean
# and SD. To visualize this theoretical distribution, let's get the mean and SD
# of the noisy distribution first.
nums.mean = mean(nums.noi)
nums.sd = sd(nums.noi)

# Generate a sequence of numbers from -5 and 15, and the interval is filled with
# numbers that differ by 0.01. The range is chosen to match the range of the
# x-axis in the density plot generated above.
range.seq = seq(from = -5, to = 15, by = 0.01)

# With this sequence of numbers, get the density value for each number from
# a normal distribution with the mean and SD of the noisy distribution.
range.seq.d = dnorm(x = range.seq, mean = nums.mean, sd = nums.sd)

# Add this density curve representing the corresponding normal distribution
# to the existing density plot.
lines(x = range.seq, y = range.seq.d, col = "purple", lwd = 2)

# Generate the Q-Q Plot of the noisy distribution to compare a sample 
# distribution to its corresponding normal distribution more easily.
# See the Unit 2 handout for a more detailed explanation.
qqnorm(nums.noi, main = "(Near-)Normal Q-Q Plot: nums:noi")
qqline(nums.noi, col = "red")

# Get the probability (proportion) represented by the area BELOW a specific
# value in a normal distribution using pnorm(). If mean and SD are not specified,
# they are 0 and 1 by default. In this case, each specific value is equivalent
# to the SD from mean itself.
pnorm(q = -1.96) # The area below -1.96
pnorm(q = 1.96) # The area below 1.96

# The area above 1.96 (i.e., the area in the upper tail)
pnorm(q = 1.96, lower.tail = FALSE)

# The area between -1 and 1.
pnorm(q = 1) - pnorm(q = -1)

# mean = 25, sd = 3
# The area below 18 (or (18-25)/3 = -2 SD from the mean)
pnorm(q = 18, mean = 25, sd = 3)
# The area above 19
pnorm(q = 19, mean = 25, sd = 3, lower.tail = F)

# Get the value of a boundary that separate an area representing a specific
# probability below the boundary (by default) in a normal distribution. Again,
# if mean and SD are not specified, the normal distribution has a mean of 0
# and an SD of 1.

# The boundary that defines an area representing the probability of 0.025
# in the lower tail
qnorm(p = .025) 

# The boundary that defines an area representing the probability of 0.025
# in the upper tail
qnorm(p = .025, lower.tail = F)

# The boundary that defines an area representing the probability of 0.05
# in the UPPER tail in a normal distribution with a mean of 25 and an SD
# of 3.
qnorm(p = .05, mean = 25, sd = 3, lower.tail = F)