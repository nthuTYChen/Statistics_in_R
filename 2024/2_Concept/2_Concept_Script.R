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