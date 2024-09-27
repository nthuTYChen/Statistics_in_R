# Part I

#1
# Set n to 5 to randomly sample 5 values from a normal distribution
# with a mean of 2 and an sd of 10. Save the output, which is already a vector,
# to randNums.
randNums = rnorm(n = 5, mean = 2, sd = 10)
mean(randNums) # Just calculate the mean of randNums with mean()

#2
set.seed(100) # Set the random seed
# Create a for loop in which n starts with 1 and increases by 1 after each cycle.
# The loop stops after the end of a cycle in which n becomes 50.
for(n in 1:50) {
  # Randomly sample one value from a normal distribution using rnorm().
  # Since the distribution has a mean of 0 and an sd of 1, which are the default
  # values for the corresponding parameters in rnorm(), you don't have to specify
  # the two parameters in this line. The randomly selected value is stored as
  # "val".
  val = rnorm(n = 1)
  print(val) # Just print out the random value in each cycle.
}

#3
# Since we use the same random seed, we will get the same sampling results.
# That is, there is only one in the 50 attempts that generates a value above
# 1.96 (i.e., 2.310297). The probability is thus:
1 / 50 # 0.02

#4
# In the lecture/handout, I've explained that in every normal distribution,
# the area occupying +-1.96SD from the mean takes up about 2.5% of the entire
# distribution. Put differently, it is very likely that we only have 2.5% chance
# of randomly sampling a value with an SD equal to or higher than 1.96 from a 
# normal distribution. In the above 50 attempts, there was also around 2% to 
# have a value higher than 1.96SD, which supports the theoretical property of 
# a normal distribution.

# Part II
# Preparation works
library(languageR)  # Load the package
head(verbs) # Check if the data frame exists

#1
# Get the mean first
length.mean = mean(verbs$LengthOfTheme) # 1.498195...
# Calculate the difference between individual data points and the mean
length.diffs = verbs$LengthOfTheme - length.mean
# Get the sum of squares; 634.638387...
length.ss = sum(length.diffs ^ 2)
# Get the variance; identical to the output of var(verbs$LengthOfTheme)
length.var = length.ss / (length(verbs$LengthOfTheme) - 1)
# Get the SD with the square root of the variance
# Identical to the output of sd(verbs$LengthOfTheme)
length.sd = sqrt(length.var)

#2
# Get the density information of LengthOfTheme
length.den = density(verbs$LengthOfTheme)
# Generate the density plot using plot() with a proper title
plot(length.den, main = "Data distribution of LengthOfTheme")
# Add the vertical line that represents the mean
abline(v = length.mean, col = "red", lwd = 2)
# Add the two vertical lines that represents the mean +- 1SD
abline(v = length.mean + length.sd, col = "blue", lwd = 1.5)
abline(v = length.mean - length.sd, col = "blue", lwd = 1.5)