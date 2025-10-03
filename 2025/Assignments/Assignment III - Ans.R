# Get ready
library(languageR)
head(english)

# Part I
# Task 1
# Mean of FamilySize: The sum divided by the number of values
family.mean = sum(english$FamilySize) / length(english$FamilySize)
family.mean  # 1.821324

mean(english$FamilySize) # The same number

# SD of FamilySize
# Sum of squares
family.ss = sum((english$FamilySize - family.mean) ^ 2)
# sqrt(ss / (n - 1))
family.sd = sqrt(family.ss / (length(english$FamilySize) - 1))
family.sd # 0.8205113

sd(english$FamilySize) # The same number

# Task 2
# In the corresponding normal distribution using the same mean and SD,
# the area in the upper tail that covers 2.5% of the entire distribution is 
# separated from the rest of the distribution by the family size of 3.42996.
# In other words, in this normal distribution, the area beyond 3.42996 only
# covers 2.5% of the entire distribution.
qnorm(p = .025, mean = family.mean, sd = family.sd, lower.tail = F) # 3.42996

# Task 3
# In the same normal distribution, the area below the familiy size of .8 covers
# 10.6% of the entire data distribution.
pnorm(q = .8, mean = family.mean, sd = family.sd)
# In the same normal distribution, the area below the familiy size of 1 covers
# 15.8% of the entire data distribution.
pnorm(q = 1, mean = family.mean, sd = family.sd)

# Task 4
# Convert FamilySize into a vector of boolean values by testing whether the
# entire vector is lower than 1.
family.lower.1 = english$FamilySize < 1

# Get the total number of TRUEs to know how many values are lower than 1
family.lower.1.num = sum(family.lower.1) # 568

# Get the proportion of the number of values lower than 1 to the total number
# of values; the numbers lower than 1 covers around 12.4% of the sample 
# distribution
family.lower.1.num / length(english$FamilySize) # 0.1243433

# Task 5 (Bonus)
# In the sample distribution, the proportion of the area below the family size
# of 1 is SMALLER than that in the corresponding distribution. This might 
# suggest that the lower tail is NARROWER/SHORTER in the sample distribution than
# in the corresponding normal distribution.

# Part II
# Task 1
# Check the range of the sample distribution, so that I can set a reasonable
# range for the x-axis of my density plot.
range(english$FamilySize) # 0.6931472 - 5.5174529
# Generate the density plot for the sample distribution with a proper title
# and a larger range that covers the numbers above.
family.den = density(english$FamilySize)
plot(family.den, main = "Sample vs. Theoretical Distribution of Family Size",
     xlim = c(-1, 7))
# Create a sequence of numeric values separated by 0.1 from -1 to 7 
family.seq = seq(-1, 7, .1)
# Get the density information for the number sequence from a normal distribution
# with the mean and SD of the sample distribution
family.norm.den = dnorm(family.seq, mean = family.mean, sd = family.sd)
# Add the curve representing the normal distribution with the density information
lines(x = family.seq, y = family.norm.den, col = "pink", lwd = 2)

# Task 2
abline(v = family.mean, col = "green", lty = 2)

# Task 3 (Bonus)
# Convert log-transformed values back to the raw values using exp()
family.raw = exp(english$FamilySize)
# Set the plot pane to have one row and two columns.
par(mfrow = c(1, 2))
# Generate the density plot
plot(density(family.raw), main = "Raw Family Size Distribution")
# Add the QQ Plot and then the QQ line
qqnorm(family.raw)
qqline(family.raw, col = "red")
# Reset the plot pane
par(mfrow = c(1, 1))