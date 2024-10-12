# Part I
# Task 1
# The areas in a normal distribution that takes up a total of 5% of the entire
# distribution are the areas beyond 1.96SD above or below the mean. Therefore,
# the two boundaries indicating each area that takes up 2.5% of the distribution
# in the upper/lower tails could be calculated as mean +- 1.96 * SD.
upper.bound = 550 + 1.96 * 25 # Upper bound = 599 ms
lower.bound = 550 - 1.96 * 25 # Lower bound = 501 ms

# Task 2
# Choose a random seed first
set.seed(1500)
# Sample 10,000 values from a normal distribution with the mean and SD from
# Task 1 and store the values in a separate vector.
rand.samp = rnorm(n = 10000, mean = 550, sd = 25)

# Check if each value in the vector is lower than (<) the lower bound OR (|)
# higher than (>) the upper bound. The result is TRUE for a value as long as 
# one of the two conditions is met, or FALSE in other cases.
outliers.196 = rand.samp < lower.bound | rand.samp > upper.bound

# Sum the vector of TRUEs (1s) and FALSEs (0s) to get the number of TRUEs (1s)
outliers.n = sum(outliers.196)
# Get the proportion of TRUEs in the entire vector
outliers.n / 10000 # 0.0491 in my case

# Task 3
# According to my proportion, I would say that the results of my random sampling
# process supports the properties of a theoretical normal distribution. The
# proportion of my outliers is around 4.91%, which is very close to the 5%
# theoretical chance represented by the two areas below/above 1.96SD from the mean.

# Task 4
# Get the density information of my random sample
rand.samp.den = density(rand.samp)
# Generate the density plot with an informative title
plot(rand.samp.den, main = "Random Sample Distribution; M = 550, SD = 25")
# Add the two vertical lines representing the boundaries indicating the two
# 2.5% areas in the upper/lower tail.
abline(v = upper.bound, col = "red", lwd = 1.5)
abline(v = lower.bound, col = "red", lwd = 1.5)

# Part II
library(languageR) # Preparation works
head(verbs)
# Task I
# Use the classic y ~ x formula to aggregate the data of y by x using the
# function mean() with "verbs" as the data source.
verbs.avg.lot = aggregate(LengthOfTheme ~ Verb, FUN = mean, data = verbs)

# Task II
# Get the reordered row numbers based on LengthOfTheme values in INCREASING order.
theme.ordered = order(verbs.avg.lot$LengthOfTheme)
# Sort the data frame based on the reordered row numbers.
verbs.ord = verbs.avg.lot[theme.ordered,]
# Since the new data frame is ordered based on LengthOfTheme from the lowest
# average to the highest one, the LAST 20 rows are the verbs that have the Top-20
# longest theme.
verbs.rows = nrow(verbs.ord)
last20row.onset = verbs.rows - 19
verbs.sub = verbs.ord[last20row.onset:verbs.rows,]

# This is easier: Show the 20 rows in the TAIL of a data frame
verbs.sub = tail(verbs.ord, 20)

# Task III
# I actually didn't tell you how to create a bar plot using barplot() with a 
# DATA FRAME, so this task doesn't count, unless you find your own way. In this
# case, you get some bonus points.

# The crucial part is to specify the LengthOfTheme column as "height", and
# the Verb column as the names of each bar with "names.arg".
barplot(height = verbs.avg.lot$LengthOfTheme, names.arg = verbs.avg.lot$Verb,
        main = "Average Theme Length in English Dative Sentences", xlab = "",
        ylab = "Average Theme Length", las = 2, ylim = c(0, 3))