# Preparation work
library(languageR)
head(verbs)

# Part I
# Task 1
# Get the row numbers sorted based on LengthOfTheme in an increasing order.
LOT.ord = order(verbs$LengthOfTheme)
# Using the sorted row numbers to reorder the entire verbs data frame and save
# the new data frame as verbs.ord
verbs.ord = verbs[LOT.ord,]

# Task 2
# The row number that represents the boundary between the 1st quartile and the
# 2nd quartile could be obtained with 1/4 of the total row numbers
nrow(verbs.ord) / 4     # 225.75, roughly equal to 225 or 226
# The row number that represents the boundary between the 3rd quartile and the
# 4th quartile could be obtained with 3/4 of the total row numbers
nrow(verbs.ord) * 3 / 4 # 677.25, roughly equal to 677 or 678

# Get the Q1-Q2 boundary of LengthOfTheme in verbs.ord
verbs.ord[225,]$LengthOfTheme  # 0.6931...
# Get the Q3-Q4 boundary of LengthOfTheme in verbs.ord
verbs.ord[678,]$LengthOfTheme  # 2.0794...

# You can validate your results using summary()
summary(verbs.ord$LengthOfTheme)

# Task 3
set.seed(200) # Set the random seed following the instruction
# Use sample() to randomly select 100 data points from LengthOfTheme
length.sam = sample(size = 100, verbs.ord$LengthOfTheme)

# Generate the density plot with a proper title
plot(density(length.sam), main = "Density Plot: 100 data points from LengthOfTheme")
# Add at least the median and the mean of the sample
abline(v = mean(length.sam), col = "red", lwd = 2)
abline(v = median(length.sam), col = "blue", lwd = 2)

# Generate the Q-Q Plot of the sample with a proper title
qqnorm(length.sam, main = "Q-Q Plot: 100 data points from LengthOfTheme")
# Add a Q-Q line to compare the distribution to a theoretical distribution
qqline(length.sam, col = "red")

# Explanation: According to the density plot, the distribution is more or less
# symmetrical and the mean and the median are very close to each other.
# According to the Q-Q Plot, the dots generally falls onaround the Q-Q line that
# represents the perfect overlap between a sample distribution and a theoretical
# distribution, although the values representing quantile boundaries are a bit
# higher in the actual sample than in the theoretical distribution. Thus, in
# general, our sample seems to have a normal-like distribution.

# Task 4
# First, calculate the SD of LengthOfTheme manually.
# Get the mean
LOT.mean = mean(verbs.ord$LengthOfTheme)
# Calculate the sum of squares
LOT.SS = sum((verbs.ord$LengthOfTheme - LOT.mean)  ^ 2)
# Get the SD
LOT.sd = sqrt(LOT.SS / (nrow(verbs.ord) - 1))

# Validate the SD
sd(verbs.ord$LengthOfTheme)

# Get the differences between individual values and the mean again
LOT.diffs = verbs.ord$LengthOfTheme - LOT.mean
# Convert the differences into SDs by dividing everything with the SD
LOT.sds = LOT.diffs / LOT.sd
# Save the new SD vector into verbs.ord
verbs.ord$LengthOfTheme.sd = LOT.sds

# Part II
# Task I
# Use the table() function to sort out the frequency of each level (word) in the
# Verb variable
verbs.table = table(verbs.ord$Verb)
# Use as.data.frame() to convert a frequency table into a data frame
verbs.freq = as.data.frame(verb.table)
# Use the order() again to sort the data frame based on verb frequency
# You'll need to specify the decreasing parameter as T(RUE)
verbs.freq.ord = verbs.freq[order(verbs.freq$Freq, decreasing = T),]

# Task II
# Just use xtabs() as in the Unit 3 handout. The sole independent variable is
# AnimacyOfRec
xtabs(~ AnimacyOfRec, data = verbs.ord)