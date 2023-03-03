# Resume the Week 1 progress
resourcesURL = "https://lngproc.hss.nthu.edu.tw/statisticsR/Week1/"

# Load the same "df" data frame with the link to the CSV file
df = read.csv(paste(resourcesURL, "dummyDataFrame.csv", sep = ""))

# Create a vector with numeric values from 1 to 2 (i.e., 1 and 2)
x = 1:2

# Extract the rows with the numeric vector
df[x, ]

# Create another vector with numeric values from 2 to 3 (i.e., 2 and 3)
y = 2:3

# Extract the subsets of rows and columns with the two numeric vectors
df[x, y]

# Create a vector using c() with the numbers 1 and 3. With the function,
# you can create a vector in which some numbers are skipped. The new
# vector replaces the old vector in "x".
x = c(1, 3)

# Extract the first and third rows and with the second and third columns
# of "df".
df[x, y]

# Create a function called "simpleAdd" that only has two statements
simpleAdd = function() {
  # Add 1 and 2 together and store the output as "x"
  # "inside this function".
  x = 1 + 2
  # Print the content of "x" to end this function.
  x
}

# Call the function by its name with a pair of parenthesis.
simpleAdd()

# Create a function that takes two values stored as "x" and "y"
# respectively.
complexSubtract = function(x, y) {
  # Inside the function, subtract "y" from "x" and store the 
  # result as "z"
  z = x - y
  # Print the content of "z" to end the function
  z
}

# When calling "complexSubtract", pass two numeric values and
# specify which value is mapped to which variable.
complexSubtract(x=2, y=4)
complexSubtract(x=9, y=12)

# When the mapping between a value and a variable is specified
# for all values, the order doesn't matter.
complexSubtract(y=12, x=9)

# If the variable names is not specified, the mapping is determined
# by the order of variables in the function. Here, 9 is mapped to "x"
# and 12 is mapped to "y" in line 62 below.
complexSubtract(9, 12)
complexSubtract(12, 9)

# If a value is specified for a variable in a function, the value
# is the "default value" of the variable.
complexCalc = function(x=0, y=0) {
  x = x ^ 2
  y = 1 / y
  z = x + y
  # Instead of printing out the content of "z" to end the function,
  # we use return() to return the content of "z" back to where
  # the function is called (see below).
  return(z)
}

# Call complexCalc() and map 7 to x and 5 to y. When the function
# "returns" the result of its calculation, store it to the variable
# "result".
result = complexCalc(7, 5)

# Call the function once again, but this time, only 5 is mapped to "y".
# Without specifying what "x" is inside the function, "x" will have its
# default value 0. Store the returned result as "result", too.
result = complexCalc(y=5)

# Summarize the entire data frame with str() (str = structure)
str(df)

# Move on to Week 3 - Load the script from my server to create three different
# vectors of numeric values: dist.norm, dist.skewL, and dist.skewR.
source("https://lngproc.hss.nthu.edu.tw/statisticsR/Week3/distExamples.R")

# Calculate the mean of each sample distribution
mean(dist.norm)
mean(dist.skewL)
mean(dist.skewR)

# Calculate the median of each sample distribution
median(dist.norm)
median(dist.skewL)
median(dist.skewR)

# Calculate the difference between a mean and a median to see
# if a distribution is "normal".
mean(dist.norm) - median(dist.norm)
mean(dist.skewL) - median(dist.skewL)
mean(dist.skewR) - median(dist.skewR)

# Get the density information of a distribution (how frequent a
# range of values is observed in the distribution), and store
# the information as dist.norm.den
dist.norm.den = density(dist.norm)
# Use plot() to generate a density plot: The first data in plot()
# should be the density information, and the variable "main" is for
# the title of the plot.
plot(dist.norm.den, main = "(Near-)Normal Distribution")
# Add a line to an existing plot: "v" means the point for placing
# a "vertical" line, which is specified as the mean of the distribution,
# "col" is for the "color" of the line, and "lwd" is the line width (
# the default is 1, so 1.5 = wider/thicker).
abline(v = mean(dist.norm), col = "red", lwd = 1.5)
# Add another blue verticle line to show the median
abline(v = median(dist.norm), col = "blue", lwd = 1.5)

# Repeat the same statements but for different sample distributions.
dist.skewL.den = density(dist.skewL)
plot(dist.skewL.den, main = "Non-normal distrubtion; Left Skewness")
abline(v = mean(dist.skewL), col = "red", lwd = 1.5)
abline(v = median(dist.skewL), col = "blue", lwd = 1.5)

dist.skewR.den = density(dist.skewR)
plot(dist.skewR.den, main = "Non-normal distrubtion; Right Skewness")
abline(v = mean(dist.skewR), col = "red", lwd = 1.5)
abline(v = median(dist.skewR), col = "blue", lwd = 1.5)