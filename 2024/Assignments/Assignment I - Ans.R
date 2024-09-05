# Preparation works
library(languageR)
head(verbs)

# 1-1: Use 6:11 for a vector of sequential numbers from 6 to 11, and use
#      this vector to retrieve the corresponding LengthOfTheme values from "verbs"
rows = 6:11
verbs[rows, ]$LengthOfTheme
# [1] 1.3862944 1.3862944 0.0000000 2.3978953 0.6931472 1.0986123

# 1-2: verb[2] = Retrieve the second column of "verbs" and output it as a data frame
#     and use head() to show just the first six rows of the single-column data frame
head(verbs[2])
#   Verb
#1  feed
#2  give
#3  give
#4  give
#5 offer
#6  give

# 1-3: Get the last row of "verbs" using nrow(), and deduct 10 from the number
#      of the last row to get the number of the first row of "the last ten rows".
#      Use the two numbers to create a number sequence, and use the sequence to 
#      retrieve the last 10 LengthOfTheme values from "verbs".
lastRow = nrow(verbs)
lastRow
# [1] 903
firstRow = lastRow - 10
firstRow
# [1] 893
verbs[893:903,]$LengthOfTheme
# [1] 0.0000000 1.6094379 2.5649494 0.6931472 2.3978953 1.0986123 1.0986123 1.7917595 0.0000000 1.3862944 1.0986123

# This method is even better, because the number of rows could be different 
# across data frames
verbs[firstRow:lastRow,]$LengthOfTheme
# [1] 0.0000000 1.6094379 2.5649494 0.6931472 2.3978953 1.0986123 1.0986123 1.7917595 0.0000000 1.3862944 1.0986123

# 1-4: Shouldn't be too difficult to understand with all the above explanations...?
fiveLengths = verbs[1:5,]$LengthOfTheme
mean(fiveLengths)
# [1] 1.802134

# 1-5: Explain line by line below
# In function(), specify two variable row1 and row2 to receive and store two
# row numbers used only in this function.
LengthDifference = function(row1, row2) {
  # Get the first length value based on row1
  length1 = verbs[row1,]$LengthOfTheme
  # Get the second length value based on row2
  length2 = verbs[row2,]$LengthOfTheme
  # Calculate the difference between the two
  diff = length1 - length2
  # Print out the difference
  print(diff)
}

# Test the function by sending different row numbers
LengthDifference(10, 18)
# [1] -0.4054651
LengthDifference(301, 25)
# [1] 0.6418539