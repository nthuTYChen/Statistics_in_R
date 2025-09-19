# Load the languageR package
library(languageR)

# Showing the first six rows of the "english" data set 
head(english)

# Part I
# Task 1
# 2:8 represents a number sequence from 2 to 8, separated by 1. These numeric
# values are used to represent x in [x, y] (i.e., row numbers) to get a subset
# of the data frame. Then, "$RTnaming" extracts the values from the RTnaming
# column as a vector.
english[2:8,]$RTnaming

# Task 2
# Each start:end creates a vector with continuous number sequence in which 
# every two values are separated by 1. The three vectors are then combined into 
# one vector using c().
seqs = c(2:8, 30:31, 67:70)

# Task 3
# This task is similar to Task 1, except that now row numbers are represented
# with the values stored in the "seqs" vector.
english[seqs,]$Word

# Task 4
# When there is only one number vector specified in [], it represents the column
# number(s) of a data frame. Here, I use "8" to get the eighth column of the
# target data frame, and the output is also a data frame with row numbers.
# Then, I put this subset in head(), which automatically gets me the first six
# rows of the data frame.
head(english[8])

# This won't work, because the output is a vector.
# english[1:6, 8]

# Part II
# Task 1
# Create the function that receives two values, which are assigned to "rowN" and
# "colN" used in the function. The are named properly to represent a row number
# and a column number, respectively.
getDFValue = function(rowN, colN) {
  # Get the value from a cell using the row number and column number received
  # by the function, and assign the value to the variable "englishVal".
  englishVal = english[rowN, colN]
  # Combine the string "The value is: " with the value assigned to "englishVal"
  # in paste(). The value sep = "" means that the things to be combined in paste()
  # are separated by an empty string (i.e., "nothing"). The combined string is
  # then assigned to the variable "outputMsg"
  outputMsg = paste("The value is: ", englishVal, sep = "")
  # Print the value of "outputMsg"
  print(outputMsg)
}

# Call the getDFValue function with the two pairs of row/column numbers as
# requested in the assignment.
getDFValue(190, 18)
getDFValue(3018, 2)

# Task 2
# Create the function that receives one value, which is a vector of multiple
# row numbers, and that's why I call it "rowsN" in this function.
getFamMean = function(rowsN) {
  # Get all the values from the rows specified in "rowsN" of the "Familiarity"
  # column from "english", and assigned the output vector to "famVals", which
  # stands for "familiarity values".
  famVals = english[rowsN, "Familiarity"]
  # Pass the entire "famVals" vector to the mean() function to get the mean,
  # and assign the mean to "famMean". It should be now clear what "famMean"
  # means.
  famMean = mean(famVals)
  # Print the mean value.
  print(famMean)
}

# Call the function and pass a vector including a vector including the numeric
# values from 200 to 300, in which every two numbers are separated by 1.
getFamMean(200:300)