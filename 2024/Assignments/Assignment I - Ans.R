# Preparation works
library(languageR)
head(verbs)

# 1-1: Use 10:12 for a vector of sequential numbers from 10 to 12, and use
#      this vector to retrieve the corresponding LengthOfTheme values from "verbs"
rows = 10:12
verbs[rows, ]$LengthOfTheme
# [1] 0.6931472 1.0986123 1.0986123

# 1-2: Every x:y is a number sequence from x:y, and you can place multiple x:y
#      into c() and concatenate them as one single number sequence in a vector.
#      And it's OK for different sequences to have their ranges overlapped,
#      because each sequence is an independent vector itself.
seqs = c(6:10, 1:20, 55:57)
head(seqs)
# [1]  6  7  8  9 10  1

# 1-3: "seqs" represents a vector of numeric values, which could thus be used as row numbers
#      to retrieve values from a column of a data frame
verbs[seqs,]$Verb
# [1] give  pay   bring teach give  feed  give  give  give  offer give  pay   bring teach give  offer give  pay...

# 1-4: This line says "show the data frame including only the second column, and then 
#      extract the first six rows of this sub-data-frame including just one column.
#      You can compare it to the output of "verbs[1:6,]$Verb", which is a vector.
head(verbs[2])

# 1-5: Get the last row of "verbs" using nrow(), and deduct 10 from the number
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

# 1-6: Shouldn't be too difficult to understand with all the above explanations...?
fiveLengths = verbs[1:5,]$LengthOfTheme
mean(fiveLengths)
# [1] 1.802134

# 1-7: Explain line by line below
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