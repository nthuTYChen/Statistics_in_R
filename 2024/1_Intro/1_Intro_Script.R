# A simple statement
1 + 2

# A long statement
(235678 / 1729 + 25)^2 * 1000 + 50 * 1.5-29 / 77^3 + 99 * 20* 65 / 4-1 + 1550

# There are two lines below, but it is one statement
(235678 / 1729 + 25)^2 * 1000 + 50 * 
  1.5-29 / 77^3 + 99 * 20* 65 / 4-1 + 1550

# Create a variable called "num", and
# assign a value to it, which is the result of 1 + 3
num = 1 + 3

# Replace the value of "num" with the result of 1 - 3
num = 1 - 3

# The equal sign "=" has an alternative, which is "<-"
num <- 1 + 3

# Create a variable "nums", which stores a vector with
# a sequence of numeric values from 6 to 15
nums = 6:15

# Get the first value of the vector
nums[1]
# Get the seventh value of the vector
nums[7]

# Replace the second value of the vector with the numeric value 3
nums[2] = 3

# Strings have to be enclosed inside a pair of double-quotation marks
str = "This is a string"

# Store the link to my course material folder for Week 1
resourcesURL = "https://lngproc.hss.nthu.edu.tw/statisticsR/Week1/"

# Load a CSV file into R as a data frame (df)
# from the folder with the file name "dummyDataFrame.csv"
df = read.csv(paste(resourcesURL, "dummyDataFrame.csv", sep = ""))

# If only one number is enclosed inside [] for a data frame,
# it represents a column number, and this line gets you all the
# values of the first column/factor (the output is still a data frame)
df[1]

# You can also place a string inside [] for a data frame, which
# represents the name of a column/factor. This line retrieves all
# the values of the Age column (the output is still a data frame).
df["Age"]

# With a $ sign, you can also retrieve the values of a column based on
# the name of the column (the output is a vector).
df$Age

# Get the value from the second column of the first row.
df[1, 2]

# Get all the columns of the first row
df[1, ]

# Get the "Age" column of the first row
df[1, "Age"]

# Get all the columns of the first row, and then get all the values
# of the Age column of the extract row (the output is the same as that of 
# line 64).
df[1, ]$Age