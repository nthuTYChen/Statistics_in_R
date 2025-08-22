# To set my working directory
setwd("~/GitHub/Statistics_in_R/2024/1_Intro")

1 + 2 # Need to add proper spacing
#1+2  # No spacing makes your codes less readable

# Multi-line statement
# The second line of a multi-line statement is indented to increase readability
(235678 / 1729 + 25) ^ 2 * 1000 + 50 * 1.5 - 
  29 / 77 ^ 3 + 99 * 20 * 65 / 4 - 1 + 1550

# Variable
num = 1   # The value on the right is assigned to the variable on the left.
num2 <- 2 # This also works, and <- may be more straightforward to some.

num       # Retrieve the data assigned to 'num'

#Num      # Case-sensitive: It is different from 'num', so you see an error.

num = 1 + 3  # You can always change the value assigned to a variable.

# Vector
nums = 6:15       # 6:15 creates a number sequence 6, 7, 8, 9...15
manyNums = 10:90  # 10:90 creates a number sequence 10, 11, 12, 13...90

nums[3]           # Get the value from the third position of the nums vector
nums[3:5]         # Get the value from the third to fifth positions of the nums vector
# Get the third, fifth, and seventh value from the nums vector
# c() 'combines' multiple values as a vector
nums[c(3, 5, 7)]  
# Get the values from 3rd-5th, 10th-12th, and 20th-22nd position of the manyNums vector
manyNums[c(3:5, 10:12, 20:22)]

# String
string = "This is a string!"  # Strings/characters are enclosed in a pair of quotation marks
string                        # Retrieve the string assigned to the variable

# Create a string vector storing two strings combined using c()
strings = c("This is the first string!", "This is the second string!")
strings[1] # Retrieve the first string
strings[2] # Retrieve the second string

# Data frame

# Create a variable for storing the URL from which the sample data is loaded
resourceURL = 
  "https://raw.githubusercontent.com/nthuTYChen/Statistics_in_R/main/2024/1_Intro/"

# For specifying the file name
filename = "dummyDataFrame.csv"

# Use the paste() function to 'glue' multiple strings into one big string
# the sep = "" part simply says that the multiple strings are separated by
# an 'empty string' (i.e., they are separated by 'nothing')
targetFile = paste(resourceURL, filename, sep = "")

# Load the sample .CSV file with the link using read.csv()
# (CSV = comma separated values)
df = read.csv(targetFile)

# Two-ways of extracting data from a data frame
# df[col]
# df[row, col]

# Refer to a column by its number in a data frame
df[1]
# Refer to multiple columns with a number vector (i.e., from 1 to 2)
df[1:2]
# Refer to multiple columns with a number vector created with c()
# (i.e., the first and third column)
df[c(1, 3)]
# Refer to a column by its name (note: case-sensitive)
df["Age"]
# Refer to a column by a vector created with c() storing different column names
df[c("Gender", "Age")]
# The outputs of the above statements are all data frames!

# The column "age" (cf. "Age") does not exist in df!
# df["age"]

# Use a $ dollar to extract the value of a column by its name as a "vector"
df$Age

df[1, 2]
df[1, ]

df[1, "Age"]
df[1, c("Gender", "Age")]
df[1, ]$Age

cols = c("Gender", "Age")

df[1, cols]

# Function

simpleAdd = function() {
  x = 1 + 2
  print(x)
}

simpleAdd()

complexSubtract = function(x, y) {
  z = x - y
  print(z)
}

complexSubtract(x = 2, y = 4)
complexSubtract(x = 9, y = 12)

complexSubtract(y = 12, x = 9)

complexSubtract(2, 4)
complexSubtract(4, 2)

complexSubtract = function(x = 0, y = 0) {
  z = x - y
  print(z)
}

complexSubtract(2, 4)
complexSubtract(9)

complexCalc = function(x = 0, y = 0) {
  x = x ^ 2
  y = 1 / y
  z = x + y
  return(z)
}

calcOutput = complexCalc(7, 5)

str(df)

attachStr = function(str1, str2) {
  longStr = paste(str1, str2, sep = "")
  return(longStr)
}

attachRes = attachStr("I love ", "linguistics!")