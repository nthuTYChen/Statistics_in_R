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