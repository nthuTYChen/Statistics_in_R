# To set my working directory
setwd("~/GitHub/Statistics_in_R/2024/1_Intro")

1 + 2 # Need to add proper spacing
#1+2  # No spacing makes your codes less readable

# Multi-line statement
# The second line of a multi-line statement is indented to increase readability
(235678 / 1729 + 25) ^ 2 * 1000 + 50 * 1.5 - 
  29 / 77 ^ 3 + 99 * 20 * 65 / 4 - 1 + 1550

# Variable
num = 1
num2 <- 2

num

#Num # Case-sensitive: It is different from 'num'

num = 1 + 3

# Vector
nums = 6:15
manyNums = 10:90

nums[3]
nums[3:5]
nums[c(3, 5, 7)]
manyNums[c(3:5, 10:12, 20:22)]

# String
string = "This is a string!"
string

strings = c("This is the first string!", "This is the second string!")
strings[1]
strings[2]