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

# Pick the name of a variable wisely - choose one that matches the nature of
# the data stored inside a variable, not just random names like a, b, c, d...

nums  # Get the entire vector

nums[1]  # Get the first value of a vector
nums[2]  # Get the second value of a vector

nums[3:5]         # Get the value from the third to fifth positions of the nums vector

num[1]   # The variable that stores only ONE value is also a vector

# String
string = "This is a string!"  # Strings/characters are enclosed in a pair of quotation marks
string                        # Retrieve the string assigned to the variable

# The link (as ONE very long string) to my GitHub repository for the Fall 2025 materials.
resourcesURL = 
  "https://raw.githubusercontent.com/nthuTYChen/Statistics_in_R/refs/heads/main/2025/1_Intro/"

resourcesURL

# Function
# Combine multiple values/vectors into a (larger) vector using c()
c(1, 3, 5)   # Combine three numeric values
c("this", "is", "a", "string") # Combine four strings
c(num, nums) # Combine two vectors as a larger vector

# Get the first, third, and fifth value from the nums vector
# Create a varible that stores a vector with three numbers that represents
# different positions in a vector
pos = c(1, 3, 5)
# Place the variable inside [], so the values in nums could be retrieved with
# the position numbers stored previously in pos.
nums[pos]

# A simple function that adds 1 to 2, store the result to x, and get the value
# of x.
simpleAdd = function() {
  x = 1 + 2
  x
}

# The function runs only when it is called.
simpleAdd()

# A slightly more complex function that takes two values, x and y, calculates
# the difference between x and y, stores the result to z, and get the value of 
# z
complexSubtract = function(x, y) {
  z = x - y
  z
}

# Call complexSubtract() and pass two different values to the function
complexSubtract(x = 2, y = 4)
complexSubtract(x = 9, y = 12)

# The order of values does not matter if you specify which is x and which is y.
complexSubtract(y = 12, x = 9)

# The order matters if you do not specify the link between each value and the
# variables in the function.
complexSubtract(2, 4)
complexSubtract(4, 2)

# A different version that specifies the default value for each variable
# when nothing is linked to the variables.
complexSubtractNew = function(x = 0, y = 0) {
  z = x - y
  z
}

# Call the new function with passing any value
complexSubtractNew()
# Call the new function with one value, which is linked to the first variable
# in the function (i.e., x)
complexSubtractNew(1)
# Call the new function with two values, which will replace both default values
# in the function
complexSubtractNew(2, 4)

# Another function that does some complex calculation and "return" the result
# to where it is called.
complexCalc = function(x = 0, y = 0) {
  x = x ^ 2
  y = 1 / y
  z = x + y
  return(z)
}

# Call complexCalc() and pass two values, get the data returned from the 
# function (i.e., z), and store the data to the variable "result."
result = complexCalc(7, 5)
result

# Data frame
df = read.csv(paste(resourcesURL, "dummyDataFrame.csv", sep = ""))
df