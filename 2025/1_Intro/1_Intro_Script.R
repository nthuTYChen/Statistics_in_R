setwd("~/GitHub/Statistics_in_R/2025/1_Intro")

1 + 2 # Simple addition

# There are multiple lines, but they are actually one single statement
(235678 / 1729 + 25) ^ 2 * 1000 + 50 * 1.5 - 29 / 77 ^ 3 + 
  99 * 20 * 65 / 4 - 1 + 1550

# Variable
num = 1 + 3

num <- 1 + 3

num

num = 1 - 3

num

# x, y, a, b

# Vector
nums = 6:15

nums

nums[1]

nums[2]

num[1]

nums[1:2]

# Characters

str = "This is a string!"

str

resourcesURL = 
  "https://raw.githubusercontent.com/nthuTYChen/Statistics_in_R/refs/heads/main/2025/1_Intro/"

resourcesURL

# Function
c(1, 3, 5)
c("this", "is", "a", "string")
c(num, nums)

pos = c(1, 3, 5)
nums[pos]

simpleAdd = function() {
  x = 1 + 2
  x
}

simpleAdd()

complexSubtract = function(x, y) {
  z = x - y
  z
}

complexSubtract(x = 2, y = 4)
complexSubtract(x = 9, y = 12)

complexSubtract(y = 12, x = 9)

complexSubtract(2, 4)
complexSubtract(4, 2)

complexSubtractNew = function(x = 0, y = 0) {
  z = x - y
  z
}

complexSubtractNew()
complexSubtractNew(1)
complexSubtractNew(2, 4)

complexCalc = function(x = 0, y = 0) {
  x = x ^ 2
  y = 1 / y
  z = x + y
  return(z)
}

result = complexCalc(7, 5)
result

# Data frame
df = read.csv(paste(resourcesURL, "dummyDataFrame.csv", sep = ""))
df