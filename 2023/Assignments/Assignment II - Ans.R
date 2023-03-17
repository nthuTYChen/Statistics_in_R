# Preparation work
nums = c(1, 5, 7, 18, 24, 50)

# 1-1
nums[1]	# First value
nums[4] # Fourth value
nums[6] # Sixth value

# 1-2
# Create a variable called "msg" to store the text message
msg = "The third number is:"
# Combine "msg" and the third value of nums, which are 
# nevertheless separated by a space (as indicated by " ")
paste(msg, nums[3], sep = " ")

# 1-3
# Create the function taking two variables named "values"
# and "pos" respectively
printNum = function(values, pos) {
	# A numeric vector will be passed to "values", and 
	# regardless of the numbers in the vector, get one 
	# number out based on the number passed to "pos"
	# to specify the target inside the vector. Store the
	# number retrieved from the vector as "value".
	value = values[pos]
	# Use paste to combine different parts of the texts, 
	# "pos", and "value" together as the output
	paste("The #", pos, "value is:", value, sep = " ")
}

# 1-4
# Just pass "nums" and a position number to the function
printNum(nums, 3)

# 2-1
rnorm(n = 5, mean = 10, sd = 2)

# 2-2
set.seed(99)	# Set the random seed to 99
# Get 100 values from a normal distribution with the seed
# based on the same mean and sd.
randNums = rnorm(n = 100, mean = 10, sd = 2)

# 2-3
mean(randNums)
median(randNums)
# To show the first and the third quartile
summary(randNums)
# The difference between the first quartile and the median
10.1 - 8.709
# The difference between the third quartile and the median
10.915 - 8.709

# 2-4
# The difference between the first quartile and the median is
# larger than that between the third quartile and the median.
# This suggests a longer tail to the left due to a larger
# distance between the first 25% of the distribution and the 
# midpoint of the distribution. Or, we can say the distribution
# is not symmetrical on both sides of the median and is skewed
# to the left.

