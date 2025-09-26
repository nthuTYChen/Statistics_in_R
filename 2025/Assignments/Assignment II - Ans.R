# Part I

# 1. Create the function named calcMeanSD, which receives one data entry
# called "vec" in this function (since it represents a vector)
calcMeanSD = function(vec) {
  # 2. Use mean() to calculate the mean of the entire vector and store it
  # temporarily into another variable. Then, manually calculate the SD and also 
  # store the output temporarily
  vec.mean = mean(vec)
  vec.sd = sqrt(sum((vec - vec.mean) ^ 2) / (length(vec) - 1))
  # 3. Combine different parts of the final message and print it out.
  # Four different parts are combined using paste() as the output message.
  # These parts are properly separated by a space to make the message readable.
  vec.info = paste("This mean is", vec.mean, "and the SD is", vec.sd, sep = " ")
  # Print out the message using print()
  print(vec.info)
}

# 4. Demonstrate the function works fine with two vectors.
vec1 = c(174.9, 196.8, 152.6, 208.5, 150.2, 174.6, 184.9, 187.7, 185.4, 118.8)
vec2 = c(81.4, 82.3, 76.9, 80.6, 82.2, 74.8, 77.2, 77.5, 85.0, 74.8)

calcMeanSD(vec1) # "This mean is 173.44 and the SD is 26.2805885271493"
calcMeanSD(vec2) # "This mean is 79.27 and the SD is 3.49445592649589"

# Part II
# The load languageR package and check the "english" data set
library(languageR)
head(english)

# 1. Generate a density plot for FrequencyInitialDiphone with lines that 
# represent different distributional information

# Get the density information with density()
freq.phon.den = density(english$FrequencyInitialDiphone)
# Generate the density plot
plot(freq.phon.den)
# Store the mean/sd separately so I don't have to keep typing 
# "english$FrequencyInitialDiphone" below.
freq.phon.mean = mean(english$FrequencyInitialDiphone)
freq.phon.sd = sd(english$FrequencyInitialDiphone)
# A red line for the mean
abline(v = freq.phon.mean, col = "red")
# A blue line for the median
abline(v = median(english$FrequencyInitialDiphone), col = "blue")
# Two green lines for +-1SD from the mean
abline(v = freq.phon.mean + freq.phon.sd * 1.5, col = "green")
abline(v = freq.phon.mean - freq.phon.sd * 1.5, col = "green")

# 2. Add the purple line representing the corresponding normal distribution
# of the sample.

# Get the minimum and the maximum value of FrequencyInitialDiphone
freq.phon.min = min(english$FrequencyInitialDiphone)
freq.phon.max = max(english$FrequencyInitialDiphone)

# Create the sequence based on the range of FrequencyInitialDiphone so 
# we can extract density information from a normal distribution
freq.phon.seq = seq(from = freq.phon.min, to = freq.phon.max, by = 0.1)

# Get the density info from a normal distribution using dnorm() with the mean/sd
# obtained from the sample distribution
freq.phon.seq.den = dnorm(x = freq.phon.seq, mean = freq.phon.mean, sd = freq.phon.sd)

# Add the purple lines representing the normal distribution; x = the actual values
# in the sequence generated above, y = the density value of each actual value in
# the sequence.
lines(x = freq.phon.seq, y = freq.phon.seq.den, col = "purple", lwd = 2)

# 3. Explains if the sample distribution is normal-like

# First of all, the curve of the sample distribution, although wiggly, moves
# largely in accordance with the curve of the corresponding normal distribution,
# In addition, the two vertical lines representing the mean/SD of the sample
# distribution are very close to each other, and they are aligned to the peak
# of the corresponding normal distribution. The curves on both sides of the
# mean/median also seem more or less symmetrical.
# All these suggest that the sample distribution is sufficiently normal-like.

# Bonus

# Generate the Q-Q Plot and Q-Q Line for FrequencyInitialDiphone
qqnorm(english$FrequencyInitialDiphone)
qqline(english$FrequencyInitialDiphone, col = "red")

# The dots on the left below the red line (which represents a perfect match between
# the sample and the theoretical distributions) suggest that the values in the
# lower tail are smaller than the predicted values in the theoretical distribution.
# That is, the lower tail in our sample is longer than in the theoretical distribution.