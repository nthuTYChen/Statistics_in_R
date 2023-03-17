# Load a script from my server, so you can simply use loadCourseCSV()
# for the rest of this course to help load some datasets for this course.
# Note that this function is not a base function, so you always have to
# load this script in order to use it.
source("https://lngproc.hss.nthu.edu.tw/statisticsR/courseUtil.R")

# Load wordRT.csv from the Week4-5 folder from my course server,
# and save the data frame as wordRT
wordRT = loadCourseCSV(week = "Week4-5", file = "wordRT.csv")

# wordRT itself is not a proper data frame, because one row represents
# multiple observations from different speakers. Also see Week 4-5 handout
# for detailed explantion.

# Try to "reshape" the data frame into a data frame in which each row represents
# one observation.
# So we need to use some function from the "reshape" package.
library(reshape)

# The data frame wordRT is transformed using melt(); id.var represents the instances
# of our observations, that is every single word. The rest two columns Male and Female
# actually belongs to one variable "Gender", so in melt(), these column names will be
# placed in the same column named "Gender" as defined by variable.name. Finally, the
# original numeric values will also be placed in one single column, and we will call
# this column "RT", as defined in value.name.
wordRT.new = melt(wordRT, id.var = "Word", variable.name = "Gender", value.name = "RT")
# The function melt() from "reshape" actually does not set the name of the two columns
# representing the variable or the value, so we need to change the column names by
# ourselves.
# colnames() gives you a vector including all column names of a data frame, and then
# you can replace the vector with another vector including new column names.
colnames(wordRT.new) = c("Word", "Gender", "RT")

# A better melt() that helps set the column name is from the package "reshape2"
wordRT = loadCourseCSV(week = "Week4-5", file = "wordRT.csv")

library(reshape2)
wordRT.new = melt(wordRT, id.var = "Word", variable.name = "Gender", value.name = "RT")

# Finally, you can check the data frame itself, and you will see one observation of
# a word per row, in which a word is specified with the speaker who produces it and
# the time in milliseconds spent on producing it.

# Then, you can use nrow() to check how many rows/observation there is in the new data frame.
nrow(wordRT.new)

# Load the Jabberwocky corpus from my server
jabberwocky.wd = loadCourseCSV("Week4-5", "jabberwocky_words.txt")
# Each observation is one word "token" (i.e., individual word) from
# the poem, so the number of rows in the data frame is equal to the number
# of individual word tokens in the poem.
nrow(jabberwocky.wd)

# Convert the Word vector into a "frequency table", which shows how many times
# each unique word type in the Word column occurs in the corpus.
jabberwocky.table = table(jabberwocky.wd$Word)

# Covert the frequency table into a data frame, in which each row represents
# one observation of a unique word type
jabberwocky.df = as.data.frame(jabberwocky.table)

# Change the column names to those that make more sense
colnames(jabberwocky.df) = c("Word", "Count")

# The number of rows = the number of unique word types in this data frame
nrow(jabberwocky.df)
# The sum of the counts are still the total number of word tokens in the corpus
sum(jabberwocky.df$Count)

# Get row numbers based on the values in Count in decreasing order.
order(jabberwocky.df$Count, decreasing = TRUE)

# The first row number returned above is 71, so check if the 71st row of the data frame
# is really the word with the highest count.
jabberwocky.df[71,]

# Store the ordered row numbers in another vector
count.des.ord = order(jabberwocky.df$Count, decreasing = TRUE)

# Put the ordered row numbers to specify the rows of a data frame, so you get a new
# data frame arranged based on the ordered row numbers.
jabberwocky.ord = jabberwocky.df[count.des.ord, ]

# Load another data frame with the word category information for each word type
# in the corpus
jabberwocky.wordCat = 
  loadCourseCSV("Week4-5", "jabberwocky_words_cat.csv")

# Check if each value in POS is NA, and the result is a vector of TRUEs and FALSEs.
pos.na = is.na(jabberwocky.wordCat$POS)
# sum() converts TRUEs into 1s and FALSES into 0s and add all the values together,
# so the output is the total number of NAs in POS.
sum(pos.na)

# Put TRUEs and FALSEs to specify the rows of a data frame, so R selects only the 
# "TRUE" rows for you.
jabberwocky.wordCat[pos.na, ]
# Adding ! reverses TRUEs and FALSEs.
jabberwocky.wordCat[!pos.na, ]

# Merge the data frame with counts into the data frame with the word type info
# based on the Word column.
jabberwocky.all = 
  merge(jabberwocky.ord, jabberwocky.wordCat, by = "Word")

# Sort the Count values by the two levels in Real (Y and N), and add all the values
# in Count together for each level so you get the total number of real word and 
# non-word tokens.
xtabs(formula = Count ~ Real, data = jabberwocky.all)

# Count the number of each of the two levels in Real (Y and N), so you get the unique
# types of real words and nonwords.
xtabs(formula = ~ Real, data = jabberwocky.all)

# A get a 2 x 2 Contingency Table that show the number of each of the four possible
# combinations between the two levels in Real (Y and N) and the two levels in POS (Cont and Func)
jabberwocky.xtabs = 
  xtabs(formula = ~ Real + POS, data = jabberwocky.all)

# Add all the numbers in the contingency table to get a total count.
jabberwocky.sum = sum(jabberwocky.xtabs)
# Divide the entire table by the sum to get the probability of each combination
jabberwocky.xtabs / jabberwocky.sum
  
# Get the percentages of each combination rounded to the first decimal
jabberwocky.perc = 
  round(x = jabberwocky.xtabs / jabberwocky.sum * 100, digit = 1)

# Load the library for the durationsOnt data frame
library(languageR)
head(durationsOnt)

# Check the distribution info for DurationOfPrefix (the duration of the ont- prefix
# in seconds)
summary(durationsOnt$DurationOfPrefix)

# Get the absolute difference between the mean and median -> very close ->
# probably a normal distribution
abs(median(durationsOnt$DurationOfPrefix) - 
      mean(durationsOnt$DurationOfPrefix))

# Get the absolute difference between the mean and the 1st/3rd quartile -> comparable ->
# probably symmetrical
abs(0.1167 - 0.1581)
abs(0.1581 - 0.1743)

# par() is used to set the layout of the plotting area; see Week 4-5 Handout.
par(mfrow = c(1, 2))
plot(density(durationsOnt$DurationOfPrefix),
     main = "Duration of ont- Prefix (Density)")
abline(v = median(durationsOnt$DurationOfPrefix), col = "blue",
       lwd = 1.5)
abline(v = mean(durationsOnt$DurationOfPrefix), col = "red", 
       lwd = 1.5)

qqnorm(durationsOnt$DurationOfPrefix, 
       main = "Duration of ont- Prefix (Q-Q Plot)")
qqline(durationsOnt$DurationOfPrefix, col = "red", lwd = 1.5)

par(mfrow = c(1, 1))

# Get the probability of observing a value in a normal distribution from
# the area in the lower tail below mean - SD * 2.5
pnorm(q = -2.5)
# Get the probability of observing a value in a normal distribution from
# the areas below mean - SD * 2.5 and above mean + SD * 2.5
pnorm(q = -2.5) * 2
# +-SD * 3 as a stricter threshold
pnorm(q = -3) * 2

# Calculate mean +- SD * 2.5 for the DurationOfPrefix distribution to
# set the boundaries for outliers.
sd.lower = mean(durationsOnt$DurationOfPrefix) -
  sd(durationsOnt$DurationOfPrefix) * 2.5

sd.upper = mean(durationsOnt$DurationOfPrefix) +
  sd(durationsOnt$DurationOfPrefix) * 2.5