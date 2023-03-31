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

# Judging by whether DurationOfPrefix is lower than sd.lower
# OR higher than sd.upper, you get TRUEs and FALSEs to indication
# if each value in DurationOfPrefix is an outlier
outliers = durationsOnt$DurationOfPrefix < sd.lower |
  durationsOnt$DurationOfPrefix > sd.upper

# TRUEs are converted into 1s and FALSES into 0s in sum(), so you
# get the total number of outliers (only 1).
sum(outliers)

# Use TRUEs and FALSES in "outliers" to select "TRUE" rows from
# the data frame durationsOnly.
durationsOnt[outliers, ]

# The "Frequency" is in fact log-transformed (natural log)
head(durationsOnt$Frequency)

# Convert log-transformed frequencies back to raw frequencies
# with exp() since the oppsite of logs is exponents.
durationsOnt$Frequency.raw = exp(durationsOnt$Frequency)

# Visualize a right-skewed raw frequency distribution following
# the Zipf's Law (many low-frequency words and a few high-frequency words)
par(mfrow = c(1, 2))
plot(density(durationsOnt$Frequency.raw), 
     main = "Raw Frequency of ont- Words (Density)")
qqnorm(durationsOnt$Frequency.raw, 
       main = "Raw Frequency of ont- Words (Q-Q Plot)")
qqline(durationsOnt$Frequency.raw, col = "red", lwd = 1.5)

# Compare the difference between small numbers to those between big numbers
# before and after log-transformation; the distance is made equal between
# small numbers and between large numbers after log-transformation.
log(c(0.5, 1, 2, 4, 8))
log(c(200, 400, 800))

# Visualize a normal-like log frequency distribution
plot(density(durationsOnt$Frequency), 
     main = "Log Frequency of ont- Words (Density)")
qqnorm(durationsOnt$Frequency,
       main = "Log Frequency of ont- Words (Q-Q Plot)")
qqline(durationsOnt$Frequency, col = "red", lwd = 1.5)
par(mfrow = c(1, 1))

# Categorize words based on whether its log frequency is higher
# than or equal to the median log frequency.
# ifelse() returns a vector of "yes" and "no" values based on
# the results of the "test", and the vector is stored into the
# Frequency.cat variable in durationsOnt data frame.
durationsOnt$Frequency.cat = 
  ifelse(test = durationsOnt$Frequency >= 
           median(durationsOnt$Frequency),
         yes = "High", no = "Low")

# Sort DurationOfPrefix data based on the frequency category 
# (High vs. Low) and apply the mean() function to calculate
# the average log frequency for each category.
aggregate(x = DurationOfPrefix ~ Frequency.cat,
          FUN = mean, data = durationsOnt)

# Sort DurationOfPrefix data based on the frequency category 
# (High vs. Low) and apply the sd() function to calculate
# the standard deviation of log frequency for each category.
aggregate(x = DurationOfPrefix ~ Frequency.cat,
          FUN = sd, data = durationsOnt)

# Get Pearson's correlation coefficient; a small negative value
# means a negative correlation between word frequency and the
# duration of the ont- prefix.
cor(durationsOnt$Frequency, durationsOnt$DurationOfPrefix)

# Make sure that you have jabberwocky.wd loaded into your R
# Generate the frequency table of jabberwocky again.
jabberwocky.table = table(jabberwocky.wd$Word)
# Sort the frequency counts by a decreasing order
jabberwocky.table = 
  jabberwocky.table[order(jabberwocky.table, decreasing = T)]

# Exclude data entries with a frequency count higher than 1
# Again, the condition "jabberwocky.table > 1" gives you
# TRUEs and FALSES, and putting them into [] of a table means...?
jabberwocky.table.2 = jabberwocky.table[jabberwocky.table > 1]

# See Week 4-5 handout for the detailed explanations of the 
# data visualization process below.
barplot(height = jabberwocky.table.2, 
        main = "Jabberwocky Word Count (Token Freq > 2)",
        xlab = "", ylab = "Count", ylim = c(0, 20), las = 2)

jabberwocky.df = as.data.frame(jabberwocky.table)
colnames(jabberwocky.df) = c("Word", "Count")

# Make sure that you have loadCourseCSV() ready, too.
jabberwocky.wordCat = 
  loadCourseCSV("Week4-5", "jabberwocky_words_cat.csv")
jabberwocky.all = 
  merge(jabberwocky.df, jabberwocky.wordCat, by = "Word")
jabberwocky.xtabs = xtabs(~ Real + POS, jabberwocky.all)

mosaicplot(x = jabberwocky.xtabs, 
           main = "Jabberwocky Word Types Dsitribution",
           xlab = "Real Word", ylab = "Part of Speech",
           color = c("white", "grey40"))

boxplot(DurationOfPrefix ~ Frequency.cat, data = durationsOnt,
        main = "Prefix Duration by Frequency", 
        xlab = "log Frequency divided by Median",
        ylab = "Duration in ms")

boxplot(durationsOnt$SpeechRate, 
        main = "Speech Rate Distribution",
        ylab = "Syllable Number per Second")

boxplot(durationsOnt$SpeechRate, 
        main = "Speech Rate Distribution",
        ylab = "Syllable Number per Second", outline = F)

plot(DurationOfPrefix ~ Frequency, data = durationsOnt,
     main = "Frequency-Duration Correlation in durationsOnt",
     xlab = "log Word Frequency per Million",
     ylab = "ont- Prefix Duration (s)", ylim =c(0, 0.3))

dursOnt.m = subset(durationsOnt, Sex == "male")
dursOnt.f = subset(durationsOnt, Sex == "female")

plot(DurationOfPrefix ~ Frequency, data = dursOnt.m,
     main = "Frequency-Duration Correlation by Gender",
     xlab = "log Word Frequency per Millon",
     ylab = "ont- Prefix Duration (s)", ylim =c(0, 0.3))

points(DurationOfPrefix ~ Frequency, data = dursOnt.f,
       pch = 2, col = "red")

legend(title = "Sex", legend = c("Male", "Female"), 
       pch = c(1, 2), col = c("black", "red"), x = "bottom",
       ncol = 2, bty = "n", cex = 0.8)

library(ggplot2)

jabberwocky.2.df = as.data.frame(jabberwocky.table.2)
colnames(jabberwocky.2.df) = c("Word", "Count")

ggplot(data = jabberwocky.2.df, 
       mapping = aes(x = Word, y = Count)) +
  geom_bar(stat = "identity") + coord_flip() +
  scale_y_continuous(expand = c(0.01, 0.01), limits = c(0, 20)) +
  labs(x = "Word", y = "Token Frequency", 
       title = "Jabberwocky Corpus", 
       caption = "Token Frequency > 1 Only") + theme_bw()

jabberwocky.2.df$Frequency = 
  ifelse(jabberwocky.2.df$Count > 2, yes = "> 2", no = "= 2")

ggplot(data = jabberwocky.2.df, 
       mapping = aes(x = Word, y = Count, fill = Frequency)) +
  geom_bar(stat = "identity") + coord_flip() +
  scale_y_continuous(expand = c(0.01, 0.01), limits = c(0, 20)) +
  labs(x = "Word", y = "Token Frequency", 
       title = "Jabberwocky Corpus", 
       caption = "Token Frequency > 1 Only") + theme_bw()

ggplot(data = durationsOnt, 
       mapping = aes(x = Frequency.cat, y = DurationOfPrefix)) +
  geom_boxplot() + 
  geom_point(mapping = aes(color = Frequency.cat), 
             alpha = 0.5, size = 3,
             position = position_jitterdodge(jitter.width = 0.5)) +
  guides(color = "none")

ggplot(data = durationsOnt, 
       mapping = aes(x = Frequency, y = DurationOfPrefix)) +
  geom_point(color = "grey40", size = 3, alpha = 0.7) +
  facet_grid(. ~ Sex) +
  labs(title = "Correlation between Frequency and ont- Prefix Duration",
       caption = "Word frequency per million is log-transformed",
       x = "Word Frequency", y = "Durations (s)") + theme_bw()