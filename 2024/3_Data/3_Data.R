# Starting in this unit, we always start by loading this script I created for you,
# which load a function loadCourseCSV() to help retrieve data sets from my GitHub
# repository
source("https://raw.githubusercontent.com/nthuTYChen/Statistics_in_R/main/courseUtil.R")

# Your specify the year, topic, and the filename in loadCourseCSV() to load a
# particular data set from my GitHub repository

# In wordRT, each row has TWO reaction time observations, and the column names 
# Male and Female are actually the two levels of the variable Gender, so we need
# and transform wordRT into another data frame in which every row represents
# one RT observation.
wordRT = loadCourseCSV(year = 2024, topic = "3_Data", file = "wordRT.csv")

# We'll use the melt() function to do the transformation, which is part of 
# the "reshape2" package.
library(reshape2)

# In melt(), we specify three parameters: "id.var" means the variable that
# serves as the identifier of our observations. In wordRT, our identifer is
# the Word column because our observations are RTs of each individual words.
# "variable.name" is the name of the variable that represents non-identifier 
# column names in the original data frame (i.e., Male and Female), which is Gender.
# "value.name" is the name of the column in the new data frame that contains the 
# values in non-identifier columns in the original data frame. Since these values
# represent reaction times, we can just call it RT.
wordRT.new = melt(data = wordRT, id.var = "Word", variable.name = "Gender",
                  value.name = "RT")

# After transformation, each row represents one observation on the reaction time
# of a word from a speaker of a specific gender. In addition, the number of rows
# in the new data frame is equivalent to the number of observastions.
nrow(wordRT.new)

# Working on the corpus/count data: Jabberwocky from Alice in Wonderland
# Load a list of individual word tokens
jabberwocky.wd = loadCourseCSV(2024, "3_Data", "jabberwocky_words.txt")

# The number of rows in this data frame represents the number of word tokens
# in the Jabberwocky corpus.
nrow(jabberwocky.wd)

# Use table() to generate a frequency table of the Word column of the corpus.
jabberwocky.table = table(jabberwocky.wd$Word)
# The frequency table shows the number of tokens of each word TYPE in the corpus.
head(jabberwocky.table)

# We can convert the frequency table into a data frame using as.data.frame().
# In the output, the column names in the frequency table are sorted into the
# column Var1, and the token frequencies into the column Freq.
jabberwocky.df = as.data.frame(jabberwocky.table)
head(jabberwocky.df)

# Replace the original column names ["Var1", "Freq"] with another vector
# ["Word", "Count"] to have more proper column names in the data frame.
colnames(jabberwocky.df) = c("Word", "Count")

# In this new data frame, each row represents an observation on a word type
# in the corpus and the token number of the word type.
nrow(jabberwocky.df)
# The sum of token frequencies is still equivalent to the original corpus size,
# namely the number of word tokens in the corpus.
sum(jabberwocky.df$Count)

# Apply the order() function to the Count column of the new data frame to get
# the number of rows sorted based on the Count values in decreasing order.
order(jabberwocky.df$Count, decreasing = T)
# The first row number in the above output is 71, and the 71st row in the
# data frame is the word type with the highest token frequency (the, 19 times)
jabberwocky.df[71,]
# The first row number in the above output is 2, and the 2nd row in the
# data frame is the word type with the second-highest token frequency (and, 14 times)
jabberwocky.df[2,]

# Save the re-ordered row numbers separately.
count.des.ord = order(jabberwocky.df$Count, decreasing = T)

# Use the re-ordered row numbers to sort the original data frame and get a re-ordered
# data frame.
jabberwocky.ord = jabberwocky.df[count.des.ord,]
head(jabberwocky.ord)

# Load another data frame in which each word type in Jabberwocky is marked with
# information regarding whether the word is a real word or not and whether it is
# a function word or a content word.
jabberwocky.wordCat = loadCourseCSV(2024, "3_Data", "jabberwocky_words_cat.csv")

# I could not identify the word category of some word types in the corpus,
# so in the Cat column, these word types have the value NA (= not applicable)
# is.na() checks if values of a vector is equal to NA and returns TRUEs and FALSES
# The result of checking the Cat column is saved to cat.na first.
cat.na = is.na(jabberwocky.wordCat$Cat)
# Sum all TRUEs to show how many NAs there are in Cat.
sum(cat.na) # 3

# Show the rows whose Cat column has an NA value.
jabberwocky.wordCat[cat.na,]
# Show the rows whose Cat colunm DOES NOT have an NA value.
# ! = Not; Not TRUE = FALSE, Not FALSE = TRUE
head(jabberwocky.wordCat[!cat.na,])

# Save all rows WITHOUT an NA value in the Cat column as a separate data frame.
jabberwocky.wordCat.noNA = jabberwocky.wordCat[!cat.na,]

# Merge the data frame with word frequency info and the data frame with word
# category and real/fake word info. Specify the "by" parameter, so the merge()
# function knows which column to refer to when merging data frames based on the
# same values across the two data frame.
jabberwocky.all = merge(jabberwocky.ord, jabberwocky.wordCat.noNA, by = "Word")

# There are 88 word types after merger because jabberwocky.wordCat.noNA does not
# include the three rows with an NA value in the Cat column
nrow(jabberwocky.all)
# The original data frame with frequency information still has 91 word types
nrow(jabberwocky.ord) 

# Use xtabs() to add values in Count based on whether a word type is real or not
# (N vs. Y)
xtabs(formula = Count ~ Real, data = jabberwocky.all)

# Use xtabs() to count the number of each levels in the Real column to know
# how many real/fake words there are in the data frame
xtabs(formula = ~ Real, data = jabberwocky.all)

# Use xtabs() to create a 2 x 2 contingency table, which shows how many
# combinations of different levels of Real and Cat (e.g., real content word,
# fake function word) in the data set.
jabberwocky.xtabs = xtabs(formula = ~ Real + Cat, data = jabberwocky.all)

# We can sum up all the values in a contingency table and get the number of 
# word types; 88
jabberwocky.sum = sum(jabberwocky.xtabs)

# We can divide the entire contingency table by the sum of its values to
# show the proportion of each combination.
jabberwocky.xtabs / jabberwocky.sum

# Convert proportions into percentages
jabberwocky.perc = jabberwocky.xtabs * 100 / jabberwocky.sum

# Round the percentages to their first decimal place
round(x = jabberwocky.perc, digit = 1)

# Continuous data from the package "languageR"
library(languageR)
# We focus on the durationsOnt data frame. For its details, check its official
# document in R or my Unit 3 handout.
head(durationsOnt[1:7])

# Get the distributional properties of the ont- prefix duration in milliseconds
summary(durationsOnt$DurationOfPrefix)

# Visualize the distribution of the prefix duration
# Set the plotting area to have one row and two columns so we can juxtapose
# a density plot and a Q-Q Plot
par(mfrow = c(1, 2))
# This part should be understandable
plot(density(durationsOnt$DurationOfPrefix), main = "Duration of ont- Prefix (Density)")
abline(v = median(durationsOnt$DurationOfPrefix), col = "red", lwd = 1.5)
abline(v = mean(durationsOnt$DurationOfPrefix), col = "blue", lwd = 1.5)

qqnorm(durationsOnt$DurationOfPrefix, main = "Duration of ont- Prefix (Q-Q Plot)")
qqline(durationsOnt$DurationOfPrefix, col = "red", lwd = 1.5)
# Reset the layout of the plotting area to have one row and one column whenever
# you don't need a multi-column/multi-row layout.
par(mfrow = c(1, 1))

# Common outlier boundaries: +-2.5SD from the mean and +-3SD from mean
# A total chance of 1.2% to randomly sample a value below/above +-2.5SD from the mean
pnorm(q = -2.5) * 2 
# A total chance of 0.27% to randomly sample a value below/above +-3SD from the mean
pnorm(q = -3) * 2

# Calculate the actual outlier boundaries for DurationOfPrefix following the
# +-2.5SD criterion
# Lower boundary = 0.051288...
sd.lower = mean(durationsOnt$DurationOfPrefix) - sd(durationsOnt$DurationOfPrefix) * 2.5
# Upper boundary = 0.246366...
sd.upper = mean(durationsOnt$DurationOfPrefix) + sd(durationsOnt$DurationOfPrefix) * 2.5

# Logical Operators
# Outliers are values of DurationOfPrefix below sd.lower OR(|) those above sd.upper
# The output again includes TRUEs and FALSEs for each value of DurationOfPrefix
outliers = 
  durationsOnt$DurationOfPrefix < sd.lower | durationsOnt$DurationOfPrefix > sd.upper

# There is only one outlier
sum(outliers) # 1

# Check which row has the outlier
durationsOnt[outliers, 1:7]

# The Frequency column of durationsOnt actually contains log frequency
# The opposite of natural logarithm is exponential, so we can use the exp() 
# function to convert log frequencies back to raw frequencies, which are then
# saved to a new column Freq.raw in durationsOnt.
durationsOnt$Freq.raw = exp(durationsOnt$Frequency)
# Show the first six raw frequencies
head(durationsOnt$Freq.raw)