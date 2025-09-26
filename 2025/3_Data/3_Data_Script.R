# Starting in this unit, we always start by loading this script I created for you,
# which load a function loadCourseCSV() to help retrieve data sets from my GitHub
# repository
source("https://raw.githubusercontent.com/nthuTYChen/Statistics_in_R/main/courseUtil.R")

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

# Get the last ten rows that represent the word type with the 10 lowest token
# frequency
tail(jabberwocky.ord, 10)

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

# Divide the entire table by the sum to convert raw type frequencies into
# probabilities.
jabberwocky.prob = jabberwocky.xtabs / jabberwocky.sum

# Multiply all probabilities by 100 to get the percentages
jabberwocky.perc = jabberwocky.prob * 100

# Get the percentages and round them to the first decimal place.
jabberwocky.perc = round(jabberwocky.prob * 100, digit = 1)


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

# I skipped the part that adds the lines for the mean and the median of the
# sample distribution. You can certainly do it by yourself.

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

# The bottom line is +-2SD from the mean
# A total chance of 4.6% to randomly sample a value below/above +-2SD from the mean
pnorm(q = -2) * 2