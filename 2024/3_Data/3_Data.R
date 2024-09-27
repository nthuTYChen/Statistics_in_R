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

cat.na = is.na(jabberwocky.wordCat$Cat)
sum(cat.na)

jabberwocky.wordCat[cat.na,]
head(jabberwocky.wordCat[!cat.na,])

jabberwocky.wordCat.noNA = jabberwocky.wordCat[!cat.na,]

jabberwocky.all = merge(jabberwocky.ord, jabberwocky.wordCat.noNA, by = "Word")

nrow(jabberwocky.all)
nrow(jabberwocky.ord)

xtabs(formula = Count ~ Real, data = jabberwocky.all)

xtabs(formula = ~ Real, data = jabberwocky.all)

jabberwocky.xtabs = xtabs(formula = ~ Real + Cat, data = jabberwocky.all)

jabberwocky.sum = sum(jabberwocky.xtabs)

jabberwocky.xtabs / jabberwocky.sum

jabberwocky.perc = jabberwocky.xtabs * 100 / jabberwocky.sum

round(x = jabberwocky.perc, digit = 1)

library(languageR)
head(durationsOnt[1:7])

summary(durationsOnt$DurationOfPrefix)

par(mfrow = c(1, 2))
plot(density(durationsOnt$DurationOfPrefix), main = "Duration of ont- Prefix (Density)")
abline(v = median(durationsOnt$DurationOfPrefix), col = "red", lwd = 1.5)
abline(v = mean(durationsOnt$DurationOfPrefix), col = "blue", lwd = 1.5)

qqnorm(durationsOnt$DurationOfPrefix, main = "Duration of ont- Prefix (Q-Q Plot)")
qqline(durationsOnt$DurationOfPrefix, col = "red", lwd = 1.5)
par(mfrow = c(1, 1))

pnorm(q = -2.5)
pnorm(q = -3)

sd.lower = mean(durationsOnt$DurationOfPrefix) - sd(durationsOnt$DurationOfPrefix) * 2.5
sd.upper = mean(durationsOnt$DurationOfPrefix) + sd(durationsOnt$DurationOfPrefix) * 2.5

# Logical Operators
outliers = 
  durationsOnt$DurationOfPrefix < sd.lower | durationsOnt$DurationOfPrefix > sd.upper

sum(outliers)

durationsOnt[outliers, 1:7]

durationsOnt$Freq.raw = exp(durationsOnt$Frequency)
head(durationsOnt$Freq.raw)