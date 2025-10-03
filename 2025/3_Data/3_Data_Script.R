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

pref.dur.mean = mean(durationsOnt$DurationOfPrefix)
pref.dur.sd = sd(durationsOnt$DurationOfPrefix)

pref.dur.upper = pref.dur.mean + pref.dur.sd * 2.5
pref.dur.lower = pref.dur.mean - pref.dur.sd * 2.5

outliers = durationsOnt$DurationOfPrefix < pref.dur.lower |
  durationsOnt$DurationOfPrefix > pref.dur.upper

sum(outliers)

durationsOnt[outliers, 1:6]

durationsOnt$DurPref.sd = 
  (durationsOnt$DurationOfPrefix - pref.dur.mean) / pref.dur.sd

outliers.sd = durationsOnt$DurPref.sd < -2.5 | 
  durationsOnt$DurPref.sd > 2.5

sum(outliers.sd)

durationsOnt[outliers.sd, 1:6]

durationsOnt$Freq.raw = exp(durationsOnt$Frequency)

head(durationsOnt$Freq.raw)

par(mfrow = c(1, 2))
plot(density(durationsOnt$Freq.raw),
     main = "Raw Frequency of ont- Words (Density)")
qqnorm(durationsOnt$Freq.raw,
       main = "Raw Frequency of ont- Words (Q-Q Plot)")
qqline(durationsOnt$Freq.raw, col = "red", lwd = 1.5)
par(mfrow = c(1, 1))

par(mfrow = c(1, 2))
plot(density(durationsOnt$Frequency),
     main = "Log Frequency of ont- Words (Density)")
qqnorm(durationsOnt$Frequency,
       main = "Log Frequency of ont- Words (Q-Q Plot)")
qqline(durationsOnt$Frequency, col = "red", lwd = 1.5)
par(mfrow = c(1, 1))

log(c(0.5, 1, 2, 4, 8))
log(c(200, 400, 800))

durationsOnt$Freq.cat = 
  ifelse(test = durationsOnt$Frequency >= median(durationsOnt$Frequency),
         yes = "High", no = "Low")

head(durationsOnt[c("Word", "Frequency", "Freq.cat")])

aggregate(x = DurationOfPrefix ~ Freq.cat, FUN = mean, data = durationsOnt)
aggregate(x = DurationOfPrefix ~ Freq.cat, FUN = sd, data = durationsOnt)

cor(durationsOnt$Frequency, durationsOnt$DurationOfPrefix)

Myers.sample = loadCourseCSV(2025, "3_Data", "Myers_2015_Sample.csv")

Myers.noResp = subset(Myers.sample, RT == 0)
head(Myers.noResp)
nrow(Myers.noResp)

Myers.resp = subset(Myers.sample, RT > 0)
nrow(Myers.resp)

Myers.noShortRT = subset(Myers.resp, RT > 200)
nrow(Myers.noShortRT) - nrow(Myers.resp)

plot(density(Myers.noShortRT$RT), main = "Raw RT Distribution in Myers (2015)")

Myers.noShortRT$logRT = log(Myers.noShortRT$RT)

plot(density(Myers.noShortRT$logRT), 
     main = "Log RT Distribution in Myers (2015)")

Myers.logrt.mean = mean(Myers.noShortRT$logRT)
Myers.logrt.sd = sd(Myers.noShortRT$logRT)

Myers.noShortRT$logRT.sd = 
  (Myers.noShortRT$logRT - Myers.logrt.mean) / Myers.logrt.sd

Myers.sd25 = subset(Myers.noShortRT, logRT.sd >= -2.5 & logRT.sd <= 2.5)
nrow(Myers.sd25) - nrow(Myers.noShortRT)

plot(density(Myers.sd25$logRT), 
     main = "Log RT without Outliers in Myers (2015)")

Myers.acc = aggregate(x = Response ~ ItemID + Item_ZhuyinFuhao,
                      FUN = mean, data = Myers.sd25)
head(Myers.acc)

acc.row.sorted = order(Myers.acc$Response, decreasing = T)
head(acc.row.sorted)

Myers.acc.ord = Myers.acc[acc.row.sorted,]
head(Myers.acc.ord)
tail(Myers.acc.ord)

aggregate(Response ~ Session + Participant, FUN = mean, data = Myers.sd25)

jabberwocky.wd = loadCourseCSV(2025, "3_Data", "jabberwocky_words.txt")
head(jabberwocky.wd)
jabberwocky.table = table(jabberwocky.wd$Word)
head(jabberwocky.table)

jabberwocky.table2 = jabberwocky.table[jabberwocky.table > 1]
head(jabberwocky.table2)

jabberwocky.table.ord = order(jabberwocky.table2, decreasing = T)
head(jabberwocky.table.ord)

jabberwocky.table3 = jabberwocky.table2[jabberwocky.table.ord]
head(jabberwocky.table3)

barplot(height = jabberwocky.table3, 
        main = "Jabberwocky Word Count (Token Frequency > 1)",
        ylab = "Raw Token Frequency", ylim = c(0, 20), las = 2)

jabberwocky.df = as.data.frame(jabberwocky.table)
head(jabberwocky.df)
colnames(jabberwocky.df) = c("Word", "Count")

jabberwocky.wordCat = loadCourseCSV(2025, "3_Data", "jabberwocky_words_cat.csv")
head(jabberwocky.wordCat)

jabberwocky.all = merge(jabberwocky.df, jabberwocky.wordCat, by = "Word")
head(jabberwocky.all)

jabberwocky.xtabs = xtabs(~ Real + Cat, jabberwocky.all)
jabberwocky.xtabs

mosaicplot(x = jabberwocky.xtabs)