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

# Check the density plot of the raw word frequency to take a look at a 
# Zipf's distribution: See p.22 of the Unit 3 handout for explanation
plot(density(durationsOnt$Freq.raw))

# Small intervals between small numbers are converted into equal intervals
# close to 0.693... after log-transformation.
log(c(0.5, 1, 2, 4, 8))
# LARGE intervals between LARGE numbers are also converted into equal intervals
# closee to 0.693... after log-transformation.
log(c(200, 400, 800))

# In sum, log-transformation helps significantly reduce the long upper tail and
# strech the lower tail a little bit, and that's why the distribution of log 
# frequency is more normal-like.
plot(density(durationsOnt$Frequency))

# Explore the possibility that high-frequency words have a phonetically shorter
# ont- prefix

# Divide log frequencies by median, ONLY FOR THE PURPOSE OF DATA EXPLORATION
# ifelse() returns "Highs" or "Low" based on the results of the "test", namely
# whether the values in Frequency is higher than or equal to the median of
# Frequency: If TRUE, then return the "yes" value, and if FALSE, return the "Low"
# value.
durationsOnt$Freq.cat = ifelse(test = durationsOnt$Frequency >= 
                                 median(durationsOnt$Frequency),
                               yes = "High", no = "Low")

# Divide DurationOfPrefix values in durationsOnt by the two levels of Freq.cat,
# and apply the mean() function to each subgroup (i.e., calculate the mean of
# Frequency for "High" and "Low" groups).
aggregate(x = DurationOfPrefix ~ Freq.cat, FUN = mean, data = durationsOnt)
# Divide DurationOfPrefix values in durationsOnt by the two levels of Freq.cat,
# and apply the sd() function to each subgroup (i.e., calculate the SD of
# Frequency for "High" and "Low" groups).
aggregate(x = DurationOfPrefix ~ Freq.cat, FUN = sd, data = durationsOnt)

# The means of the prefix duration are close for "High" and "Low" ont- words,
# and the SD shows that the ranges of prefix duration greatly overlap for "High"
# and "Low" ont- words. So, it doesn't seem that "High" words have a significantly
# shorter ont- prefix than the "Low" words.

# Directly correlate one continuous variable with another continuous variable
# using cor().
# Pearson's correlation coefficient = -0.02... It's close to zero, which suggests
# no correlation between log frequency and the duration of the ont- prefix
cor(durationsOnt$Frequency, durationsOnt$DurationOfPrefix)

# Explore binary/categorical data based on Myers' (2015) study. See p.15-16 of 
# the Unit 3 handout for the detailed explanation of the study.
Myers.sample = loadCourseCSV(2024, "3_Data", "Myers_2015_Sample.csv")

# Nonwords without a valid response have a zero RT, which are useless.
# Use subset() to extract the rows in Myers.sample who's RT is zero. 
Myers.noResp = subset(Myers.sample, RT == 0)
# There are only seven nonwords in the subset
Myers.noResp
# Use nrow() to confirm the number of invalid responses.
nrow(Myers.noResp)

# What we need are nonwords with a valid responses, namely the rows whose RT
# is above zero.
Myers.resp = subset(Myers.sample, RT > 0)
nrow(Myers.resp)

# But responses made within the first 200ms after seeing a nonword were probably
# accidental (e.g., press a response key by accident), so they might not provide
# useful information and should be dropped.
Myers.noShortRT = subset(Myers.resp, RT > 200)
# More than 2,000 responses were dropped in this screening process.
nrow(Myers.resp) - nrow(Myers.noShortRT)

# Check the distribution of raw RTs: The distribution is severely rightly skewed.
# For the reason why, see p.17 of the Unit 3 handout
plot(density(Myers.noShortRT$RT))

# Right skewness could be remedied with log-transformation.
Myers.noShortRT$logRT = log(Myers.noShortRT$RT)
# The distribution of log-RT is not perfectly normal, but it looks much better
# than the distribution of raw RTs
plot(density(Myers.noShortRT$logRT))

# Exclude outlier log-RTs that go beyond +-2.5SD from the mean, because unusually
# slow/fast responses might be meaningless, too.
Myers.logRT.mean = mean(Myers.noShortRT$logRT)
Myers.logRT.sd = sd(Myers.noShortRT$logRT)

# Extract rows whose logRT has a value within the range -2.5SD<--Mean-->+2.5SD
Myers.sd25 = subset(Myers.noShortRT, 
                    logRT > Myers.logRT.mean - Myers.logRT.sd * 2.5 &
                      logRT < Myers.logRT.mean + Myers.logRT.sd * 2.5)

# 200+ observations with an outlier RT were dropped.
nrow(Myers.noShortRT) - nrow(Myers.sd25)

# Because binary responses were coded as 1 vs. 0 in Response, we can calculate
# the average of Response by ItemID to get the average acceptability of each
# nonword using aggregate(). Item_ZhuyinFuhao is equal to ItemID but is still 
# included here as another grouping variable so ZhuyinFuhao can be included
# in the output data frame as well. This part was updated to the Unit 3 handout
# right before the Week 5 lecture on Oct 4, so make sure that you download the
# most updated handout and check p.19.
Myers.acc = aggregate(x = Response ~ ItemID + Item_ZhuyinFuhao, 
                      FUN = mean, data = Myers.sd25)

# Sort the data frame based on average acceptabilities in decreasing order. 
acc.row.sorted = order(Myers.acc$Response, decreasing = T)
Myers.acc.ord = Myers.acc[acc.row.sorted,]
# Check the six most acceptable nonwords
head(Myers.acc.ord)
# Check the six least acceptable nonwords
tail(Myers.acc.ord)

# Examine the "satiation effect" in Myers' (2015) study. See p.19-20 for detailed
# explanations

# Directly correlate Response (0 vs. 1) to Session (1 vs. 2) since both variables
# are numeric in a way. However, the correlation coefficient is close to 0,
# which means that acceptability patterns do not change from Session 1 to 2.
# Does that mean that there's no satiation effect?
cor(Myers.sd25$Response, Myers.sd25$Session)

# Calculate response patterns by Participant ID and Session number, so we can
# obtain "by-subject" variation across the two experiment sessions. It turns
# out participants demonstrate opposite satiation effects; see p.20 of the Unit 3
# handout for explanations.
aggregate(Response ~ Session + Participant, FUN = mean, data = Myers.sd25)

# Plotting your data

# Starting with the Jabberwocky corpus and make sure that you have loaded the 
# required datasets 

#head(jabberwocky.wd)
#head(jabberwocky.table)

# Since there are too many word types in the Jabberwocky frequency table,
# we extract the subset with a token frequency above 1.
jabberwocky.table.2 = jabberwocky.table[jabberwocky.table > 1]

# Barplot is the best choice for visualizing word frequency for reasons 
# specified on p.21 of the Unit 3 handout.

# The "height" parameter takes a table, so the height of each bar
# is determined by the value of each column in the table. We need to adjust
# the range of y-axis by specifying "ylim", so the scale of the y-axis covers 
# the highest  token frequency (i.e., 19) in the table. Other parameters are
# explained on p.21 of the handout.
barplot(height = jabberwocky.table.2, ylim = c(0, 20),
        main = "Jabberwocky Word Count (Token Freq > 1)", xlab = "",
        ylab = "Count", las = 2)

# Mosaic plot is the best choice for visualizing "the distribution of counts";
# in the Jabberwocky corpus, the number of word types varies by whether a word
# type represents a real word or not and whether a word type represents a
# content word or a function word.

# We need the data from these variables from the first half of the script

#head(jabberwocky.all)
#jabberwocky.xtabs

# The special parameter used in mosaicplot() is "color", and check p.23 of the
# handout for explanations.
mosaicplot(x = jabberwocky.xtabs, main = "Jabberwocky Word Types Distribution",
           xlab = "Read Word", ylab = "Word Category", 
           color = c("white", "grey40"))

# Next we try to plot continuous data, namely log-RT in Myers' (2015) study

# head(Myers.resp)
Myers.resp$logRT = log(Myers.resp$RT)

# A box plot is the best choice for visualizing continuous data since it shows
# all the distributional properties of the data: Median, the range from the 
# 1st quartile to the 3rd quartile, an extended range based on IQR, and outliers.
# Check p.24 of the handout to see how to read a box plot.

# In this demo, we plot log RTs grouped by Session, thus the y ~ x syntax again.
boxplot(logRT ~ Session, data = Myers.resp, 
        main = "Wordlikeness Judgment Latency", xlab = "Session",
        ylab = "log-transformed Reaction Times", ylim = c(0, 10))

# Finally, we need a scatter plot to show the correlation between two continuous
# variable.

#library(languageR)
#head(durationsOnt)

# We see the y ~ x syntax again, because in a correlation test, we want to know
# if y changes with x. In this case, we once again explore the correlation between
# the duration of the ont- prefix and log word frequency. Note that DurationOfPrefix
# is indeed mapped to the y-axis and Frequency to the x-axis in the scatter plot!
# We also expand the range of the y-axis a little bit to (1) leave some blanks
# at the top and the bottom to make our plot more readable and (2) reduce the 
# distance between individual data points to make a weak trend more visible.

plot(DurationOfPrefix ~ Frequency, data = durationsOnt, 
     main = "Frequency-Duration Correlation in durationsOnt",
     xlab = "log Word Frequency per Million Words", 
     ylab = "ont- Prefix Duration (s)", ylim = c(0, 0.3))

# Check if male speakers and female speakers show a similar correlation. See
# p.25 for the explanation of the assumption as well as detailed explanations
# of the R codes below.
dursOnt.m = subset(durationsOnt, Sex == "male")
dursOnt.f = subset(durationsOnt, Sex == "female")

plot(DurationOfPrefix ~ Frequency, data = dursOnt.m,
     main = "Frequency-Duration Correlation by Gender",
     xlab = "log Word Frequency per Million Words",
     ylab = "ont- Prefix Duration (s)", xlim = c(0, 7), ylim = c(0, 0.3))

points(DurationOfPrefix ~ Frequency, data = dursOnt.f, pch = 2, col = "red")

legend(title = "Sex", legend = c("Male", "Female"), pch = c(1, 2), 
       col = c("black", "red"), x = "bottom", ncol = 2, bty = "n", cex = 0.8)