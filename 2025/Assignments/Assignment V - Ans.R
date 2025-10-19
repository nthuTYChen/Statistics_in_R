# Preparation work
library(languageR)
head(durationsOnt)

# Task 1
# Both table() and xtabs() can complete this task very easily
# Get a frequency table of the Sex variable
table(durationsOnt$Sex) # 49 females and 53 males
# Get a cross-table with only the Sex variable
xtabs(formula = ~ Sex, data = durationsOnt) # Same numbers

# Task 2
# You need a frequency table for the YearOfBirth variable, so you can use
# barplot() for generating the bar plot for the task. Note that you cannot 
# simply use hist(), because it doesn't show "each specific AD year" by default
# as requested in the instructions.
dur.yob.tab = table(durationsOnt$YearOfBirth)

# Generate the bar plot with a proper title, axis titles, and verticle x-axis
# labels.
barplot(height = dur.yob.tab, 
        main = "The number of speakers by year of birth (AD)",
        xlab = "Year of Birth (AD)", ylab = "Number of Speakers", las = 2)

# Get decreasing order of values in the frequency table 
dur.yob.ord = order(dur.yob.tab, decreasing = T)

# Show the first six values of the reordered table, which naturally includes
# the year with the highest number of speakers - there are 10 born in 1976.
head(dur.yob.tab[dur.yob.ord])

# An easier way, which I didn't share with you in previous lectures, is to use
# sort(), which directly returns ordered values
head(sort(dur.yob.tab, decreasing = T))

# Task 3
# Get the median of YearOfBirth first
dur.yob.median = median(durationsOnt$YearOfBirth)
# Use ifelse() to check if each YearOfBirth value is >= median, which returns
# either "Old" or "Young" based on the check result, and the entire vector is
# stored to the Generation variable.
durationsOnt$Generation = ifelse(durationsOnt$YearOfBirth >= dur.yob.median,
                                 yes = "Young", no = "Old")

# Task 4
# Use aggregate() to sort the continuous variable SpeechRate based on the 
# categorical variable Generation, and then apply the mean() function to each
# subset of SpeechRate.
# For older speakers, it's 5.24 syllables per second on average, and for younger
# speakers, it's 5.82 syllables per second on average, so older speakers are
# faster on average.
aggregate(SpeechRate ~ Generation, FUN = mean, data = durationsOnt)

# Task 5
# Split the entire data set to two subsets based on Generation using subset()
durOnt.old = subset(durationsOnt, Generation == "Old")
durOnt.young = subset(durationsOnt, Generation == "Young")
# Get the SpeechRate mean and SD for each subset; spr = Speech Rate
old.spr.mean = mean(durOnt.old$SpeechRate) # 5.24
old.spr.sd = sd(durOnt.old$SpeechRate) # 1.24
young.spr.mean = mean(durOnt.young$SpeechRate) # 5.82
young.spr.sd = sd(durOnt.young$SpeechRate) # 1.21

# Check the range of the entire SpeechRate variable, so I can set a reasonable
# x-axis limits.
range(durationsOnt$SpeechRate) # 2.46 - 8.48, so I'll choose 2-9 for my limits.

# Start with the older speaker sample
old.spr.den = density(durOnt.old$SpeechRate)
# Need to have everything information ready in the plot() function.
# Note how I specify the colors representing different samples in the title,
# which is followed in the rest of the code.
# It turns out that it is also necessary to adjust the y-axis limit, or the
# peak of the second density curve would exceed the top edge.
plot(old.spr.den, 
     main = "Speech Rate Distribution for Old (Dark) and Young (Red) Speakers",
     xlab = "Syllable per Second", xlim = c(2, 9), ylim = c(0, 0.4),
     col = "black", lwd = 2)

# Add a solid dark grey line (a blackish color) to represent the mean for
# the older speakers, and two dashed dark grey lines to represent the +-2SD for
# this sample. It is important to have visually similar but also distinct cues
# for presenting different information of the same sample.
abline(v = old.spr.mean, col = "darkgrey")
abline(v = old.spr.mean + 2 * old.spr.sd, col = "darkgrey", lty = 2)
abline(v = old.spr.mean - 2 * old.spr.sd, col = "darkgrey", lty = 2)

# Use lines() to add the distribution for the younger speakers.
young.spr.den = density(durOnt.young$SpeechRate)
lines(young.spr.den, col = "red", lwd = 2)

# Add the vertical lines that represent the mean and +-2SD for the younger
# speakers. I choose the red4 color since it is also redish. You can check all
# the color labels you can use in R at: https://r-charts.com/colors/
abline(v = young.spr.mean, col = "red4")
abline(v = young.spr.mean + 2 * young.spr.sd, col = "red4", lty = 2)
abline(v = young.spr.mean - 2 * young.spr.sd, col = "red4", lty = 2)

# Task 6
# According to the density plot, the two sample distributions are more or less
# normal for a fair comparison. The means are very close to each other based on
# the numbers calculated in (4) and the plot generated in (5). In addition, the
# range that covers roughly 95% of the data (i.e., +-2SD from the mean) in each 
# sample largely overlaps. Thus, you can say that the speech rate might NOT be
# significantly different between older and younger speakers.