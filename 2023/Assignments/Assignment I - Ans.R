# Preparation works
library(languageR)
head(verbs)

# 1-1: Get the number of rows of the data frame 'verbs'
nrow(verbs) # 903

# 1-2: Use head() to get the first six values of the AnimacyOfRec variable
#      with the output being a data frame
head(verbs["AnimacyOfRec"])
#AnimacyOfRec
#1      animate
#2      animate
#3      animate
#4      animate
#5      animate
#6      animate

# 1-3: Create a vector with a number sequence ranged from 894 to 903 with 894:903
#      and use it to get the last 10 rows of 'verbs'. Then, use a $ sign to
#      get the values of the Verb column as a vector.
rows = 894:903
verbs[rows, ]$Verb
# [1] sell  sell  sell  sell  give  give  offer pay   give  give

# 1-4: Use '1:5' to denote a number sequence from 1 to 5 to represent the
#      row numbers, and use a $ sign to get the values from the LengthOfTheme
#      column so the output will be a vector.
fiveLengths = verbs[1:5, ]$LengthOfTheme
fiveLengths
# [1] 2.639057 1.098612 2.564949 1.609438 1.098612

# 1-5: Just pass all the values of LengthOfTheme as a vector to mean()
mean(verbs$LengthOfTheme)
# [1] 1.498195

# 1-6: The logic is based on the previous tasks; reversing the order of the two
#      parts is acceptable in this task
verbs[151, ]$LengthOfTheme - verbs[303, ]$LengthOfTheme
# [1] -0.4054651