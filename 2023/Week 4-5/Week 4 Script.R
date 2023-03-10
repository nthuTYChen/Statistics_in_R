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