source("https://lngproc.hss.nthu.edu.tw/statisticsR/courseUtil.R")

wordRT = loadCourseCSV(week = "Week4-5", file = "wordRT.csv")

library(reshape)

wordRT.new = melt(wordRT, id.var = "Word", variable.name = "Gender", value.name = "RT")
colnames(wordRT.new) = c("Word", "Gender", "RT")

nrow(wordRT.new)