source("https://lngproc.hss.nthu.edu.tw/statisticsR/courseUtil.R")

Myers.clean = loadCourseCSV(week = "Week7-9", file = "MyersClean.csv")

s1.rt = Myers.clean[Myers.clean$Session == 1, ]$logRT
s2.rt = Myers.clean[Myers.clean$Session == 2, ]$logRT

var.test(s1.rt, s2.rt)

length(s1.rt)
length(s2.rt)

var(s1.rt) / var(s2.rt)

pf(q = 1.160577, df1 = 6750, df2 = 7200, lower.tail = FALSE)
pf(q = 1.160577, df1 = 6750, df2 = 7200, lower.tail = FALSE) * 2

sl.sim = loadCourseCSV(week = "Week10-11", file = "SaitoLysterSim.csv")
levels(as.factor(sl.sim$Group))