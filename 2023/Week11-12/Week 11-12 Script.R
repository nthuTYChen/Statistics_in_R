# Load the course script
source("https://lngproc.hss.nthu.edu.tw/statisticsR/courseUtil.R")

# Get the sample dataset from Myers (2015) again for the F-test
Myers.clean = loadCourseCSV(week = "Week7-9", file = "MyersClean.csv")

# Get RTs from the two different sessions in the "wordlikeness"
# experiment, so we can compare the variances in the two samples
# using a F-test.
s1.rt = Myers.clean[Myers.clean$Session == 1, ]$logRT
s2.rt = Myers.clean[Myers.clean$Session == 2, ]$logRT

# Run F-test and get the full report.
var.test(s1.rt, s2.rt)

# Get the number of data points in each sample, which minus 1 is 
# the degree of freedom for each sample variance.
length(s1.rt)
length(s2.rt)

# Get the variance for each sample and calculate the ratio,
# which is also the F-value in the F-test.
var(s1.rt) / var(s2.rt)

# Manually retrieve a one-tailed p-value from an F-distribution
# with the ratio and two degrees of freedom. Since the ratio is
# above 1 (the assumed population mean), get the one-tailed p
# from the upper tail first.
pf(q = 1.160577, df1 = 6750, df2 = 7200, lower.tail = FALSE)
# Get the two-tailed p.
pf(q = 1.160577, df1 = 6750, df2 = 7200, lower.tail = FALSE) * 2

# A simulated dataset of Saito & Lyster's (2012) study of how form-
# focused teaching could help with the learning English r for Japanese
# speakers.
sl.sim = loadCourseCSV(week = "Week10-11", file = "SaitoLysterSim.csv")

# Convert the variable/column with characters into a "factor" using
# as.factor(), which helps divide our dataset into subsets based on
# the "unique labels", or formally known as "levels/categories".
# We can use levels() to check the levels of a factor.
levels(as.factor(sl.sim$Group))

sl.sim.aov = aov(formula = rF3 ~ Group, data = sl.sim)
summary(sl.sim.aov)

5345500 / 107131

mean.grand = mean(sl.sim$rF3)
mean.control = mean(sl.sim[sl.sim$Group == "Control",]$rF3)
mean.ffi = mean(sl.sim[sl.sim$Group == "FFI",]$rF3)
mean.fficf = mean(sl.sim[sl.sim$Group == "FFI+CF",]$rF3)

ss.control = (mean.control - mean.grand) ^ 2
ss.ffi = (mean.ffi - mean.grand) ^ 2
ss.fficf = (mean.fficf - mean.grand) ^ 2

control.n = nrow(subset(sl.sim, Group == "Control"))
ffi.n = nrow(subset(sl.sim, Group == "FFI"))
fficf.n = nrow(subset(sl.sim, Group == "FFI+CF"))

ss.between = control.n * ss.control + ffi.n * ss.ffi + fficf.n * ss.fficf

control.rF3 = sl.sim[sl.sim$Group == "Control",]$rF3
ffi.rF3 = sl.sim[sl.sim$Group == "FFI",]$rF3
fficf.rF3 = sl.sim[sl.sim$Group == "FFI+CF",]$rF3

ss.control.within = sum((control.rF3 - mean.control) ^ 2)
ss.ffi.within = sum((ffi.rF3 - mean.ffi) ^ 2)
ss.fficf.within = sum((fficf.rF3 - mean.fficf) ^ 2)

ss.within = sum(ss.control.within, ss.ffi.within, ss.fficf.within)

sl.sim.sub = subset(sl.sim, Group != "FFI+CF")
sl.sim.sub.aov = aov(formula = rF3 ~ Group, data = sl.sim.sub)
summary(sl.sim.sub.aov)

t.test(formula = rF3 ~ Group, data = sl.sim.sub, var = TRUE)

TukeyHSD(sl.sim.aov)

sl.rep.sim = loadCourseCSV(week = "Week10-11", 
                           file = "SaitoLysterRepSim.csv")

sl.rep.avg = aggregate(rF3 ~ Subject + Condition, FUN = mean, 
                       data = sl.rep.sim)

sl.rep.aov = aov(formula = rF3 ~ Condition, data = sl.rep.avg)
summary(sl.rep.aov)

sl.rep.avg$Subject = as.factor(sl.rep.avg$Subject)
sl.rep.aov.good = aov(formula = rF3 ~ Condition + 
                        Error(Subject / Condition), data = sl.rep.avg)
summary(sl.rep.aov.good)

TukeyHSD(sl.rep.aov.good)

alpha = .05 / 3

sl.cl.ffi = subset(sl.rep.avg, Condition != "FFI+CF")
t.test(formula = rF3 ~ Condition, paired = T, data = sl.cl.ffi)

sl.cl.fficf = subset(sl.rep.avg, Condition != "FFI")
t.test(formula = rF3 ~ Condition, paired = T, data = sl.cl.fficf)

sl.ffi.fficf = subset(sl.rep.avg, Condition != "Control")
t.test(formula = rF3 ~ Condition, paired = T, data = sl.ffi.fficf)