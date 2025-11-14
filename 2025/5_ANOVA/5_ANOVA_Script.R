source("https://raw.githubusercontent.com/nthuTYChen/Statistics_in_R/refs/heads/main/courseUtil.R")
Myers.clean = loadCourseCSV(2025, "5_ANOVA", "MyersClean.csv")

s1.rt = Myers.clean[Myers.clean$Session == 1,]$logRT
s2.rt = Myers.clean[Myers.clean$Session == 2,]$logRT

var.test(s1.rt, s2.rt)

var(s1.rt)
var(s2.rt)

var(s1.rt) / var(s2.rt)

length(s1.rt)
length(s2.rt)

pf(q = 1.160577, df1 = 6750, df2 = 7200, lower.tail = F) * 2

sl.sim = loadCourseCSV(2025, "5_ANOVA", "SaitoLysterSim.csv")

sl.sim.aov = aov(formula = rF3 ~ Group, data = sl.sim)
summary(sl.sim.aov)

5345500 / 107131

mean.grand = mean(sl.sim$rF3)
mean.control = mean(sl.sim[sl.sim$Group == "Control",]$rF3)
mean.ffi = mean(sl.sim[sl.sim$Group == "FFI",]$rF3)
mean.fficf = mean(sl.sim[sl.sim$Group == "FFI+CF",]$rF3)

control.n = nrow(subset(sl.sim, Group == "Control"))
ffi.n = nrow(subset(sl.sim, Group == "FFI"))
fficf.n = nrow(subset(sl.sim, Group == "FFI+CF"))

ss.between = control.n * (mean.control - mean.grand) ^ 2 +
  ffi.n * (mean.ffi - mean.grand) ^ 2 +
  fficf.n * (mean.fficf - mean.grand) ^ 2
ss.between

ss.between / (3 - 1)

ss.control = sum((sl.sim[sl.sim$Group == "Control",]$rF3 - mean.control) ^ 2)
ss.ffi = sum((sl.sim[sl.sim$Group == "FFI",]$rF3 - mean.ffi) ^ 2)
ss.fficf = sum((sl.sim[sl.sim$Group == "FFI+CF",]$rF3 - mean.fficf) ^ 2)
ss.within = sum(ss.control, ss.ffi, ss.fficf)
ss.within

ss.within / (control.n + ffi.n + fficf.n - 3)

pf(q = 49.9, df1 = 2, df2 = 637, lower.tail = F) * 2

sl.sim.sub = subset(sl.sim, Group != "FFI+CF")
sl.sim.sub.aov = aov(rF3 ~ Group, data = sl.sim.sub)
summary(sl.sim.sub.aov)

t.test(formula = rF3 ~ Group, data = sl.sim.sub, var = TRUE)

1.3268 ^ 2

t.test(formula = rF3 ~ Group, data = subset(sl.sim, Group != "FFI+CF"))
t.test(formula = rF3 ~ Group, data = subset(sl.sim, Group != "Control"))
t.test(formula = rF3 ~ Group, data = subset(sl.sim, Group != "FFI"))

TukeyHSD(sl.sim.aov)

sl.rep.sim = loadCourseCSV(2025, "5_ANOVA", "SaitoLysterRepSim.csv")
str(sl.rep.sim)

sl.rep.avg = aggregate(rF3 ~ Subject + Condition, FUN = mean, data = sl.rep.sim)

sl.rep.aov = aov(formula = rF3 ~ Condition, data = sl.rep.avg)
summary(sl.rep.aov)

sl.rep.avg$Subject = as.factor(sl.rep.avg$Subject)
sl.rep.aov.rp = aov(rF3 ~ Condition + Error(Subject / Condition),
                    data = sl.rep.avg)
summary(sl.rep.aov.rp)

mean.grand = mean(sl.rep.avg$rF3)
ss.total = sum((sl.rep.avg$rF3 - mean.grand) ^ 2)
ss.total

26222 + 48823
18996 + 26222 + 29827

control.n = nrow(subset(sl.rep.avg, Condition == "Control"))
ffi.n = nrow(subset(sl.rep.avg, Condition == "FFI"))
fficf.n = nrow(subset(sl.rep.avg, Condition == "FFI+CF"))

mean.control = mean(sl.rep.avg[sl.rep.avg$Condition == "Control",]$rF3)
mean.ffi = mean(sl.rep.avg[sl.rep.avg$Condition == "FFI",]$rF3)
mean.fficf = mean(sl.rep.avg[sl.rep.avg$Condition == "FFI+CF",]$rF3)

ss.between = control.n * (mean.control - mean.grand) ^ 2 +
  ffi.n * (mean.ffi - mean.grand) ^ 2 + fficf.n * (mean.fficf - mean.grand) ^ 2
ss.between

sl.subj.mean = aggregate(rF3 ~ Subject, FUN = mean, data = sl.rep.avg)
sl.subj.mean

ss.subj = 3 * sum((sl.subj.mean$rF3 - mean.grand) ^ 2)
ss.subj

ss.total - ss.between - ss.subj
ss.total - ss.between

TukeyHSD(sl.rep.aov.rp)