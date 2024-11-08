source("https://raw.githubusercontent.com/nthuTYChen/Statistics_in_R/main/courseUtil.R")

Myers.clean = loadCourseCSV(2024, "5_ANOVA", "MyersClean.csv")

s1.rt = Myers.clean[Myers.clean$Session == 1,]$logRT
s2.rt = Myers.clean[Myers.clean$Session == 2,]$logRT

var.test(s1.rt, s2.rt)

var(s1.rt) / var(s2.rt)
pf(q = 1.160577, df1 = 6750, df2 = 7200, lower.tail = F)
pf(q = 1.160577, df1 = 6750, df2 = 7200, lower.tail = F) * 2

sl.sim = loadCourseCSV(2024, "5_ANOVA", "SaitoLysterSim.csv")

sl.sim.aov = aov(formula = rF3 ~ Group, data = sl.sim)
summary(sl.sim.aov)

5345500 / 107131

mean.global = mean(sl.sim$rF3)

sl.sim.control = subset(sl.sim, Group == "Control")
sl.sim.ffi = subset(sl.sim, Group == "FFI")
sl.sim.fficf = subset(sl.sim, Group == "FFI+CF")

mean.control = mean(sl.sim.control$rF3)
mean.ffi = mean(sl.sim.ffi$rF3)
mean.fficf = mean(sl.sim.fficf$rF3)

control.size = nrow(sl.sim.control)
ffi.size = nrow(sl.sim.ffi)
fficf.size = nrow(sl.sim.fficf)

ss.between = control.size * (mean.control - mean.global) ^ 2 +
  ffi.size * (mean.ffi - mean.global) ^ 2 +
  fficf.size * (mean.fficf - mean.global) ^ 2

df.between = 3 - 1

ss.between / df.between

ss.control = sum((sl.sim.control$rF3 - mean.control) ^ 2)
ss.ffi = sum((sl.sim.ffi$rF3 - mean.ffi) ^ 2)
ss.fficf = sum((sl.sim.fficf$rF3 - mean.fficf) ^ 2)
ss.within = ss.control + ss.ffi + ss.fficf

df.within = control.size - 1 + ffi.size - 1 + fficf.size - 1

ss.within / df.within

pf(q = (ss.between / df.between) / (ss.within / df.within), df1 = df.between,
   df2 = df.within, lower.tail = F)

sl.sim.sub = subset(sl.sim, Group != "FFI+CF")
sl.sim.sub.aov = aov(formula = rF3 ~ Group, data = sl.sim.sub)
summary(sl.sim.sub.aov)

t.test(formula = rF3 ~ Group, data = sl.sim.sub, var.equal = T)

1.3268 ^ 2

bonferroni.a = .05 / 3

t.test(sl.sim.fficf$rF3, sl.sim.control$rF3, var.equal = T)
t.test(sl.sim.ffi$rF3, sl.sim.control$rF3, var.equal = T)
t.test(sl.sim.fficf$rF3, sl.sim.ffi$rF3, var.equal = T)

TukeyHSD(sl.sim.aov)