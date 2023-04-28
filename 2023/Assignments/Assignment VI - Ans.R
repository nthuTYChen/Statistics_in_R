source("https://lngproc.hss.nthu.edu.tw/statisticsR/courseUtil.R")
phoible.inv = loadCourseCSV("Week7-9", "phoible.inv.csv")
# Include rows with a SegmentN <= 100 AND without a duplicated LanguageName 
phoible.unique = subset(phoible.inv, SegmentN <= 100 
                        & !duplicated(LanguageName))

# The R programming in Task I-VII should be something you're kind of familiar 
# with. You can also check the previous assignments or the midterm for similar 
# tasks and their explanation, or just ask me if you still don't have a clue 
# after reviewing the previous tasks.

# Task I
phoible.area = subset(phoible.unique, macroarea %in% c("Africa", "Australia"))

# Task II
seg.mean = mean(phoible.area$SegmentN)
seg.sd = sd(phoible.area$SegmentN)
phoible.area.clean = subset(phoible.area, SegmentN >= seg.mean - 2 * seg.sd &
                         SegmentN <= seg.mean + 2*seg.sd)

#Task III
# The distribution is naturally right-skewed for two reasons. First,
# although it represents a continuous variable, it has a lower bound, which is
# zero since a language cannot have a negative number of consonants and vowels.
# Second, most of the numeric values are clustered around the means with a few
# that are unusually high. Altogether, the distribution would have a longer
# upper tail and the lower tail.
plot(density(phoible.area.clean$SegmentN))

# Task IV
# log transformation makes the distance similar between small numeric values and
# between large numeric values. As a result, the short lower tail would be 
# slightly elongated and the long upper tail would be significantly reduced.
# Altogether, the changes make a right-skewed distribution more normal like.

# Create a new variable/column in phoible.area.clean, and store log-transformed
# segment numbers into it.
phoible.area.clean$SegmentN.log = log(phoible.area.clean$SegmentN)

# Task V
# Although the center looks like a camel's back with two peaks, it is indeed
# more symmetrical on both sides.
plot(density(phoible.area.clean$SegmentN.log))

# Task VI
SegmentN.mu = mean(phoible.area.clean$SegmentN.log)

# Task VII
phoible.afr = subset(phoible.area.clean, macroarea == "Africa")
phoible.aus = subset(phoible.area.clean, macroarea == "Australia")

# Task VIII
# For phoible.afr
# Get the sample mean
afr.mean = mean(phoible.afr$SegmentN.log)
# Get the sample SD
afr.sd = sd(phoible.afr$SegmentN.log)
# Get the number of data points
afr.n = nrow(phoible.afr)
# Calculate SE
afr.se = afr.sd / sqrt(afr.n)
# Calculate t, t = 19.984
afr.t = (afr.mean - SegmentN.mu) / afr.se
# Get one-tailed p; t is positive, so we need to get the probability
# in the upper tail.
afr.p = pt(q = afr.t, df = afr.n - 1, lower.tail = FALSE)
# Get two-tailed p; p < .001
afr.p * 2
# Same output using t-test()
t.test(phoible.afr$SegmentN.log, mu = SegmentN.mu)

# For phoible.aus
# Similar calculations for a different sample
aus.mean = mean(phoible.aus$SegmentN.log)
aus.sd = sd(phoible.aus$SegmentN.log)
aus.n = nrow(phoible.aus)
aus.se = aus.sd / sqrt(aus.n)
aus.t = (aus.mean - SegmentN.mu) / aus.se # -32.53
# t is negative, so we get the one-tailed probability in the lower tail first.
aus.p = pt(q = aus.t, df = aus.n - 1, lower.tail = TRUE) # p < .001
aus.p * 2 # p < .001
t.test(phoible.aus$SegmentN.log, mu = SegmentN.mu)

# Bonus Task IX
# Explain "SegmentN.log" by "macroarea" in a boxplot. "ylim" is set to expand
# the y-axis a little bit for a better visual effect.
boxplot(formula = SegmentN.log ~ macroarea, data = phoible.area.clean,
        ylim = c(2.5, 4.5), xlab = "Macroareas", ylab = "Segment Numbers (log scale)",
        main = "Log-transformed Segment Numbers by Macroarea")
# Add a red horizontal line representing the assumed poplulation mean, lwd =
# line width
abline(h = SegmentN.mu, col = "red", lwd = 2)