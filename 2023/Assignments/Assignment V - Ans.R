# Preparation works
source("https://lngproc.hss.nthu.edu.tw/statisticsR/courseUtil.R")
phoible.inv = loadCourseCSV("Week7-9", "phoible.inv.csv")
phoible.unique = subset(phoible.inv, SegmentN <= 100 
                        & !duplicated(LanguageName))

# Task I
# Create a vector containing the values to be included in the 
# "macroarea" variable of phoible.unique
areas =  c("Africa", "Eurasia", "Australia", "North America", "Papunesia")
# Use subset() and %in% to extract a subset of phoible.inv
phoible.area = subset(phoible.unique, macroarea %in% areas)

# Task II
# The formula "~ level" means to count the number of every unique values in
# the "level" variable (i.e., "language" and "dialect") of the phoible.area 
# data frame.
xtabs(formula = ~ level, data = phoible.area)

# Task III
# "Aggregate" the SegmentN variable by the macroarea variable using the mean() 
# function
phoible.seg.mean = aggregate(SegmentN ~ macroarea, FUN = mean, data = phoible.area)
# "Aggregate" the SegmentN variable by the macroarea variable using the sd() 
# function
phoible.seg.sd = aggregate(SegmentN ~ macroarea, FUN = sd, data = phoible.area)

# Task IV
# Merge the two data frames with means and SDs together by macroarea
phoible.seg = merge(phoible.seg.mean, phoible.seg.sd, by = "macroarea")

# Task V
# After the merger, there are two SegmentN variable SegmentN.x and SegmentN.y
# since there is SegmentN in both of phoible.seg.mean and phoible.seg.sd so
# SegmentN has to be renamed in the output to avoid the same variable name.
# And since the first data frame is referred to as "x" and the second
# one as "y" in merge(), SegmentN is renamed as SegmentN.x and SegmentN.y
# respectively. But what they really represent is mean and SD respectively,
# so we have to rename the variable names accordingly.
colnames(phoible.seg) = c("Macroarea", "SegmentN.mean", "SegmentN.sd")

# Task VI
# A bar plot is not appropriate to visualize a data distribution because it is
# mainly for count data (i.e., how frequently something is observed) and it does
# not show any information about the distribution. There is also a baseline for
# count data, which is zero, but a data distribution could have zeros or even 
# negative values. Thus, a boxplot is more appropriate since it shows crucial
# information about a distribution, including median, quartiles, and outliers.

# Task VII
# The formula "SegmentN ~ macroarea" means to group segment numbers by macroareas
boxplot(SegmentN ~ macroarea, data = phoible.area, 
        main = "Segment Numbers by Macroarea", xlab = "Macroareas", 
        ylab = "Total Number of Segments")

# Task VIII
# Neither the numbers from Task IV nor the boxplot from Task VII supports the
# hypothesis. The means in Task IV suggest that Australian languages on average
# have fewer segments than African languages. The boxplot also indicates that
# the two distributions of African and Austrialian languages do not really
# overlap with each other, which is in part due to a smaller variance in the
# distribution of Australian languages. Thus, it seems true that African 
# languages do have more segments, which is totally opposite to the hypothesis.