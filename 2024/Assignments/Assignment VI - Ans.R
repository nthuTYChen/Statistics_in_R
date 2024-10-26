# Preparation Work
source("https://raw.githubusercontent.com/nthuTYChen/Statistics_in_R/main/courseUtil.R")
Myers.sub = loadCourseCSV(2024, "4_Parametric", "MyersSub.csv")

# Just to quickly check how many observations there are.
nrow(Myers.sub)

# Task 1
# Exclude the observations with a zero RT first.
Myers.sub.nozero = subset(Myers.sub, RT > 0)

# Convert raw RTs into logRTs first because raw RTs are not normally distributed.
# You can only properly estimate outliers only in a normal-like distribution.
Myers.sub.nozero$logRT = log(Myers.sub.nozero$RT)

# Calculate the mean and the SD of logRTs
logRT.mean = mean(Myers.sub.nozero$logRT)
logRT.sd = sd(Myers.sub.nozero$logRT)

# I use mean+-2.5SD to be the range for including non-outliers.
Myers.sub.clean = subset(Myers.sub.nozero, logRT < logRT.mean + 2.5 * logRT.sd |
                           logRT > logRT.mean - 2.5 * logRT.sd)

# Check how many observations there are after excluding outliers and zero-RT 
# observations
nrow(Myers.sub.clean)

# Task 2
# Calculate by-item average acceptability rate using aggregate(). If you only
# use the syntax Response ~ ItemID, the output does not have the three critical
# columns required for the later analysis. So, we can use the syntax 
# Response ~ ItemID + ASCII + Item_ZhuyinFuhao + OnsetC to include all three columns.
# The output still contain by-item average accetability rates, since Response is
# firstly averaged by ItemID, and ItemID = ASCII = Item_ZhuyinFuhao. OnsetC has
# no effect since ItemID is already the smallest unit for calculating the averages.
Myers.sub.avg = aggregate(Response ~ ItemID + ASCII + Item_ZhuyinFuhao + OnsetC, 
                          FUN = mean, data = Myers.sub.clean)
# Sort the data frame following the average acceptability rates in Response.
Myers.sub.avg = Myers.sub.avg[order(Myers.sub.avg$Response),]

# Task 3
# Since my dataset was sorted based on Response in INCREASING order, I have to
# list the last 20 rows to show the 20 nonwords with the highest average acceptability
# rates. In my list, 12 nonwords begin with [p] and 8 begin with [ʐ]. So, perhaps
# a nonword is really more acceptable if it begins with [p]...?
tail(Myers.sub.avg, 20)

# If you use the Response ~ ItemID syntax in Task 2, you need to merge two datasets
# to include ASCII, Item_ZhuyinFuhao, and OnsetC in your Myers.sub.avg:

# Merge Myers.sub.clean with Myers.sub.avg by the ItemID column. Here I only include
# the four columns in Myers.sub.clean which are crucial for the merger process.
# Myers.sub.avg = merge(Myers.sub.avg, 
#    Myers.sub.clean[c("ItemID", "ASCII", "Item_ZhuyinFuhao", "OnsetC")], 
#    by = "ItemID")

# Task 4
# Load the package first
library(ggplot2)

# The best choice is obviously a boxplot in which you can demonstrate the distribution
# of continuous data. I'm just taking this chance to demonstrate how to generate
# a "violin" plot, in which the curves reflect the amount of data points in a 
# specific range (narrower = fewer). A violine plot could be combined with a 
# boxplot to combine the benefits of two.
ggplot(data = Myers.sub.avg, mapping = aes(x = OnsetC, y = Response)) +
  geom_violin() + geom_boxplot(width = 0.1) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Mean Acceptability Rates in Myers (2015)", x = "Onset Consonant",
       y = "Mean Acceptability Rate", 
       caption = "Z = voiced retroflex fricative\np = voiceless aspirated bilabial stop") +
  theme_bw()

# Task 5
# Save the two subgroups into separate objects
Myers.onset.z = subset(Myers.sub.avg, OnsetC == "Z")
Myers.onset.p = subset(Myers.sub.avg, OnsetC == "p")
# Show the distributional properties of mean acceptability rates using summary()
summary(Myers.onset.z$Response)
summary(Myers.onset.p$Response)
# The two distributions almost perfectly overlap. They have similar means and
# identical 1st/3rd quartiles. Thus, the two distributions do not seem to be
# significantly different from each other, which suggests that the two groups
# of nonwords have similar mean acceptability rates.

# Part II
# We know the assumed population mean (220), and we have our sample to calculate
# the sample mean and the sample SD. So, we can use a one-sample t-test to
# tell whether Bo is a weirdo based on its(?) 15 lengths of [a].
# Below, I demonstrate this task with manual calculation.

# Create a vector for the vowel lengths using c()
bo.a.len = c(150, 275, 113, 268, 175, 241, 134, 146, 145, 237, 174, 185, 196, 211, 195)

# Calculate the sample mean/SD
bo.mean = mean(bo.a.len)
bo.sd = sd(bo.a.len)

# Population mean and sample size
pop.mean = 220
bo.n = length(bo.a.len)

# Get the t-value
bo.t = (bo.mean - pop.mean) / (bo.sd / sqrt(bo.n)) # -2.392678

# Since the t-value is negative, we can get the lower-tail p first
bo.lower.p = pt(q = bo.t, df = bo.n - 1)

# The hypothesis is NON-DIRECTIONAL because Bo assumes himself to be weird,
# but it(?) does not assume himself to be weird "in which way".
# Thus, we need to calculate a two-tailed p.
bo.lower.p * 2

# The two-tailed p-value is lower than .05, the default alpha level, so yeah,
# Bo is a weirdo.

# Validate the p-value using t.test()
t.test(x = bo.a.len, mu = 220)