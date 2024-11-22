# Part I
# Preparation works
library(languageR)
head(lexdec)

# Task 1
# Extract the subset with correct responses only (Correct == "correct")
lexdec.corr = subset(lexdec, Correct == "correct")

# Task 2
# Exclude RT outliers beyond mean+-2.5SD. RTs are already log-transformed
# in the data set, so there's no need to convert them again. But here, I'm showing
# that this task can be done more easily by firstly converting all raw values
# into z-scores, so you can just exclude observations based on RT's z-scores.
lexdec.corr$RT.z = scale(lexdec.corr$RT)
lexdec.no.out = subset(lexdec.corr, RT.z <= 2.5 & RT.z >= -2.5)

# Calculate the proportion of outliers
# nrow(lexdec.no.out) = the number of non-outliers
# nrow(lexdec.corr) = the number of outliers AND non-outliers
1 - nrow(lexdec.no.out) / nrow(lexdec.corr) # 0.02634881 = 2.6%

# Task 3
# Divide the grand data set into two samples by NativeLanguage
lexdec.eng = subset(lexdec.no.out, NativeLanguage == "English")
lexdec.noeng = subset(lexdec.no.out, NativeLanguage != "English")

# Manually run an unpaired t-test
# Get the mean/variance/size of both RT samples
lexdec.eng.m = mean(lexdec.eng$RT)
lexdec.noeng.m = mean(lexdec.noeng$RT)

lexdec.eng.var = var(lexdec.eng$RT)
lexdec.noeng.var = var(lexdec.noeng$RT)

lexdec.eng.n = nrow(lexdec.eng)
lexdec.noeng.n = nrow(lexdec.noeng)

# When we assume an equal variance, we can adopt the simpler version of the
# unpaired two-sample t-test formula in the Unit 4 handout
lexdec.rt.t = (lexdec.eng.m - lexdec.noeng.m) / 
  sqrt(lexdec.eng.var / lexdec.eng.n + lexdec.noeng.var / lexdec.noeng.n)
# t = -11.5836

# Get the p-value using pt(). The degree of freedom is the total number of
# data points minus two. Our research hypothesis is non-directional, so we MUST
# to calculate a two-tailed p-value.
lexdec.rt.df = lexdec.eng.n + lexdec.noeng.n - 2
lexdec.rt.p = pt(q = lexdec.rt.t, df = lexdec.rt.df) * 2
# The unpaired two-sample t-test assuming an equal variance indicates a 
# significant log-RT difference caused by the speakers' L1: t(1550) = -11.584,
# two-tailed p < .001. Thus, the null hypothesis that the two samples divided
# by the speakers' L1 have the same mean RT (i.e., RT difference = 0) should be
# rejected.

# The English speakers were on average faster than non-native speakers
# in correct responses to real English words. It makes sense because 
# speakers should be able to recognize real words in their native language
# more easily.

# Task 4
# Convert log-RTs back to raw RTs using exp() in both subsets
lexdec.eng$rawRT = exp(lexdec.eng$RT)
lexdec.noeng$rawRT = exp(lexdec.noeng$RT)

# Get the mean of the raw RTs in both samples and the SD of all raw RTs for
# Cohen's d.
lexdec.eng.m.rawrt = mean(lexdec.eng$rawRT)
lexdec.noeng.m.rawrt = mean(lexdec.noeng$rawRT)
# Combine the two raw RT vectors into one vector
lexdec.rawrt.all = c(lexdec.eng$rawRT, lexdec.noeng$rawRT)
# Cohen's d
abs(lexdec.eng.m.rawrt - lexdec.noeng.m.rawrt) / sd(lexdec.rawrt.all)
# 0.58 - A medium-to-large effect size.

# Task 5
# Get the grand log-RT mean
lexdec.grand.m = mean(lexdec.no.out$RT)

# Get the SS_between and MS
SS.bet = sum(
  lexdec.eng.n * (lexdec.eng.m - lexdec.grand.m) ^ 2, 
  lexdec.noeng.n * (lexdec.noeng.m - lexdec.grand.m) ^ 2
  )
# 5.377706
# There are only two levels in the factor NativeLanguage, 
# so the df for SS_between is 2 - 1 = 1
df.bet = 2 - 1
MS.bet = SS.bet / df.bet
# 5.377706

# Get the SS_within and MSE
SS.within = sum(
  (lexdec.eng$RT - lexdec.eng.m) ^ 2, # For the English speaker sample
  (lexdec.noeng$RT - lexdec.noeng.m) ^ 2 # For the non-English speaker sample
)
# 59.11306
# The degree of freedom is the total sample size minus two because we have two
# samples here.
df.within = lexdec.eng.n + lexdec.noeng.n - 2
MSE = SS.within / df.within
# 0.03813746

# Get the F value
lexdec.f = MS.bet / MSE
# 141.0085

# Get the UPPER-TAIL p value: The point is to see if the interesting variance
# is substantially higher than the boring variance.
lexdec.f.p = pf(q = lexdec.f, df1 = df.bet, df2 = df.within, lower.tail = F)
# p = 3445144e-31 < .001

# Validate the stats with aov()
lexdec.rt.aov = aov(RT ~ NativeLanguage, data = lexdec.no.out)
summary(lexdec.rt.aov)

# Report the results following the APA format:
# To test the hypothesis that speakers' native language affects how fast they
# responded correctly to real English word, we ran a one-way independent measures
# ANOVA, in which log RT was the dependent variable and Native Language was the 
# only fixed factor with two levels (English vs. Others). The analysis suggested
# a significant main effect of Native Language (F(1, 1550) = 141.01, p < .001).

# Run an unpaired two-sample t-test ASSUMING AN EQUAL VARIANCE
# You need to set "var" or "var.equal" to be TRUE!
t.test(RT ~ NativeLanguage, data = lexdec.no.out, var.equal = T)
# Same degree of freedom; 1550
# t(1550) ^ 2 = F(1, 1550)
(-11.875) ^ 2
# An unpaired two-sample t-test ASSUMING AN EQUAL VARIANCE is indeed a special
# case of a one-way independent-measures ANOVA

# Part II
library(ggplot2)

# The key point here is to map the second factor to a different element in
# the plot, such as the colors for filling the boxes, so the data can be further
# divided into subgroups within each language level to show the potential
# interaction.
ggplot(data = lexdec.no.out, mapping = aes(x = NativeLanguage, y = RT, 
                                           fill = PrevType)) +
  geom_boxplot() + 
  scale_y_continuous(limits = c(5.5, 7.5)) +
  labs(title = "Lexical Decision Time", subtitle = "Correct responses to real English words only",
       x = "Participant L1", y = "log Reaction Time", fill = "Previous Test Item") +
  theme_bw()

# According to the box plot, if the previous test item is also a real word, then
# correct responses to real English words are indeed faster than when the
# previous test item is not a real word. However, this seems true for both
# native and non-native English speakers. Put differently, there seems to be
# no interaction between NativeLanguage and PrevType on visual inspection.