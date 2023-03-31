source("https://lngproc.hss.nthu.edu.tw/statisticsR/Week4-5/AssignmentIV.R")

# Task I
# Get the distribution density information
vh.density = density(vh.subj$WMTotal)

# Generate the density plot with a proper title
plot(vh.density, main = "Density Plot (WM Scores)")

# Task II
# There are a few participants whose WM scores are much lower than 
# the peak of the density plot, so the distribution is skewed to the left.

# Task III
# Extract observations (i.e., participants) with a WM score > 0
vh.nozero = subset(vh.subj, WMTotal > 0)

# Task IV
# You're allowed to use sd() and mean() (or summary())
sd(vh.nozero$WMTotal)	# 7.439758
mean(vh.nozero$WMTotal)	# 96.85

# Task V
# +2.5SD from mean, 115.449
wm.upper = mean(vh.nozero$WMTotal) + sd(vh.nozero$WMTotal) * 2.5
# -2.5SD from mean, 78.251
wm.lower = mean(vh.nozero$WMTotal) - sd(vh.nozero$WMTotal) * 2.5

# Task VI
# Extract the subject of vh.subj, in which WMTotal must be within
# the range 78.251 >= WMTotal <= 115.449
vh.sub = subset(vh.nozero, WMTotal <= wm.upper & WMTotal >= wm.lower)

# Task VII
# Generate another density plot for vh.sub
vh.den.sub = density(vh.sub$WMTotal)
# Generate the density plot with a proper title
plot(vh.den.sub, main = "Density Plot (WM Scores, no outliers)")

# The distribution is still slightly left-skewed, but it is also very like
# a normal distribution already.

# Task VIII
# For the mean; "lty" is not specified for the default line type
abline(v = mean(vh.sub$WMTotal), col = "red")
abline(v = median(vh.sub$WMTotal), col = "blue", lty = 2)
abline(v = mean(vh.sub$WMTotal) + sd(vh.sub$WMTotal), col = "green", lty = 3)
abline(v = mean(vh.sub$WMTotal) - sd(vh.sub$WMTotal), col = "green", lty = 3)