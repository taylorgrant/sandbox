## Calculating needed sample size for two-sample, independent proportions

# toy data - some rate of conversion between groups 

conversion <- matrix(c(10,120,190,1880,200,2000),ncol=2,byrow=TRUE)
colnames(conversion) <- c("A","B")
rownames(conversion) <- c("Convert","Skip", "Total")
conversion <- as.table(conversion)
conversion

# A conversion rate 10/200 ~ 5%
# B conversion rate 120/2000 ~ 6%

# How many participants in each sample do we need to determine significance
# choose significance level of 5% and a power of 80%

power.prop.test(p1 = .05, p2 = .06, power=0.8, sig.level=0.05)

# Two-sample comparison of proportions power calculation 
# 
# n = 8157.731
# p1 = 0.05
# p2 = 0.06
# sig.level = 0.05
# power = 0.8
# alternative = two.sided
# 
# NOTE: n is number in *each* group