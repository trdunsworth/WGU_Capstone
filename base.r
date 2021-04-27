dfull <- read_csv("full_set.csv")
df19 <- read_csv("full_set_2019.csv")
df20 <- read_csv("full_set_2020.csv")

dfull_s1 <- dfull[order(dfull$T2Q), ]
dfull_s2 <- dfull[order(dfull$T2Disp), ]
dfull_s3 <- dfull[order(dfull$ProcTime), ]

sum(dfull$T2Q < 0, na.rm=TRUE)
sum(dfull$T2Disp < 0, na.rm = TRUE)
sum(dfull$ProcTime < 0, na.rm=TRUE)
sum(dfull$T2Q < 0 & dfull$T2Disp < 0, na.rm=TRUE)
sum(dfull$T2Q < 0 & dfull$ProcTime < 0, na.rm=TRUE)
sum(dfull$ProcTime < 0 & dfull$T2Disp < 0, na.rm=TRUE)

create_report(dfull)
summary(dfull)
dim(dfull)
str(dfull)
uv_full <- unique(dfull)
dim(uv_full)
dupv_full <- get_dupes(dfull)
dupv_full
introduce(dfull)
describe(dfull)
makeDataReport(dfull)
ls(dfull)

# Outlier Detection
hist(dfull$T2Q,
     xlab = "Time to Queue",
     main = "Histogram of Time to Queue",
     breaks = sqrt(nrow(dfull))
) # set number of bins

hist(dfull$T2Disp,
     xlab = "Time to Dispatch",
     main = "Histogram of Time to Dispatch",
     breaks = sqrt(nrow(dfull))
) # set number of bins

hist(dfull$ProcTime,
     xlab = "DECC Processing Time",
     main = "Histogram of Processing Time",
     breaks = sqrt(nrow(dfull))
) # set number of bins

ggplot(dfull) +
  aes(x = "", y = T2Q) +
  geom_boxplot(fill = "#1c5789") +
  theme_minimal()

ggplot(dfull) +
  aes(x = "", y = T2Disp) +
  geom_boxplot(fill = "#1c5789") +
  theme_minimal()

ggplot(dfull) +
  aes(x = "", y = ProcTime) +
  geom_boxplot(fill = "#1c5789") +
  theme_minimal()

T2Q_lb <- quantile(dfull$T2Q, 0.01)
T2Q_ub <- quantile(dfull$T2Q, 0.99)
T2Q_lb
T2Q_ub
T2Q_outTest <- grubbs.test(dfull$T2Q)
T2Q_outTest

T2Q_T2D_lb <- quantile(dfull$T2Disp, 0.01)
T2D_ub <- quantile(dfull$T2Disp, 0.99)
T2D_lb
T2D_ub
grubbs.test(dfull$T2Disp)

PT_lb <- quantile(dfull$ProcTime, 0.01)
PT_ub <- quantile(dfull$ProcTime, 0.99)
PT_lb
PT_ub
grubbs.test(dfull$ProcTime)

dfull <- na.omit(dfull)
df19 <- na.omit(df19)

dfull = dfull[!dfull$T2Q<0, ]
dfull = dfull[!dfull$T2Disp<0, ]

# normality tests
# Kolmogorov-Smirnov normality test P < 0.05 = non-normal distribution
ksnormTest(dfull$T2Q)
ksnormTest(dfull$T2Disp)
ksnormTest(dfull$ProcTime)

# Jarque-Bera normality test P < 0.05 = non-normal distribution
jarqueberaTest(dfull$T2Q)
jarqueberaTest(dfull$T2Disp)
jarqueberaTest(dfull$ProcTime)

# D'Agostino normality test P < 0.05 = non-normal distribution
dagoTest(dfull$T2Q)
dagoTest(dfull$T2Disp)
dagoTest(dfull$ProcTime)

# Anderson-Darling Normality Tests - P < 0.05 = non-normal distribution
ad.test(dfull$T2Q)
ad.test(dfull$T2Disp)
ad.test(dfull$ProcTime)

# Lillefors Test
lillieTest(dfull$T2Q)
lillieTest(dfull$T2Disp)
lillieTest(dfull$ProcTime)

# Pearson Chi-Square Normality Test
pchiTest(dfull$T2Q)
pchiTest(dfull$T2Disp)
pchiTest(dfull$ProcTime)


# plots full
ggdensity(dfull$T2Q, main="Density of Time to Queue", xlab="Time to Queue", fill="lightgrey")
ggdensity(dfull$T2Disp, main="Density of Time to Dispatch", xlab = "Time to Dispatch", fill = "lightgrey")
ggdensity(dfull$ProcTime, main = "Density of Processing Time", xlab = "Processing Time", fill = "lightgrey")
ggqqplot(dfull$T2Q)
ggqqplot(dfull$T2Disp)
ggqqplot(dfull$ProcTime)

tapply(dfull$T2Q,dfull$Shift,mean,na.rm=TRUE)
tapply(dfull$T2Q,dfull$DOW,mean,na.rm=TRUE)

attach(dfull)
kruskal.test(T2Q, g = Year)
kruskal.test(T2Disp, g=Year)
kruskal.test(ProcTime, g=Year)

kruskal.test(dfull$T2Q, g=dfull$Hour)
posthoc.kruskal.nemenyi.test(dfull$T2Q, dfull$Hour, data=dfull)
kruskal.test(T2Q, g=Problem)
kruskal.test(dfull$T2Q, g=as.factor(dfull$DOW))
posthoc.kruskal.nemenyi.test(dfull$T2Q, as.factor(dfull$DOW), data=dfull)
boxplot(T2Q ~ DOW, data = dfull, ylab="Time 2 Queue in secs", xlab = "Day of the Week")
pairwise.wilcox.test(dfull$T2Q, g=as.factor(dfull$DOW))
pairwise.wilcox.test(dfull$T2Q, g=as.factor(dfull$DOW), p.adjust.method = "bonferroni")
kruskalmc(dfull$T2Q ~ as.factor(dfull$DOW), probs = .05, cont=NULL)
res.dow <- manova(cbind(T2Q, T2Disp) ~ DOW, data = dfull)
summary(res.dow)
summary.aov(res.dow)
detach(dfull)

wilcox.test(T2Q ~ Shift, data = dfull, exact = FALSE)
wilcox.test(T2Disp ~ Shift, data = dfull, exact = FALSE)
wilcox.test(ProcTime ~ Shift, data = dfull, exact = FALSE)

wilcox.test(T2Q ~ Shift, data = dfull, exact = FALSE, alterative = "greater")
wilcox.test(T2Q ~ Shift, data = dfull, exact = FALSE, alternative = "less")

t2q_rank <- rank(dfull$T2Q)
