dfq1 <- read_csv("Q1_21.csv")
dfq1 %>% drop_na()
str(dfq1)
spec(dfq1)

dfq1$Agency <- as.factor(dfq1$Agency)
dfq1$Call_Disposition <- as.factor(dfq1$Call_Disposition)
dfq1$Master_Incident_Number <- as.factor(dfq1$Master_Incident_Number)
dfq1$Month <- as.factor(dfq1$Month)
dfq1$DOW <- as.factor(dfq1$DOW)
dfq1$Shift <- as.factor(dfq1$Shift)
dfq1$Problem <- as.factor(dfq1$Problem)
dfq1$Call_Received <- as.factor(dfq1$Call_Received)
dfq1$`Call Taker` <- as.factor(dfq1$`Call Taker`)
dfq1$Dispatcher <- as.factor(dfq1$Dispatcher)
dfq1$Call_Taker <- as.factor(dfq1$Call_Taker)


dfq1$QueueTime <- as.numeric(dfq1$Fixed_Time_CallEnteredQueue - dfq1$Phone_Pick_Up)
dfq1$DispatchTime <-as.numeric(dfq1$Time_First_Unit_Assigned - dfq1$Fixed_Time_CallEnteredQueue)
dfq1$CallTime <- as.numeric(dfq1$End_Call_Taking - dfq1$Phone_Pick_Up)
dfq1$ProcessTime <- as.numeric(dfq1$Time_First_Unit_Assigned - dfq1$Phone_Pick_Up)

dfq1 = dfq1[!dfq1$QueueTime<0, ]
dfq1 = dfq1[!dfq1$DispatchTime<0, ]
dfq1 = dfq1[!dfq1$CallTime<0, ]
dfq1 = dfq1[!dfq1$ProcessTime<0, ]

attach(dfq1)

Q1MonTbl <- table(Month)
Q1WkTabl <- table(WeekNo)
Q1DowTbl <- table(DOW)
Q1HrTbl <- table(Hour)
Q1ShiftTbl <- table(Shift)
Q1AgcyTbl<- table(Agency)
Q1ProbTbl <- table(Problem)
Q1PNTbl <- table(Priority_Number)
Q1RcvTbl <- table(Call_Received)
Q1CTTbl <- table(`Call Taker`)
Q1DSpTbl <- table(Dispatcher)
Q1CDTbl <- table(Call_Disposition)

Q1DowDf <- as.data.frame(Q1DowTbl)

Q1MonxShift <- table(Month, Shift)
q1mxsdf <- as.data.frame(Q1MonxShift)

# normality tests
# Density Plots
ggdensity(dfq1$QueueTime, fill = "lightblue", xlab = "Queue Time")
ggdensity(dfq1$DispatchTime, fill = "lightblue", xlab = "Dispatch Time")
ggdensity(dfq1$CallTime, fill = "lightblue", xlab = "Call Handling Time")
ggdensity(dfq1$ProcessTime, fill = "lightblue", xlab = "Call Processing Time")

# QQ Plots
ggqqplot(dfq1$QueueTime, title = "Queue Time QQ Plot")
ggqqplot(dfq1$DispatchTime, title = "Dispatch Time QQ Plot")
ggqqplot(dfq1$CallTime, title = "Call Handling Time QQ Plot")
ggqqplot(dfq1$ProcessTime, title = "Call Processing Time QQ Plot")

# Anderson-Darling Normality Tests - P < 0.05 = non-normal distribution
ad.test(dfq1$QueueTime)
ad.test(dfq1$DispatchTime)
ad.test(dfq1$CallTime)
ad.test(dfq1$ProcessTime)

ggdensity(dfq1, x = "QueueTime",
          add = "median", rug = TRUE,
          color = "Shift", fill = "Shift",
          palette = c("#00AFBB", "#E7B800"))

ggdensity(dfq1, x = "QueueTime",
          add = "median", rug = TRUE,
          color = "DOW", fill = "DOW",
          palette = get_palette("jco", 7))

gghistogram(dfq1, x = "QueueTime",
            add = "median", rug = TRUE,
            color = "Month", fill = "Month",
            palette = get_palette("jco", 3), bins = 10)

ggboxplot(dfq1, x = "DOW", y = "QueueTime",
          color = "DOW", palette = get_palette("jco", 7),
          add = "jitter")

ggscatter(dfq1, x = "QueueTime", y = "DispatchTime",
          color = "DOW",
          palette = get_palette("jco", 7))

ggbarplot(Q1DowDf, x = "DOW", y = "Freq",
          fill = "DOW",
          palette = "jco",            
          x.text.angle = 90,
          title = "Call Count by Day of Week",
          xlab = "Day of Week",
          ylab = "Call Count",
          label = TRUE,
          lab.pos = "in",
          lab.col = "white"
)

ggbarplot(q1mxsdf, x = "Month", y = "Freq",
          fill = "Shift",
          palette = "jco",
          title = "Monthly Calls by Shift",
          xlab = "Month",
          ylab = "Call Count",
          label = TRUE,
          lab.pos = "in",
          lab.col = "white")

ggdensity(dfq1, x = "DispatchTime", add = "median", rug = TRUE, color = "Shift", fill = "Shift", palette = c("#00AFBB", "#E7B800"))



chisq.test(table(DOW, Shift))
chisq.test(table(Agency, Shift))
fisher.test(Problem, Shift, alternative = "two-sided", simulate.p.value = TRUE)
fisher.test(Problem, DOW, alternative = "two-sided", simulate.p.value = TRUE)

median(QueueTime)
median(DispatchTime)
median(CallTime)
median(ProcessTime)

mean(QueueTime)
mean(QueueTime, 0.1)
mean(QueueTime, 0.2)

mean(DispatchTime)
mean(DispatchTime, 0.1)
mean(DispatchTime, 0.2)

mean(ProcessTime)
mean(ProcessTime, 0.1)
mean(ProcessTime, 0.2)

mean(CallTime)
mean(CallTime, 0.1)
mean(CallTime, 0.2)

median(QueueTime ~ `Call Taker`)
median(DispatchTime ~ Dispatcher)
median(DispatchTime ~ Dispatcher + Priority_Number)

median(QueueTime ~ Agency)
median(DispatchTime ~ Agency)
median(CallTime ~ Agency)
median(ProcessTime ~ Agency)

write.table(Q1ProbTbl, file = "q1_problem_count.txt", sep = ",", quote = FALSE, row.names = F)

ggplot(dfq1) + aes(x = Month, fill = Shift) + geom_bar()
ggplot(dfq1) + aes(x = DOW, fill = Shift) + geom_bar()


median(QueueTime ~ Shift + DOW, data = dfq1)

kruskal.test(QueueTime ~ `Call Taker`, data = dfq1)
kruskal_effsize(QueueTime ~ `Call Taker`, data = dfq1)
pairwise.wilcox.test(QueueTime, `Call Taker`, p.adjust.method = "BH", data = dfq1)
dunn_test(dfq1, QueueTime ~ DOW, p.adjust.method = "bonferroni")
dunn_Q_CT <- dunn_test(dfq1, QueueTime ~ Call_Taker, p.adjust.method = "bonferroni")

detach(dfq1)