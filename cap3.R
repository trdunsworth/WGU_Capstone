dfull3 <- read_csv("full_set_3.csv") 
dfull3 <- na.omit(dfull3)

ls(dfull3)

attach(dfull3)

dfull3$Agency <- as.factor(dfull3$Agency)
dfull3$Month <- as.factor(dfull3$Month)
dfull3$DOW <- as.factor(dfull3$DOW)
dfull3$Shift <- as.factor(dfull3$Shift)
dfull3$Problem <- as.factor(dfull3$Problem)
dfull3$Call_Received <- as.factor(dfull3$Call_Received)
dfull3$Week_No <- as.factor(dfull3$WeekNo)

summary(dfull3)

dfull3$QueueTime <- as.numeric(dfull3$Fixed_Time_CallEnteredQueue - dfull3$PhonePickUp)
dfull3$DispatchTime <-as.numeric(dfull3$Time_First_Unit_Assigned - dfull3$Fixed_Time_CallEnteredQueue)
dfull3$CallTime <- as.numeric(dfull3$Fixed_Time_CallTakingComplete - dfull3$PhonePickUp)
dfull3$ProcessTime <- as.numeric(dfull3$Time_First_Unit_Assigned - dfull3$PhonePickUp)

dfull3 = dfull3[!dfull3$QueueTime<0, ]
dfull3 = dfull3[!dfull3$DispatchTime<0, ]
dfull3 = dfull3[!dfull3$CallTime<0, ]
dfull3 = dfull3[!dfull3$ProcessTime<0, ]

YearTbl <- table(dfull3$Year)
MonthTbl <- table(dfull3$Month)
WeekTbl <- table(dfull3$WeekNo)
DowTbl <- table(dfull3$DOW)
ShiftTbl <- table(dfull3$Shift)
HourTbl <- table(dfull3$Hour)
PriNoTbl <- table(dfull3$Priority_Number)
ProbTbl <- table(dfull3$Problem)
AgencyTbl <- table(dfull3$Agency)
CRTbl <- table(dfull3$`Call Received`)

median(dfull3$QueueTime)
median(dfull3$DispatchTime)
median(dfull3$CallTime)
median(dfull3$ProcessTime)

# normality tests
# Density Plots
ggdensity(dfull3$QueueTime, fill = "lightblue")
ggdensity(dfull3$DispatchTime, fill = "lightblue")
ggdensity(dfull3$CallTime, fill = "lightblue")
ggdensity(dfull3$ProcessTime, fill = "lightblue")

# QQ Plots
ggqqplot(dfull3$QueueTime)
ggqqplot(dfull3$DispatchTime)
ggqqplot(dfull3$CallTime)
ggqqplot(dfull3$ProcessTime)

# Anderson-Darling Normality Tests - P < 0.05 = non-normal distribution
ad.test(dfull3$QueueTime)
ad.test(dfull3$DispatchTime)
ad.test(dfull3$CallTime)
ad.test(dfull3$ProcessTime)

describeBy(QueueTime + DispatchTime + CallTime + ProcessTime ~ Year, data = dfull3)
describeBy(QueueTime + DispatchTime + CallTime + ProcessTime ~ Year + Agency, data = dfull3)
describeBy(QueueTime + DispatchTime + CallTime + ProcessTime ~ Year + Month, data = dfull3)
describeBy(QueueTime + DispatchTime + CallTime + ProcessTime ~ Year + Priority_Number, data = dfull3)
describeBy(QueueTime + DispatchTime + CallTime + ProcessTime ~ Year + Agency, data = dfull3)

tapply(dfull3$QueueTime, dfull3$Year, median)

median(QueueTime ~ Year, data = dfull3)
median(DispatchTime ~ Year, data = dfull3)
median(CallTime ~ Year, data = dfull3)
median(ProcessTime ~ Year, data = dfull3)

median(QueueTime ~ Year + Shift, data = dfull3)
median(DispatchTime ~ Year + Shift, data = dfull3)
median(CallTime ~ Year + Shift, data = dfull3)
median(ProcessTime ~ Year, data = dfull3)

median(QueueTime ~ Year + WeekNo, data = dfull3)
median(DispatchTime ~ Year + WeekNo, data = dfull3)
median(CallTime ~ Year + WeekNo, data = dfull3)
median(ProcessTime ~ Year + WeekNo, data = dfull3)

median(QueueTime ~ Year + Agency, data = dfull3)
median(DispatchTime ~ Year + Agency, data = dfull3)
median(CallTime ~ Year + Agency, data = dfull3)
median(ProcessTime ~ Year + Agency, data = dfull3)

median(QueueTime ~ Year + DOW, data = dfull3)
median(DispatchTime ~ Year + DOW, data = dfull3)
median(CallTime ~ Year + DOW, data = dfull3)
median(ProcessTime ~ Year + DOW, data = dfull3)

median(QueueTime ~ Year + Month, data = dfull3)
median(DispatchTime ~ Year + Month, data = dfull3)
median(CallTime ~ Year + Month, data = dfull3)
median(ProcessTime ~ Year + Month,data = dfull3)

median(QueueTime ~ Year + Hour, data = dfull3)
median(DispatchTime ~ Year + Hour, data = dfull3)
median(CallTime ~ Year + Hour, data = dfull3)
median(ProcessTime ~ Year + Hour, data = dfull3)

median(QueueTime ~ Year + Priority_Number, data = dfull3)
median(DispatchTime ~ Year + Priority_Number, data = dfull3)
median(CallTime ~ Year + Priority_Number, data = dfull3)
median(ProcessTime ~ Year + Priority_Number, data = dfull3)

median(QueueTime ~ Year + `Call Received`, data = dfull3)
median(DispatchTime ~ Year + `Call Received`, data = dfull3)
median(CallTime ~ Year + `Call Received`, data = dfull3)
median(ProcessTime ~ Year + `Call Received`, data = dfull3)

median(QueueTime ~ Year + Problem, data = dfull3)
median(DispatchTime ~ Year + Problem, data = dfull3)
median(CallTime ~ Year + Problem, data = dfull3)
median(ProcessTime ~ Year + Problem, data = dfull3)

median(QueueTime ~ Year + Shift + WeekNo, data = dfull3)
median(DispatchTime ~ Year + Shift + WeekNo, data = dfull3)
median(CallTime ~ Year + Shift + WeekNo, data = dfull3)
median(ProcessTime ~ Year + Shift + WeekNo, data = dfull3)

median(QueueTime ~ Year + Agency + WeekNo, data = dfull3)
median(DispatchTime ~ Year + Agency + WeekNo, data = dfull3)
median(CallTime ~ Year + Agency + WeekNo, data = dfull3)
median(ProcessTime ~ Year + Agency + WeekNo, data = dfull3)

median(QueueTime ~ Year + Agency + Shift + WeekNo, data = dfull3)
median(DispatchTime ~ Year + Agency + Shift + WeekNo, data = dfull3)
median(CallTime ~ Year + Agency + Shift + WeekNo, data = dfull3)
median(ProcessTime ~ Year + Agency + Shift + WeekNo, data = dfull3)

kruskal.test(QueueTime ~ Year, data = dfull3)
kruskal_effsize(QueueTime ~ Year, data = dfull3)
pairwise.wilcox.test(QueueTime, Year, p.adjust.method = "BH", data = dfull3)
dunn_shift <- dunn_test(dfull3, QueueTime ~ Year, p.adjust.method = "bonferroni")

kruskal.test(DispatchTime ~ Year, data = dfull3)
kruskal_effsize(DispatchTime ~ Year, data = dfull3)
dunn_disp_wk_kw <- dunn_test(dfull3, DispatchTime ~ Year, p.adjust.method = "bonferroni")

dfull3 %>% kruskal_test(CallTime ~ Year)
dfull3 %>% kruskal_effsize(CallTime ~ Year)
dunn_call_wk_kw <- dfull3 %>% dunn_test(CallTime ~ Year, p.adjust.method = "bonferroni")

dfull3 %>% kruskal_test(QueueTime ~ Problem)
dfull3 %>% kruskal_effsize(QueueTime ~ Problem)
dunn_q_problem <- dfull3 %>% dunn_test(QueueTime ~ Problem, p.adjust.method = "bonferroni")

scheirerRayHare(QueueTime ~ Year + WeekNo, data = dfull3, verbose = TRUE)
dunn_q_wkno <- dunn_test(QueueTime ~ Week_No, data = dfull3, p.adjust.method = "holm", detailed = TRUE)
scheirerRayHare(DispatchTime ~ Year + WeekNo, data = dfull3, verbose = TRUE)
dunn_dsp_wk <- dunn_test(DispatchTime ~ Week_No, data = dfull3, p.adjust.method = "holm", detailed = TRUE)

scheirerRayHare(QueueTime ~ Year + Agency + WeekNo, data = dfull3)
dunn_q_agcy <- dunn_test(QueueTime ~ Agency, data = dfull3, p.adjust.method = "holm", detailed = TRUE)
