dfull3 <- read_csv("full_set_3.csv") 
dfull3 <- na.omit(dfull3)

df_20 <- na.omit(df_20)

df_med_qywk <- read_csv("med_q_y_wk.csv")
df_med_dywk <- read_csv("med_d_y_wk.csv")
df_med_cywk <- read_csv("med_c_y_wk.csv")
df_med_pywk <- read_csv("med_p_y_wk.csv")

ls(dfull3)

attach(dfull3)

dfull3$Agency <- as.factor(dfull3$Agency)
dfull3$Month <- factor(dfull3$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
dfull3$DOW <- factor(dfull3$DOW, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
dfull3$Shift <- as.factor(dfull3$Shift)
dfull3$Problem <- as.factor(dfull3$Problem)
dfull3$Call_Received <- as.factor(dfull3$Call_Received)
dfull3$Week_No <- as.factor(dfull3$WeekNo)
dfull3$Year_f <- as.factor(dfull3$Year)
dfull3$PriNo <- as.factor(dfull3$Priority_Number)

df_20$Agency <- as.factor(df_20$Agency)
df_20$Month <- factor(df_20$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
df_20$DOW <- factor(df_20$DOW, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
df_20$Shift <- as.factor(df_20$Shift)
df_20$Problem <- as.factor(df_20$Problem)
df_20$Call_Received <- as.factor(df_20$Call_Received)
df_20$Week_No <- as.factor(df_20$WeekNo)
df_20$Year_f <- as.factor(df_20$Year)
df_20$PriNo <- as.factor(df_20$Priority_Number)

df_19$PriNo <- as.factor(df_19$Priority_Number)
df_20$PriNo <- as.factor(df_20$Priority_Number)
df_20$Agcy <- sapply(df_20$Agency, unclass)

df_med_qywk$Year_f <- as.factor(df_med_qywk$Year)
df_med_qywk$Week_No <- as.factor(df_med_qywk$WeekNo)
df_med_dywk$Year_f <- as.factor(df_med_dywk$Year)
df_med_dywk$Week_No <- as.factor(df_med_dywk$WeekNo)
df_med_cywk$Year_f <- as.factor(df_med_cywk$Year)
df_med_cywk$Week_No <- as.factor(df_med_cywk$WeekNo)
df_med_pywk$Year_f <- as.factor(df_med_pywk$Year)
df_med_pywk$Week_No <- as.factor(df_med_pywk$WeekNo)

names(df_med_qywk)[2] <- "WeekNo"
names(df_med_dywk)[2] <- "WeekNo"

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

tblqwk <- table(med_q_y_wk)
df_qwk <- as.data.frame(tblqwk)
view(df_qwk)
remove(df_qwk)

view(WeekTbl)

YearDf <- as.data.frame(YearTbl)

df_19 <- subset(dfull3, Year == 2019)
df_20 <- subset(dfull3, Year == 2020)

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

ggdensity(dfull3, x = "QueueTime",
          add = "median", rug = TRUE,
          color = "Year_f", fill = "Year_f",
          palette = get_palette("startrek", 2),
          title = "Density plot of QueueTime")

ggdensity(dfull3, x = "DispatchTime",
          add = "median", rug = TRUE,
          color = "Year_f", fill = "Year_f",
          palette = get_palette("startrek", 2),
          title = "Density plot of DispatchTime")

ggdensity(dfull3, x = "CallTime",
          add = "median", rug = TRUE,
          color = "Year_f", fill = "Year_f",
          palette = get_palette("startrek", 2),
          title = "Density plot of CallTime")

ggdensity(dfull3, x = "ProcessTime",
          add = "median", rug = TRUE,
          color = "Year_f", fill = "Year_f",
          palette = get_palette("startrek", 2),
          title = "Density plot of ProcessTime")

ggdensity(dfull3, x = "QueueTime",
          add = "median", rug = TRUE,
          color = "Month", fill = "Month",
          palette = get_palette("startrek", 12),
          title = "Density plot Queue Time by Month")

# Bivariate Bar Plots
ggbarplot(YearDf, x = "Var1", y = "Freq",
          fill = "Var1",
          palette = "startrek",            
          title = "Call Count by Year",
          xlab = "Year",
          ylab = "Call Count",
          label = TRUE,
          lab.pos = "in",
          lab.col = "white"
)

ggscatter(df_med_qywk, x = "Week_No", y = "Median",
          color = "Year_f", palette = "startrek",
          shape = "Year_f", title = "Median of Queue Time by Week")

ggscatter(df_med_dywk, x = "Week_No", y = "Median",
          color = "Year_f", palette = "startrek",
          shape = "Year_f", title = "Median of Dispatch Time by Week" )

ggscatter(df_med_cywk, x = "Week_No", y = "Median",
          color = "Year_f", palette = "startrek",
          shape = "Year_f", title = "Median of Call Time by Week")

ggscatter(df_med_pywk, x = "Week_No", y = "Median",
          color = "Year_f", palette = "startrek",
          shape = "Year_f", title = "Median of Processing Time by Week")

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



med_q_y_wk <- median(QueueTime ~ Year + WeekNo, data = dfull3)
view(med_q_y_wk)
write.csv(med_q_y_wk, file = "/cloud/project/med_q_y_wk.csv", row.names = TRUE)
med_d_y_wk <- median(DispatchTime ~ Year + WeekNo, data = dfull3)
view(med_d_y_wk)
write.csv(med_d_y_wk, file = "/cloud/project/med_d_y_wk.csv", row.names = TRUE)
med_c_y_wk <- median(CallTime ~ Year + WeekNo, data = dfull3)
view(med_c_y_wk)
write.csv(med_c_y_wk, file = "/cloud/project/med_c_y_wk.csv", row.names = TRUE)
med_p_y_wk <- median(ProcessTime ~ Year + WeekNo, data = dfull3)
view(med_p_y_wk)
write.csv(med_p_y_wk, file = "/cloud/project/med_p_y_wk.csv", row.names = TRUE)

med_20_q_wk_ag <- median(QueueTime ~ Agency, data = df_20)

med_q_y_m <- median(QueueTime ~ Year + Month, data = dfull3)
view(med_q_y_m)
med_d_y_m <- median(DispatchTime ~ Year + Month, data = dfull3)
view(med_d_y_m)
med_c_y_m <- median(CallTime ~ Year + Month, data = dfull3)
view(med_c_y_m)
med_p_y_m <- median(ProcessTime ~ Year + Month, data = dfull3)
view(med_p_y_m)

write.csv(as.data.frame(med_q_y_wk), "C:\\Users\\trdun\\OneDrive\\Documents\\WGU_Capstone\\med_q_y_wk.csv", row.names = TRUE)

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
kruskal_effsize(QueueTime ~ Year, data = dfull3)

kruskal.test(CallTime ~ Year, data = dfull3)
kruskal_effsize(CallTime ~ Year, data = dfull3)

kruskal.test(ProcessTime ~ Year, data = dfull3)
kruskal_effsize(ProcessTime ~ Year, data = dfull3)

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

kruskal.test(QueueTime ~ Agency, data = dfull3)
kruskal_effsize(QueueTime ~ Agency, data = dfull3)

kruskal.test(DispatchTime ~ Agency, data = dfull3)
kruskal_effsize(DispatchTime ~ Agency, data = dfull3)

kruskal.test(CallTime ~ Agency, data = dfull3)
kruskal_effsize(CallTime ~ Agency, data = dfull3)

kruskal.test(ProcessTime ~ Agency, data = dfull3)
kruskal_effsize(ProcessTime ~ Agency, data = dfull3)

scheirerRayHare(QueueTime ~ Year + Agency + WeekNo, data = dfull3)
dunn_q_agcy <- dunn_test(QueueTime ~ Agency, data = dfull3, p.adjust.method = "bonferroni", detailed = TRUE)

scheirerRayHare(DispatchTime ~ Year + Agency + WeekNo, data = dfull3)
dunn_dsp_agcy <- dunn_test(DispatchTime ~ Agency, data = dfull3, p.adjust.method = "bonferroni", detailed = TRUE)

q1model <- art(QueueTime ~ Year_f + Shift + Year_f:Shift, data = dfull3)
q1model
anova(q1model)

d1model <- art(DispatchTime ~ Year_f + DOW + Year_f:DOW, data = dfull3)
d1model
anova(d1model)

scheirerRayHare(QueueTime ~ Year_f + Shift, data = dfull3)
scheirerRayHare(QueueTime ~ Year_f + DOW, data = dfull3)
dunn_q_dow <- dunn_test(QueueTime ~ DOW, data = dfull3, p.adjust.method = "bonferroni", detailed = TRUE)

# S-R-H Tests
scheirerRayHare(QueueTime ~ Year_f + Week_No, data = dfull3)
scheirerRayHare(DispatchTime ~ Year_f + Week_No, data = dfull3)
scheirerRayHare(CallTime ~ Year_f + Week_No, data = dfull3)
scheirerRayHare(ProcessTime ~ Year_f + Week_No, data = dfull3)

scheirerRayHare(QueueTime ~ Year_f + Agency, data = dfull3)
scheirerRayHare(DispatchTime ~ Year_f + Agency, data = dfull3)
scheirerRayHare(CallTime ~ Year_f + Agency, data = dfull3)
scheirerRayHare(ProcessTime ~ Year_f + Agency, data = dfull3)

# Dunn tests for Week_No
dunn_q_wk <- dunn_test(QueueTime ~ Week_No, data = dfull3, p.adjust.method = "BH", detailed = TRUE)
view(dunn_q_wk)
dunn_d_wk <- dunn_test(DispatchTime ~ Week_No, data = dfull3, p.adjust.method = "BH", detailed = TRUE)
view(dunn_d_wk)
dunn_c_wk <- dunn_test(CallTime ~ Week_No, data = dfull3, p.adjust.method = "BH", detailed = TRUE)
view(dunn_c_wk)
dunn_p_wk <- dunn_test(ProcessTime ~ Week_No, data = dfull3, p.adjust.method = "BH", detailed = TRUE)
view(dunn_p_wk)

write.csv(dunn_q_wk, file = "/cloud/project/dunn_q_wk.csv", row.names = TRUE)
write.csv(dunn_d_wk, file = "/cloud/project/dunn_d_wk.csv", row.names = TRUE)
write.csv(dunn_c_wk, file = "/cloud/project/dunn_c_wk.csv", row.names = TRUE)
write.csv(dunn_p_wk, file = "/cloud/project/dunn_p_wk.csv", row.names = TRUE)

d19_q_wk <- dunn_test(QueueTime ~ Week_No, data = df_19, p.adjust.method = "BH", detailed = TRUE)
view(d19_q_wk)
d19_d_wk <- dunn_test(DispatchTime ~ Week_No, data = df_19, p.adjust.method = "BH", detailed = TRUE)
view(d19_d_wk)
d19_c_wk <- dunn_test(CallTime ~ Week_No, data = df_19, p.adjust.method = "BH", detailed = TRUE)
view(d19_c_wk)
d19_p_wk <- dunn_test(ProcessTime ~ Week_No, data = df_19, p.adjust.method = "BH", detailed = TRUE)
view(d19_p_wk)
d20_q_wk <- dunn_test(QueueTime ~ Week_No, data = df_20, p.adjust.method = "BH", detailed = TRUE)
view(d20_q_wk)
d20_d_wk <- dunn_test(DispatchTime ~ Week_No, data = df_20, p.adjust.method = "BH", detailed = TRUE)
view(d20_d_wk)
d20_c_wk <- dunn_test(CallTime ~ Week_No, data = df_20, p.adjust.method = "BH", detailed = TRUE)
view(d20_c_wk)
d20_p_wk <- dunn_test(ProcessTime ~ Week_No, data = df_20, p.adjust.method = "BH", detailed = TRUE)
view(d20_p_wk)

# By year + WeekNo + Agency
scheirerRayHare(QueueTime ~ Week_No + Agency, data = df_19)
scheirerRayHare(DispatchTime ~ Week_No + Agency, data = df_19)
scheirerRayHare(CallTime ~ WeekNo + Agency, data = df_19)
scheirerRayHare(ProcessTime ~ Week_No + Agency, data = df_19)

scheirerRayHare(QueueTime ~ Week_No + Agency, data = df_20)
scheirerRayHare(DispatchTime ~ Week_No + Agency, data = df_20)
scheirerRayHare(CallTime ~ WeekNo + Agency, data = df_20)
scheirerRayHare(ProcessTime ~ Week_No + Agency, data = df_20)

scheirerRayHare(QueueTime ~ Week_No + PriNo, data = df_19)
scheirerRayHare(DispatchTime ~ Week_No + PriNo, data = df_19)
scheirerRayHare(CallTime ~ Week_No + PriNo, data = df_19)
scheirerRayHare(ProcessTime ~ Week_No + PriNo, data = df_19)

scheirerRayHare(QueueTime ~ Week_No + PriNo, data = df_20)
scheirerRayHare(DispatchTime ~ Week_No + PriNo, data = df_20)
scheirerRayHare(CallTime ~ Week_No + PriNo, data = df_20)
scheirerRayHare(ProcessTime ~ Week_No + PriNo, data = df_20)


