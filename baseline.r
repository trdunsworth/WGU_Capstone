df1 <- read_csv("c20210406.csv")
df_1 <- read_csv("c20210406.csv")
spec(df1)

ls(df1)

df1$Agency <- as.factor(df1$Agency)
df1$Call_Disposition <- as.factor(df1$Call_Disposition)
df1$Master_Incident_Number <- as.factor(df1$Master_Incident_Number)
df1$Month <- as.factor(df1$Month)
df1$DOW <- as.factor(df1$DOW)
df1$Shift <- as.factor(df1$Shift)
df1$Problem <- as.factor(df1$Problem)
df1$Call_Received <- as.factor(df1$Call_Received)
df1$CallTaking_Performed_By <- as.factor(df1$CallTaking_Performed_By)
df1$Dispatcher <- as.factor(df1$Dispatcher)

summary(df1)

df1 %>% drop_na()

df1$QueueTime <- as.numeric(df1$Fixed_Time_CallEnteredQueue - df1$`Phone_Pick_Up`)
df1$DispatchTime <-as.numeric(df1$Time_First_Unit_Assigned - df1$Fixed_Time_CallEnteredQueue)
df1$ProcessTime <- as.numeric(df1$`End_Call_Taking` - df1$`Phone_Pick_Up`)

sum(df1$QueueTime < 0, na.rm=TRUE)
sum(df1$DispatchTime < 0, na.rm = TRUE)
sum(df1$ProcessTime < 0, na.rm=TRUE)
sum(df1$QueueTime < 0 & df1$DispatchTime < 0, na.rm=TRUE)
sum(df1$QueueTime < 0 & df1$ProcessTime < 0, na.rm=TRUE)
sum(df1$ProcessTime < 0 & df1$DispatchTime < 0, na.rm=TRUE)

df1 = df1[!df1$QueueTime<0, ]
df1 = df1[!df1$DispatchTime<0, ]
df1 = df1[!df1$ProcessTime<0, ]

create_report(df1)

summary(df1$DOW)


hist(df1$QueueTime)
hist(df1$DispatchTime)
hist(df1$ProcessTime)

describe(df1$QueueTime)
describe(df1$DispatchTime)
describe(df1$ProcessTime)

barplot(table(df1$DOW), horiz = TRUE, lab)
barplot(table(df1$Shift))

# normality tests
# Density Plots
ggdensity(df1$QueueTime, fill = "lightblue")
ggdensity(df1$DispatchTime, fill = "lightblue")
ggdensity(df1$ProcessTime, fill = "lightblue")

# QQ Plots
ggqqplot(df1$QueueTime)
ggqqplot(df1$DispatchTime)
ggqqplot(df1$ProcessTime)

# Shapiro Wilk normality test p < 0.05 = non-normal distribution
shapiro.test(df1$QueueTime)
shapiro.test(df1$DispatchTime)
shapiro.test(df1$ProcessTime)

describeBy(QueueTime + DispatchTime + ProcessTime ~ Agency, data = df1)
describeBy(QueueTime + DispatchTime + ProcessTime ~ DOW, data = df1)
describeBy(QueueTime + DispatchTime + ProcessTime ~ Hour, data = df1)
describeBy(QueueTime + DispatchTime + ProcessTime ~ Shift, data = df1)
describeBy(QueueTime + DispatchTime + ProcessTime ~ DOW + Hour, data = df1)
describeBy(QueueTime + DispatchTime + ProcessTime ~ DOW + Shift, data = df1)

group_by(df1, DOW) %>% summarise(count = n(), median(QueueTime, na.rm = TRUE), median(DispatchTime, na.rm = TRUE), median(ProcessTime, na.rm = TRUE))
group_by(df1, Problem) %>% summarise(count = n(), median(QueueTime, na.rm = TRUE), median(DispatchTime, na.rm = TRUE), median(ProcessTime, na.rm = TRUE))
Problem_stats <- group_by(df1, Problem) %>% summarise(count = n(), median(QueueTime, na.rm = TRUE), median(DispatchTime, na.rm = TRUE), median(ProcessTime, na.rm = TRUE))
group_by(df1, CallTaking_Performed_By) %>% summarise(count = n(), median(QueueTime, na.rm = TRUE), median(DispatchTime, na.rm = TRUE), median(ProcessTime, na.rm = TRUE))
CT_Stats <- group_by(df1, CallTaking_Performed_By) %>% summarise(count = n(), median(QueueTime, na.rm = TRUE), IQR(QueueTime, na.rm = TRUE))
Disp_Stats <- group_by(df1, Dispatcher) %>% summarise(count = n(), median(DispatchTime, na.rm = TRUE), IQR(DispatchTime, na.rm = TRUE))


kruskal.test(QueueTime ~ DOW, data = df1)
kruskal.test(QueueTime ~ CallTaking_Performed_By, data = df1)
kruskal_effsize(QueueTime ~ CallTaking_Performed_By, data = df1)
pairwise.wilcox.test(df1$QueueTime, df1$CallTaking_Performed_By, p.adjust.method = "BH")
dunn_ct <- dunn_test(df1, QueueTime ~ CallTaking_Performed_By, p.adjust.method = "bonferroni")

kruskal.test(QueueTime ~ Shift, data = df1)
kruskal_effsize(QueueTime ~ Shift, data = df1)
pairwise.wilcox.test(df1$QueueTime, df1$Shift, p.adjust.method = "BH")
dunn_shift <- dunn_test(df1, QueueTime ~ Shift, p.adjust.method = "bonferroni")
