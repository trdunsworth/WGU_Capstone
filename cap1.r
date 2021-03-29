# Install packages and define libraries for use

install.packages("car", "DataExplorer", "dataMaid", "dataReporter", "dplyr", "fBasics", "forcats", "ggplot2","ggpubr", "knitr","MASS","nortest","outliers","pgirmess", "PCMCR","PCMCRplus","psych","purrr","readr","rmarkdown","statmod",'stringr',"tibble","tidyr","tidyverse","timeDate","timeSeries")
library(car)
library(DataExplorer)
library(dataMaid)
library(dataReporter)
library(dplyr)
library(fBasics)
library(forcats)
library(ggplot2)
library(ggpubr)
library(knitr)
library(MASS)
library(nortest)
library(outliers)
library(pgirmess)
library(PCMCR)
library(PCMCRplus)
library(psych)
library(purrr)
library(readr)
library(rmarkdown)
library(statmod)
library(stringr)
library(tibble)
library(tidyr)
library(tidyverse)
library(timeDate)
library(timeSeries)

# set the working directory and create the base data frames

swd("C:\\Users\\trdun\\OneDrive\\Documents\\WGU_Capstone")

# Full two year dataset
dfull <- read_csv("full_set.csv")
dfull_t2q <- dfull[order(dfull$T2Q), ]
dfull_t2d <- dfull[order(dfull$T2Disp), ]
dfull_pt <- dfull[order(dfull$ProcTime), ]

df19 <- read_csv("full_set_2019.csv")
df19_t2q <- df19[order(df19$T2Q), ]
df19_t2d <- df19[order(df19$T2Disp), ]
df19_pt <- df19[order(df19$ProcTime), ]

df20 <- read_csv("full_set_2020.csv")
df20_t2q <- df20[order(df20$T2Q), ]
df20_t2d <- df20[order(df20$T2Disp), ]
df20_pt <- df20[order(df20$ProcTime), ]

# Use DataExplorer to do a quick EDA for the datasets
create_report(dfull)
create_report(df19)
create_report(df20)

# Longer intoductory univariate analysis
summary(dfull)
summary(df19)
summary(df20)

dim(dfull)
dim(df19)
dim(df20)

str(dfull)
str(df19)
str(df20)

introduce(dfull)
introduce(df19)
introduce(df20)

describe(dfull)
describe(df19)
describe(df20)

ls(dfull)
ls(df19)
ls(df20)

head(dfull)
head(df19)
head(df20)

tail(dfull)
tail(df19)
tail(df20)

makeDataReport(dfull)
makeDataReport(df19)
makeDataReport(df20)

# Check for the number of rows where the processing time in question is less than 0
sum(dfull$T2Q < 0, na.rm=TRUE)
sum(dfull$T2Disp < 0, na.rm = TRUE)
sum(dfull$ProcTime < 0, na.rm=TRUE)
sum(dfull$T2Q < 0 & dfull$T2Disp < 0, na.rm=TRUE)
sum(dfull$T2Q < 0 & dfull$ProcTime < 0, na.rm=TRUE)
sum(dfull$ProcTime < 0 & dfull$T2Disp < 0, na.rm=TRUE)

# Outlier detection and identification

fullT2Q_lb <- quantile(dfull$T2Q, 0.01)
fullT2Q_ub <- quantile(dfull$T2Q, 0.99)
fullT2Q_outTest <- grubbs.test(dfull$T2Q)
fullT2Q_lb
fullT2Q_ub
fullT2Q_outTest

fullT2D_lb <- quantile(dfull$T2Disp, 0.01)
fullT2D_ub <- quantile(dfull$T2Disp, 0.99)
fullT2D_outTest <- grubbs.test(dfull$T2Disp)
fullT2D_lb
fullT2D_ub
fullT2D_outTest

fullPT_lb <- quantile(dfull$ProcTime, 0.01)
fullPT_ub <- quantile(dfull$ProcTime, 0.99)
fullPT_outTest <- grubbs.test(dfull$ProcTime)
fullPT_lb
fullPT_ub
fullPT_outTest

f19T2Q_lb <- quantile(df19$T2Q, 0.01)
f19T2Q_ub <- quantile(df19$T2Q, 0.99)
f19T2Q_outTest <- grubbs.test(df19$T2Q)
f19T2Q_lb
f19T2Q_ub
f19T2Q_outTest

f19T2D_lb <- quantile(df19$T2Disp, 0.01)
f19T2D_ub <- quantile(df19$T2Disp, 0.99)
f19T2D_outTest <- grubbs.test(df19$T2Disp)
f19T2D_lb
f19T2D_ub
f19T2D_outTest

f19PT_lb <- quantile(df19$ProcTime, 0.01)
f19PT_ub <- quantile(df19$ProcTime, 0.99)
f19PT_outTest <- grubbs.test(df19$ProcTime)
f19PT_lb
f19PT_ub
f19PT_outTest

f20T2Q_lb <- quantile(df20$T2Q, 0.01)
f20T2Q_ub <- quantile((df20$T2Q, 0.99)
f20T2Q_outTest <- grubbs.test((df20$T2Q)
f20T2Q_lb
f20T2Q_ub
f20T2Q_outTest

f20T2D_lb <- quantile((df20$T2Disp, 0.01)
f20T2D_ub <- quantile((df20$T2Disp, 0.99)
f20T2D_outTest <- grubbs.test((df20$T2Disp)
f20T2D_lb
f20T2D_ub
f20T2D_outTest

f20PT_lb <- quantile((df20$ProcTime, 0.01)
f20PT_ub <- quantile((df20$ProcTime, 0.99)
f20PT_outTest <- grubbs.test((df20$ProcTime)
f20PT_lb
f20PT_ub
f20PT_outTest

# Remove na and rows where processing times are less than 0
dfull <- na.omit(dfull)
df19 <- na.omit(df19)
df20 <- na.omit(df20)

