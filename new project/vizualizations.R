library(ggplot2)
library(tidyverse)

mostResults <- read.csv("data/knn_results.csv")
XGBResults <- read.csv("data/XGB_results_final.csv") %>% 
  rename(XGB_Pres_Prof = profit)
LR_Results <- read.csv("data/LR_results_final.csv")
XGB_pt_Results <- read.csv("data/XBG_pt_results.csv")
LR_pt_Results <- read.csv("data/LR_pt_results.csv")

allResults <- mostResults %>% 
  left_join(XGBResults, by="WeekNum") %>% 
  left_join(LR_Results, by="WeekNum") %>% 
  left_join(XGB_pt_Results, by="WeekNum") %>% 
  left_join(LR_pt_Results, by="WeekNum")

################### LINE POINT PLOTS #############################################

# All Results
ggplot(allResults) + ylim(4e7,8e7) + 
  geom_line(aes(x = WeekNum, y= OracleProf, color="Oracle")) + 
  geom_line(aes(x = WeekNum, y= KnnPresProf, color="KNN Pres")) + 
  geom_line(aes(x = WeekNum, y= KnnAvgProf, color="KNN Avg")) + 
  geom_line(aes(x = WeekNum, y= XGB_Pres_Prof, color="XGB Pres")) + 
  geom_line(aes(x = WeekNum, y= LR_Profit, color="LR Pres")) + 
  geom_line(aes(x = WeekNum, y= XGB_pt_prof, color="XGB Point")) +
  geom_line(aes(x = WeekNum, y= LR_pt_profit, color="LR Point")) +
  geom_point(shape=8,aes(x = WeekNum, y= OracleProf,color="Oracle")) + 
  geom_point(aes(x = WeekNum, y= KnnPresProf, color="KNN Pres")) + 
  geom_point(shape=0,aes(x = WeekNum, y= KnnAvgProf, color="KNN Avg")) + 
  geom_point(aes(x = WeekNum, y= XGB_Pres_Prof, color="XGB Pres")) + 
  geom_point(aes(x = WeekNum, y= LR_Profit, color="LR Pres")) +
  geom_point(shape=0,aes(x = WeekNum, y= XGB_pt_prof, color="XGB Point")) +
  geom_point(shape=0,aes(x = WeekNum, y= LR_pt_profit, color="LR Point")) +
  xlab("Week") + ylab("Realized Profit ($)") + labs(color="Method") + 
  ggtitle("Realized Profits for All Plans") 
  
# KNN
ggplot(allResults) + ylim(4e7,8e7) + 
  geom_line(aes(x = WeekNum, y= OracleProf, color="Oracle")) + 
  geom_line(aes(x = WeekNum, y= KnnPresProf, color="KNN Pres")) + 
  geom_line(aes(x = WeekNum, y= KnnAvgProf, color="KNN Avg")) +
  geom_point(shape=8,aes(x = WeekNum, y= OracleProf, color="Oracle")) + 
  geom_point(aes(x = WeekNum, y= KnnPresProf, color="KNN Pres")) + 
  geom_point(shape=0,aes(x = WeekNum, y= KnnAvgProf, color="KNN Avg")) +
  xlab("Week") + ylab("Realized Profit ($)") + labs(color="Method") + 
  ggtitle("Realized Profits for KNN Plans") + theme_bw()
  
# XGB
ggplot(allResults) + ylim(4e7,8e7) + 
  geom_line(aes(x = WeekNum, y= OracleProf, color="Oracle")) + 
  geom_line(aes(x = WeekNum, y= XGB_Pres_Prof, color="XGB Pres")) +
  geom_line(aes(x = WeekNum, y= XGB_pt_prof, color="XGB Point")) +
  geom_point(shape=8,aes(x = WeekNum, y= OracleProf,color="Oracle")) + 
  geom_point(aes(x = WeekNum, y= XGB_Pres_Prof, color="XGB Pres")) + 
  geom_point(shape=0,aes(x = WeekNum, y= XGB_pt_prof, color="XGB Point")) +
  xlab("Week") + ylab("Realized Profit ($)") + labs(color="Method") + 
  ggtitle("Realized Profits for XGBoost Plans") + theme_bw()
  
# Ride
ggplot(allResults) + ylim(4e7,8e7) + 
  geom_line(aes(x = WeekNum, y= OracleProf, color="Oracle")) +  
  geom_line(aes(x = WeekNum, y= LR_Profit, color="LR Pres")) + 
  geom_line(aes(x = WeekNum, y= LR_pt_profit, color="LR Point")) +
  geom_point(shape=8,aes(x = WeekNum, y= OracleProf,color="Oracle")) + 
  geom_point(aes(x = WeekNum, y= LR_Profit, color="LR Pres")) +
  geom_point(shape=0,aes(x = WeekNum, y= LR_pt_profit, color="LR Point")) +
  xlab("Week") + ylab("Realized Profit ($)") + labs(color="Method") + 
  ggtitle("Realized Profits for Ridge Regression Plans") + theme_bw()

# Prescription Plans
ggplot(allResults) + ylim(4e7,8e7) + 
  geom_line(aes(x = WeekNum, y= OracleProf, color="Oracle")) + 
  geom_line(aes(x = WeekNum, y= KnnPresProf, color="KNN Pres")) + 
  geom_line(aes(x = WeekNum, y= XGB_Pres_Prof, color="XGB Pres")) + 
  geom_line(aes(x = WeekNum, y= LR_Profit, color="LR Pres")) + 
  geom_point(shape=8,aes(x = WeekNum, y= OracleProf,color="Oracle")) + 
  geom_point(aes(x = WeekNum, y= KnnPresProf, color="KNN Pres")) + 
  geom_point(aes(x = WeekNum, y= XGB_Pres_Prof, color="XGB Pres")) + 
  geom_point(aes(x = WeekNum, y= LR_Profit, color="LR Pres")) +
  xlab("Week") + ylab("Realized Profit ($)") + labs(color="Method") + 
  ggtitle("Realized Profits for Prescription Plans") + theme_bw()

# Point Estimate Plans
ggplot(allResults) + ylim(4e7,8e7) + 
  geom_line(aes(x = WeekNum, y= OracleProf, color="Oracle")) + 
  geom_line(aes(x = WeekNum, y= KnnAvgProf, color="KNN Avg")) + 
  geom_line(aes(x = WeekNum, y= XGB_pt_prof, color="XGB Point")) +
  geom_line(aes(x = WeekNum, y= LR_pt_profit, color="LR Point")) +
  geom_point(shape=8,aes(x = WeekNum, y= OracleProf,color="Oracle")) + 
  geom_point(shape=0,aes(x = WeekNum, y= KnnAvgProf, color="KNN Avg")) + 
  geom_point(shape=0,aes(x = WeekNum, y= XGB_pt_prof, color="XGB Point")) +
  geom_point(shape=0,aes(x = WeekNum, y= LR_pt_profit, color="LR Point")) +
  xlab("Week") + ylab("Realized Profit ($)") + labs(color="Method") + 
  ggtitle("Realized Profits for Point Estimate Plans") + theme_bw()



####

write.csv(allResults, "data/allResults.csv")

