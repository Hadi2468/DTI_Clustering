library(ggplot2)
getwd()
#setwd('./Desktop/R_scripts')
##-------------------------------------------------------
B_GIA       <- read.csv("cluster_base_GIA.csv")
B_ProcSpd   <- read.csv("cluster_base_ProcSpd.csv")
B_WorkMem   <- read.csv("cluster_base_WorkingMem.csv")
B_BrdAttn   <- read.csv("cluster_base_BroadAttn.csv")
m36_GIA     <- read.csv("cluster_m36base_GIA.csv")
m36_ProcSpd <- read.csv("cluster_m36base_ProcSpd.csv")
m36_WorkMem <- read.csv("cluster_m36base_WorkingMem.csv")
m36_BrdAttn <- read.csv("cluster_m36base_BroadAttn.csv")
##-------------------------------------------------------

df <- B_GIA
dim(df)
qplot(factor(class_e1500), GIA_SS, data=df, geom='boxplot', fill=factor(class_e1500))
boxplot(GIA_SS~as.factor(class_e1500), data=df)
