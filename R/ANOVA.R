## One-way ANOVA test
# install.packages("data.table")
library(dplyr)
# setwd("/Users/shossein/GitHub/DTI_Clustering/R")
getwd()
##---------------------- Baseline ---------------------------------
baseline <- read.csv("../Data/Neurocognitive_Scores/Joins/df_base_all_scores.csv")
m36 <- read.csv("../Data/Neurocognitive_Scores/Joins/df_m36_all_scores.csv")

baseline$class_e500  <- factor(baseline$class_e500)
baseline$class_e1500 <- factor(baseline$class_e1500)
m36$class_e500       <- factor(m36$class_e500)
m36$class_e1500      <- factor(m36$class_e1500)

levels(baseline$class_e500)
is.factor(baseline$class_e500)
baseline$class_e500
head(baseline)
dim(baseline)

baseline$class_e500  <- ordered(baseline$class_e500,  levels=levels(baseline$class_e500))
baseline$class_e1500 <- ordered(baseline$class_e1500, levels=levels(baseline$class_e1500))
m36$class_e500       <- ordered(m36$class_e500,  levels=levels(m36$class_e500))
m36$class_e1500      <- ordered(m36$class_e1500, levels=levels(m36$class_e1500))

# group_by(baseline, class_e500) %>%
#   summarise(count = n(), mean = mean(baseline$GIA_SS, na.rm=TRUE), sd = sd(baseline$GIA_SS, na.rm=TRUE))

summary(aov(cbind(GIA_SS, Proc_Spd_SS, Working_Mem_SS, Broad_Attn_SS) ~ class_e500 + class_e1500, data = baseline))
summary(aov(cbind(GIA_SS, Proc_Spd_SS, Working_Mem_SS, Broad_Attn_SS) ~ class_e500 + class_e1500, data = m36))
