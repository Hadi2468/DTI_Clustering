# install.packages("data.table")
library(dplyr)
library(ggplot2)
# setwd("/Users/shossein/GitHub/DTI_Clustering/R")
getwd()

##---------------------- Data Loading --------------------------------------------------------------------------------------------------
baseline <- read.csv("../../Data/data_random_1/LF_Joins/LF_r_base_total.csv")
m36base  <- read.csv("../../Data/data_random_1/LF_Joins/LF_r_m36base_total.csv")

baseline$Cluster_SS_e500  <- factor(baseline$Cluster_SS_e500)
baseline$Cluster_SS_e1500 <- factor(baseline$Cluster_SS_e1500)
m36base$Cluster_SS_e500   <- factor(m36base$Cluster_SS_e500)
m36base$Cluster_SS_e1500  <- factor(m36base$Cluster_SS_e1500)

levels(m36base$Cluster_SS_e500)
is.factor(m36base$Cluster_SS_e500)
m36base$Cluster_SS_e500
m36base[1:5, 1:10]
dim(m36base)

baseline$Cluster_SS_e500  <- ordered(baseline$Cluster_SS_e500,  levels=levels(baseline$Cluster_SS_e500))
baseline$Cluster_SS_e1500 <- ordered(baseline$Cluster_SS_e1500, levels=levels(baseline$Cluster_SS_e1500))
m36base$Cluster_SS_e500   <- ordered(m36base$Cluster_SS_e500,   levels=levels(m36base$Cluster_SS_e500))
m36base$Cluster_SS_e1500  <- ordered(m36base$Cluster_SS_e1500,  levels=levels(m36base$Cluster_SS_e1500))

##---------------------- Statistics ----------------------------------------------------------------------------------------------------
group_by(m36, class_e500) %>%
  summarise(count = n(), mean = mean(m36$GIA_SS, na.rm=TRUE), sd = sd(m36$GIA_SS, na.rm=TRUE))

##---------------------- One-way ANOVA test --------------------------------------------------------------------------------------------
summary(aov(cbind(GIA_SS, Proc_Spd_SS, Working_Mem_SS, Broad_Attn_SS) ~ class_e500 + class_e1500, data = baseline))
summary(aov(cbind(GIA_SS, Proc_Spd_SS, Working_Mem_SS, Broad_Attn_SS) ~ class_e500 + class_e1500, data = m36))

res.aov <- aov(cbind(GIA_SS, Proc_Spd_SS, Working_Mem_SS, Broad_Attn_SS) ~ class_e500 + class_e1500, data = m36)
summary(res.aov)

##---------------------- Box Plot ------------------------------------------------------------------------------------------------------
qplot(factor(class_e500), GIA_SS, data=m36, geom='boxplot', fill=factor(class_e500))
boxplot(GIA_SS~as.factor(class_e500), data=m36)
ggplot(m36, aes(x=class_e500, y=GIA_SS)) + geom_boxplot(color="blue", fill="pink", size=1)

##---------------------- t test --------------------------------------------------------------------------------------------------------

m36_2class <- m36
levels(m36_2class$class_e500)  <- c("1", "1", "2", "2")
levels(m36_2class$class_e1500) <- c("1", "1", "2", "2")
levels(m36_2class$class_e500)

t.test(GIA_SS         ~ class_e500, data = m36_2class, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Proc_Spd_SS    ~ class_e500, data = m36_2class, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Working_Mem_SS ~ class_e500, data = m36_2class, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Broad_Attn_SS  ~ class_e500, data = m36_2class, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)

t.test(cbind(GIA_SS, Proc_Spd_SS, Working_Mem_SS, Broad_Attn_SS) ~ class_e1500, data=m36_2class, 
       mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)

##---------------------- Kruskal–Wallis test (Non-parametric alternative to one-way ANOVA test) ----------------------------------------

kruskal.test(GIA_SS ~ class_e500, data = m36)
kruskal.test(Proc_Spd_SS ~ class_e500, data = m36)
kruskal.test(Working_Mem_SS ~ class_e500, data = m36)
kruskal.test(Broad_Attn_SS ~ class_e500, data = m36)
pairwise.wilcox.test(m36$Broad_Attn_SS, m36$class_e500, p.adjust.method = "BH")

##---------------------- Wilcoxon rank sum test (Non-parametric alternative to t-test) -------------------------------------------------

wilcox.test(x, y, alternative = "two.sided")

res <- wilcox.test(GIA_SS ~ class_e500, data = m36_2class, exact = FALSE)
res
res$p.value

wilcox.test(GIA_SS ~ class_e500, data = m36_2class, exact = FALSE)
wilcox.test(Proc_Spd_SS ~ class_e500, data = m36_2class, exact = FALSE)
wilcox.test(Working_Mem_SS ~ class_e500, data = m36_2class, exact = FALSE)
wilcox.test(Broad_Attn_SS ~ class_e500, data = m36_2class, exact = FALSE)






