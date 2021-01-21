# install.packages("data.table")
library(dplyr)
# setwd("/Users/shossein/GitHub/DTI_Clustering/R")
getwd()

##---------------------- Data Loading ---------------------------------------------------------------------------------------------
baseline <- read.csv("../Data/Neurocognitive_Scores/Joins/df_base_all_scores.csv")
m36 <- read.csv("../Data/Neurocognitive_Scores/Joins/df_m36_all_scores.csv")

baseline$class_e500  <- factor(baseline$class_e500)
baseline$class_e1500 <- factor(baseline$class_e1500)
m36$class_e500       <- factor(m36$class_e500)
m36$class_e1500      <- factor(m36$class_e1500)

levels(m36$class_e500)
is.factor(m36$class_e500)
m36$class_e500
head(m36)
dim(m36)

baseline$class_e500  <- ordered(baseline$class_e500,  levels=levels(baseline$class_e500))
baseline$class_e1500 <- ordered(baseline$class_e1500, levels=levels(baseline$class_e1500))
m36$class_e500       <- ordered(m36$class_e500,  levels=levels(m36$class_e500))
m36$class_e1500      <- ordered(m36$class_e1500, levels=levels(m36$class_e1500))

##---------------------- Statistics ---------------------------------------------------------------------------------------------
group_by(m36, class_e500) %>%
  summarise(count = n(), mean = mean(m36$GIA_SS, na.rm=TRUE), sd = sd(m36$GIA_SS, na.rm=TRUE))

##---------------------- ANOVA ---------------------------------------------------------------------------------------------
summary(aov(cbind(GIA_SS, Proc_Spd_SS, Working_Mem_SS, Broad_Attn_SS) ~ class_e500 + class_e1500, data = baseline))
summary(aov(cbind(GIA_SS, Proc_Spd_SS, Working_Mem_SS, Broad_Attn_SS) ~ class_e500 + class_e1500, data = m36))

res.aov <- aov(cbind(GIA_SS, Proc_Spd_SS, Working_Mem_SS, Broad_Attn_SS) ~ class_e500 + class_e1500, data = m36)
summary(res.aov)

##---------------------- Box Plot ---------------------------------------------------------------------------------------------
qplot(factor(class_e500), GIA_SS, data=m36, geom='boxplot', fill=factor(class_e500))
boxplot(GIA_SS~as.factor(class_e500), data=m36)
ggplot(m36, aes(x=class_e500, y=GIA_SS)) + geom_boxplot(color="blue", fill="pink", size=1)

##---------------------- t test ---------------------------------------------------------------------------------------------

m36_2class <- m36
levels(m36_2class$class_e500)  <- c("1", "1", "2", "2")
levels(m36_2class$class_e1500) <- c("1", "1", "2", "2")
levels(m36_2class$class_e500)

t.test(GIA_SS         ~ class_e500, data=m36_2class, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)
t.test(Proc_Spd_SS    ~ class_e500, data=m36_2class, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)
t.test(Working_Mem_SS ~ class_e500, data=m36_2class, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)
t.test(Broad_Attn_SS  ~ class_e500, data=m36_2class, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)

t.test(cbind(GIA_SS, Proc_Spd_SS, Working_Mem_SS, Broad_Attn_SS) ~ class_e1500, data=m36_2class, 
       mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)

##---------------------- t test ---------------------------------------------------------------------------------------------



