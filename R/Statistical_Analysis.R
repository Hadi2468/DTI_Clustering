library(dplyr)
library(ggplot2)
library(ggplot2)
getwd()    # setwd("/Users/shossein/GitHub/DTI_Clustering/R")

##---------------------- Loading master data -------------------------------------------------------------------------------------------

master_data <- read.csv("../../Data/data_random_1/LF_Joins/master_data_union.csv")
master_data[master_data$Kmeans_4 != master_data$Kmeans_Sparse_4, ]
master_data[master_data$Kmeans_2 != master_data$Kmeans_Sparse_2, ]

master_data$Kmeans_2  <- factor(master_data$Kmeans_2)
master_data$Kmeans_4  <- factor(master_data$Kmeans_4)
master_data$Kmeans_Sparse_2  <- factor(master_data$Kmeans_Sparse_2)
master_data$Kmeans_Sparse_4  <- factor(master_data$Kmeans_Sparse_4)

master_data$Kmeans_2 <- ordered(master_data$Kmeans_2,  levels = levels(master_data$Kmeans_2))
master_data$Kmeans_4 <- ordered(master_data$Kmeans_4,  levels = levels(master_data$Kmeans_4))
master_data$Kmeans_Sparse_2 <- ordered(master_data$Kmeans_Sparse_2,  levels = levels(master_data$Kmeans_Sparse_2))
master_data$Kmeans_Sparse_4 <- ordered(master_data$Kmeans_Sparse_4,  levels = levels(master_data$Kmeans_Sparse_4))

dim(master_data)
levels(master_data$Kmeans_4)
is.factor(master_data$Kmeans_4)
master_data$Kmeans_4

##---------------------- Statistics ----------------------------------------------------------------------------------------------------
group_by(master_data, Kmeans_Sparse_4) %>%
  summarise(count = n(), mean = mean(master_data$GIA_diff, na.rm=TRUE), sd = sd(master_data$GIA_diff, na.rm=TRUE))

##---------------------- One-way ANOVA test for K_means --------------------------------------------------------------------------------
summary(aov(cbind(GIA_base, Proc_Spd_base, Working_Mem_base, Broad_Attn_base) ~ Kmeans_4 + Kmeans_Sparse_4, data = master_data))
summary(aov(cbind(GIA_diff, Proc_Spd_diff, Working_Mem_diff, Broad_Attn_diff) ~ Kmeans_4 + Kmeans_Sparse_4, data = master_data))

res.aov <- aov(cbind(GIA_base, Proc_Spd_base, Working_Mem_base, Broad_Attn_base) ~ Kmeans_4 + Kmeans_Sparse_4, data = master_data)
summary(res.aov)

##---------------------- Kruskal–Wallis test (Non-parametric alternative to one-way ANOVA test) ----------------------------------------

kruskal.test(GIA_base         ~ Kmeans_4,        data = master_data)
kruskal.test(GIA_base         ~ Kmeans_Sparse_4, data = master_data)
kruskal.test(Proc_Spd_base    ~ Kmeans_4,        data = master_data)
kruskal.test(Proc_Spd_base    ~ Kmeans_Sparse_4, data = master_data)
kruskal.test(Working_Mem_base ~ Kmeans_4,        data = master_data)
kruskal.test(Working_Mem_base ~ Kmeans_Sparse_4, data = master_data)
kruskal.test(Broad_Attn_base  ~ Kmeans_4,        data = master_data)
kruskal.test(Broad_Attn_base  ~ Kmeans_Sparse_4, data = master_data)

kruskal.test(GIA_diff         ~ Kmeans_4,        data = master_data)
kruskal.test(GIA_diff         ~ Kmeans_Sparse_4, data = master_data)
kruskal.test(Proc_Spd_diff    ~ Kmeans_4,        data = master_data)
kruskal.test(Proc_Spd_diff    ~ Kmeans_Sparse_4, data = master_data)
kruskal.test(Working_Mem_diff ~ Kmeans_4,        data = master_data)
kruskal.test(Working_Mem_diff ~ Kmeans_Sparse_4, data = master_data)
kruskal.test(Broad_Attn_diff  ~ Kmeans_4,        data = master_data)
kruskal.test(Broad_Attn_diff  ~ Kmeans_Sparse_4, data = master_data)

# pairwise.wilcox.test(m36base$Broad_Attn_SS, m36base$Cluster_SS_e1500, p.adjust.method = "BH")

##---------------------- Pallet creation -----------------------------------------------------------------------------------------------

drsimonj_colors <- c(`blue` = "#0CB8FC", `red` = "#FF7F7F", `green` = "#00FF00", `yellow` = "#ffc425", 
                     `dark blue` = "#0011FF", `dark red` = "#B20000", `dark green` = "#067F10", `orange` = "#f37735")

drsimonj_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return (drsimonj_colors)
  drsimonj_colors[cols]}
drsimonj_palettes <- list(`cool` = drsimonj_cols("blue", "red", "green", "yellow"), 
                          `hot`  = drsimonj_cols("dark blue", "dark red", "dark green", "orange"))
drsimonj_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- drsimonj_palettes[[palette]]
  if (reverse) pal <- rev(pal)
  colorRampPalette(pal, ...)}
scale_color_drsimonj <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- drsimonj_pal(palette = palette, reverse = reverse)
  if (discrete) {discrete_scale("colour", paste0("drsimonj_", palette), palette = pal, ...)}
  else {scale_color_gradientn(colours = pal(256), ...)}}
scale_fill_drsimonj <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- drsimonj_pal(palette = palette, reverse = reverse)
  if (discrete) {discrete_scale("fill", paste0("drsimonj_", palette), palette = pal, ...)}
  else {scale_fill_gradientn(colours = pal(256), ...)}}

##---------------------- Box plots for ANOVA & Kruskal–Wallis tests --------------------------------------------------------------------
# Box-plot 1
ggplot(master_data, aes(x = Kmeans_4, y = GIA_base, color = Kmeans_4, fill = Kmeans_4)) + 
  ggtitle("K-Means (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.5961") + ylab("GIA") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 130))
# Box-plot 2
ggplot(master_data, aes(x = Kmeans_Sparse_4, y = GIA_base, color = Kmeans_Sparse_4, fill = Kmeans_Sparse_4)) + 
  ggtitle("K-Means Sparse (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.9058") + ylab("GIA") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 130))
# Box-plot 3
ggplot(master_data, aes(x = Kmeans_4, y = Proc_Spd_base, color = Kmeans_4, fill = Kmeans_4)) + 
  ggtitle("K-Means (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.9032") + ylab("Process Speed") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(30, 130))
# Box-plot 4
ggplot(master_data, aes(x = Kmeans_Sparse_4, y = Proc_Spd_base, color = Kmeans_Sparse_4, fill = Kmeans_Sparse_4)) + 
  ggtitle("K-Means Sparse (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.5335") + ylab("Process Speed") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(30, 130))
# Box-plot 5
ggplot(master_data, aes(x = Kmeans_4, y = Working_Mem_base, color = Kmeans_4, fill = Kmeans_4)) + 
  ggtitle("K-Means (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.6187") + ylab("Working Memory") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 140))
# Box-plot 6
ggplot(master_data, aes(x = Kmeans_Sparse_4, y = Working_Mem_base, color = Kmeans_Sparse_4, fill = Kmeans_Sparse_4)) + 
  ggtitle("K-Means Sparse (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.8395") + ylab("Working Memory") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 140))
# Box-plot 7
ggplot(master_data, aes(x = Kmeans_4, y = Broad_Attn_base, color = Kmeans_4, fill = Kmeans_4)) + 
  ggtitle("K-Means (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.8973") + ylab("Broad Attention") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 130))
# Box-plot 8
ggplot(master_data, aes(x = Kmeans_Sparse_4, y = Broad_Attn_base, color = Kmeans_Sparse_4, fill = Kmeans_Sparse_4)) + 
  ggtitle("K-Means Sparse (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.9984") + ylab("Broad Attention") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 130))
# Box-plot 9
ggplot(master_data, aes(x = Kmeans_4, y = GIA_diff, color = Kmeans_4, fill = Kmeans_4)) + 
  ggtitle("K-Means (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.1207") + ylab("GIA") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))
# Box-plot 10
ggplot(master_data, aes(x = Kmeans_Sparse_4, y = GIA_diff, color = Kmeans_Sparse_4, fill = Kmeans_Sparse_4)) + 
  ggtitle("K-Means Sparse (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.3187") + ylab("GIA") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))
# Box-plot 11
ggplot(master_data, aes(x = Kmeans_4, y = Proc_Spd_diff, color = Kmeans_4, fill = Kmeans_4)) + 
  ggtitle("K-Means (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.3373") + ylab("Process Speed") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 60))
# Box-plot 12
ggplot(master_data, aes(x = Kmeans_Sparse_4, y = Proc_Spd_diff, color = Kmeans_Sparse_4, fill = Kmeans_Sparse_4)) + 
  ggtitle("K-Means Sparse (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.1546") + ylab("Process Speed") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 60))
# Box-plot 13
ggplot(master_data, aes(x = Kmeans_4, y = Working_Mem_diff, color = Kmeans_4, fill = Kmeans_4)) + 
  ggtitle("K-Means (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.3152") + ylab("Working Memory") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))
# Box-plot 14
ggplot(master_data, aes(x = Kmeans_Sparse_4, y = Working_Mem_diff, color = Kmeans_Sparse_4, fill = Kmeans_Sparse_4)) + 
  ggtitle("K-Means Sparse (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.7402") + ylab("Working Memory") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))
# Box-plot 15
ggplot(master_data, aes(x = Kmeans_4, y = Broad_Attn_diff, color = Kmeans_4, fill = Kmeans_4)) + 
  ggtitle("K-Means (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.9205") + ylab("Broad Attention") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))
# Box-plot 16
ggplot(master_data, aes(x = Kmeans_Sparse_4, y = Broad_Attn_diff, color = Kmeans_Sparse_4, fill = Kmeans_Sparse_4)) + 
  ggtitle("K-Means Sparse (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.7983") + ylab("Broad Attention") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))

##---------------------- t test --------------------------------------------------------------------------------------------------------

t.test(GIA_base        ~ Kmeans_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Proc_Spd_base    ~ Kmeans_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Working_Mem_base ~ Kmeans_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Broad_Attn_base  ~ Kmeans_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(GIA_base         ~ Kmeans_Sparse_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Proc_Spd_base    ~ Kmeans_Sparse_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Working_Mem_base ~ Kmeans_Sparse_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Broad_Attn_base  ~ Kmeans_Sparse_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)

t.test(GIA_diff         ~ Kmeans_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Proc_Spd_diff    ~ Kmeans_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Working_Mem_diff ~ Kmeans_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Broad_Attn_diff  ~ Kmeans_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(GIA_diff         ~ Kmeans_Sparse_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Proc_Spd_diff    ~ Kmeans_Sparse_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Working_Mem_diff ~ Kmeans_Sparse_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Broad_Attn_diff  ~ Kmeans_Sparse_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)

t.test(cbind(GIA_base, Proc_Spd_base, Working_Mem_base, Broad_Attn_base) ~ Kmeans_2, data = master_data, 
       mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)

t.test(cbind(GIA_base, Proc_Spd_base, Working_Mem_base, Broad_Attn_base) ~ Kmeans_Sparse_2, data = master_data, 
       mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)

t.test(cbind(GIA_diff, Proc_Spd_diff, Working_Mem_diff, Broad_Attn_diff) ~ Kmeans_2, data = master_data, 
       mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)

t.test(cbind(GIA_diff, Proc_Spd_diff, Working_Mem_diff, Broad_Attn_diff) ~ Kmeans_Sparse_2, data = master_data, 
       mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)

##---------------------- Wilcoxon rank sum test (Non-parametric alternative to t-test) -------------------------------------------------

res <- wilcox.test(GIA_SS ~ Cluster_SS_e500, data = m36_2class, alternative = "two.sided", exact = FALSE)
res
res$p.value

wilcox.test(GIA_SS ~ Cluster_SS_e1500, data = m36_2class, exact = FALSE)
wilcox.test(Proc_Spd_SS ~ Cluster_SS_e1500, data = m36_2class, exact = FALSE)
wilcox.test(Working_Mem_SS ~ Cluster_SS_e1500, data = m36_2class, exact = FALSE)
wilcox.test(Broad_Attn_SS ~ Cluster_SS_e1500, data = m36_2class, exact = FALSE)






