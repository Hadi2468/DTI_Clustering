library(dplyr)
library(ggplot2)
getwd()    # setwd("/Users/shossein/GitHub/DTI_Clustering/R")

##---------------------- Pallet creation -----------------------------------------------------------------------------------------------

drsimonj_colors <- c(`blue` = "#0CB8FC", `red` = "#FF7F7F", `green` = "#00FF00", `yellow` = "#ffc425", `dark blue` = "#0011FF",
                     `dark red` = "#B20000", `dark green` = "#067F10", `orange` = "#f37735", `gray`="#8c8c8c", `black`="#000000") 

drsimonj_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return (drsimonj_colors)
  drsimonj_colors[cols]}
drsimonj_palettes <- list(`cool` = drsimonj_cols("blue", "yellow", "green", "red"), 
                          `hot`  = drsimonj_cols("dark blue", "orange", "dark green", "dark red"))
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

##---------------------- Loading master data -------------------------------------------------------------------------------------------

# master_data <- read.csv("../../Data/data_random_1/R_master/master_data_124.csv")
master_data <- read.csv("../../Data/data_random_1/R_master/master_data_124_original_labels.csv")

nrow(master_data[master_data$Kmeans_2 != master_data$Sparse_Kmeans_2, ])
nrow(master_data[master_data$Kmeans_3 != master_data$Sparse_Kmeans_3, ])
nrow(master_data[master_data$Kmeans_4 != master_data$Sparse_Kmeans_4, ])
nrow(master_data[master_data$Kmeans_5 != master_data$Sparse_Kmeans_5, ])

master_data$Kmeans_2  <- factor(master_data$Kmeans_2)
master_data$Kmeans_3  <- factor(master_data$Kmeans_3)
master_data$Kmeans_4  <- factor(master_data$Kmeans_4)
master_data$Kmeans_5  <- factor(master_data$Kmeans_5)
master_data$Sparse_Kmeans_2  <- factor(master_data$Sparse_Kmeans_2)
master_data$Sparse_Kmeans_3  <- factor(master_data$Sparse_Kmeans_3)
master_data$Sparse_Kmeans_4  <- factor(master_data$Sparse_Kmeans_4)
master_data$Sparse_Kmeans_5  <- factor(master_data$Sparse_Kmeans_5)

master_data$Kmeans_2 <- ordered(master_data$Kmeans_2,  levels = levels(master_data$Kmeans_2))
master_data$Kmeans_3 <- ordered(master_data$Kmeans_3,  levels = levels(master_data$Kmeans_3))
master_data$Kmeans_4 <- ordered(master_data$Kmeans_4,  levels = levels(master_data$Kmeans_4))
master_data$Kmeans_5 <- ordered(master_data$Kmeans_5,  levels = levels(master_data$Kmeans_5))
master_data$Sparse_Kmeans_2 <- ordered(master_data$Sparse_Kmeans_2,  levels = levels(master_data$Sparse_Kmeans_2))
master_data$Sparse_Kmeans_3 <- ordered(master_data$Sparse_Kmeans_3,  levels = levels(master_data$Sparse_Kmeans_3))
master_data$Sparse_Kmeans_4 <- ordered(master_data$Sparse_Kmeans_4,  levels = levels(master_data$Sparse_Kmeans_4))
master_data$Sparse_Kmeans_5 <- ordered(master_data$Sparse_Kmeans_5,  levels = levels(master_data$Sparse_Kmeans_5))

dim(master_data)
levels(master_data$Kmeans_5)
is.factor(master_data$Kmeans_5)
master_data$Kmeans_5

##---------------------- Statistics ----------------------------------------------------------------------------------------------------
group_by(master_data, Sparse_Kmeans_5) %>%
  summarise(count = n(), 
            mean_GIA_diff =              mean(master_data$GIA_diff, na.rm=TRUE),          
            std_GIA_diff =              sd(master_data$GIA_diff, na.rm=TRUE),
            mean_Processing_Speed_diff = mean(master_data$Proc_Spd_diff, na.rm=TRUE),     
            std_Processing_Speed_diff = sd(master_data$Proc_Spd_diff, na.rm=TRUE),
            mean_Working_Memory_diff =   mean(master_data$Working_Mem_diff, na.rm=TRUE),  
            std_Working_Memory_diff =   sd(master_data$Working_Mem_diff, na.rm=TRUE),
            mean_Broad_Attention_diff =  mean(master_data$Broad_Attn_diff, na.rm=TRUE),   
            std_Broad_Attention_diff =  sd(master_data$Broad_Attn_diff, na.rm=TRUE))

group_by(master_data, Sparse_Kmeans_4) %>%
  summarise(count = n())

group_by(master_data, Sparse_Kmeans_3) %>%
  summarise(count = n())

group_by(master_data, Sparse_Kmeans_2) %>%
  summarise(count = n())

##---------------------- One-way ANOVA test for K_means --------------------------------------------------------------------------------

# summary(aov(cbind(GIA_base, Proc_Spd_base, Working_Mem_base, Broad_Attn_base) ~ Kmeans_5 + Sparse_Kmeans_5, data = master_data))
summary(aov(cbind(GIA_diff, Proc_Spd_diff, Working_Mem_diff, Broad_Attn_diff) ~   Kmeans_5 + Sparse_Kmeans_5, data = master_data))

# summary(aov(cbind(GIA_base, Proc_Spd_base, Working_Mem_base, Broad_Attn_base) ~ Kmeans_4 + Sparse_Kmeans_4, data = master_data))
summary(aov(cbind(GIA_diff, Proc_Spd_diff, Working_Mem_diff, Broad_Attn_diff) ~   Kmeans_4 + Sparse_Kmeans_4, data = master_data))

# summary(aov(cbind(GIA_base, Proc_Spd_base, Working_Mem_base, Broad_Attn_base) ~ Kmeans_3 + Sparse_Kmeans_3, data = master_data))
summary(aov(cbind(GIA_diff, Proc_Spd_diff, Working_Mem_diff, Broad_Attn_diff) ~   Kmeans_3 + Sparse_Kmeans_3, data = master_data))

##---------------------- Kruskal–Wallis test (Non-parametric alternative to one-way ANOVA test) (K=5) ----------------------------------

kruskal.test(GIA_base         ~        Kmeans_5, data = master_data)
kruskal.test(Proc_Spd_base    ~        Kmeans_5, data = master_data)
kruskal.test(Working_Mem_base ~        Kmeans_5, data = master_data)
kruskal.test(Broad_Attn_base  ~        Kmeans_5, data = master_data)

kruskal.test(GIA_base         ~ Sparse_Kmeans_5, data = master_data)
kruskal.test(Proc_Spd_base    ~ Sparse_Kmeans_5, data = master_data)
kruskal.test(Working_Mem_base ~ Sparse_Kmeans_5, data = master_data)
kruskal.test(Broad_Attn_base  ~ Sparse_Kmeans_5, data = master_data)

kruskal.test(GIA_diff         ~        Kmeans_5, data = master_data)
kruskal.test(Proc_Spd_diff    ~        Kmeans_5, data = master_data)
kruskal.test(Working_Mem_diff ~        Kmeans_5, data = master_data)
kruskal.test(Broad_Attn_diff  ~        Kmeans_5, data = master_data)

kruskal.test(GIA_diff         ~ Sparse_Kmeans_5, data = master_data)
kruskal.test(Proc_Spd_diff    ~ Sparse_Kmeans_5, data = master_data)
kruskal.test(Working_Mem_diff ~ Sparse_Kmeans_5, data = master_data)
kruskal.test(Broad_Attn_diff  ~ Sparse_Kmeans_5, data = master_data)

##---------------------- Box plots for ANOVA & Kruskal–Wallis tests (K=5) --------------------------------------------------------------
# Box-plot 1
ggplot(master_data, aes(x = Kmeans_5, y = GIA_base, color = Kmeans_5, fill = Kmeans_5)) + 
  ggtitle("K-Means (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.8073") + ylab("GIA") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 130))
# Box-plot 2
ggplot(master_data, aes(x = Kmeans_5, y = Proc_Spd_base, color = Kmeans_5, fill = Kmeans_5)) + 
  ggtitle("K-Means (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.7989") + ylab("Process Speed") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(30, 130))
# Box-plot 3
ggplot(master_data, aes(x = Kmeans_5, y = Working_Mem_base, color = Kmeans_5, fill = Kmeans_5)) + 
  ggtitle("K-Means (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.7362") + ylab("Working Memory") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 140))
# Box-plot 4
ggplot(master_data, aes(x = Kmeans_5, y = Broad_Attn_base, color = Kmeans_5, fill = Kmeans_5)) + 
  ggtitle("K-Means (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.8805") + ylab("Broad Attention") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 130))

# Box-plot 5
ggplot(master_data, aes(x = Sparse_Kmeans_5, y = GIA_base, color = Sparse_Kmeans_5, fill = Sparse_Kmeans_5)) + 
  ggtitle("K-Means Sparse (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.5696") + ylab("GIA") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 130))
# Box-plot 6
ggplot(master_data, aes(x = Sparse_Kmeans_5, y = Proc_Spd_base, color = Sparse_Kmeans_5, fill = Sparse_Kmeans_5)) + 
  ggtitle("K-Means Sparse (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.4908") + ylab("Process Speed") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(30, 130))
# Box-plot 7
ggplot(master_data, aes(x = Sparse_Kmeans_5, y = Working_Mem_base, color = Sparse_Kmeans_5, fill = Sparse_Kmeans_5)) + 
  ggtitle("K-Means Sparse (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.5674") + ylab("Working Memory") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 140))
# Box-plot 8
ggplot(master_data, aes(x = Sparse_Kmeans_5, y = Broad_Attn_base, color = Sparse_Kmeans_5, fill = Sparse_Kmeans_5)) + 
  ggtitle("K-Means Sparse (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.4613") + ylab("Broad Attention") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 130))

# Box-plot 9
ggplot(master_data, aes(x = Kmeans_5, y = GIA_diff, color = Kmeans_5, fill = Kmeans_5)) + 
  ggtitle("K-Means (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.4212") + ylab("GIA") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))
# Box-plot 10
ggplot(master_data, aes(x = Kmeans_5, y = Proc_Spd_diff, color = Kmeans_5, fill = Kmeans_5)) + 
  ggtitle("K-Means (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.1851") + ylab("Process Speed") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 60))
# Box-plot 11
ggplot(master_data, aes(x = Kmeans_5, y = Working_Mem_diff, color = Kmeans_5, fill = Kmeans_5)) + 
  ggtitle("K-Means (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.2661") + ylab("Working Memory") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))
# Box-plot 12
ggplot(master_data, aes(x = Kmeans_5, y = Broad_Attn_diff, color = Kmeans_5, fill = Kmeans_5)) + 
  ggtitle("K-Means (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.7838") + ylab("Broad Attention") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))

# Box-plot 13
ggplot(master_data, aes(x = Sparse_Kmeans_5, y = GIA_diff, color = Sparse_Kmeans_5, fill = Sparse_Kmeans_5)) + 
  ggtitle("K-Means Sparse (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.1424") + ylab("GIA") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))
# Box-plot 14
ggplot(master_data, aes(x = Sparse_Kmeans_5, y = Proc_Spd_diff, color = Sparse_Kmeans_5, fill = Sparse_Kmeans_5)) + 
  ggtitle("K-Means Sparse (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.06057") + ylab("Process Speed") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 60))
# Box-plot 15
ggplot(master_data, aes(x = Sparse_Kmeans_5, y = Working_Mem_diff, color = Sparse_Kmeans_5, fill = Sparse_Kmeans_5)) + 
  ggtitle("K-Means Sparse (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.9090") + ylab("Working Memory") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))
# Box-plot 16
ggplot(master_data, aes(x = Sparse_Kmeans_5, y = Broad_Attn_diff, color = Sparse_Kmeans_5, fill = Sparse_Kmeans_5)) + 
  ggtitle("K-Means Sparse (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.8478") + ylab("Broad Attention") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))

##---------------------- Kruskal–Wallis test (Non-parametric alternative to one-way ANOVA test) (K=4) ----------------------------------

kruskal.test(GIA_base         ~        Kmeans_4, data = master_data)
kruskal.test(Proc_Spd_base    ~        Kmeans_4, data = master_data)
kruskal.test(Working_Mem_base ~        Kmeans_4, data = master_data)
kruskal.test(Broad_Attn_base  ~        Kmeans_4, data = master_data)

kruskal.test(GIA_base         ~ Sparse_Kmeans_4, data = master_data)
kruskal.test(Proc_Spd_base    ~ Sparse_Kmeans_4, data = master_data)
kruskal.test(Working_Mem_base ~ Sparse_Kmeans_4, data = master_data)
kruskal.test(Broad_Attn_base  ~ Sparse_Kmeans_4, data = master_data)

kruskal.test(GIA_diff         ~        Kmeans_4, data = master_data)
kruskal.test(Proc_Spd_diff    ~        Kmeans_4, data = master_data)
kruskal.test(Working_Mem_diff ~        Kmeans_4, data = master_data)
kruskal.test(Broad_Attn_diff  ~        Kmeans_4, data = master_data)

kruskal.test(GIA_diff         ~ Sparse_Kmeans_4, data = master_data)
kruskal.test(Proc_Spd_diff    ~ Sparse_Kmeans_4, data = master_data)
kruskal.test(Working_Mem_diff ~ Sparse_Kmeans_4, data = master_data)
kruskal.test(Broad_Attn_diff  ~ Sparse_Kmeans_4, data = master_data)

##---------------------- Box plots for ANOVA & Kruskal–Wallis tests (K=4) --------------------------------------------------------------
# Box-plot 1
ggplot(master_data, aes(x = Kmeans_4, y = GIA_base, color = Kmeans_4, fill = Kmeans_4)) + 
  ggtitle("K-Means (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.8642") + ylab("GIA") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 130))
# Box-plot 2
ggplot(master_data, aes(x = Kmeans_4, y = Proc_Spd_base, color = Kmeans_4, fill = Kmeans_4)) + 
  ggtitle("K-Means (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.7711") + ylab("Process Speed") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(30, 130))
# Box-plot 3
ggplot(master_data, aes(x = Kmeans_4, y = Working_Mem_base, color = Kmeans_4, fill = Kmeans_4)) + 
  ggtitle("K-Means (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.7562") + ylab("Working Memory") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 140))
# Box-plot 4
ggplot(master_data, aes(x = Kmeans_4, y = Broad_Attn_base, color = Kmeans_4, fill = Kmeans_4)) + 
  ggtitle("K-Means (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.7736") + ylab("Broad Attention") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 130))

# Box-plot 5
ggplot(master_data, aes(x = Sparse_Kmeans_4, y = GIA_base, color = Sparse_Kmeans_4, fill = Sparse_Kmeans_4)) + 
  ggtitle("K-Means Sparse (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.5131") + ylab("GIA") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 130))
# Box-plot 6
ggplot(master_data, aes(x = Sparse_Kmeans_4, y = Proc_Spd_base, color = Sparse_Kmeans_4, fill = Sparse_Kmeans_4)) + 
  ggtitle("K-Means Sparse (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.8177") + ylab("Process Speed") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(30, 130))
# Box-plot 7
ggplot(master_data, aes(x = Sparse_Kmeans_4, y = Working_Mem_base, color = Sparse_Kmeans_4, fill = Sparse_Kmeans_4)) + 
  ggtitle("K-Means Sparse (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.4454") + ylab("Working Memory") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 140))
# Box-plot 8
ggplot(master_data, aes(x = Sparse_Kmeans_4, y = Broad_Attn_base, color = Sparse_Kmeans_4, fill = Sparse_Kmeans_4)) + 
  ggtitle("K-Means Sparse (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.5316") + ylab("Broad Attention") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 130))

# Box-plot 9
ggplot(master_data, aes(x = Kmeans_4, y = GIA_diff, color = Kmeans_4, fill = Kmeans_4)) + 
  ggtitle("K-Means (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.1246") + ylab("GIA") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))
# Box-plot 10
ggplot(master_data, aes(x = Kmeans_4, y = Proc_Spd_diff, color = Kmeans_4, fill = Kmeans_4)) + 
  ggtitle("K-Means (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.08521") + ylab("Process Speed") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 60))
# Box-plot 11
ggplot(master_data, aes(x = Kmeans_4, y = Working_Mem_diff, color = Kmeans_4, fill = Kmeans_4)) + 
  ggtitle("K-Means (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.8286") + ylab("Working Memory") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))
# Box-plot 12
ggplot(master_data, aes(x = Kmeans_4, y = Broad_Attn_diff, color = Kmeans_4, fill = Kmeans_4)) + 
  ggtitle("K-Means (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.7434") + ylab("Broad Attention") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))

# Box-plot 13
ggplot(master_data, aes(x = Sparse_Kmeans_4, y = GIA_diff, color = Sparse_Kmeans_4, fill = Sparse_Kmeans_4)) + 
  ggtitle("K-Means Sparse (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.1692") + ylab("GIA") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))
# Box-plot 14
ggplot(master_data, aes(x = Sparse_Kmeans_4, y = Proc_Spd_diff, color = Sparse_Kmeans_4, fill = Sparse_Kmeans_4)) + 
  ggtitle("K-Means Sparse (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.3186") + ylab("Process Speed") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 60))
# Box-plot 15
ggplot(master_data, aes(x = Sparse_Kmeans_4, y = Working_Mem_diff, color = Sparse_Kmeans_4, fill = Sparse_Kmeans_4)) + 
  ggtitle("K-Means Sparse (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.9208") + ylab("Working Memory") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))
# Box-plot 16
ggplot(master_data, aes(x = Sparse_Kmeans_4, y = Broad_Attn_diff, color = Sparse_Kmeans_4, fill = Sparse_Kmeans_4)) + 
  ggtitle("K-Means Sparse (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.8950") + ylab("Broad Attention") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))


##---------------------- Kruskal–Wallis test (Non-parametric alternative to one-way ANOVA test) (K=3) ----------------------------------

kruskal.test(GIA_base         ~        Kmeans_3, data = master_data)
kruskal.test(Proc_Spd_base    ~        Kmeans_3, data = master_data)
kruskal.test(Working_Mem_base ~        Kmeans_3, data = master_data)
kruskal.test(Broad_Attn_base  ~        Kmeans_3, data = master_data)

kruskal.test(GIA_base         ~ Sparse_Kmeans_3, data = master_data)
kruskal.test(Proc_Spd_base    ~ Sparse_Kmeans_3, data = master_data)
kruskal.test(Working_Mem_base ~ Sparse_Kmeans_3, data = master_data)
kruskal.test(Broad_Attn_base  ~ Sparse_Kmeans_3, data = master_data)

kruskal.test(GIA_diff         ~        Kmeans_3, data = master_data)
kruskal.test(Proc_Spd_diff    ~        Kmeans_3, data = master_data)
kruskal.test(Working_Mem_diff ~        Kmeans_3, data = master_data)
kruskal.test(Broad_Attn_diff  ~        Kmeans_3, data = master_data)

kruskal.test(GIA_diff         ~ Sparse_Kmeans_3, data = master_data)
kruskal.test(Proc_Spd_diff    ~ Sparse_Kmeans_3, data = master_data)
kruskal.test(Working_Mem_diff ~ Sparse_Kmeans_3, data = master_data)
kruskal.test(Broad_Attn_diff  ~ Sparse_Kmeans_3, data = master_data)

##---------------------- Box plots for ANOVA & Kruskal–Wallis tests (K=3) --------------------------------------------------------------
# Box-plot 1
ggplot(master_data, aes(x = Kmeans_3, y = GIA_base, color = Kmeans_3, fill = Kmeans_3)) + 
  ggtitle("K-Means (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.2338") + ylab("GIA") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 130))
# Box-plot 2
ggplot(master_data, aes(x = Kmeans_3, y = Proc_Spd_base, color = Kmeans_3, fill = Kmeans_3)) + 
  ggtitle("K-Means (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.711") + ylab("Process Speed") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(30, 130))
# Box-plot 3
ggplot(master_data, aes(x = Kmeans_3, y = Working_Mem_base, color = Kmeans_3, fill = Kmeans_3)) + 
  ggtitle("K-Means (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.1368") + ylab("Working Memory") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 140))
# Box-plot 4
ggplot(master_data, aes(x = Kmeans_3, y = Broad_Attn_base, color = Kmeans_3, fill = Kmeans_3)) + 
  ggtitle("K-Means (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.1695") + ylab("Broad Attention") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 130))

# Box-plot 5
ggplot(master_data, aes(x = Sparse_Kmeans_3, y = GIA_base, color = Sparse_Kmeans_3, fill = Sparse_Kmeans_3)) + 
  ggtitle("K-Means Sparse (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.4808") + ylab("GIA") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 130))
# Box-plot 6
ggplot(master_data, aes(x = Sparse_Kmeans_3, y = Proc_Spd_base, color = Sparse_Kmeans_3, fill = Sparse_Kmeans_3)) + 
  ggtitle("K-Means Sparse (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.7000") + ylab("Process Speed") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(30, 130))
# Box-plot 7
ggplot(master_data, aes(x = Sparse_Kmeans_3, y = Working_Mem_base, color = Sparse_Kmeans_3, fill = Sparse_Kmeans_3)) + 
  ggtitle("K-Means Sparse (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.3209") + ylab("Working Memory") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 140))
# Box-plot 8
ggplot(master_data, aes(x = Sparse_Kmeans_3, y = Broad_Attn_base, color = Sparse_Kmeans_3, fill = Sparse_Kmeans_3)) + 
  ggtitle("K-Means Sparse (baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.3340") + ylab("Broad Attention") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 130))

# Box-plot 9
ggplot(master_data, aes(x = Kmeans_3, y = GIA_diff, color = Kmeans_3, fill = Kmeans_3)) + 
  ggtitle("K-Means (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.3075") + ylab("GIA") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))
# Box-plot 10
ggplot(master_data, aes(x = Kmeans_3, y = Proc_Spd_diff, color = Kmeans_3, fill = Kmeans_3)) + 
  ggtitle("K-Means (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.2630") + ylab("Process Speed") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 60))
# Box-plot 11
ggplot(master_data, aes(x = Kmeans_3, y = Working_Mem_diff, color = Kmeans_3, fill = Kmeans_3)) + 
  ggtitle("K-Means (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.8819") + ylab("Working Memory") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))
# Box-plot 12
ggplot(master_data, aes(x = Kmeans_3, y = Broad_Attn_diff, color = Kmeans_3, fill = Kmeans_3)) + 
  ggtitle("K-Means (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.8546") + ylab("Broad Attention") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))

# Box-plot 13
ggplot(master_data, aes(x = Sparse_Kmeans_3, y = GIA_diff, color = Sparse_Kmeans_3, fill = Sparse_Kmeans_3)) + 
  ggtitle("K-Means Sparse (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.2324") + ylab("GIA") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))
# Box-plot 14
ggplot(master_data, aes(x = Sparse_Kmeans_3, y = Proc_Spd_diff, color = Sparse_Kmeans_3, fill = Sparse_Kmeans_3)) + 
  ggtitle("K-Means Sparse (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.1790") + ylab("Process Speed") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 60))
# Box-plot 15
ggplot(master_data, aes(x = Sparse_Kmeans_3, y = Working_Mem_diff, color = Sparse_Kmeans_3, fill = Sparse_Kmeans_3)) + 
  ggtitle("K-Means Sparse (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.9684") + ylab("Working Memory") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))
# Box-plot 16
ggplot(master_data, aes(x = Sparse_Kmeans_3, y = Broad_Attn_diff, color = Sparse_Kmeans_3, fill = Sparse_Kmeans_3)) + 
  ggtitle("K-Means Sparse (36 months - baseline)") + xlab("\n Kruskal–Wallis test,   p-value = 0.8786") + ylab("Broad Attention") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))

##---------------------- t test (K=2) --------------------------------------------------------------------------------------------------

t.test(GIA_base         ~        Kmeans_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Proc_Spd_base    ~        Kmeans_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Working_Mem_base ~        Kmeans_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Broad_Attn_base  ~        Kmeans_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(GIA_base         ~ Sparse_Kmeans_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Proc_Spd_base    ~ Sparse_Kmeans_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Working_Mem_base ~ Sparse_Kmeans_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Broad_Attn_base  ~ Sparse_Kmeans_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)

t.test(GIA_diff         ~        Kmeans_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Proc_Spd_diff    ~        Kmeans_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Working_Mem_diff ~        Kmeans_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Broad_Attn_diff  ~        Kmeans_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(GIA_diff         ~ Sparse_Kmeans_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Proc_Spd_diff    ~ Sparse_Kmeans_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Working_Mem_diff ~ Sparse_Kmeans_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)
t.test(Broad_Attn_diff  ~ Sparse_Kmeans_2, data = master_data, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F)

##---------------------- Box plots for Wilcoxon rank sum tests -------------------------------------------------------------------------
# Box-plot 1
ggplot(master_data, aes(x = Kmeans_2, y = GIA_base, color = Kmeans_2, fill = Kmeans_2)) + 
  ggtitle("K-Means (baseline)") + xlab("\n Wilcoxon-rank-sum test,   p-value = 0.2354") + ylab("GIA") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 130))
# Box-plot 2
ggplot(master_data, aes(x = Kmeans_2, y = Proc_Spd_base, color = Kmeans_2, fill = Kmeans_2)) + 
  ggtitle("K-Means (baseline)") + xlab("\n Wilcoxon-rank-sum test,   p-value = 0.9874") + ylab("Process Speed") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(40, 130))
# Box-plot 3
ggplot(master_data, aes(x = Kmeans_2, y = Working_Mem_base, color = Kmeans_2, fill = Kmeans_2)) + 
  ggtitle("K-Means (baseline)") + xlab("\n Wilcoxon-rank-sum test,   p-value = 0.7301") + ylab("Working Memory") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 140))
# Box-plot 4
ggplot(master_data, aes(x = Kmeans_2, y = Broad_Attn_base, color = Kmeans_2, fill = Kmeans_2)) + 
  ggtitle("K-Means (baseline)") + xlab("\n Wilcoxon-rank-sum test,   p-value = 0.9872") + ylab("Broad Attention") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 130))
# Box-plot 5
ggplot(master_data, aes(x = Sparse_Kmeans_2, y = GIA_base, color = Sparse_Kmeans_2, fill = Sparse_Kmeans_2)) + 
  ggtitle("K-Means Sparse (baseline)") + xlab("\n Wilcoxon-rank-sum test,   p-value = 0.1177") + ylab("GIA") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 130))
# Box-plot 6
ggplot(master_data, aes(x = Sparse_Kmeans_2, y = Proc_Spd_base, color = Sparse_Kmeans_2, fill = Sparse_Kmeans_2)) + 
  ggtitle("K-Means Sparse (baseline)") + xlab("\n Wilcoxon-rank-sum test,   p-value = 0.9372") + ylab("Process Speed") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(40, 130))
# Box-plot 7
ggplot(master_data, aes(x = Sparse_Kmeans_2, y = Working_Mem_base, color = Sparse_Kmeans_2, fill = Sparse_Kmeans_2)) + 
  ggtitle("K-Means Sparse (baseline)") + xlab("\n Wilcoxon-rank-sum test,   p-value = 0.4623") + ylab("Working Memory") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 140))
# Box-plot 8
ggplot(master_data, aes(x = Sparse_Kmeans_2, y = Broad_Attn_base, color = Sparse_Kmeans_2, fill = Sparse_Kmeans_2)) + 
  ggtitle("K-Means Sparse (baseline)") + xlab("\n Wilcoxon-rank-sum test,   p-value = 0.7939") + ylab("Broad Attention") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(50, 130))

# Box-plot 9
ggplot(master_data, aes(x = Kmeans_2, y = GIA_diff, color = Kmeans_2, fill = Kmeans_2)) + 
  ggtitle("K-Means (36 months - baseline)") + xlab("\n Wilcoxon-rank-sum test,   p-value = 0.4367") + ylab("GIA") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))
# Box-plot 10
ggplot(master_data, aes(x = Kmeans_2, y = Proc_Spd_diff, color = Kmeans_2, fill = Kmeans_2)) + 
  ggtitle("K-Means (36 months - baseline)") + xlab("\n Wilcoxon-rank-sum test,   p-value = 0.07074") + ylab("Process Speed") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 60))
# Box-plot 11
ggplot(master_data, aes(x = Kmeans_2, y = Working_Mem_diff, color = Kmeans_2, fill = Kmeans_2)) + 
  ggtitle("K-Means (36 months - baseline)") + xlab("\n Wilcoxon-rank-sum test,   p-value = 0.7249") + ylab("Working Memory") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))
# Box-plot 12
ggplot(master_data, aes(x = Kmeans_2, y = Broad_Attn_diff, color = Kmeans_2, fill = Kmeans_2)) + 
  ggtitle("K-Means (36 months - baseline)") + xlab("\n Wilcoxon-rank-sum test,   p-value = 0.8544") + ylab("Broad Attention") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))
# Box-plot 13
ggplot(master_data, aes(x = Sparse_Kmeans_2, y = GIA_diff, color = Sparse_Kmeans_2, fill = Sparse_Kmeans_2)) + 
  ggtitle("K-Means Sparse (36 months - baseline)") + xlab("\n Wilcoxon-rank-sum test,   p-value = 0.9570") + ylab("GIA") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))
# Box-plot 14
ggplot(master_data, aes(x = Sparse_Kmeans_2, y = Proc_Spd_diff, color = Sparse_Kmeans_2, fill = Sparse_Kmeans_2)) + 
  ggtitle("K-Means Sparse (36 months - baseline)") + xlab("\n Wilcoxon-rank-sum test,   p-value = 0.1739") + ylab("Process Speed") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 60))
# Box-plot 15
ggplot(master_data, aes(x = Sparse_Kmeans_2, y = Working_Mem_diff, color = Sparse_Kmeans_2, fill = Sparse_Kmeans_2)) + 
  ggtitle("K-Means Sparse (36 months - baseline)") + xlab("\n Wilcoxon-rank-sum test,   p-value = 0.3000") + ylab("Working Memory") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))
# Box-plot 16
ggplot(master_data, aes(x = Sparse_Kmeans_2, y = Broad_Attn_diff, color = Sparse_Kmeans_2, fill = Sparse_Kmeans_2)) + 
  ggtitle("K-Means Sparse (36 months - baseline)") + xlab("\n Wilcoxon-rank-sum test,   p-value = 0.4864") + ylab("Broad Attention") + 
  geom_boxplot(size = 0.8, outlier.size = 3) + scale_fill_drsimonj(palette = "cool") + scale_color_drsimonj(palette = "hot") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(-50, 30))

