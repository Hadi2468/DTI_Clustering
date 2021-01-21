## HeatMap
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("ComplexHeatmap")
# install.packages("Cairo")
# install.packages("plyr")
# library(Cairo)
library(ComplexHeatmap)
library(circlize)
library (plyr)
library(cluster)
##-------------------------------------------------------
B_GIA       <- read.csv("../Data/Neurocognitive_Scores/Joins/cluster_base_GIA.csv")


Heatmap(as.matrix(Data_SubjectAsRow_FeatureAsColumn), name="ComeUpWithAName",
        row_names_side="left",
        row_dend_side = "right",
        row_names_max_width=unit(80, "mm"),
        column_names_max_height =unit(200, "mm"),
        top_annotation = ha,
        column_dend_height=unit(5, "mm"),
        clustering_distance_rows=spdist1,
        clustering_method_rows = "complete",
        clustering_distance_columns = spdist1,
        clustering_method_columns = "complete",
        show_column_names = VSFlag)
  Heatmap(AVectorOfClusterMembership,
          name = " ComeUpWithAName ",
          show_row_names = FALSE,
          width = unit(5, "mm")) 
  Heatmap(AVectorOfDemographicOrNeurocognitiveVariable,
          name = " ComeUpWithAName ",
          show_row_names = FALSE,
          width = unit(5, "mm"))

