##########################################
##########################################
###RScript to make correlations between###
###Data of Primates from AnAge for their##
###lifespan and the computed ortholog ####
#########lengths from ensembl#############
##########################################

#IMPORT libraries

library(ape)
library(geiger)
library(nlme)
library(phytools)
library(ggplot2)
library(qqman)
library(qualityTools)
library(Haplin)
library(ggpubr)
library(gridExtra)
library(grid)
library(reshape2) ## or library(reshape2)
library(tidyr)
library(dplyr)
library(corrplot)

#First we read all traits information
All_traits <- read.csv("../data/Phenome/Primate_Traits/OUTPUT/MergedTraitsALL.txt", header = T, sep = "\t")
summary(All_traits)
#First quick visualization
res <- cor(data.matrix(All_traits))

col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white",
                           "cyan", "#007FFF", "blue", "#00007F"))
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                           "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                           "#4393C3", "#2166AC", "#053061"))
col3 <- colorRampPalette(c("red", "white", "blue")) 
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
                           "cyan", "#007FFF", "blue", "#00007F"))

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45,)


