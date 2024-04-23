# libraries
library(tidyverse)
library(dplyr)
library(MASS)
library(viridis)
library(spatstat.geom) 

# setup
options(scipen=9)

# for the maths bit, we need functions to convert between measurements
# acos works with radians, not with degrees
deg2rad = function(deg) {
  return((pi * deg) / 180)
}

rad2deg = function(rad) {
  return((180 * rad) / pi)
}

# data
LLD <- read.csv(file = '/Users/dana/Documents/R/heatmaps_lal/LLD_All_Features_clean.csv', header = TRUE) 
SCD <- read.csv(file = '/Users/dana/Documents/R/heatmaps_lal/SCD_All_Features_clean.csv', header = TRUE) 
# correct names > remove /
LLD$feature <- gsub("\\/", "", LLD$feature)
SCD$feature <- gsub("\\/", "", SCD$feature)
# gets unique features, counts occurences, drops any under 10
LLD_list_df <- LLD %>% group_by(feature) %>% summarize(count=n()) %>% filter(count >= 10)
SCD_list_df <- SCD %>% group_by(feature) %>% summarize(count=n()) %>% filter(count >= 10)

# for maths, different list to loop is needed
LLD_list_names <- as.data.frame(unique(LLD$feature))
SCD_list_names <- as.data.frame(unique(SCD$feature))
colnames(LLD_list_names) <- "feature"
colnames(SCD_list_names) <- "feature"

# combine list
both_list <- rbind(LLD_list_names, SCD_list_names) %>% unique() %>% dplyr::filter(!(feature == ""))

# also not all features allow looping over, so to avoid errors exclude them from automation
# both_list <- rbind(LLD_list_names, SCD_list_names) %>% unique() %>% dplyr::filter(!(feature == "")) %>% dplyr::filter(!(feature == "table")) %>% dplyr::filter(!(feature == "icentered")) %>% dplyr::filter(!(feature == "iccentered")) %>% dplyr::filter(!(feature == "bic")) %>% dplyr::filter(!(feature == "drawing")) %>% dplyr::filter(!(feature == "thumbprint")) %>% dplyr::filter(!(feature == "ic")) %>% dplyr::filter(!(feature == "stamp")) %>% dplyr::filter(!(feature == "iuc"))

# loop for LLD
for (feature_item in LLD_list_df$feature) {
  # get feature
  LLD_feature <- LLD %>% dplyr::filter(feature == feature_item)
  LLD_feature_coord <- LLD_feature %>% dplyr::select(x, y) %>% as.data.frame()

  # density calc
  density_LLD <- kde2d(LLD_feature_coord$x, LLD_feature_coord$y)
  density_LLD_df <- as.data.frame(density_LLD)
  
  # histogram
  jpeg(paste0("/Users/dana/Documents/R/heatmaps_lal/first_outputs/LLD_histogram_", feature_item,".jpeg"), units = "in", width = 6, height = 7.5, res = 300)
  print(
    ggplot(LLD_feature_coord, aes(x=x, y=y) ) +
    geom_hex(bins = 30, show.legend = FALSE) +
    scale_fill_viridis(option = "magma", direction = -1) +
    theme_bw() +
    xlab(" ") +
    ylab(" ") +
    scale_x_continuous(expand=c(0, 0), limits=c(0, 1700)) +
    scale_y_continuous(expand=c(0, 0), limits=c(2210, 0), trans = "reverse") +
    ggtitle(paste("LLD: ", feature_item),
            subtitle = "Histogram")
    )
  dev.off()

  # density plot
  jpeg(paste0("/Users/dana/Documents/R/heatmaps_lal/first_outputs/LLD_density_", feature_item,".jpeg"), units = "in", width = 6, height = 7.5, res = 300)
  print(
    ggplot(density_LLD_df, aes(x, y)) +
    stat_density_2d(aes(fill = ..level..),
                    geom = "polygon",
                    h = 150,
                    n = 10,
                    bins = 100,
                    contour = TRUE,
                    show.legend = FALSE,
                    alpha = 0.5) +
    scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "transparent") +
    theme_bw() +
    xlab(" ") +
    ylab(" ") +
    scale_x_continuous(expand=c(0, 0), limits=c(0, 1700)) +
    scale_y_continuous(expand=c(0, 0), limits=c(2210, 0), trans = "reverse") +
    ggtitle(paste("LLD: ", feature_item),
            subtitle = "Density Plot")
  )
  dev.off()
}



# loop for SCD
for (feature_item in SCD_list_df$feature) {
  # get feature
  SCD_feature <- SCD %>% filter(feature == feature_item)
  SCD_feature_coord <- SCD_feature %>% dplyr::select(x, y) %>% as.data.frame()

  # density calc
  density_SCD <- kde2d(SCD_feature_coord$x, SCD_feature_coord$y)
  density_SCD_df <- as.data.frame(density_SCD)

  # histogram
  jpeg(paste0("/Users/dana/Documents/R/heatmaps_lal/first_outputs/SCD_histogram_", feature_item,".jpeg"), units = "in", width = 6, height = 7.5, res = 300)
  print(
    ggplot(SCD_feature_coord, aes(x=x, y=y) ) +
    geom_hex(bins = 30, show.legend = FALSE) +
    scale_fill_viridis(option = "magma", direction = -1) +
    theme_bw() +
    xlab(" ") +
    ylab(" ") +
    scale_x_continuous(expand=c(0, 0), limits=c(0, 1700)) +
    scale_y_continuous(expand=c(0, 0), limits=c(2210, 0), trans = "reverse") +
    ggtitle(paste("SCD: ", feature_item),
            subtitle = "Histogram")
  )
  dev.off()

  # density plot
  jpeg(paste0("/Users/dana/Documents/R/heatmaps_lal/first_outputs/SCD_density_", feature_item,".jpeg"), units = "in", width = 6, height = 7.5, res = 300)
  print(
    ggplot(density_SCD_df, aes(x, y)) +
    stat_density_2d(aes(fill = ..level..),
                    geom = "polygon",
                    h = 150,
                    n = 10,
                    bins = 100,
                    contour = TRUE,
                    show.legend = FALSE,
                    alpha = 0.5) +
    scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "transparent") +
    theme_bw() +
    xlab(" ") +
    ylab(" ") +
    scale_x_continuous(expand=c(0, 0), limits=c(0, 1700)) +
    scale_y_continuous(expand=c(0, 0), limits=c(2210, 0), trans = "reverse") +
    ggtitle(paste("SCD: ", feature_item),
            subtitle = "Density Plot")
  )
  dev.off()
}






