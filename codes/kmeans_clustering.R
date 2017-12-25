# Title: kmeans_clustering
# Date: December 2017
# Goal: Build clustering algorithm and plot our results



# Load packages -----------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(broom)


# Run kmeans algorithm ----------------------------------------------------

set.seed(666)
kmeans_results <- kmeans(x = data_clustering, 3, nstart = 100)


# Plot the output ---------------------------------------------------------

data_output <- cbind(data_renamed, kmeans_results$cluster) %>% 
                rename(
                  cluster = V2
                )

data_output[Team %in% c("MTL", "COL", "PIT"),] %>% 
  ggplot(aes(x = Shot.SlapShot, TOI_GP, label = Last_Name, color = as.factor(cluster))) +
    geom_label()



# Evaluate numbers of clusters --------------------------------------------

kclusts <- data.frame(k = 1:9) %>% 
            group_by(k) %>% 
            do(kclust = kmeans(data_clustering, .$k))

clusterings <- kclusts %>% 
                group_by(k) %>%
                do(glance(.$kclust[[1]]))

ggplot(clusterings, aes(k, tot.withinss)) + 
  geom_line()
