data <- read.csv("gtd_lite.csv")
head(data, 10)
dim(data)
library(dplyr)
glimpse(data)
summary(data)
library(DataExplorer)
DataExplorer::create_report(data)
library(FactoMineR)
library("factoextra")
res.pca <- PCA(data,  graph = FALSE)
scatterplotMatrix(data)
library(purrr)
library(tidyr)
library(ggplot2)
data %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram()
library(ggpubr)
ggbarplot(data, x = "country", y = "nkill",
          fill = "cyl",               # change fill color by cyl
          color = "white",            # Set bar border colors to white
          palette = "jco",            # jco journal color palett. see ?ggpar
          sort.val = "desc",          # Sort the value in dscending order
          sort.by.groups = FALSE,     # Don't sort inside each group
          x.text.angle = 90           # Rotate vertically x axis texts)

          