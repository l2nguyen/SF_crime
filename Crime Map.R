library(dplyr)
library(ggmap)
library(ggplot2)
library(readr)

setwd("/Users/Zelda/Data Science/SF_crime")

# Load dataset
train <- read.csv("/train.csv")

# Quick look at the data
str(train)
head(train, n=10)

# Get map of SF
map< qmap(location="sanfrancisco",zoom=12,source="osm",color="bw")

counts<- summarise(group_by(counts, Category), Counts=length(Category))
counts<- counts[order(-counts$Counts)]
top12 <- train[train$Category %in% counts$Category[c(1,3:13)],]

# Generate map
p<- ggmap(map) +
    geom_point(data=top12, aes(x=X, y=Y, color=factor(Category)), alpha=0.1) +
    guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0),
                                 title="Type of Crime")) +
    scale_colour_brewer(type="qual",palette="Paired") +
    ggtitle("Top Crimes in San Francisco") +
    theme_light(base_size=20)

# Save map
ggsave("sf_top_crimes_map.png", p, width=14, height=10, units="in")