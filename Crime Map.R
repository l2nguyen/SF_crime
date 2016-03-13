rm(list = ls())

library(dplyr)
library(ggmap)
library(ggplot2)
library(readr)

setwd("/Users/Zelda/Data Science/SF_crime")

# Load dataset
crime <- read.csv("train.csv", header=TRUE)

# Quick look at the data
str(crime)
head(crime, n=10)

# Convert dates variable into POSIXct for more date/time functionality
crime$Dates <- strptime(crime$Dates, format="%Y-%m-%d %H:%M:%S")
class(crime$Dates)  # Check it worked

# Split date into separate variables
crime$day <- crime$Dates$mday  # Date
crime$month <- crime$Dates$mon + 1  # Month
crime$hour <- crime$Dates$hour  # Hour of the day

head(crime)  # Check it worked

###--- DATA VISUALIZATION ---###

# Which neighborhood has the most crime?


plot1<- ggplot(data=crime,
               aes(x=reorder(PdDistrict, -table(PdDistrict)[PdDistrict]))) +
        geom_bar(stat="count", width=0.5, fill="steelblue") +
        scale_size_area() +
        xlab("District") +
        ylab("Count") +
        theme(axis.text.x = element_text(angle = 45, vjust=1, hjust = 1))

###--- MAPPING OF SF ---###

# Get map of SF
map< qmap(location="sanfrancisco",zoom=12,source="osm",color="bw")

counts<- summarise(group_by(counts, Category), Counts=length(Category))
counts<- counts[order(-counts$Counts)]
top12 <- crime[crime$Category %in% counts$Category[c(1,3:13)],]

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