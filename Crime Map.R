rm(list = ls())

library(dplyr)
library(ggmap)
library(ggplot2)
library(readr)
library(leaflet)
library(RColorBrewer)

setwd("/Users/Zelda/Data Science/SF_crime")

# Load dataset
crime <- read.csv("train.csv", header=TRUE)

# Quick look at the data
str(crime)
head(crime, n=10)

# Convert dates variable into POSIXct for more date/time functionality
crime$Dates <- strptime(crime$Dates, format="%Y-%m-%d %H:%M:%S")
class(crime$Dates)  # Check it worked

# Add new variables about year/month/day of date and hour
crime <-
  crime %>%
  mutate(Year  = (crime$Dates$year + 1900),
         Month = (crime$Dates$mon + 1),
         dayDate = crime$Dates$mday,
         Hour  = crime$Dates$hour,
         DayOfWeek = factor(DayOfWeek, levels=c("Monday",
                                                "Tuesday",
                                                "Wednesday",
                                                "Thursday",
                                                "Friday",
                                                "Saturday",
                                                "Sunday"))
  )

str(crime)  # Check it worked
crime$Dates <- as.POSIXct(crime$Dates)  # dplyr functions do not like POSIXlt

# Look at the type of crimes in the dataset
unique(crime$Category)

# Count the occurence of each crime
counts<- crime %>% count(Category, sort=TRUE)
# Make data frame with only top 10 crimes
top10 <-
  crime %>%
  filter(Category %in% counts$Category[1:10]) %>%
  droplevels()


# Make two data frames to focus on certain crimes

# Violent crimes
violent <-
  crime %>%
  filter(Category %in% c("ASSAULT", "ROBBERY", "SEX OFFENSES FORCIBLE")) %>%
  droplevels()

# Theft
theft <-
  crime %>%
  filter(Category %in%
           c("LARCENY/THEFT", "ROBBERY", "BURGLARY", "STOLEN PROPERTY")) %>%
  droplevels()

###--- DATA WRANGLING/VISUALIZATION ---###

# Which neighborhood has the most crime?
plot1<- ggplot(data=crime,
        aes(x=reorder(PdDistrict, -table(PdDistrict)[PdDistrict]))) +
        geom_bar(stat="count", width=0.5, fill="steelblue") +
        scale_size_area() +
        xlab("District") +
        ylab("Count") +
        theme(axis.text.x = element_text(angle = 45, vjust=1, hjust = 1))

# HOURS where the most crime occurs
plot2<- ggplot(data=crime, aes(x=Hour)) +
        geom_bar(colour="black", fill="royalblue") +
        xlab("Hour of Day") +
        ylab('Count')
# Fewer crime occur around 1-6 AM

# DAYS OF THE WEEK where the most crime occurs
plot3<- ggplot(data=crime, aes(x=DayOfWeek)) +
        geom_bar(colour="black", fill="royalblue") +
        xlab("Day of the Week") +
        ylab('Count')
# Crime seems to be happening every day

# How often are crimes solved?
unique(crime$Resolution)
crime %>% count(Resolution, sort=TRUE)
# Looks like most cases do not have a resolution
# Note: What is a psychopathic case?

###--- MAPPING OF SF ---###

# Get map of SF
map<- get_map(location="sanfrancisco",zoom=12,source="osm",color="bw")

# Generate map
p<- ggmap(map) +
    geom_point(data=top10, aes(x=X, y=Y, color=factor(Category)), alpha=0.1) +
    guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0),
                                 title="Type of Crime")) +
    scale_colour_brewer(type="qual",palette="Paired") +
    ggtitle("Top Crimes in San Francisco") +
    theme_light(base_size=20)

# Save map
ggsave("sf_top_crimes_map.png", p, width=14, height=10, units="in")

# Trying out leaflet package

crime.map <- function(categories, n) {

  new.crimes <- filter(crime, Category %in% categories) %>% droplevels()

  pal <- colorFactor(brewer.pal(length(unique(new.crimes$Category)), "Set1"),
                     domain = new.crimes$Category)

  leaflet(new.crimes[1:n,]) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addCircleMarkers (lng =  ~X, lat =  ~Y,
                      color = ~pal(Category),
                      opacity = .7, radius  = 1) %>%
    addLegend(pal = pal, values = new.crimes$Category)
}

# Violent crime map
crime.map(c("ASSAULT", "ROBBERY", "SEX OFFENSES FORCIBLE"), n = 2000)
