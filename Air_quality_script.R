library(tidyverse)
library(ggplot2)
library(dplyr)
#importing cleaned data set of average measurements for the year
year_avg <- read.csv("C:\\Users\\willd\\OneDrive\\Desktop\\DataAnalysis\\AirQT\\Air_Quality_avg.csv")
#importing cleaned data set of seasonal information
season <- read.csv("C:\\Users\\willd\\OneDrive\\Desktop\\DataAnalysis\\AirQT\\Air_Quality.csv")
colnames(season)
# on average is the FPM higher in the summer or the winter 
season_agg <- aggregate(season$Mean.mcg.per.cubic.meter, list(season$Season), FUN=mean)
colnames(season_agg) <- c("Season", "AVG_FPM")
# aggregate by part of city and year to find worst year and area 
colnames(year_avg)
city_agg <- aggregate(year_avg$Mean.mcg.per.cubic.meter, list(year_avg$Geo.Place.Name), FUN=mean)
colnames(city_agg) <- c("Area", "AVG_mcg_per_cubic_meter")
worst <- city_agg[order(city_agg$AVG_mcg_per_cubic_mete, decreasing = TRUE), ]
worst <- worst[1:10, ]
year_agg <- aggregate(year_avg$Mean.mcg.per.cubic.meter, list(year_avg$Year), FUN=mean)
colnames(year_agg) <- c("Year", "AVG_mcg_per_cubic_meter")
#Visuals 
# Bar chart of the trend over the years for the entire city
year_viz <- ggplot(year_agg, aes(x=Year, y=AVG_mcg_per_cubic_meter, group=1)) + 
  geom_line(color="red")+
  geom_point()+
  ggtitle("Average Fine Particulate Matter in NYC")
# visual of top 10 areas with the most FPM over
area_viz <- ggplot(worst, aes(x=Area, y=AVG_mcg_per_cubic_meter, fill=Area)) + 
  geom_bar(stat = "identity")+
  ggtitle("NYC Areas With Highest FPM")+
  theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1))
area_viz
# which season is worse visual 
season_viz <- ggplot(season_agg, aes(x= Season, y=AVG_FPM, fill=Season))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(breaks = round(seq(min(0), max(15), by = 0.25)))+
  ggtitle("AVG FPM by Season")
plot(area_viz)
