# Carla Weidner - Data Analytics Presentation 

# Look at top 3 areas with highest crime rates
### Look at time of crimes
### Type of crimes against women in those districts


#### Import data ####
library(rio)
la_crime_2010 <- import("/Users/carlasuzanneweidner/Documents/Crime_Data_from_2010_to_2019.csv")
la_crime_2020 <- import("/Users/carlasuzanneweidner/Documents/Crime_Data_from_2020_to_Present.csv")

#### Subset to only females ####
crime_2010_females <- la_crime_2010[la_crime_2010$`Vict Sex` == 'F', ]
crime_2020_females <- la_crime_2020[la_crime_2020$`Vict Sex` == 'F', ]
female.data <- rbind(crime_2010_females, crime_2020_females)

#### Create a column for only years ####
library(stringr)
crime_2010_females$year <- substr(crime_2010_females$`DATE OCC`, 7, 11)
crime_2020_females$year <- substr(crime_2020_females$`DATE OCC`, 7, 11)

#### Column for Month ####
crime_2010_females$month <- substr(crime_2010_females$`DATE OCC`, 1, 2)
crime_2020_females$month <- substr(crime_2020_females$`DATE OCC`, 1, 2)

#### Column for Weekday ####
library(lubridate)
wday(mdy(LACrime$DateOccurred),label = TRUE)

#### Combine into 1 dataset ####
year.data <- c(crime_2010_females$year, crime_2020_females$year)
time.data <- c(crime_2010_females$`TIME OCC`, crime_2020_females$`TIME OCC`)
area.data <- c(crime_2010_females$`AREA NAME`, crime_2020_females$`AREA NAME`)
desc.data <- c(crime_2010_females$`Crm Cd Desc`, crime_2020_females$`Crm Cd Desc`)
lat.data <- c(crime_2010_females$LAT, crime_2020_females$LAT)
lon.data <- c(crime_2010_females$LON, crime_2020_females$LON)
month.data <- c(crime_2010_females$month, crime_2020_females$month)
age.data <- c(crime_2010_females$age, crime_2020_females$age)
rpdst.data <- c(crime_2010_females$`Rpt Dist No`, crime_2020_females$`Rpt Dist No`)
females <- data.frame(year=year.data,
                      time=time.data,
                      area=area.data,
                      desc=desc.data,
                      lat=lat.data,
                      lon=lon.data,
                      month=month.data,
                      age=age.data,
                      rpdst=rpdst.data)
#females$n <- 1
#years <- seq(from=2010, to=2021, by=1)

#### Visualization ####
library(ggplot2)
library(tidyverse)
library(leaflet)
library(stringr)
library(rgdal)
library(lubridate)
library(forecast)
library(DT)
library(prophet)
library(caret)
#install.packages("magrittr") # package installations are only needed the first time you use it
#install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%

females_latlon <- cbind(as.numeric(females$lat), as.numeric(females$lon))

# Month of crime
females %>%
  group_by(month) %>%
  summarise(CountIncidents = n()) %>%
  mutate(month = reorder(month,CountIncidents)) %>%
  
  ggplot(aes(x = month,y = CountIncidents)) +
  geom_bar(stat='identity',colour="white", fill="orange") +
  geom_text(aes(x = month, y = 1, label = paste0("(",CountIncidents,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Month Of Crime', y = 'Count of Incidents', 
       title = 'Count of Incidents') +
  coord_flip() + 
  theme_bw()

# Day of crime

# Age of Victims
breaks = seq(0,100,5)
ListSex = c("F")
females %>%
  ggplot(aes(age)) +
  geom_histogram(binwidth = 5,fill = c("red")) +
  facet_wrap(~ "F") +
  scale_x_continuous(limits = c(0, 100),breaks=breaks ) +
  labs(x = 'Victim Age', y = 'Count of Crimes', 
       title = 'Age and Crimes') +
  theme_bw()

# Map of crime sites!!
library(leaflet)
center_lon = median(female$lon,na.rm = TRUE)
center_lat = median(females$lat,na.rm = TRUE)

femalesCrimeSample = females %>% sample_n(50e3)

leaflet(femalesCrimeSample) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircles(lng = ~lon, lat = ~lat, 
             color = c("red"))  %>%
  # controls
  setView(lng=center_lon, lat=center_lat, zoom=10)

# Grouped by reporting district
RDLocation = female.data %>% 
  group_by('Rpt Dist') %>%
  summarise(RDlat = median(LAT,na.rm = TRUE)
            ,RDlon = median(LON,na.rm = TRUE)
            ,CountIncidents = n()) %>%
  arrange(desc(CountIncidents)) %>%
  head(50)

leaflet(RDLocation) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
addCircles(lng = ~RDlon, lat = ~RDlat, radius = ~sqrt(CountIncidents)*30,
           color = c("red"))  %>%
  # controls
  setView(lng=center_lon, lat=center_lat, zoom=10)


# Crimes committed against women in LA, 2010-2021 
occurences.year <- table(unlist(females$year))
plot(occurences.year, xlab = "Year", ylab = "Count of Crimes",
     col="blue", type = "h")
mtext("Crimes Committed Against Women in LA, 2010-2021")

# Crimes committed against women in LA, 2010-2021 (ggplot)
occurences <- as.data.frame(occurences.year)
# plot(occurences$Var1, occurences$Freq, type = "l")
ggplot(occurences, aes(x = Var1, y = Freq)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.3) +
  labs(x = "Year", y = "Count of Crimes", 
       title = "Crimes Committed Against Women in LA",
       subtitle = "2010-2021")

# Color bar plot
ggplot(females,aes(fill=area, x=year,y=n)) + 
  geom_bar(position="stack", stat = "identity") + 
  labs(x = "Year", y = "Count of Crimes", 
       title = "Crimes Committed Against Women in LA",
       subtitle = "2010-2021") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  scale_fill_discrete(name = "Area") +
  scale_y_continuous(breaks=seq(0,150000,15000))

# Not working code (labels on bars)
geom_text(aes(label = n), vjust = -0.3)

# Pie chart of Areas (not working yet)
library(scales)
ggplot(females, aes(x=factor(1), fill=area))+
  geom_bar(width = 1)+
  coord_polar("y") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),) 
  geom_text(aes(y = area/21 + c(0, cumsum(area)[-length(area)]), 
              label = percent(area/100)), size=5)

  
# Finding max area of crimes
counts.area <- table(unlist(females$area))
sort(counts.area)
# Time occ of 3 max crimes
hist(females$time[females$area=="77th Street"], 
     xlim = c(0000, 2400),
     breaks = 24,
     main = "Time of Crimes at 77th Street",
     xlab = "Time",
     ylab = "Count of Crimes",
     col = "red")

hist(females$time[females$area=="Southwest"], 
     xlim = c(0000, 2400),
     breaks = 24,
     main = "Time of Crimes at Southwest",
     xlab = "Time",
     ylab = "Count of Crimes",
     col = "blue")

hist(females$time[females$area=="Southeast"], 
     xlim = c(0000, 2400),
     breaks = 24,
     main = "Time of Crimes at Southeast",
     xlab = "Time",
     ylab = "Count of Crimes",
     col = "purple")

# Plot of times
# hist(females$time, col="blue", xlab)
ggplot(females, aes(time)) + 
  geom_bar(width = 100)  + 
  labs(x = "Time", y = "Count of Crimes", 
       title = "Crimes Committed Against Women in LA by Time",
       subtitle = "00:00 - 24:00") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(0,2400,100))




library(ggpubr)
ggpie(
  females, x = "prop", label = "prop",
  lab.pos = "in", lab.font = list(color = "white"), 
  fill = "area", color = "white",
  palette = "jco"
)




y2010 <- females[females$year == 2010,]


# others
library(dplyr)
library(tidyr)
newframe <- females %>% gather("variable","value",-year)
ggplot(newframe, aes(x=year, y=value, color=variable)) +
  geom_point() +
  geom_smooth() +

ggplot(females, aes(x=year, y=n)) + geom_line()
#plot(years, occurences.year)
#ggplot(data = females) +
# geom_line(aes(x = years, y = occurences.year))




#### Not Used Code ####
for (i in years){
  hist(crime_2010_females$'DESC'[crime_2010_females$year == i])
  mtext(i)
  occurences <- table(unlist(crime_2010_females$'DESC'
                             [crime_2010_females$year == i]))
}

# Read in Excel doc
library("readxl")
my_data <- read_excel("/Users/carlasuzanneweidner/Library/Mobile Documents/com~apple~CloudDocs/Crime_Data_from_2010_to_2019.xls")

library("rlang")
library("ggplot2")

g <- ggplot(my_data, aes(x = Vict_Age, y = Vict_Sex))

qplot()
qplot(Vict_Age, Vict_Sex, data = my_data, geom = "line")
qplot(Vict_Age, data = my_data, geom = "histogram", bins = 50)
qplot(Vict_Age, data = my_data, geom = "boxplot")
qplot(Status, data = my_data)

qplot(Vict_Descent, data = my_data)
qplot(TIME_OCC, data = my_data, geom = "histogram", bins = 24)
qplot(AREA_NAME, data = my_data)


#### Matching lat/lon to Census Block Data ####
library(maptools)
library(maps)
library(sp)
library(classInt)
library(rgdal)
library(tidyr)
library(RColorBrewer)
#library("spdep")
library(plyr)
library(tmap)
la_shp <- readShapePoly("/Users/carlasuzanneweidner/Downloads/Census_Block_Groups_2020/Census_Block_Groups_2020.shp")

dsn <- system.file("vectors", package = "rgdal")[1]
shapefile <- readOGR(dsn=dsn, layer="scot_BNG")
points_in_group <- over(lat_lon,la_shp)

shapefile <- spTransform(shapefile, CRS("+proj=longlat +datum=WGS84")) #change CRS

# https://geohub.lacity.org/datasets/lacounty::census-block-groups-2020/about
lat_lon <- data.frame(la_crime_2020$LAT, la_crime_2020$LON)
coordinates(lat_lon) <- ~la_crime_2020.LAT+la_crime_2020.LON
over(lat_lon,la_shp)
loc_group <- lat_lon %over% la_shp


districts <- readOGR(dsn = "../input/LAPD_Reporting_Districts.shp")

w <- table(la_crime_2020$`Rpt Dist No`)
dist.no <- as.data.frame(w)
#districts@data <- merge(districts@data, rep.dis, by.x = "REPDIST", by.y = "Var1", all.x = TRUE)
la_shp@data <- merge(la_shp@data, dist.no, by.x = "OBJECTID", by.y = "Var1", all.x = TRUE)

# trying cloropeth map
var <- la_shp@data[,"Freq"]
breaks <- classIntervals(var, n = 9, style = "fisher")
my_colours <- rev(brewer.pal(9, "RdBu"))
plot(la_shp, col = my_colours[findInterval(var, breaks$brks, all.inside = TRUE)],   
     axes = FALSE, border = NA)
legend(x = -118.7, y = 34, legend = leglabs(breaks$brks), fill = my_colours, bty = "n", cex = 0.6)

# trying plotting lat/lon on shp map
LA <-spTransform(la_shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(LA, xlab="long")
axis(1)  #Check the coordinates
axis(2)
#plot the points
plot(la_shp)
points(x=females$lon, y=females$lat, pch=16, col=rgb(red=0.2, green=0.2, blue=1.0, alpha=0.2))

LA <-spTransform(la_shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
plot(NY, xlab="long")
axis(1)  #Check the coordinates
axis(2)
#plot the points
points(x=df_2$Long, y=df_2$Lat, pch = 20, col = "orange")

## GGplot test
library(ggplot2)
library("rnaturalearth")
library("rnaturalearthdata")
library("rgeos")

lat.lon.df <- as.data.frame(lat_lon)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf() +
  geom_point(data = lat.lon.df, aes(x = la_crime_2020.LON, y = la_crime_2020.LAT), size = 4, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-120, -115), ylim = c(30, 35), expand = FALSE)



#### Understanding data ####
pacman::p_load(pacman, dplyr, GGaly, ggplot2, ggthemes,
               ggavis, httr, lubridate, plotly, rio,
               markdown, shiny, stringr, tidyr)

