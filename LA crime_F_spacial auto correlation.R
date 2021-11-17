
#CLEAN UP
#clear base packages
detach("package:datasets",unload=TRUE) 
#clear plots
dev.off()
#clear Console
cat("\014")
# :)

require(pacman)
pacman::p_load(plyr,tmap,pacman,dplyr,GGally,ggplot2,ggthemes,ggvis,httr,lubridate,plotly,rio,rmarkdown,shiny,stringr,tidyr,classInt,maptools,rgdal,RColorBrewer,"spdep")

# Import Data
districts <- readOGR(dsn = "C:/Users/91875/Desktop/LA Crime Spatial AC Project/LAPD_Reporting_Districts.shp")
crime_data = read.csv("C:/Users/91875/Desktop/LA Crime Spatial AC Project/crime_against_women_cleaned.csv")

#taking a look at the shape file data
head(districts@data)
length(unique(districts@data$REPDIST))
# So, we have 1135 unique dist codes in the shape file

# taking a look at the crime data
head(crime_data)

#Now merging two data with crime_data$Rpt.Dist.No and districts@data$REPDIST
# Before that, we need to sum up all the crimes against women on a district level
w<-table(crime_data$Rpt.Dist.No)
head(w)

#Putting the district codes to district_crime data frame
district_crime<-as.data.frame(w) 
head(district_crime)

#Looking at the no of uniq districts in the crime data
length(unique(district_crime$Var1))
head(district_crime)

# There are 1223 distinct districts.Now there is a problem. The no of districs
#in crime data is > shape file. since the shape file is for LA crime reporting dist's, 
#it means that these dists had 0 reported crime
# Lets try joining.when joining, we keep these columns by using "all.x = TRUE".
#This means we keep all rows of data in the shapefile.  
districts@data <- merge(districts@data,district_crime, by.x = "REPDIST", by.y = "Var1", all.x = TRUE)

#replacing NA with 0
districts$Freq[is.na(districts$Freq)] <- 0
length(districts$Freq)       
head(districts@data)

#Plotting crime frequency
var <- districts@data[,"Freq"]
head(var)
?classIntervals
breaks <- classIntervals(var, n = 9, style = "fisher")
head(breaks)
#The "fisher" style uses the algorithm proposed by W. D. Fisher (1958) as the Fisher-Jenks algorithm; 
#This style will sub-sample by default for more than 3000 observations.

#Making the intervals using quantile logic
breaks <- classIntervals(var, n = 9, style = "quantile")
my_colours <- rev(brewer.pal(9, "RdBu"))
head(my_colours)

#plotting crime freq
plot(districts, col = my_colours[findInterval(var, breaks$brks, all.inside = TRUE)],   
     axes = FALSE, border = NA)
legend(x = -118.7, y = 34, legend = leglabs(breaks$brks), fill = my_colours, bty = "n", cex = 0.6)

#We can see some isolated instances of high crime numbers against women in the north, east and south parts of LA

#Objective: To find out if there are clustering of similar values across LA, more than expected by chance (Spatial Randomness)
#which could lead to geographical patterns and to find the causes for it, if there are any.

# How do we do it?

#By calculating Spatial Autocorrelation--> Basically trying to measure the degree of dependency among the district wise crime numbers;
#helps understand the degree to which one object is similar to other neighboring objs in a geographical space
#The basis of this work is Tobler's First Law of Geography: "everything is related to everything else, but near things are more related than distant things."

#First we Calculate the Global Spacial Auto Correlation Statistic to see if there is clustering in the data as a whole.
#Null Hypothesis-->Spacial Randomness (absence of any pattern; values of one location doesn't depend upon the value of others). If rejected then there is evidence of spatial structure
#Test statistic (we use Moran's I) is used to test whether the Null hypothesis is True. It is calculated from data and is compared to the reference Null distribution 
#Basically looking at, how likely the value is, if it had occurred under the Null Hypothesis(Spatial Randomness) 
#The test statistic has two components 
#1)Attribute similarity (we use cross product, abs square etc)
#2) Location Similarity
#How does the test statistic work?
# measures the similarity for the attributes we are interested in, and add this for all possible pairs, but exclude all pairs that are not neighbors
#We use Spatial Weights Matrix for this 

#The first step requires that we define "neighboring" polygons.
#Here, we'll adopt a contiguous neighbor definition where we'll accept any contiguous polygon that shares at least on vertex 
#(this is the "queen" case and is defined by setting the parameter queen=TRUE).

nb <- poly2nb(districts, queen=TRUE)

#For each polygon in our polygon object, nb lists all neighboring polygons.
nb[[1]]
#The numbers represent the polygon IDs as stored in the spatial object districts

#Next, we need to assign weights to each neighboring polygon. In our case, each neighboring polygon will be assigned equal weight (style="W").
lw <- nb2listw(nb, style="W", zero.policy=TRUE)

#To see the weight of the first polygon's four neighbors type:
lw$weights[1]

#Now calculating Morans I statistic using MC simulation method. We calculate the pseudo p value from the MC simulation
MC<- moran.mc(districts$Freq, lw, nsim=999)
MC
plot(MC, main="", las=1)

#The Global Moran I statistic is 0.11226 and p value is 0.001 (positive and significant), showing that there is some clustering of like values.

#Local Spatial Auto Correlation--> To tell us where the clusters are
# Local Indicator of Spatial Association (LISA)--> We wanna assess significance of Local Statistic at each Location
# We do this to identify hot spots (High High) and cold spots in the cross section
#Let us calculate Local Moran's I value for all the districts.

# First let us look at Moran's Scatter plot  (centers the scatter plot on the mean and gives you 4 kinds of spatial Association.
# High-High, Low- Low clusters (surrounded by similar neighbors) & Spatial Outliers )
# Y axis you have the spatial Lag and X axis Zi
moran <- moran.plot(districts$Freq, listw = nb2listw(nb, style = "W"))

#Compute Local Moran
?localmoran
local <- localmoran(x = districts$Freq, listw = nb2listw(nb, style = "W"))
head(local)

#We get a number of useful statistics from the model which are as defined:
#Ii: local moran statistic
#E.Ii: expectation of local moran statistic
#Var.Ii: variance of local moran statistic
#Z.Ii: standard deviate of local moran statistic
#Pr(): p-value of local moran statistic

#Plot local Moran (A map the local moran statistic (Ii))
#A positive value for Ii indicates that the unit is surrounded by units with similar values.

# binds results to our polygon shapefile
moran.map <- cbind(districts, local)
tm_shape(moran.map) + tm_fill(col = "Ii", style = "quantile", title = "local moran statistic")

#Now we have to see out these Ii values, which are significant.
# Plotting LISA clusters (High High--> Hotspots and Low Low---> COldspots;Makes the connection between the significance and the Morans Scatter plot)

quadrant <- vector(mode="numeric",length=nrow(local))

# centers the variable of interest around its mean
m.crimes <- districts$Freq - mean(districts$Freq)     

# centers the local Moran's around the mean
m.local <- local[,1] - mean(local[,1])    

# significance threshold
signif <- 0.05 

# builds a data quadrant
quadrant[m.crimes >0 & m.local>0] <- 4  
quadrant[m.crimes <0 & m.local<0] <- 1      
quadrant[m.crimes <0 & m.local>0] <- 2
quadrant[m.crimes >0 & m.local<0] <- 3
quadrant[local[,5]>signif] <- 0   

# plot in r
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(districts,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
box()
legend("bottomleft", legend = c("insignificant","low-low","low-high","high-low","high-high"),
       fill=colors,bty="n")

#Code Source: 
#https://rpubs.com/quarcs-lab/spatial-autocorrelation  
#https://mgimond.github.io/Spatial/spatial-autocorrelation-in-r.html
