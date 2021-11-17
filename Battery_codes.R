

# Import Data
districts_battery_shp <- readOGR(dsn = "C:/Users/91875/Desktop/LA Crime Spatial AC Project/LAPD_Reporting_Districts.shp")
crime_data = read.csv("C:/Users/91875/Desktop/LA Crime Spatial AC Project/crime_against_women_cleaned.csv")

#taking a look at the shape file data
head(districts_battery_shp@data)
length(unique(districts_battery_shp@data$REPDIST))
# So, we have 1135 unique dist codes in the shape file

# taking a look at the crime data
head(crime_data)

#Our objective is to find out if there are spatial clustering for BATTERY - SIMPLE ASSAULT crime numbers across police districts    
#we need to sum up all the battery crimes against women on a district level
battery_only <- subset(crime_data,Crm.Cd.Desc == "BATTERY - SIMPLE ASSAULT")
nrow(battery_only)
w1<-table(battery_only$Rpt.Dist.No)
head(w1)

#Putting the district codes to district_battery data frame
district_battery<-as.data.frame(w1) 
head(district_battery)

#Looking at the no of uniq districts in the battery data
length(unique(district_battery$Var1))

# There are 1144 distinct districts.Now there is a problem. The no of districts
#in battery data is > shape file. since the shape file is for LA crime reporting dist's, 
#it means that these dists had 0 reported crime
# Lets try joining. when joining, we keep these columns by using "all.x = TRUE".
#This means we keep all rows of data in the shapefile. 
#Now merging two data with battery_only$Rpt.Dist.No and districts@data$REPDIST
districts_battery_shp@data <- merge(districts_battery_shp@data,district_battery, by.x = "REPDIST", by.y = "Var1", all.x = TRUE)

#replacing NA with 0
districts_battery_shp$Freq[is.na(districts_battery_shp$Freq)] <- 0


#Plotting crime frequency
var <- districts_battery_shp@data[,"Freq"]
head(var)

breaks <- classIntervals(var, n = 9, style = "quantile")

my_colours <- rev(brewer.pal(9, "RdBu"))
head(my_colours)

#plotting crime freq
plot(districts_battery_shp, col = my_colours[findInterval(var, breaks$brks, all.inside = TRUE)],   
     axes = FALSE, border = NA)
legend(x = -118.7, y = 34, legend = leglabs(breaks$brks), fill = my_colours, bty = "n", cex = 0.6)

#We can see some isolated instances of high battery numbers in the north,east and south parts of LA

#Objective: To find out if there are clustering of similar values across LA, more than expected by chance (Spatial Randomness)
#which could lead to geographical patterns and to find the causes for it, if there are any.

# How do we do it?

#By calculating Spatial Autocorrelation--> Basically trying to measure the degree of dependency among the district wise crime numbers;
#helps understand the degree to which one object is similar to other neighboring objs in a geographical space
#The basis of this work is Tobler's First Law of Geography: "everything is related to everything else, but near things are more related than distant things."

#First we Calculate the Global Spacial Auto Correlation Statistic to see if there is clustering in the data as a whole.
#Null Hypothesis-->Spacial Randomness (absence of any pattern; values of one location doesn't depend upon the value of others). If rejected then there is evidence of spatial structure
#Test statistic (we use Moran's I) is used to test whether the Null hypothesis is True. It is calculated from data and compared to the reference Null distribution
#Basically looking how likely the value is, if it had occurred under the Null Hypothesis(Spatial Randomness) 
#The test statistic has two components 1)Attribute similarity we use cross product, abs square etc 2) Location Similarity
#How does the test statistic work?
# measures the similarity for the attributes we are interested in, and add this for all possible pairs, but exclude all pairs that are not neighbours
#We use Spatial Weights Matrix for this 

#The first step requires that we define "neighboring" polygons.
#Here, we'll adopt a contiguous neighbor definition where we'll accept any contiguous polygon that shares at least on vertex 
#(this is the "queen" case and is defined by setting the parameter queen=TRUE).
nb <- poly2nb(districts_battery_shp, queen=TRUE)

#For each polygon in our polygon object, nb lists all neighboring polygons.
nb[[1]]
#The numbers represent the polygon IDs as stored in the spatial object districts

#Next, we need to assign weights to each neighboring polygon. In our case, each neighboring polygon will be assigned equal weight (style="W").
lw <- nb2listw(nb, style="W", zero.policy=TRUE)

#To see the weight of the first polygon's four neighbors type:
lw$weights[1]

#Now calculating Morans I statistic using MC simulation method. We calculate the pseudo p value from the MC simulation
MC<- moran.mc(districts_battery_shp$Freq, lw, nsim=999)
MC
plot(MC, main="", las=1)

#The Global Moran I statistic is 0.1583 and p value is 0.001 (positive and significant), showing that there is some clustering of like values.

#Local Spatial Auto Correlation--> To tell us where the clusters are
# Local Indicator of Spatial Association (LISA)--> We wanna assess significance of Local Statistic at each Location
# We do this to identify hot spots (High High) and cold spots in the cross section
#Let us calculate Local Moran's I value for all the districts.

# First let us look at Moran's Scatter plot  (centers the scatter plot on the mean and gives you 4 kinds of spatial Association.
# High-High, Low- Low clusters (surrounded by similar neighbors) & Spatial Outliers )
# Y axis you have the spatial Lag and X axis Zi
moran <- moran.plot(districts_battery_shp$Freq, listw = nb2listw(nb, style = "W"))

#Compute Local Moran
?localmoran
local <- localmoran(x = districts_battery_shp$Freq, listw = nb2listw(nb, style = "W"))
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
moran.map <- cbind(districts_battery_shp, local)
tm_shape(moran.map) + tm_fill(col = "Ii", style = "quantile", title = "local moran statistic")

#Now we have to see out these Ii values, which are significant.
# Plotting LISA clusters (High High--> Hotspots and Low Low---> COldspots;Makes the connection between the significance and the Morans Scatter plot)

quadrant <- vector(mode="numeric",length=nrow(local))

# centers the variable of interest around its mean
m.crimes <- districts_battery_shp$Freq - mean(districts_battery_shp$Freq)     

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
plot(districts_battery_shp,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
box()
legend("bottomleft", legend = c("insignificant","low-low","low-high","high-low","high-high"),
       fill=colors,bty="n")

