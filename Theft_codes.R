

# Import Data
districts_theft_shp <- readOGR(dsn = "C:/Users/91875/Desktop/LA Crime Spatial AC Project/LAPD_Reporting_Districts.shp")
crime_data = read.csv("C:/Users/91875/Desktop/LA Crime Spatial AC Project/crime_against_women_cleaned.csv")

#taking a look at the shape file data
head(districts_theft_shp@data)
length(unique(districts_theft_shp@data$REPDIST))
# So, we have 1135 unique dist codes in the shape file

# taking a look at the crime data
head(crime_data)

#Our objective is to find out if there are spatial clustering for theft crime numbers across police districts    
#we need to sum up all the theft crimes against women on a district level
theft_only <- subset(crime_data,Crm.Cd.Desc == "THEFT PLAIN - PETTY ($950 & UNDER)")
nrow(theft_only)
w1<-table(theft_only$Rpt.Dist.No)
head(w1)

#Putting the district codes to district_theft data frame
district_theft<-as.data.frame(w1) 
head(district_theft)

#Looking at the no of uniq districts in the burg data
length(unique(district_theft$Var1))

# There are 1135 distinct districts.Now there is a problem. The no of districs
#in burg data is > shape file. since the shape file is for LA crime reporting dist's, 
#it means that these dists had 0 reported crime
# Lets try joining.when joining, we keep these columns by using "all.x = TRUE".
#This means we keep all rows of data in the shapefile. 
#Now merging two data with theft_only$Rpt.Dist.No and districts@data$REPDIST
districts_theft_shp@data <- merge(districts_theft_shp@data,district_theft, by.x = "REPDIST", by.y = "Var1", all.x = TRUE)

#replacing NA with 0
districts_theft_shp$Freq[is.na(districts_theft_shp$Freq)] <- 0


#Plotting crime frequency
var <- districts_theft_shp@data[,"Freq"]
head(var)
?classIntervals
breaks <- classIntervals(var, n = 9, style = "quantile")
head(breaks)

my_colours <- rev(brewer.pal(9, "RdBu"))
head(my_colours)

#plotting crime freq
plot(districts_theft_shp, col = my_colours[findInterval(var, breaks$brks, all.inside = TRUE)],   
     axes = FALSE, border = NA)
legend(x = -118.7, y = 34, legend = leglabs(breaks$brks), fill = my_colours, bty = "n", cex = 0.6)

#We can see some isolated instances of high theft numbers in the north and south parts of LA

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
nb <- poly2nb(districts_theft_shp, queen=TRUE)

#For each polygon in our polygon object, nb lists all neighboring polygons.
nb[[1]]
#The numbers represent the polygon IDs as stored in the spatial object districts

#Next, we need to assign weights to each neighboring polygon. In our case, each neighboring polygon will be assigned equal weight (style="W").
lw <- nb2listw(nb, style="W", zero.policy=TRUE)

#To see the weight of the first polygon's four neighbors type:
lw$weights[1]

#Now calculating Morans I statistic using MC simulation method. We calculate the pseudo p value from the MC simulation
MC<- moran.mc(districts_theft_shp$Freq, lw, nsim=999)
MC
plot(MC, main="", las=1)

#Not significant--> Means that theft numbers across districts are spatially random.